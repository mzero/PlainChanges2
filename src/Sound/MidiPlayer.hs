module Sound.MidiPlayer
    (
    midiPlayer,
    )
where

{-
    A cleaner, (text) interactive version of the
    'Euterpea.IO.MIDI.MidiIO.playMidi' function.

    Plays a 'M.Midi' value via "Sound.PortMidi". Rather than dump the whole
    stream into the driver, this code keeps the output buffer a fraction of
    a second ahead of the current play position. This allows the player to
    respond to commands such as pause and rewind, quickly.

    While playing, the current play head time is updated on 'stdout'.
    Characters on 'stdin' control the player:

        space   - toggle playing / paused
        [ and ] - jump back/forward 10 seconds
        x       - pause
        q       - quit

    The player doesn't exit at the end of the Midi, allowing easy review and
    replay of the end (via the [ command). Use the q command to quit.

    The player will silence all notes (via Control Change 123) on all channels
    as needed to keep from having stuck notes.

    Notes aren't tracked, so notes that should continue to play across a jump
    or a pause/play sequence, won't.

    TODO: track notes
    TODO: all jumping to markers in the MIDI
-}

import qualified Codec.Midi as M
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CULong)
import qualified Sound.PortMidi as P
import System.IO
import Text.Printf (printf)

-- | Play midi on a PortMidi device. When the end of the midi is reached, the
-- player pauses, but does not quit. User must type q to quit.
midiPlayer :: P.DeviceID -> M.Midi -> IO ()
midiPlayer devOut midi = do
    P.initialize >>= reportPMError "initialize"
    eStream <- P.openOutput devOut 1 -- latency of 1 means timed output
    case eStream of
        Right err -> reportPMError "openOutput" err
        Left stream -> initialPlayState stream makeTrack >>= run
    P.terminate >>= reportPMError "terminate"
  where
    makeTrack = mapMaybe toPMEvent oneAbsRealTrack
    singleTrackMidi = M.toSingleTrack midi
    oneAbsRealTrack =  case M.tracks singleTrackMidi of
        [t] -> M.toRealTime (M.timeDiv singleTrackMidi) $ M.toAbsTime t
        _ -> []

{-
    CONVERSION

    "Codec.Midi" and "Sound.PortMidi" use differnt formats to represent MIDI
    messages and streams.

    These conversion functions are currently incomplete, but enough for our
    purposes.
-}


toPMMsg :: M.Message -> Maybe P.PMMsg
toPMMsg m = case m of
    M.NoteOff c p v         -> chMsg 0x80 c p v
    M.NoteOn c p v          -> chMsg 0x90 c p v
    M.KeyPressure c p pr    -> chMsg 0xA0 c p pr
    M.ControlChange c cn cv -> chMsg 0xB0 c cn cv
    M.ProgramChange c pn    -> chMsg 0xC0 c pn unused
    M.ChannelPressure c pr  -> chMsg 0xD0 c pr unused
    M.PitchWheel c pb       -> let (hi,lo) = pb `divMod` 256
                               in chMsg 0xE0 c lo hi
    _                       -> Nothing
  where
    chMsg s c a b = Just $ P.PMMsg (s .|. (fromIntegral c .&. 0x0F))
                                (fromIntegral a) (fromIntegral b)
    unused = 0 :: Int

fromPMMsg :: P.PMMsg -> Maybe M.Message
fromPMMsg (P.PMMsg m d1 d2) = case (m .&. 0xF0) of
    0x80 -> chMsg2 M.NoteOff
    0x90 -> chMsg2 M.NoteOn
    0xA0 -> chMsg2 M.KeyPressure
    0xB0 -> chMsg2 M.ControlChange
    0xC0 -> chMsg1 M.ProgramChange
    0xD0 -> chMsg1 M.ChannelPressure
    0xE0 -> Just $ M.PitchWheel c (b * 256 + a)
    0xF0 -> Nothing -- SysEx event not handled
    _    -> Nothing
  where
    chMsg2 f = Just $ f c a b
    chMsg1 f = Just $ f c a
    c = fromIntegral (m .&. 0x0F)
    a = fromIntegral d1
    b = fromIntegral d2


toPMEvent :: (M.Time, M.Message) -> Maybe P.PMEvent
toPMEvent (t,m) = (\pm -> P.PMEvent pm $ cvt t) `fmap` toPMMsg m
  where
    cvt = round . (*1000)

{-
    LOW LEVEL
-}

type TimestampMS -- = P.Timestamp   -- saddly not exported by Sound.PortMidi
                 = CULong           -- in milliseconds

-- | How far into the future to send messages. Too long and transport controls
-- will feel sluggish. Too short, and the queue may empty before the next tick.
-- Should be at least twice 'tickInterval'.
queueDuration :: TimestampMS
queueDuration = 250

-- | How often to push events into the queue when playing. Too short and too
-- much CPU time is wasted. Too long and 'queueDuration' must be made longer,
-- hence unresponsive.
tickInterval :: TimestampMS
tickInterval = 100

data PlayState = PS
    { psTrack :: [P.PMEvent]        -- in ascending time order
    , psT0Abs :: TimestampMS        -- where t=0 is in absolute time
    , psTNext :: TimestampMS        -- the timestamp of the next event to send
    , psTrackNext :: [P.PMEvent]    -- remaining events to send,
                                    -- corresponds to psTNext
    , psTMax :: TimestampMS         -- last relative timestamp in the track
    , psPlaying :: Bool
    , psStream :: P.PMStream
    }

initialPlayState :: P.PMStream -> [P.PMEvent] -> IO PlayState
initialPlayState stream evs = P.time >>= \now -> return $ PS
        { psTrack = evs
        , psT0Abs = now
        , psTNext = 0
        , psTrackNext = evs
        , psTMax = lastTimestamp evs
        , psPlaying = True
        , psStream = stream
        }
  where
    lastTimestamp [] = 0
    lastTimestamp evs = P.timestamp $ last evs


sendEvents :: TimestampMS -> [P.PMEvent] -> PlayState -> IO ()
sendEvents t0Abs es ps = send >>= reportPMError "writeEvents"
  where
    send = P.writeEvents (psStream ps) $ map offset es
    offset e = e { P.timestamp = t0Abs + P.timestamp e }

quiet :: PlayState -> IO ()
quiet ps = do
    tNowAbs <- P.time
    sendEvents tNowAbs qEvents ps
  where
    qEvents = mapMaybe toPMEvent [(0, M.ControlChange ch 123 0) | ch <- [0..15]]

tick :: PlayState -> IO PlayState
tick ps = if not (psPlaying ps) then return ps else do
    tNowAbs <- P.time
    let t0Abs = psT0Abs ps
        tStart = psTNext ps
        tEnd = (tNowAbs - t0Abs) + queueDuration
        (evsToSend, evsNext) = span ((< tEnd) . P.timestamp) $ psTrackNext ps
    sendEvents t0Abs evsToSend ps
    let ps' = ps { psTNext = tEnd, psTrackNext = evsNext }
    if tStart > psTMax ps
        then pause ps'
        else return ps'

play :: PlayState -> IO PlayState
play ps =
    if psPlaying ps
        then return ps
        else do
            tNowAbs <- P.time
            let t0Abs = tNowAbs - psTNext ps
                ps' = ps { psT0Abs = t0Abs, psPlaying = True }
            tick ps'

pause :: PlayState -> IO PlayState
pause ps =
    if not $ psPlaying ps
        then return ps
        else do
            quiet ps
            return ps { psPlaying = False }

toggle :: PlayState -> IO PlayState
toggle ps = if psPlaying ps then pause ps else play ps

jumpTo :: TimestampMS -> PlayState -> IO PlayState
jumpTo tNext ps = pause ps >>= play . adjustNext
  where
    adjustNext ps = ps { psTNext = tNext, psTrackNext =  trackNext }
    trackNext = dropWhile ((< tNext) . P.timestamp) trackFrom
    trackFrom = if tNext < psTNext ps then psTrack ps else psTrackNext ps

jumpForward, jumpBackward :: TimestampMS -> PlayState -> IO PlayState
jumpForward delta ps = jumpTo tNext' ps
  where tNext' = min (psTMax ps) (psTNext ps + delta)
jumpBackward delta ps = jumpTo tNext' ps
  where tNext' = if psTNext ps < delta then 0 else psTNext ps - delta

status :: PlayState -> IO ()
status ps = P.time >>= putStr. statusLine >> hFlush stdout
  where
    statusLine tAbs = printf "  %4d:%02d.%1d - %4s\r" tMin tSec tTenths st
      where
        t :: Int
        t = fromIntegral $ if psPlaying ps then tAbs - psT0Abs ps else psTNext ps
        (tInSec, tMs) = t `divMod` 1000
        (tMin, tSec) = tInSec `divMod` 60
        tTenths = tMs `div` 100
        st = if psPlaying ps then "play" else "stop"

message :: String -> PlayState -> IO PlayState
message msg ps = putStrLn msg >> return ps

reportPMError :: String -> P.PMError -> IO ()
reportPMError fn e = when (e /= P.NoError) $ do
    err <- P.getErrorText e
    hPutStrLn stderr $ fn ++ ": " ++ show err


-- | The main logic of the player. The running player consists of three threads.
--
-- The 'cmdThread' does all the work. It serializes its actions by pulling
-- commands from an 'MVar'. This thread explicitly maintains the succession of
-- 'PlayState' values (rather than use a state monad). It also is responsible
-- for outputing to 'stdout'.
--
-- The 'tickThread' simply sleeps and injects a 'tick' command into the 'MVar'.
--
-- The 'kbdLoop' is run in the invoking thread, and listens for characters
-- on 'stdin', and injects commands based on what is typed. When this function
-- finishes, the player exits.
--
-- Note that 'stdin' and 'stdout' are set and reset so as to have no buffering
-- during operation.
run :: PlayState -> IO ()
run ps0 = newEmptyMVar >>= run' ps0

run' :: PlayState -> MVar (PlayState -> IO PlayState) -> IO ()
run' ps0 cmds = do
    cmdThread <- forkIO $ cmdLoop ps0
    tickThread <- forkIO tickLoop
    bracket setupIO id $ \_ -> do
        kbdLoop
        waitTimestamp queueDuration -- make sure it is drained
        killThread tickThread
        killThread cmdThread
  where
    setupIO = do
        ibuf <- hGetBuffering stdin
        obuf <- hGetBuffering stdout
        iecho <- hGetEcho stdin
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        return $ do
            hSetEcho stdin iecho
            hSetBuffering stdin ibuf
            hSetBuffering stdout obuf
            putStr "\n\n"

    cmdLoop ps = status ps >> takeMVar cmds >>= ($ ps) >>= cmdLoop

    tickLoop = forever $  waitTimestamp tickInterval >> putMVar cmds tick
    kbdLoop = do
        ch <- hGetChar stdin
        case ch of
            ' ' -> putMVar cmds toggle
            'x' -> putMVar cmds pause
            '[' -> putMVar cmds $ jumpBackward 10000
            ']' -> putMVar cmds $ jumpForward 10000
            'q' -> putMVar cmds pause
            _ -> putMVar cmds $ message $ "Unmapped key: " ++ show ch
        if ch /= 'q'
            then kbdLoop
            else return ()

waitTimestamp :: TimestampMS -> IO ()
waitTimestamp t = threadDelay $ fromIntegral $ t * 1000



{- TO DO
[] ensure quiet on crash in cmd thread (perhaps?)
[] stop ticking when paused
[] add more commands
-}


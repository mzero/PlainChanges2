module MidiUtil
    ( Checker, CheckResult
    , check, okay

    , TrackChecker
    , checkChannel, runCheckChannel
    )
where

import Codec.Midi as M
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.List (foldl')


type Checker err = Writer [(M.Time, err)] ()
type CheckResult err = Either [(M.Time, err)] ()

execChecker :: Checker err -> CheckResult err
execChecker c = case execWriter c of
    [] -> Right ()
    errs -> Left errs

check :: M.Time -> Bool -> err -> Checker err
check _ True _ = return ()
check t False err = tell [(t, err)]

okay :: Checker a
okay = return ()


-- | Extract all the information for a single channel into a single track.
-- Non-channelized messages are retained, but only from tracks that have
-- at least one message for the channel.
channelTrack :: M.Channel -> M.Midi -> M.Track M.Time
channelTrack ch midi =
    convert $ foldl' M.merge [] $ map onlyCh $ filter hasCh $ tracks midi
  where
    convert = toRealTime (timeDiv midi) . toAbsTime
    hasCh = not . null . filter ((== Just ch) . msgCh . snd)
    onlyCh = filter (maybe True (== ch) . msgCh . snd)
    msgCh m = if M.isChannelMessage m then Just (M.channel m) else Nothing

type TrackChecker err = M.Track M.Time -> Checker err

checkChannel :: M.Channel -> TrackChecker err -> M.Midi -> CheckResult err
checkChannel ch chkr = execChecker . chkr . channelTrack ch

runCheckChannel :: (Show err) => M.Channel -> TrackChecker err -> M.Midi -> IO ()
runCheckChannel ch chkr midi = case checkChannel ch chkr midi of
    Left errs -> mapM_ print errs
    Right () -> print "PASSED"


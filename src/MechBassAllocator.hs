module MechBassAllocator
    ( AllocMessages
    , allocator
    , recombine
    )
where

import Codec.Midi (Channel, Key, Message(..), Time, Track, Velocity,
    isChannelMessage)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Data.Function (on)
import Data.List (intercalate, maximumBy, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

import MechBass
import MidiUtil (messageOrder)


data PlayState
    = Free
    | Playing Fret Time Key
    | Played Fret Time Key Time Velocity

data PerString = Ps { psState :: PlayState
                    , psString :: BassString
                    }

type OffAction = Time -> Velocity -> AllocM ()

data AllocState = As { asEvents :: Track Time
                     , asStrings :: Map.Map Int PerString
                     , asOffActions :: [((Key, Maybe Channel), OffAction)]
                     , asMessages :: AllocMessages
                     }

initialAllocatorState :: AllocState
initialAllocatorState =
    As { asEvents = []
       , asStrings = Map.fromList $ zip [0..] $ map (Ps Free) bassStrings
       , asOffActions = []
       , asMessages = []
       }

data AllocMsg = Unplayable Key
              | UnmatchedNoteOff Key
              | StolenNote Channel Key Time Time
              | Dump String
    deriving (Eq)

instance Show AllocMsg where
    show (Unplayable k) = "Unplayable " ++ show k
    show (UnmatchedNoteOff k) = "UnmatchedNoteOff " ++ show k
    show (StolenNote ch k tOrig tShort) =
        printf "Stolen Note %d %d : %.3f -> %.3f" ch k tOrig tShort
    show (Dump s) = "Dump " ++ s

type AllocMessages = [(Time, AllocMsg)]


type AllocM = State AllocState

outputEvent :: Time -> Message -> AllocM ()
outputEvent te ev = modify (\s -> s { asEvents = (te,ev) : asEvents s })

outputMessage :: Time -> AllocMsg -> AllocM ()
outputMessage te am = do
    modify (\s -> s { asMessages = (te,am) : asMessages s })
    case am of
        Dump _ -> return ()
        _ -> outputEvent te $ Text $ show am

modifyString :: Channel -> (PerString -> PerString) -> AllocM ()
modifyString ch f = modify (\s -> s { asStrings = Map.adjust f ch $ asStrings s })

setPlayState :: Channel -> PlayState -> AllocM ()
setPlayState ch ps = modifyString ch (\s -> s { psState = ps })

addNoteOff :: (Key, Maybe Channel) -> OffAction -> AllocM ()
addNoteOff kc act = modify (\s -> s { asOffActions = asOffActions s ++ [(kc, act)] })

removeNoteOff :: ((Key, Maybe Channel) -> Bool) -> AllocM (Maybe OffAction)
removeNoteOff p = do
    es <- gets asOffActions
    let (as, bs) = break (p . fst) es
    case bs of
        [] -> return Nothing
        ((_, act) : cs) -> do
            modify (\s -> s { asOffActions = as ++ cs })
            return $ Just act

onNoteOffPlaying :: Key -> Channel -> OffAction -> AllocM ()
onNoteOffPlaying k c = addNoteOff (k, Just c)

onNoteOffStolen :: Key -> Channel -> OffAction -> AllocM ()
onNoteOffStolen k c act = do
    es <- gets asOffActions
    let (as, bs) = break ((== (k, Just c)) . fst) es
    case bs of
        [] -> return ()
        (_ : cs) -> do
            modify (\s -> s { asOffActions = as ++ ((k, Nothing), act) : cs })

onNoteOffMisc :: Key -> OffAction -> AllocM ()
onNoteOffMisc k = addNoteOff (k, Nothing)

handleNoteOff :: Time -> Key -> Velocity -> AllocM ()
handleNoteOff te key vel = do
    mAct <- removeNoteOff ((== key) . fst)
    case mAct of
        Nothing -> outputMessage te $ UnmatchedNoteOff key
        Just act -> act te vel

dumpState :: Time -> AllocM ()
dumpState te = gets asStrings >>= outputMessage te . Dump . showStates
  where
    showStates = intercalate " / " . map (showState . psState . snd) . Map.toAscList
    showState Free = "Free"
    showState (Playing f tOn k) = printf "Playing %d(%d) %.3f" k f tOn
    showState (Played f tOn k tOff _) = printf "Played %d(%d) %.3f -> %.3f" k f tOn tOff

data Allocation = Unavailable
                | Steal Time        -- length of shortend, stolen note
                | Available
    deriving (Eq, Ord, Show)



-- | Allocate notes to channels 0..3.
-- Assumes well formed note events, w/o prepositioning events.
allocator :: Track Time -> (AllocMessages, Track Time)
allocator trk = postProcess $ execState run initialAllocatorState
  where
    run = mapM_ go trk >> flush
    postProcess as = (reverse $ asMessages as, sortOnTime $ asEvents as)
    sortOnTime = map snd . sortBy (compare `on` fst) . map withOrder
    withOrder ev@(te,msg) = ((te,messageOrder msg),ev)

    go' ev@(te, _) = dumpState te >> go ev

    go (te, NoteOn _ key vel)   = allocateNoteOn te key vel
    go (te, NoteOff _ key vel)  = handleNoteOff te key vel
    go (te, ev)                 = outputEvent te ev

    flush = gets asStrings >>= mapM_ flushEvent . Map.toAscList
    flushEvent (ch, Ps { psState = Played _ _ key tOff vOff }) =
        outputEvent tOff (NoteOff ch key vOff)
    flushEvent _ = return ()

allocateNoteOn :: Time -> Key -> Velocity -> AllocM ()
allocateNoteOn te key vel = gets asStrings >>= pickBest . findOptions
  where
    findOptions = map optionKey . Map.toAscList
    pickBest = snd . maximumBy (compare `on` fst)


    optionKey a@(_, ps) = maybe unavailable (optionFret a)
                            $ fretForKey (psString ps) key

    optionFret (ch, ps) f1 = case psState ps of
        Free -> (Available, playNote ch (te - shifterMaxTime f1) f1)

        Playing f0 tOn k -> case te - shifterTime f0 f1 of
            ts | tOn < ts -> (Steal (ts - tOn), do
                    onNoteOffStolen k ch $ noteOff ch tOn k ts
                    playNote ch ts f1
                    )
               | otherwise -> unavailable

        Played f0 tOn k tOff vOff -> case te - shifterTime f0 f1 of
            ts | tOff <= ts -> (Available, do
                    noteOff ch tOn k tOff tOff vOff
                    playNote ch ts f1
                    )
               | tOn <= ts -> (Steal (tOn - ts), do
                    noteOff ch tOn k ts tOff vOff
                    playNote ch ts f1
                    )
               | otherwise -> unavailable

    unavailable = (Unavailable, do
                    outputMessage te $ Unplayable key
                    onNoteOffMisc key (\_ _ -> return ())
                    )

    playNote ch ts f1 = do
        when (ts < te) $
            outputEvent ts (NoteOn ch key 1)    -- the preposition message
        outputEvent te (NoteOn ch key vel)  -- the actual note on
        setPlayState ch (Playing f1 te key) -- note that string is now playing
        onNoteOffPlaying key ch (\tOff vOff -> setPlayState ch (Played f1 te key tOff vOff))

    noteOff ch tOn k ts tOff vOff = do
        when (ts < tOff) $
            outputMessage ts $ StolenNote ch k (tOff - tOn) (ts - tOn)
        outputEvent ts (NoteOff ch k vOff)

-- | Recombine allocated notes back onto one channel.
-- Prepositioning events (NoteOn w/ velocity 1) are stripped.
recombine :: Channel -> Track Time -> Track Time
recombine rch = mapMaybe adjust
  where
    adjust e@(t, NoteOn ch k v)
        | not (ch < 4)  = Just e
        | v <= 1        = Nothing
        | otherwise     = Just (t, NoteOn rch k v)
    adjust e@(t, msg)
        | isChannelMessage msg && channel msg < 4
                        = Just (t, msg { channel = rch })
        | otherwise     = Just e

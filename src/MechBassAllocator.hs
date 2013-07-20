module MechBassAllocator
    ( AllocMessages
    , allocator
    , recombine
    )
where

{-
Notes composed for the MechBass must be "allocated" to strings, as each string
has its own channel. Allocation takes into account which strings are able to
play a given pitch, and whether the string is already sounding another note.

A further complication is that the MechBass needs additional pre-positioning
messages (notes with velocity 1) inserted prior to the onset of the note, lest
the attack be delayed as the fret mechanism is positioned to the correct place
for the pitch. The allocator tracks the position of all the fret shifters, and
uses the timing information (see 'shifterTimes' in "MechBass") to pre-position
in time for a note's attack.

In general, notes on strings may have to be "stolen" to play a later note. Notes
that have been sounding longer are stolen first when there is a choice, as they
will have decayed the most (ignoring any compressor effects that may be later
applied). Because of this feature, the allocator state must maintain NoteOff
messages that should be eventually emitted but are held back lest a later note
need to move the NoteOff up in time.

Sometimes there is no available string to play an otherwise playable note. In
this case it is simply dropped. Dropped notes and stolen notes are noted in the
output messages.

TODO: The current allocator doesn't attempt to find an optimal allocation.
When there are multiple options for note placement, it is possible that choosing
one of several equal options, or even a less desirable option may later result
in a better overall result.
-}

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

-- | The state of string from the view point of the allocator.
-- `Free` and `Playing` are straight forward. `Played` keeps track of the last
-- note played, because the final NoteOff isn't output until the next note
-- allocated to this string. This is so that pre-positioning for the next note
-- can shorten the note if needed.
data PlayState
    = Free
    | Playing Fret Time Key
    | Played Fret Time Key Time Velocity

-- | Information kept by the allocator per string.
data PerString = Ps { psState :: PlayState
                    , psString :: BassString
                    }

-- | Index into list of actions to perform on NoteOff.
-- The Channel, Key tuple is the channel and key of the note from the source
-- track. The Maybe Channel is the allocated channel that the note is playing
-- on. It is Nothing for stolen and dropped notes.
type OffIndex = ((Channel, Key), Maybe Channel)

-- | What to do when we a NoteOff is processed
type OffAction = Time -> Velocity -> AllocM ()

-- \ The full state of the allocator.
data AllocState =
    As { asEvents :: Track Time
            -- ^ the allocated events, unsorted
       , asStrings :: Map.Map Int PerString
            -- ^ state per string, indexed by channel 0..3
       , asOffActions :: [(OffIndex, OffAction)]
            -- ^ how to handle a NoteOff
       , asMessages :: AllocMessages
            -- ^ allocation messages, in reverse order
       }

initialAllocatorState :: AllocState
initialAllocatorState =
    As { asEvents = []
       , asStrings = Map.fromList $ zip bassChannels $ map (Ps Free) bassStrings
       , asOffActions = []
       , asMessages = []
       }

data AllocMsg = Unplayable Key
              | UnmatchedNoteOff Key
              | StolenNote Channel Key Time Time
                    -- ^ times are original note length, and shortened length
              | Dump String
                    -- ^ used for debugging the allocator
    deriving (Eq)

instance Show AllocMsg where
    show (Unplayable k) = "Unplayable " ++ show k
    show (UnmatchedNoteOff k) = "UnmatchedNoteOff " ++ show k
    show (StolenNote ch k tOrig tShort) =
        printf "Stolen Note %d %d : %.3f -> %.3f" ch k tOrig tShort
    show (Dump s) = "Dump " ++ s

type AllocMessages = [(Time, AllocMsg)]


{-
    The allocation state monad, and utility functions for manipulating
    the state safely.
-}

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

addNoteOff :: OffIndex -> OffAction -> AllocM ()
addNoteOff i act = modify (\s -> s { asOffActions = asOffActions s ++ [(i, act)] })

removeNoteOff :: (OffIndex -> Bool) -> AllocM (Maybe OffAction)
removeNoteOff p = do
    es <- gets asOffActions
    let (as, bs) = break (p . fst) es
    case bs of
        [] -> return Nothing
        ((_, act) : cs) -> do
            modify (\s -> s { asOffActions = as ++ cs })
            return $ Just act

onNoteOffPlaying :: Channel -> Key -> Channel -> OffAction -> AllocM ()
onNoteOffPlaying ct k cp = addNoteOff ((ct, k), Just cp)

onNoteOffStolen :: Channel -> Key -> Channel -> OffAction -> AllocM ()
onNoteOffStolen ct k cp act = do
    es <- gets asOffActions
    let (as, bs) = break ((== ((ct, k), Just cp)) . fst) es
    case bs of
        [] -> return ()
        (_ : cs) -> do
            modify (\s -> s { asOffActions = as ++ (((ct, k), Nothing), act) : cs })

onNoteOffMisc :: Channel -> Key -> OffAction -> AllocM ()
onNoteOffMisc ct k = addNoteOff ((ct, k), Nothing)

handleNoteOff :: Time -> Channel -> Key -> Velocity -> AllocM ()
handleNoteOff te ct key vel = do
    mAct <- removeNoteOff ((== (ct, key)) . fst)
    case mAct of
        Nothing -> outputMessage te $ UnmatchedNoteOff key
        Just act -> act te vel

-- | Generate a debug message showing the state of the string allocation.
dumpState :: Time -> AllocM ()
dumpState te = gets asStrings >>= outputMessage te . Dump . showStates
  where
    showStates = intercalate " / " . map (showState . psState . snd) . Map.toAscList
    showState Free = "Free"
    showState (Playing f tOn k) = printf "Playing %d(%d) %.3f" k f tOn
    showState (Played f tOn k tOff _) = printf "Played %d(%d) %.3f -> %.3f" k f tOn tOff


-- | A possible allocation decision. One of these is determined for each string
-- for a given NoteOn. The best (maximum) is choosen. It is important, therefore
-- that the Ord instance (derived) orders these decisions in least to most
-- desirable order.
data Allocation = Unavailable
                | Steal Time        -- length of shortend, stolen note
                | Available
    deriving (Eq, Ord, Show)



-- | Allocate notes to channels 0..3.
-- Assumes well-formed note events, w/o prepositioning events.
-- Events on channels 0..3 will only be allocated on the respective strings.
-- Other events will be allocated on any available string.
allocator :: Track Time -> (AllocMessages, Track Time)
allocator trk = postProcess $ execState run initialAllocatorState
  where
    run = mapM_ go preProcess >> flush
            -- replace go with go' for debugging output
    preProcess = sortOnTime trk
    postProcess as = (reverse $ asMessages as, sortOnTime $ asEvents as)
    sortOnTime = map snd . sortBy (compare `on` fst) . map withOrder
    withOrder ev@(te,msg) = ((te,messageOrder msg),ev)

    go' ev@(te, _) = dumpState te >> go ev

    go (te, NoteOn ch key vel)  = allocateNoteOn te ch key vel
    go (te, NoteOff ch key vel) = handleNoteOff te ch key vel
    go (te, ev)                 = outputEvent te ev

    flush = gets asStrings >>= mapM_ flushEvent . Map.toAscList
    flushEvent (ch, Ps { psState = Played _ _ key tOff vOff }) =
        outputEvent tOff (NoteOff ch key vOff)
    flushEvent _ = return ()

allocateNoteOn :: Time -> Channel -> Key -> Velocity -> AllocM ()
allocateNoteOn te chOrig key vel = do
    strings <- gets asStrings
    let opts = case Map.lookup chOrig strings of
                    Just ps -> [(chOrig, ps)]
                    Nothing -> Map.toAscList strings
    pickBest $ map optionKey opts
  where
    pickBest = snd . maximumBy (compare `on` fst)

    optionKey :: (Channel, PerString) -> (Allocation, AllocM ())
    optionKey a@(_, ps) = maybe unavailable (optionFret a)
                            $ fretForKey (psString ps) key

    optionFret :: (Channel, PerString) -> Fret -> (Allocation, AllocM ())
    optionFret (ch, ps) f1 = case psState ps of
        Free -> (Available, playNote ch (te - shifterMaxTime f1) f1)

        Playing f0 tOn k -> case te - shifterTime f0 f1 of
            ts | tOn < ts -> (Steal (ts - tOn), do
                    onNoteOffStolen chOrig k ch $ noteOff ch tOn k ts
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
                    onNoteOffMisc chOrig key (\_ _ -> return ())
                    )

    playNote ch ts f1 = do
        when (ts < te) $ do
            outputEvent ts (NoteOn ch key 1)    -- the preposition message
            outputEvent te (NoteOff ch key 1)
        outputEvent te (NoteOn ch key vel)  -- the actual note on
        setPlayState ch (Playing f1 te key) -- note that string is now playing
        onNoteOffPlaying chOrig key ch
            (\tOff vOff -> setPlayState ch (Played f1 te key tOff vOff))

    noteOff ch tOn k ts tOff vOff = do
        when (ts < tOff) $
            outputMessage ts $ StolenNote ch k (tOff - tOn) (ts - tOn)
        outputEvent ts (NoteOff ch k vOff)

-- | Recombine allocated notes back onto one channel. Used to produce a version
-- that can be played with a bass synth, but plays as the MechBass will.
--
-- Prepositioning events (NoteOn/Off w/ velocity 1) are stripped.
recombine :: Channel -> Track a -> Track a
recombine rch = mapMaybe adjust
  where
    adjust e@(t, NoteOn ch k v)
        | not (ch < 4)  = Just e
        | v <= 1        = Nothing
        | otherwise     = Just (t, NoteOn rch k v)
    adjust e@(t, NoteOff ch k v)
        | not (ch < 4)  = Just e
        | v <= 1        = Nothing
        | otherwise     = Just (t, NoteOff rch k v)
    adjust e@(t, msg)
        | isChannelMessage msg && channel msg < 4
                        = Just (t, msg { channel = rch })
        | otherwise     = Just e

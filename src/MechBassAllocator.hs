module MechBassAllocator
    ( AllocMessages
    , allocator
   )
where

import Codec.Midi (Channel, Key, Message(..), Time, Track, Velocity)
import Control.Monad.Trans.State.Strict
import Data.Function (on)
import Data.List (maximumBy, sortBy)
import qualified Data.Map.Strict as Map

import MechBass
import MidiUtil (orderEvents)


data PlayState
    = Free
    | Playing Fret Time Key
    | Played Fret Time Key Time Velocity

data PerString = Ps { psState :: PlayState
                    , psString :: BassString
                    }

data AllocState = As { asEvents :: Track Time
                     , asStrings :: Map.Map Int PerString
                     , asOffActions :: Map.Map Key (Time -> Velocity -> AllocM ())
                     , asMessages :: AllocMessages
                     }

initialAllocatorState :: AllocState
initialAllocatorState =
    As { asEvents = []
       , asStrings = Map.fromList $ zip [0..] $ map (Ps Free) bassStrings
       , asOffActions = Map.empty
       , asMessages = []
       }

data AllocMsg = Unplayable Key
              | UnmatchedNoteOff Key
              | StolenNote Channel Key Time
    deriving (Eq, Show)

type AllocMessages = [(Time, AllocMsg)]


type AllocM = State AllocState

outputEvent :: Time -> Message -> AllocM ()
outputEvent te ev = modify (\s -> s { asEvents = (te,ev) : asEvents s })

outputMessage :: Time -> AllocMsg -> AllocM ()
outputMessage te am = do
    modify (\s -> s { asMessages = (te,am) : asMessages s })
    outputEvent te $ Text $ show am

modifyString :: Channel -> (PerString -> PerString) -> AllocM ()
modifyString ch f = modify (\s -> s { asStrings = Map.adjust f ch $ asStrings s })

setPlayState :: Channel -> PlayState -> AllocM ()
setPlayState ch ps = modifyString ch (\s -> s { psState = ps })

onNoteOff :: Key -> (Time -> Velocity -> AllocM ()) -> AllocM ()
onNoteOff key act = modify (\s -> s { asOffActions = Map.insert key act $ asOffActions s })

dropNoteOff :: Key -> AllocM ()
dropNoteOff key = modify (\s -> s { asOffActions = Map.delete key $ asOffActions s })


data Allocation = Unavailable
                | Available
                | Steal Time        -- length of shortend, stolen note
    deriving (Eq, Ord, Show)



-- | Allocate notes to channels 0..3.
-- Assumes well formed note events, w/o prepositioning events.
allocator :: Track Time -> (AllocMessages, Track Time)
allocator trk = postProcess $ execState (mapM_ go trk) initialAllocatorState
  where
    postProcess as = (reverse $ asMessages as, sortOnTime $ asEvents as)
    sortOnTime = sortBy orderEvents

    go (te, NoteOn _ key vel) = allocateNoteOn te key vel

    go (te, NoteOff _ key vel) = do
        acts <- gets asOffActions
        case Map.lookup key acts of
            Nothing -> outputMessage te $ UnmatchedNoteOff key
            Just act -> dropNoteOff key >> act te vel

    go (te, ev) = outputEvent te ev


allocateNoteOn :: Time -> Key -> Velocity -> AllocM ()
allocateNoteOn te key vel = gets asStrings >>= pickBest . findOptions
  where
    findOptions = map optionKey . Map.toList
    pickBest = snd . maximumBy (compare `on` fst)


    optionKey a@(_, ps) = maybe unavailable (optionFret a)
                            $ fretForKey (psString ps) key

    optionFret (ch, ps) f1 = case psState ps of
        Free -> (Available, playNote ch (te - shifterMaxTime f1) f1)

        Playing f0 tOn k -> case te - shifterTime f0 f1 of
            ts | tOn < ts -> (Steal (ts - tOn), do
                    outputMessage ts $ StolenNote ch k tOn
                    onNoteOff key (\_ vOff -> outputEvent ts (NoteOff ch key vOff))
                    playNote ch ts f1
                    )
               | otherwise -> unavailable

        Played f0 tOn k tOff vOff -> case te - shifterTime f0 f1 of
            ts | tOff <= ts -> (Available, do
                     outputEvent tOff (NoteOff ch k vOff)
                     playNote ch ts f1
                     )
               | tOn <= ts -> (Steal (tOn - ts), do
                    outputMessage ts $ StolenNote ch k tOn
                    outputEvent ts (NoteOff ch k vOff)
                    playNote ch ts f1
                    )
               | otherwise -> unavailable

    unavailable = (Unavailable, outputMessage te $ Unplayable key)

    playNote ch ts f1 = do
        outputEvent ts (NoteOn ch key 1)    -- the preposition message
        outputEvent te (NoteOn ch key vel)  -- the actual note on
        setPlayState ch (Playing f1 te key) -- note that string is now playing
        onNoteOff key (\tOff vOff -> setPlayState ch (Played f1 te key tOff vOff))



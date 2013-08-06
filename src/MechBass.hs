module MechBass
    ( BassString()
    , bassStrings
    , validate
    , AllocMessages
    , allocator
    )
where

import Codec.Midi (Key, Time)
import qualified Codec.Midi as M
import Data.List (mapAccumL)
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Map.Strict as Map

import MidiUtil

data BassString = BassString { stringLoKey, stringHiKey :: !Key }

bassStrings :: [BassString] -- lowest pitch
bassStrings = map (\lo -> BassString lo (lo + numFrets - 1)) notes
  where
    notes = [ 28, 33, 38, 43 ] -- (E,2), ((A,2), (D,3), (G,3)
    numFrets = 13

eString, aString, dString, gString :: BassString
[eString, aString, dString, gString] = bassStrings


type Fret = Int             -- zero is lowest note on string

fretForKey :: BassString -> Key -> Maybe Fret
fretForKey bs k = if stringLoKey bs <= k && k <= stringHiKey bs
                    then Just (k - stringLoKey bs)
                    else Nothing


-- | Columns are "from fret", rows are "to fret"
rawShifterTimes :: [[Time]]
rawShifterTimes =
    [ [   0, 101, 156, 187, 215, 240, 265, 284, 301, 316, 332, 344, 357, 369 ]
    , [ 102,   0,  96, 145, 181, 209, 237, 257, 276, 292, 306, 321, 333, 345 ]
    , [ 156,  95,   0,  92, 144, 179, 202, 224, 244, 262, 278, 297, 310, 322 ]
    , [ 187, 146,  92,   0,  91, 135, 173, 200, 222, 240, 259, 270, 284, 296 ]
    , [ 215, 179, 145,  91,   0,  89, 136, 168, 194, 216, 235, 247, 261, 275 ]
    , [ 240, 209, 179, 136,  90,   0,  82, 132, 163, 187, 209, 222, 239, 254 ]
    , [ 266, 237, 202, 174, 137,  83,   0,  82, 123, 153, 177, 202, 219, 236 ]
    , [ 284, 257, 225, 200, 168, 132,  82,   0,  80, 119, 148, 177, 197, 213 ]
    , [ 302, 275, 244, 221, 194, 162, 123,  80,   0,  78, 115, 148, 171, 190 ]
    , [ 317, 291, 263, 242, 215, 188, 152, 119,  77,   0,  75, 108, 143, 166 ]
    , [ 331, 306, 278, 259, 234, 208, 177, 147, 115,  75,   0,  66, 103, 138 ]
    , [ 344, 321, 297, 269, 246, 222, 201, 176, 148, 108,  68,   0,  69, 104 ]
    , [ 357, 333, 310, 284, 261, 240, 220, 196, 170, 144, 104,  70,   0,  67 ]
    , [ 370, 345, 323, 296, 274, 253, 235, 214, 189, 165, 138, 104,  68,   0 ]
    ]

shifterTimes :: [[Time]]
shifterTimes = map (map (* 1.15)) rawShifterTimes

-- | Time it takes to shift from one fret to another
shifterTime :: Int -> Int -> Time
shifterTime startFret endFret = (shifterTimes !! endFret) !! startFret

-- | Maximum time it takes to shift to a fret
shifterMaxTime :: Int -> Time
shifterMaxTime endFret = maximum (shifterTimes !! endFret)



data StringState
    = Unknown                   -- no sound, location of shifter unknown
    | Damped Fret               -- no sound, shifter at fret
    | Shifting Fret Time Bool   -- moving to fret, arrives at given time
                                -- the bool indicates if pluck on arrival
    | Plucked Fret Time         -- sounding note, plucked at given time
  deriving (Eq, Show)

playingFret :: StringState -> Maybe Fret
playingFret (Shifting f _ True) = Just f
playingFret (Plucked f _) = Just f
playingFret _ = Nothing

updateState :: Time -> StringState -> StringState
updateState te (Shifting f t p) | t <= te = if p then Plucked f t else Damped f
updateState _ st = st

stringEvent :: BassString -> Time -> M.Message -> StringState -> StringState
stringEvent bs te ev s0 = case ev of
    M.NoteOn _ key vel | vel <= 1   -> preposition `onKey` key
                       | otherwise  -> startNote `onKey` key
    M.NoteOff _ key _               -> stopNote `onKey` key
    _ -> st
  where
    st = updateState te s0

    mod `onKey` key = maybe s0 mod $ fretForKey bs key

    preposition f1 = case st of
        Unknown         -> positionFromUnknown f1
        Damped f0       -> positionFrom f0 f1
        Shifting f0 _ _ -> if f0 == f1 then st else positionFromUnknown f1
        Plucked f0 _    -> positionFrom f0 f1

    positionFromUnknown f1 = Shifting f1 (te + shifterMaxTime f1) False
    positionFrom f0 f1 | f0 == f1  = Damped f1
                       | otherwise = Shifting f1 (te + shifterTime f0 f1) False

    startNote f1 = case preposition f1 of
        Unknown         -> error "preposition left string Unknown"
        Damped f        -> Plucked f te
        Plucked f _     -> Plucked f te
        Shifting f t _  -> Shifting f t True

    stopNote f1 = case st of
        Shifting f0 _ _ | f0 == f1 -> Unknown
        Plucked f0 _    | f0 == f1 -> Damped f1
        _ -> st





{-
Things to validate:

    Single String tracks
    1) all notes on playable range for that string
    2) only one note at a time
    3) note on, vel > 1, shifter finishes before end of note
    4) note on, vel 1, after note off
    5) first note assume maximal travel
-}


data MechBassError
    = NotMonophonic
    | BadNoteOff
    | KeyOutOfRange Key
    | StartsLate Time
    | FretTooLate Time
  deriving (Eq, Show)

validate :: BassString -> TrackChecker MechBassError
validate bs = go Unknown
  where
    go s0 ((te, ev):es) =
        let stBefore = updateState te s0
            stAfter = stringEvent bs te ev stBefore
            pFret = playingFret stBefore
        in do
            case ev of
                M.NoteOn _ key _ -> do
                    check te (isJust $ fretForKey bs key) $ KeyOutOfRange key
                    check te (isNothing pFret) $ NotMonophonic
                M.NoteOff _ key _ -> do
                    check te (isJust $ fretForKey bs key) $ KeyOutOfRange key
                    check te (pFret == fretForKey bs key) $ BadNoteOff
                _ -> okay
            go stAfter es
    go _ [] = okay

{-
positioner:
    1) compute time to shift from previous note
    2) if positioner message after previous note off, output it
    3) else if previous note off - shift time > reasonable note length
        3a) shorten previous note
        3b) output positiononer message
    4) else if previous note off earlier, output positioner after that
    5) else don't output positioner!

    position at
        thisStart - shiftTime
-}


data Allocation = Available Time | Steal Time Time
    deriving (Eq, Show)

data AllocMsg = Unplayable Key
              | UnmatchedNoteOff Key
              | StolenNote M.Channel Key Time
    deriving (Eq, Show)

instance Ord Allocation where
    compare (Available t) (Available v) = compare v t -- shorter is better
    compare (Available _) _ = GT
    compare _ (Available _) = LT
    compare (Steal s t) (Steal u v) = compare (s - t) (u - v) -- longer is better

data AllocState = AllocState { asKey :: Maybe Key
                             , asString :: BassString
                             , asState :: StringState
                             }

type AllocMessages = [(Time, AllocMsg)]

-- | Allocate notes to channels 0..3.
-- Assumes well formed note events, w/o prepositioning events.
allocator :: M.Track Time -> (AllocMessages, M.Track Time)
allocator = postProcess . mapAccumL go ([], initialAllocatorState)
  where
    initialAllocatorState = Map.fromList $ zip [0..]
        $ map (\bs -> AllocState Nothing bs Unknown) bassStrings
    postProcess ((msgs, _), mtrk) = (reverse msgs, catMaybes mtrk)

    go s@(_, as) (te, M.NoteOn _ key vel) =
        case pickBest $ findOptions key as of
            Nothing ->
                message s te (Unplayable key)
            Just (Available _, ch) ->
                output s te ch (M.NoteOn ch key vel) (Just key) Nothing
            Just (Steal tn ts, ch) ->
                output s te ch (M.NoteOn ch key vel) (Just key)
                    (Just (StolenNote ch key (tn - te - ts)))
    go s@(_, as) (te, M.NoteOff _ key vel) =
        case Map.toList $ Map.filter (\a -> asKey a == Just key) as of
            [] ->
                message s te (UnmatchedNoteOff key)
            ((ch, _):_) ->
                output s te ch (M.NoteOff ch key vel) Nothing Nothing
    go s teev = (s, Just teev)

    findOptions key = Map.mapMaybe (optionKey key)
    optionKey key as =
        optionFret (asState as) `fmap` fretForKey (asString as) key
    optionFret Unknown              f1 = Available (shifterMaxTime f1)
    optionFret (Damped f0)          f1 = Available (shifterTime f0 f1)
    optionFret (Shifting f0 t _)    f1 = Steal t (shifterTime f0 f1)
    optionFret (Plucked f0 t)       f1 = Steal t (shifterTime f0 f1)

    pickBest = Map.foldlWithKey (\a ch opt -> a `max` Just (opt, ch)) Nothing

    output (msgs, as) te ch ev k mm =
        let f a = a { asKey = k,
                      asState = stringEvent (asString a) te ev (asState a) }
            mf = maybe id (\m -> ((te, m):)) mm
        in ((mf msgs, Map.adjust f ch as), Just (te, ev))
    message (msgs, as) te m = (((te, m):msgs, as), Nothing)


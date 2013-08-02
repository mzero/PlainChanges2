module MechBass where

import Codec.Midi (Key, Time)

data BassString = BassString { stringLoKey, stringHiKey :: !Key }

bassStrings :: [BassString] -- lowest pitch
bassStrings = map (\lo -> BassString lo (lo + numFrets - 1)) notes
  where
    notes = [ 28, 33, 38, 43 ] -- (E,2), ((A,2), (D,3), (G,3)
    numFrets = 13

eString, aString, dString, gString :: BassString
[eString, aString, dString, gString] = bassStrings


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

shifterTime :: Int -> Int -> Time
shifterTime startFret endFret = (rawShifterTimes !! endFret) !! startFret


{-
Things to validate:

    Single String tracks
    1) all notes on playable range for that string
    2) only one note at a time
    3) note on, vel > 1, shifter finishes before end of note
    4) note on, vel 1, after note off
    5) first note assume maximal travel
-}

type Fret = Int

data ShifterState = ShifterUnknown | ShifterAt Fret | ShifterMoving Fret Time
    deriving (Eq)

data SoundState = SoundDamped | SoundPlucked Time Time

type StringState = (ShifterState, SoundState)

{-
validate :: String -> M.Track M.Time -> CheckResult
validate (String lo hi) = execChecker . go initialState
  where
    lo = stringLoKey st
    hi = stringHiKey st
    initialState = (ShifterUnknown, SoundDamped)

    go (sh, so) ((t, M.NoteOn _ key _):es) = do
        check t (lo <= key) "not too low"
        check t (key <= hi) "not too high"
        go (sh, so) es
-}

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


{-
allocator
-}




module SimplePeal where

import Codec.Midi
import Euterpea
import Euterpea.IO.MIDI

import Changes

pitches :: [Pitch]
pitches = reverse $ do
    o <- [3..7]
    [(Fs, o - 1), (Gs, o - 1), (As, o - 1), (Cs, o), (Ds, o)]

durations :: [Dur]
durations = do
    m <- [1, 3/2, 4/3]
    map (*m) [wn, hn, qn, en, sn]

ring :: [Pitch] -> Dur -> Music Pitch
ring bells d = line $ zipWith note (cycle durs) (concat $ ringOf bells ++ [bells])
  where
    durs = durs0 ++ durs1
    durs0 = map (const d) bells
    durs1 = drop 1 durs0 ++ [2*d]

p0 :: Music Pitch
p0 = ring (take 4 $ drop 10 pitches) en

playPeal :: Int -> Int -> Dur -> Dur -> Rational -> Rational -> Rational -> Music Pitch
playPeal n pitchOffset start duration amplitude _location _reverb =
    delayM start $ phrase [volume] $ ring bells beat
  where
    volume = Dyn $ Loudness $ 80 * amplitude + 40
    bells = reverse $ take n $ drop pitchOffset $ reverse pitches
    beat = duration / numNotes
    numNotes = fromIntegral $ nFac * n + (nFac `div` 2) + n
    nFac = product [1..n]

pealSet :: [Music Pitch] -> Music Pitch
pealSet = tempo ((wn  / qn) * (60 / 120)) . chord

comp0 :: Music Pitch
comp0 = pealSet
    [ playPeal 3   5   0.0  30.0  0.4  45.0  0.35
    , playPeal 2  10   2.0   2.0  0.7   0.0  0.00
    , playPeal 2  10   4.0   1.0  0.6  10.0  0.30
    , playPeal 2  11   4.1   0.9  0.6  10.0  0.30
    , playPeal 4  15   6.8   3.2  0.1   0.0  0.00
    , playPeal 3  15   9.0   6.0  0.2  90.0  0.30
    , playPeal 3  10  13.0   2.0  0.3  35.0  0.10
    , playPeal 5   7  14.9  15.1  0.4  35.0  0.05
    , playPeal 2   8  20.0   3.0  0.6  90.0  0.40
    , playPeal 2  13  20.5   3.0  0.3  85.0  0.00
    ]

comp1bass, comp1main :: [Music Pitch]
comp1bass =
    [ playPeal 5   5   0.0  90.0  0.2   0.0  0.20
    ]
comp1main =
    [ playPeal 2  10   2.0   2.0  0.7  90.0  0.00
    , playPeal 2  10   5.0   1.0  0.6  80.0  0.15
    , playPeal 2  11   5.1   0.9  0.6  80.0  0.15
    , playPeal 3  15   9.0   6.0  0.4  45.0  0.30
    , playPeal 3  14  14.0   6.0  0.4  90.0  0.25
    , playPeal 3  13  19.0   6.0  0.4  45.0  0.20
    , playPeal 3  12  24.0   6.0  0.4  90.0  0.15
    , playPeal 2  15  30.5   1.0  0.1  35.0  0.00
    , playPeal 2  15  30.6   1.0  0.1  35.0  0.00
    , playPeal 2  15  30.7   1.0  0.1  35.0  0.00
    , playPeal 4  15  30.8  24.0  0.1  35.0  0.00
    , playPeal 3   5  41.0   5.0  0.4  90.0  0.20
    , playPeal 3   6  47.5   5.0  0.3  90.0  0.25
    , playPeal 3   5  54.0   5.5  0.3  90.0  0.25
    , playPeal 2  15  70.0  12.0  0.2  75.0  0.10
    , playPeal 2  15  75.0  12.0  0.2  75.0  0.10
    , playPeal 2  15  85.0  12.0  0.2  75.0  0.10
    ]

testPeals =
    [ playPeal 3   0   0.0   5.0  0.8  90.0  0.20
    , playPeal 3   4   5.0   5.0  0.8  90.0  0.20
    , playPeal 3   8  10.0   5.0  0.8  90.0  0.20
    , playPeal 3  12  15.0   5.0  0.8  90.0  0.20
    , playPeal 3  16  20.0   5.0  0.8  90.0  0.20
    , playPeal 3  20  25.0   5.0  0.8  90.0  0.20
    ]

comp1 :: Music Pitch
comp1 = pealSet $ comp1bass ++ comp1main

playOnBells :: Performable a => Music a -> IO ()
playOnBells = play . instrument Vibraphone

midiFileBells :: Performable a => FilePath -> Music a -> IO ()
midiFileBells f = exportFile f . testMidi . delayM wn . instrument Vibraphone

comp1split :: Music Pitch
comp1split = (instrument ElectricBassPicked $ pealSet comp1bass)
         :=: (instrument Vibraphone $ pealSet comp1main)

splitPatchMap :: UserPatchMap
splitPatchMap = [(Vibraphone, 1), (ElectricBassPicked, 0)]

splitMidi :: Performable a => Music a -> Midi
splitMidi m = toMidi (defToPerf m) splitPatchMap

playTest :: Int -> IO ()
playTest i = playMidi i $ splitMidi $ instrument Vibraphone $ pealSet testPeals


module PartII (partII) where

import Euterpea

import Changes
import Elements

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Part II
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

fMinorScale :: [PitchClass]
fMinorScale = [F, G, Af, Bf, C, Df, Ef, F]


b1pitches, b2pitches, b3pitches, b4pitches :: [Pitch]
b1pitches = [(E,  3), (Df, 3), (Bf, 2), (G, 2)]
b2pitches = [(C,  4), (Af, 3), (F,  3), (C, 3)]
b3pitches = [(Df, 4), (C,  4), (Af, 3), (E, 3)]
b4pitches = [(F,  4), (Ef, 4), (C, 4), (G, 3)]

p2Ostinado :: Music Pitch
p2Ostinado = onBass
    $   riff  0   0    0 b1pitches
    :=: riff  4   0 (-2) b2pitches
    :=: riff  8   0 (-1) b3pitches
    :=: riff 10   4 (-3) b4pitches
  where
    riff a b c ps = delayM (a*9*qn + b*qn + c*sn) $ ringNotes qn ps

p2OnCoil :: Music Pitch -> Music Pitch
p2OnCoil = onCoil . phrase [Art $ Staccato $ 1/4]

p2coil :: Music Pitch
p2coil = p2OnCoil
    $   riff  2 [(Af, 5), (F,  5), (C, 5)]
    :=: riff  3 [(C,  6), (Af, 5), (E, 5)]
    :=: riff  7 [(Ef, 6), (C, 6), (G, 5)]
  where
    riff a ps = delayM (a*9*qn) $ ringNotes (sn/2) ps

p2coilAccentB3 :: Music Pitch
p2coilAccentB3 = p2OnCoil $ delayM (8*9*qn)
    $ chord (map acc [4, 6, 10, 11, 12])
  where
    acc n = delayM (fromIntegral n*9*qn) $ chord $
        zipWith accLine [0..] $ take 8 $ drop (n * 8) b3line
    accLine m (p,o) = delayM (m*qn) $ ringNotes (sn) [(p,o+2)]
    b3line = concat $ ringOf b3pitches ++ [b3pitches]

p2tempo :: Music a -> Music a
p2tempo m = tempo (startTempo / 120) mAll
  where
    startTempo = 70
    endTempo = 85
    accl = (endTempo - startTempo) / endTempo
    firstDur = 6 * 9 * qn
    mFirst = removeZeros $ takeM firstDur m
    mRest = removeZeros $ dropM firstDur m
    mAll = mFirst :+: phrase [Tmp $ Accelerando accl] mRest

interruptionB :: Music Pitch
interruptionB = tempo (170 / 120) $ v :=: b :=: c :=: d
  where
    t = en
    notes oct = ringNotes t [(C, oct), (G, oct-1)]
    v = onVoice $ notes 5
    b = onBass $ notes 4
    c = onCoil $ notes 5
    d = onDrums $ ringPerc t [AcousticSnare, LowTom]

partII :: Music Pitch
partII = (p2tempo $ p2Ostinado :=: p2coil :=: p2coilAccentB3) -- :+: interruptionB

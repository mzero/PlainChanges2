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
b2pitches = [(Bf,  3), (Af, 3), (F,  3), (C, 3)]
b3pitches = [(Df, 4), (Bf,  3), (Af, 3), (E, 3)]
b4pitches = [(F,  4), (Ef, 4), (C, 4), (G, 3)]

r4phraseDur :: Dur
r4phraseDur = 9*qn

ostinado :: Music Pitch
ostinado = bassMF $
        (onBassEString $ riff  0   0    0 b1pitches)
    :=: (onBassAString $ riff  4   0 (-2) b2pitches)
    :=: (onBassDString $ riff  8   0 (-1) b3pitches)
    :=: (onBassGString $ riff 10   4 (-3) b4pitches)
  where
    riff a b c ps = delayM (a*r4phraseDur + b*qn + c*sn) $ ringNotes qn ps


coil :: Music Pitch
coil = onCoilShort
    $   riff  2 [(Af, 5), (F,  5), (C, 5)]
    :=: riff  3 [(C,  6), (Af, 5), (E, 5)]
    :=: riff  7 [(Ef, 6), (C, 6), (G, 5)]
  where
    riff a ps = delayM (a*r4phraseDur) $ ringNotes (sn/2) ps

coilAccentB3 :: Music Pitch
coilAccentB3 = onCoilShort $ delayM (8*r4phraseDur)
    $ chord (map acc [4, 6, 7, 10, 11, 12])
  where
    acc n = delayM (fromIntegral n*r4phraseDur) $ chord $
        zipWith accLine [0..] $ take 8 $ drop (n * 8) b3line
    accLine m (p,o) = delayM (m*qn) $ ringNotes (sn) [(p,o+2)]
    b3line = concat $ ringOf b3pitches ++ [b3pitches]

percA :: Music Pitch
percA = onDrums $
    vol FF slowPerc
    :+: timesM 3 (vol MP slowPerc)
  where
    slowPerc = ringCymbal qn 2

ringCymbal :: Dur -> Int -> Music Pitch
ringCymbal d n = ringPerc d $ replicate n RideCymbal2

vol :: StdLoudness -> Music a -> Music a
vol d = phrase [Dyn $ StdLoudness d]

percB :: Music Pitch
percB = onDrums $ delayM (4*r4phraseDur - qn) $
    vol MP (ringCymbal sn 4)
    :+: rest (11*sn)
    :+: phrase [Dyn (Diminuendo 0.5)] (vol MP $ ringCymbal sn 4)

percC :: Music Pitch
percC = onDrums $ delayM (810*sn) rollAndCrash

rollAndCrash :: Music Pitch
rollAndCrash = holdLast $
     phrase [Tmp $ Accelerando 0.20, Dyn $ Crescendo 2, Dyn $ StdLoudness PPP]
     (timesM 32 (perc RideCymbal2 (sn/2) )) :+: perc RideCymbal2 dqn

p2tempo :: Music a -> Music a
p2tempo m = tempo (startTempo / 120) mAll
  where
    startTempo = 70
    endTempo = 80
    accl = (endTempo - startTempo) / endTempo
    firstDur = 6 * r4phraseDur
    mFirst = removeZeros $ takeM firstDur m
    mRest = removeZeros $ dropM firstDur m
    mAll = mFirst :+: phrase [Tmp $ Accelerando accl] mRest


partII :: Music Pitch
partII = p2tempo $ percA :=: delayM (5*qn)
        (ostinado :=: coil :=: coilAccentB3 :=: percB :=: percC)

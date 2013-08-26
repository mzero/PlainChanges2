module PartIII ( partIII ) where

import Euterpea

import Elements

-- [] Consider dropping the whole part to GMajor, so that the Coil part can
--    be raised an octave

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Part III
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
bFlatMajorScale :: [PitchClass]
bFlatMajorScale = [Bf, C, D, Ef, F, G, A, Bf]


p3Ostinado :: Music Pitch
p3Ostinado = onBass $ ringNotes en [(Bf, 3), (F, 3), (D, 3), (C, 3), (Bf, 2)]

p3r2, p3r2', p3r3, p3r4 :: Music Pitch
p3r2 = ringNotes qn [(Bf, 5), (F, 5)]
p3r2' = ringNotes qn [(F, 5), (D, 5)]
p3r3 = ringNotes qn [(D, 5), (C, 5), (Bf, 4)]
p3r4 = ringNotes en [(Bf, 4), (F, 4), (D, 4), (C, 4)]

p3r2dur, p3r3dur, p3r4dur :: Dur
p3r2dur = dur p3r2
p3r3dur = dur p3r3
p3r4dur = dur p3r4

p3OstinadoPhraseDur :: Dur
p3OstinadoPhraseDur = 11 * qn

p3SetA :: Music Pitch
p3SetA = onCoil1516 $ chord
    [ timesM 10 $ p3r2
    , delayM (2 * p3r2dur) $ timesM 2 p3r3
    , delayM (2 * p3r2dur + p3r3dur) p3r4
    ]

p3SetB :: Music Pitch
p3SetB = onBells (p3r2 :+: rest p3r2dur :+: p3r2' :+: rest p3r2dur :+: p3r2) :+: chord
    [ onBells $ p3r3 :+: p3r3 :+: p3r4
    , onCoil1516 p3r4
    ]

p3SetC :: Music Pitch
p3SetC = chord
    [ onBells $ timesM 8 final
    , onCoil1516 $ chord $ zipWith cCycle [5,3,1] [0,12,24]
        -- N.B.: The high (Bf,6) of the very last cycle is too high for the
        -- coil - it just gets dropped... oh well!
    ]
  where
    final = line $ map (note qn) [(Bf, 4), (F, 4), (D, 4), (C, 4), (Bf, 3)]
    cCycle n t =
        delayM ((8-fromIntegral n)*dur final) $ timesM n $ transpose t final

partIII :: Music Pitch
partIII = tempo (160/120) $
    p3Ostinado
    :=: delayM ( 1*p3OstinadoPhraseDur) p3SetA
    :=: delayM (12*p3OstinadoPhraseDur) (p3SetB :+: p3SetC)

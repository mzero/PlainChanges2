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
p3Ostinado = onBass $ holdLast $ bassMF $
    ringNotes en [(Bf, 3), (F, 3), (D, 3), (C, 3), (Bf, 2)]

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

coilAndPad :: Music Pitch -> Music Pitch
coilAndPad m = onCoilLong m :=: onPad m

p3SetA :: Music Pitch
p3SetA = coilAndPad $ chord
    [ timesM 10 $ p3r2
    , delayM (2 * p3r2dur) $ timesM 2 p3r3
    , delayM (2 * p3r2dur + p3r3dur) p3r4
    ]

p3SetB :: Music Pitch
p3SetB = onBells (p3r2 :+: rest p3r2dur :+: p3r2' :+: rest p3r2dur :+: p3r2) :+: chord
    [ onBells $ p3r3 :+: p3r3 :+: p3r4
    , coilAndPad p3r4
    , delayM (2 * p3r3dur) $ onCoilShort coilOrnaments
    ]
  where
    coilOrnaments = chord $ map (\(n,m) -> delayM (n*9*en) m)
        [ (5, ringNotes en [(A,4)])
        , (6, ringNotes en [(G,4)] :+: ringNotes en [(Ef,4)])
        , (7, ringNotes sn [(A,4), (G,4), (Ef,4)])
        , (9, ringNotes sn [(C,5), (D,4), (G,5)] :+: ringNotes en [(C,6)] :+: ringNotes en [(F,6)])
        , (12, ringNotes sn [(F,6), (D,6), (C,6)])
        ]
p3SetC :: Music Pitch
p3SetC = chord
    [ onBells $ holdLast $ timesM 7 final
    , coilAndPad $ chord $ zipWith cCycle [5,3,1] [0,12,24]
        -- N.B.: The high (Bf,6) of the very last cycle is too high for the
        -- coil - it just gets dropped... oh well!
    ]
  where
    final = line $ map (note qn) [(Bf, 4), (F, 4), (D, 4), (C, 4), (Bf, 3)]
    cCycle n t =
        delayM ((7-fromIntegral n)*dur final) $ timesM n $ transpose t final

partIII :: Music Pitch
partIII = tempo (160/120) $
    p3Ostinado
    :=: delayM ( 1*p3OstinadoPhraseDur) p3SetA
    :=: delayM (12*p3OstinadoPhraseDur) (p3SetB :+: p3SetC)

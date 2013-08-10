module PartI (preamble, partI) where

import Euterpea

import Elements


gMajorScale :: [PitchClass]
gMajorScale = [G, A, B, C, D, E, Fs]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Preamble
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preamble :: Music Pitch
preamble = onBass bassRingUp :=: preOnCoil coilRingUp
  where
    ringUp d p = line [ note (n*d) p | n <- [1..8]]
    bassRingUp = chord $ zipWith3 bassRingOne
        onBassStrings
        [0..]
        [(E,3), (A,3), (D,4), (G,4)]
    bassRingOne onString n p =
        onString $ delayM (fromIntegral n*(24*en-sn))
            $ ringUp en p :+: timesM ((3-n)*3+3) (note wn p)
    preOnCoil = phrase [Art $ Staccato $ 1/2] . onCoil
    coilRingUp = chord
        [ ringTail  50 sn 0 (B,4)
        , ringTail  72 sn 0 (Fs,5)
        , ringTail  88 sn 2 (C,4)
        , ringTail 100 sn 8 (G,5)
        , ringTail 108 sn 6 (D,6)
        , ringTail 116 sn 4 (A,5)
        , ringTail 124 sn 2 (E,6)
        ]
    ringTail r d t p =
        delayM (r*en) $ ringUp d p :+: timesM t (note (8*d) p)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Part I
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

p1Ostinado :: Music Pitch
p1Ostinado = onBass $ line $ concat
    [ riffs qn starts
    , riffs en mids
    , riffs en finals
    ]
  where
    riffs d = map (ringNotes d)
    starts = map init mids
    mids = map tail finals
    finals =
        [ [(G,4), (E,4), (C, 4), (G, 3)]
        , [(Fs,4), (B,3), (G, 3), (D, 3)]
        , [(D,4), (A, 3), (Fs, 3), (B, 2)]
        , [(G,4), (D,4), (B, 3), (E, 3)]
        ]


p1OnCoil :: Music Pitch -> Music Pitch
p1OnCoil = phrase [Art $ Staccato $ 7/8] . onCoil

p1coilA :: Music Pitch
p1coilA = delayM (30*qn) $
    ringNotes qn [(D,6), (B,5)] :+: ringNotes en [(E,6), (C, 6), (G, 5)]

p1coilB :: Music Pitch
p1coilB = delayM (40*qn+27*en+14*en) $ chord $
    zipWith (\n ps -> delayM (n*15*dsn) $ ringNotes dsn ps) [0..]
        [ [(B,5), (G, 5), (D, 5)]
        , [(A, 5), (Fs, 5), (B, 4)]
        , [(D,6), (B, 5), (E, 5)]
        ]

p1coil :: Music Pitch
p1coil = p1OnCoil $ p1coilA :=: p1coilB

partITempo :: Music Pitch -> Music Pitch
partITempo = tempoInterp (125/120) (140/120)

partI :: Music Pitch
partI = partITempo $ p1Ostinado :=: p1coil

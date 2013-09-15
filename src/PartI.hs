module PartI (preamble, partI) where

import Euterpea

import Elements


gMajorScale :: [PitchClass]
gMajorScale = [G, A, B, C, D, E, Fs]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Preamble
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preamble :: Music Pitch
preamble = bassRingUp :=: onCoilRingUp coilRingUp
  where
    ringUp d p = line [ note (n*d) p | n <- [1..8]]
    bassRingUp = chord $ zipWith3 bassRingOne
        onBassStrings
        [0..]
        [(E,3), (A,3), (D,4), (G,4)]
    bassRingOne onString n p =
        onString $ bassMF $ delayM (fromIntegral n*(24*en-sn))
            $ ringUp en p :+: timesM ((3-n)*3+3) (note wn p)
    coilRingUp = chord
        [ ringTail  50 sn 0 (B,4)
        , ringTail  72 sn 0 (Fs,5)
        , ringTail  88 sn 2 (C,4)
        , ringTail 100 sn 8 (G,5)
        , ringTail 108 sn 6 (D,6)
        , ringTail 116 sn 4 (A,5)
        , ringTail 124 sn 2 (E,6)
        ] :+: final
    ringTail r d t p =
        delayM (r*en) $ ringUp d p :+: timesM t (note (8*d) p)
    final = chord $ map (note (2*hn)) [(G,5), (D,6), (A,5), (E,6)]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Part I
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

p1Ostinado :: Music Pitch
p1Ostinado = onBass $ bassMF $ line $ concat
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

phrase2, phrase3, phrase4 :: Dur
phrase2 = 5 * qn
phrase3 = 7 * en
phrase4 = 9 * en

dur2s, dur3s, dur4s :: Dur
dur2s = 10 * qn
dur3s = 27 * en
dur4s = 115 * en

start2s, start3s, start4s :: Dur
start2s = 0
start3s = start2s + 4 * dur2s
start4s = start3s + 4 * dur3s

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

p1coilC :: Music Pitch
p1coilC = delayM (40*qn+108*en+27*en) $ chord $
    zipWith (\n ps -> delayM (n*19*sn) $ ringNotes sn ps) [0..]
        [ [(G,6), (E,6), (C, 6), (G, 5)]
        , [(Fs,6), (B,5), (G, 5), (D, 5)]
        , [(D,6), (A, 5), (Fs, 5), (B, 4)]
        , [(G,6), (D,6), (B, 5), (E, 5)]
        ]


startSectD :: Dur
startSectD = start4s + dur4s + 3 * phrase4
    -- start 3 phrases into the second ring of 4

sectionD :: Music Pitch
sectionD = delayM startSectD $
    p1OnDrums percD
    :=: onCoilLong coilD
    :=: padD
  where
    percD = timesM 8 $ ringPerc en [AcousticSnare, AcousticSnare]
    coilD = delayM (20*en) $ line $ map (ringNotes en) pairs
    pairs = [ [(Fs,6), (B,5)]
            , [(B,5), (G, 5)]
            , [(G,5), (D, 5)]
            , [(Fs,6), (G, 5)]
            , [(B,5), (D, 5)]
            , [(Fs,6), (D, 5)]
            ]
    padD = delayM (20*en) $ padLine hn


startSectE :: Dur
startSectE = start4s + 2 * dur4s + 2 * phrase4

sectionE :: Music Pitch
sectionE = delayM startSectE $ onCoilLong coilE :=: padE
  where
    coilE = chord $ zipWith riff [0..]
                [ [(Fs,6), (B,5), (G, 5)]
                , [(D,6), (A, 5), (Fs, 5)]
                , [(G,6), (D,6), (B, 5)]
                , [(G,5), (E,5), (C, 5)]
                , [(Fs,5), (B,4), (G, 4)]
                , [(D,5), (A, 4), (Fs, 4)]
                ]
    riff n ps = delayM (n * 15 * q) $ ringNotes q ps
    q = dsn
    padE = padLine dhn

startSectF :: Dur
startSectF = start4s + 3 * dur4s + 2 * phrase4

sectionF :: Music Pitch
sectionF = delayM startSectF $
    onCoilLong coilF
    :=: p1OnDrums percF
    :=: padF
  where
    coilF = chord $ zipWith (\n ps -> delayM (n*37*sn) $ ringNotes sn ps) [0..]
        [ [(G,6), (E,6), (C, 6), (G, 5)]
        , [(Fs,6), (B,5), (G, 5), (D, 5)]
        , [(D,6), (A, 5), (Fs, 5), (B, 4)]
        , [(G,6), (D,6), (B, 5), (E, 5)]
        , [(G,5), (E,5), (C, 5), (G, 4)]
        , [(Fs,5), (B,4), (G, 4), (D, 4)]
        , [(D,5), (A, 4), (Fs, 4), (B, 3)]
        , [(G,5), (D,5), (B, 4), (E, 4)]
        ]
    percF = timesM 3 $
        rest hn :+: ringSnareButOneAccented sn 4
    padF = padLine wn

padLine :: Dur -> Music Pitch
padLine q = onPad $ phrase [Dyn $ StdLoudness P] $ chord $ zipWith riff [0..]
        [ [(G,4), (D,4)]
        , [(Fs,5), (Fs,4)]
        , [(B,5), (D,4)]
        ]
  where
    riff n ps = delayM (n*3*q) $ ringNotes q ps

ringSnareButOne :: Dur -> Int -> Music Pitch
ringSnareButOne d n = line $ snareButOne d n

ringSnareButOneAccented :: Dur -> Int -> Music Pitch
ringSnareButOneAccented d n = line $ zipWith ($) (cycle accent) $ snareButOne d n
  where
    accent = (phrase [Dyn $ StdLoudness MF]) : replicate (n-1) id
          ++ (phrase [Dyn $ StdLoudness SF]) : replicate (n-1) id

snareButOne :: Dur -> Int -> [Music Pitch]
snareButOne d n = map (\(d,f) -> f d) $ ring d butOne
  where
    butOne = replicate (n-1) (perc AcousticSnare) ++ [rest]

p1coil :: Music Pitch
p1coil = onCoilLong $ p1coilA :=: p1coilB :=: p1coilC

partITempo :: Music Pitch -> Music Pitch
partITempo = tempoInterp (125/120) (130/120)

p1OnDrums :: Music Pitch -> Music Pitch
p1OnDrums = onDrums . phrase [Dyn $ StdLoudness MP]

partI :: Music Pitch
partI = partITempo $ p1Ostinado :=: p1coil :=: sectionD :=: sectionE :=: sectionF

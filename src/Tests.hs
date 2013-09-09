module Tests
    ( levelTest
    , bassVolumeTest
    , bassTimingTest
    ) where

import Euterpea

import Elements

levelTest :: Music Pitch
levelTest = line parts :+: timesM 4 (chord parts)
  where
    parts = [coilTest, bassTest, bellTest, padTest, drumTest]

    coilTest = timesM 2 $ onCoilLong (line coilNotes)
            :+: onCoilLong (timesM 4 $ chord coilNotes)
            :+: onCoilShort (timesM 4 $ tempo (4/1) $ line coilNotes)
    coilNotes = map (note qn) [(B,4),(E,5),(Fs,5),(Gs,5)]

    bassTest = bassMF $ bassBit :+: transpose 7 bassBit
    bassBit = line (concatMap (replicate 2) $ openStrings qn)
            :+: chord [rest wn, strum en]
    bellTest = onBells $ line $ zipWith note (cycle [qn, qn, qn, dhn])
        [ (E,5), (Gs,5), (Fs,5), (B,4)
        , (E,5), (Fs,5), (Gs,5), (E,5)
        , (Gs,5), (E,5), (Fs,5), (B,4)
        , (B,4), (Fs,5), (Gs,5), (E,5)
        ]
    drumTest = line $ map (\v -> phrase [Dyn $ StdLoudness v] drumBit) [MP, FF]
    drumBit = onDrums $
        timesM 2 (line (map (perc AcousticSnare) [qn, qn, en, en, en]) :+: rest en)
        :+: timesM 3 (perc RideCymbal2 sn :+: perc RideCymbal2 sn :+: perc CrashCymbal2 en)
        :+: perc CrashCymbal2 en :+: perc CrashCymbal2 en

    padTest = onPad $ timesM 2 $ lowLine :+: highLine
    lowLine = line $ map (note hn) [(D,4), (B,4), (B,5), (Fs,5)]
    highLine = line $ map (note qn) [(D,5), (Gs,5), (Cs,6), (E,6)]

bassVolumeTest :: Music Pitch
bassVolumeTest = line $ map atVolume volumes
  where
    atVolume v =
        phrase [Dyn $ Loudness 95] (countOff v)
        :+: rest qn
        :+: phrase [Dyn $ Loudness $ toVel v] arppeg
        :+: rest qn

    volumes = [20,19..1]
    toVel = fromIntegral . (ceiling :: Double -> Int) . (/20) . (*128) . fromIntegral

    countOff v = onBassGString $ line $ concat $ zipWith (:)
        (replicate v $ note sn (G,4))
        (cycle [[], [], [rest sn]])

    arppeg = line $ map (\n -> transpose n $ strum en) [7, 5, 0]

strum :: Dur -> Music Pitch
strum = phrase [Art $ Legato 3.5 ] . line . openStrings

openStrings :: Dur -> [Music Pitch]
openStrings d =
        [ onBassGString $ note d (G,3)
        , onBassDString $ note d (D,3)
        , onBassAString $ note d (A,2)
        , onBassEString $ note d (E,2)
        ]


bassTimingTest :: Music Pitch
bassTimingTest = line $ map timingTo [0..12]
  where
    timingTo t = rest hn :+: line (map (timingToFrom t) [0..12])
    timingToFrom t f =
        onBassGString (fretNote qn (G,3) f 1 :+: fretNote qn (G,3) t 70)
        :=:
        onBassDString (fretNote qn (D,3) t 1 :+: fretNote qn (D,3) t 70)
    fretNote d p t v = phrase [Dyn $ Loudness v] $ note d (trans t p)


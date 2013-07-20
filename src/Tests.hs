module Tests
    ( levelTest
    , bassVolumeTest
    , bassTimingTest
    ) where

import Euterpea

import Elements

-- | A short piece of music for testing that each of the instruments is set up
-- correctly, and that the levels are adjusted. Each instrument is played
-- indivudally for a few bars, and then several repeats of playing them all at
-- once.
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

-- | This test piece plays three arpeggiated chords at each of the 20 different
-- loudness levels the MechBass can produce. It was used to learn the relative
-- volumes of each setting.
--
-- Before each set of chords, the loudness level being tested is counted off by
-- playing notes on the G string. This is so that one can tell what is going on
-- when listening to an audio recording of the test.
--
-- See 'bassFF', 'bassMF', 'bassP', and 'bassPP' in "Elements" for the settings
-- used in the peice.
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

-- | A piece of music that tests if the fret shifter timing is correct.
--
-- For each pair of starting and ending fret position, the shifter on the G
-- string is pre-positioned to the starting position, then played at the ending
-- position. At the same time, the shifter on the D string is simply pre-
-- positioned and played at the ending fret position, and thus doesn't need to
-- move before playing.
--
-- If the allocator (see "MechBassAllocator") dosn't allow enough time for the
-- shifter motion from starting to ending, the not on the G string will play
-- late relative to the D string. If it allows enough (or too much) time, the
-- notes will sound together.
--
-- See "FindSkews.hs" for a program that can use an audio recoding of this test
-- to compute adjustments to the shifter timing in 'MechBass.shifterTimes'.
bassTimingTest :: Music Pitch
bassTimingTest = line $ map timingTo [0..12]
  where
    timingTo t = rest hn :+: line (map (timingToFrom t) [0..12])
    timingToFrom t f =
        onBassGString (fretNote qn (G,3) f 1 :+: fretNote qn (G,3) t 70)
        :=:
        onBassDString (fretNote qn (D,3) t 1 :+: fretNote qn (D,3) t 70)
    fretNote d p t v = phrase [Dyn $ Loudness v] $ note d (trans t p)


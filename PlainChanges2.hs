module PlainChanges2 where

import Data.List (isPrefixOf)

import Codec.Midi
import Euterpea
import Euterpea.IO.MIDI
import Euterpea.IO.MIDI.MidiIO (getAllDevices)

import Changes

{-
    preamble - base "ring up"

    I.
    bass r5 ostinato, moderate
    section 1 - coil r2s
    (pause)
    section 2 - coil r3s
    (pause)
    interlude A - coil r2 into drum r4

    interruption A - coil & bass r2 (ostinato quiet? paused?)

    II.
    bass r4 ostinatos, slow, minor
    section 3 - coil r4 (w/coil r2s, building to end)
    section 4 - coil r2 descending scale
    interlude B - voice r2, r3 (2x?), voice mimic ostinato section
    (pause)
    section 5 - coil r2s

    interruption B - voice, bass, drums, voice r2

    III.
    bass r5 ostinato, fast
    (pause)
    section 6 - coil growing forest of r2,r3,r4
    section 7 - coil r4, cross fade to bells (same r4)
    finale - bell repeat 1-2-3-4 16x, ends with final change of ostinato r5
-}

{-
TODO:
    performance
        [ ] need note off to preceed note on by small fixed amount
            - for bass
            - for coil (as otherwise there is no attack)
        [ ] generate split, 4 midi ch bass line
        [ ] generate velocity 1 pre-positioning bass commands

    partIII
        [ ] toll the coil's r3 and "fade"?

    general
        [ ] volumes?
-}
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Algorithms
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

ring :: Dur -> [a] -> [(Dur, a)]
ring d as = zip (cycle durs) (concat $ ringOf as) ++ zip dursLast as
  where
    durs = dursFront ++ dursBack
    dursFront = map (const d) as
    dursBack = drop 1 dursFront ++ [2*d]
    dursLast = drop 1 dursFront ++ [4*d]

ringNotes :: Dur -> [a] -> Music a
ringNotes d = line . map (uncurry note) . ring d

ringPerc :: Dur -> [PercussionSound] -> Music Pitch
ringPerc d = line . map (uncurry $ flip perc) . ring d

tempoInterp :: Rational -> Rational -> Music a -> Music a
tempoInterp start end = phrase [Tmp $ Accelerando accl] . tempo start
  where
    accl = (end - start) / end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Orchestration
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

onBass, onBells, onCoil, onDrums, onVoice :: Music a -> Music a
onBass  = instrument ElectricBassPicked
onBells = instrument TubularBells
onCoil  = instrument Lead2Sawtooth
onDrums = instrument Percussion
onVoice = instrument ChoirAahs

mainStagePatchMap :: UserPatchMap   -- N.B.: 0-based midi channels!
mainStagePatchMap = [ (ElectricBassPicked, 0)
                    , (Lead2Sawtooth, 1)
                    , (TubularBells, 2)
                    , (ChoirAahs, 3)
                    , (Percussion, 9)
                    ]

playMainStage :: Music Pitch -> IO ()
playMainStage m = do
    devs <- getAllDevices
    case findIacOutput devs of
        ((iacOut,_):_) -> playMidi iacOut $ toMidi (defToPerf m) mainStagePatchMap
        [] -> putStrLn "*** No IAC Driver output found"
  where
    findIacOutput = filter (namedIAC . snd) .  filter (output . snd)
    namedIAC = ("IAC Driver" `isPrefixOf`) . name

writeMidiFile :: FilePath -> Music Pitch -> IO ()
writeMidiFile fp m = exportFile fp $ toMidi (defToPerf m) mainStagePatchMap

bassStrings :: [(Pitch, Pitch)] -- lowest pitch, highest pitch
bassStrings =
    [ ((E,2), (F, 3))
    , ((A,2), (As, 3))
    , ((D,3), (Ds, 4))
    , ((G,3), (Gs, 4))
    ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Preamble & Part I
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

gMajorScale :: [PitchClass]
gMajorScale = [G, A, B, C, D, E, Fs]

preamble' :: Music Pitch
preamble' = onBass bassRingUp :=: onCoil coilRingUp
  where
    ringUp d p = line [ note (n*d) p | n <- [1..8]]
    bassRingUp = line $ map (ringUp en) [(E,2), (A,2), (D,3), (G,3)]
    coilRingUp = rest qn


preamble :: Music Pitch
preamble = onBass bassRingUp :=: onCoil coilRingUp
  where
    ringUp d p = line [ note (n*d) p | n <- [1..8]]
    bassRingUp = chord $ zipWith bassRingOne [0..] [(E,3), (A,3), (D,4), (G,4)]
    bassRingOne n p = delayM (fromIntegral n*(24*en-sn)) $ ringUp en p :+: timesM ((3-n)*3+3) (note wn p)
    coilRingUp = rest qn


p1Ostinado :: Music Pitch
p1Ostinado = onBass $ line
    [ ringNotes qn [(E,4), (C, 4)]
    , ringNotes qn [(B,3), (G, 3)]
    , ringNotes qn [(D,4), (B, 3)]
    , ringNotes qn [(C,4), (A, 3)]

    , ringNotes en [(E,4), (C, 4), (G, 3)]
    , ringNotes en [(B,3), (G, 3), (D, 3)]
    , ringNotes en [(D,4), (G, 3), (E, 3)]
    , ringNotes en [(E,4), (B, 3), (Fs, 3)]

    , ringNotes en [(C,4), (A, 3), (Fs, 3)]
    , ringNotes en [(G,3), (D, 3), (B, 2)]
    , ringNotes en [(A,3), (Fs, 3), (E, 3)]
    , ringNotes en [(D,4), (G, 3), (E, 3)]

    , ringNotes en [(D,4), (A, 3), (Fs, 3), (E, 3)]
    , ringNotes en [(Fs,4), (D, 4), (G, 3), (E, 3)]
    , ringNotes en [(A,3), (E, 3), (C, 3), (G, 2)]
    ]




p1OnCoil :: Music Pitch -> Music Pitch
p1OnCoil = phrase [Art $ Staccato $ 15/16] . onCoil

p1coilA :: Music Pitch
p1coilA = p1OnCoil $ chord
    [ delayM (13*qn) $ ringNotes sn [(E,5), (C, 5), (G, 4)]
    , delayM (1*12*qn) $ ringNotes sn [(D,5), (B, 4), (Fs, 4)]
    ]

partITempo :: Music Pitch -> Music Pitch
partITempo = tempoInterp (125/120) (145/120)

partI :: Music Pitch
partI = partITempo $ p1Ostinado

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

p3OnCoil :: Music Pitch -> Music Pitch
p3OnCoil = phrase [Art $ Staccato $ 15/16] . onCoil . transpose 12

p3SetA = p3OnCoil $ chord
    [ timesM 10 $ p3r2
    , delayM (2 * p3r2dur) $ timesM 2 p3r3
    , delayM (2 * p3r2dur + p3r3dur) p3r4
    ]

p3SetB = onBells (p3r2 :+: rest p3r2dur :+: p3r2' :+: rest p3r2dur :+: p3r2) :+: chord
    [ onBells $ p3r3 :+: p3r3 :+: p3r4
    , p3OnCoil p3r4
    ]

p3SetC :: Music Pitch
p3SetC = onBells $ timesM 8 $ line $ map (note qn) [(Bf, 4), (F, 4), (D, 4), (C, 4), (Bf, 3)]

partIII :: Music Pitch
partIII = tempo (160/120) $
    p3Ostinado
    :=: delayM ( 1*p3OstinadoPhraseDur) p3SetA
    :=: delayM (12*p3OstinadoPhraseDur) p3SetB
    :=: delayM (26.5*p3OstinadoPhraseDur) p3SetC


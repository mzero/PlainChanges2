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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Preamble & Part I
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preamble :: Music Pitch
preamble = undefined


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Part II
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

fMinorScale :: [PitchClass]
fMinorScale = [F, G, Af, Bf, C, Df, Ef, F]

arpeggioChord :: Dur -> Dur -> [a] -> Music a
arpeggioChord d e = chord . zipWith delayedNote [n * e | n <- [0..]]
  where
    delayedNote ne p = rest ne :+: note (d - ne) p


p2Ostinado :: Music Pitch
p2Ostinado = onBass
        $ chord $ zipWith delayedRing starts pitchSets
  where
    delayedRing d ps = rest d :+: ringNotes qn ps
    pitchSets = [ [(E,  3), (Df, 3), (Bf, 2), (G, 2)]
                , [(C,  4), (Af, 3), (F,  3), (C, 3)]
                , [(Df, 4), (C,  4), (Af, 3), (E, 3)]
                , [(F,  4), (Ef, 4), (C, 4), (G, 3)]
                ]
    starts = [  0 * 9 * qn - 0 * sn
             ,  4 * 9 * qn - 2 * sn
             ,  8 * 9 * qn - 1 * sn
             , 10 * 9 * qn + 4 * qn - 3 * sn
             ]

p2coil :: Music Pitch
p2coil = phrase [Art $ Staccato $ 1/4] (onCoil cParts)
  where
    cParts = chord [ cPart (2*9*qn) (sn/2) [(Af, 5), (F,  5), (C, 5)]
                   , cPart (3*9*qn) (sn/2) [(C,  6), (Af, 5), (E, 5)]
                   , cPart (7*9*qn) (sn/2) [(Ef, 6), (C, 6), (G, 5)]
                   ]
    cPart r d ps = rest r :+: ringNotes d ps
    cascade = chord . zipWith (flip cPart en) [0,5*en..]

p2tempo :: Music a -> Music a
p2tempo m = tempo (startTempo / 120) mAll
  where
    startTempo = 70
    endTempo = 85
    accl = (endTempo - startTempo) / startTempo
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
partII = (p2tempo $ p2Ostinado :=: p2coil) -- :+: interruptionB

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


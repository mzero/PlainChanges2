module PlainChanges2 where

import Codec.Midi
import Euterpea
import Euterpea.IO.MIDI

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
ringPerc d = instrument Percussion . line . map (uncurry $ flip perc) . ring d

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Orchestration
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

onBass, onBells, onCoil, onDrums, onVoice :: Music a -> Music a
onBass  = instrument ElectricBassPicked
onBells = instrument TubularBells
onCoil  = instrument Lead2Sawtooth
onDrums = instrument Percussion
onVoice = instrument Lead6Voice

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
                   --, cPart (14*9*qn) sn [(F, 5), (Df, 5)]
                   --, cPart (16*9*qn) sn [(Df, 5), (Af, 4)]
                   --, cPart (18*9*qn) sn [(F, 5), (Df, 5)]
                   --, cPart (20*9*qn) sn [(Df, 5), (Af, 4)]
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

partII :: Music Pitch
partII = p2tempo $ p2Ostinado :=: p2coil

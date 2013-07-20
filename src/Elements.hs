module Elements
    ( ring, ringNotes, ringPerc
    , tempoInterp

    , onBass, onBells, onCoil, onDrums, onPad
    , onBassStrings
    , onBassEString, onBassAString, onBassDString, onBassGString
    , bassFF, bassMF, bassP, bassPP
    , onCoilShort, onCoilRingUp, onCoilLong

    , extendedPlayer, toExtendedPerf
    , coilPlayer, toCoilPerf
    , holdLast
    )
where

import Euterpea

import Changes

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Algorithms
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Ring a sequence. This produces the sequence of notes for ringing the
-- changes of the sequence. The permutations of the elements are produced in
-- the proper order (see `ringOf` in "Changes"), and the initial sequence is
-- repeated at the end, as traditionally played.
--
-- Each element in the rung change is paired with the duration that it should
-- take, based on a basic pulse duration supplied. The last note of every
-- second permutation take twice as long, and the very final note, four times.
ring :: Dur -> [a] -> [(Dur, a)]
ring d as = zip (cycle durs) (concat $ ringOf as) ++ zip dursLast as
  where
    durs = dursFront ++ dursBack
    dursFront = map (const d) as
    dursBack = drop 1 dursFront ++ [2*d]
    dursLast = drop 1 dursFront ++ [4*d]

-- Like `ring` but produces a `Music` which is a `line` of notes.
ringNotes :: Dur -> [a] -> Music a
ringNotes d = line . map (uncurry note) . ring d

-- Like `ring` but produces a `Music` from a sequence of percussion sounds.
ringPerc :: Dur -> [PercussionSound] -> Music Pitch
ringPerc d = line . map (uncurry $ flip perc) . ring d

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Orchestration
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Assign a piece of music to an instrument.
-- These assign `instrument`, and if needed, `player`.
onBass, onBells, onCoil, onDrums, onPad :: Music a -> Music a
onBass  = instrument ElectricBassPicked
onBells = instrument TubularBells
onCoil  = instrument Lead2Sawtooth . player "Coil"
onDrums = instrument Percussion
onPad   = instrument Pad6Metallic

-- | To assign parts to specitic strings of the MechBass, use these.
-- They are a bit of a hack: Each is assigned a different instrument, which is
-- later mapped to the specific channel for the bass string.
onBassEString, onBassAString, onBassDString, onBassGString :: Music a -> Music a
[onBassEString, onBassAString, onBassDString, onBassGString] = onBassStrings

onBassStrings :: [Music a -> Music a]
onBassStrings = map instrument
    [SlapBass1, SlapBass2, SynthBass1, SynthBass2]

-- | Volumes on the MechBass are mapped to velocity somewhat differently than
-- the standard mappings. These values were determined empirically (See
-- `bassVolumeTest` in "Tests").
bassFF, bassMF, bassP, bassPP :: Music a -> Music a
[bassFF, bassMF, bassP, bassPP] =
    map (\v -> phrase [Dyn $ Loudness v]) [96, 70, 45, 19]

-- | There are three kinds of articulations used for the coil part. These are
-- represented by two forms of Staccato, and a no-op Legato marking. When played
-- on Pyramider, with `coilPlayer`, these markings are recognized and mapped to
-- the very short note timings required. When played on more conventional
-- instruments, the Staccato and Legato markings are interpreted normally.
onCoilShort, onCoilRingUp, onCoilLong :: Music a -> Music a
onCoilShort = onCoil . phrase [Art $ Staccato (1/4)]
onCoilRingUp = onCoil . phrase [Art $ Staccato (1/2)]
onCoilLong = onCoil . phrase [Art $ Legato 1]

onCoilArt :: Rational -> Music a -> Music a
onCoilArt r = onCoil . phrase [Art $ Staccato r]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Players
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type AttributeTest = PhraseAttribute -> Bool
type AttributeApply =
    PhraseAttribute -> (Performance, DurT) -> (Performance, DurT)

-- | Create a `Player` that extends the standard `fancyPlayer`. The extended
-- player handles some attributes (as determined by the test), with the supplied
-- `AttributeApply` function.
--
-- This code is needed because `fancyPlayer` as written can't be easily
-- extended: When interpreting a string of `PhraseAttribute` values, it does so
-- without a way to extend them. This code achieves the extension by breaking
-- up strings of `PhraseAttributes` at points where the extended player should
-- handle the attributes.
extendFancyPlayer :: String -> AttributeTest -> AttributeApply
    -> Player (Pitch, [NoteAttribute])
extendFancyPlayer name aTest aApply =
    fancyPlayer { pName = name, interpPhrase = interp }
  where
    interp :: PMap a -> Context a -> [PhraseAttribute]
                     -> Music a -> (Performance, DurT)
    interp pm ctx pas m = case break aTest pas of
        ([], [])            -> perf pm ctx m
        ([], (ea : pre))    -> aApply ea $ interp pm ctx pre m
        (_,  [])            -> fancyInterpPhrase pm ctx pas m
        (post, pre)         -> perf pm ctx $ phrase post $ phrase pre m

-- | The standard player used in this work. Currently, the only attribute it
-- adds suport for is `Fermata` (and the equivalent `FermataDown`).
--
-- `Fermata` (and `FermataDown`) is handled by extending the duration of the
-- last note of the phrase 4x. Note that the duration of the phrase is not
-- altered, and the last note will spill over into whatever comes next.
--
-- Bug: Does not correctly handle a final chord, only a final single note.
extendedPlayer :: Player (Pitch, [NoteAttribute])
extendedPlayer = extendFancyPlayer "Extended" isExtraAttr extraInterp
  where
    isExtraAttr (Art Fermata) = True
    isExtraAttr (Art FermataDown) = True
    isExtraAttr _ = False

    extraInterp (Art Fermata) = extendLastEvent
    extraInterp (Art FermataDown) = extendLastEvent
    extraInterp _ = id

    extendLastEvent p@([],_) = p
    extendLastEvent (evs,d) = (init evs ++ [extendEvent $ last evs], d)

    extendEvent ev = ev { eDur = 4 * eDur ev }

-- | Utility to create a performance where the default player is the
-- `extendedPlayer`.
toExtendedPerf :: Performable a => Music a -> Performance
toExtendedPerf = fst . perfDur pMap con0
  where
    pMap "Extended" = extendedPlayer
    pMap p = defPMap p

    con0 = defCon { cPlayer = extendedPlayer }

-- | Player for the coil part, when played on the actual coil.
--
-- Two transformations are made: 1) The notes are played an octave lower than
-- notated. 2) The articulation is interpreted by setting the note durations to
-- the very short times needed on the coil.
coilPlayer :: Player (Pitch, [NoteAttribute])
coilPlayer = basicCoilPlayer { playNote = (playNote basicCoilPlayer) . down12 }
  where
    basicCoilPlayer = extendFancyPlayer "Coil" isCoilAttr coilInterp
    down12 ctx = ctx { cPch = cPch ctx - 12 }

    isCoilAttr (Art (Staccato _)) = True
    isCoilAttr (Art (Legato _)) = True
    isCoilAttr _ = False

    coilInterp (Art (Staccato f)) | f <= 1/4  = mapDurs (const 0.045)
                                  | otherwise = mapDurs (const 0.090)
    coilInterp (Art (Legato _)) = mapDurs (\d -> (d * 3/4) `min` 0.150)
    coilInterp _ = id

    mapDurs f (evs,d) = (map (adjustDur f) evs, d)
    adjustDur f ev = ev { eDur = f $ eDur ev }

-- | Utility to create a performance intended for the actual coil. The
-- `coilPlayer` is used for the coil part, and `extendedPlayer` for all others.
toCoilPerf :: Performable a => Music a -> Performance
toCoilPerf = fst . perfDur pMap con0
  where
    pMap "Extended" = extendedPlayer
    pMap "Coil" = coilPlayer
    pMap p = defPMap p

    con0 = defCon { cPlayer = extendedPlayer }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helpers
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Slew the tempo of a piece of `Music` from one tempo setting to another by
-- marking the phrase with both `tempo` and `Accelerando`. The start and end
-- values should be the same as one would give to `tempo`.
tempoInterp :: Rational -> Rational -> Music a -> Music a
tempoInterp start end = phrase [Tmp $ Accelerando accl] . tempo start
  where
    accl = (end - start) / end

-- | Apply a `Fermata` to a phrase. Using the `extendedPlayer` this will
-- lengthen the last note, but not the phrase it is part of. That is, the
-- note will "spill over".
holdLast :: Music a -> Music a
holdLast = phrase [Art Fermata]

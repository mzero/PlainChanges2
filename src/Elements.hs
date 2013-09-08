module Elements
    ( ring, ringNotes, ringPerc
    , tempoInterp

    , onBass, onBells, onCoil, onDrums, onVoice
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
onCoil  = instrument Lead2Sawtooth . player "Coil"
onDrums = instrument Percussion
onVoice = instrument ChoirAahs

-- | To assign parts to specitic strings of the MechBass, use these.
-- They are a bit of a hack: Each is assigned a different instrument, which is
-- later mapped to the specific channel for the bass string.
onBassStrings :: [Music a -> Music a]
onBassStrings = map instrument
    [SlapBass1, SlapBass2, SynthBass1, SynthBass2]

onBassEString, onBassAString, onBassDString, onBassGString :: Music a -> Music a
[onBassEString, onBassAString, onBassDString, onBassGString] = onBassStrings

bassFF, bassMF, bassP, bassPP :: Music a -> Music a
[bassFF, bassMF, bassP, bassPP] =
    map (\v -> phrase [Dyn $ Loudness v]) [96, 70, 45, 19]

onCoilArt :: Rational -> Music a -> Music a
onCoilArt r = onCoil . phrase [Art $ Staccato r]

onCoilShort, onCoilRingUp, onCoilLong :: Music a -> Music a
onCoilShort = onCoil . phrase [Art $ Staccato (1/4)]
onCoilRingUp = onCoil . phrase [Art $ Staccato (1/2)]
onCoilLong = onCoil . phrase [Art $ Legato 1]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helpers
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type AttributeTest = PhraseAttribute -> Bool
type AttributeApply =
    PhraseAttribute -> (Performance, DurT) -> (Performance, DurT)

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

toExtendedPerf :: Performable a => Music a -> Performance
toExtendedPerf = fst . perfDur pMap con0
  where
    pMap "Extended" = extendedPlayer
    pMap p = defPMap p

    con0 = defCon { cPlayer = extendedPlayer }

coilPlayer :: Player (Pitch, [NoteAttribute])
coilPlayer = basicCoilPlayer { playNote = (playNote basicCoilPlayer) . down12 }
  where
    basicCoilPlayer = extendFancyPlayer "Coil" isCoilAttr coilInterp
    down12 ctx = ctx { cPch = cPch ctx - 12 }

    isCoilAttr (Art (Staccato _)) = True
    isCoilAttr (Art (Legato _)) = True
    isCoilAttr _ = False

    coilInterp (Art (Staccato f)) | f <= 1/4 = mapDurs (const 0.045)
                                  | f <= 1/2 = mapDurs (const 0.090)
                                  | otherwise = id
    coilInterp (Art (Legato _)) = mapDurs (\d -> (d * 7/8) `min` 0.150)
    coilInterp _ = id

    mapDurs f (evs,d) = (map (adjustDur f) evs, d)
    adjustDur f ev = ev { eDur = f $ eDur ev }

toCoilPerf :: Performable a => Music a -> Performance
toCoilPerf = fst . perfDur pMap con0
  where
    pMap "Extended" = extendedPlayer
    pMap "Coil" = coilPlayer
    pMap p = defPMap p

    con0 = defCon { cPlayer = extendedPlayer }

holdLast :: Music a -> Music a
holdLast = phrase [Art Fermata]

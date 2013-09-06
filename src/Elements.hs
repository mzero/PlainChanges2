module Elements
    ( ring, ringNotes, ringPerc
    , tempoInterp

    , onBass, onBells, onCoil, onDrums, onVoice
    , onBassStrings
    , onBassEString, onBassAString, onBassDString, onBassGString
    , bassFF, bassMF, bassP, bassPP
    , onCoilArt, onCoil14, onCoil12, onCoil78, onCoil1516

    , extendedPlayer, toExtendedPerf
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
onCoil  = instrument Lead2Sawtooth
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

onCoil14, onCoil12, onCoil78, onCoil1516 :: Music a -> Music a
[onCoil14, onCoil12, onCoil78, onCoil1516] =
        map onCoilArt [1/4, 1/2, 7/8, 15/16]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Helpers
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

extendedPlayer :: Player (Pitch, [NoteAttribute])
extendedPlayer = fancyPlayer { pName = "Extended", interpPhrase = extraInterp }
  where
    extraInterp :: PMap a -> Context a -> [PhraseAttribute]
                     -> Music a -> (Performance, DurT)
    extraInterp pm ctx pas m = case break isExtraAttr pas of
        ([], [])            -> perf pm ctx m
        ([], (ea : pre))    -> extraInterpAttr ea $ extraInterp pm ctx pre m
        (_,  [])            -> fancyInterpPhrase pm ctx pas m
        (post, pre)         -> perf pm ctx $ phrase post $ phrase pre m

    isExtraAttr :: PhraseAttribute -> Bool
    isExtraAttr (Art Fermata) = True
    isExtraAttr (Art FermataDown) = True
    isExtraAttr _ = False

    extraInterpAttr (Art Fermata) = extendLastEvent
    extraInterpAttr (Art FermataDown) = extendLastEvent
    extraInterpAttr _ = id

    extendLastEvent p@([],_) = p
    extendLastEvent (evs,d) = (init evs ++ [extendEvent $ last evs], d)

    extendEvent ev = ev { eDur = 4 * eDur ev }

toExtendedPerf :: Performable a => Music a -> Performance
toExtendedPerf = fst . perfDur extPMap extCon
  where
    extPMap "Extended" = extendedPlayer
    extPMap p = defPMap p

    extCon = defCon { cPlayer = extendedPlayer }

holdLast :: Music a -> Music a
holdLast = phrase [Art Fermata]

module PlainChanges2 where

import Codec.Midi
import Data.List (isPrefixOf)
import Euterpea
import Euterpea.IO.MIDI.MidiIO (getAllDevices)

import qualified Coil
import qualified MechBass
import qualified MechBassAllocator as MechBass
import MidiUtil
import PartI
import PartII
import PartIII

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

plainChanges2_30 :: Music Pitch
plainChanges2_30 =
    preamble
    :+: rest (2*wn)
    :+: partI
    :+: rest (wn)
    :+: partII
    :+: rest (wn)
    :+: partIII

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Performance
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

mainStagePatchMap :: UserPatchMap   -- N.B.: 0-based midi channels!
mainStagePatchMap = [ (ElectricBassPicked, 4)
                    , (Lead2Sawtooth, 5)
                    , (TubularBells, 6)
                    , (ChoirAahs, 7)
                    , (Percussion, 9)
                    ]

mainStageMidi :: Music Pitch -> Midi
mainStageMidi m = toMidi (defToPerf m) mainStagePatchMap

checkCoilTrack :: Music Pitch -> IO ()
checkCoilTrack = runCheckChannel 5 Coil.validate . mainStageMidi

playMainStage :: Music Pitch -> IO ()
playMainStage m = do
    devs <- getAllDevices
    case findIacOutput devs of
        ((iacOut,_):_) -> playMidi iacOut $ mainStageMidi m
        [] -> putStrLn "*** No IAC Driver output found"
  where
    findIacOutput = filter (namedIAC . snd) .  filter (output . snd)
    namedIAC = ("IAC Driver" `isPrefixOf`) . name

writeMidiFile :: FilePath -> Music Pitch -> IO ()
writeMidiFile fp = exportFile fp . mainStageMidi

allocateBass :: Music Pitch -> (MechBass.AllocMessages, Midi)
allocateBass = processChannel MechBass.allocator 4 . mainStageMidi

validateBassMidi :: Midi -> IO ()
validateBassMidi m = do
    putStrLn "== Validation =="
    mapM_ run $ zip [0..] MechBass.bassStrings
  where
    run (ch, bs) = do
        putStrLn $ "-- Channel " ++ show ch ++ " --"
        runCheckChannel ch (MechBass.validate bs) m

debugBass :: String -> Music Pitch -> IO ()
debugBass prefix m = do
    writeFile ("dump/" ++ prefix ++ "-pretalloc.txt")
        $ unlines $ dumpMidi $ mainStageMidi m
    writeFile ("dump/" ++ prefix ++ "-postalloc.txt")
        $ unlines $ dumpMidi $ mPost
    writeFile ("dump/" ++ prefix ++ "-alloclog.txt")
        $ unlines $ showErrorMessages msgs
    validateBassMidi mPost
  where
    (msgs, mPost) = allocateBass m

debugAllBass :: IO ()
debugAllBass = do
    debugBass "preamble" preamble
    debugBass "partI" partI
    debugBass "partII" partII
    debugBass "partIII" partIII



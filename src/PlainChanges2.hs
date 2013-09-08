module PlainChanges2 where

import Codec.Midi
import Data.List (isPrefixOf)
import Euterpea
import Euterpea.IO.MIDI.MidiIO (getAllDevices)

import qualified Coil
import Elements
import qualified MechBass
import qualified MechBassAllocator as MechBass
import MidiUtil
import PartI
import PartII
import PartIII
import Tests

import Sound.MidiPlayer

plainChanges2_30 :: Music Pitch
plainChanges2_30 =
    rest hn
    :+: preamble
    :+: rest dwn
    :+: partI
    :+: rest dwn
    :+: partII
    :+: rest wn
    :+: partIII
    :+: rest (4*wn)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Performance
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Maps parts, identified by instruments, to MIDI channels
-- The bass part, as composed, is either on the strings indvidually, or on the
-- whole bass part channel.
patchMap :: UserPatchMap   -- N.B.: 0-based midi channels!
patchMap = [ (SynthBass2, 0)            -- MechBass G string
           , (SynthBass1, 1)            -- MechBass D string
           , (SlapBass2, 2)             -- MechBass A string
           , (SlapBass1, 3)             -- MechBass E string
           , (ElectricBassPicked, 4)    -- whole bass part
           , (Lead2Sawtooth, 5)         -- Coil
           , (TubularBells, 6)          -- bells
           , (ChoirAahs, 7)             -- voices
           , (Percussion, 9)            -- drums
           ]

-- | Convert music to MIDI, as composed. Generally this form isn't playable
-- directly, as the bass part is divided amongst five channels.
midiForCoil, midiForOrch :: Music Pitch -> Midi
midiForCoil m = toMidi (toCoilPerf m) patchMap
midiForOrch m = toMidi (toExtendedPerf m) patchMap

-- | Prepare MIDI for performance. Full bass part is allocated to strings, and
-- all bass parts are adjusted with pre-positioning events.
preparePerformance :: Midi -> (([String], [String]), Midi)
preparePerformance mComposed = (msgs, mPerformed)
  where
    (cMsgs, mRestricted) = restrictCoil mComposed
    (bMsgs, mAllocated) = allocateBass mRestricted
    msgs = (showErrorMessages cMsgs, showErrorMessages bMsgs)
    mPerformed = filterMessages (not . isProgramChange) mAllocated


-- | Prepare MIDI for preview on synths. Bass parts are combined back into a
-- single track, and any pre-positioning evnets are removed.
preparePreview :: Midi -> Midi
preparePreview = processTracks (MechBass.recombine 4)


-- | Play music as composed, on synthesizers
playOnCoil :: Music Pitch -> IO ()
playOnCoil = playMidiOnSynth . midiForCoil

-- | Play music as performed, on synthesizers
performOnCoil :: Music Pitch -> IO ()
performOnCoil = playMidiOnSynth . snd . preparePerformance . midiForCoil

-- | Play music as composed, on orchestral instruments
playOnOrch :: Music Pitch -> IO ()
playOnOrch = playMidiOnSynth . midiForOrch


-- | Play MIDI on synthesizers. The MIDI is prepared for preview (bass on one
-- channel). Plays on the first output port of the IAC Driver.
playMidiOnSynth :: Midi -> IO ()
playMidiOnSynth midi = do
    devs <- getAllDevices
    case findIacOutput devs of
        ((iacOut,_):_) -> midiPlayer iacOut $ preparePreview midi
        [] -> putStrLn "*** No IAC Driver output found"
  where
    findIacOutput = filter (namedIAC . snd) .  filter (output . snd)
    namedIAC = ("IAC Driver" `isPrefixOf`) . name


restrictCoil :: Midi -> (Coil.Messages, Midi)
restrictCoil = processChannels Coil.restrict (== 5)

allocateBass :: Midi -> (MechBass.AllocMessages, Midi)
allocateBass = processChannels MechBass.allocator (<= 4)

validateCoil :: Midi -> [String]
validateCoil m = "== Coil Validation ==" :
    runCheckChannel 5 Coil.validate m

validateBass :: Midi -> [String]
validateBass m = "== Bass Validation ==" :
    concat (zipWith run MechBass.bassChannels MechBass.bassStrings)
  where
    run ch bs = ("-- Channel " ++ show ch ++ " --") :
        runCheckChannel ch (MechBass.validate bs) m

prepareMidiFiles :: String -> Music Pitch -> IO ()
prepareMidiFiles prefix m = do
    writeMidiFile (prefix ++ "-composed") mComposed
    writeMidiFile (prefix ++ "-performed") mPerformed
    writeMidiFile (prefix ++ "-previewed") mPreviewed
    writeMidiFile (prefix ++ "-orchestral") mOrchestral
    writeFile (prefix ++ "-log.txt") $ unlines logLines
  where
    mComposed = midiForCoil $ rest hn :+: m
    ((cMsgs, bMsgs), mPerformed) = preparePerformance mComposed
    mPreviewed = preparePreview mPerformed
    mOrchestral = midiForOrch m

    logLines = validateCoil mPerformed
            ++ validateBass mPerformed
            ++ "== Coil Restriction ==" : cMsgs
            ++ "== Bass Allocation ==" : bMsgs

writeMidiFile :: String -> Midi -> IO ()
writeMidiFile path midi = do
    exportFile (path ++ ".midi") midi'
    writeFile (path ++ ".txt") $ unlines $ dumpMidi midi'
  where
    midi' = prepTrackEnds midi


debugAllParts :: IO ()
debugAllParts = do
    prepareMidiFiles "dump/plain-changes-2" plainChanges2_30
    prepareMidiFiles "dump/preamble" preamble
    prepareMidiFiles "dump/partI" partI
    prepareMidiFiles "dump/partII" partII
    prepareMidiFiles "dump/partIII" partIII
    prepareMidiFiles "dump/level-test" levelTest
    prepareMidiFiles "dump/bass-volume-test" bassVolumeTest
    prepareMidiFiles "dump/bass-timing-test" bassTimingTest


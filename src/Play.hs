module Play
    ( playOnCoil
    , performOnCoil
    , playOnOrch

    , prepareMidiFiles
    )
  where

import Codec.Midi
import Data.List (isPrefixOf)
import Euterpea
import Euterpea.IO.MIDI.MidiIO (getAllDevices)

import qualified Coil
import Elements
import qualified MechBass
import qualified MechBassAllocator as MechBass
import MidiUtil

import Sound.MidiPlayer


-- | Maps parts, identified by instruments, to MIDI channels
-- The bass part, as composed, is either on the strings indvidually, or on the
-- bass as a whole.
patchMap :: UserPatchMap   -- N.B.: 0-based midi channels!
patchMap = [ (SynthBass2, 0)            -- MechBass G string
           , (SynthBass1, 1)            -- MechBass D string
           , (SlapBass2, 2)             -- MechBass A string
           , (SlapBass1, 3)             -- MechBass E string
           , (ElectricBassPicked, 4)    -- whole bass part
           , (Lead2Sawtooth, 5)         -- Coil
           , (TubularBells, 6)          -- bells
           , (Pad6Metallic, 7)          -- pad
           , (Percussion, 9)            -- drums
           ]

-- | Convert music to MIDI, as composed. Generally this form isn't playable
-- directly, as the bass part is divided amongst five channels.
--
-- The two forms differ in the conversion of the coil part: 'midiForCoil'
-- uses the 'coilPlayer' to adjust for performance on Pyramider. 'midiForOrch'
-- leaves the coil part as written for play back with orchestral synths.
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

-- | Generate a full set of MIDI files for a given part.
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


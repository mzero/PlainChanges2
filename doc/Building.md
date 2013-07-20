Prequisites
-----------

The code is written in [Haskell], and uses the [Euterpea] library.

Building
--------

This package can be built with the normal cabal build sequence:

    cabal configure
    cabal build

Running
-------

Running the executable with no arguments prints a usage:

    & ./dist/build/plainchanges2/plainchanges2

    realtime output to synths:
        plainchanges2 play [<part>]                # play as composed on synths
        plainchanges2 perform [<part>]             # play as performed on synths
        plainchanges2 orch [<part>]                # play on orchestral instruments

    utilities:
        plainchanges2 midi <dir> [<part> | all]    # write midi files to a directory
        plainchanges2 visual                       # output text visualization of bass
        plainchanges2 skew <file-1> <file-2>       # compute bass skews between two audio files

    [<part>] defaults to the whole work, or is one of:
        whole | preamble | partI | partII | partIII | levelTest | bassVolume | bassTiming

Interactive Use
---------------

It is common to want to be able to run the code interactively. For this, you
can invoke ghci as follows:

    ghci -isrc

You can then execute this series of commands to set things up:

    :m + Euterpea
    :m + Euterpea.IO.MIDI
    :m + Euterpea.IO.MIDI.MidiIO
    :l src/PlainChanges2.hs

At this point there are several commond things to do:

### Discover available MIDI devices

    initializeMidi >> getAllDevices >>= mapM_ print

### Play the work as performed

    performOnCoil plainChanges2_30

### Play the work as composed

    playOnCoil plainChanges2_30

### Play the work as composed, on orchestral instruments

    playOnOrch plainChanges2_30

### Other things to play

    preamble
    partI
    partII
    partIII
    levelTest

---


The MIDI player takes live keyboard commands:

    space - toggle play/stop status
      [   - skip back 10 seconds
      ]   - skip forward 10 seconds
      x   - stop
      q   - quit


The MIDI player outputs on the output device associated with the
IAC MIDI port. On Macintosh, this lets one pipe that MIDI to a software synth.
such as MainStage, or using the applicaiton __Audio MIDI Setup__, route it out
and actual HW port.

If you need to change where it outputs (not on OS X, say), then you can either
edit the code for `playMidiOnSynth` in `PlainChanges2.hs`, or do something like:

    midiPlayer dev $ preparePreview $ midiForCoil plainChanges2_30

where `dev` is replaced with the id of the output MIDI device.





[Haskell]: http://haskell.org/
[Euterpea]: http://euterpea.music.yale.edu/

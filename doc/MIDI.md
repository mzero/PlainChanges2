Channels
========

_N.B.: Euterpea numbers channels from 0, but almost all MIDI literature numbers
them from 1. This file uses the from 1 numbering as that is what you'll
encounter on devices and synth software. The code uses from 0. Sigh._

MIDI channels are assinged as follows:

### Ch.s 1,2,3,4: Bass Strings

The MechBass uses separate channels for each string. The strings are standard
bass strings, E - A - D - G, but are assigned high to low: 1 is the G string,
and 4 is the E string.

### Ch. 5: Bass

When playing the Bass part for a synth. instrument, or when written before
string allocation, then the whole bass part is on channel 5.

### Ch. 6: Coil

In the performance version, intended for Pyramider, this is an octave lower than
written.

### Ch. 7: Bells

### Ch. 8: Pad

### Ch. 10: Percussion


Messages
========

There are no program change messages in the MIDI streams.

Percussion notes follow the General MIDI guidlines for percussion assignment.
Only these percussion sounds are used:

    * AcousticSnare
    * RideCymbal2

MIDI intended for performance on the MechBass has prepositioning events
(__Note On__ messages with velocity of 1).


MIDI File Versions
==================

The function `prepareMidiFiles` in `PlainChanges2.hs` writes any given music
out in for versions, identified by suffix:

### `-composed.midi`

This is the version as composed. Bass parts are both channels 1 ~ 4 when
composed for particular strings, and on channel 5 when written for bass as a
whole.

### `-orchestral.midi`

This is perpared for orchestral instruments, or really anything other than the
actual coil. Bass is all on channel 5, and the coil notes are played as written.

### `-performed.midi`

The MIDI file for performance on the MechBass and Pyramider coil. The bass parts
are all on channels 1 ~ 4, with pre-positioning events. The coil is an octave
lower.

### `-previewed.midi`

Just like the performed version, but with the bass bounced back to channel 5,
with the prepositioning events removed. This version is for playing a
simulation on synth.s of how the MechBass and Coil will sound.

### `.txt`

All of the above have `.txt` text file variants for quick human checking. These
are produced by the function `dumpMidi` in `MidiUtil.hs`.

### `-log.txt`

A text file containing messages from the preparation of the above files. Useful
to see how the MechBass string allocation went, and if the files pass the
validation checks for MechBass and Pyramider.


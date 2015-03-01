# tidal-midi
Tidal module for sending patterns over MIDI.

PortMIDI variant. Should work on OS X, Linux and Windows

This (still) is experimental software

## Usage

in your `.ghci` add the following:

```haskell
import Sound.Tidal.SimpleSynth

keyStreams <- keyproxy 100000 1 [1]

[k1] <- sequence keyStreams
```

`keyproxy` accepts three arguments, first is latency for MIDI signals. Adjust to match your other playbacks. Second is the PortMIDI device ID (`tidal-midi-outputs` comes with this library to list PortMIDI Device ids). Third is a list of MIDI channels to create keyStreams for.

Then as usual with Tidal run `ghci -XOverloadedStrings` and send MIDI commands to a compliant device (software synth).

```haskell
k1 $ note "50*4" |+| slow 2 (kcutoff (scale 0.2 0.9 tri1))
```

## Known issues and limitations

- Timing, currently PortMIDI is sending out realtime messages without timestamps. Depending on the CPU-usage this might lead to hickups.
- SysEx support is missing but possible

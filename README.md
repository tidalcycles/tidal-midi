# tidal-midi
Tidal module for sending patterns over MIDI.

__PortMIDI__ variant. Should work on OS X, Linux and Windows

This _still_ is __experimental__ software

## Usage

in your `.ghci` add the following, given you need an _additional_ latency of __1ms__, your _device ID_ is __12__ and you want to send commands on MIDI channel __1__:

```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.SimpleSynth

keyStreams <- keyproxy 1 12 keys [1]

[k1] <- sequence keyStreams
```

You can alter the latency to fit your other sources (e.g. audio buffers etc.), but be aware that there is _already 100ms latency added_ to make sure incoming osc commands can be scheduled in the future. Note that the given latency is directly passed to __PortMidi__ `openOutput` which will send real-time __MIDI__ messages for latency __0__ which may or may not be what you want.

To find out a particular Device ID use the supplied tool `tidal-midi-outputs` to list __PortMidi__ device IDs for output devices.

Channels can be multiple, e.g. for polyphonic synthesizers this makes it possible to have separate streams (like d1-9 for dirt) for each midi channel on the same device.

`keys` in this case refers to the `ControllerShape` defined by the example simple synth. See [other supported synths](#supported-synths)

Eventually run `ghci -XOverloadedStrings` and send __MIDI__ commands to a compliant device (software synth).

```haskell
k1 $ note "50*4" |+| slow 2 (kcutoff (scale 0.2 0.9 tri1))
```

## Supported Synths

### Korg Volca Keys

### Korg Volca Bass

### Waldorf Blofeld


### DSI Tetra


## Known issues and limitations

- SysEx support is missing but possible

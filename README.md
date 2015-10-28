# tidal-midi
Tidal module for sending patterns over MIDI.

__PortMIDI__ variant. Should work on OS X, Linux and Windows

This _still_ is __experimental__ software

## Installation

Simply do

```shell
~$ cabal install tidal-midi
```

__Note:__ On OS X with GHC 7.10 it is necessary to reinstall PortMidi again with frameworks correctly linked:

```shell
cabal install portmidi --ghc-options="-optl-Wl,-framework,CoreMIDI,-framework,CoreAudio" --reinstall --jobs=1 --force-reinstalls
```

### Installation from source

If you want, you can also install tidal-midi from source. You will have to clone this repository and install from source:

```shell
~$ git clone https://github.com/tidalcycles/tidal-midi.git
~$ cd tidal-midi
~/tidal-midi$ cabal install
```

After that you can import Sound.Tidal.MIDI.Output within Haskell. To make use of tidal-midi within emacs and running along with tidal, depending on your editor you will have to edit the load script for tidal.

### Setup for emacs

Within your `tidal.el` script, locate the function `tidal-start-haskell` and add:

```emacs
(tidal-send-string "import Sound.Tidal.MIDI.Output")
```

after

```emacs
(tidal-send-string "import Sound.Tidal.Context")
```

Additionally you will have to add lines to import the synth you want to control via MIDI, e.g. `(tidal-send-string "import Sound.Tidal.SimpleSynth")` as well as the initialization commands for streams:

```emacs
(tidal-send-string "keyStreams <- midiproxy 1 \"SimpleSynth virtual input\" [(keys, 1)]")
(tidal-send-string "[t1] <- sequence keyStreams")
```
For adding the MIDI device "SimpleSynth virtual input" and control it via MIDI channel 1. With this set up you will be able to use it via e.g. `t1 $ note "50"`

Synth specific usage instructions can be found below. Note that these are simply assuming you are running tidal-midi directly via ghci command line. If you want any of the other synths within emacs, you will have to edit your `tidal.el` accordingly.

## Usage

in your `.ghci` add the following, given you need an _additional_ latency of __1ms__, your _device name_ is __SimpleSynth virtual input__ and you want to send commands on MIDI channel __1__:

```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.SimpleSynth

keyStreams <- midiproxy 1 "SimpleSynth virtual input" [(keys, 1)]

[k1] <- sequence keyStreams
```

You can alter the latency to fit your other sources (e.g. audio buffers etc.), but be aware that there is _already 100ms latency added_ to make sure incoming osc commands can be scheduled in the future. Note that the given latency is directly passed to __PortMidi__ `openOutput` which will send real-time __MIDI__ messages for latency __0__ which may or may not be what you want.

To find out a particular device name you can use `aconnect -o` on linux and the __Audio MIDI Setup__ on Mac OS X.

Channels can be multiple, e.g. for polyphonic synthesizers this makes it possible to have separate streams (like d1-9 for dirt) for each midi channel on the same device.

`keys` in this case refers to the `ControllerShape` defined by the example simple synth. See [other supported synths](#supported-synths)

Eventually run `ghci -XOverloadedStrings` and send __MIDI__ commands to a compliant device (software synth).

```haskell
k1 $ note "50*4" |+| slow 2 (modwheel (scale 0.2 0.9 tri1))
```

The simple synth comes with _simple_ MIDI parameters, that any device should understand:

* modwheel
* balance
* expression
* sustainpedal

all of these parameters map the given values from __0..1__ to MIDI values ranging from __0..127__.

## Supported Synths

### Korg Volca Keys

Example:
```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.VolcaKeys

keyStreams <- midiproxy 1 "VolcaKeys" [(keys, 1)]

[k1] <- sequence keyStreams
```

### Korg Volca Bass

Example:
```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.VolcaBass

bassStreams <- midiproxy 1 "VolcaBass" [(bass, 1)]

[k1] <- sequence bassStreams
```


### Korg Volca Beats

Example:
```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.VolcaBeats

beatStreams <- midiproxy 1 "VolcaBeats" [(beats, 1)]

[k1] <- sequence beatStreams
```

### Waldorf Blofeld

Example:

```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.Blofeld

keyStreams <- midiproxy 1 "Waldorf Blofeld" [(keys, 1)]

[k1] <- sequence keyStreams
```

### DSI Tetra

#### Example

assumes the following Tetra Global parameters:

* `Multi mode`: __On__
* `M Param Rec`: __NRPN__
* `MIDI Channel`: __1__

```haskell
import Sound.Tidal.MIDI.Output
import Sound.Tidal.Tetra

keyStreams <- midiproxy 1 "DSI Tetra" [(keys 1),(keys, 2),(keys, 3),(keys, 4)]

[k1,k2,k3,k4] <- sequence keyStreams
```

## Known issues and limitations

- SysEx support is there but really limited to work with the Waldorf Blofeld

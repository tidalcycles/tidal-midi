#tidal-midi [![Build Status](https://travis-ci.org/tidalcycles/tidal-midi.svg?branch=master)](https://travis-ci.org/tidalcycles/tidal-midi)

A [TidalCycles](http://tidalcycles.org) module for sending patterns over MIDI.

__PortMIDI__ variant. Should work on OS X, Linux and Windows.

This _still_ is __experimental__ software.

<ul>
<li><a href="#installation">Installation</a>
  <ul>
    <li><a href="#prerequisites">Prerequisites</a></li>
    <li><a href="#install">Install tidal-midi</a></li>
  </ul>
</li>
<li><a href="#usage">Usage</a>
  <ul>
  <li><a href="#mididevices">MIDI devices on your system</a></li>
  <li><a href="#boot">Boot tidal-midi</a></li>
  <li><a href="#playingpatterns">Playing Patterns</a>
    <ul>
      <li><a href="#veldur">Note length, velocity, and other MIDI CC parameters</li>
    </ul>
  </li>
  <li><a href="#custommidichannels">Custom MIDI Channels</a></li>
  <li><a href="#multichannel">Multiple MIDI Channels</a></li>
  <li><a href="#defaultsynthcontroller">The default synthController</a></li>
  </ul>
</li>
<li><a href="#supportedsynths">Supported Synthesizers</a></li>
<li><a href="#custommappings">How to write your own synth mapping</a></li>
<li><a href="#emacs">Automatic bootup in Emacs</a></li>
<li><a href="#known_issues">Known Issues</a></li>
</ul>

<a name="installation"></a>
# Installation

<a name="prerequisites"></a>
## Prerequisites

Depending on your operating system, you will need to install some prerequisites
first.

### All Systems

`tidal-midi` requires the latest version of `tidal`. Run these two commands
in your terminal to install the latest version:

```shell
cabal update
cabal install tidal
```

### Linux

Run the following to install `libasound2-dev` and `libportmidi-dev`:

```shell
apt-get install libasound2-dev and libportmidi-dev
```

### Mac OS X

Install PortMIDI:

```shell
brew install portmidi
```

<a name="install"></a>
## Install tidal-midi

Simply do:

```shell
cabal update
cabal install tidal-midi
```

__Note:__ On OS X with GHC 7.10 it is necessary to reinstall PortMidi again with
frameworks correctly linked:

```shell
cabal install portmidi --ghc-options="-optl-Wl,-framework,CoreMIDI,-framework,CoreAudio" --reinstall --jobs=1 --force-reinstalls
```

<a name="usage"></a>
# Usage

_This guide assumes you are already familiar with Tidal and creating patterns
with samples._

<a name="mididevices"></a>
## Get the names of MIDI devices on your system

In order to use `tidal-midi` you will need the _exact_ name of a MIDI
device on your system. You can get a list of MIDI devices on your system
by running some code in a regular `.tidal` file.

Assuming you're using the Atom editor, create a new file and save it with
a `.tidal` extension (e.g. `midi-test.tidal`). Then, type the following in
the editor:

```haskell
import Sound.Tidal.MIDI.Context

displayOutputDevices >>= putStrLn
```

Evalulate both of those above lines separately using `Shift+Enter` in Atom.
After evaluating the last line, it will output a list of MIDI devices
in your editor (in Atom, at the bottom output panel).

After listing MIDI devices on your system, take note of the device name you
will use. Devices names are case-sensitive.

For the purposes of this guide, we'll assume your device name is "USB MIDI Device".

> You only need to do this step whenever you want to get a list of devices.
> Once you take note of your system's device names, you don't need to perform
> this step ever again (unless you acquire a new MIDI device).

<a name="boot"></a>
## Boot tidal-midi

Make sure you're currently working in a file with a `.tidal` extension in
your editor (it could be the same file from the device list step above).
Then type these three lines of bootup code:

```haskell
import Sound.Tidal.MIDI.Context

devices <- midiDevices

m1 <- midiStream devices "USB MIDI Device" 1 synthController
```

Evaluate each of those lines (use `Shift+Enter` in the Atom
editor). Now Atom is ready to run MIDI patterns using `m1`.

> In the last line of the boot code above, the last three parameters
> are the most important:
>
> - "USB MIDI Device" is the name of your device
> - 1 is the MIDI channel number
> - synthController is the type of synthesizer code to use (you can use custom ones)

<a name="playingpatterns"></a>
## Playing patterns on your device

The following code will play a very simple pattern on middle-C:

```haskell
m1 $ note "0"
```

Above, the `note` param indicates a MIDI note, where `0` equals middle-C. The
following pattern plays a major scale:

```haskell
m1 $ note "0 2 4 5 7 9 11 12"
```

Alternatively, you can use `midinote` to explicitly use a MIDI note from 0 to 127:

```haskell
m1 $ midinote "60 62 64 65 67 69 71 72"
```

You can use normal TidalCycles pattern transform functions to change `tidal-midi`
patterns:

```haskell
m1 $ every 3 (rev) $ every 2 (density 2) $ note "0 2 4 5 7 9 11 12"
```

<a name="veldur"></a>
### Note length, velocity, and other MIDI CC parameters

Note length and velocity are controlled using the `dur` and `velocity`
parameters, respectively.

The value of `dur` is given in seconds:

```haskell
m1 $ note "0 2" # dur "0.05 0.2"

m1 $ note "0 2" # dur (scale 0.05 0.3 $ slow 1.5 tri1)
```

Alternatively, the `legato` parameter tells Tidal to scale the note
duration to fill it's "slot" in the pattern.  For example, the following
will give four notes each a quarter cycle in duration (values of legato
  greater or less than one will multiply the duration):

```haskell
m1 $ note "0 1 0 2" # legato "1"
```

`velocity` has a range from *0 to 1*, and equates to MIDI values *0 to 127*:

```haskell
m1 $ note "0 2 4 5 7 9 11 12" # velocity "0.5 0.75 1"

m1 $ note "0 2 4 5 7 9 11 12" # velocity (scale 0.5 1 $ slow 1.5 saw1)
```

The `synthController` has some params that support MIDI Change Control messages,
such as the mod wheel:

```haskell
m1 $ note "0 2 4 5 7 9 11 12" # modwheel "0.1 0.4 0.9"
```

Details about the default MIDI CC messages can be found in the
[default synth controller](#defaultsynthcontroller) section below.

MIDI CC params can have decimal values in the range *0 to 1*, which map to MIDI
CC values *0 to 127*.

_Custom synthesizer implementations may implement additional MIDI CC parameters.
Please refer to the [supported synths](doc/synths.md) for more information._

<a name="custommidichannels"></a>
## Custom MIDI Channels

Let's review this line from the boilerplate code above:

```haskell
m1 <- midiStream devices "USB MIDI Device" 1 synthController
```

The 2nd to last parameter on that line indicates the channel number. Let's say
your device is running on channel 7. You can specify channel 7 by changing the
2nd to last parameter:

```haskell
m1 <- midiStream devices "USB MIDI Device" 7 synthController
```

<a name="multichannel"></a>
## Multiple MIDI Channels

`tidal-midi` supports devices with multiple channels so that you can create
patterns on each channel separately:

```haskell
m1 <- midiStream devices "USB MIDI Device" 1 synthController
m2 <- midiStream devices "USB MIDI Device" 2 synthController
m5 <- midiStream devices "USB MIDI Device" 5 synthController

m1 $ note (run 4) # velocity "0.5"
m2 $ note "0*2 5 7" # dur "0.1"
m5 $ midinote "36 60"
```

> Note: at the time of this writing, multiple channels can cause scheduling
> problems if the synth controller's latency is too low. This is mainly an
> issue for `tidal-midi` developers to improve, but users may be impacted.
> Latency values can be increased by modifying the synth controller's source
> code (e.g. in `SimpleSynth.hs`), then re-compiling `tidal-midi`
> with `cabal install`.

<a name="defaultsynthcontroller"></a>
## The default synthController (a.k.a "simple synth")

The simple synth comes with _simple_ MIDI parameters, that any device should understand:

* modwheel (MIDI CC #1)
* balance (MIDI CC #8)
* expression (MIDI CC #11)
* sustainpedal (MIDI CC #64)

All of these parameters map the given values from __0..1__ to MIDI values ranging from __0..127__.

You can use all of these parameters like the familiar synth parameters in
TidalCycles:

```haskell
m1 $ note "0*8" # modwheel "0.25 0.75" # balance "0.1 0.9" # expression (sine1)
```

__All tidal-midi synthesizers__ "inherit" from the "simple synth" and
automatically expose these same parameters.

See the section below on [Supported Synthesizers](#supportedsynths) for details
on custom controllers for popular hardware synthesizers.

<a name="supportedsynths"></a>
# Supported Synthesizers

A variety of custom mappings have been created in `tidal-midi` for popular hardware synthesizers.
Click on a device below to get details on its usage:

* [DSI Tetra](doc/synths.md#dsi-tetra)
* Elektron Analog RYTM
* [Korg Volca Bass](doc/synths.md#korg-volca-bass)
* [Korg Volca Beats](doc/synths.md#korg-volca-beats)
* [Korg Volca Keys](doc/synths.md#korg-volca-keys)
* Roland System-1M
* Synthino
* [Waldorf Blofeld](doc/synths.md#waldorf-blofeld)


<a name="custommappings"></a>
# How to write your own synth mapping

Interested in using `tidal-midi` with your own synthesizer? Please read the guide on [Writing a new synth mapping](doc/synth-mapping.md).


<a name="emacs"></a>
# Automatic startup in Emacs

Within your `tidal.el` script, locate the function `tidal-start-haskell` and add:

```emacs
(tidal-send-string "import Sound.Tidal.MIDI.Context")
```

after

```emacs
(tidal-send-string "import Sound.Tidal.Context")
```

Additionally you will have to add lines to import the synth you want to control via MIDI, e.g. `(tidal-send-string "import Sound.Tidal.VolcaKeys")` as well as the initialization commands for streams:

```emacs
(tidal-send-string "devices <- midiDevices")
(tidal-send-string "t1 <- midiStream devices \"USB MIDI Device\" 1 synthController")
```

The above code adds the MIDI device "USB MIDI Device" and controls it via MIDI channel 1.
With this set up you will be able to use it via e.g. `t1 $ note "50"`


<a name="known_issues"></a>
# Known issues and limitations

- SysEx support is there but really limited to work with the Waldorf Blofeld

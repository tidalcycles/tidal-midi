# Supported Synths

In addition to the simple synth (implemented by `synthController`), there are
other custom implementations that support popular hardware synths:

## Korg Volca Keys
<a name="korg-volca-keys"></a>

Example:
```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.MIDI.VolcaKeys

devices <- midiDevices
m1 <- midiStream devices "USB MIDI Device" 1 keysController
```

## Korg Volca Bass
<a name="korg-volca-bass"></a>

Example:
```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.MIDI.VolcaBass

devices <- midiDevices
m1 <- midiStream devices "USB MIDI Device" 1 bassController
```


## Korg Volca Beats
<a name="korg-volca-beats"></a>

Example:
```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.MIDI.VolcaBeats

devices <- midiDevices
m1 <- midiStream devices "USB MIDI Device" 1 beatsController
```

## Waldorf Blofeld
<a name="waldorf-blofeld"></a>

Example:

```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.Blofeld

devices <- midiDevices
m1 <- midiStream devices "USB MIDI Device" 1 blofeldController

m1
$ note "[7,14] ~ 4 9"
# ae_rel "0.5"
# osc1shape "4"
```

## DSI Tetra
<a name="dsi-tetra"></a>

### Example

assumes the following Tetra Global parameters:

* `Multi mode`: __On__
* `M Param Rec`: __NRPN__
* `MIDI Channel`: __1__

```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.Tetra

devices <- midiDevices
m1 <- midiStream devices "USB MIDI Device" 1 polysynth
m2 <- midiStream devices "USB MIDI Device" 2 polysynth
m3 <- midiStream devices "USB MIDI Device" 3 polysynth
m4 <- midiStream devices "USB MIDI Device" 4 polysynth
```

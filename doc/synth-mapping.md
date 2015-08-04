# Writing a new synth mapping

If a MIDI device you own is not yet supported by tidal-midi you can write a Haskell module to map specific functions to certain parameters of your synth. Since every synth has a more or less different set of parameters the documentation of the various synthesizer parameters and their corresponding MIDI CC number is crucial for writing such a module.

Most user guides for MIDI devices contain an appendix listing all of the supported MIDI commands it can receive. If you haven't got a digital copy of the user guide a simple way to get one is googling it. Due to recent request I will use the Waldorf Streichfett as an example of how to write such a mapping, starting with finding the original user guide pdf and the MIDI mapping information therein. Note that for your own synth MIDI CC values will be different and not all synths have the same capabilities (i.e. you cannot control the same parameters).

## A new synth mapping for _Waldorf Streichfett_

WE Google up the user guide for the synthesizer: waldorf streichfett user guide pdf

Top hit is the [original download link by waldorf music](http://www.waldorf-music.info/downloads/Streichfett/Streichfett%20Manual%20EN.pdf). In any case you should resort to the manufacturers downloads to avoid using an incorrect or otherwise different documentation. Make sure you have the same version of the device as written in the PDF, in this case there doesn't seem to be any revised or newer version of the Streichfett, so we choose this one.

Within the PDF find the section with MIDI Controller numbers and implementation. Most of the time and certainly in this case it can be found within the appendix of the manual. Page 21 lists the MIDI Controller numbers for the synth.

(To keep things simple, I'll not dig into the MIDI implementation listed afterwards, which makes it possible to gain even more control over the synth, this can be done in another tutorial)

As soon as we have these information at hand, we can fire up an editor and start writing our new Haskell module for tidal-midi. ([Make sure you have cloned this repository first](https://github.com/lennart/tidal-midi)

We create a new file within `Sound/Tidal/` in the source folder of tidal-midi called `Streichfett.hs`. In the future it might be wise to start adding namespaces for the various vendors to avoid name collisions (e.g. Waldorf/Streichfett) but for now its fine.

Within this file we first declare our module within Tidal namespace:

```haskell
module Sound.Tidal.Streichfett where
```

followed by two needed imports that give access to a few functions I'll list briefly:

```haskell
import Sound.Tidal.Stream (makeI, makeF)
```
taken from Tidal itself we import `makeI` and `makeF` to create parameter functions. These will be used to map our parameter names to named functions. We'll see how these are used later on.

```haskell
import Sound.Tidal.MIDI.Control
```
To create a configuration for Streichfett we need to import the ControllerShape type to define what kind of parameters this synthesizer has and which Midi controller numbers it uses.

We start by defining a ControllerShape that will be passed to any new streams to talk to the Streichfett. You can think of this shape as a way to tell tidal how to create messages sent to the Streichfett. Every time you want to talk to Streichfett, make sure you pass in this shape.

### Looking at how things tie together
If you remember using tidal-midi the first time, you had to set up your editor to load a module in addition to `Sound.Tidal.MIDI.Output` namely the synthesizer mapping module, e.g. `Sound.Tidal.SimpleSynth`. SimpleSynth defines its own ControllerShape as well, named __keys__. So in order to talk to SimpleSynth via Tidal we need to create a stream we can use from within Haskell that writes messages SimpleSynth can understand, and the __keys__ shape will tell tidal how to do it:

```haskell
keyStreams <- keyproxy 1 "SimpleSynth virtual input1" keys [1]
[k1] <- sequence keyStreams
```

notice how we pass __keys__ to the keyproxy that will in turn create a stream for us that we can later use via `k1`, e.g. `k1 $ note "50" |+| modwheel "0.4"`

So when we are finished with our new module, we'll use `Sound.Tidal.Streichfett` instead and pass in our own controller shape instead of `keys`. Double check this when we are finished, since otherwise you might be talking to your synth with a wrong controller shape and weird things, or, what would be bad: nothing happens.

### Defining our own `ControllerShape`
At first we define the simplest controller shape we can imagine defining only one parameter for the Streichfett along all need configuration options for a full shape:

```haskell
fett :: ControllerShape
fett = ControllerShape {params = [
                          mCC "phaser" 93
                        ],
                        duration = ("dur", 0.05),
                        latency = 0.1}
```

That's a lot at first, some things may not be of interest for you now, so let's focus on the main parts. We define our shape named `fett` which is of type `ControllerShape`. We give it three options: `params`, `duration` and `latency`. Ignoring duration and latency for now, `params` is what tells tidal to map names to MIDI control numbers. In this case we define one name `phaser` with the controller number `93` as taken from the Streichfett manual.

### Creating our parameter functions
Great, tidal-midi now knows about the shape of Streichfett messages (At least, that the `phaser` param is controlled via controller number 93) but sadly we cannot tell tidal to actually change values for this param through patterns. What we will do now is creating something like a handle to send value changes to the `phaser` param

```haskell
oscFett = toOscShape fett

phaser = makeF oscFett "phaser"
```

And that's it. If you really cannot wait, this is the minimal working example of your own synth mapping. One last step is needed for really making use of it, namely telling the tidal-midi package about your module. Just add a line to `tidal-midi.cabal` under the key `exposed-modules`:

```
...
library
    Exposed-modules: Sound.Tidal.MIDI.Output
                   Sound.Tidal.MIDI.Device
                   Sound.Tidal.MIDI.Control
                   Sound.Tidal.SimpleSynth
                   Sound.Tidal.VolcaKeys
                   Sound.Tidal.VolcaBass
                   Sound.Tidal.Blofeld
                   Sound.Tidal.Tetra
                   Sound.Tidal.Rytm
                   Sound.Tidal.Synthino
                   Sound.Tidal.Streichfett
...
```

After that do a cabal install within tidal-midi source.

### Using our custom synth mapping in emacs

To use your mapping, we need to integrate it within emacs. If you already have a setup for tidal-midi for e.g. SimpleSynth, the following changes need to be made within your `tidal.el`. Instead of importing `Sound.Tidal.SimpleSynth` import `Sound.Tidal.Streichfett` and instead of passing `keys` to `keyproxy` pass in `fett`.

Restart emacs, start up tidal and try out your new phaser param (assuming k1 is the name of your tidal midi stream):

```haskell
k1 $ note "50(5,8)" |+| slow 3 (phaser sine1)
```

have fun and implement all the parameters!

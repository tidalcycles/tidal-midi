# Writing a new synth mapping

If a MIDI device you own is not yet supported by tidal-midi you can write a Haskell module to map specific functions to certain parameters of your synth. Since every synth has a more or less different **set of parameters** the documentation of the various synthesizer parameters and their **corresponding MIDI CC** number is crucial for writing such a module.

Most user guides for MIDI devices contain an appendix listing all of the supported MIDI commands it can receive. If you haven't got a digital copy of the user guide a simple way to get one is googling it. Due to recent request I will use the Waldorf Streichfett as an example of how to write such a mapping, starting with finding the original user guide pdf and the MIDI mapping information therein.

Note that for your own synth MIDI CC values will be different and not all synths have the same capabilities (i.e. you cannot control the same parameters).

## A new synth mapping for _Waldorf Streichfett_

### Find the MIDI control table

We Google up the user guide for the synthesizer: waldorf streichfett user guide pdf

Top hit is the [original download link by waldorf music](http://www.waldorf-music.info/downloads/Streichfett/Streichfett%20Manual%20EN.pdf). In any case you should resort to the manufacturers downloads to avoid using an incorrect or otherwise different documentation. Make sure you have the same version of the device as written in the PDF, in this case there doesn't seem to be any revised or newer version of the Streichfett, so we choose this one.

Within the PDF find the section with MIDI Controller numbers and implementation. Most of the time and certainly in this case it can be found within the appendix of the manual. Page 21 lists the MIDI Controller numbers for the synth.

(To keep things simple, I'll not dig into the MIDI implementation listed afterwards, which makes it possible to gain even more control over the synth, this can be done in another tutorial)

As soon as we have these information at hand, we can fire up an editor and start writing our new Haskell module for tidal-midi. ([Make sure you have cloned this repository first](https://github.com/tidalcycles/tidal-midi))

### Write the Haskell Module file

We create a new file within `Sound/Tidal/MIDI/` from the source folder of tidal-midi called `Streichfett.hs`.

Within this file we first declare our module within Tidal namespace:

```haskell
module Sound.Tidal.MIDI.Streichfett where
```

followed by two needed imports that give access to a few functions:

```haskell
import Sound.Tidal.Params
```

taken from Tidal itself we import `Params` to create parameter functions. These will be used to map our parameter names to named functions. We'll see how these are used later on.

```haskell
import Sound.Tidal.MIDI.Control
```

To create a configuration for Streichfett we need to import the `ControllerShape` type to define what kind of parameters this synthesizer has and which Midi controller numbers it uses.

We start by defining a new `ControllerShape` that will be passed to any new streams to talk to the Streichfett. You can think of this shape as a way to tell tidal how to create messages sent to the Streichfett. Every time you want to talk to Streichfett, make sure you pass in this shape.


#### Defining our own `ControllerShape`
At first we define the simplest controller shape we can imagine defining only one parameter for the Streichfett along all need configuration options for a full shape:

```haskell
fettController :: ControllerShape
fettController = ControllerShape {
  controls = [
    mCC phaser_p 93
    ],
  latency = 0.1
  }
```

That's a lot to take in at first and some things may not be of interest for you now, so let's focus on the main parts. We define our shape named `fett` which is of type `ControllerShape`. We give it two options: `controls` and `latency`.

`controls` is a list to tell tidal how to map `Param` to MIDI control numbers. We map a new `Param` `phaser_p` to control change number `93` (see Streichfett manual).
`latency` in seconds, tells tidal-midi streams to delay sending of midi messages by given time, so in this case **100ms**. Increase if you experience **late messages**!

#### Creating our parameter functions
Great, tidal-midi now knows about the shape of Streichfett messages, but it doesn't _yet_ know what `phaser_p` is.

```haskell
(phaser,  phaser_p) = pF "phaser" (Just 0)
```

`phaser_p` is a floating point tidal param, `pF` named "phaser" and has a default of `0`.
`phaser` is an action to make a pattern using our new param.

We already use `phaser_p` in our controller shape and can play with `phaser` later!

...and that's it! If you really cannot wait, this is the minimal working example of your own synth mapping.

#### Installing the mapping

One last step is needed for really making use of it, namely telling the tidal-midi package about your module. Just add a line to `tidal-midi.cabal` under the key `exposed-modules`:

```
...
library
  Exposed-modules: Sound.Tidal.MIDI.Context
                   Sound.Tidal.MIDI.Control
                   Sound.Tidal.MIDI.Device
                   Sound.Tidal.MIDI.KorgKP3
                   Sound.Tidal.MIDI.Stream
                   Sound.Tidal.MIDI.Streichfett
                   Sound.Tidal.MIDI.Synth
                   Sound.Tidal.MIDI.SynthParams
                   Sound.Tidal.MIDI.System1M
                   Sound.Tidal.MIDI.VolcaBass
                   Sound.Tidal.MIDI.VolcaBeats
                   Sound.Tidal.MIDI.VolcaKeys
...
```

After that do a ```cabal install``` within tidal-midi source.

## Tying it all together in Tidal

### Recap: tidal-midi boot

Maybe you remember: we `import Sound.Tidal.MIDI.Context` to make tidal-midi work. This also gives us a simple synth that defines its own `ControllerShape` named __synthController__.

So without having a custom tidal mapping for a synthesizer we just needed to create a stream to use from within Haskell. This stream wrote messages any simple synth could understand, and the __synthController__ shape told tidal how to do it:

```haskell
devices <- midiDevices
k1 <- midiStream devices "USB MIDI Device" 1  synthController
```

notice how we passed __synthController__ to the midiStream that in turn created a stream for us that we can later use via `k1`, e.g. `k1 $ note "50" |+| modwheel "0.4"`

So when you are finished with your new module, additionally `import Sound.Tidal.MIDI.YourSynthName` pass in your own controller shape `yourSynthNameController` instead of `synthController`.

In the following example, we stick to `Streichfett` as _your synth name_.

### Using the Streichfett module

To try your module, open or restart your editor for tidal (or `cd ~ && ghci -XOverloadedStrings`) and evaluate each of the following codes line by line, first imports:

```haskell
import Sound.Tidal.MIDI.Context
import Sound.Tidal.MIDI.Streichfett
```

Then the usual steps to list the midi output devices and create the MIDI devices:

```haskell
displayOutputDevices >>= putStrLn
devices <- midiDevices
```

Finally, we can define our midi stream `f1`:

```haskell
f1 <- midiStream devices "USB Midi MIDI 1" 1 fettController
```

Double check the device **name** **channel** and `ControllerShape` when you are finished,  
otherwise weird things, or, what would be worse: _nothing happens_.

Then try out your new param:

```haskell
f1 $ note "50(5,8)" |+| slow 3 (phaser sine1)
```

Have fun and implement all the parameters but remember to reinstall via `cabal install` and restart your editor before you can use newly written parameters!


### Using our custom synth mapping in emacs

To use your mapping, we need to integrate it within emacs. If you already have a setup for tidal-midi the following changes need to be made within your `tidal.el`. In addition to importing `Sound.Tidal.MIDI.Context` import `Sound.Tidal.MIDI.Streichfett` and create a new stream `f1` and pass `fettController` to `midiStream` instead of `synthController`.

Restart emacs, start up tidal and try out the Streichfett specific phaser param on `f1`.

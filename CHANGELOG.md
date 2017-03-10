# Changelog

## 0.9

- normalize Shape names as lowercased Controller Module name, e.g.
  if you `import Sound.Tidal.MIDI.VolcaKeys` use `midiStream "Device Name" 1 volcakeys`
- default latency of `0.1` to work around late messages _most of the time_
- drop default values in `ControlChange`, can be specified via Tidal `Param` helpers
  `pF`, `pS` and `pI`
- default `n` to `128` to allow CC-only patterns, e.g. `m1 $ resonance "0.3 0.1 0.9"`
- params that _leave_ patterns will be reset to their defaults
- `unit "cycle"` calculates inter-onsets and tries to behave like SuperDirt
- `Stream` is now just the Tidal communication layer and internal MIDI
  messaging/scheduling lives in `Output`

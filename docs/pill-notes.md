# Notes on compiling traps for jets

- Core composed type is `(trap vase)` i.e. a one-armed no-sample core
  which produces a vase as the result of the arm.
- Get text of a Hoon file: `.^(@t cx+/path/to/file/hoon)`
- `+rain` parses a hoon file including ford runes
- `+mist` in lib/pill/hoon skips ford runes, but it's not exported so
  need to copypaste
- `+swat` is a deferred `+slap` that takes a `(trap vase)` and some Hoon 
- pattern is `(swat <subject> (mist <path> .^(@t <path>)))`
- would be good to package this in a generator

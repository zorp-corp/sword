# Developing Sword

## Rust

### Build

To build Sword, make sure Rust is installed, then run:

```bash
cargo build
```

to build the Sword executable. This will place the built executable at `target/debug/sword` under the `rust/sword` directory.

#### Pills

Sword development and testing, unlike regular development and ship operation, currently requires careful control over what pill is used to launch a ship. Currently, there are several pills available in `resources/pills/`:
- **baby.pill**: an extremely minimal Arvo-shaped core and Hoon standard library (`~wicdev-wisryt` [streamed a
video of its development](https://youtu.be/fOVhCx1a-9A))
- **toddler.pill**: a slightly more complex Arvo and Hoon than `baby`, which runs slow recursive operations for testing jets
- **azimuth.pill**: a pill that processes an Azimuth snapshot
- **full.pill**: the complete Urbit `v2.11` pill
- **slim.pill**: a slimmed down version of the Urbit `v2.11` pill that has had every desk and agent not necessary for booting to dojo removed

More information on the pills used by Sword can be found [here](https://github.com/zorp-corp/sword/blob/status/docs/pills.md).

### Test

The command to run the Sword suite of unit tests is:

```bash
cargo test --verbose -- --test-threads=1
```

The tests must be run with `-- --test-threads=1` because Rust does not have any way to specify test setup / teardown functions, nor does it have any way to
specify ordered test dependencies. Therefore, the only way to ensure that tests that share resources don't clobber each other **and** that tests setup / teardown in the right order is to force all unit tests to be single-threaded.

### Style

Sword uses the default Rust formatting and style. The CI jobs are configured to reject any code which produces linter or style warnings. Therefore, as a final step before uploading code changes to GitHub, it's recommended to run the following commands:

```bash
cargo fmt
cargo clippy --all-targets --no-deps -- -D warnings -A clippy::missing_safety_doc
```

This will auto-format your code and check for linter warnings.

### Watch

To watch rust and check for errors, run

```bash
cargo watch --clear
```

Until terminated with ctrl-c, this will rebuild Sword library on any change to the underlying source files and report any warnings and errors. It will *not* produce the executable. You must run the build command above to rebuild the executable.

## Hoon

The Nock analysis and lowering for Sword is written in Hoon, and lives at `hoon/codegen.` It is meant to be jammed and included in the Sword binary. (See [`src/load.rs`](rust/sword/src/load.rs) in the Rust sources for details.)

If the hoon source has been synced to a desk, e.g. `sandbox`, on a fakezod, then the build generator can be invoked as:

```
.cg/jam +sandbox!cg-make
```

This will build the Hoon standard library and the Sword Nock analysis as a "trap" meant to be run by Sword. The jammed output can be found at `<fakezod-pier>/.urb/put/cg.jam`, and should be copied to the `rust/sword/bin` directory, from whence the rust build will include it in the executable.

Instructions on testing the analysis in a fakezod are forthcoming.

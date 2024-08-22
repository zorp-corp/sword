# Developing Sword

## Nix
Sword uses a Nix developer shell to set up the environment for rust builds. Please [Install Nix](https://nixos.org/download#download-nix).
With Nix installed, you can run

```bash
nix develop
```

in `rust/` or any subdirectory, and you will be dropped into a BASH shell with the build environment set up. This will provide proper versions of non-rust dependencies, as well as the rust environment.

If you receive the error

```
error: experimental Nix feature 'nix-command' is disabled; use '--extra-experimental-features nix-command' to override
```

edit your `$HOME/.config/nix/nix.conf` to include the line

```
extra-experimental-features = nix-command
```

## Rust

### Build

To build Sword, start a nix development shell as above. Within the shell, in the `rust/sword` directory, you can run:

```bash
cargo build
```

to build the Sword executable. This will place the built executable at `target/debug/sword` under the `rust/sword` directory.

### Run

Sword is made to run as an urbit "serf", meaning it is intended to be invoked by a "king" which sends it commands and performs side-effects specified by its output. We use the Vere king.

To run the Vere king with Sword as serf, it's necessary to modify the Vere king to launch Sword instead of its own serf. This is done by modifying the executable of the serf protocol in the `u3_lord_init` function in `lord.c` of the Vere source:

```C
// arg_c[0] = god_u->bin_c;
arg_c[0] = "/path/to/sword/repo/rust/sword/target/debug/sword";
```

Then, it is necessary to follow the [Vere build instrcutions](https://github.com/urbit/vere/blob/develop/INSTALL.md). (You should exit the `nix develop` shell first for such a build.) Afterwards, it's possible to launch Vere with Sword as the serf using the usual commands:

```bash
bazel-bin/pkg/vere/urbit -F zod
```

#### Pills

Sword development and testing, unlike regular development and ship operation, currently requires careful control over what pill is used to launch a ship. Currently, there are several pills available in `resources/pills/`:
- **baby.pill**: an extremely minimal Arvo-shaped core and Hoon standard library (`~wicdev-wisryt` [streamed a
video of its development](https://youtu.be/fOVhCx1a-9A))
- **toddler.pill**: a slightly more complex Arvo and Hoon than `baby`, which runs slow recursive operations for testing jets
- **azimuth.pill**: a pill that processes an Azimuth snapshot
- **full.pill**: the complete Urbit `v2.11` pill
- **slim.pill**: a slimmed down version of the Urbit `v2.11` pill that has had every desk and agent not necessary for booting to dojo removed

More information on the pills used by Sword can be found [here](https://github.com/urbit/sword/blob/status/docs/pills.md).

To launch a ship with a local pill (instead of downloading the default pill from urbit.org), the `-B` option is used:

```bash
bazel-bin/pkg/vere/urbit -F zod -B /path/to/sword/repo/resources/pills/baby.pill
```

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

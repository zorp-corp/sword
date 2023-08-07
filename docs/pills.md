# Running a pill with Ares

We can run a pill with Ares by performing a one-line modification to Vere in order
to use Ares as a serf. This is accurate as of August 2023 - details may have
changed since then.

We first cover how to run `rust/ares/test_data/baby.pill`, then
`hoon/scaffolding/azimuth-pill.hoon`, then how to run your own pill.

## Step 1: Modify Vere

Download the Vere repo and open `/pkg/vere/lord.c`. Search for `u3_lord_init()`.
In this function definition, you will find several lines that look something
like

```
    arg_c[0] = god_u->bin_c;            //  executable
    arg_c[1] = "serf";                  //  protocol
    arg_c[2] = god_u->pax_c;            //  path to checkpoint directory
    arg_c[3] = key_c;                   //  disk key
    arg_c[4] = wag_c;                   //  runtime config
...
```

Change the right hand side of the first line to the path of the Ares executable
inside a string literal (probably something like `rust/ares/target/debug/ares`):

```
arg_c[0] = "/path/to/ares/rust/ares/target/debug/ares";
```

Next we need to compile the new version of Vere that uses Ares as a serf. See
[INSTALL.md](https://github.com/urbit/vere/blob/develop/INSTALL.md) in the Vere
repo on how to do this. Now `cd` to the directory you'd like this version of
Vere to reside in and make a symlink to the `urbit` executable you just
created like so:

```
ln -s path/to/vere/bazel-bin/pkg/vere/urbit urbit
```

## Step 2: Run `baby.pill`

`baby.pill` is an extremely minimal Arvo-shaped core and Hoon standard library
equipped with `%sham` jets needed to run it. ~wicdev-wisryt [streamed a
video](https://youtu.be/fOVhCx1a-9A) of its development. You can find the Hoon
for `baby.pill` in the Ares repo at `/hoon/scaffolding/baby.hoon`, and the
library is `hoon/scaffolding/cradle.hoon`.

The jammed pill is already in the Ares repo at `rust/ares/test_data/baby.pill`.
To run it with our new version of Vere, we make a fakezod with `baby.pill` as
the pill:

```
./urbit -F zod -B /path/to/ares/rust/ares/test_data/baby.pill
```

This will boot a fakezod. If it writes `effect` to the terminal with every
keystroke, you have succeeded!

## Step 3: Run `azimuth-pill.pill`

Next we will show how to build a pill that processes an Azimuth snapshot, called
`azimuth-pill.pill`.

Boot a fakezod using the ordinary Urbit executable (not the one you created
above) and run `|mount %`.

Next, copy the contents of the `hoon/scaffolding/` folder from the Ares repo to
`path/to/fakezod/base/lib` and run `|commit %base`, then

```
.azimuth-pill/pill -build-file %/lib/azimuth-pill/hoon
```

This will make a file `azimuth-pill.pill` in `path/to/fakezod/.urb/put`.

Now we can run this pill with the version of Vere we built above:

```
./urbit -F dev -B /path/to/fakezod/.urb/put/azimuth-pill.pill
```

If you succeeded, you should see the standard boot header followed by `ran a
thousand logs` every few moments. If you're running an optimized build (e.g.
`opt-level = 3`) of Ares this should finish in a few minutes or less. If you're
running an unoptimized build (e.g. `opt-level = 0`) this could take much longer.

## Making your own pill

At time of writing, most jets have not yet been ported to Ares and so you cannot
boot a typical solid pill. However, following the example of `baby.pill` and the
development video linked above, and making use of the simplified standard libary
`cradle.hoon`, you can still get Ares to run some interesting non-trivial Hoon.

After making your own pill, you can write it to disk with `.my-pill/pill
-build-file /path/to/my/pill` from a fakezod, and then run it just like you did
with `baby.pill` and `azimuth-pill.pill` above.

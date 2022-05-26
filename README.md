# cargoless-rust-experiments

Rust without cargo, with the idea of eventually stopping wasting energy.  Some WIP experiments into making rust work with Guix properly.

Currently, the Rust apps 'agate', 'castor', 'diffr', 'hexyl and 'sniffglue' can be built, see guix.scm:

$ guix build -L . -f antioxidant-packages.scm
$ [...]/bin/hexyl
(input some lines)

Warning: some packages have been updated without checking the source code diff!

# Using as a channel (for now)

After a "git pull", do:

```
$ guix git authenticate 020851ad649480ee4769b77a947642e993ea5956 "C1F3 3EE2 0C52 8FDB 7DD7  011F 49E3 EE22 1917 25EE" --keyring=keys
```

You can also use it as a channel, e.g. with the following configuration
```
(use-modules (guix ci))

(cons
 (channel
  (name 'antioxidated-packages)
  (url "https://notabug.org/maximed/cargoless-rust-experiments")
  (introduction
   (make-channel-introduction
    "020851ad649480ee4769b77a947642e993ea5956"
    (openpgp-fingerprint
     "C1F3 3EE2 0C52 8FDB 7DD7  011F 49E3 EE22 1917 25EE"))))
 (list (channel-with-substitutes-available
            %default-guix-channel
            "https://ci.guix.gnu.org")))
```

(TODO: make some vitaminated packages available, test "guix pull")

# How to help

  1. Choose a Rust app or library to build.
  2. guix.scm ends with (map vitaminate/auto (list ...)). Add the package to the list.
  3. Run "guix build -L. -f guix.scm" to build it.
  4. If it builds succesfully, go back to (1) and choose something else.
  5. Fix build failures (see next section)
  6. Send a patch to <https://notabug.org/maximed/cargoless-rust-experiments/pulls>
     or guix-patches@gnu.org.
  7. Repeat.

# Which features are built?

If the package as a #:features #~'("this" "that" ...) argument, then the crate is
build with features this and that and their implied features.

If the package has no such argument and Cargo.toml has a "default" feature, then the
crate is built with the default feature and all its implied features.

If the package has no such argument and Cargo.toml has no "default" feature, then
the crate is built with all features except "nightly" and "unstable".

(for technnical detiails, see 'make-feature-closure' and 'choose-features' in
antioxidant.scm)

# How to fix build failures

## Build failures caused by incompatible features?

What if rust-foo requires the 'yes' feature of rust-bar, and rust-oof requires
the _absence_ of the 'yes' feature of rust-bar?

Then add a context-dependent replacement to %replacements:

```scheme
  ("rust-bar" ,(package-with-rust-features rust-bar #~'() #:metatada "guix-variant=without-yes")
  	      #:for-dependent
	      ,(lambda (dependent)
	         (string=? "rust-oof" (package-name dependent))))
```

and enable the feature by default in %features.

## Build failures caused by missing features.

Does "rust-foo" fail to build because it requires the feature "baz" of "rust-bar"?
Then go to %features in guix.scm and add an entry

  ("rust-bar" ,#~'("default" "baz")).

If there's already an entry for "rust-bar", modify the existing entry to add "baz".

## Build failures due to non-building features

If you see

  warning: #<<crate-mapping> true-name: "packed_simd" local-name: "packed_simd"> not found in the available crates -- this might cause the build to fail!

and

error[E0463]: can't find crate for `packed_simd`
 --> src/simd/generic.rs:1:1
  |
1 | extern crate packed_simd;
  | ^^^^^^^^^^^^^^^^^^^^^^^^^ can't find crate

the solution is probably to remove "simd" or "generic-simd" feature or such,
by making the list of features explicit instead of implicit.

  ("rust-bytecount" ,#~'())

## Build failures related to rust-digest

E.g.:

```
error[E0432]: unresolved imports `digest::BlockInput`, `digest::FixedOutputDirty`
   --> lib/lib.rs:133:14
    |
133 | use digest::{BlockInput, FixedOutputDirty, Reset, Update};
    |              ^^^^^^^^^^  ^^^^^^^^^^^^^^^^
    |              |           |
    |              |           no `FixedOutputDirty` in the root
    |              |           help: a similar name exists in the module: `FixedOutput`
    |              no `BlockInput` in the root
```

The fixed-length API has been removed in <https://github.com/RustCrypto/traits/pull/380>.
To solve the build failure, maybe update the crate that depends on rust-digest@0.9.0
to a version that supports rust-digest@0.10.0.  Alternatively, look if it rust-digest
is only used when a certain feature is enabled, and if so, disable that feature.

## Unstable Rust

If the crate assumes unstable rust (e.g. by using #![feature ...]),
updating the crate often fixes that.

## Version incompatibilities

No method named `foo` from `bar`?  Try updating the crate, maybe an updated crate uses the new API version.  Can be caused by over-eagerly replacing rust-syn by a newer version.  Could be avoided by implementing support for multiple versions of a crate as dependencies.

## Cycles

# Changelog

2022-05-12:
   source code location of binaries can now be inferred.  Previously, only src/main.rs was understood as default and crates had to set the 'target' field explicitely.  But not all crates do that, e.g. rust-alloc-no-stdlib@2.

   'set-platform-independent-manifest-variables' doesn't barf anymore on versions like 1.2.3+4.5.6,
   allowing rust-bzip2-sys to compile.
# cargoless-rust-experiments

Rust without cargo, with the idea of eventually stopping wasting energy.  Some WIP experiments into making rust work with Guix properly.

Currently, the Rust apps 'agate', 'diffr', 'hexyl and 'sniffglue' can be built, see guix.scm:

$ guix build -L . -f guix.scm
$ [...]/bin/hexyl
(input some lines)

Warning: some packages have been updated without checking the source code diff!

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
the crate is built with all features except "nightly".

(for technnical detiails, see 'make-feature-closure' and 'choose-features' in
antioxidant.scm)

# How to fix build failures

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
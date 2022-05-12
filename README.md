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

# How to fix build failures

## Build failures caused by missing features.

Does "rust-foo" fail to build because it requires the feature "baz" of "rust-bar"?
Then go to %features in guix.scm and add an entry

  ("rust-bar" ,#~'("default" "baz")).

If there's already an entry for "rust-bar", modify the existing entry to add "baz".

## Version incompatibilities

## Cycles

# Changelog

2022-05-12: source code location of binaries can now be inferred.  Previously, only src/main.rs was understood as default and crates had to set the 'target' field explicitely.  But not all crates do that, e.g. rust-alloc-no-stdlib@2.
# cargoless-rust-experiments

Rust without cargo, with the idea of eventually stopping wasting energy.  Some WIP experiments into making rust work with Guix properly.

Currently, the Rust app 'hexyl' can be built, see guix.scm:

$ guix build -L . -f guix.scm
$ [...]/bin/hexyl
(input some lines)
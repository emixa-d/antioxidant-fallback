compile_and_run: out/hello-oxygen/bin/hello
	./out/hello-oxygen/bin/hello

# Observations:
#  * rustc expects crate names to use _ instead of -.
#  * rustc needs to be passed --crate-name=..., otherwise the crate cannot be found later

out/libhello/lib/libhello.rlib: libhello/hello.rs out/cfg-if/lib/libcfg_if.rlib
	mkdir -p out/libhello/lib && rustc --extern=cfg_if=out/cfg-if/lib/libcfg_if.rlib -Lout/cfg-if/lib --crate-type=lib libhello/hello.rs -o out/libhello/lib/libhello.rlib

out/hello-oxygen/bin/hello: libhello/hello.rs out/libhello/lib/libhello.rlib
	mkdir -p out/hello-oxygen/bin && rustc --extern=hello=out/libhello/lib/libhello.rlib -Lout/libhello/lib -Lout/cfg-if/lib hello-app/main.rs -o out/hello-oxygen/bin/hello

out/cfg-if/lib/libcfg_if.rlib: cfg-if-1.0.0/src/lib.rs
	mkdir -p out/cfg-if/lib && rustc --crate-type=lib --crate-name=cfg_if cfg-if-1.0.0/src/lib.rs -o out/cfg-if/lib/libcfg_if.rlib

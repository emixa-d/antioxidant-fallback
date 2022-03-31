compile_and_run: out/hello-oxygen/bin/hello
	./out/hello-oxygen/bin/hello

out/libhello/lib/libhello.rlib: libhello/hello.rs
	mkdir -p out/libhello/lib && rustc --crate-type=lib libhello/hello.rs -o out/libhello/lib/libhello.rlib

out/hello-oxygen/bin/hello: libhello/hello.rs out/libhello/lib/libhello.rlib
	mkdir -p out/hello-oxygen/bin && rustc -Lout/libhello/lib hello-app/main.rs -o out/hello-oxygen/bin/hello

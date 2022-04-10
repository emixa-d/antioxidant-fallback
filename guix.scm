;;; Antioxidant --- Building Rust without cargo
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;;
;;; This file is part of Antioxidant.
;;;
;;; Antioxidant is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Antioxidant is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.
(use-modules (guix packages) (guix build-system) (guix gexp) (guix utils) (guix modules)
	     (gnu packages compression) (gnu packages python) (gnu packages python-build)
	     (gnu packages guile) (ice-9 match) (srfi srfi-1)
	     (guix search-paths) (gnu packages rust) (gnu packages base))

(define (target-environment-variables target)
  ;; TODO gnueabihf?
  `(("CARGO_CFG_TARGET_ENV" .
     ,(if (or (target-linux? target) (target-hurd? target))
	  "gnu"
	  (unrecognised)))
    ("CARGO_CFG_TARGET_VENDOR" . "unknown") ; TODO: or pc?
    ("CARGO_CFG_TARGET_ENDIAN" . "little") ; TODO: big-endian
    ("CARGO_CFG_TARGET_ARCH" .
     ,(cond ((target-x86-64? target) "x86_64")
	    ((target-aarch64? target) "aarch64")
	    ((target-riscv64? target) "riscv64")
	    (#true (unrecognised))))
    ("CARGO_CFG_TARGET_OS" .
     ,(cond ((target-linux? target) "linux")
	    ((target-hurd? target) "hurd") ; TODO: or gnu?
	    (#true (unrecognised))))
    ("CARGO_CFG_TARGET_FAMILY" .
     ,(cond ((target-linux? target) "unix")
	    ((target-hurd? target) "unix") ; TODO: or gnu?
	    (#true (unrecognised))))
    ("CARGO_CFG_TARGET_POINTER_WIDTH" .
     ,(cond ((target-64bit? target) "64")
	    (#true "32")))))

;; features = default: Use whatever Cargo.toml lists as defaults (or nothing if nothing
;; is listed).
(define* (antioxidant-build name inputs #:key system target source search-paths outputs
			    ;; TODO: consider optimisations (what does cargo-build-system
			    ;; do?)
			    (optimisation-level 0)
			    (features #~'default)
			    (cargo-env-variables
			     #~'#$(target-environment-variables
				   (or target
				       (nix-system->gnu-triplet system)))))
  (define builder
    (with-extensions (list guile-json-4)
    (with-imported-modules
	(cons '(antioxidant)
	      (source-module-closure '((guix build utils) (guix build gnu-build-system)
				       (antioxidant))))
      #~(begin
	  (use-modules (guix build utils) (guix build gnu-build-system)
		       (srfi srfi-1) (ice-9 match) (antioxidant))
	  (gnu-build #:name #$name
		     #:source #+source
		     #:system #+(nix-system->gnu-triplet system)
		     #:build #+(nix-system->gnu-triplet system)
		     ;; used even when building natively
		     #:target #$(or target
				    (nix-system->gnu-triplet system))
		     #:outputs #$(outputs->gexp outputs)
		     #:inputs #$(input-tuples->gexp inputs)
		     #:native-inputs #$(input-tuples->gexp inputs)
		     #:search-paths '#$(map search-path-specification->sexp
					    search-paths)
		     #:features #$features
		     #:optimisation-level '#$optimisation-level
		     #:cargo-env-variables #$cargo-env-variables
		     #:phases (modify-phases %standard-phases
				;; TODO: before configure?
				(add-before 'unpack 'read-dependency-environment-variables
					    read-dependency-environment-variables)
				(delete 'configure)
				#$@(cond ((string-prefix? "rust-backtrace-sys" name)
				          #~((add-after 'unpack 'break-cycle
					       (lambda _
					         ;; only needed for Android targets,
					         ;; by removing it we avoid depending
					         ;; on crate-cc, breaking a cycle
					         (delete-file "build.rs")
					         (substitute* "Cargo.toml"
					           (("^build =(.*)$") ""))))))
					 ;; TODO: when deciding what binaries to build,
					 ;; respect [[bin]]/required-features, then this
					 ;; phase can be removed.
					 ((string-prefix? "rust-phf-generator" name)
					  #~((add-after 'unpack 'delete-bin
					       (lambda _
						 (delete-file "src/bin/gen_hash_test.rs")))))
					 ((string-prefix? "rust-cpp-demangle" name)
					  #~((add-after 'unpack 'delete-bin
					       (lambda _
						 (delete-file "src/bin/afl_runner.rs")))))
					 ((string-prefix? "rust-tokio-sync" name)
					  #~((add-after 'unpack 'unpreview
					       (lambda _
						 (substitute* "Cargo.toml"
						   (("-preview\\]") "]"))))))
					 ;; 'cc' and 'c++' don't exist
					 ((or (string-prefix? "rust-gcc-" name)
					      (string-prefix? "rust-cc-" name))
					  #~((add-after 'unpack 'fix-cc
					       (lambda _
						 (substitute* "src/lib.rs"
						   (("\"cc\"") "\"gcc\"")
						   (("\"c++\"") "\"g++\""))))))
					 (#true #~()))
				(replace 'build compile-cargo)
				(delete 'check)
				(delete 'install)))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target
		(features #~'default)
		#:rest arguments)
  (define private-keywords
    '(#:inputs #:native-inputs #:outputs))
  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(("source" ,source)
		    ("rust" ,rust)
		    ("python" ,python)
		    ("python-toml" ,python-toml) ; for convert-toml->json
		    ;; CARGO_PKG_AUTHORS can contain non-ASCII characters,
		    ;; make sure 'setenv' won't fail by including glibc
		    ;; and glibc-utf8-locales
		    ,@(@ (gnu packages commencement) %final-inputs)
		    ,@native-inputs))
    (host-inputs inputs)
    (build (if target antioxidant-cross-build antioxidant-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define antioxidant-build-system
  (build-system
   (name 'antioxidant)
   (description "Build software written in Rust, without cargo")
   (lower lower)))

;; Convert from cargo-build-system to antioxidant-build-system,
;; for now leaving inputs intact.
(define* (vitaminate-library/no-inputs crate-package
				       #:key (features #~'default))
  (package
    (inherit crate-package)
    (build-system antioxidant-build-system)
    (arguments (list #:features features))))

(define (is-cargo-toml-phases? phases)
  ;; This probably just relaxes versions, so no need to keep this phase
  ;; anymore.
  (match (if (gexp? phases) (gexp->approximate-sexp phases) phases)
    (('modify-phases '%standard-phases _
		     (_ _ _ (_ _ ('substitute* "Cargo.toml" . _))))
     #t)
    (_ #false)))

(use-modules (guix download))
(define crate-uri (@ (guix build-system cargo) crate-uri))

;; Use an updated set of rust-futures-... crates to avoid build failures
;; caused by uses of unstable rust things.  (and because they will need to
;; be updated anyway eventually).  TODO: verify for malware?
(define-public rust-futures-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-0.3))
   (name "rust-futures")
   (version "0.3.21")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "futures" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "17id2zvn2acny759indn6yj2acfa6lhkwzaidxr2pqfiaigycgzp"))))
   (arguments
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-io" ,rust-futures-io-0.3)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-futures-executor" ,rust-futures-executor-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3))))))

(define rust-futures-task-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-task-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-task" "0.3.21"))
     (file-name (string-append "rust-futures-task" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "0skpiz2ljisywajv79p70yapfwhkqhb39wxy3f09v47mdfbnmijp"))))))

(define rust-futures-util-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-util-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-util" "0.3.21"))
     (file-name (string-append "rust-futures-util" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "0sh3wqi8p36csjffy0irq8nlx9shqxp7z4dsih6bknarsvaspdyq"))))
   (arguments
    ;; TODO: upstream includes rust-futures
    `(#:cargo-inputs
      (("rust-futures-core" ,rust-futures-core-0.3)
       ("rust-futures-task" ,rust-futures-task-0.3)
       ("rust-futures-macro" ,rust-futures-macro-0.3)
       ("rust-futures-sink" ,rust-futures-sink-0.3)
       ("rust-futures-io" ,rust-futures-io-0.3)
       ("rust-futures-channel" ,rust-futures-channel-0.3)
       ("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))
       ("rust-pin-utils" ,(@ (gnu packages crates-io) rust-pin-utils-0.1))
       ("rust-slab" ,(@ (gnu packages crates-io) rust-slab-0.4)))))))

(define rust-futures-channel-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-channel-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-channel" "0.3.21"))
     (file-name (string-append "rust-futures-channel" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "0420lz2fmxa356ax1rp2sqi7b27ykfhvq4w9f1sla4hlp7j3q263"))))))

(define rust-futures-core-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-core-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-core" "0.3.21"))
     (file-name (string-append "rust-futures-core" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "1lqhc6mqklh5bmkpr77p42lqwjj8gaskk5ba2p3kl1z4nw2gs28c"))))))

(define rust-futures-sink-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-sink-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-sink" "0.3.21"))
     (file-name (string-append "rust-futures-sink" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "0s58gx5yw1a21xviw2qgc0wzk225vgn4kbzddrp141m3kw9kw5i1"))))))

(define rust-futures-io-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-io-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-io" "0.3.21"))
     (file-name (string-append "rust-futures-io" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "0swn29fysas36ikk5aw55104fi98117amvgxw9g96pjs5ab4ah7w"))))))

(define rust-futures-macro-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-macro-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-macro" "0.3.21"))
     (file-name (string-append "rust-futures-macro" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "04pmj5xfk5rdhlj69wc7w3zvdg3xardg8srig96lszrk00wf3h9k"))))))

(define rust-futures-executor-0.3
  (package
   (inherit (@ (gnu packages crates-io) rust-futures-executor-0.3))
   (version "0.3.21")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "futures-executor" "0.3.21"))
     (file-name (string-append "rust-futures-executor" "-" "0.3.21" ".tar.gz"))
     (sha256
      (base32 "19mq96kwgf06axgdc2fbrjhqzdnxww9vw6cz8b82gqr9z86bj84l"))))))

;; The old tokio doesn't build against recent rust-futures
(define rust-tokio-io-0.2
  (package
   (inherit (@ (gnu packages crates-io) rust-tokio-io-0.1))
   (version "0.2.0-alpha.6")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "tokio-io" version))
     (file-name (string-append "rust-tokio-io" "-" version ".tar.gz"))
     (sha256
      (base32 "1i92ichh2m7i23vdr51gnf5hxngv7d1clwjan1h0dwrxakaq89qi"))))))

(define rust-tokio-codec-0.2
  (package
   (inherit (@ (gnu packages crates-io) rust-tokio-codec-0.1))
   (version "0.2.0-alpha.6")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "tokio-codec" version))
     (file-name (string-append "rust-tokio-codec" "-" version ".tar.gz"))
     (sha256
      (base32 "0ykqx22rmw0k49y5302wshsaxjnpnwf4j4w8s92l1gc43vyj4pcz"))))))

(define rust-tokio-util-0.7
  (package
   (inherit (@ (gnu packages crates-io) rust-tokio-util-0.6))
   (version "0.7.1")
   (source
    (origin
     (method (@ (guix download) url-fetch))
     (uri ((@ (guix build-system cargo) crate-uri) "tokio-util" version))
     (file-name (string-append "rust-tokio-util" "-" version ".tar.gz"))
     (sha256
      (base32 "0r0p83nisf732qydg23qvmdd6gbrvyr1qvfs8hhbl7a1cyqdxpqf"))))))

;; Old combinations of rust-rustls & rust-tokio-rustls fail to build
(define-public rust-rustls-0.20
  (package
   (inherit (@ (gnu packages crates-io) rust-rustls-0.20))
   (name "rust-rustls")
   (version "0.20.4")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rustls" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "08b941jj4kk1bfg82zrr5b2ifa4ip155g9cpqmmp116v1n6ypgsg"))))))

(define-public rust-tokio-rustls-0.23
  (package
   (inherit (@ (gnu packages crates-io) rust-tokio-rustls-0.22))
   (name "rust-tokio-rustls")
   (version "0.23.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "tokio-rustls" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "17iqy9a8x0d8ydl5r28w8z9akhnwp74wyjxks055b617ryhgsla1"))))))

;; old rust-test-case@1 is incompatible with new rust-syn
(define-syntax p
  (syntax-rules ()
    ((_ foo) (@ (gnu packages crates-io) foo))))
(define-public rust-test-case-2
  (package
   (inherit (@ (gnu packages crates-io) rust-test-case-1))
   (name "rust-test-case")
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "test-case" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "07xgb68pfwb9j132wr3b78kzcbfapsy4scg8lipiv5ykk6d5hi33"))))
   (arguments
    `(#:cargo-inputs
      (("rust-cfg-if" ,(p rust-cfg-if-1))
       ("rust-proc-macro-error" ,(p rust-proc-macro-error-1))
       ("rust-proc-macro2" ,(p rust-proc-macro2-1))
       ("rust-quote" ,(p rust-quote-1))
       ("rust-syn" ,(p rust-syn-1)))
      #:cargo-development-inputs
      (("rust-indexmap" ,(p rust-indexmap-1))
       ("rust-insta" ,(p rust-insta-1))
       ("rust-itertools" ,(p rust-itertools-0.10))
       ("rust-lazy-static" ,(p rust-lazy-static-1)))))))

;; todo: ‘stub‘ rust-rustc-version to reduce deps?
;; grrr rust-backtrace
(define (vitaminate/auto* pack)
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 (features #~'default))
	 (unless (or (eq? phases '%standard-phases)
		     (not (is-cargo-toml-phases? phases)))
	   (error "phases?"))
	 (define fix-input
	   (match-lambda
	     ((label dependency . maybe-output)
	      ;; Some of these are only used for tests, cause cycles, ???
	      (and (not (member (package-name dependency)
				'("rust-quickcheck" ; (quickcheck env-logger humantime chrono bincode) cycle
				  "rust-macrotest"
				  "rust-afl" ; TODO: move to 'native-inputs'/development-inputs
				  "rust-js-sys" ; TODO: guix doesn't support those targets (yet)
				  "rust-cortex-m" ; ARM targets not yet supported for Rust in Guix
				  ;;"rust-cc" ;; todo: build.rs, hence move to 'native-inputs'?
				  "rust-stdweb" "rust-web-sys" ;; web, js, wasm?
				  "rust-bencher" ; FTB
				  "rust-criterion"
				  "rust-proptest"
				  "rust-futures-util-preview" ; futures-util has been updated?
				  "rust-errno-dragonfly" ;; TODO: DragonflyBSD not supported
				  ;; TODO: how do the two following crates even work?
				  "rust-rustc-std-workspace-std"
				  "rust-rustc-std-workspace-core"
				  "rust-compiler-builtins"
				  "rust-compiletest-rs" ;; TODO: rustc-dev?
				  "rust-winapi" "rust-kernel32-sys" ; skip Windows support for now
				  "rust-security-framework" "rust-cocoa" "rust-cocoa-foundation" "rust-core-foundation" "rust-core-foundation-sys" "rust-core-text" "rust-fsevent" "rust-fsevent-sys" "rust-objc-foundation" ; non-Linux, non-Hurd things
				  "rust-ws2-32-sys"
				  "rust-winapi-util" "rust-winapi-build"
				  "rust-doc-comment"
				  "rust-hermit-abi"
				  "rust-model" ;; doesn't build, avoid for now
				  "rust-tokio-core" ;; doesn't exist in recent tokios
				  "rust-tokio-process" ;; doesn't exist in recent tokios
				  "rust-tokio-executor" ;; doesn't exist in recent tokios, I think?
				  "rust-tokio-io" ;; doesn't exist in recent tokios, I think?
				  #;"rust-lazy-static" "rust-version-sync"
				  "rust-rustversion" "rust-trybuild"
				  "rust-clippy"
				  "rust-tokio-mock-task" ; doesn't build
				  "rust-tokio-test"
				  "rust-rand-xorshift"
				  "rust-walkdir"
				  "rust-serde-test"
				  "rust-wasm-bindgen" "rust-wasi"
				  "rust-wasm-bindgen-test")))
		   ;; rust-futures-cpupool isn't updated anymore and doesn't
		   ;; build anymore?
		   (not (string=? (package-name dependency) "rust-futures-cpupool"))
		   ;; The Redox operating system is not supported by Guix.
		   (not (string-prefix? "rust-redox" (package-name dependency)))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-derive" "rust-serde")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-anyhow" "rust-thiserror")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-proc-macro-hack" "rust-demo-hack")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-proc-macro-hack" "rust-demo-hack-impl")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-nom" "rust-jemallocator")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-bytes" "rust-bincode")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-failure-derive" "rust-failure")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-bytes" "rust-bincode")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-json" "rust-serde-stacker")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-erased-serde" "rust-serde-json")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-proc-macro2" "rust-quote")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-indexmap" "rust-itertools")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tracing-attributes" "rust-tracing")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tracing-attributes" "rust-async-trait")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tracing-attributes" "rust-tracing-futures")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tracing" "rust-tokio")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-hashbrown" "rust-ahash"))) ; todo: remove from #:cargo-inputs?, unused?
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-hashbrown" "rust-bumpalo"))) ; todo: remove from #:cargo-inputs?, unused?
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-fastrand" "rust-getrandom")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-rand" "rust-packed-simd-2")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-fastrand" "rust-instant")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-fastrand" "rust-wyhash")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-io" "rust-tokio-current-thread")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-core" "rust-flate2")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-core" "rust-httparse")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-httparse")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-async-stream"))) ;; test
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-nix"))) ;; test
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-process" "rust-failure"))) ;; otherwise cc needs to be removed from rust-cloudflare-zlib-sys
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-executor")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-current-thread")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-fs")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-reactor")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-sync")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-stream")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-sync" "rust-loom")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-tcp")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-timer")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-threadpool")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-udp")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-uds")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-tokio-macros")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-strum-macros" "rust-strum")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-parking-lot-core" "rust-backtrace")))
		   ;; TODO: can be removed by relaxing versions in rust-signal-hook@0.1
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-signal-hook-registry" "rust-signal-hook")))
		   ;; TODO why benchmark?
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-unicode-bidi" "rust-flame")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-unicode-bidi" "rust-flamer")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-odds" "rust-lazy-static")))
		   ;; TODO
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-boxxy" "rust-ctrlc")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-cast" "rust-rustc-version")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-flate2" "rust-tokio-tcp")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-flate2" "rust-tokio-threadpool")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio" "rust-flate2"))) ;; TODO remove old tokios
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-semver" "rust-crates-index"))) ;; TODO why????
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-semver-parser" "rust-pest-generator")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-spmc" "rust-loom")))
		   (not (and (member (package-name pack)
				     (list "rust-futures-util"
					   #;"rust-hex-literal-impl" "rust-hex-literal"))
			     (string=? (package-name dependency) "rust-proc-macro-hack")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-tokio-test" "rust-tokio"))) ; TODO
		   ;; Break dev-dependencies cycle
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-regex-automata" "rust-bstr")))
		   ;; These are actually test inputs! (TODO guix)
		   ;; (TODO: this isn't build from source)
		   ;;(not (equal? (package-name pack) "rust-pure-rust-locales"))
		   
		   (pk 'p pack dependency)
		   (cons* label (vitaminate/auto
				 (match (list (package-name dependency) (package-version dependency))
				   (("rust-test-case" _) rust-test-case-2)
				   (("rust-insta" _)
				    (@ (gnu packages crates-io) rust-insta-1))
				   (("rust-sct" _)
				    (@ (gnu packages crates-io) rust-sct-0.7))
				   (("rust-quote" _)
				    (@ (gnu packages crates-io) rust-quote-1))
				   (("rust-rustls-native-certs" _)
				    ;; Old versio incompatible with new rustls
				    (@ (gnu packages crates-io) rust-rustls-native-certs-0.6))
				   (("rust-ron" _)
				    (@ (gnu packages crates-io) rust-ron-0.6)) ; old versions don't build
				   (("rust-serde" _)
				    (@ (gnu packages crates-io) rust-serde-1)) ; old versions don't build
				   (("rust-sha1" _)
				    (@ (gnu packages crates-io) rust-sha1-0.6))
				   (("rust-webpki" _)
				    (@ (gnu packages crates-io) rust-webpki-0.22))
				   ;; Old versions don't build (because rust-tokio-io disappeared)
				   (("rust-hyper-rustls" _)
				    (@ (gnu packages crates-io) rust-hyper-rustls-0.22))
				   (("rust-rustls" _)
				    ;; Remove old (not-building) and potentially unsecure versions
				    ;; Also, rust-tokio-rustls requires a newer rust-rustls.
				    rust-rustls-0.20)
				   (("rust-tokio-rustls" _) ;0.10.3 doesn't build
				    rust-tokio-rustls-0.23)
				   ;; TODO: respect SSL_CERT_DIR instead of hardcoding trusting
				   ;; whoever Mozilla trusts.
				   ;; TODO: build from source
				   ;; TODO: remove certificates with restrictions
				   (("rust-webpki-roots" _)
				    ;; 0.17.0 doesn't build
				    (@ (gnu packages crates-io) rust-webpki-roots-0.22))
				   (("rust-nix" _)
				    (@ (gnu packages crates-io) rust-nix-0.23))
				   (("rust-bytes" _)
				    (@ (gnu packages crates-io) rust-bytes-1))
				   (("rust-tokio-io" _)
				    rust-tokio-io-0.2)
				   (("rust-tokio-codec" _)
				    rust-tokio-io-0.2)
				   (("rust-tokio-util" _)
				    rust-tokio-util-0.7)
				   (("rust-tokio" _)
				    (@ (gnu packages crates-io) rust-tokio-1.8))
				   (("rust-futures" _)
				    rust-futures-0.3)
				   (("rust-futures-channel" _)
				    rust-futures-channel-0.3)
				   (("rust-futures-core" _)
				    rust-futures-core-0.3)
				   (("rust-futures-executor" _)
				    rust-futures-executor-0.3)
				   (("rust-futures-io" _)
				    rust-futures-io-0.3)
				   (("rust-futures-sink" _)
				    rust-futures-sink-0.3)
				   (("rust-futures-task" _)
				    rust-futures-task-0.3)
				   (("rust-futures-util" _)
				    rust-futures-util-0.3)
				   (("rust-http" _) ; 0.1 doesn't build
				    (@ (gnu packages crates-io) rust-http-0.2))
				   ;; rust-http-body@0.1.0's dependency rust-tokio-buf doesn't
				   ;; build anymore.  (TODO remove from Guix)
				   (("rust-http-body" _)
				    (@ (gnu packages crates-io) rust-http-body-0.4))
				   ;; Likewise.
				   (("rust-hyper" _)
				    (@ (gnu packages crates-io) rust-hyper-0.14))
				   (("rust-nb" "0.1.3")
				    ;; Avoid E0519, caused by multiple versions of the same crate
				    ;; being used.  TODO: bump version in 'sniffglue'
				    (@ (gnu packages crates-io) rust-nb-1))
				   (("rust-num-traits" "0.1.43")
				    (@ (gnu packages crates-io) rust-num-traits-0.2))
				   (("rust-cfg-if" "0.1.10")
				    (@ (gnu packages crates-io) rust-cfg-if-1))
				   (("rust-env-logger" _) ; old versions don't build
				    (@ (gnu packages crates-io) rust-env-logger-0.9))
				   (("rust-lazy-static" _)
				    (@ (gnu packages crates-io) rust-lazy-static-1))
				   (("rust-rand" _)
				    (@ (gnu packages crates-io) rust-rand-0.8))
				   (("rust-lock-api" _) ; 0.3, 0.2, 0.1
				    (@ (gnu packages crates-io) rust-lock-api-0.4))
				   (("rust-sysctl" _) ; 0.1 does not compile (type errors)
				    (@ (gnu packages crates-io) rust-sysctl-0.4))
				   ;; The (now deprecated) rust-tempdir doesn't build
				   ;; against current rust-rand, use the new rust-tempfile
				   ;; instead as upstream recommends.
				   (("rust-tempdir" _)
				    (@ (gnu packages crates-io) rust-tempfile-3))
				   (("rust-bare-metal" _)
				    (@ (gnu packages crates-io) rust-bare-metal-1))
				   ;; The old parking-lot doesn't build against
				   ;; the new lock api.
				   (("rust-parking-lot" _)
				    ;; TODO: inherit?
				    (@ (gnu packages crates-io) rust-parking-lot-0.11))
				   ;; 0.4.30 fails to build.
				   (("rust-proc-macro2" "0.4.30")
				    (@ (gnu packages crates-io) rust-proc-macro2-1))
				   (("rust-log" "0.3.9")
				    (@ (gnu packages crates-io) rust-log-0.4))
				   (_ dependency)))
			  maybe-output)))))
	 ;; Detect cycles early by unthunking
	 (define i (filter-map fix-input (package-inputs pack)))
	 (define n-i (append (filter-map fix-input cargo-development-inputs)
			     ;; TODO: move zlib of rust-libz-sys-1 from
			     ;; native-inputs to inputs.
			     (package-native-inputs pack)))
	 (define p-i (append (filter-map fix-input
					 (append cargo-inputs
						 ;; Add missing dependencies (TODO upstream Guix)
						 (match (package-name pack)
						   ("rust-petgraph" `(("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
						   ;; TODO: is this sufficient?
						   ("rust-futures-core-preview"
						    `(("rust-futures-core" ,rust-futures-core-0.3)))
						   ("rust-http-body" ; at least for 0.4
						    `(("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))))
						   ("rust-tokio-sync"
						    `(("rust-futures-core" ,rust-futures-core-0.3)
						      ("rust-futures-util" ,rust-futures-util-0.3)))
						   (_ '()))))
			     (package-propagated-inputs pack)))
	 (package
	  (inherit (vitaminate-library/no-inputs pack))
	  (arguments (list #:features
			   ;; TODO: can some now be removed now that default features
			   ;; are enabled by default?  And maybe the features can be moved
			   ;; to Guix upstream?
			   (match (package-name pack)
			     ;; rust-x509-parser requires bigint
			     ("rust-der-parser" #~'("default" "bigint"))
			     ;; rust-x509-parser required 'crypto' and 'x509'
			     ("rust-oid-registry" #~'("default" "crypto" "x509"))
			     ("rust-similar" #~'("default" "text" "inline"))
			     ;; 'derive' is needed by rust-ron
			     ("rust-serde" #~'("std" "alloc" "derive"))
			     ("rust-webpki" #~'("std" "alloc"))
			     ;; Required by rust-tokio
			     ;; TODO remove os-poll, as implied features are implemented.
			     ("rust-mio"
			      #~'("net" "os-ext" "os-poll"))
			     ;; By default zero features are enabled, which is rather
			     ;; minimalistic and often not sufficient.  TODO: teach
			     ;; antioxidant about ‘implied’ features.
			     ("rust-tokio"
			      #~'("full" "io-util" "io-std"
				  ;;"feature=\"macros\""  ;; TODO
				  "net"
				  "parking_lot"
				  "process"
				  "rt"
				  "rt-multi-thread"
				  "signal"
				  "sync"
				  "time"))
			     ("rust-tokio-util" #~'("full" "codec"))
			     ;; extra-traits is required by rust-nix
			     ("rust-libc" #~'("std" "extra_traits"))
			     ;; Enable some features such that "rust-futures" actually builds.
			     ("rust-futures-task"
			      #~'("std" "alloc"))
			     ("rust-futures-util"
			      #~'("std" "alloc" "sink"
				  "io" "async-await"
				  "async-await-macro"
				  "channel"))
			     ("rust-futures-core"
			      #~'("std" "alloc"))
			     ("rust-futures-channel"
			      #~'("std" "alloc"))
			     ;; Without "getrandom" or "alloc", it fails to build (TODO upstream?).
			     ;; small_rngs is required by rust-phf-generator.
			     ("rust-rand"
			      #~'("std" "std_rng" "getrandom"
				  "alloc" "small_rng"))
			     ;; Required by rust-rand when using the getrandom feature
			     ("rust-rand-core" #~'("std" "getrandom"))
			     ;; Required by rust-rand-core.
			     ("rust-getrandom" #~'("std"))
			     ;; Required by rust-env-logger
			     ("rust-log" #~'("std"))
			     ;; The feature "alloc" is not set by default, causing the
			     ;; build to fail (TODO: maybe report upstream?)
			     ("rust-bitvec"
			      #~'("std" "atomic" "alloc"))
			     ;; Likewise.
			     ("rust-chrono" #~'("alloc"))
			     ;; The non-default feature "alloc" is required by rust-pure-rust-locales.
			     ("rust-nom"
			      #~'("std" "lexical" "alloc"))
			     ;; This addresses the build failure
			     ;; ‘could not find `collector` in the crate root’
			     ;; and ‘cannot find function `pin` in crate `epoch`’
			     ("rust-crossbeam-epoch"
			      #~'("std" "alloc"))
			     ;; Required by rust-unicode-normalization
			     ("rust-tinyvec" #~'("alloc"))
			     ;; TODO: use default features from Cargo.toml
			     ;; rust-serde-bytes requires the 'parsing' feature.
			     ;; visit is required by rust-synstructure.
			     ;; visit-mut is used by rust-tracing-attributes.
			     ("rust-syn"
			      #~'("derive" "parsing" "printing"
				  "clone-impls"
				  "proc-macro" "full"
				  "visit" "visit-mut"
				  ;; Used by rust-strum-macros
				  "extra-traits"))
			     ("rust-proc-macro2"
			      ;; Required by rust-serde-bytes via rust-syn.  If
			      ;; absent, this causes errors like
			      ;; <<https://github.com/google/cargo-raze/issues/159>.
			      #~'("proc-macro"))
			     ;; TODO: move into Guix proper?
			     ((or "rust-hashbrown" "rust-os-str-bytes")
			      #~'("raw"))
			     (_ (match features
				  ((? gexp? f) f)
				  (('quote l)
				   ;; TODO: escapes, less ad-hoc
				   #~'#$l))))))
	  (inputs i)
	  (native-inputs n-i)
	  (propagated-inputs p-i)))
       (package-arguments pack))
      pack))

(define vitamination-stack  ; only for cycle detection
  (make-parameter '()))

(define vitaminate/auto
  ((@ (guix memoization) mlambda) (pack)
   (when (member pack (vitamination-stack))
     (call-with-values
	 (lambda ()
	   (break (lambda (x) (eq? x pack)) (vitamination-stack)))
       (lambda (cycle before)
	 (pk 'cyclic-vitamines)
	 (pk #:begin (reverse (map package-name before)))
	 (pk #:cycle (reverse (cons '... (map package-name (cons pack cycle)))))))
     (error "oops, a cycle?"))
   (parameterize ((vitamination-stack (cons pack (vitamination-stack))))
     (vitaminate/auto* pack))))

;; todo: cycle between rust-demo-hack and rust-demo-hack-impl

(vitaminate/auto (@ (gnu packages rust-apps) hexyl))
(vitaminate/auto (@ (gnu packages crates-io) rust-serde-bytes-0.11))
(vitaminate/auto (@ (gnu packages rust-apps) sniffglue))
(vitaminate/auto (@ (gnu packages crates-io) rust-bytes-0.3))

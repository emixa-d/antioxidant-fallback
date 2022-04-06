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
		     #:system #$system
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
				(delete 'configure)
				#$@(if (string-prefix? "rust-backtrace-sys" name)
				       #~((add-after 'unpack 'break-cycle
					    (lambda _
					      ;; only needed for Android targets,
					      ;; by removing it we avoid depending
					      ;; on crate-cc, breaking a cycle
					      (delete-file "build.rs")
					      (substitute* "Cargo.toml"
					        (("^build =(.*)$") "")))))
				       #~())
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
		    ("tar" ,tar)
		    ("gzip" ,gzip)
		    ("python" ,python)
		    ("python-toml" ,python-toml) ; for convert-toml->json
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
				  "rust-afl" ; TODO: move to 'native-inputs'/development-inputs
				  "rust-js-sys" ; TODO: guix doesn't support those targets (yet)
				  ;;"rust-cc" ;; todo: build.rs, hence move to 'native-inputs'?
				  "rust-stdweb" "rust-web-sys" ;; web, js, wasm?
				  "rust-bencher" ; FTB
				  "rust-criterion"
				  "rust-proptest"
				  "rust-errno-dragonfly" ;; TODO: DragonflyBSD not supported
				  "rust-rustc-std-workspace-std"
				  "rust-rustc-std-workspace-core"
				  "rust-compiler-builtins"
				  "rust-winapi" "rust-kernel32-sys" ; skip Windows support for now
				  "rust-serde-json" "rust-doc-comment"
				  "rust-hermit-abi"
				  "rust-model" ;; doesn't build, avoid for now
				  #;"rust-lazy-static" "rust-version-sync"
				  "rust-rustversion" "rust-trybuild"
				  "rust-clippy"
				  "rust-rand-xorshift"
				  "rust-walkdir" "rust-yaml-rust"
				  "rust-serde-test"
				  "rust-wasm-bindgen" "rust-wasi"
				  "rust-wasm-bindgen-test")))
		   ;; The Redox operating system is not supported by Guix.
		   (not (string-prefix? "rust-redox" (package-name dependency)))
		   ;; Avoid cycle!
		   (or (member (package-name pack)
			       '("rust-serde-bytes" "rust-erased-serde" "rust-docopt"
				 "rust-toml" "rust-serde-fmt" "rust-csv"))
		       (not (string=? (package-name dependency) "rust-serde")))
		   (or (string=? (package-name pack) "rust-serde")
		       (not (string=? (package-name dependency) "rust-serde-derive")))
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
				(list "rust-proc-macro2" "rust-quote")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-indexmap" "rust-itertools")))
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
		   
;;		   (pk 'p pack dependency #t)
		   (cons* label (vitaminate/auto
				 (match (list (package-name dependency) (package-version dependency))
				   (("rust-nb" "0.1.3")
				    ;; Avoid E0519, caused by multiple versions of the same crate
				    ;; being used.  TODO: bump version in 'sniffglue'
				    (@ (gnu packages crates-io) rust-nb-1))
				   (("rust-num-traits" "0.1.43")
				    (@ (gnu packages crates-io) rust-num-traits-0.2))
				   (("rust-rand" _)
				    (@ (gnu packages crates-io) rust-rand-0.8))
				   (("rust-proc-macro2" "0.4.30")
				    (@ (gnu packages crates-io) rust-proc-macro2-1))
				   (("rust-log" "0.3.9")
				    (@ (gnu packages crates-io) rust-log-0.4))
				   (_ dependency)))
			  maybe-output)))))
	 ;; Detect cycles early by unthunking
	 (define i (filter-map fix-input (package-inputs pack)))
	 (define n-i (filter-map fix-input cargo-development-inputs))
	 (define p-i (append (filter-map fix-input
					 (append cargo-inputs
						 ;; Add missing dependencies (TODO upstream Guix)
						 (match (package-name pack)
						   ("rust-petgraph" `(("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
						   (_ '()))))
			     (package-propagated-inputs pack)))
	 (package
	  (inherit (vitaminate-library/no-inputs pack))
	  (arguments (list #:features
			   ;; TODO: can some now be removed now that default features
			   ;; are enabled by default?
			   (match (package-name pack)
			     ;; Without "getrandom" or "alloc", it fails to build (TODO upstream?)
			     ("rust-rand"
			      #~'("feature=\"std\"" "feature=\"std_rng\"" "feature=\"getrandom\""
				  "feature=\"alloc\""))
			     ;; Required by rust-rand when using the getrandom feature
			     ("rust-rand-core" #~'("feature=\"std\" ""feature=\"getrandom\""))
			     ;; Required by rust-rand-core.
			     ("rust-getrandom" #~'("feature=\"std\""))
			     ;; Required by rust-env-logger
			     ("rust-log" #~'("feature=\"std\""))
			     ;; The feature "alloc" is not set by default, causing the
			     ;; build to fail (TODO: maybe report upstream?)
			     ("rust-bitvec"
			      #~'("feature=\"std\"" "feature=\"atomic\"" "feature=\"alloc\""))
			     ;; Likewise.
			     ("rust-chrono" #~'("feature=\"alloc\""))
			     ;; The non-default feature "alloc" is required by rust-pure-rust-locales.
			     ("rust-nom"
			      #~'("feature=\"std\"" "feature=\"lexical\"" "feature=\"alloc\""))
			     ;; This addresses the build failure
			     ;; ‘could not find `collector` in the crate root’
			     ;; and ‘cannot find function `pin` in crate `epoch`’
			     ("rust-crossbeam-epoch"
			      #~'("feature=\"std\"" "feature=\"alloc\""))
			     ;; TODO: use default features from Cargo.toml
			     ;; rust-serde-bytes requires the 'parsing' feature
			     ("rust-syn"
			      #~'("feature=\"derive\"" "feature=\"parsing\"" "feature=\"printing\"" "feature=\"clone-impls\""
				  "feature=\"proc-macro\"" "feature=\"full\""
				  ;; Used by rust-strum-macros
				  "feature=\"extra-traits\""))
			     ("rust-proc-macro2"
			      ;; Required by rust-serde-bytes via rust-syn.  If
			      ;; absent, this causes errors like
			      ;; <<https://github.com/google/cargo-raze/issues/159>.
			      #~'("feature=\"proc-macro\""))
			     ;; TODO: move into Guix proper?
			     ((or "rust-hashbrown" "rust-os-str-bytes")
			      #~'("feature=\"raw\""))
			     (_ (match features
				  ((? gexp? f) f)
				  (('quote l)
				   ;; TODO: escapes, less ad-hoc
				   #~'#$(map (lambda (s)
					       (string-append "feature=\"" s "\""))
					     l)))))))
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

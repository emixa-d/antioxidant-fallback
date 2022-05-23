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
(define-module (antioxidant-packages))

(use-modules (guix packages) (guix build-system) (guix gexp) (guix utils) (guix modules)
	     (gnu packages compression) (gnu packages python) (gnu packages python-build)
	     (gnu packages guile) (ice-9 match) (srfi srfi-1)
	     (guix git-download) (ice-9 optargs)
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
	    (#true "32")))
    ;; These CPU features are set by default, but antioxidant needs to know
    ;; these as well to set the appropriate environment variables expected by,
    ;; e.g., rust-sleef-sys.  TODO: CPU tuning.
    ,@(if (target-x86-64? target)
	  '(("CARGO_CFG_TARGET_FEATURE" . "sse,sse2"))
	  '())))

(define* (antioxidant-build name inputs #:key
			    modules ; what to do about 'modules'
			    install-source? ; not used by antioxidant-build-system
			    system target source search-paths outputs
			    ;; TODO: consider optimisations (what does cargo-build-system
			    ;; do?)
			    (optimisation-level 0)
			    (features #~'("default"))
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
		     #:phases (modify-phases %standard-antioxidant-phases
				#$@(cond ((string-prefix? "rust-backtrace-sys" name)
				          #~((add-after 'unpack 'break-cycle
					       (lambda _
					         ;; only needed for Android targets,
					         ;; by removing it we avoid depending
					         ;; on crate-cc, breaking a cycle
					         (delete-file "build.rs")
					         (substitute* "Cargo.toml"
							      (("^build =(.*)$") ""))))))
					 ((string-prefix? "rust-freetype-sys-" name)
					  #~((add-after 'unpack 'unbundle
					       (lambda _ ; TODO: move to origin snippet (& upstream Guix?)
						 (delete-file-recursively "freetype2")))))
					 ;; TODO: upstream / update
					 ((string-prefix? "rust-x509-parser" name)
					  #~((add-after 'unpack 'use-nondeprecated
					       (lambda _
						 (substitute* "src/time.rs"
						   (("use std::time::Duration;")
						    "use std::time::Duration;use std::convert::TryInto;")
						   (("\\.to_std\\(\\)") ".try_into()"))))))
					 ;; Preserve this phase from (gnu packages crates-io)
					 ((string-prefix? "rust-pkg-config-" name)
					  #~((add-after 'unpack 'hardcode-pkg-config-loation
					       (lambda* (#:key inputs #:allow-other-keys)
						 (substitute* "src/lib.rs"
							      (("\"pkg-config\"")
							       (string-append "\"" (assoc-ref inputs "pkg-config")
									      "/bin/pkg-config\"")))))))
					 ;; TODO: Upstream/update
					 ((string-prefix? "rust-structopt-derive" name)
					  #~((add-after 'unpack 'use-existing
					       (lambda _
						 (substitute* "src/attrs.rs"
						   (("CamelCase, KebabCase, MixedCase, ShoutySnakeCase, SnakeCase")
						    ;; ?? CamelCase, MixedCase
						    "ToUpperCamelCase, ToKebabCase, ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase")
						   (("to_camel_case") "to_upper_camel_case")
						   (("to_mixed_case") "to_lower_camel_case"))))))
					 ;; TODO: update
					 ((string-prefix? "rust-glib-macros" name)
					  #~((add-after 'unpack 'use-existing
					       (lambda _
						 (substitute* '("src/genum_derive.rs" "src/gflags_attribute.rs")
						   (("CamelCase, KebabCase") "ToUpperCamelCase, ToKebabCase")
						   (("to_camel_case") "to_upper_camel_case"))))))
					 ;; TODO: add rust-peg-macros to native-inputs for
					 ;; cross-compilation reasons.

					 ;; There appears to be a bootstrapping cycle here:
					 ;; IIUC, rust-peg/rust-peg-macros accepts a PEG grammar.
					 ;; This grammar is parsed & compiled into Rust code.
					 ;; The parser (of the grammar of the PEG grammar) is
					 ;; generated with rust-peg/rust-peg-macros.
					 ;;
					 ;; The solution to the cycle appears to be:
					 ;;
					 ;;   * peg-macros ships a pre-generated parser
					 ;;   * the generated code is sufficiently short
					 ;;     for there not to be any opportunity for
					 ;;     hiding anything malicious
					 ;;   * peg (in its bootstrap.sh) regenerates
					 ;;     the parser and check that its a fixpoint.
					 ((string-prefix? "rust-peg-0" name)
					  #~((replace 'bootstrap
					       (lambda* (#:key native-inputs #:allow-other-keys)
						 (with-output-to-file "new-grammar.rs"
						   (lambda ()
						     (invoke "rust-peg"
							     (search-input-file native-inputs
										"share/rust-peg-macros/grammar.rustpeg"))))
						 ;; TODO: the fixpoint test fails!
						 #;(invoke "diff" "-s" "--ignore-space-change" "new-grammar.rs"
							   (search-input-file native-inputs
									      "share/rust-peg-macros/grammar.rs"))
						 (delete-file "new-grammar.rs")))))
					 ((string-prefix? "rust-peg-macros-" name)
					  #~((add-after 'install 'install-grammar
					       (lambda _
						 (install-file "grammar.rustpeg" (string-append #$output "/share/rust-peg-macros"))
						 (install-file "grammar.rs" (string-append #$output "/share/rust-peg-macros"))))))
					 ((string-prefix? "rust-chrono" name)
					  #~((add-after 'unpack 'use-nondeprecated-names
					       (lambda _
						 ;; TODO: upstream
						 (substitute* '("src/naive/date.rs" "src/naive/time.rs" "src/round.rs")
						   (("num_days\\(\\)") "whole_days()")
						   (("num_weeks\\(\\)") "whole_weeks()")
						   (("num_seconds\\(\\)") "whole_seconds()")
						   ;; XXX this is not exactly the same behaviour,
						   ;; as a panic has been replaced by a truncation.
						   (("rhs.num_nanoseconds\\(\\)\\.unwrap\\(\\)") "(rhs.whole_nanoseconds() as i64)")
						   (("\\(rhs - OldDuration::seconds\\(rhssecs\\)\\)\\.num_nanoseconds\\(\\)\\.unwrap\\(\\)")
						    "((rhs - OldDuration::seconds(rhssecs)).whole_nanoseconds() as i64)")
						   (("duration\\.num_nanoseconds\\(\\)")
						    "Some(duration.whole_nanoseconds() as i64)"))))))
					 ((string-prefix? "rust-pkcs1" name)
					  #~((add-after 'unpack 'fix-typing
					       (lambda _
						 ;; Already upstream: <https://github.com/RustCrypto/formats/blob/fbf4334be7717e1f393c3f7b9b4c85c584ce8395/pkcs1/src/lib.rs#L49>, but not yet in any release.
						 (substitute* "src/lib.rs"
						   (("ObjectIdentifier::new") "ObjectIdentifier::new_unwrap"))))))
					 ;; TODO: change in Guix upstream.
					 ;; TODO: adjust README.md? Make sure LICENSE-APACHE
					 ;; is installed?
					 ((string-prefix? "rust-cmake" name)
					  #~((add-after 'unpack 'absolute-cmake
					       (lambda* (#:key inputs #:allow-other-keys)
						 (substitute* "src/lib.rs"
						   (("\"cmake\"") (format #f "\"~a\"" (search-input-file inputs "bin/cmake"))))))))
					 ((string-prefix? "rust-clang-sys" name)
					  ;; TODO: are there some paths that need to be
					  ;; absolutised?
				          #~((add-after 'unpack 'set-libclang-path
					       (lambda* (#:key inputs #:allow-other-keys)
						 (setenv "LIBCLANG_PATH"
							 (dirname (search-input-file inputs "lib/libclang.so")))))))
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
					 ((string-prefix? "rust-tuikit" name)
					  ;; TODO: upstream
					  #~((add-after 'unpack 'fix-unresolved+deprecated
					       (lambda _
						 (substitute* "src/raw.rs"
						   (("use nix::Error::Sys;") "")
						   (("match err \\{") "{")
						   (("nix::Error::from_errno\\(ENOTTY\\)") "ENOTTY")
						   (("Sys\\((.*)") "err.into()")
						   (("_ => (.*)$") ""))))))
					 ;; 'cc' and 'c++' don't exist
					 ((or (string-prefix? "rust-gcc-" name)
					      (string-prefix? "rust-cc-" name))
					  #~((add-after 'unpack 'fix-cc
					       (lambda _
						 (substitute* "src/lib.rs"
						   (("\"cc\"") "\"gcc\"")
						   (("\"c++\"") "\"g++\""))))))
					 (#true #~()))))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target
		(features #~'("default"))
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

(define-public antioxidant-build-system
  (build-system
   (name 'antioxidant)
   (description "Build software written in Rust, without cargo")
   (lower lower)))

;; Convert from cargo-build-system to antioxidant-build-system,
;; for now leaving inputs intact.
(define*-public (vitaminate-library/no-inputs crate-package
					      #:key (features #~'("default")))
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

(define-syntax p
  (syntax-rules ()
    ((_ foo) (@ (gnu packages crates-io) foo))))

(use-modules (guix download))
(define crate-uri (@ (guix build-system cargo) crate-uri))

;; Use an updated set of rust-futures-... crates to avoid build failures
;; caused by uses of unstable rust things.  (and because they will need to
;; be updated anyway eventually).  TODO: verify for malware?
(define rust-futures-0.3
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
       ("rust-futures-channel" ,rust-futures-channel-0.3)
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
       ("rust-memchr" ,(p rust-memchr-2))
       ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))
       ("rust-pin-utils" ,(p rust-pin-utils-0.1))
       ("rust-slab" ,(p rust-slab-0.4)))))))

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
(define rust-rustls-0.20
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

(define rust-tokio-rustls-0.23
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

;; rust-tokio-util needs a slab with 'compact'
(define rust-slab
  (package
   (inherit (@ (gnu packages crates-io) rust-slab-0.4))
   (name "rust-slab")
   (version "0.4.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "slab" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0cmvcy9ppsh3dz8mi6jljx7bxyknvgpas4aid2ayxk1vjpz3qw7b"))))))

(define rust-hyper-rustls
  (package
   (inherit (@ (gnu packages crates-io) rust-hyper-rustls-0.22))
   (name "rust-hyper-rustls")
   (version "0.23.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "hyper-rustls" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1b2wi75qrmwfpw3pqwcg1xjndl4z0aris15296wf7i8d5v04hz6q"))))))

;; Old versions don't support the new rustls
(define rust-boxxy
  (package
   (inherit (@ (gnu packages crates-io) rust-boxxy-0.11))
   (name "rust-boxxy")
   (version "0.12.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "boxxy" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1q0wpz955y3iwd35bqk3pbx2vx904fhyj75j7d6mrb7ib5fs5kxg"))))))

;; Old versions don't support the new nom
(define rust-pktparse
  (package
   (inherit (@ (gnu packages crates-io) rust-pktparse-0.5))
   (name "rust-pktparse")
   (version "0.7.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "pktparse" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "06sy7lwnhwmkyqfdbi4cs11z55rihipxafbdspnd5zg76pnbgbm8"))))))

;; Old versions don't support the new nom
(define rust-pure-rust-locales
  (package
   (inherit (@ (gnu packages crates-io) rust-pure-rust-locales-0.5))
   (name "rust-pure-rust-locales")
   (version "0.5.6")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "pure-rust-locales" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1n1jqy8g7ph9lvzncc8vy5jaqq2dljryp1agcnp5pwwi9zy4jp5l"))))))

;; Old version doesn't build
(define sniffglue
  (package
    (inherit (@ (gnu packages rust-apps) sniffglue))
    (name "sniffglue")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sniffglue" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "038wcjiiay825wc8inmn62flklc1adxskg5fmjhmxqnhwmj1k5gn"))))))

;; old rust-test-case@1 is incompatible with new rust-syn
(define rust-test-case-2
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

;; needed for rcgen
(define rust-yasna
  (package
    (name "rust-yasna")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yasna" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0k1gk11hq4rwlppv9f50bz8bnmgr73r66idpp7rybly96si38v9l"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-bit-vec" ,(@ (gnu packages crates-io) rust-bit-vec-0.6))
         ("rust-num-bigint" ,(@ (gnu packages crates-io) rust-num-bigint-0.4))
         ("rust-time" ,(@ (gnu packages crates-io) rust-time-0.3)))
        #:cargo-development-inputs
        (("rust-num-traits" ,(@ (gnu packages crates-io) rust-num-traits-0.2)))))
    (home-page "https://github.com/qnighy/yasna.rs")
    (synopsis "ASN.1 library for Rust")
    (description "ASN.1 library for Rust")
    (license '(list license:expat license:asl2.0))))

;; rust-tokio-openssl@0.6.3 needs a recent rust-openssl
(define rust-openssl-macros
  (package
    (name "rust-openssl-macros")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0v3kgnzbadrf9c06q4cqmbjas53av73n5w7wwz3n0nb6257y80dm"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,(p rust-proc-macro2-1))
         ("rust-quote" ,(p rust-quote-1))
         ("rust-syn" ,(p rust-syn-1)))))
    (home-page "")
    (synopsis "Internal macros used by the openssl crate.")
    (description "Internal macros used by the openssl crate.")
    (license '(list license:expat license:asl2.0))))
(define-public rust-openssl-sys
  (package
   (inherit (p rust-openssl-sys-0.9))
   (name "rust-openssl-sys")
   (version "0.9.73")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "openssl-sys" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1h0bv7cwrbbwdnpj96mb2b0p0gkajwc5g4rl3qf1ka70nfgx2pwx"))))))
(define-public rust-openssl
  (package
   (inherit (p rust-openssl-0.10))
   (name "rust-openssl")
   (version "0.10.39")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "openssl" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0hi72bwh6ipfj3cj7dlzk6n8aixiswj2fzbv5nk17n6r8rnr3wr8"))))
   (inputs ; TODO: or native-inputs?  Where to put macros?
    (modify-inputs (package-inputs (p rust-openssl-0.10))
      (prepend (p rust-once-cell-1)
	       rust-openssl-macros)))))

;; not yet in Guix, but needed for updated agate
(define rust-rcgen
  (package
    (name "rust-rcgen")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rcgen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ppwfl9g504x2qwk7m7mag8c3l70w9mcfha93013nlzqdlw2vynp"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:tests? #false ;; some test dependencies removed
       #:cargo-inputs
        (("rust-pem" ,(@ (gnu packages crates-io) rust-pem-1))
         ("rust-ring" ,(@ (gnu packages crates-io) rust-ring-0.16))
         ("rust-time" ,(@ (gnu packages crates-io) rust-time-0.3))
	 ;; TODO: 0.13
         ("rust-x509-parser" ,(@ (gnu packages crates-io) rust-x509-parser-0.12))
         ("rust-yasna" ,rust-yasna)
         ("rust-zeroize" ,(@ (gnu packages crates-io) rust-zeroize-1)))
        #:cargo-development-inputs
        (#;("rust-botan" ,(@ (gnu packages crates-io) rust-botan-0.8))
         ("rust-openssl" ,(@ (gnu packages crates-io) rust-openssl-0.10))
         ("rust-rand" ,(@ (gnu packages crates-io) rust-rand-0.8))
         ("rust-rsa" ,rust-rsa)
         ("rust-webpki" ,(@ (gnu packages crates-io) rust-webpki-0.22))
         ("rust-x509-parser" ,(@ (gnu packages crates-io) rust-x509-parser-0.12)))))
    (home-page "https://github.com/est31/rcgen")
    (synopsis "Rust X.509 certificate generator")
    (description "Rust X.509 certificate generator") ;; TODO
    (license '(list license:expat license:asl2.0))))

;; Old pkcs5 doesn't build
(define rust-pkcs5
  (package
    (inherit (@ (gnu packages crates-io) rust-pkcs5-0.3))
    (name "rust-pkcs5")
    (version "0.5.0-pre.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1n605kk594vif1rzrc09739nw6fky41mz6jpz9czs7lagq75jkvs"))))))
(define rust-pkcs8 ;; old pkcs8 doesn't build against new rust-der
  (package
    (inherit (p rust-pkcs8-0.7))
    (name "rust-pkcs8")
    (version "0.9.0-pre.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs8" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0257q8i3xqar8lb9lg6hvd17aykf5f3n1v3v7p7r4g1wsrncm7pk"))))))
(define-public rust-der
  (package
    (inherit (@ (gnu packages crates-io) rust-der-0.4))
    (name "rust-der")
    (version "0.6.0-pre.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1qag0zb2n7fj8qv7a83pmm9rqqz0zxvx5zpyzfy05a1rxxz73vdk"))))))
(define-public rust-pkcs1 ; old doesn't build against new rust-der
  (package
    (inherit (@ (gnu packages crates-io) rust-pkcs1-0.2))
    (name "rust-pkcs1")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkcs1" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0813szfx13n4xl6l19m3lwj7pqgljqwc6ipxhr2dv0yc9k06d3x7"))))))
(define-public rust-spki ; old doesn't build against new rust-der
  (package
    (inherit (@ (gnu packages crates-io) rust-spki-0.4))
    (name "rust-spki")
    (version "0.6.0-pre.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spki" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1cbdffcfp1zivxw4hiqj681api2gqxcgcqf64rq2wbvrk10jffq9"))))))

;; Required by rust-pkcs5' cbc feature
(define rust-cbc
  (package
   (name "rust-cbc")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "cbc" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))))
   (build-system (@ (guix build-system cargo) cargo-build-system))
   (arguments
    `(#:cargo-inputs
      (("rust-cipher" ,rust-cipher))
      #:cargo-development-inputs
      (("rust-aes" ,rust-aes)
       ("rust-cipher" ,rust-cipher)
       ("rust-hex-literal" ,(p rust-hex-literal-0.3)))))
   (home-page "https://github.com/RustCrypto/block-modes")
   (synopsis "Cipher Block Chaining (CBC) block cipher mode of operation")
   (description "Cipher Block Chaining (CBC) block cipher mode of operation")
   (license '(list license:expat license:asl2.0))))

;; Old vesion incompatible with new rust-hmac
(define rust-cookie
  (package
    (inherit (p rust-cookie-0.15))
    (name "rust-cookie")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "01fa6z8sqqg19ya0l9ifh8vn05l5hpxdzkbh489mpymhw5np1m4l"))))))


;; Old agate doesn't build
(define agate
  (package
    (inherit (@ (gnu packages rust-apps) agate))
    (name "agate")
    (version "3.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "agate" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1bxar93jh80jv5xvr6rnlc6gcsvzlf673m9874jzhjp78mhdbmwx"))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages rust-apps) agate))
       (append rust-rcgen rust-futures-util-0.3)))))

;; Old castor doesn't build against new rust-gtk
(define castor
  (package
    (inherit (@ (gnu packages web) castor))
    (name "castor")
    (version "0.9.0")
    (source
      (origin
       (inherit (package-source (@ (gnu packages web) castor)))
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~julienxx/castor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gda77ya2qbmjxfbw3yfr64inm8xw8243iwnfsgwwiwl35pw70n9"))
       (modules '((guix build utils)))
       ;; Submitted upstream at <https://lists.sr.ht/~julienxx/castor/patches/32444>
       (patches (list (local-file "0001-Update-to-new-GTK-version-and-new-version-of-depende.patch")))))))

(define rust-enum-as-inner ; old version doesn't build against new rust-heck
  (package
    (inherit (@ (gnu packages crates-io) rust-enum-as-inner-0.3))
    (name "rust-enum-as-inner")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enum-as-inner" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wwvjmy2bvqqc3pyylsmlqpkswxrzsg40xva7z27szva8j0svk91"))))))


;; Old version doesn't have the block_buffer required by rust-sha3
(define rust-block-buffer
  (package
    (inherit (@ (gnu packages crates-io) rust-block-buffer-0.10))
    (name "rust-block-buffer")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "097k9xkd8gqrl03qg4fwhjvanp3ac0pq4drg8pynk9cyhi8zxxqb"))))))

;; Doesn't build against new block-buffer
(define rust-md-5
  (package
    (inherit (@ (gnu packages crates-io) rust-md-5-0.9))
    (name "rust-md-5")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "10h5kna43cpggp9hy1hz4zb1qpixdl4anf3hdj3gfwhb3sr4d1k5"))))))

(define rust-digest
  (package
    (inherit (@ (gnu packages crates-io) rust-digest-0.10))
    (name "rust-digest")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "01nmj9cci5qdm4q4wlmz104rzr68d5m823kdzd95bypslq68dyzj"))))))

(define rust-crypto-common
  (package
    (inherit (@ (gnu packages crates-io) rust-crypto-common-0.1))
    (name "rust-crypto-common")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-common" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1s1wpm88qlrp079mzh3dlxm9vbqs4ch016yp9pzhcdjygfi2r5ap"))))))

(define rust-sha3
  (package
    (inherit (@ (gnu packages crates-io) rust-sha3-0.9))
    (name "rust-sha3")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha3" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "11hclx8ijnlx82dyd0bh9hi629zb3vqjfsyaqlgk1dl7dhazh6w8"))))))

;; Old things don't build?
(define rust-scrypt
  (package
    (inherit (@ (gnu packages crates-io) rust-scrypt-0.8))
    (name "rust-scrypt")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scrypt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0pglmppcl8mdzfxdv2x9dsjrwxhc1bm9zvxjibnlv59jnv9297lz"))))))

;; rust-scrypt@0.8 doesn't build against rust-password-hash@0.3
(define rust-password-hash
  (package
   (inherit (@ (gnu packages crates-io) rust-password-hash-0.3))
   (name "rust-password-hash")
   (version "0.4.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "password-hash" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1552dd98v6yd4l5myz4g1r2hzln8dfng22638590dc4gpi5fjag0"))))))

;; rust-pbkkdf2@0.10 doesn't build
(define rust-pbkdf2
  (package
   (inherit (@ (gnu packages crates-io) rust-pbkdf2-0.10))
   (name "rust-pbkdf2")
   (version "0.11.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "pbkdf2" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"))))))

(define rust-salsa20
  (package
    (inherit (@ (gnu packages crates-io) rust-salsa20-0.9))
    (name "rust-salsa20")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "salsa20" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))))))
(define rust-cipher;; TODO WIP
  (package
    (inherit (@ (gnu packages crates-io) rust-cipher-0.3))
    (name "rust-cipher")
    (version "0.4.3")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "cipher" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "17mmmqaalirdx7bpdhrgzp1sd392zm08mjrr24cjr57pz1q351yi"))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages crates-io) rust-cipher-0.3))
		    (append rust-crypto-common rust-inout
			    (@ (gnu packages crates-io) rust-zeroize-1))))))
(define-public rust-block-padding ; 0.3.2 required by rust-cipher
  (package
    (inherit (@ (gnu packages crates-io) rust-block-padding-0.2))
    (name "rust-block-padding")
    (version "0.3.2")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "block-padding" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "0y5v92alqzn9ikmyqfl3a4j6va87j967ii2n3jh2h330z4nyr40a"))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages crates-io) rust-block-padding-0.2))
		    (append (@ (gnu packages crates-io) rust-generic-array-0.14))))))

(define rust-block-modes ; 0.8.1 uses removed NewBlockCipher
  (package
    (inherit (@ (gnu packages crates-io) rust-block-modes-0.8))
    (name "rust-block-modes")
    (version "0.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "block-modes" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "07pjna64v0ng30j8ss9w7rv7k7l7gsii37yxm011a1kzh6q128ly"))))))

(define rust-aes ; 0.7.1 uses removed NewBlockCipher
  (package
    (inherit (@ (gnu packages crates-io) rust-aes-0.7))
    (name "rust-aes")
    (version "0.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "aes" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "1fj03mqa45nf2scxcd7mvg1xcbavrkqlmkfzwcgnx660g0si7q5z"))))))

(define rust-des ; 0.7.0 uses removed NewBlockCipher
  (package
    (inherit (@ (gnu packages crates-io) rust-des-0.7))
    (name "rust-des")
    (version "0.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "des" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "07kshslxanmg0g6007scvglfhg6mli2a8qzhx4kxx4z9ik781pgz"))))))

(define rust-ctr ; 0.8.1 uses private ciphererrors:LoopError
  (package
    (inherit (@ (gnu packages crates-io) rust-ctr-0.8))
    (name "rust-ctr")
    (version "0.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "ctr" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "18222kdhghd2j5vc5lm7bqy6glk5wbvzz1sydghd1xdsrwlz650d"))))))

;; rust-cipher expects a rust-typenum that has 'static' lifetimes in some places,
;; see <https://github.com/RustCrypto/traits/pull/937>.
(define rust-typenum
  (package
   (inherit (@ (gnu packages crates-io) rust-typenum-1))
   (name "rust-typenum")
   (version "1.15.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "typenum" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))))

;; Some packages require new_unwrap, which is not present in old versions
(define rust-const-oid
  (package
   (inherit (@ (gnu packages crates-io) rust-const-oid-0.6))
   (name "rust-const-oid")
   (version "0.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "const-oid" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0q8n1zsa73130hxa2w88qw36g8nprz21j52abpva3khm59a26bkj"))))))
;; Old rust-sha1 doesn't implement CoreProxy while required by rust-pkcs5
(define rust-sha1
  (package
   (inherit (@ (gnu packages crates-io) rust-sha1-0.6))
   (name "rust-sha1")
   (version "0.10.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "sha1" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0bw56hxajrgb3pjg0cr5xrvmx0jna39564iw2p14ama5cmzlwzy7"))))))
(define rust-rsa ;; rust-rsa@0.5 doesn't build against new rust-pkcs1
  (package
   (inherit (@ (gnu packages crates-io) rust-rsa-0.5))
   (name "rust-rsa")
   (version "0.6.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "rsa" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "02viiiylxpk2hx5h5qrpm4lcd8ildvafbw0rn6rx44wnqia2gwjc"))))
   (inputs
    (modify-inputs (package-inputs (p rust-rsa-0.5))
      (prepend (p rust-rand-core-0.6))))))

(define rust-backtrace ;; old rust-backtrace doesn't build against new rust-object
  (package
   (inherit (p rust-backtrace-0.3))
   (name "rust-backtrace")
   (version "0.3.65")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "backtrace" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0qggp0d8pbw5vfnpm0r7lrn6wmh5yjiz4yc4bzynb8l26i2pv88i"))))))
(define rust-gimli ;; new rust-backtrace doesn't build against old rust-gimli
  (package
   (inherit (p rust-gimli-0.23))
   (name "rust-giml")
   (version "0.26.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "gimli" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1m0vi36ypv4gx9gzcw6y456yqnlypizhwlcqrmg6vkwd0lnkgk3q"))))))
(define rust-addr2line ;; new rust-addr2line doesn't build against old rust-gimli
  (package
   (inherit (p rust-addr2line-0.14))
   (name "rust-addr2line")
   (version "0.17.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "addr2line" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0sw16zqy6w0ar633z69m7lw6gb0k1y7xj3387a8wly43ij5div5r"))))))
(define rust-instant
  (package
   (inherit (p rust-instant-0.1))
   (name "rust-instant")
   (version "0.1.12")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "instant" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0b2bx5qdlwayriidhrag8vhy10kdfimfhmb3jnjmsz2h9j1bwnvs"))))
   ;;TODO: add upstream Guix
   (inputs (modify-inputs (package-inputs (p rust-instant-0.1))
			  (append (p rust-cfg-if-1))))))
(define-public rust-syscallz ; @0.15 doesn't build
  (package
   (inherit (p rust-syscallz-0.15))
   (name "rust-syscallz")
   (version "0.16.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "syscallz" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "05ws3w0lfbcazx2ay5nkfhgbs4kb6nvk230kbvql44ykjrjm0jnr"))))))
(define-public rust-strum-macros ; needed by rust-syscallz
  (package
   (inherit (p rust-strum-macros-0.21))
   (name "rust-strum-macros")
   (version "0.24.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "strum-macros" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1vyh8m1skr8h4f2lnhnq1r7v3mah545bp4k1p8z4svj42ydhfy38"))))
   (inputs
    (modify-inputs (package-inputs (p rust-strum-macros-0.21))
      (append (p rust-rustversion-1))))))

(define rust-strum ; needed by rust-syscallz
  (package
   (inherit (p rust-strum-0.21))
   (name "rust-strum")
   (version "0.24.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "strum" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1y77vshrhm1grlgcfmnm0nxpsv0pb5zcb97zy6rbh106nz0wysp9"))))))

(define rust-actix-derive ; old one doesn't build against new rust-syn
  (package
   (inherit (p rust-actix-derive-0.5))
   (name "rust-actix-derive")
   (version "0.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "actix-derive" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "19rp2xcwqf5p4q5h6xxzb44xsfgpvvnnsis3l0dngnffw7zbhi3d"))))))


;;Not yet inGuix,requiredby rust-cipher
(define rust-inout
  (package
   (name "rust-inout")
   (version "0.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "inout" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))))
   (build-system (@ (guix build-system cargo) cargo-build-system))
   (arguments
    `(#:cargo-inputs
      (("rust-block-padding" ,(@ (gnu packages crates-io) rust-block-padding-0.2)) ;; XXX 0.3
       ("rust-generic-array" ,(@ (gnu packages crates-io) rust-generic-array-0.14)))))
   (home-page "https://github.com/RustCrypto/utils")
   (synopsis
    "Custom reference types for code generic over in-place and buffer-to-buffer modes of operation.")
   (description
    "Custom reference types for code generic over in-place and buffer-to-buffer modes
of operation.")
   (license '(list license:expat license:asl2.0))))

;; devise-core@0.2 requires unstable
(define rust-devise-core
  (package
   (inherit (p rust-devise-core-0.2))
   (name "rust-devise-core")
   (version "0.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "devise-core" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1l00qiih4z14ai0c3s16nlvw0kv4p07ygi6a0ms0knc78xpz87l4"))))
   (inputs
    (modify-inputs (package-inputs (p rust-devise-core-0.2))
      (append rust-proc-macro2-diagnostics)))))
(define rust-devise-codegen ;; 0.2 doesn't build against new rust-quote / rust-devise-core.
  (package
    (inherit (p rust-devise-codegen-0.2))
    (name "rust-devise-codegen")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "devise-codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1cp7nnfwvjp6wfq11n0ffjjrwfa1wbsb58g1bz3ha6z5lvkp6g0j"))))))

(define rust-proc-macro2-diagnostics ; not yet in Guix but required by rust-devise-core
  (package
    (name "rust-proc-macro2-diagnostics")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2-diagnostics" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1nmazlb1dkznjds7qwms7yxhi33ajc3isji2lsgx8r3lsqk9gwjb"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,(p rust-proc-macro2-1))
         ("rust-quote" ,(p rust-quote-1))
         ("rust-syn" ,(p rust-syn-1))
         ("rust-version-check" ,(p rust-version-check-0.9))
         ("rust-yansi" ,(p rust-yansi-0.5)))
        #:cargo-development-inputs
        (("rust-trybuild" ,(p rust-trybuild-1)))))
    (home-page "")
    (synopsis "Diagnostics for proc-macro2.")
    (description "Diagnostics for proc-macro2.")
    (license '(list license:expat license:asl2.0))))

(define rust-hash32-derive ; @0.1.0 doesn't build gainst new rust-quote and  friends
  (package
   (inherit (p rust-hash32-derive-0.1))
   (name "rust-hash32-derive")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "hash32-derive" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1zy60cdqrccd9kc8w4hvk1q584b4gjr4d48n3dff42xn6alapljr"))))))

(define rust-as-slice ; 0.1 uses multiple generic-array version which antioxidant doesn't support (TODO?)
  (package
    (name "rust-as-slice")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "as-slice" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05j52y1ws8kir5zjxnl48ann0if79sb56p9nm76hvma01r7nnssi"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-stable-deref-trait" ,(p rust-stable-deref-trait-1)))))
    (home-page "https://github.com/japaric/as-slice")
    (synopsis "`AsSlice` and `AsMutSlice` traits")
    (description "`AsSlice` and `AsMutSlice` traits")
    (license '(list license:expat license:asl2.0))))

(define rust-blake2 ; 0.9 doesn't build against new rust-digest.
  (package
    (inherit (p rust-blake2-0.9))
    (name "rust-blake2")
    (version "0.10.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blake2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "121k5yj3c8fr826pbh0gf0b3jly2ivzrfvz3lpxyabjvw2g89kxr"))))))

(define rust-hkdf ; 0.11 doesn't build against new rust-digest
  (package
    (inherit (p rust-hkdf-0.11))
    (name "rust-hkdf")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hkdf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0dyl16cf15hka32hv3l7dwgr3xj3brpfr27iyrbpdhlzdfgh46kr"))))))

(define rust-chacha20 ; @0.8 doesn't build against old rust-cipher
  (package
    (inherit (p rust-chacha20-0.8))
    (name "rust-chacha20")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chacha20" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "021g1917r0jzpvgah76667nzk0p3p9kj7ka5zqns1rxrqp3qkz67"))))))

(define rust-aead ; rust-aes-gcm@0.10 needs new version
  (package
    (inherit (p rust-aead-0.3))
    (name "rust-aead")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aead" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0xw8kp9j1whfdxhgmr2qf9xgslkg52zh6gzmhsh13y9w3s73nq8b"))))
    (inputs (modify-inputs (package-inputs (p rust-aead-0.3))
	      (append (p rust-rand-core-0.6)))))) ; new dependency

(define rust-aes-gcm ; @0.8 doesn't build against old rust-cipher
  (package
    (inherit (p rust-aes-gcm-0.8))
    (name "rust-aes-gcm")
    (version "0.10.0-pre")
    (source
     (origin
      (method url-fetch)
      (uri (crate-uri "aes-gcm" version))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "0krsrhji8j5smi35rdg0r31adx1nrpnb1fkpzwl5xipj7yrfh140"))))))

(define rust-enum-to-u8-slice-derive ; 0.1.0 doesn't build against new rust-syn
  (package
    (inherit (p rust-enum-to-u8-slice-derive-0.1))
    (name "rust-enum-to-u8-slice-derive")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enum-to-u8-slice-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1ab19saqm5vdhz15gshpj5xj2p6j9i3708mya25vdgyrmb7y1kpc"))))
    (inputs
     ;; TODO: do macro inputs go in native-inputs or inputs?
     (modify-inputs (package-inputs (p rust-enum-to-u8-slice-derive-0.1))
       (prepend (p rust-proc-macro2-1))))))

(define rust-input-buffer
  (package
    (inherit (p rust-input-buffer-0.3)) ; @0.3 doesn't build against rust-bytes@1
    (name "rust-input-buffer")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "input-buffer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "044qxqdkcq6mv07bsvm35hl7hy3rmf87lrxjyz8zaq57i0xngvmc"))))))

(define rust-system-deps ; @3 doesn't build against new rust-heck
  (package
    (inherit (p rust-system-deps-3))
    (name "rust-system-deps")
    (version "6.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "system-deps" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "02g750rlhh7ynqa3p4a3qm7jrkjp3d0jlzrl29z225ch9hf5m951"))))))

(define rust-version-compare ; rust-system-deps needs new version
  (package
    (inherit (p rust-version-compare-0.0.11))
    (name "rust-version-compare")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version-compare" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wyasmnqqngvm54x0gsxbwpxznvn747jkp0dx1nnppy1j9xj927y"))))))

(define rust-ansi-parser ; old version doesn't build against new rust-nom
  (package
    (inherit (p rust-ansi-parser-0.6))
    (name "rust-ansi-parser")
    (version "0.6.0") ; TODO: 0.6.0/0.7.0/0.8.0?
    (source
     ;; For nom 0.7 compatibility, submitted upstream at
     ;; <https://gitlab.com/davidbittner/ansi-parser/-/merge_requests/11>
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://gitlab.com/emixa-d/ansi-parser")
	     (commit "bcf3d534e38d6afaca9e898cef1af7fa3e0ecdb3")))
       (sha256
	(base32 "0jhsd8vhz0z0x8p3k5gaf8wwyn253n5fjvf27sdvv4nkh4b1cp2d"))
       (file-name (git-file-name name version))))))

;; Some of these are only used for tests, cause cycles, ???,
;; so remove them.  (TODO: some of them can probably now be removed.)
;; TODO: write a "guix style" thing for doing this.
(define %removed-dependencies
  '("rust-blakeout" ; doesn't build and no new version available, let's avoid for now.
    "rust-derive-error-chain" ; doesn't build (even when updated) and unmaintained, avoid it for now
    "rust-crypto-tests" ; test dependency doesn't build against new rust-digest, avoid for now
    "rust-quickcheck" ; (quickcheck env-logger humantime chrono bincode) cycle
    "rust-pear" "rust-pear-codegen" ; current version in Guix requires non-stable
    "rust-defmt" ; accidentally requires unstable-test?
    "rust-heapsize-plugin" ; makes use of removed features
    "rust-rustc-test" ; doesn't build against recent rust-time
    "rust-speculate" ; @0.1.2 doesn't build against recent rust-syn
    "rust-skeptic" ; @0.13.4 doesn't build
    "rust-boxxy" ; doesn't build and not supposed to be used ‘in production’
    "rust-macrotest"
    "rust-afl" ; TODO: move to 'native-inputs'/development-inputs
    "rust-js-sys" ; TODO: guix doesn't support those targets (yet)
    "rust-cortex-m" ; ARM targets not yet supported for Rust in Guix
    ;;"rust-cc" ;; todo: build.rs, hence move to 'native-inputs'?
    "rust-stdweb" "rust-web-sys" ;; web, js, wasm?
    "rust-bencher" ; FTB
    "rust-criterion" "rust-criterion-cycles-per-byte" ;; fails to build because rust-async-log-attributes fails to build
    "rust-proptest"
    "rust-futures-util-preview" ; futures-util has been updated?
    "rust-errno-dragonfly" ;; TODO: DragonflyBSD not supported
    ;; TODO: how do the three following crates even work?
    "rust-rustc-std-workspace-std"
    "rust-rustc-std-workspace-core"
    "rust-rustc-std-workspace-alloc"
    "rust-runtime" ; deprecated and doesn't build
    ;; rust-structopt-derive doesn't build and upstream recommends
    ;; migrating to 'clap'
    #;"rust-structopt" #;"rust-structopt-derive"
    "rust-compiler-builtins"
    "rust-compiletest-rs" ;; TODO: rustc-dev?
    "rust-winapi" "rust-kernel32-sys" "rust-winreg" ; skip Windows support for now
    "rust-nodrop-union" ; required unstable, and deprecated
    "rust-sleef-sys" ; requires unstable
    "rust-packed-simd" ; requires unstable (TODO: rust-packed-simd-2?)
    "rust-security-framework" "rust-cocoa" "rust-cocoa-foundation" "rust-core-foundation" "rust-core-foundation-sys" "rust-core-text" "rust-fsevent" "rust-fsevent-sys" "rust-core-video-sys" "rust-core-graphics" "rust-core-graphics-types" "rust-objc-foundation" "rust-security-framework-sys" ; non-Linux, non-Hurd things,
    "rust-mach" ; skip Mach (used by Hurd and others) support for now.
    "rust-ws2-32-sys"
    "rust-winapi-util" "rust-winapi-build"
    "rust-fuchsia-zircon" "rust-fuchsia-zircon-sys" "rust-fuchsia-cprng" ; fuchsia not supported by Guix
    "rust-dwrote" ; Windows-only, skip for now, cross-compilation can be implemented later
    "rust-core-arch" ; doesn't build, nowadays part of Rust itself?
    "rust-doc-comment"
    "rust-hermit-abi"
    "rust-model" ;; doesn't build, avoid for now
    "rust-tokio-core" ;; doesn't exist in recent tokios
    "rust-tokio-process" ;; doesn't exist in recent tokios
    "rust-tokio-executor" ;; doesn't exist in recent tokios, I think?
    "rust-tokio-io" ;; doesn't exist in recent tokios, I think?
    #;"rust-lazy-static"
    "rust-version-sync"
    "rust-trybuild"
    "rust-clippy"
    "rust-tokio-mock-task" ; doesn't build
    "rust-tokio-test"
    "rust-rand-xorshift"
    "rust-serde-test"
    "rust-wasm-bindgen" "rust-wasi"
    "rust-wasm-bindgen-test"))

(define %features
  ;; rust-rsa requires "prime" and "zeroize"
  `(("rust-num-bigint-dig" ,#~'("default" "prime" "zeroize"))
    ;; TODO: investigate build_dictionaries, and maybe not embedding libraries.
    ;; TODO: cannot choose multiple normalization forms, is this important?
    ("rust-hyphenation" ,#~'("embed_all"))
    ;; The "dox" feature requires non-stable.
    ("rust-glib-sys" ,#~'("v2_68"))
    ("rust-glib" ,#~'("log" "log_macros" "v2_68")) ; likewise
    ("rust-gobject-sys" ,#~'("v2_68")) ; likewise
    ("rust-gio-sys" ,#~'("v2_66")) ; likewise
    ("rust-gio" ,#~'("v2_66")) ; likewise
    ("rust-atk-sys" ,#~'("v2_34")) ; likewise (for dox)
    ("rust-pango-sys" ,#~'("v1_46")) ; likewise (for dox)
    ("rust-gdk-pixbuf-sys" ,#~'("v2_40")) ; likewise (for dox)
    ("rust-gdk-pixbuf" ,#~'("v2_40")) ; likewise (for dox)
    ("rust-gdk-sys" ,#~'("v3_24")) ; likewise (for dox) (look in the .pc for the version)
    ("rust-gdk" ,#~'("v3_24")) ; likewise (for dox) (look in the .pc for the version)
    ("rust-gtk-sys" ,#~'("v3_24_11")) ; likewise (for dox)
    ("rust-gtk" ,#~'("v3_24_9")) ; likewise (for dox)
    ("rust-atk" ,#~'("v2_34")) ; likewise
    ("rust-pango" ,#~'("v1_46")) ; likewise
    ("rust-lzma-sys" ,#~'()) ; don't enable "static" (TODO: add it to the list in antioxidant?)
    ;; Avoid "digest_trait" which requires old rust-digest@0.9.0
    ("rust-sha1collisiondetection" ,#~'("std" "structopt"))
    ;; The default "benchmarks" feature requires unstable.
    ("rust-galil-seiferas" ,#~'())
    ("rust-plotters-svg" ,#~'()) ; "debug" feature causes a build failure
    ;; Don't accidentally enable multiple encoding features, even
    ;; though rust-fmt only supports one at the time.  An encoding
    ;; will automatically be chosen.
    ("rust-defmt" ,#~'("alloc"))
    ("rust-lazycell" ,#~'()) ;; avoid nightly things
    ;; "pattern" and "benchmarks" require non-stable (rust-jetscii@0.5)
    ("rust-jetscii" ,#~'())
    ;; rust-cookie requires the non-default "parsing" and "macros" feature. Might as well enable
    ;; "formatting" as well.
    ("rust-time" ,#~'("default" "macros" "formatting" "parsing"))
    ;; Avoid "use-intrisics", which requires unstable.
    ("rust-half" ,#~'("alloc" "serialize" "std"))
    ;; Avoid removed feature(custom_derive)
    ("rust-language-tags" ,#~'())
    ("rust-tiny-keccak"
     ;; By default nothing is build, which seems rather useless.
     ;; Let's enable everything.
     ,#~'("cshake" "fips202" "k12" "keccak" "kmac" "parallel_hash" "sha3" "shake" "sp800" "tuple_hash"))
    ;; the default "generic-simd" feature required rust-packed-simd
    ;; which is currently uncompilable.
    ("rust-bytecount" ,#~'())
    ;; "paw" required by sniffglue
    ("rust-structopt" ,#~'("default" "paw"))
    ;; rust-rcgen requires "time". While at it, enable other
    ;; features as well.
    ("rust-yasna" ,#~'("default" "time" "bit-vec" "bigint" "std"))
    ;; rust-num-bigint-dig's zeroize feature requires the "derive"
    ;; feature of rust-zeroize
    ("rust-zeroize" ,#~'("default" "derive"))
    ;; For now avoid optional dependencies
    ("rust-typenum" ,#~'())
    ;; serde1 failure requires undeclared ‘Glob’ dependency
    ("rust-globset" ,#~'())
    ("rust-openssl-sys" ,#~'()) ;; avoid the 'vendored' feature
    ;; The 'backtrace' and 'petgraph' dependency has been removed.
    ;; (including petgraph causes a cycle between rust-ahash and rust-hashbrown,
    ;; but it's ‘only’ required for deadlock detection).
    ("rust-parking-lot-core" ,#~'())
    ;; asm! syntax not supported anymore, and "capture"
    ;; requires non-existent io::set_panic
    ("rust-rustc-test" ,#~'())
    ;; The 'inline-asm' feature requires non-stable
    ("rust-riscv" ,#~'())
    ("rust-smallvec" ,#~'()) ; default features require non-stable
    ;; Default serde1_lib requires unpackaged rust-serde1-lib
    ("rust-sval" ,#~'("alloc" "arbitrary-depth" "derive" "fmt" "std"))
    ;; rust-cipher requires non-default rand_core
    ("rust-crypto-common" ,#~'("std" "rand_core"))
    ;; Require rust-cipher.
    ("rust-inout" ,#~'("std" "block-padding"))
    ;; zeroize required by rust-ctr
    ("rust-cipher" ,#~'("alloc" "std" "block-padding" "rand_core" "dev" "zeroize"))
    ;; Likewise.
    ("rust-value-bag" ,#~'("std"))
    ;; rust-pkcs1 requires "pem"
    ("rust-der" ,#~'("std" "alloc" "oid" "pem"))
    ;; Required by hmac.
    ("rust-digest" ,#~'("default" "std" "mac"))
    ;; Required by 'sniffglue'
    ("rust-pktparse" ,#~'("serde"))
    ;; Avoid extra dependencies by using the C library that
    ;; is used elsewhere anyway.
    ("rust-flate2" ,#~'("zlib"))
    ;; Don't just support libclang 3.5, also include
    ;; bindings for later versions which rust-bindgen might
    ;; need.  Don't include the "runtime" feature, because
    ;; then libclang.so needs to be in LD_LIBRARY_PATH or such
    ;; to be found.  Don't include the "static" feature for
    ;; the standard reasons against static linking in Guix.
    ("rust-clang-sys" ,#~'("clang_10_0")) ; Guix by default does dynamic linking, not static linking, which would use the "static" feature IIUC
    ;; Do _not_ include 'runtime', as dlopen isn't used,
    ;; linking happens at compile time (and at program
    ;; startup).
    ("rust-bindgen" ,#~'("logging" "clap" "which-rustfmt"))
    ("rust-numtoa" ,#~'("std"))
    ;; rust-cargo-metadata requires the serialisation
    ;; / deserialisation traits.
    ("rust-semver" ,#~'("default" "serde"))
    ("rust-hyper" ,#~'("full"))
    ("rust-itoa" ,#~'("std"))
    ;; rust-x509-parser requires bigint
    ("rust-der-parser" ,#~'("default" "bigint"))
    ;; rust-x509-parser required 'crypto' and 'x509'
    ("rust-oid-registry" ,#~'("default" "crypto" "x509"))
    ("rust-similar" ,#~'("default" "text" "inline"))
    ;; 'derive' is needed by rust-ron
    ("rust-serde" ,#~'("std" "alloc" "derive"))
    ("rust-webpki" ,#~'("std" "alloc"))
    ;; Required by rust-tokio
    ;; TODO remove os-poll, as implied features are implemented.
    ("rust-mio"
     ,#~'("net" "os-ext" "os-poll"))
    ;; By default zero features are enabled, which is rather
    ;; minimalistic and often not sufficient.  TODO: teach
    ;; antioxidant about ‘implied’ features.
    ("rust-tokio"
     ,#~'("full" "io-util" "io-std"
	  ;;"feature=\"macros\""  ;; TODO
	  "net"
	  "parking_lot"
	  "process"
	  "rt"
	  "rt-multi-thread"
	  "signal"
	  "sync"
	  "time"))
    ("rust-tokio-util" ,#~'("full" "codec"))
    ;; extra-traits is required by rust-nix
    ("rust-libc" ,#~'("std" "extra_traits"))
    ;; Enable some features such that "rust-futures" actually builds.
    ("rust-futures-task"
     ,#~'("std" "alloc"))
    ("rust-futures-util"
     ,#~'("std" "alloc" "sink"
	  "io" "async-await"
	  "async-await-macro"
	  "channel"))
    ("rust-futures-core"
     ,#~'("std" "alloc"))
    ("rust-futures-channel"
     ,#~'("std" "alloc"))
    ;; Without "getrandom" or "alloc", it fails to build (TODO upstream?).
    ;; small_rngs is required by rust-phf-generator.
    ("rust-rand"
     ,#~'("std" "std_rng" "getrandom"
	  "alloc" "small_rng"))
    ;; Some features required rust-rand when using the getrandom feature,
    ;; serde for rust-rand-isaac@0.3.0 ... (now building with all features)
    ;; ("rust-rand-core" #~'("std" "getrandom"))
    ;; Required by rust-rand-core.
    ("rust-getrandom" ,#~'("std"))
    ("rust-ena" ,#~'())  ;; disable "bench", which fails for stable build
    ;; Required by rust-env-logger
    ("rust-log" ,#~'("std"))
    ;; The feature "alloc" is not set by default, causing the
    ;; build to fail (TODO: maybe report upstream?)
    ("rust-bitvec"
     ,#~'("std" "atomic" "alloc"))
    ;; Likewise.
    ("rust-chrono" ,#~'("default" "alloc"))
    ;; The non-default feature "alloc" is required by rust-pure-rust-locales.
    ("rust-nom"
     ,#~'("std" "lexical" "alloc"))
    ;; This addresses the build failure
    ;; ‘could not find `collector` in the crate root’
    ;; and ‘cannot find function `pin` in crate `epoch`’
    ("rust-crossbeam-epoch"
     ,#~'("std" "alloc"))
    ;; Required by rust-unicode-normalization
    ("rust-tinyvec" ,#~'("alloc"))
    ;; TODO: use default features from Cargo.toml
    ;; rust-serde-bytes requires the 'parsing' feature.
    ;; visit is required by rust-synstructure.
    ;; visit-mut is used by rust-tracing-attributes.
    ("rust-syn"
     ,#~'("derive" "parsing" "printing"
	  "clone-impls"
	  "proc-macro" "full"
	  "visit" "visit-mut"
	  ;; Used by rust-strum-macros
	  "extra-traits"))
    ("rust-proc-macro2"
     ;; Required by rust-serde-bytes via rust-syn.  If
     ;; absent, this causes errors like
     ;; <<https://github.com/google/cargo-raze/issues/159>.
     ,#~'("proc-macro"))
    ;; TODO: move into Guix proper?
    ("rust-hashbrown" ,#~'("default" "raw")) ; default "ahash" is required by rust-lru@0.7
    ("rust-os-str-bytes" ,#~'("raw"))))

(define %replacements
  `(("rust-blake2" ,rust-blake2)
    ("rust-semver" ,(p rust-semver-1))
    ("rust-rustc-version" ,(p rust-rustc-version-0.4)) ; @0.2.3 doesn't build against rust-semver@1
    ("rust-dotenv" ,(p rust-dotenv-0.15)) ; @0.10 doesn't build
    ("rust-quickcheck-macros" ,(p rust-quickcheck-macros-1)) ; 0.9 doesn't build against rust-syn@1
    ("rust-glib-sys" ,(@ (gnu packages crates-gtk) rust-glib-sys-0.14)) ; @0.10 doesn't build
    ("rust-glib" ,(@ (gnu packages crates-gtk) rust-glib-0.14)) ; @0.9 doesn't build
    ("rust-gobject-sys" ,(@ (gnu packages crates-gtk) rust-gobject-sys-0.14)) ; @0.10 doesn't build
    ("rust-gio-sys" ,(@ (gnu packages crates-gtk) rust-gio-sys-0.14)) ; @0.10 doesn't build
    ("rust-gdk-pixbuf-sys" ,(@ (gnu packages crates-gtk) rust-gdk-pixbuf-sys-0.14)) ; @0.10 doesn't build
    ("rust-gdk-sys" ,(@ (gnu packages crates-gtk) rust-gdk-sys-0.14)) ; no need for old versions
    ("rust-gdk" ,(@ (gnu packages crates-gtk) rust-gdk-0.14)) ; no need for old versions
    ("rust-cairo-sys-rs" ,(@ (gnu packages crates-gtk) rust-cairo-sys-rs-0.14)) ; avoid version conflicts
    ("rust-pango-sys" ,(@ (gnu packages crates-gtk) rust-pango-sys-0.14)) ; likewise
    ("rust-gtk" ,(@ (gnu packages crates-gtk) rust-gtk-0.14)) ; avoid potential problems
    ("rust-ansi-parser" ,rust-ansi-parser)
    ("rust-system-deps" ,rust-system-deps)
    ("rust-version-compare" ,rust-version-compare)
    ("rust-input-buffer" ,rust-input-buffer)
    ("rust-enum-to-u8-slice-derive" ,rust-enum-to-u8-slice-derive)
    ("rust-cookie" ,rust-cookie)
    ("rust-aead" ,rust-aead)
    ("rust-actix-threadpool" ,(p rust-actix-threadpool-0.3)) ; old rust-actix-threadpool requires old rust-futures
    ("rust-aes-gcm" ,rust-aes-gcm)
    ("rust-chacha20" ,rust-chacha20)
    ("rust-unicase" ,(p rust-unicase-2)) ; @1 doesn't build because of removed features
    ("rust-hkdf" ,rust-hkdf)
    ("rust-as-slice" ,rust-as-slice)
    ("rust-jetscii" ,(p rust-jetscii-0.5)) ; use recent version of jetscii that actually builds
    ("rust-hash32-derive" ,rust-hash32-derive)
    ("rust-hash32" ,(p rust-hash32-0.2)) ; @0.1 doesn't build
    ("rust-derive-more" ,(p rust-derive-more-0.99)) ; avoid non-building @0.15
    ("rust-devise-core" ,rust-devise-core)
    ("rust-devise-codegen" ,rust-devise-codegen)
    ("rust-openssl" ,rust-openssl)
    ("rust-openssl-sys" ,rust-openssl-sys)
    ;; The old rust-tokio-openssl@0.4 doesn't build
    ("rust-tokio-openssl" ,(p rust-tokio-openssl-0.6))
    ("rust-bindgen"
     ;; In the old version 'runtime' cannot be
     ;; disabled.
     ,(p rust-bindgen-0.59))
    ("rust-heck" ,(p rust-heck-0.4)) ; 0.3 too old for rust-strum-macros@0.24
    ("rust-peg" ,(p rust-peg-0.6)) ; 0.5 misses dependency information
    ;; Avoid potential incompatibilities.
    ;; TODO: package rust-actix-web@0.4, then the new version of
    ;; rust-actix-web-codegen can be used instead.
    #;("rust-actix-web-codegen" ,(p rust-actix-web-codegen-0.3)) ; doesn't exist?
    ("rust-actix-web" ,(p rust-actix-web-3)) ; TODO: why was this 0.3?
    ;; rust-atcix-derive@0.4.0,0.5.0 don't build against new
    ;; rust-syn@1.0.82 (Literal has been renamed to Lit)
    ("rust-actix-derive" ,rust-actix-derive)
    ("rust-typenum" ,rust-typenum)
    ("rust-syscallz" ,rust-syscallz)
    ("rust-strum" ,rust-strum)
    ("rust-strum-macros" ,rust-strum-macros)
    ("rust-actix" ,(p rust-actix-0.10))
    ("rust-backtrace" ,rust-backtrace) ; old backtrace doesn't build with the new rust-object
    ("rust-gimli" ,rust-gimli)
    ;; rust-pkcs5@0.5.0-pre.1 requires new_unwrap
    ("rust-const-oid" ,rust-const-oid)
    ("rust-aes" ,rust-aes)
    ("rust-des" ,rust-des)
    ("rust-pkcs8" ,rust-pkcs8)
    ("rust-pkcs5" ,rust-pkcs5)
    ("rust-pkcs1" ,rust-pkcs1)
    ("rust-spki" ,rust-spki)
    ("rust-der" ,rust-der)
    ("rust-sha-1" ,(p rust-sha-1-0.10))
    ("rust-sha2" ,(p rust-sha2-0.10))
    ("rust-time" ; resolve version conflict
     ,(p rust-time-0.3))
    ("rust-instant" ; 0.1.4 doesn't build against rust-time@0.3
     ,rust-instant)
    ;; 0.3 requires unstable
    ("rust-hex" ,(p rust-hex-0.4))
    ("rust-sha3" ,rust-sha3)
    ("rust-h2" ,(p rust-h2-0.3)) ; @0.2 doesn't build
    ("rust-scrypt" ,rust-scrypt)
    ("rust-password-hash" ,rust-password-hash)
    ("rust-block-modes" ,rust-block-modes)
    ("rust-ctr" ,rust-ctr)
    ("rust-salsa20" ,rust-salsa20)
    ("rust-cipher" ,rust-cipher)
    ("rust-block-padding" ,rust-block-padding)
    ("rust-streebog" ,(p rust-streebog-0.10))
    ("rust-pbkdf2" ,rust-pbkdf2)
    ("rust-hmac" ,(p rust-hmac-0.12))
    ("rust-boxxy" ,rust-boxxy)
    ("rust-block-buffer" ,rust-block-buffer)
    ("rust-enum-as-inner" ,rust-enum-as-inner)
    ("rust-md-5" ,rust-md-5)
					; TODO version conflict -- AUTOMATE?
    ("rust-syn" ,(p rust-syn-1))
    ("rust-object" ,(p rust-object-0.28))
    ("rust-addr2line" ,rust-addr2line)
    ("rust-generic-array" ,(p rust-generic-array-0.14))
    ("rust-digest" ,rust-digest)
    ("rust-crypto-common" ,rust-crypto-common)
    ("rust-rustyline" ,(p rust-rustyline-9))
    ("rust-base64" ,(p rust-base64-0.13))
    ("rust-test-case" ,rust-test-case-2)
    ("rust-slab" ,rust-slab)
    ("rust-socket2" ,(p rust-socket2-0.4))
    ("rust-insta" ,(p rust-insta-1))
    ("rust-nom" ; avoid version conflicts
     ,(p rust-nom-7))
    ;; rust-pktparse@0.5 doesn't build against nom@7
    ("rust-pktparse" ,rust-pktparse)
    ("rust-rusticata-macros" ; old version doesn't build against nom@7
     ,(p rust-rusticata-macros-4))
    ("rust-pure-rust-locales" ; old version doesn't build against nom@7
     ,rust-pure-rust-locales)
    ("rust-itoa" ,(p rust-itoa-1))
    ("rust-sct" ,(p rust-sct-0.7))
    ("rust-quote" ,(p rust-quote-1))
    ;; 0.3.0 fails to build against new rust-serde
    ("rust-linked-hash-map"
     ,(p rust-linked-hash-map-0.5))
    ("rust-rustls-native-certs"
     ;; Old versio incompatible with new rustls
     ,(p rust-rustls-native-certs-0.6))
    ("rust-ron"
     ,(p rust-ron-0.6)) ; old versions don't build
    ("rust-serde"
     ,(p rust-serde-1)) ; old versions don't build
    ("rust-sha1" ,rust-sha1)
    ("rust-rsa" ,rust-rsa)
    ("rust-hashbrown" ,(p rust-hashbrown-0.11))
    ("rust-scopeguard" ,(p rust-scopeguard-1))
    ("rust-webpki" ,(p rust-webpki-0.22))
    ;; Old versions don't build (because rust-tokio-io disappeared)
    ("rust-hyper-rustls" ,rust-hyper-rustls)
    ("rust-rustls"
     ;; Remove old (not-building) and potentially unsecure versions
     ;; Also, rust-tokio-rustls requires a newer rust-rustls.
     ,rust-rustls-0.20)
    ("rust-tokio-rustls" ;0.10.3 doesn't build
     ,rust-tokio-rustls-0.23)
    ;; TODO: respect SSL_CERT_DIR instead of hardcoding trusting
    ;; whoever Mozilla trusts.
    ;; TODO: build from source
    ;; TODO: remove certificates with restrictions
    ("rust-webpki-roots"
     ;; 0.17.0 doesn't build
     ,(p rust-webpki-roots-0.22))
    ("rust-nix" ,(p rust-nix-0.23))
    ("rust-autocfg" ,(p rust-autocfg-1))
    ("rust-bytes" ,(p rust-bytes-1))
    ("rust-tokio-io" ,rust-tokio-io-0.2)
    ("rust-tokio-codec" ,rust-tokio-io-0.2)
    ("rust-tokio-util" ,rust-tokio-util-0.7)
    ("rust-tokio" ,(p rust-tokio-1.8))
    ("rust-futures" ,rust-futures-0.3)
    ("rust-futures-channel" ,rust-futures-channel-0.3)
    ("rust-futures-core" ,rust-futures-core-0.3)
    ("rust-futures-executor" ,rust-futures-executor-0.3)
    ("rust-futures-io" ,rust-futures-io-0.3)
    ("rust-futures-sink" ,rust-futures-sink-0.3)
    ("rust-futures-task" ,rust-futures-task-0.3)
    ("rust-futures-util" ,rust-futures-util-0.3)
    ("rust-http" ; 0.1 doesn't build
     ,(p rust-http-0.2))
    ;; rust-http-body@0.1.0's dependency rust-tokio-buf doesn't
    ;; build anymore.  (TODO remove from Guix)
    ("rust-http-body" ,(p rust-http-body-0.4))
    ("rust-crossbeam-channel"
     ;; avoid old version that don't build
     ,(p rust-crossbeam-channel-0.5))
    ;; Likewise.
    ("rust-hyper"
     ,(p rust-hyper-0.14))
    ("rust-nb"
     ;; Avoid E0519, caused by multiple versions of the same crate
     ;; being used.  TODO: bump version in 'sniffglue'
     ,(p rust-nb-1))
    ("rust-num-traits" ,(p rust-num-traits-0.2))
    ("rust-cfg-if" ,(p rust-cfg-if-1))
    ("rust-env-logger" ; old versions don't build
     ,(p rust-env-logger-0.9))
    ("rust-lazy-static"
     ,(p rust-lazy-static-1))
    ("rust-rand"
     ,(p rust-rand-0.8))
    ("rust-lock-api" ; 0.3, 0.2, 0.1
     ,(p rust-lock-api-0.4))
    ("rust-sysctl" ; 0.1 does not compile (type errors)
     ,(p rust-sysctl-0.4))
    ;; The (now deprecated) rust-tempdir doesn't build
    ;; against current rust-rand, use the new rust-tempfile
    ;; instead as upstream recommends.
    ("rust-tempdir"
     ,(p rust-tempfile-3))
    ("rust-bare-metal"
     ,(p rust-bare-metal-1))
    ;; The old parking-lot doesn't build against
    ;; the new lock api.
    ("rust-parking-lot"
     ;; TODO: inherit?
     ,(p rust-parking-lot-0.11))
    ;; 0.4.30 fails to build.
    ("rust-proc-macro2" ,(p rust-proc-macro2-1))
    ("rust-log" ,(p rust-log-0.4))))

;; TODO: add these (upstream) or teach "guix style" to add them
(define %extra-inputs
  `(("rust-structopt" ; for paw feature
     (("rust-paw" ,(p rust-paw-1))))
    ("castor" ;; TODO: add them in upstream Guix
     (("rust-gio" ,(@ (gnu packages crates-gtk) rust-gio-0.14))
      ("rust-glib" ,(@ (gnu packages crates-gtk) rust-glib-0.14))
      ("rust-gdk-pixbuf" ,(@ (gnu packages crates-gtk) rust-gdk-pixbuf-0.14))
      ("rust-pango-sys" ,(@ (gnu packages crates-gtk) rust-pango-sys-0.14))
      ("rust-pango" ,(@ (gnu packages crates-gtk) rust-pango-0.14))
      ("rust-lazy-static" ,(p rust-lazy-static-1))
      ("rust-serde" ,(p rust-serde-1))
      ("rust-serde-derive" ,(p rust-serde-derive-1))
      ("rust-toml" ,(p rust-toml-0.5))
      ("rust-tempfile" ,(p rust-tempfile-3))
      ("rust-openssl" ,(p rust-openssl-0.10))
      ("rust-regex" ,(p rust-regex-1))
      ("rust-textwrap" ,(p rust-textwrap-0.12))))
    ("rust-freetype-sys"
     (("freetype" ,(@ (gnu packages fontutils) freetype))))
    ;; No need to avoid Rust dependencies.
    ("rust-flate2"
     ,(list (list "zlib" (@ (gnu packages compression) zlib))))
    ("rust-cmake"
     ,(list (list "cmake" (@ (gnu packages cmake) cmake-minimal))))
    ("rust-clang-sys"
     ;; TODO needs more work for
     ,(list (list "clang" (@ (gnu packages llvm) clang-13))))
    ;; for "pem" feature
    ("rust-der"
     (("rust-pem-rfc7468" ,(@ (gnu packages crates-io) rust-pem-rfc7468-0.2))))
    ;; for "pem" and "alloc" feature
    ("rust-pkcs1"
     (("rust-pkcs8" ,(@ (gnu packages crates-io) rust-pkcs8-0.7))))
    ;; for "cbc" feature
    ("rust-pkcs5"
     (("rust-cbc" ,rust-cbc)
      ("rust-sha1" ,rust-sha1))) ; missing dep (for pbes2)
    ("rust-sha1" (("rust-digest" ,rust-digest)
		  ("rust-cfg-if" ,(p rust-cfg-if-1)) ;missing dep
		  ("rust-cpufeatures" ,(p rust-cpufeatures-0.2))))
    ;; for "sha1" and "sha2" features
    ("rust-spki" (("rust-sha1" ,rust-sha1)
		  ("rust-base64ct" ,(p rust-base64ct-1)) ; missing dep
		  ("rust-sha2" ,(@ (gnu packages crates-io) rust-sha2-0.10))))
    ;; possibly only required by new version
    ("rust-boxxy" (("rust-anyhow" ,(@ (gnu packages crates-io) rust-anyhow-1))))
    ("rust-petgraph" (("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
    ("sniffglue" (("rust-bstr" ,(@ (gnu packages crates-io) rust-bstr-0.2))))
    ;; TODO: is this sufficient?
    ("rust-futures-core-preview"
     (("rust-futures-core" ,rust-futures-core-0.3)))
    ("rust-http-body" ; at least for 0.4
     (("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))))
    ("rust-tokio-sync"
     ;; TODO: remove 'preview' dependencies?
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-futures-sink" ,rust-futures-sink-0.3)
      ("rust-futures-util" ,rust-futures-util-0.3)))
    ("rust-tokio-util"
     (("rust-tracing" ,(p rust-tracing-0.1)))))) ; missing dependency

;; todo: ‘stub‘ rust-rustc-version to reduce deps?
;; grrr rust-backtrace
(define (vitaminate/auto* pack)
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 modules ; TODO: handle #:modules
		 install-source? ; not used by antioxidant-build-system
		 (features #~'("default")))
	 (unless (or (eq? phases '%standard-phases)
		     (not (is-cargo-toml-phases? phases)))
	   (error "phases?"))
	 (define fix-input
	   (match-lambda
	     ((label dependency . maybe-output)
	      (and (not (member (package-name dependency) %removed-dependencies))
		   ;; Maybe a test or example cycle?
		   (not (and (string=? (package-name dependency) "rust-bytemuck")
			     (string=? (package-name pack) "rust-bytemuck-derive")))
		   (not (and (string=? (package-name dependency) "rust-diesel")
			     (string=? (package-name pack) "rust-diesel-derives")))
		   (not (and (string=? (package-name dependency) "rust-rspec")
			     (string=? (package-name pack) "rust-colored")))
		   ;; Not a dependency anymore, resolve cycle.
		   (not (and (string=? (package-name dependency) "rust-pkcs1")
			     (string=? (package-name pack) "rust-pkcs8")))
		   ;; Break cycle (test or something like that?)
		   (not (and (string=? (package-name dependency) "rust-rustversion")
			     (string=? (package-name pack) "rust-quote")))
		   ;; Break cycle.
		   (not (and (string=? (package-name dependency) "rust-async-std")
			     (string=? (package-name pack) "rust-async-attributes")))
		   (not (and (string=? (package-name dependency) "rust-blocking")
			     (string=? (package-name pack) "rust-async-channel")))
		   (not (and (string=? (package-name dependency) "rust-async-net")
			     (string=? (package-name pack) "rust-async-io")))
		   ;; Optional dependency cycle
		   (not (and (string=? (package-name dependency) "rust-surf")
			     (string=? (package-name pack) "rust-async-std")))
		   ;; Optional dependency cycle
		   (not (and (string=? (package-name dependency) "rust-image")
			     (string=? (package-name pack) "rust-ravif")))
		   (not (and (string=? (package-name dependency) "rust-image")
			     (string=? (package-name pack) "rav1e")))
		   ;; TODO: rust-rav1e and rav1e
		   ;; rust-futures-cpupool isn't updated anymore and doesn't
		   ;; build anymore?
		   (not (string=? (package-name dependency) "rust-futures-cpupool"))
		   ;; The Redox operating system is not supported by Guix.
		   (not (string-prefix? "rust-redox" (package-name dependency)))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-derive" "rust-serde")))
		   ;; (Test?) cycle
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-actix-web" "rust-actix-web-codegen")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-actix-macros" "rust-actix-rt")))
		   ;; Test cycle (rust-paw <-> rust-paw-structopt).
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-paw" "rust-paw-structopt")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-paw" "rust-structopt")))
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
		   ;; Remove unused dependencies
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-flate2" "rust-cloudflare-zlib-sys")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-flate2" "rust-miniz-sys")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-flate2" "rust-miniz-oxide")))
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
				(list "rust-tokio-macros" "rust-tokio")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-parking-lot-core" "rust-backtrace")))
		   ;; See %features
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-parking-lot-core" "rust-petgraph")))
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
				     (list "rust-futures-util"))
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
				 ;; Resolve version conflicts, choose newer versions,
				 ;; etc.
				 (match (assoc (package-name dependency) %replacements)
				   ((_ new) new)
				   (#false dependency)
				   (stuff (pk 'oops stuff)
					  (error "bogus entry in %extra-inputs"))))
			  maybe-output)))))
	 ;; Detect cycles early by unthunking
	 (define i
 	   (filter-map fix-input
		       (append (match (assoc-ref %extra-inputs (package-name pack))
				 ((association-list) association-list)
				 (#false '())) ; no extra inputs
			       cargo-inputs
			       (package-inputs pack))))
	 (define n-i (filter-map fix-input
				 (append cargo-development-inputs
					 ;; TODO: move zlib of rust-libz-sys-1 from
					 ;; native-inputs to inputs.
					 (package-native-inputs pack)
					 (match (package-name pack)
					   ("rust-backtrace"
					    `(("rust-cc" ,(p rust-cc-1)))) ; missing dep
					   (_ '())))))
	 (define p-i (filter-map fix-input (package-propagated-inputs pack)))
	 (package
	  (inherit (vitaminate-library/no-inputs pack))
	  (source
	   (match (package-name pack)
	     ("rust-itoa"
	      (origin
	       (inherit (package-source pack))
	       ;; TODO: for compatibility with rust-http
	       (patches (list (local-file "rust-itoa-Reintroduce-fmt.patch")))))
	     (_ (package-source pack))))
	  (arguments (list #:features
			   ;; TODO: can some now be removed now that default features
			   ;; are enabled by default?  And maybe the features can be moved
			   ;; to Guix upstream?
			   (match (assoc (package-name pack) %features)
			     ((_ value) value)
			     (#false (match features
				       ((? gexp? f) f)
				       (('quote l)
					;; TODO: escapes, less ad-hoc
					#~'#$l))))))
	  (inputs i)
	  (native-inputs n-i)
	  (propagated-inputs p-i)
	  (search-paths
	   (append
	    (match (package-name pack)
	      ;; Make sure that PKG_CONFIG_PATH is available to build.rs.
	      ;; rust-system-deps uses rust-pkg-config, so it needs the
	      ;; search paths too -- needed for compiling rust-glib-sys@0.14.0.
	      ;; TODO: upstream Guix
	      ((or "rust-pkg-config"
		   "rust-system-deps")
	       (package-search-paths (@ (gnu packages pkg-config) pkg-config)))
	      (_ '()))
	    (package-search-paths pack)))
	  (native-search-paths
	   (append
	    (match (package-name pack)
	      ;; Make sure that PKG_CONFIG_PATH is available to build.rs.
	      ((or "rust-pkg-config"
		   "rust-system-deps")
	       (package-native-search-paths (@ (gnu packages pkg-config) pkg-config)))
	      (_ '()))
	    (package-native-search-paths pack)))))
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

;; Make some functioning packages available to "guix install ANTIOXIDATED-FOO"
;; when using channels.

(define (public-test-package base)
  (package
   (inherit base)
   (name (string-append "antioxidated-" (package-name base))))) ; don't shadow the cargo package to avoid ambiguity
(define-public antioxidated-rust-bindgen
  (public-test-package (vitaminate/auto (p rust-bindgen-0.59)))) ; fragile w.r.t. changes to code for linking to C libraries, avoid breaking it
(define-public antioxidated-agate
  (public-test-package (vitaminate/auto agate)))
(define-public antioxidated-castor
  (public-test-package (vitaminate/auto castor)))
(define-public antioxidated-diffr
  (public-test-package (vitaminate/auto (@ (gnu packages rust-apps) diffr))))
(define-public antioxidated-hexyl
  (public-test-package (vitaminate/auto (@ (gnu packages rust-apps) hexyl))))
(define-public antioxidated-sniffglue
  (public-test-package (vitaminate/auto sniffglue)))

;; For local development
(list antioxidated-rust-bindgen
      antioxidated-agate
      antioxidated-castor
      antioxidated-diffr
      antioxidated-hexyl
      antioxidated-sniffglue)
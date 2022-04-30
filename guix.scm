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
	    (#true "32")))
    ;; These CPU features are set by default, but antioxidant needs to know
    ;; these as well to set the appropriate environment variables expected by,
    ;; e.g., rust-sleef-sys.  TODO: CPU tuning.
    ,@(if (target-x86-64? target)
	  '(("CARGO_CFG_TARGET_FEATURE" . "sse,sse2"))
	  '())))

(define* (antioxidant-build name inputs #:key
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
					 ;; TODO: upstream / update
					 ((string-prefix? "rust-x509-parser" name)
					  #~((add-after 'unpack 'use-nondeprecated
					       (lambda _
						 (substitute* "src/time.rs"
						   (("use std::time::Duration;")
						    "use std::time::Duration;use std::convert::TryInto;")
						   (("\\.to_std\\(\\)") ".try_into()"))))))
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

(define antioxidant-build-system
  (build-system
   (name 'antioxidant)
   (description "Build software written in Rust, without cargo")
   (lower lower)))

;; Convert from cargo-build-system to antioxidant-build-system,
;; for now leaving inputs intact.
(define* (vitaminate-library/no-inputs crate-package
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

;; rust-tokio-util needs a slab with 'compact'
(define-public rust-slab
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

(define-public rust-hyper-rustls
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
(define-public rust-boxxy
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
(define-public rust-pktparse
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
(define-public rust-pure-rust-locales
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
(define-public sniffglue
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

;; needed for rcgen
(define-public rust-yasna
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

;; not yet in Guix, but needed for updated agate
(define-public rust-rcgen
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
(define-public rust-pkcs5
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
(define-public rust-pkcs8 ;; old pkcs8 doesn't build against new rust-der
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
(define-public rust-cbc
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


;; Old agate doesn't build
(define-public agate
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

;; Old version doesn't have the block_buffer required by rust-sha3
(define-public rust-block-buffer
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
(define-public rust-md-5
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

(define-public rust-digest
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

(define-public rust-crypto-common
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

(define-public rust-sha3
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
(define-public rust-scrypt
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
(define-public rust-password-hash
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
(define-public rust-pbkdf2
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

(define-public rust-salsa20
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
(define-public rust-cipher;; TODO WIP
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

(define-public rust-block-modes ; 0.8.1 uses removed NewBlockCipher
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

(define-public rust-aes ; 0.7.1 uses removed NewBlockCipher
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

(define-public rust-des ; 0.7.0 uses removed NewBlockCipher
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

(define-public rust-ctr ; 0.8.1 uses private ciphererrors:LoopError
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
(define-public rust-typenum
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
(define-public rust-const-oid
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
(define-public rust-sha1
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
(define-public rust-rsa ;; rust-rsa@0.5 doesn't build against new rust-pkcs1
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
      (base32 "02viiiylxpk2hx5h5qrpm4lcd8ildvafbw0rn6rx44wnqia2gwjc"))))))
(define-public rust-backtrace ;; old rust-backtrace doesn't build against new rust-object
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
(define-public rust-gimli ;; new rust-backtrace doesn't build against old rust-gimli
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
(define-public rust-addr2line ;; new rust-addr2line doesn't build against old rust-gimli
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
(define-public rust-instant
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
      (base32 "0b2bx5qdlwayriidhrag8vhy10kdfimfhmb3jnjmsz2h9j1bwnvs"))))))

;;Not yet inGuix,requiredby rust-cipher
(define-public rust-inout
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

;; Some of these are only used for tests, cause cycles, ???,
;; so remove them.  (TODO: some of them can probably now be removed.)
;; TODO: write a "guix style" thing for doing this.
(define %removed-dependencies
  '("rust-quickcheck" ; (quickcheck env-logger humantime chrono bincode) cycle
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
    "rust-criterion"
    "rust-proptest"
    "rust-futures-util-preview" ; futures-util has been updated?
    "rust-errno-dragonfly" ;; TODO: DragonflyBSD not supported
    ;; TODO: how do the three following crates even work?
    "rust-rustc-std-workspace-std"
    "rust-rustc-std-workspace-core"
    "rust-rustc-std-workspace-alloc"
    "rust-compiler-builtins"
    "rust-compiletest-rs" ;; TODO: rustc-dev?
    "rust-winapi" "rust-kernel32-sys" ; skip Windows support for now
    "rust-nodrop-union" ; required unstable, and deprecated
    "rust-sleef-sys" ; requires unstable
    "rust-packed-simd" ; requires unstable (TODO: rust-packed-simd-2?)
    "rust-security-framework" "rust-cocoa" "rust-cocoa-foundation" "rust-core-foundation" "rust-core-foundation-sys" "rust-core-text" "rust-fsevent" "rust-fsevent-sys" "rust-objc-foundation" ; non-Linux, non-Hurd things
    "rust-ws2-32-sys"
    "rust-winapi-util" "rust-winapi-build"
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
    "rust-rustversion" "rust-trybuild"
    "rust-clippy"
    "rust-tokio-mock-task" ; doesn't build
    "rust-tokio-test"
    "rust-rand-xorshift"
    "rust-walkdir"
    "rust-serde-test"
    "rust-wasm-bindgen" "rust-wasi"
    "rust-wasm-bindgen-test"))

(define %features
  ;; rust-rsa requires "prime" and "zeroize"
  `(("rust-num-bigint-dig" ,#~'("default" "prime" "zeroize"))
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
    ;; The 'backtrace' dependency has been removed.
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
    ;; need.
    ("rust-clang-sys" ,#~'("default" "clang_10_0"))
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
    ("rust-hashbrown" ,#~'("raw"))
    ("rust-os-str-bytes" ,#~'("raw"))))

;; todo: ‘stub‘ rust-rustc-version to reduce deps?
;; grrr rust-backtrace
(define (vitaminate/auto* pack)
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 (features #~'("default")))
	 (unless (or (eq? phases '%standard-phases)
		     (not (is-cargo-toml-phases? phases)))
	   (error "phases?"))
	 (define fix-input
	   (match-lambda
	     ((label dependency . maybe-output)
	      (and (not (member (package-name dependency) %removed-dependencies))
		   ;; Not a dependency anymore, resolve cycle.
		   (not (and (string=? (package-name dependency) "rust-pkcs1")
			     (string=? (package-name pack) "rust-pkcs8")))
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
				   (("rust-bindgen" _)
				    ;; In the old version 'runtime' cannot be
				    ;; disabled.
				    (@ (gnu packages crates-io) rust-bindgen-0.59))
				   (("rust-typenum" _) rust-typenum)
				   (("rust-backtrace" _) rust-backtrace) ; old backtrace doesn't build with the new rust-object
				   (("rust-gimli" _) rust-gimli)
				   ;; rust-pkcs5@0.5.0-pre.1 requires new_unwrap
				   (("rust-const-oid" _) rust-const-oid)
				   (("rust-aes" _) rust-aes)
				   (("rust-des" _) rust-des)
				   (("rust-pkcs8" _) rust-pkcs8)
				   (("rust-pkcs5" _) rust-pkcs5)
				   (("rust-pkcs1" _) rust-pkcs1)
				   (("rust-spki" _) rust-spki)
				   (("rust-der" _) rust-der)
				   (("rust-sha-1" _) 
				    (@ (gnu packages crates-io) rust-sha-1-0.10))
				   (("rust-sha2" _) 
				    (@ (gnu packages crates-io) rust-sha2-0.10))
				   (("rust-time" _) ; resolve version conflict
				    (@ (gnu packages crates-io) rust-time-0.3))
				   (("rust-instant" _) ; 0.1.4 doesn't build against rust-time@0.3
				    rust-instant)
				   ;; 0.3 requires unstable
				   (("rust-hex" _)
				    (@ (gnu packages crates-io) rust-hex-0.4))
				   (("rust-sha3" _) rust-sha3)
				   (("rust-scrypt" _) rust-scrypt)
				   (("rust-password-hash" _) rust-password-hash)
				   (("rust-block-modes" _) rust-block-modes)
				   (("rust-ctr" _) rust-ctr)
				   (("rust-salsa20" _) rust-salsa20)
				   (("rust-cipher" _) rust-cipher)
				   (("rust-block-padding" _) rust-block-padding)
				   (("rust-streebog" _) (@ (gnu packages crates-io) rust-streebog-0.10))
				   (("rust-pbkdf2" _) rust-pbkdf2)
				   (("rust-hmac" _) (@ (gnu packages crates-io) rust-hmac-0.12))
				   (("rust-boxxy" _) rust-boxxy)
				   (("rust-block-buffer" _) rust-block-buffer)
				   (("rust-md-5" _) rust-md-5)
				   ;; TODO version conflict -- AUTOMATE?
				   (("rust-syn" _) (p rust-syn-1))
				   (("rust-object" _) (p rust-object-0.28))
				   (("rust-addr2line" _) rust-addr2line)
				   (("rust-generic-array" _) (@ (gnu packages crates-io) rust-generic-array-0.14))
				   (("rust-digest" _) rust-digest)
				   (("rust-crypto-common" _) rust-crypto-common)
				   (("rust-rustyline" _)
				    (@ (gnu packages crates-io) rust-rustyline-9))
				   (("rust-base64" _)
				    (@ (gnu packages crates-io) rust-base64-0.13))
				   (("rust-test-case" _) rust-test-case-2)
				   (("rust-slab" _) rust-slab)
				   (("rust-socket2" _) (@ (gnu packages crates-io) rust-socket2-0.4))
				   (("rust-insta" _)
				    (@ (gnu packages crates-io) rust-insta-1))
				   (("rust-nom" _) ; avoid version conflicts
				    (@ (gnu packages crates-io) rust-nom-7))
				   ;; rust-pktparse@0.5 doesn't build against nom@7
				   (("rust-pktparse" _) rust-pktparse)
				   (("rust-rusticata-macros" _) ; old version doesn't build against nom@7 
				    (@ (gnu packages crates-io) rust-rusticata-macros-4))
				   (("rust-pure-rust-locales" _) ; old version doesn't build against nom@7
				    rust-pure-rust-locales)
				   (("rust-itoa" _)
				    (@ (gnu packages crates-io) rust-itoa-1))
				   (("rust-sct" _)
				    (@ (gnu packages crates-io) rust-sct-0.7))
				   (("rust-quote" _)
				    (@ (gnu packages crates-io) rust-quote-1))
				   ;; 0.3.0 fails to build against new rust-serde
				   (("rust-linked-hash-map" _)
				    (@ (gnu packages crates-io) rust-linked-hash-map-0.5))
				   (("rust-rustls-native-certs" _)
				    ;; Old versio incompatible with new rustls
				    (@ (gnu packages crates-io) rust-rustls-native-certs-0.6))
				   (("rust-ron" _)
				    (@ (gnu packages crates-io) rust-ron-0.6)) ; old versions don't build
				   (("rust-serde" _)
				    (@ (gnu packages crates-io) rust-serde-1)) ; old versions don't build
				   (("rust-sha1" _) rust-sha1)
				   (("rust-rsa" _) rust-rsa)
				   (("rust-hashbrown" _)
				    (@ (gnu packages crates-io) rust-hashbrown-0.11))
				   (("rust-scopeguard" _)
				    (@ (gnu packages crates-io) rust-scopeguard-1))
				   (("rust-webpki" _)
				    (@ (gnu packages crates-io) rust-webpki-0.22))
				   ;; Old versions don't build (because rust-tokio-io disappeared)
				   (("rust-hyper-rustls" _) rust-hyper-rustls)
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
				   (("rust-autocfg" _)
				    (@ (gnu packages crates-io) rust-autocfg-1))
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
				   (("rust-crossbeam-channel" _)
				    ;; avoid old version that don't build
				    (@ (gnu packages crates-io) rust-crossbeam-channel-0.5))
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
	 (define i
 	   (filter-map fix-input
		       (append (match (package-name pack)
				 ;; No need to avoid Rust dependencies.
				 ("rust-flate2"
				  (list (list "zlib" (@ (gnu packages compression) zlib))))
				 ("rust-cmake"
				  (list (list "cmake" (@ (gnu packages cmake) cmake-minimal))))
				 ("rust-clang-sys"
				  ;; TODO needs more work for
				  (list (list "clang" (@ (gnu packages llvm) clang-13))))
				 ;; for "pem" feature
				 ("rust-der" `(("rust-pem-rfc7468" ,(@ (gnu packages crates-io) rust-pem-rfc7468-0.2))))
				 ;; for "pem" and "alloc" feature
				 ("rust-pkcs1" `(("rust-pkcs8" ,(@ (gnu packages crates-io) rust-pkcs8-0.7))))
				 ;; for "cbc" feature
				 ("rust-pkcs5" `(("rust-cbc" ,rust-cbc)))
				 ("rust-sha1" `(("rust-digest" ,rust-digest)
						("rust-cpufeatures" ,(p rust-cpufeatures-0.2))))
				 ;; for "sha1" and "sha2" features
				 ("rust-spki" `(("rust-sha1" ,rust-sha1)
						("rust-sha2" ,(@ (gnu packages crates-io) rust-sha2-0.10))))
				 ;; possibly only required by new version
				 ("rust-boxxy" `(("rust-anyhow" ,(@ (gnu packages crates-io) rust-anyhow-1))))
				 ("rust-petgraph" `(("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
				 ("sniffglue" `(("rust-bstr" ,(@ (gnu packages crates-io) rust-bstr-0.2))))
				 ;; TODO: is this sufficient?
				 ("rust-futures-core-preview"
				  `(("rust-futures-core" ,rust-futures-core-0.3)))
				 ("rust-http-body" ; at least for 0.4
				  `(("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))))
				 ("rust-tokio-sync"
				  `(("rust-futures-core" ,rust-futures-core-0.3)
				    ("rust-futures-util" ,rust-futures-util-0.3)))
				 (_ '()))
			       cargo-inputs
			       (package-inputs pack))))
	 (define n-i (filter-map fix-input
				 (append cargo-development-inputs
					 ;; TODO: move zlib of rust-libz-sys-1 from
					 ;; native-inputs to inputs.
					 (package-native-inputs pack))))
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

(map vitaminate/auto (list sniffglue (@ (gnu packages rust-apps) hexyl)))

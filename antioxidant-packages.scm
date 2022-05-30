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
	     (gnu packages guile) (ice-9 match) (srfi srfi-1) (srfi srfi-71)
	     (gnu packages rust-apps) (guix utils) (srfi srfi-26)
	     (guix git-download) (ice-9 optargs) ((guix licenses) #:prefix license:)
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

(define %custom-phases
  ;; TODO home page is incorrect
  `(("rust-servo-fontconfig-sys"
     ,#~((add-after 'unpack 'unbundle
	   (lambda _
	     (for-each ((@ (srfi srfi-26) cut) delete-file-recursively <>)
		       '("doc" "test" "src" "missing" "m4" "fonts.dtd" "fonts.conf.in"
			 "fontconfig.spec" "fontconfig.spec.in" "fontconfig.pc.in"
			 "fontconfig-zip.in" "fontconfig" "fc-validate"
			 "fc-scan" "fc-query" "fc-pattern" "fc-match" "fc-list"
			 "fc-lang" "fc-glyphname" "fc-cat" "fc-case"
			 "depcomp" "configure.ac" "configure" "config.sub" "config.h.in"
			 "config.guess" "config-fixups.h" "conf.d" "compile" "aclocal.m4"
			 "Tools.mk" "Makefile.in"
			 "README" "NEWS" "INSTALL" "ChangeLog" "AUTHORS" ; these are for freetype, not rust-servo-freetype-sys
			 "COPYING" ; this is for freetype, not rust-servo-freetype-sys which is MPL
			 ))))))
    ("rust-mesalink" ,#~((delete 'bootstrap))) ; build.rs is sufficient
    ("rust-backtrace-sys"
     ,#~((add-after 'unpack 'break-cycle
	   (lambda _
	     ;; only needed for Android targets,
	     ;; by removing it we avoid depending
	     ;; on crate-cc, breaking a cycle
	     (delete-file "build.rs")
	     (substitute* "Cargo.toml"
	       (("^build =(.*)$") ""))))))
    ("rust-libssh2-sys"
     ;; Otherwise, build.rs fails to find libssh2, causing
     ;; a build failure.
     ,#~((add-after 'unpack 'find-ssh2
	   (lambda _
	     (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "don't use the bundled copy in the git submodule")))))
    ("rust-freetype-sys"
     ,#~((add-after 'unpack 'unbundle
	   (lambda _ ; TODO: move to origin snippet (& upstream Guix?)
	     (delete-file-recursively "freetype2")))))
    ("rust-ed25519-dalek"
     ;; TODO: upstream
     ,#~((add-after 'unpack 'fix-uses
	   (lambda _
	     (substitute* "src/secret.rs"
	       (("use curve25519_dalek::digest::Digest;")
		"use curve25519_dalek::digest::{Digest,Update};")
	       ;; Resolve the resulting ambiguity
	       (("h\\.update\\(secret_key\\.as_bytes\\(\\)\\)")
		"Digest::update(&mut h, secret_key.as_bytes())")
	       (("h\\.update\\(&self.nonce\\)")
		"Digest::update(&mut h, &self.nonce)")
	       (("h\\.update\\(&message\\)")
		"Digest::update(&mut h, &self.nonce)")
	       (("h\\.update\\(R\\.as_bytes\\(\\)\\)")
		"Digest::update(&mut h, R.as_bytes())")
	       (("h\\.update\\(public_key\\.as_bytes\\(\\)\\)")
		"Digest::update(&mut h, public_key.as_bytes())"))))))
    ;; TODO: in upstream Guix, replace
    ;; (delete-file-recursively "jemalloc")
    ;; by (delete-file-recursively "rep")
    ;; TODO: why a static library?
    ("rust-jemalloc-sys"
     ,#~((add-after 'unpack 'unbundle
	   (lambda _
	     (delete-file-recursively "rep")))
	 ;; keep upstream phase
	 (add-before 'configure 'find-jemalloc
	   (lambda* (#:key inputs #:allow-other-keys)
	     (setenv "JEMALLOC_OVERRIDE"
		     (search-input-file inputs "lib/libjemalloc.so.2"))))))
    ;; TODO: upstream / update
    ("rust-x509-parser"
     ,#~((add-after 'unpack 'use-nondeprecated
	   (lambda _
	     (substitute* "src/time.rs"
	       (("use std::time::Duration;")
		"use std::time::Duration;use std::convert::TryInto;")
	       (("\\.to_std\\(\\)") ".try_into()"))))))
    ;; Preserve this phase from (gnu packages crates-io)
    ("rust-pkg-config"
     ,#~((add-after 'unpack 'hardcode-pkg-config-loation
	   (lambda* (#:key inputs #:allow-other-keys)
	     (substitute* "src/lib.rs"
	       (("\"pkg-config\"")
		(string-append "\"" (assoc-ref inputs "pkg-config")
			       "/bin/pkg-config\"")))))))
    ;; TODO: Upstream/update
    ("rust-structopt-derive"
     ,#~((add-after 'unpack 'use-existing
	   (lambda _
	     (substitute* "src/attrs.rs"
	       (("CamelCase, KebabCase, MixedCase, ShoutySnakeCase, SnakeCase")
		;; ?? CamelCase, MixedCase
		"ToUpperCamelCase, ToKebabCase, ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase")
	       (("to_camel_case") "to_upper_camel_case")
	       (("to_mixed_case") "to_lower_camel_case"))))))
    ;; TODO: Upstream/update
    ("rust-cbindgen"
     ,#~((add-after 'unpack 'use-existing
	   (lambda _
	     (substitute* "src/bindgen/rename.rs"
	       (("to_camel_case") "to_upper_camel_case")
	       (("to_mixed_case") "to_lower_camel_case"))))))
    ;; TODO: update
    ("rust-glib-macros"
     ,#~((add-after 'unpack 'use-existing
	   (lambda _
	     (substitute* '("src/genum_derive.rs" "src/gflags_attribute.rs")
	       (("CamelCase, KebabCase") "ToUpperCamelCase, ToKebabCase")
	       (("to_camel_case") "to_upper_camel_case"))))))
    ;; TODO: add rust-peg-macros to native-inputs for
    ;; cross-compilation reasons.
    ;;
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
    ("rust-peg"
     ,#~((replace 'bootstrap
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
    ("rust-peg-macros"
     ,#~((add-after 'install 'install-grammar
	   (lambda _
	     (install-file "grammar.rustpeg" (string-append #$output "/share/rust-peg-macros"))
	     (install-file "grammar.rs" (string-append #$output "/share/rust-peg-macros"))))))
    ("rust-chrono"
     ,#~((add-after 'unpack 'use-nondeprecated-names
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
    ("rust-pkcs1"
     ,#~((add-after 'unpack 'fix-typing
	   (lambda _
	     ;; Already upstream: <https://github.com/RustCrypto/formats/blob/fbf4334be7717e1f393c3f7b9b4c85c584ce8395/pkcs1/src/lib.rs#L49>, but not yet in any release.
	     (substitute* "src/lib.rs"
	       (("ObjectIdentifier::new") "ObjectIdentifier::new_unwrap"))))))
    ("rust-mio-extras"
     ,#~((add-after 'unpack 'mio@0.6.21-compatibility
	   (lambda _
	     (substitute* '("src/channel.rs" "src/timer.rs")
	       (("::\\{Evented,") "::{event::Evented,"))))))
    ;; TODO: change in Guix upstream.
    ;; TODO: adjust README.md? Make sure LICENSE-APACHE
    ;; is installed?
    ("rust-cmake"
     ,#~((add-after 'unpack 'absolute-cmake
	   (lambda* (#:key inputs #:allow-other-keys)
	     (substitute* "src/lib.rs"
	       (("\"cmake\"") (format #f "\"~a\"" (search-input-file inputs "bin/cmake"))))))))
    ("rust-clang-sys"
     ;; TODO: are there some paths that need to be
     ;; absolutised?
     ,#~((add-after 'unpack 'set-libclang-path
	   (lambda* (#:key inputs #:allow-other-keys)
	     (setenv "LIBCLANG_PATH"
		     (dirname (search-input-file inputs "lib/libclang.so")))))))
    ;; TODO: when deciding what binaries to build,
    ;; respect [[bin]]/required-features, then this
    ;; phase can be removed.
    ("rust-phf-generator"
     ,#~((add-after 'unpack 'delete-bin
	   (lambda _
	     (delete-file "src/bin/gen_hash_test.rs")))))
    ("rust-multipart"
     ,#~((add-after 'unpack 'remove-uncompilable-example
	   (lambda _
	     (delete-file "src/bin/read_file.rs")))))
    ("rust-cpp-demangle"
     ,#~((add-after 'unpack 'delete-bin
	   (lambda _
	     (delete-file "src/bin/afl_runner.rs")))))
    ("rust-tokio-sync"
     ,#~((add-after 'unpack 'unpreview
	   (lambda _
	     (substitute* "Cargo.toml"
	       (("-preview\\]") "]"))))))
    ("dutree"
     ;; See <https://github.com/nachoparker/dutree/pull/40>
     ,#~((add-after 'unpack 'update-to-new-signal-hookversion
	   (lambda _
	     (substitute* "src/main.rs"
	       (("signal_hook::register") "signal_hook::low_level::register"))
	     (substitute* "src/main.rs"
	       (("signal_hook::SIGPIPE") "signal_hook::consts::signal::SIGPIPE"))))))
    ("rust-tuikit"
     ;; TODO: upstream
     ,#~((add-after 'unpack 'fix-unresolved+deprecated
	   (lambda _
	     (substitute* "src/raw.rs"
	       (("use nix::Error::Sys;") "")
	       (("match err \\{") "{")
	       (("nix::Error::from_errno\\(ENOTTY\\)") "ENOTTY")
	       (("Sys\\((.*)") "err.into()")
	       (("_ => (.*)$") ""))))))
    ;; 'cc' and 'c++' don't exist
    ("rust-gcc"
     ,#~((add-after 'unpack 'fix-cc
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("\"cc\"") "\"gcc\"")
	       (("\"c++\"") "\"g++\""))))))
    ("rust-cc"
     ,#~((add-after 'unpack 'fix-cc
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("\"cc\"") "\"gcc\"")
	       (("\"c++\"") "\"g++\""))))))))

(define (custom-phases name)
  (define (drop-version name)
    (let ((name _ (package-name->name+version name #\-)))
      name))
  (let ((name
	 (match name
	   ((? (cut string-prefix? "antioxidated-" <>) name)
	    (drop-version (string-drop name (string-length "antioxidated-"))))
	   ;; name+version is confused by the -alpha suffix
	   ((? (cut string-prefix? "rust-tokio-sync-0.2.0-alpha" <>) name)
	    "rust-tokio-sync")
	   ;; likewise
	   ((? (cut string-prefix? "rust-mesalink-1.1.0-cratesio" <>) name)
	    "rust-mesalink")
	   (_ (drop-version name)))))
    (match (assoc name %custom-phases)
      ((_ phases) phases)
      (#false #~()))))

(define* (antioxidant-build name inputs #:key
			    modules ; what to do about 'modules'
			    install-source? ; not used by antioxidant-build-system
			    system target source search-paths outputs
			    (rust-metadata "")
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
		     #:rust-metadata #$rust-metadata
		     #:phases (modify-phases %standard-antioxidant-phases
				#$@(custom-phases name)))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target
		(features #~'("default"))
		(rust-metadata #~"")
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

(define* (package-with-rust-features base new-features #:key (name (package-name base))
				     (rust-metadata #~""))
  "Return a variant of BASE with name NAME build with the features FEATURES.
To distinguish this variant from other variants, RUST-METADATA can be set to
an unique string, which can be useful for resolving symbol conflicts."
  (package
   (inherit base)
   (name name)
   (arguments
    (ensure-keyword-arguments
     (package-arguments base)
     (list #:features new-features #:rust-metadata rust-metadata)))))

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

(define-public rust-futures-intrusive
  (package
    (inherit (p rust-futures-intrusive-0.3)) ; 0.3 doesn't build because of a type error
    (name "rust-futures-intrusive")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-intrusive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0gmnn86ifc2ngmwf3mpiw00kmxm8m2wxxxqnchmpraj6mj97a032"))))))

(define rust-mio ; new rust-tokio is incompatible with rust-mio@0.8.0
  (package
    (inherit (p rust-mio-0.8))
    (name "rust-mio")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "168pqd9v7llhhal1jy5l1k0k8qp0g8hsddv6w1s93n24kc6magbi"))))))

;; The old tokio doesn't build against recent rust-futures
#; ; currently removed
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

(define rust-tokio ; old tokio doesn't build against new rust-mio
  (package
    (inherit (p rust-tokio-1))
    (name "rust-tokio")
    (version "1.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "158klcakw40y37kgbafg9z1y12vgflh35ad6bbfxss6g4w2by0s9"))))))

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
      (base32 "1b2wi75qrmwfpw3pqwcg1xjndl4z0aris15296wf7i8d5v04hz6q"))))
   (inputs (modify-inputs (package-inputs (p rust-hyper-rustls-0.22))
	     (prepend (p rust-http-0.2))))))

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
    (license (list license:expat license:asl2.0))))

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
    (license (list license:expat license:asl2.0))))
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
    (license (list license:expat license:asl2.0))))

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
   (license (list license:expat license:asl2.0))))

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
(define-public rust-cookie-store ; old version incompatible with new rust-time
  (package
    (inherit (p rust-cookie-store-0.15))
    (name "rust-cookie-store")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie-store" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0idrphkllykjmvx1vnjyihi3w76lphwbj6k0vqzpiib4lqvgsfzq"))))))

(define rust-partial-io
  (package
    (inherit (p rust-partial-io-0.3)) ; @0.3.1 requires old rust-futures
    (name "rust-partial-io")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "partial-io" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "17q39vnwas6f4l5wiiqzlxh8la21rzpiy010mb95d9f0bj5ik056"))))
    (inputs
     (modify-inputs (package-inputs (p rust-partial-io-0.3))
		    (prepend (p rust-tokio-1)
			     (p rust-rand-0.8)
			     (p rust-pin-project-1))))))

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

(define rust-chacha20poly1305 ; @0.9 doesn't build against rust-cipher@0.4
  (package
    (inherit (p rust-chacha20poly1305-0.9))
    (name "rust-chacha20poly1305")
    (version "0.10.0-pre")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chacha20poly1305" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1skr6v12mlaccsxd1hjgr1bvk1hs9d412g29rymnjr76f47l6v3l"))))))

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

(define rust-actix-codec ; @0.3 doesn't build against new rust-bytes@1
  (package
    (inherit (p rust-actix-codec-0.3))
    (name "rust-actix-codec")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-codec" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zm7nk8irjgkf08a6x632niwd9iprq43rdda4wqmgwx70ja5b9sp"))))))

(define rust-actix-tls
  (package
    (inherit (p rust-actix-tls-2))
    (name "rust-actix-tls")
    (version "3.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-tls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "15rj6gn83fzv5w2b2y0s690q80awsjhbjg40f3qcgkgpjbr0rplz"))))))

(define rust-actix-utils ; @2 doesn't build against new rust-actix-rt
  (package
    (inherit (p rust-actix-utils-2))
    (name "rust-actix-utils")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-utils" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "155aj87z8634mfmggfixyqy3pqhpyf7g97zrzy6piz77qamcp4g4"))))))

(define rust-local-waker
  (package
    (name "rust-local-waker")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "local-waker" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1w9zqlh18mymvb82ya0sailiy5d3wsjamaakgl70x50i6vmpckz3"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis "A synchronization primitive for thread-local task wakeup")
    (description
      "This package provides a synchronization primitive for thread-local task wakeup")
    (license (list license:expat license:asl2.0))))

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
   (license (list license:expat license:asl2.0))))

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
    (license (list license:expat license:asl2.0))))

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
    (license (list license:expat license:asl2.0))))

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

(define-public rust-async-process ; @1.0.1 is not compatible with new rust-signal-hook
  (package
    (inherit (p rust-async-process-1))
    (name "rust-async-process")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "async-process" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "037ns7dazk1z0vdpnzh2bvrnxxzd604pzl47765cgs141bihcb6g"))))
    (inputs
     (modify-inputs (package-inputs (p rust-async-process-1))
       (prepend (p rust-libc-0.2)))))) ; new dependency

(define-public rust-trust-dns-proto ; requires by rust-trust-dns-openssl@0.21
  (package
    (inherit (p rust-trust-dns-proto-0.20))
    (name "rust-trust-dns-proto")
    (version "0.21.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-proto" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0p95ig8dfp30ga6gz01m683zy33abbna0givpgac6xwqym0g4ccw"))))
    (inputs (modify-inputs (package-inputs (p rust-trust-dns-proto-0.20))
	      (prepend (p rust-tinyvec-1)
		       (p rust-tokio-native-tls-0.3)
		       (p rust-native-tls-0.2)
		       (p rust-rustls-0.20)
		       (p rust-tokio-rustls-0.22)
		       (p rust-rustls-pemfile-0.2)
		       (p rust-tokio-openssl-0.6)
		       (p rust-http-0.2)
		       (p rust-h2-0.3)
		       (p rust-bytes-1))))))

(define-public rust-trust-dns-openssl ; @0.20 doesn't build
  (package
    (inherit (p rust-trust-dns-openssl-0.20))
    (name "rust-trust-dns-openssl")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-openssl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0r8z7l757lf5yvsdxkb9324f663i3kyqkjjj3g3whilby5fz36z1"))))))
(define-public rust-trust-dns-native-tls ; likewise
  (package
    (inherit (p rust-trust-dns-native-tls-0.20))
    (name "rust-trust-dns-native-tls")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-native-tls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "18yjln9h49rj2gxy79a8pipp3wl66g7cnjqkpa7azv955mkwcw97"))))))
(define-public rust-trust-dns-rustls ; likewise
  (package
    (inherit (p rust-trust-dns-rustls-0.20))
    (name "rust-trust-dns-rustls")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-rustls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1vsyy6zn0jv30nfyzs4y5rl6rnb4dm0m502gawk3klm2xq4dr5jx"))))))

(define-public rust-trust-dns-https ; @0.20 incompatible with rust-trust-dns-proto@0.21
  (package
    (inherit (p rust-trust-dns-https-0.20))
    (name "rust-trust-dns-https")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-https" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0002375rn8hlakrvi0r0d7xm4kvcykxi93hrn2hz3hlx69gq814b"))))))

(define-public rust-trust-dns-resolver ; likewise
  (package
    (inherit (p rust-trust-dns-resolver-0.20))
    (name "rust-trust-dns-resolver")
    (version "0.21.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-resolver" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0n6m9yvhaip8dml5247d6qqdzf8bcrn4rvzwr685clc4xb175fp4"))))))

(define-public rust-askama-shared ; @0.11 doesn't build against new rust-nom
  (package
    (inherit (p rust-askama-shared-0.11))
    (name "rust-askama-shared")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "askama-shared" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1l4fycmw65zyvfabf672sj2pc0ilfcj0y6a0csygq1wa26a2nwmz"))))
    (inputs
     (modify-inputs (package-inputs (p rust-askama-shared-0.11))
       (prepend (p rust-mime-0.3)
		(p rust-mime-guess-2))))))

(define-public rust-askama-derive
  (package
    (inherit (p rust-askama-derive-0.10))
    (name "rust-askama-derive")
    (version "0.11.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "askama-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0wbb5l1x1bx8x8vvz4ayw196l9y64mi3vrmxm7pn8wmlx3k8ggw7"))))))

(define-public rust-clap-derive ; 3.0.0-beta.2 doesn't build against new rust-heck
  (package
    (inherit (p rust-clap-derive-3))
    (name "rust-clap-derive")
    (version "3.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0g53w6qkqcc122bqh51jzfg51147il643idvq1czxkr2x5306ci5"))))))

(define rust-proptest-derive
  (package
    (inherit (p rust-proptest-derive-0.1))
    (name "rust-proptest-derive")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proptest-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p4x6k1zxq9lrpk46npdnzj6894mjx5bpwkwrdk63ird72an5d4h"))
       (modules '((guix build utils)))
       (patches (list (local-file "rust-proptest-derive-upgrade-to-stable-proc-macro-ecosyst.patch")))))))

(define-public rust-syslog ; @0.4 doesn't build against new rust-time
  (package
    (inherit (p rust-syslog-4))
    (name "rust-syslog")
    (version "6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1k0snk06c3gzq8g6kkqvpbbh5zg64nkzdjc303jda2hmd364904p"))))))

(define rust-ivf ; @0.1.0 doesn't build against new rust-bitstream-io@1
  (package
    (inherit (p rust-ivf-0.1))
    (name "rust-ivf")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ivf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1qmpqnwlcvp7xpi1f6l63icaafpsak6hv7s326snffhs6rj1rc0g"))))))

(define rust-actix-rt ; @1.1.1 doesn't build against new rust-tokio
  (package
    (inherit (p rust-actix-rt-1))
    (name "rust-actix-rt")
    (version "2.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-rt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "000hxsbaxgd8jdmnw4dnlff4xdhggprnw2lk67pmiscqa4lnr8by"))))))

(define rust-actix-server
  (package
    (inherit (p rust-actix-server-1)) ; @1 doesn't build against new rust-actix-rt
    (name "rust-actix-server")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-server" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "096q1hj88kgzfk6zv251sn57dlswh65r8ds6pdvv18cycn74z8qd"))))))

(define rust-actix-service ; new rust-actix-service doesn't build @1
  (package
    (inherit (p rust-actix-service-1))
    (name "rust-actix-service")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-service" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0fipjcc5kma7j47jfrw55qm09dakgvx617jbriydrkqqz10lk29v"))))))

(define rust-avif-serialize ; @0.6 doesn't build against new rust-arrayvec
  (package
    (inherit (@ (gnu packages crates-graphics) rust-avif-serialize-0.6))
    (name "rust-avif-serialize")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "avif-serialize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1fb9ld4iq8d5q5i9nr60hsdvdpjw4zb65kagv7xp08gphycwqy0f"))))))

(define rust-nasm-rs ; likewise
  (package
    (inherit (p rust-nasm-rs-0.2))
    (name "rust-nasm-rs")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nasm-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "10zl67i9gr7qarmnnnd8538mydw0yr6jlpbsvb5kxap9mr15h2ff"))))))

(define rust-miniz-oxide ; new rust-png incompatible with old rust-miniz-oxide (probably?)
  (package
    (inherit (p rust-miniz-oxide-0.4))
    (name "rust-miniz-oxide")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz-oxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10phz3ppw4p8pz4rwniy3qkw95wiq64kbvpb0l8kjcrzpka9pcnj"))))))

(define rust-deflate ; maybe required for new rust-png
  (package
    (inherit (p rust-deflate-0.9))
    (name "rust-deflate")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "deflate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bs319wa9wl7pn9j6jrrxg1gaqbak581rkx210cbix0qyljpwvy8"))))))

(define rust-png ; old version doesn't build against certain new crates
  (package
    (inherit (@ (gnu packages crates-graphics) rust-png-0.16))
    (name "rust-png")
    (version "0.17.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fp3vnaxmjdv71dcakc21k07ir5s31dlx1mrazfqddzgaynw0f6w"))))))

(define rust-lebe ; required by new rust-exr
  (package
    (name "rust-lebe")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lebe" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1zr6g3d35h349j0dsx6722lrjws00x2d8z0sy5p9wxdhimlivzby"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-development-inputs
        (("rust-bencher" ,(p rust-bencher-0.1))
         ("rust-byteorder" ,(p rust-byteorder-1)))))
    (home-page "https://github.com/johannesvollmer/lebe")
    (synopsis
      "Tiny, dead simple, high performance endianness conversions with a generic API")
    (description
      "Tiny, dead simple, high performance endianness conversions with a generic API")
    (license '(list license:bsd-3))))  ;; FIXME: Validate

(define rust-exr ; maybe required by new rust-image
  (package
    (name "rust-exr")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "exr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05f269cpxvnyiwvdbijilwdcmfn9zs1a7wxdpvbfarszzc30xk0l"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-bit-field" ,(p rust-bit-field-0.10))
         ("rust-deflate" ,rust-deflate)
         ("rust-flume" ,(p rust-flume-0.10))
         ("rust-half" ,(p rust-half-1))
         ("rust-inflate" ,(p rust-inflate-0.4))
         ("rust-lebe" ,rust-lebe)
         ("rust-smallvec" ,(p rust-smallvec-1))
         ("rust-threadpool" ,(p rust-threadpool-1)))
        #:cargo-development-inputs
        (("rust-bencher" ,(p rust-bencher-0.1))
         ;;("rust-image" ,(p rust-image-0.23))  ;; cycle
         ("rust-rand" ,(p rust-rand-0.8))
         ("rust-rayon" ,(p rust-rayon-1))
         ("rust-walkdir" ,(p rust-walkdir-2)))))
    (home-page "https://github.com/johannesvollmer/exrs")
    (synopsis "Read and write OpenEXR files without any unsafe code")
    (description "Read and write OpenEXR files without any unsafe code")
    (license '(list license:bsd-3))))

(define rust-jpeg-decoder ; required by new rust-tiff
  (package
    (inherit (@ (gnu packages crates-graphics) rust-jpeg-decoder-0.1))
    (name "rust-jpeg-decoder")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jpeg-decoder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fsv9hns5yfm900h8f02agxiazfdwn5rq43milcjhx9yyw8aly4l"))))))

(define rust-tiff ; required by new rust-image
  (package
    (inherit (@ (gnu packages crates-graphics) rust-tiff-0.6))
    (name "rust-tiff")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11i2smxqa35a921pqs3x5xl7kf3cav3fhqd4xiqafiplhq4xmykw"))))
    (arguments
      `(#:cargo-inputs
        (("rust-flate2" ,(p rust-flate2-1))
         ("rust-jpeg-decoder" ,rust-jpeg-decoder)
         ("rust-weezl" ,(p rust-weezl-0.1)))
        #:cargo-development-inputs
        (("rust-criterion" ,(p rust-criterion-0.3)))))))

(define rust-image ; old rust-image doesn't build
  (package
    (inherit (@ (gnu packages crates-graphics) rust-image-0.23))
    (name "rust-image")
    (version "0.24.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04hjf319s6hswfmy0llv3c0bfc6yidic0nij5r8f4sr5pkbxkv98"))))))

(define rust-pico-args
  (package
    (name "rust-pico-args")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pico-args" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0s646i0pbcck300rqldb21m151zxp66m3mdskha063blrfbcv2yv"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (home-page "https://github.com/RazrFalcon/pico-args")
    (synopsis "An ultra simple CLI arguments parser.")
    (description "An ultra simple CLI arguments parser.")
    (license '(list license:expat))))

(define rust-lalrpop-util
  (package
    (inherit (p rust-lalrpop-util-0.19))
    (name "rust-lalrpop-util")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lalrpop-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i1dbp489pjzsmxqhnsc47p37akkjbfawk2g861xkd79g34rdxxw"))))))

(define rust-lalrpop ; old lalrpop doesn't build
  (package
    (inherit (p rust-lalrpop-0.19))
    (name "rust-lalrpop")
    (version "0.19.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lalrpop" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08yqsp93vijrhskarrlxb16bbbyyakzhlm34z9vg460f3cs5a15k"))))))

(define rust-nettle-7 ; old rust-nettle doesn't build
  (package
    (inherit (p rust-nettle-7))
    (name "rust-nettle")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0da0d5kv2w93zjy7acsxvmg969ybi05bqibfs72nj0ri16l97lgm"))))))

(define rust-nettle-sys-2 ; for new rust-nettle
  (package
    (inherit (p rust-nettle-sys-2))
    (name "rust-nettle-sys")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nettle-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05rdwqzkv83wvrby2x1xwjn3gv2795cwxwycjqhs7qw3g1f6hfxi"))))
    (arguments
      `(#:cargo-inputs
        (("rust-bindgen" ,(p rust-bindgen-0.57))
         ("rust-cc" ,(p rust-cc-1))
         ("rust-libc" ,(p rust-libc-0.2))
         ("rust-pkg-config" ,(p rust-pkg-config-0.3))
         ("rust-tempfile" ,(p rust-tempfile-3))
         ("rust-vcpkg" ,(p rust-vcpkg-0.2)))))))

(define rust-headers ; @0.3.3 doesn't build against new rust-time
  (package
    (inherit (p rust-headers-0.3))
    (name "rust-headers")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "headers" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0pd5i8aywnmx9q7wfzn9bs0jq2fm5rmk0kdhcnmy1qcbg3jpizsc"))))))

(define rust-tungstenite ; @0.11 doesn't build
  (package
    (inherit (p rust-tungstenite-0.11))
    (name "rust-tungstenite")
    (version "0.17.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tungstenite" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1x848392ihy5mh098sns0lcmb5rdwkxpmdcfya108mz783m2ssnr"))))))

;; Some of these are only used for tests, cause cycles, ???,
;; so remove them.  (TODO: some of them can probably now be removed.)
;; TODO: write a "guix style" thing for doing this.
(define %removed-dependencies
  '("rust-blakeout" ; doesn't build and no new version available, let's avoid for now.
    "rust-derive-error-chain" ; doesn't build (even when updated) and unmaintained, avoid it for now
    "rust-crypto-tests" ; test dependency doesn't build against new rust-digest, avoid for now
    "rust-quickcheck" ; (quickcheck env-logger humantime chrono bincode) cycle
    "rust-pear" "rust-pear-codegen" ; current version in Guix requires non-stable
    "rust-mesalink" ; doesn't build against recent rust-rustls
    "rust-defmt" ; accidentally requires unstable-test?
    "rust-heapsize-plugin" ; makes use of removed features
    "rust-rustc-test" ; doesn't build against recent rust-time
    "rust-mio-uds" ; doesn't build against new rust-mio, now included in new rust-mio
    "rust-speculate" ; @0.1.2 doesn't build against recent rust-syn
    "rust-skeptic" ; @0.13.4 doesn't build
    "rust-boxxy" ; doesn't build and not supposed to be used ‘in production’
    "rust-macrotest"
    "rust-tokio-tls" ; @0.3.1 doesn't build
    "rust-rust-hawktracer-sys" ; only for tracing (debugging-only), so maybe the build failure can be avoided?
    "rust-ntest" "rust-ntest-test-cases" ; test-only, and @0.3.4 tries using non-exported syn::export
    "rust-afl" ; TODO: move to 'native-inputs'/development-inputs
    "rust-js-sys" ; TODO: guix doesn't support those targets (yet)
    "rust-cortex-m" ; ARM targets not yet supported for Rust in Guix
    ;;"rust-cc" ;; todo: build.rs, hence move to 'native-inputs'?
    "rust-stdweb" "rust-web-sys" ;; web, js, wasm?
    "rust-gloo-timers" ; web-only crate
    "rust-bencher" ; FTB
    "rust-criterion" "rust-criterion-cycles-per-byte" ;; fails to build because rust-async-log-attributes fails to build
    "rust-femme" ; some dependencies fail to build
    "rust-proptest" "rust-proptest-derive"
    "rust-futures-util-preview" ; futures-util has been updated?
    "rust-iron" ; its dependency rust-hyper-native-tls fails to build
    "rust-rocket" ; its dependency rust-hyper-sync-rustls fails to build
    "rust-nickel" ; fails to build
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
    "rust-winapi" "rust-kernel32-sys" "rust-winreg" "rust-wepoll-sys" "rust-wepoll-sys-stjepang" "rust-ipconfig" ; skip Windows support for now
    "rust-nodrop-union" ; required unstable, and deprecated
    "rust-sleef-sys" ; requires unstable
    "rust-packed-simd" "rust-packed-simd-2" ; requires unstable (TODO: rust-packed-simd-2?)
    "rust-security-framework" "rust-cocoa" "rust-cocoa-foundation" "rust-core-foundation" "rust-core-foundation-sys" "rust-core-text" "rust-fsevent" "rust-fsevent-sys" "rust-core-video-sys" "rust-core-graphics" "rust-core-graphics-types" "rust-objc-foundation" "rust-security-framework-sys" ; non-Linux, non-Hurd things,
    "rust-ndk" "rust-ndk-glue" "rust-ndk-sys" "rust-ndk-macro" "rust-android-logger" "rust-android-glue" "rust-android-log-sys" ; Android userspace is not yet supported
    "rust-redox-syscall" "rust-redox-termios" "rust-redox-users" ; Redox is not a supported target of Guix
    "rust-mach" ; skip Mach (used by Hurd and others) support for now.
    "rust-ws2-32-sys"
    "rust-winapi-util" "rust-winapi-build"
    "rust-fuchsia-zircon" "rust-fuchsia-zircon-sys" "rust-fuchsia-cprng" ; fuchsia not supported by Guix
    "rust-dwrote" ; Windows-only, skip for now, cross-compilation can be implemented later
    "rust-core-arch" ; doesn't build, nowadays part of Rust itself?
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
    "rust-cargo" "rust-cargo-c" ; rust-cargo doesn't build even when updated to @0.62
    "rust-tokio-mock-task" ; doesn't build
    "rust-tokio-test"
    "rust-rand-xorshift"
    "rust-serde-test"
    "rust-actix-testing" ; doesn't build
    "rust-wasm-bindgen" "rust-wasi"
    "rust-wasm-bindgen-futures" ; ECMAScript-only and doesn't build
    "rust-wasm-bindgen-test"))

;; Like %removed-dependencies, but only remove the dependency right of the '->'
;; when used by the dependent left of the '<-'.
(define %removed-dependencies->
  ;; Maybe a test or example cycle?
  '(("rust-bytemuck-derive" -> "rust-bytemuck")
    ("rust-nasm-rs" -> "rust-arrayvec") ; not required anymore due to package update
    ("rust-diesel-derives" -> "rust-diesel")
    ("rust-colored" -> "rust-rspec")
    ;; Not a dependency anymore, resolve cycle.
    ("rust-pkcs8" -> "rust-pkcs1")
    ;; Break cycle (test or something like that?)
    ("rust-quote" -> "rust-rustversion")
    ("rust-multipart" -> "rust-hyper") ; incompatible with hyper>0.10
    ;; Break cycle.
    ("rust-async-attributes" -> "rust-async-std")
    ("rust-async-channel" -> "rust-blocking")
    ("rust-async-io" -> "rust-async-net")
    ;; Optional dependency cycle
    ("rust-async-std" -> "rust-surf")
    ;; Optional dependency cycle
    ("rust-ravif" -> "rust-image")
    ("rav1e" -> "rust-image")
    ; ravif not used by default image features
    ("rust-image" -> "rust-ravif")
    ;; TODO: rust-rav1e and rav1e
    ;; rust-futures-cpupool isn't updated anymore and doesn't
    ;; build anymore?
    ("rust-serde-derive" -> "rust-serde")
    ;; (Test?) cycle
    ("rust-actix-web" -> "rust-actix-web-codegen")
    ("rust-actix-macros" -> "rust-actix-rt")
    ;; Test cycle (rust-paw <-> rust-paw-structopt).
    ("rust-paw" -> "rust-paw-structopt")
    ("rust-paw" -> "rust-structopt")
    ("rust-anyhow" -> "rust-thiserror")
    ("rust-proc-macro-hack" -> "rust-demo-hack")
    ("rust-proc-macro-hack" -> "rust-demo-hack-impl")
    ("rust-nom" -> "rust-jemallocator")
    ("rust-serde-bytes" -> "rust-bincode")
    ("rust-failure-derive" -> "rust-failure")
    ("rust-serde-bytes" -> "rust-bincode")
    ("rust-serde-json" -> "rust-serde-stacker")
    ("rust-erased-serde" -> "rust-serde-json")
    ("rust-proc-macro2" -> "rust-quote")
    ("rust-indexmap" -> "rust-itertools")
    ("rust-tracing-attributes" -> "rust-tracing")
    ("rust-tracing-attributes" -> "rust-async-trait")
    ("rust-tracing-attributes" -> "rust-tracing-futures")
    ("rust-tracing" -> "rust-tokio")
    ("rust-headers" -> "rust-time") ; dependency removed in new version
    ("rust-hashbrown" -> "rust-bumpalo") ; todo: remove from #:cargo-inputs?, unused?
    ("rust-fastrand" -> "rust-getrandom")
    ("rust-fastrand" -> "rust-instant")
    ("rust-fastrand" -> "rust-wyhash")
    ;; ("rust-tokio-io" -> "rust-tokio-current-thread") ; rust-tokio-io currently removed
    ;; ("rust-tokio-core" -> "rust-flate2") ; likewise
    ;; ("rust-tokio-core" -> "rust-httparse")
    ;; ("rust-tokio-process" -> "rust-failure") ;; otherwise cc needs to be removed from rust-cloudflare-zlib-sys ;; likewise, rust-tokio-process is currently removed
    ;; ("rust-tokio" -> "rust-tokio-executor") ; similar
    ;; Remove unused dependencies
    ("rust-flate2" -> "rust-cloudflare-zlib-sys")
    ("rust-flate2" -> "rust-miniz-sys")
    ("rust-flate2" -> "rust-miniz-oxide")
    ("rust-tokio" -> "rust-httparse")
    ("rust-tokio" -> "rust-async-stream") ;; test
    ("rust-tokio" -> "rust-nix") ;; test
    ("rust-tokio" -> "rust-tokio-current-thread")
    ("rust-tokio" -> "rust-tokio-fs")
    ("rust-tokio" -> "rust-tokio-reactor")
    ("rust-tokio" -> "rust-tokio-sync")
    ("rust-tokio" -> "rust-tokio-stream")
    ("rust-tokio-sync" -> "rust-loom")
    ("rust-tokio" -> "rust-tokio-tcp")
    ("rust-tokio" -> "rust-tokio-timer")
    ("rust-tokio" -> "rust-tokio-threadpool")
    ("rust-tokio" -> "rust-tokio-udp")
    ("rust-tokio" -> "rust-tokio-uds")
    ("rust-tokio-macros" -> "rust-tokio")
    ("rust-parking-lot-core" -> "rust-backtrace")
    ;; See %features
    ("rust-parking-lot-core" -> "rust-petgraph")
    ;; TODO: can be removed by relaxing versions in rust-signal-hook@0.1
    ("rust-signal-hook-registry" -> "rust-signal-hook")
    ;; TODO why benchmark?
    ("rust-unicode-bidi" -> "rust-flame")
    ("rust-unicode-bidi" -> "rust-flamer")
    ("rust-odds" -> "rust-lazy-static")
    ;; TODO
    #;("rust-boxxy" -> "rust-ctrlc") ; TODO currently useless because boxxy is in %removed-dependencies, revisit when tests are supported
    ("rust-flate2" -> "rust-tokio-tcp")
    ("rust-flate2" -> "rust-tokio-threadpool")
    ("rust-tokio" -> "rust-flate2") ;; TODO remove old tokios
    ("rust-semver" -> "rust-crates-index") ;; TODO why????
    ("rust-semver-parser" -> "rust-pest-generator")
    ("rust-spmc" -> "rust-loom")
    ;; ("rust-tokio-test" -> "rust-tokio") ; currently rust-tokio-test is removed
    ;; Break dev-dependencies cycle
    ("rust-regex-automata" -> "rust-bstr")))

;; Try keeping things sorted, to avoid rebase/merge conflicts.
(define %features
  ;; rust-swayipcs requires 'spawn_blocking' which is only
  ;; public if "unstable" is enabled.
  `(("rust-async-std" ,#~'("default" "unstable"))
    ;; The "dox" feature requires non-stable.
    ("rust-atk" ,#~'("v2_34"))
    ("rust-atk-sys" ,#~'("v2_34"))
    ;; Do _not_ include 'runtime', as dlopen isn't used,
    ;; linking happens at compile time (and at program
    ;; startup).
    ("rust-bindgen" ,#~'("logging" "clap" "which-rustfmt"))
    ;; The feature "alloc" is not set by default, causing the
    ;; build to fail (TODO: maybe report upstream?)
    ("rust-bitvec"
     ,#~'("std" "atomic" "alloc"))
    ;; the default "generic-simd" feature required rust-packed-simd
    ;; which is currently uncompilable.
    ("rust-bytecount" ,#~'())
    ("rust-bzip2" ,#~'("futures")) ; "tokio" requires old tokio-io
    ;; "alloc" is required by some crates.
    ("rust-chrono" ,#~'("default" "alloc"))
    ;; zeroize required by rust-ctr
    ("rust-cipher" ,#~'("alloc" "std" "block-padding" "rand_core" "dev" "zeroize"))
    ;; Don't just support libclang 3.5, also include
    ;; bindings for later versions which rust-bindgen might
    ;; need.  Don't include the "runtime" feature, because
    ;; then libclang.so needs to be in LD_LIBRARY_PATH or such
    ;; to be found.  Don't include the "static" feature for
    ;; the standard reasons against static linking in Guix.
    ("rust-clang-sys" ,#~'("clang_10_0")) ; Guix by default does dynamic linking, not static linking, which would use the "static" feature IIUC
    ;; This addresses the build failure
    ;; ‘could not find `collector` in the crate root’
    ;; and ‘cannot find function `pin` in crate `epoch`’
    ("rust-crossbeam-epoch"
     ,#~'("std" "alloc"))
    ;; rust-cipher requires non-default rand_core
    ("rust-crypto-common" ,#~'("std" "rand_core"))
    ;; Don't accidentally enable multiple encoding features, even
    ;; though rust-fmt only supports one at the time.  An encoding
    ;; will automatically be chosen.
    ("rust-defmt" ,#~'("alloc"))
    ;; rust-pkcs1 requires "pem"
    ("rust-der" ,#~'("std" "alloc" "oid" "pem"))
    ;; rust-x509-parser requires bigint
    ("rust-der-parser" ,#~'("default" "bigint"))
    ;; Required by hmac.
    ("rust-digest" ,#~'("default" "std" "mac"))
    ("rust-doc-comment" ,#~'()) ; no_core requires unstable, and old_macros is detected by build.rs
    ("rust-ena" ,#~'())  ;; disable "bench", which fails for stable build
    ("rust-encoding-rs" ,#~'()) ; "simd-accel" requires unstable "packed_simd"
    ("rust-fern" ,#~'("syslog-6")) ; avoid having to include multiple versions of syslog
    ;; Avoid extra dependencies by using the C library that
    ;; is used elsewhere anyway.
    ("rust-flate2" ,#~'("zlib"))
    ("rust-futures-core"
     ,#~'("std" "alloc"))
    ("rust-futures-channel"
     ,#~'("std" "alloc"))
    ;; Enable some features such that "rust-futures" actually builds.
    ("rust-futures-task"
     ,#~'("std" "alloc"))
    ("rust-futures-util"
     ,#~'("std" "alloc" "sink"
	  "io" "async-await"
	  "async-await-macro"
	  "channel"))
    ;; The default "benchmarks" feature requires unstable.
    ("rust-galil-seiferas" ,#~'())
    ("rust-gdk-pixbuf" ,#~'("v2_40")) ; "dox" requires non-stable
    ("rust-gdk-pixbuf-sys" ,#~'("v2_40")) ; likewise (for dox)
    ("rust-gdk-sys" ,#~'("v3_24")) ; likewise (for dox) (look in the .pc for the version)
    ("rust-gdk" ,#~'("v3_24")) ; likewise (for dox) (look in the .pc for the version)
    ;; Required by rust-rand-core.
    ("rust-getrandom" ,#~'("std"))
    ("rust-gio" ,#~'("v2_66")) ; likewise
    ("rust-gio-sys" ,#~'("v2_66")) ; likewise
    ;; serde1 failure requires undeclared ‘Glob’ dependency
    ("rust-globset" ,#~'())
    ;; The "dox" feature requires non-stable.
    ("rust-glib" ,#~'("log" "log_macros" "v2_68")) ; likewise
    ("rust-glib-sys" ,#~'("v2_68"))
    ("rust-gobject-sys" ,#~'("v2_68")) ; likewise
    ("rust-gtk" ,#~'("v3_24_9")) ; likewise (for dox)
    ("rust-gtk-sys" ,#~'("v3_24_11")) ; likewise (for dox)
    ;; Avoid "use-intrisics", which requires unstable.
    ("rust-half" ,#~'("alloc" "serialize" "std"))
    ;; TODO: move into Guix proper?
    ("rust-hashbrown" ,#~'("default" "raw")) ; default "ahash" is required by rust-lru@0.7
    ("rust-hyper" ,#~'("full"))
    ;; TODO: investigate build_dictionaries, and maybe not embedding libraries.
    ;; TODO: cannot choose multiple normalization forms, is this important?
    ("rust-hyphenation" ,#~'("embed_all"))
    ("rust-im-rc" ,#~'()) ; "pool" feature doesn't build and "debug" probably makess things less efficient
    ;; Require rust-cipher.
    ("rust-inout" ,#~'("std" "block-padding"))
    ("rust-itoa" ,#~'("std"))
    ;; "pattern" and "benchmarks" require non-stable (rust-jetscii@0.5)
    ("rust-jetscii" ,#~'())
    ;; Avoid removed feature(custom_derive)
    ("rust-language-tags" ,#~'())
    ("rust-lazycell" ,#~'()) ;; avoid nightly things
    ;; extra-traits is required by rust-nix
    ("rust-libc" ,#~'("std" "extra_traits"))
    ;; Required by rust-env-logger.
    ;; kv_unstable is required by rust-kv-log-macro.
    ("rust-log" ,#~'("std" "kv_unstable"))
    ("rust-lzma-sys" ,#~'()) ; don't enable "static" (TODO: add it to the list in antioxidant?)
    ;; Required by rust-tokio
    ;; TODO remove os-poll, as implied features are implemented.
    ("rust-mio"
     ,#~'("net" "os-ext" "os-poll"))
    ("rust-multipart"
     ;; default "iron" feature requires rust-iron which currently fails to build.
     ;; Likewise for "nickel".
     ;; The "hyper" feature is incompatible with rust-hyper>0.11
     ,#~'("client" "mock" "server" "tiny_http"))
    ;; The non-default feature "alloc" is required by rust-pure-rust-locales.
    ("rust-nom"
     ,#~'("std" "lexical" "alloc"))
    ("rust-numtoa" ,#~'("std"))
    ;; rust-rsa requires "prime" and "zeroize"
    ("rust-num-bigint-dig" ,#~'("default" "prime" "zeroize"))
    ;; rust-x509-parser required 'crypto' and 'x509'
    ("rust-oid-registry" ,#~'("default" "crypto" "x509"))
    ("rust-openssl-sys" ,#~'()) ;; avoid the 'vendored' feature
    ("rust-os-str-bytes" ,#~'("raw"))
    ("rust-pango" ,#~'("v1_46")) ; "dox" feature requires non-stable
    ("rust-pango-sys" ,#~'("v1_46")) ; likewise
    ;; The 'backtrace' and 'petgraph' dependency has been removed.
    ;; (including petgraph causes a cycle between rust-ahash and rust-hashbrown,
    ;; but it's ‘only’ required for deadlock detection).
    ("rust-parking-lot-core" ,#~'())
    ;; "quickcheck" features requires removed crate "quickcheck"
    ("rust-partial-io" ,#~'("futures03" "tokio1"))
    ;; Required by 'sniffglue'
    ("rust-pktparse" ,#~'("serde"))
    ("rust-plotters-svg" ,#~'()) ; "debug" feature causes a build failure
    ;; Required by rust-unicode-normalization
    ("rust-proc-macro2"
     ;; Required by rust-serde-bytes via rust-syn.  If
     ;; absent, this causes errors like
     ;; <<https://github.com/google/cargo-raze/issues/159>.
     ,#~'("proc-macro"))
    ;; Without "getrandom" or "alloc", it fails to build (TODO upstream?).
    ;; small_rngs is required by rust-phf-generator.
    ("rust-rand"
     ,#~'("std" "std_rng" "getrandom"
	  "alloc" "small_rng"))
    ("rust-reqwest" ,#~'("default" "blocking")) ; tealdeer@1.4.1 requires "blocking" to build
    ;; The 'inline-asm' feature requires non-stable
    ("rust-riscv" ,#~'())
    ;; Some features required rust-rand when using the getrandom feature,
    ;; serde for rust-rand-isaac@0.3.0 ... (now building with all features)
    ;; ("rust-rand-core" #~'("std" "getrandom"))
    ("rust-rust-hawktracer-normal-macro" ,#~'()) ; for now, don't enable the profiling feature which requires a currently non-building package rust-hawktracer-sys (which also bundles things!)
    ("rust-rust-hawktracer-proc-macro" ,#~'()) ; likewise!
    ;; asm! syntax not supported anymore, and "capture"
    ;; requires non-existent io::set_panic
    ("rust-rustc-test" ,#~'())
    ;; rust-cargo-metadata requires the serialisation
    ;; / deserialisation traits.
    ("rust-semver" ,#~'("default" "serde"))
    ;; 'derive' is needed by rust-ron
    ("rust-serde" ,#~'("std" "alloc" "derive"))
    ("rust-servo-fontconfig-sys" ,#~'("force_system_lib")) ; be extra sure the bundled copy isn't used
    ;; Avoid "digest_trait" which requires old rust-digest@0.9.0
    ("rust-sha1collisiondetection" ,#~'("std" "structopt"))
    ("rust-similar" ,#~'("default" "text" "inline"))
    ;; "nested-values" is required by the "nested-values" feature of rust-slog-term
    ("rust-slog" ,#~'("default" "nested-values"))
    ("rust-smallvec" ,#~'()) ; default features require non-stable
    ;; Default serde1_lib requires unpackaged rust-serde1-lib
    ("rust-sval" ,#~'("alloc" "arbitrary-depth" "derive" "fmt" "std"))
    ;; "paw" required by sniffglue
    ("rust-structopt" ,#~'("default" "paw"))
    ;; TODO: use default features from Cargo.toml
    ;; rust-serde-bytes requires the 'parsing' feature.
    ;; visit is required by rust-synstructure.
    ;; visit-mut is used by rust-tracing-attributes.
    ("rust-syn"
     ,#~'("derive" "parsing" "printing"
	  "clone-impls"
	  "proc-macro" "full"
	  "visit" "visit-mut"
	  "fold" ; used by rust-diesel-derives
	  ;; Used by rust-strum-macros
	  "extra-traits"))
    ("rust-tinyvec" ,#~'("alloc"))
    ("rust-tiny-keccak"
     ;; By default nothing is build, which seems rather useless.
     ;; Let's enable everything.
     ,#~'("cshake" "fips202" "k12" "keccak" "kmac" "parallel_hash" "sha3" "shake" "sp800" "tuple_hash"))
    ;; rust-cookie requires the non-default "parsing" and "macros" feature. Might as well enable
    ;; "formatting" as well.
    ("rust-time" ,#~'("default" "macros" "formatting" "parsing" "local-offset"))
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
    ;; early-data is required by rust-trust-dns-proto
    ("rust-tokio-rustls" ,#~'("default" "early-data"))
    ;; dns-over-openssl is required by rust-trust-dns-openssl.
    ;; dns-over-native-tls is required by rust-trust-dns-native-tls.
    ;; dns-over-rustls is required by rust-trust-dns-rustls.
    ;; dns-over-https is required by rust-trust-dns-https.
    ("rust-trust-dns-proto"
     ,#~'("default" "dns-over-openssl" "dns-over-native-tls" "dns-over-rustls"
	  "dns-over-https"))
    ;; For now avoid optional dependencies
    ("rust-typenum" ,#~'())
    ("rust-value-bag" ,#~'("std"))
    ("rust-v-frame" ,#~'("serialize")) ; wasm doesn't build, tracing seems unnecessary
    ("rust-webpki" ,#~'("std" "alloc"))
    ("rust-xz2" ,#~'("futures")) ; ???
    ;; rust-rcgen requires "time". While at it, enable other
    ;; features as well.
    ("rust-yasna" ,#~'("default" "time" "bit-vec" "bigint" "std"))
    ;; rust-num-bigint-dig's zeroize feature requires the "derive"
    ;; feature of rust-zeroize
    ("rust-zeroize" ,#~'("default" "derive"))
    ("rust-zstd-safe" ,#~'("default" "std")))) ; std is reaquired by rust-zstd@0.9.0

(define %replacements
  `(("rust-atk-sys" ,(@ (gnu packages crates-gtk) rust-atk-sys-0.14)) ; @0.10 doesn't build
    ("rust-gtk-sys" ,(@ (gnu packages crates-gtk) rust-gtk-sys-0.14)) ; @0.10 doesn't build
    ("rust-getrandom" ,(p rust-getrandom-0.2)) ; avoid multiple versions
    ("rust-rand-core" ,(p rust-rand-core-0.6)) ; avoid multiple versions
    ("rust-blake2" ,rust-blake2)
    ("rust-actix-codec" ,rust-actix-codec)
    ("rust-actix-utils" ,rust-actix-utils)
    ("rust-actix-service" ,rust-actix-service)
    ("rust-chacha20poly1305" ,rust-chacha20poly1305)
    ("rust-miniz-oxide" ,(p rust-miniz-oxide-0.4)) ; avoid multiple versions
    ("rust-arrayvec" ,(p rust-arrayvec-0.7)) ; avoid multiple versions
    ("rust-bitstream-io" ,(p rust-bitstream-io-1)) ; avoid multiple versions
    ("rust-avif-serialize" ,rust-avif-serialize)
    ("rust-nasm-rs" ,rust-nasm-rs)
    ("rust-ivf" ,rust-ivf)
    ("rust-syslog" ,rust-syslog)
    ("rust-clap-derive" ,rust-clap-derive)
    ("rust-askama-shared" ,rust-askama-shared)
    ("rust-askama-derive" ,rust-askama-derive)
    ("rust-zstd" ,(p rust-zstd-0.9)) ; @0.6 doesn't build a dependency failing to build
    ("rust-reqwest" ,(p rust-reqwest-0.11)) ; @0.10 has
    ("rust-cookie-store" ,rust-cookie-store) ; fix failing build by updating
    ("rust-cookie-store-15" ,rust-cookie-store)
    ("rust-structopt" ,(p rust-structopt-0.3))
    ("rust-structopt-derive" ,(p rust-structopt-derive-0.4)) ; @0.2.18 doesn't build
    ("rust-trust-dns-proto" ,rust-trust-dns-proto)
    ("rust-trust-dns-openssl" ,rust-trust-dns-openssl)
    ("rust-trust-dns-native-tls" ,rust-trust-dns-native-tls)
    ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
    ("rust-trust-dns-https" ,rust-trust-dns-https)
    ("rust-trust-dns-resolver" ,rust-trust-dns-resolver)
    ("rust-tungstenite" ,rust-tungstenite)
    ("rust-pulse" ,(package-with-extra-patches
		    (p rust-pulse-0.5)
		    ;; For compatibility with new rust-time
		    (list (local-file "0001-use-std-time-bump-to-0.5.4.patch"))))
    ("rust-partial-io" ,rust-partial-io)
    ;; swayhide requires non-async to build
    ("rust-swayipc" ,(package-with-rust-features (p rust-swayipc-2)
						 #~'()
						 #:name "rust-swayipc+sync"
						 #:rust-metadata "guix-variant=sync")
     #:for-dependent
     ,(lambda (dependent)
	(string=? "swayhide" (package-name dependent))))
    ("rust-mio" ,rust-mio)
    ("rust-smol" ,(p rust-smol-1)) ; @0.1 or its dependencies don't build
    ("rust-actix-rt" ,rust-actix-rt)
    ("rust-actix-tls" ,rust-actix-tls)
    ("rust-async-process" ,rust-async-process) ; @1.0.1 doesn't build against new rust-signal-hookx
    ("rust-blocking" ,(p rust-blocking-1)) ; @0.4 doesn't build
    ("rust-inotify" ,(p rust-inotify-0.9)) ; @0.8 doesn't build
    ("rust-futures-intrusive" ,rust-futures-intrusive)
    ("rust-multipart" ,(p rust-multipart-0.18)) ; @0.17 doesn't build
    ("rust-memoffset" ,(p rust-memoffset-0.6)) ; @0.5 doesn't build
    ("rust-signal-hook" ,(p rust-signal-hook-0.3)) ; @0.1 doesn't build
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
    ("rust-tokio-native-tls" ,(p rust-tokio-native-tls-0.3)) ; @0.1 doesn't build
    ("rust-tokio" ,rust-tokio) ; rust-tokio@1 in Guix doesn't build against new rust-mio
    ("rust-bindgen"
     ;; In the old version 'runtime' cannot be
     ;; disabled.
     ,(p rust-bindgen-0.59))
    ("rust-headers" ,rust-headers)
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
    ("rust-actix-server" ,rust-actix-server)
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
    ; ("rust-boxxy" ,rust-boxxy) ; TODO: currently useless because in %removed-dependencies, revisit when tests are supported
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
    ;; ("rust-tokio-io" ,rust-tokio-io-0.2) ; tokio-io currently removed
    ; ("rust-tokio-codec" ,rust-tokio-io-0.2) ; looks like an error in retrospect
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
    ("rust-crossbeam-utils" ; avoid errors by multiple versions of the same crate
     ,(p rust-crossbeam-utils-0.8))
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
    ("rust-proptest-derive" ,rust-proptest-derive)
    ("rust-arc-swap" ,(p rust-arc-swap-1))
    ("rust-gif" ,(p rust-gif-0.11)) ;; @0.11.2 - crates-graphics @0.11.3 doesn't build ATM
    ("rust-miniz-oxide" ,rust-miniz-oxide)
    ("rust-deflate" ,rust-deflate)
    ("rust-png" ,rust-png)
    ("rust-tiff" ,rust-tiff)
    ("rust-jpeg-decoder" ,rust-jpeg-decoder)
    ("rust-image" ,rust-image)
    ("rust-lebe" ,rust-lebe)
    ("rust-exr" ,rust-exr)
    ("rust-lalrpop" ,rust-lalrpop)
    ("rust-lalrpop-util" ,rust-lalrpop-util)
    ("rust-nettle-sys" ,rust-nettle-sys-2)
    ("rust-nettle" ,rust-nettle-7)
    ;; 0.4.30 fails to build.
    ("rust-proc-macro2" ,(p rust-proc-macro2-1))
    ("rust-log" ,(p rust-log-0.4))))

;; TODO: add these (upstream) or teach "guix style" to add them
(define %extra-inputs
  `(("rust-structopt" ; for paw feature
     (("rust-paw" ,(p rust-paw-1))))
    ("rust-aom-sys"
     (("rust-system-deps" ,(p rust-system-deps-3)))) ; missing input (TODO: native-input)
    ("rust-servo-fontconfig-sys"
     (("fontconfig" ,(@ (gnu packages fontutils) fontconfig))))
    ("rust-swayipc"
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-failure" ,(p rust-failure-0.1))))
    ("rust-syslog"
     (("rust-hostname" ,(p rust-hostname-0.3)))) ; new dependency of new version of rust-syslog
    ("rust-swayipc+sync"
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-failure" ,(p rust-failure-0.1))))
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
    ("rust-actix-codec" ; new inputs of new version
     (("rust-memchr" ,(p rust-memchr-2))
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))))
    ("rust-actix-rt" ;new dependencies for new version
     (("rust-futures-core" ,(p rust-futures-core-0.3))))
    ("rust-actix-utils" ;new dependencies for new version
     (("rust-local-waker" ,rust-local-waker)
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))))
    ("rust-actix-server" ;new dependencies for new version
     (("rust-mio" ,rust-mio)
      ("rust-tokio" ,rust-tokio)
      ("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-futures-util" ,rust-futures-util-0.3)
      ("rust-tracing" ,(p rust-tracing-0.1))))
    ("rust-actix-service" ;likewise
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))
      ("rust-paste" ,(p rust-paste-1))))
    ("rust-actix-tls" ;likewise
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-actix-rt" ,rust-actix-rt)
      ("rust-log" ,(p rust-log-0.4))
      ("rust-tokio-util" ,(p rust-tokio-util-0.3))))
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
    ("rust-image" (("rust-exr" ,rust-exr)))
    ;; possibly only required by new version
    #;("rust-boxxy" (("rust-anyhow" ,(@ (gnu packages crates-io) rust-anyhow-1)))) ; TODO: currently useless because in %removed-dependencies, revisit when tests are supported
    ("rust-petgraph" (("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
    ("sniffglue" (("rust-bstr" ,(@ (gnu packages crates-io) rust-bstr-0.2))))
    ("rust-lalrpop" (("rust-tiny-keccak" ,(p rust-tiny-keccak-2))
                     ("rust-pico-args" ,rust-pico-args)))
    ;; TODO: is this sufficient?
    ("rust-futures-core-preview"
     (("rust-futures-core" ,rust-futures-core-0.3)))
    ("rust-http-body" ; at least for 0.4
     (("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))))
    ("rust-headers"
     (("rust-httpdate" ,(p rust-httpdate-1)))) ; new dependency
    ("rust-tungstenite"
     (("rust-thiserror" ,(p rust-thiserror-1))))
    ("rust-tokio" ; new dependency for new version
     (("rust-socket2" ,(p rust-socket2-0.4))))
    ("rust-tokio-sync"
     ;; TODO: remove 'preview' dependencies?
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-futures-sink" ,rust-futures-sink-0.3)
      ("rust-futures-util" ,rust-futures-util-0.3)))
    ("rust-tokio-util"
     (("rust-tracing" ,(p rust-tracing-0.1)))))) ; missing dependency

(define (find-replacement dependent dependency)
  (define test-replacement
    (match-lambda
      ((key new) (and (equal? key (package-name dependency)) new))
      ((key new #:for-dependent dependent-match?)
       (and (equal? key (package-name dependency))
	    (dependent-match? dependent)
	    new))
      (stuff (pk 'oops stuff)
	     (error "bogus entry in %replacments"))))
  (any test-replacement %replacements))

;; todo: ‘stub‘ rust-rustc-version to reduce deps?
;; grrr rust-backtrace
(define (vitaminate/auto* pack)
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 (rust-metadata "")
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
		   (not (member (list (package-name pack)
				      '->
				      (package-name dependency))
				%removed-dependencies->))
		   ;; These are actually test inputs! (TODO guix)
		   ;; (TODO: this isn't build from source)
		   ;;(not (equal? (package-name pack) "rust-pure-rust-locales"))
		   #;(pk 'p pack dependency)
		   (cons* label (vitaminate/auto
				 ;; Resolve version conflicts, choose newer versions,
				 ;; etc.
				 (or (find-replacement pack dependency) dependency))
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
	  (arguments (list #:rust-metadata rust-metadata
			   #:features
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

;; Self-checks
(define (check-removed-extra-inputs)
  "Verify that %extra-inputs is not in contradiction with
%removed-dependencies or %removed-dependencies->"
  ;; List packages in %removed-dependencies which have an entry in
  ;; %extra-inputs defined (useless, unless it's a leaf package)
  (define (check-removed-dependency name)
    (when (assoc name %extra-inputs)
      (pk name "in %removed-dependencies and %extra-inputs (probably useless)")
      (throw 'oops)))
  (define (check-removed-dependency-> entry)
    (match entry
      (((? string? left) '-> (? string? right))
       (when (member right %removed-dependencies)
	 (pk right "after %removed-dependencies-> and in %removed-dependencies (useless)")
	 (throw 'oops))
       (when (member left %removed-dependencies)
	 (pk left "left in %removed-dependencies-> and in %removed-dependencies (redundant)")
	 (throw 'oops)))))
  ;; list names listed as an extra-input for some package and also in
  ;; %removed-dependencies (confusing, because they would have to be both
  ;; added and removed).
  (define check-extra-input
    (match-lambda
      (((? string? name) (? list? dependencies))
       (let ((check-dependency
	      (match-lambda ((dependency-name _)
			     (when (member dependency-name %removed-dependencies)
			       (pk "extra-input " dependency-name " of " name "in %removed-dependencies (contradictory)")
			       (throw 'oops))))))
	 (for-each check-dependency dependencies)))))
  (for-each check-removed-dependency %removed-dependencies)
  (for-each check-removed-dependency-> %removed-dependencies->)
  (for-each check-extra-input %extra-inputs))
(check-removed-extra-inputs)


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
(define-public antioxidated-alfis
  (public-test-package (vitaminate/auto alfis)))
(define-public antioxidated-castor
  (public-test-package (vitaminate/auto castor)))
(define-public antioxidated-diffr
  (public-test-package (vitaminate/auto (@ (gnu packages rust-apps) diffr))))
(define-public antioxidated-dutree
  (public-test-package (vitaminate/auto dutree)))
(define-public antioxidated-git-absorb
  (public-test-package (vitaminate/auto git-absorb)))
(define-public antioxidated-hex
  (public-test-package (vitaminate/auto hex)))
(define-public antioxidated-hexyl
  (public-test-package (vitaminate/auto (@ (gnu packages rust-apps) hexyl))))
(define-public antioxidated-ripgrep
  (public-test-package (vitaminate/auto ripgrep)))
(define-public antioxidated-rtss
  (public-test-package (vitaminate/auto rtss)))
(define-public antioxidated-sniffglue
  (public-test-package (vitaminate/auto sniffglue)))
(define-public antioxidated-swayhide
  (public-test-package (vitaminate/auto swayhide)))
(define-public antioxidated-tealdeer
  (public-test-package (vitaminate/auto tealdeer)))

;; For local development
(list antioxidated-rust-bindgen
      antioxidated-agate
      antioxidated-castor
      antioxidated-dutree
      antioxidated-diffr
      antioxidated-git-absorb
      antioxidated-hex
      antioxidated-hexyl
      antioxidated-ripgrep
      antioxidated-rtss
      antioxidated-sniffglue
      antioxidated-swayhide
      antioxidated-tealdeer)

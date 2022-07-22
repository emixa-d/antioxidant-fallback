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
(define-module (antioxidant-packages)
  #:export (vitaminate/auto public-test-package
			    %rust-library-outputs))

(use-modules (guix packages) (guix build-system) (guix gexp) (guix utils) (guix modules)
	     ((guix build-system gnu) #:select (%gnu-build-system-modules))
	     (gnu packages compression) (gnu packages python) (gnu packages python-build)
	     (gnu packages guile) (ice-9 match) (srfi srfi-1) (srfi srfi-71)
	     (gnu packages rust-apps) (guix utils) (srfi srfi-26) (guix download)
	     (guix git-download) (ice-9 optargs) ((guix licenses) #:prefix license:)
	     (guix search-paths) (gnu packages rust) (gnu packages base))

;; Idea is to save some closure size (because things "lib" are usually linked
;; to statically so no point in downloading them if only the binary is needed instead).
;; Also, examples are usually just examples, not something that the end-user
;; is likely to want to run.
(define %rust-library-outputs '("out" ; rest
				"lib" ; the .rlib and the .crate-info
				;;   "env" ; the .crate-info (does splitting this have a point?)
				"examples"
				"tests"
				"benchmarks"))

(define (rust-environment-variables target)
  `(;; TODO gnueabihf?
    ("CARGO_CFG_TARGET_ENV" .
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
	  '())
    ("RUST_BACKTRACE" . "full") ; backtraces are useful for investigating build.rs and test failures, so enable them.
    ("RUSTC_BOOTSTRAP" . "1"))) ; make it sometimes possible to use unstable features (TODO: not really a ‘target’ environment variable, needs some renaming).

;; TODO: move to antioxidant.scm
(define generate-cbindgen-metadata-phase
  #~(lambda _
      ;; Generate the metadata as expected by cbindgen.
      ;; Not all fields are set, only the ones that seem to be required
      ;; are set and even then sometimes a dummy value suffices for now.
      (define package ((@@ (antioxidant) manifest-package) *manifest*))
      (define json-as-s-expression
	`(("packages" .
	   #((("name" . ,((@@ (antioxidant) package-name) package))
	      ("version" . ,((@@ (antioxidant) package-version) package))
	      ("id" . "the package we are building")
	      ("source" . null)
	      ("dependencies" . #())
	      ("targets" . #((("kind" . #("lib"))
			      ("crate_types" . #("lib"))
			      ("name" . ,((@@ (antioxidant) package-name) package))
			      ("src_path" . "src/lib.rs")))) ; TODO not true in general but sufficient for now
	      ("features")
	      ("manifest_path" . ,(in-vicinity (getcwd) "Cargo.toml")))))
	  ("workspace_members" . #("the package we are building"))
	  ("target_directory" . ,(getcwd)) ; TODO investigate proper valu
	  ("version" . ,1)
	  ("workspace_root" . ,(getcwd))))
      (call-with-output-file ".cbindgen-metadata.json"
	((@ (srfi srfi-26) cut)
	 (@ (json builder) scm->json) json-as-s-expression <>
	 #:pretty #true); #:pretty: might help with debugging and doesn't cost much
	#:encoding "UTF-8")
      (setenv "ANTIOXIDANT_CBINDGEN_METADATA" (in-vicinity (getcwd) ".cbindgen-metadata.json"))
      #;(copy-file #$(local-file "md.js") (getenv "ANTIOXIDANT_CBINDGEN_METADATA"))))

(define nu-plugin-phases
  ;; Otherwise, building nushell will fail with:
  ;;   warning: #<<crate-mapping> dependency-name: "nu" local-name: "nu"> not found in the available crates -- this might cause the build to fail!
  ;; error[E0460]: found possibly newer version of crate `nu_command` which `nu_cli` depends on
  ;;  --> src/main.rs:1:5
  ;;   |
  ;; 1 | use nu_cli::App as CliApp;
  ;;   |     ^^^^^^
  ;;   |
  ;;   = note: perhaps that crate needs to be recompiled?
  ;;   = note: the following crate versions were found:
  ;;           crate `nu_command`: /gnu/store/[...]-rust-nu-command-0.44.0/lib/guixcrate/libnu_command.rlib
  ;;           crate `nu_cli`: /gnu/store/[...]-rust-nu-cli-0.44.0/lib/guixcrate/libnu_cli.rlib
  ;;
  ;; TODO: maybe delete the compiled static libraries automatically when there's a main binary?
  ;; Would save disk space, and if the dependencies are compiled statically, possibly reduce the
  ;; closure ...
  #~((add-after 'install 'delete-libraries
       (lambda _
	 (delete-file-recursively (string-append #$output "/lib"))))))
	  
(define %custom-phases
  `(("rust-blake3-reference-impl"
     ,#~((add-after 'unpack 'chdir
	   (lambda _
	     (chdir "reference_impl")))))
    ;; TODO home page is incorrect
    ("rust-servo-fontconfig-sys"
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
    ("rust-test-cert-gen"
     ,#~((add-after 'unpack 'fix-references
	   (lambda* (#:key inputs #:allow-other-keys)
	     (substitute* "src/lib.rs"
	       (("Command::new\\(\"openssl\"\\)")
		(format #f "Command::new(~s)" (search-input-file inputs "bin/openssl"))))))))
    ("rust-mysqlclient-sys" ; the pkg-config file is named mariadb.pc, not mysqclient.pc
     ,#~((add-after 'unpack 'fix-pkg-config-use
	   (lambda _
	     (substitute* "build.rs"
	       ;; the mariadb.pc uses -lz without adding an appropriate -L
	       ;; (TODO).  Using the libmariadb.pc instead seems to work.
	       (("\"mysqlclient\"") "\"libmariadb\""))))))
    ("rust-libsqlite3-sys"
     ,#~((add-after 'unpack 'unbundle
	   (lambda _
	     (delete-file-recursively "sqlcipher")
	     (delete-file-recursively "sqlite3")))))
    ("rust-nitrokey-sys"
     ,#~((add-after 'unpack 'unbundle ;; TODO: upstream Guix
	   (lambda* (#:key inputs #:allow-other-keys)
	     (delete-file-recursively "libnitrokey-v3.6")
	     ;; tell build.rs to not use its bundled copy
	     (setenv "USE_SYSTEM_LIBNITROKEY" "1")
	     ;; used by Makefile
	     (setenv "LIBNITROKEY"
		     (search-input-directory inputs "include/libnitrokey"))
	     ;; Regenerate bindings.
	     (delete-file "src/ffi.rs")
	     (substitute* "Makefile"
	       (("quilt") ":") ; only usable from a git checkout
	       (("-- (.*)\n" _ argument) ; make it find stdbool.h (TODO: is there a more proper way (actually cross-platform, etc)?
		(string-append "-- " argument
			       " -I"
			       (search-input-directory inputs "lib/gcc/x86_64-unknown-linux-gnu/10.3.0/include")
			       "\n")))
	     (invoke "make" "src/ffi.rs")))
	 ;; TODO: simplify and make for robust by making it support pkg-config.
	 ;;
	 ;; Ensure that dependencies will actually find libnitrokey.
	 (add-before 'configure 'add-library-directory
	   (lambda* (#:key inputs #:allow-other-keys)
	     ((@@ (antioxidant) add-c-library-directory!)
	      (dirname (search-input-file inputs "lib/libnitrokey.so")))))))
    ("rust-x11rb"
     ;; TODO: x11rb does link(name = "xcb") to link to the lib, but does
     ;; not add the corresponding -L argument (which needs pkg-config
     ;; or such, or changes to antioxidant.).  TODO: if -l is added,
     ;; but not -L, bail out automaticallt.  TODO: takes 5.5 minutes to compile,
     ;; can this be reduced?
     ,#~((add-after 'configure 'find-xcb
	   (lambda* (#:key inputs #:allow-other-keys)
	     ((@@ (antioxidant) add-c-library-directory!)
	      (dirname (search-input-file inputs "lib/libxcb.so")))))))
    ("rust-mesalink" ,#~((delete 'bootstrap))) ; build.rs is sufficient
    ("rust-buffering-nocopy-macro"
     ,#~((add-after 'unpack 'new-syn-compatibility
	   ;; TODO: upstream
	   (lambda _
	     (substitute* "Cargo.toml"
	       (("\\[dependencies\\.quote\\]" line)
		(string-append "[dependencies.proc-macro2]\nversion=\"1\"\n"
			       line)))
	     (substitute* "src/lib.rs"
	       (("export::Span, ") "")
	       (("use proc_macro::TokenStream;")
		"use proc_macro::TokenStream; use proc_macro2::Span;"))))))
    ;; Make sure the headers will be installed in a proper location.
    ;; TODO: make sure dependencies actually find the result (newsboat-ffi).
    ;; TODO: set RUST_CXX_BUILD_OUTPUT in antioxidant.scm.
    ("rust-cxx-build"
     ,#~((add-after 'unpack 'avoid-scratch
	   ;; not sure if .clone is required
	   (lambda _
	     (substitute* "src/target.rs"
	       (("fn find_target_dir\\(out_dir: &Path) -> TargetDir \\{" line)
		(string-append line "
let mut out = out_dir.to_path_buf().clone();
out.push(\".debugging\");
return TargetDir::Path(out);
}
fn _find_target_dir_unused(out_dir: &Path) -> TargetDir {"
			       #;"return TargetDir::Path(out_dir.clone().push(\".debugging\"));}fn _find_target_dir_unused(out_dir: &Path) -> TargetDir {")))
	     (substitute* "src/lib.rs"
	       (("scratch::path\\(\"cxxbridge\"\\)")
		"panic!(\"rust-scratch is incompatible with the antioxidant compilation model without shenanigans, please set the output directory!\")")
	       (("paths::out_dir\\(\\)\\?")
		"Path::new(&env::var_os(\"RUST_CXX_BUILD_OUTPUT\").expect(\"RUST_CXX_BUILD_OUTPUT should be set\")).to_path_buf()"))))))
    ("rust-cxx"
     ,#~((add-after 'unpack 'do-not-install-headers-in-/tmp
	   (lambda _
	     ;; By default, this header is located in the unpacked source
	     ;; and rust-cxx will (when used by dependencies in their build.rs)
	     ;; try to refer to that location, in /tmp/guix-build-rust-cxx...,
	     ;; which cannot work, so properly install the header instead.
	     (install-file "include/cxx.h"
			   (string-append #$output "/include/rust-cxx"))
	     (substitute* "build.rs"
	       (("cxx_h.to_string_lossy\\(\\)")
		(string-append "\"" #$output "/include/rust-cxx/cxx.h\"")))))))
    ("rust-backtrace-sys"
     ,#~((add-after 'unpack 'break-cycle
	   (lambda _
	     ;; only needed for Android targets,
	     ;; by removing it we avoid depending
	     ;; on crate-cc, breaking a cycle
	     (delete-file "build.rs")
	     (substitute* "Cargo.toml"
			  (("^build =(.*)$") ""))))))
    ("rust-blakeout"
     ,#~((add-after 'unpack 'update-blake2
	   ;; Resolve build failure.
	   ;; Submitted upstream at
	   ;; <https://github.com/Revertron/Blakeout/pull/1>
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("use digest::Digest;") ; suggested by compiler
		"use digest::{Update,VariableOutput};")
	       (("Blake2s") "Blake2sVar")
	       (("Blake2sVar::default\\(\\)") "Blake2sVar::new(DEFAULT_HASH_SIZE).expect(\"incorrect output size\")")
	       (("let buf = Digest::finalize\\(digest\\);")
		"digest.finalize_variable(slice).expect(\"incorrect output size\");")
	       ((" slice.copy_from_slice\\(&buf\\[..\\]\\);") ""))))))
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
    ("nushell"
     ,#~((add-after 'unpack 'unbundle-self
	   (lambda _
	     (delete-file-recursively "src/plugins") ; we have separate rust-nu-plugin-... packages, so no need to build them again (TODO: add wrap-program)
	     (delete-file-recursively "crates") ; we have individual rust-nu-... packages
	     #; ; <--- TODO: let ci.guix.gnu test if still necessary
	     ;; TODO: build-binaries tries to build things anyway even though they were removed.
	     ;; For now, work-around
	     (substitute* "Cargo.toml"
	       (("\\[\\[bin\\]\\]") "[[bogus]]"))))
	 (add-after 'build-binaries 'find-plugins
	   (lambda* (#:key inputs #:allow-other-keys)
	     ;; TODO: maybe some substitute*-ions or a search path would be better?
	     ;; Use prefix instead of =, because this is a shell.
	     (wrap-program (string-append #$output "/bin/nu")
	       `("PATH" ":" prefix
		 ,(map (lambda (plugin)
			 (dirname
			  (search-input-file inputs
					     (string-append "bin/nu_plugin_" plugin))))
		       '("binaryview" "chart_bar" "from_bson" "from_sqlite" "inc"
			 "match" "query_json" "s3" "selector" "start" "textview"
			 "to_bson" "to_sqlite" "tree" "xpath"))))))))
    ("rust-shadow-rs"
     ,#~((add-after 'unpack 'fixup-source-date-epoch
	   (lambda _
	     ;; TODO: it nominally supports SOURCE_DATE_EPOCH, yet something things go wrong,
	     ;; as the shadow.rs still contains the unnormalised time stamp ...
	     ;; For now, do a work-around.  (Not yet reported upstream, because we are using
	     ;; and old version and maybe different chrono and time versions, so maybe
	     ;; a bug in the Guix packaging).
	     (substitute* '("src/lib.rs" "src/env.rs")
	       (("BuildTime::Local\\(Local::now\\(\\)\\)\\.human_format\\(\\)")
		(object->string "[timestamp expunged for reproducibility]"))
	       (("time\\.human_format\\(\\)")
		"\"[timestamp expunged for reproducibility]\".to_string()")
	       (("time\\.to_rfc3339_opts\\(SecondsFormat::Secs, true)")
		"\"[timestamp expunged for reproducibility]\".to_string()")
	       (("time\\.to_rfc2822\\(\\)")
		"\"[timestamp expunged for reproducibility]\".to_string()"))))
	 (add-after 'unpack 'more-reproducibility
	   ;; By default, it uses a hashmap, leading to an irreproducible ordering
	   ;; in the shadow.rs and hence an irreproducible .rmeta in the compiled
	   ;; crate.  Upstream: <https://github.com/baoyachi/shadow-rs/issues/96>.
	   (lambda _
	     (substitute* "src/lib.rs" ; sort
	       (("\\(k, v\\) in self\\.map\\.clone\\(\\)")
		"(k, v) in std::collections::BTreeMap::from_iter(self.map.clone().iter())")
	       (("self\\.write_const\\(k, v\\)") "self.write_const(k, v.clone())")
	       (("self\\.map\\.keys\\(\\)") "std::collections::BTreeSet::from_iter(self.map.keys())"))))))
    ("rust-nu-plugin-binaryview" ,nu-plugin-phases)
    ("rust-nu-plugin-chart" ,nu-plugin-phases)
    ("rust-nu-plugin-from-bson" ,nu-plugin-phases)
    ("rust-nu-plugin-from-sqlite" ,nu-plugin-phases)
    ("rust-nu-plugin-inc" ,nu-plugin-phases)
    ("rust-nu-plugin-match" ,nu-plugin-phases)
    ("rust-nu-plugin-query-json" ,nu-plugin-phases)
    ("rust-nu-plugin-s3" ,nu-plugin-phases)
    ("rust-nu-plugin-selector" ,nu-plugin-phases)
    ("rust-nu-plugin-start" ,nu-plugin-phases)
    ("rust-nu-plugin-textview" ,nu-plugin-phases)
    ("rust-nu-plugin-to-bson" ,nu-plugin-phases)
    ("rust-nu-plugin-to-sqlite" ,nu-plugin-phases)
    ("rust-nu-plugin-tree" ,nu-plugin-phases)
    ("rust-nu-plugin-xpath" ,nu-plugin-phases)
    ;; TODO: slow to compile nushell on my computer, let ci.guix.gnu.org
    ;; compile things, then continue with checking "nushell".
    ;; TODO: maybe compile nushell things with opt-level=s,
    ;; as done in the Cargo.toml?
    ("rust-nu-ansi-term"
     ,#~((add-after 'unpack 'avoid-difficult-rustc-errors-in-nushell
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("#!\\[crate_name (.*)") "")
	       (("#!\\[crate_type (.*)") ""))))))
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
    ("alacritty" ; TODO: install info pages etc
     ,#~((add-after 'unpack 'enter-directory
	   (lambda _
	     (chdir "alacritty")))))
    ("rust-arboard"
     ;; TODO: upstream/update
     ,#~((add-after 'unpack 'new-image-compatibility
	   (lambda _
	     (substitute* "src/common_linux.rs"
	       (("image::png::PngEncoder") "image::codecs::png::PngEncoder"))))))
    ;; TODO: upstream
    ("rust-argh-derive"
     ,#~((add-after 'unpack 'new-heck-compatibility
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("heck::KebabCase::to_kebab_case") "heck::ToKebabCase::to_kebab_case"))))))
    ;; TODO: upstream
    ("rust-salsa-macros"
     ,#~((add-after 'unpack 'new-heck-compatibility
	   (lambda _
	     (substitute* '("src/database_storage.rs" "src/query_group.rs")
	       (("heck::SnakeCase") "heck::ToSnakeCase")
	       (("heck::CamelCase") "heck::ToUpperCamelCase")
	       (("to_camel_case") "to_upper_camel_case"))))))
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
    ("rust-plotters"
     ,#~((add-after 'unpack 'new-dependencies-compatibility
	   ;; TODO: upstream
	   (lambda _
	     (substitute* "src/coord/ranged1d/types/datetime.rs"
	       (("use std::ops::\\{Add, Range, Sub\\};") "use std::ops::{Add, Range, Sub}; use std::convert::TryInto;")
	       (("total_span\\.num_nanoseconds\\(\\)") "Some(total_span.whole_nanoseconds())")
	       (("value_span\\.num_nanoseconds\\(\\)") "Some(value_span.whole_nanoseconds())")
	       (("self.0.num_nanoseconds\\(\\)") "self.0.whole_nanoseconds().try_into()")
	       (("\\bnum_days\\(\\)") "whole_days()")
	       (("\\bnum_weeks\\(\\)") "whole_weeks()"))
	     (substitute* "src/element/image.rs"
	       (("\\bto_bgra8\\b") "to_rgba8")))))) ;; TODO: does the ordering of the letters matter?
    ("rust-arrow2"
     ;; TODO: upstream/update
     ,#~((add-after 'unpack 'use-nondeprecated-names
	   (lambda _
	     (substitute* "src/temporal_conversions.rs"
	       (("\\bnum_days\\(\\)") "whole_days()"))))))
    ("rust-timer"
     ;; There's another patch available upstream:
     ;; <https://github.com/Yoric/timer.rs/pull/21>
     ;; but doesn't apply cleanly.  Do something simpler
     ;; for now.
     ,#~((add-after 'unpack 'use-nondeprecated-names
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("\\bnum_seconds\\(\\)") "whole_seconds()")
	       (("\\bnum_nanoseconds\\(\\)\\.unwrap\\(\\)") ; this replacement technically has different semantics, but in this context, the result is the same.
		"subsec_nanoseconds()"))))))
    ("rust-chrono-humanize"
     ;; TODO: upstream/update
     ,#~((add-after 'unpack 'use-new-names
	   (lambda _
	     ;; TODO: upstream
	     (substitute* "src/humantime.rs"
	       (("use crate::Humanize;") "use crate::Humanize; use std::convert::TryInto;")
	       (("Duration::zero\\(\\)") "Duration::ZERO")
	       (("num_days\\(\\)") "whole_days()")
	       (("num_weeks\\(\\)") "whole_weeks()")
	       (("num_hours\\(\\)") "whole_hours()")
	       (("num_seconds\\(\\)") "whole_seconds()")
	       (("num_nanoseconds\\(\\)\\.unwrap_or_default\\(\\)")
		"whole_nanoseconds().try_into().unwrap_or_default()")
	       (("num_microseconds\\(\\)\\.unwrap_or_default\\(\\)")
		"whole_microseconds().try_into().unwrap_or_default()")
	       (("num_minutes\\(\\)") "whole_minutes()")
	       (("self\\.0\\.num_milliseconds\\(\\)")
		"self.0.whole_milliseconds().try_into().expect(\"milliseconds overflow\")")))))) ; TODO?
    ("rust-pkcs1"
     ,#~((add-after 'unpack 'fix-typing
	   (lambda _
	     ;; Already upstream: <https://github.com/RustCrypto/formats/blob/fbf4334be7717e1f393c3f7b9b4c85c584ce8395/pkcs1/src/lib.rs#L49>, but not yet in any release.
	     (substitute* "src/lib.rs"
	       (("ObjectIdentifier::new") "ObjectIdentifier::new_unwrap"))))))
    ("rust-markup5ever" ; for rust-markup5ever@0.9, not needed by newever versions
     ,#~((add-after 'unpack 'new-phf-compatibility
	   (lambda _
	     (substitute* "build.rs"
	       (("phf_map\\.build\\(&mut file\\)\\.unwrap\\(\\)")
		"write!(&mut file, \"{}\", phf_map.build())"))))))
    ("rust-mio-extras"
     ,#~((add-after 'unpack 'mio@0.6.21-compatibility
	   (lambda _
	     (substitute* '("src/channel.rs" "src/timer.rs")
	       (("::\\{Evented,") "::{event::Evented,"))))))
    ("rust-num-format" ;; TODO: upstream
     ,#~((add-after 'unpack 'rust-arrayvec@0.7-compatibility
	   (lambda _
	     (substitute* "src/strings.rs"
	       (("ArrayString<\\[u8; \\$max_len\\]>")
		"ArrayString<$max_len>"))))))
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
    ("rust-multipart"
     ,#~((add-after 'unpack 'remove-uncompilable-example
	   (lambda _
	     (delete-file "src/bin/read_file.rs")))))
    ("rust-cpp-demangle"
     ,#~((add-after 'unpack 'delete-bin
	   (lambda _
	     (delete-file "src/bin/afl_runner.rs")))))
    ("rust-email" ;; TODO: upstream
     ,#~((add-after 'unpack 'new-rand-compatibility
	   (lambda _
	     (define first? #true)
	     (substitute* "src/message.rs"
	       (("\\.collect\\(\\)")
		(if first?
		    (begin
		      (set! first? #false)
		      ".map(char::from).collect()")
		    ".collect()")))))))
    ("rust-parasail-sys" ; TODO: license of bundled library seems to forbid writing a Wikipedia article of whatever that calls the Battelle Memorial Institute by name without consent by Battelle.  Also, the license fishes for citations.  Also, it isn't cross-compiled as required.
     ,#~((add-after 'unpack 'fixup-installation-location
	   (lambda _
	     (mkdir-p (in-vicinity #$output "/lib"))
	     (substitute* "build.rs"
	       (("env::var\\(\"OUT_DIR\"\\).unwrap\\(\\)") ; TODO: maybe set OUT_DIR to somewhere in the store, then this wouldn't be necessary
		(object->string (string-append #$output "/lib")))))) ; exact location doesn't matter as long as it's in the store
	 (add-after 'unpack 'set-shell-for-configure-script ; keep upstream Guix phase
	   (lambda* _
	     (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh")))))) ; TODO: bundles a C library
    ("rust-rspotify"
     ,#~((add-after 'unpack 'new-rand-compatibility
	   ;; TODO: upstream / update
	   (lambda _
	     (substitute* "src/util.rs"
	       (("\\.take\\(length\\)") ".take(length).map(char::from)"))))))
    ("spotify-tui" ;; TODO: upstream / update
     ,#~((add-after 'unpack 'new-tokio-compatibility
	   (lambda _
	     (substitute* "src/network.rs"
       	       (("tokio::time::delay_for") "tokio::time::sleep"))))))
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
    ("rust-tectonic-bridge-core"
     ;; Required to invoke 'cbindgen' later.
     ,#~((add-after 'load-manifest 'generate-cbindgen-metadata
		    #$generate-cbindgen-metadata-phase)
	 ;; Build a generated file from source, using the instructions
	 ;; mentioned in the README.
	 (add-after 'generate-cbindgen-metadata-phase 'regenerate-cbindgen-things
	   (lambda _
	     (delete-file "support/tectonic_bridge_core_generated.h")
	     (invoke "cbindgen" "--output" "support/tectonic_bridge_core_generated.h")))
	 ;; Actually install the headers somewhere where they can be found
	 ;; by dependencies.
	 (add-after 'unpack 'fixup-headers-locations
	   (lambda _
	     (substitute* "build.rs"
	       (("main_header_src\\.display\\(\\)")
		(string-append "\"" #$output "/include\""))
	       (("env::var\\(\"OUT_DIR\"\\).unwrap\\(\\)") ; TODO: maybe set OUT_DIR to somewhere in the store, then this wouldn't be necessary
		(string-append "\"" #$output "/include\"")))))
	 (add-after 'install 'install-header
	   (lambda _
	     (install-file "support/tectonic_bridge_core_generated.h"
			   (string-append #$output "/include"))
	     (install-file "support/tectonic_bridge_core.h"
			   (string-append #$output "/include"))))))
    ("rust-tectonic-bridge-flate"
     ;; required to make rust-cbindgen produce building C code.
     ,#~((add-after 'load-manifest 'generate-cbindgen-metadata
		    #$generate-cbindgen-metadata-phase)
	 (add-after 'unpack 'fixup-headers-locations ; see rust-tectonic-bridge-core
	   (lambda _
	     (substitute* "build.rs"
	       (("env::var\\(\"OUT_DIR\"\\).unwrap\\(\\)") ; TODO: maybe set OUT_DIR to somewhere in the store, then this wouldn't be necessary
		(string-append "\"" #$output "/include\"")))))))
    ("rust-tectonic-pdf-io"
     ;; Put headers somewhere where they can be found by dependencies.
     ,#~((add-after 'unpack 'fixup-headers-location
	   (lambda _
	     (copy-recursively "pdf_io" (in-vicinity #$output "include"))
	     (for-each delete-file (find-files (in-vicinity #$output "include")
					       "\\.c$"))
	     (substitute* "build.rs"
			  (("main_header_src\\.display\\(\\)")
			   (object->string (in-vicinity #$output "include"))))))))
    ("rust-tectonic-xetex-layout"
     ;; Put headers somewhere where they can be found by dependencies.
     ;; TODO: OUT_DIR.
     ,#~((add-after 'unpack 'fixup-headers-location
	   (lambda _
	     (define destination (in-vicinity #$output "include"))
	     (mkdir-p destination)
	     (substitute* "build.rs"
	       (("\\benv::var\\(\"OUT_DIR\"\\)\\.unwrap\\(\\)")
		(object->string destination)))))))
    ("rust-tectonic-engine-xetex"
     ,#~((add-after 'unpack 'find-fontconfig ; TODO: what causes <fontconfig/fontconfig.h> not to be found even though it is in C_INCLUDE_PATH?
	   (lambda* (#:key inputs #:allow-other-keys)
	     (substitute* "xetex/xetex-core.h"
	       (("\\bfontconfig/fontconfig\\.h\\b")
		(search-input-file inputs "include/fontconfig/fontconfig.h"))
	       (("<harfbuzz\\b") ; XXX: -I points to subdirectory yet parent directory is used
		(string-append
		 "<"
		 (search-input-directory inputs "include/harfbuzz"))))))))
    ("rust-tectonic-engine-bibtex"
     ;; required to use rust-cbindgen.
     ,#~((add-after 'load-manifest 'generate-cbindgen-metadata
		    #$generate-cbindgen-metadata-phase)))
    ("rust-tectonic-engine-xdvipdfmx"
     ;; required to use rust-cbindgen.
     ,#~((add-after 'load-manifest 'generate-cbindgen-metadata
		    #$generate-cbindgen-metadata-phase)))
    ("rust-tikv-jemalloc-ctl"
     ,#~((add-after 'unpack 'use-conventional-name
	   (lambda _
	     (substitute* "Cargo.toml"
	       (("\\[dependencies.tikv-jemalloc-sys\\]" line)
		(string-append line "\npackage = \"jemalloc-sys\"")))))))
    ("rust-ncurses"
     ,#~((add-before 'configure 'find-library-directory
	   (lambda* (#:key inputs #:allow-other-keys)
	     ;; Somehow the library directory is not found, curses!
	     ;; Work-around thi by manually adding the library directory.
	     ((@@ (antioxidant) add-c-library-directory!)
	      (dirname (search-input-file inputs "lib/libncurses.so")))))))
    ("rust-tree-magic-mini"
     ,#~((add-after 'unpack 'use-unbundled-magic-data
	   (lambda* (#:key inputs #:allow-other-keys)
	     (substitute* "src/fdo_magic/builtin/runtime.rs"
	       ;; Looks broken in Guix w.r.t. foreign distros.  The $HOME/.local/share/mime/magic
	       ;; looks ok-ish, but would be unpopulated in Guix.  These substitutions are fragile,
	       ;; please check them after an update!
	       ;; See: <https://issues.guix.gnu.org/56173>.
	      (("&\\[&str; 3\\]") "&[&str; 1]")
	      (("\"/usr/share/mime/subclasses\",") "")
	      (("\"/usr/local/share/mime/subclasses\",") "")
	      (("\\$HOME/\\.local/share/mime/subclasses")
	       (search-input-file inputs "share/mime/subclasses"))
	      (("\"/usr/share/mime/magic\",") "")
	      (("\"/usr/local/share/mime/magic\",") "")
	      (("\\$HOME/\\.local/share/mime/magic")
	       (search-input-file inputs "share/mime/magic"))
	      (("\"/usr/share/mime/aliases\",") "")
	      (("\"/usr/local/share/mime/aliases\",") "")
	      (("\\$HOME/\\.local/share/mime/aliases")
	       (search-input-file inputs "share/mime/aliases")))
	     #;(error "barf"))))) ; for checking after an update with --keep-failed
    #; ; <--- TODO: let ci.guix.gnu test if still necessary
    ("tectonic" ; TODO: binary is compiled thrice
     ;; TODO: bug in implementation in 'autobins'?
     ,#~((add-after 'unpack 'fix-found-binaries
	   (lambda _
	     (substitute* "Cargo.toml"
	       (("^(repository = .*)$" line)
		(string-append line "autobins = false\n[[bin]]\nname = \"tectonic\"\npath=\"src/bin/tectonic/main.rs\"\n")))))))
    #; ; <--- TODO: let ci.guix.gnu test if still necessary
    ;; TODO: likewise
    ("bat"
     ;; TODO: bug in implementation in 'autobins'?
     ,#~((add-after 'unpack 'fix-found-binaries
	   (lambda _
	     (substitute* "Cargo.toml"
	       (("^(repository = .*)$" line)
		(string-append line "autobins = false\n[[bin]]\nname = \"bat\"\npath=\"src/bin/bat/main.rs\"\n")))))))
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
    ("rust-dashmap"
     ,#~((add-after 'unpack 'unstable-rust
	   ;; Required to use std::thread::available_parallelism
	   (lambda _
	     (substitute* "src/lib.rs"
			  (("#!\\[allow\\(clippy::type_complexity\\)]" line)
			   (string-append "#![feature(available_parallelism)]\n" line)))))))
    ("rust-nu-protocol"
     ,#~((add-after 'unpack 'new-chrono-compatibility
	   (lambda _
	     ;; TODO: upstream
	     (substitute* "src/value/primitive.rs"
	       (("&chrono::Duration::nanoseconds")
		"chrono::Duration::nanoseconds")
	       (("&chrono::Duration::seconds")
		"chrono::Duration::seconds")
	       (("\\bnum_seconds\\(\\)") "whole_seconds()")
	       (("\\bnum_nanoseconds\\(\\)") "whole_nanoseconds()") ; technically has a different semantics but in this context the result is the same
	       (("\\.expect\\(\"Unexpected overflow\"\\) as u32") "as u32"))))))
    ("rust-os-pipe"
     ,#~((add-after 'unpack 'no-cargo
	   ;; TODO would be nice to build them with debug
	   ;; assertions. Calling cargo isn't going to work.
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("path_to_exe\\(\"([a-z_]+)\"\\)" _ name)
		(object->string
		 (string-append #$output "/bin/" name))))))))
    ("circtools"
     ,#~((add-after 'unpack 'fixup-link-search
	   ;; TODO: what's the rustc-flags=-L for?
	   ;; Why not rustc-link-search?
	   ;; What's up with separate lib/lib64 directories?
	   (lambda _
	     (substitute* "build.rs"
	       (("\\bdst\\.display\\(\\)") (object->string "build/lib"))
	       (("^.*cargo:rustc-flags=.*$") ""))))))
    ("rust-cc"
     ,#~((add-after 'unpack 'fix-cc
	   (lambda _
	     (substitute* "src/lib.rs"
	       (("\"cc\"") "\"gcc\"")
	       (("\"c++\"") "\"g++\""))))))
    ("rust-watchexec"
     ;; All src/main.rs does is throwing a compilation error telling that the
     ;; actual binary is moved to another crate, so don't try to build it.
     ,#~((delete 'build-binaries)))))

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
	   ((? (cut string-prefix? "rust-salsa-macros-0.17.0-pre.2" <>) name)
	    "rust-salsa-macros")
	   (_ (drop-version name)))))
    (match (assoc name %custom-phases)
      ((_ phases) phases)
      (#false #~()))))

;; 4.7.1 has an incompatible change, see
;; <https://github.com/aconchillo/guile-json/issues/78#issuecomment-1166225845>.
;; Let's see wait to see what the maintainer thinks of (partially) reverting
;; that commit.
(define-public guile-json-4
  (package
    (inherit (@ (gnu packages guile) guile-json-4))
    (name "guile-json")
    (version "4.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0cqr0ljqmzlc2bwrapcsmcgxg147h66mcxf23824ri5i6vn4dc0s"))))))


(define* (antioxidant-build name inputs #:key
			    (phases #~%standard-antioxidant-phases)
			    (rust-dynamic-library-arguments #false)
			    (tests? #true)
			    (test-options #~'()) ; G-exp evaluating to a list of CLI arguments
			    modules ; what to do about 'modules'
			    install-source? ; not used by antioxidant-build-system
			    system target source search-paths outputs
			    (rust-metadata "")
			    (optimisation-level "3") ; the Cargo default.  IIUC, also enables LTO.
			    (debuginfo-level "1") ; 1: line tables
			    (features #~'("default"))
			    (cargo-target-directory #false)
			    (rust-crate-type #false)
			    (rust-environment-variables
			     #~'#$(rust-environment-variables
				   (or target
				       (nix-system->gnu-triplet system)))))
  (define builder
    (with-extensions (list guile-json-4)
    (with-imported-modules
	(cons '(antioxidant)
	      (source-module-closure '((guix build utils)
				       (guix build syscalls) ; <-- can be removed once in (guix build antioxidant)
				       (guix build gnu-build-system)
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
		     #:debuginfo-level '#$debuginfo-level
		     #:rust-environment-variables #$rust-environment-variables
		     #:cargo-target-directory #$cargo-target-directory ; <-- TODO: unused, maybe remove?
		     #:rust-crate-type #$rust-crate-type
		     #:rust-metadata #$rust-metadata
		     #:rust-dynamic-library-arguments #$rust-dynamic-library-arguments
		     #:strip-binaries? #false ; TODO exported symbols are removed
		     #:tests? #$tests?
		     #:test-options #$test-options
		     #:phases (modify-phases #$phases
				#$@(custom-phases name)))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target
		(features #~'("default"))
		(rust-metadata #~"")
		#:allow-other-keys
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
    (outputs outputs)
    (build (if target antioxidant-cross-build antioxidant-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define-public antioxidant-build-system
  (build-system
   (name 'antioxidant)
   (description "Build software written in Rust, without cargo")
   (lower lower)))

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

(define crate-uri (@ (guix build-system cargo) crate-uri))

(define* (package-with-rust-features base new-features #:key (name (package-name base))
				     (rust-metadata #false))
  "Return a variant of BASE with name NAME build with the features FEATURES.
To distinguish this variant from other variants, RUST-METADATA can be set to
an unique string, which can be useful for resolving symbol conflicts.  By default,
the metadata will be set based on the major version (or the minor version, if the major
version is 0), if it is in %automatic-metadata.."
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

(define rust-atomic-take ; required by new rust-watchexec
  (package
    (name "rust-atomic-take")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atomic-take" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zmn2pay3p94kcg9b8qz2kd26flzchlg1lcq685sixjznd7mxxpr"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-development-inputs (("rust-futures" ,rust-futures-0.3))))
    (home-page "https://github.com/Darksonn/atomic-take")
    (synopsis "Atomically take a value out of a container once.")
    (description "Atomically take a value out of a container once.")
    (license license:expat)))

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

(define rust-signal-hook-mio ; @0.2.1 doesn't suppport rust-mio@0.8
  (package
    (inherit (p rust-signal-hook-mio-0.2))
    (name "rust-signal-hook-mio")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "signal-hook-mio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bwrrbd0lhwzlf63708vyzlh20693s5bg5s0ak6adjbyycajxb99"))))))

(define rust-time ; rust-sequoia-sq requires new rust-time or old rust-chrono, to have the std::fmt::Display trait implemented for chrono::Duration
  (package
    (inherit (p rust-time-0.3))
    (name "rust-time")
    (version "0.3.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05rjpgfsq6gvizn89ydwwmy0ihgfvikxcwq8bz09dw5jvi0izjbj"))))))

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

(define rust-tokio-postgres ; @0.7.2 doesn't build against new rust-phf
  (package
    (inherit (p rust-tokio-postgres-0.7))
    (name "rust-tokio-postgres")
    (version "0.7.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-postgres" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bn8f466n1s5q674hr7yq7fybiszpa7v73yjxk4jsp9wl93qmj0r"))))))

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

(define rust-rust-decimal ; @1.14.3 incompatible with new rust-arrayvec
  (package
    (inherit (p rust-rust-decimal-1))
    (name "rust-rust-decimal")
    (version "1.25.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rust-decimal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z78vwr4apw2h8c6iijv8xvvsvjq9c87ky8v36mz2cskx1cbp8rl"))))))

;; From: https://issues.guix.gnu.org/54299 (make sure to include attribution!)
(define rust-alacritty-terminal
  (package
   (inherit (p rust-alacritty-terminal-0.15))
   (name "rust-alacritty-terminal")
   (version "0.16.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "alacritty_terminal" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0bvffvjmkran068p9bz0p9nrkj1k4bggd7q39mszjfafz155vyq2"))))))

(define rust-dlib
  (package
    (inherit (p rust-dlib-0.4)) ; new rust-smithay-client-toolkit needs new rust-dlib to build without 'dlopen'
    (name "rust-dlib")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dlib" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1547hy7nrhkrb2i09va244c0h8mr845ccbs2d2mc414c68bpa6xc"))))))

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

(define rust-http ; new rust-actix-http doesn't compile against old rust-http
  (package
    (inherit (p rust-http-0.2))
    (name "rust-http")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1fxzyvspr6g8znc6i0kif0bhpih8ibhy7xc6k984j8pm19bp11pz"))))))

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
;; Old rust-sha1 doesn't implement CoreProxy while required by rust-pkcs5.
;; To avoid a collision with rust-sha-1, give it a different name.
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
      (base32 "0bw56hxajrgb3pjg0cr5xrvmx0jna39564iw2p14ama5cmzlwzy7"))))
   (arguments (append (list #:rust-metadata "guix-variant=rust-sha1")
		      (package-arguments (p rust-sha1-0.6))))))
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

(define rust-bytestring ; rust-actix-http@3 requires into_bytes
  (package
    (inherit (p rust-bytestring-0.1))
    (name "rust-bytestring")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytestring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ka9gkn2rrxms0d5s32ckpchh06qmgidbh4xw630gfcpkshnnw4h"))))))

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

(define rust-actix-http ; @2 has non-building dependency rust-actix-connect@2
  (package
    (inherit (p rust-actix-http-2))
    (name "rust-actix-http")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-http" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0ykrwybs3ssi9ifn5p2gddi4909adjxs3gk450r0sk8d3aw5r255"))))))

(define rust-actix-web ; @3 doesn't build against updated actix dependencies
  (package
    (inherit (p rust-actix-web-3))
    (name "rust-actix-web")
    (version "4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-web" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0cadlzb6syha1jwx0pghasj0qd47jkjy03dfldbdyl0xspzyprgl"))))))

(define rust-actix-web-codegen ; rust-actix-web@4 is incompatible with rust-actix-web-codegen@0.4
  (package
    (inherit (p rust-actix-web-codegen-0.4))
    (name "rust-actix-web-codegen")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-web-codegen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1g4imsr56b82lp76k2krg0vzfv92x7kxg24y8qfvnjkhakgvw9bm"))))))

(define rust-actix-router
  (package
    (inherit (p rust-actix-router-0.2)) ; new rust-actix-web doesn't build against old @0.2
    (name "rust-actix-router")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix-router" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "107s475ghxl1sliqcjdji7272hi71a491k2n9bqg4661a9mq8q7b"))))))

(define rust-awc ; @2 doesn't build
  ;; TODO: some build failures remain
  (package
    (inherit (p rust-awc-2))
    (name "rust-awc")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "awc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "16l8zvzx522vnhvn9cgfqhrvf4z50vbrfsz8cpiwxj7kzd20rik5"))))))

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

(define rust-warp ; @0.2 doesn't build
  (package
    (inherit (p rust-warp-0.2))
    (name "rust-warp")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "warp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0zjqbg2j1fdpqq74bi80hmvyakf1f771d7vrmkqvg90lj4g4xvrw"))
	(patches (list (local-file "warp-Update-tokio-rustls-to-v0.23.patch")
		       (local-file "warp-Update-tungstenite-to-0.17.2.patch")))
	(snippet #~(begin
		     (delete-file "Cargo.toml")
		     (rename-file "Cargo.toml.orig" "Cargo.toml")))))))

(define rust-wayland-client ; for compatibility with new rust-smithay-client-toolkit
  (package
    (inherit (@ (gnu packages crates-graphics) rust-wayland-client-0.28))
    (name "rust-wayland-client")
    (version "0.29.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wayland-client" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13s5sj9344izk2g48yizk81kcg8jg4940gg2v6bzcmrjwxh388li"))))))

(define rust-wayland-commons
  (package ; for compatibility with new rust-nix
    (inherit (@ (gnu packages crates-graphics) rust-wayland-commons-0.28))
    (name "rust-wayland-commons")
    (version "0.29.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wayland-commons" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gnk4a771i3g1k4fbzx54xnganpc9j68jrx8xj839hfp83iybxll"))))))

(define rust-wayland-cursor
  (package ; @ 0.28.3 doesn't build against new rust-nix
    (inherit (@ (gnu packages crates-graphics) rust-wayland-cursor-0.28))
    (name "rust-wayland-cursor")
    (version "0.29.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wayland-cursor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gd6aswkrdz556n54pjpd4rchw7jkgcx6hnrhgy62y2y7pqmh9y5"))))))

(define rust-wayland-protocols ; new rust-winit required a new version
  (package
    (inherit (@ (gnu packages crates-graphics) rust-wayland-protocols-0.28))
    (name "rust-wayland-protocols")
    (version "0.29.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wayland-protocols" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hap8vky2fwsq05c98c8xs00gb9m5kxp8kq3zr0jwh036gi7l530"))))))

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

(define rust-blake3-reference-impl
  (package
   (inherit (@ (gnu packages crypto) rust-blake3-1))
   (build-system antioxidant-build-system)
   (arguments '())
   (name "rust-blake3-reference-impl")
   (synopsis "Reference implementation of BLAKE3.")
   (description "This package is the reference implementation of BLAKE3.

It is not optimised for performance.  As such, users are recommended to use
@code{rust-blake3} instead.")))

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

(define rust-actix ; @0.10 doesn't build
  (package
    (inherit (p rust-actix-0.10))
    (name "rust-actix")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "actix" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1mdy5i7f1px5kfyv3s5li985xry9zby4zc2bpy2qac8wr950ca7p"))))))

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

(define rust-btoi ; required by rust-git-actor
  (package
    (name "rust-btoi")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "btoi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04ba4j96icaan10c613s2rwpn2kdbl8728qhz2xzi0dakyd8dh4p"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-num-traits" ,(p rust-num-traits-0.2)))
       #:cargo-development-inputs
       (("rust-bencher" ,(p rust-bencher-0.1))
	("rust-quickcheck" ,(p rust-quickcheck-0.9)))))
    (home-page "https://github.com/niklasf/rust-btoi")
    (synopsis "Parse integers directly from ASCII byte slices")
    (description "Parse integers directly from ASCII byte slices")
    (license (list license:expat license:asl2.0))))

(define rust-h2
  (package
    (inherit (p rust-h2-0.3)) ; new version required by new rust-reqwest
    (name "rust-h2")
    (version "0.3.13")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "h2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0msasdyv0n7avs5i1csjrs0rvdsp4k5z3fwl8rd53jbzcdnjra1p"))))))

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

(define rust-dashmap ; rust-git-tempfile@2.0.1 requires new dashmap
  (package
    (inherit (p rust-dashmap-4))
    (name "rust-dashmap")
    (version "5.3.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dashmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13s4rdgb2rkavsgzjks23zb0zviz7x2g8fb4jwc2xkqwkhn9359l"))))))

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

(define rust-emacs-macros
  (package
    (inherit (p rust-emacs-macros-0.11)) ; @0.11 doesn't build against new rust-syn
    (name "rust-emacs-macros")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "emacs-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qg1dcn5acbirq617qq2fgg9adswif2dnr292s3qnq62wzgnyrb9"))))))

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

(define rust-miette ; required by new rust-watchexec
  (package
    (name "rust-miette")
    (version "4.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "miette" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1365xpl8l66lsvn6bk4mhbpxf5gciiazj4apyiaqn87r8jg3540w"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-atty" ,(p rust-atty-0.2))
		       ("rust-backtrace" ,(p rust-backtrace-0.3))
                       ("rust-miette-derive" ,rust-miette-derive)
                       ("rust-once-cell" ,(p rust-once-cell-1))
                       ;; ("rust-owo-colors" ,(p rust-owo-colors-3)) ; TODO
                       ;; ("rust-supports-color" ,(p rust-supports-color-1)) ; TODO
                       ;; ("rust-supports-hyperlinks" ,(p rust-supports-hyperlinks-1)) ; TODO
                       ;; ("rust-supports-unicode" ,(p rust-supports-unicode-1)) ; TODO
                       ("rust-terminal-size" ,(p rust-terminal-size-0.1))
                       ("rust-textwrap" ,(p rust-textwrap-0.12)) ; TODO: requires @0.15 in Cargo.toml
                       ("rust-thiserror" ,(p rust-thiserror-1))
                       ("rust-unicode-width" ,(p rust-unicode-width-0.1)))
       #:cargo-development-inputs
       (("rust-futures" ,(p rust-futures-0.3))
	("rust-indenter" ,(p rust-indenter-0.3))
        ("rust-rustversion" ,(p rust-rustversion-1))
        ("rust-semver" ,(p rust-semver-1))
        ("rust-syn" ,(p rust-syn-1))
        ("rust-trybuild" ,(p rust-trybuild-1)))))
    (home-page "https://github.com/zkat/miette")
    (synopsis
     "Fancy diagnostic reporting library and protocol for us mere mortals who aren't compiler hackers.")
    (description
     "Fancy diagnostic reporting library and protocol for us mere mortals who aren't
compiler hackers.")
    (license license:asl2.0)))

(define rust-miette-derive ; required by rust-miette
  (package
    (name "rust-miette-derive")
    (version "4.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "miette-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k0ph38lxzqamaabind8463j2k5qjg6jhhbcdrg1pkqvfrdw8nvb"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,(p rust-proc-macro2-1))
		       ("rust-quote" ,(p rust-quote-1))
                       ("rust-syn" ,(p rust-syn-1)))))
    (home-page "https://github.com/zkat/miette")
    (synopsis "Derive macros for miette. Like `thiserror` for Diagnostics.")
    (description
     "Derive macros for miette.  Like `thiserror` for Diagnostics.")
    (license license:asl2.0)))

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

(define rust-pwd
  (package
    (name "rust-pwd")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pwd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yxhkkkqi1ckln37vdz6gc16aykw88h02548rafi153mhl207jpr"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,(p rust-failure-0.1))
	("rust-libc" ,(p rust-libc-0.2)))))
    (home-page "https://gitlab.com/pwoolcoc/pwd.git")
    (synopsis "Safe interface to pwd.h
")
    (description "Safe interface to pwd.h")
    (license 'unknown-license!)));; TODO: https://gitlab.com/pwoolcoc/pwd/-/issues/1


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

(define rust-local-waker ; used by rust-local-channel, which is used by rust-actix-http
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

(define rust-local-channel
  (package
    (name "rust-local-channel")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "local-channel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "177wqgadrlw6m7r6xxafkj58asgpgbpv1ww4gx258v2cx703wc3z"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
      `(#:cargo-inputs
        (("rust-futures-core" ,rust-futures-core-0.3)
         ("rust-futures-sink" ,rust-futures-sink-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-local-waker" ,rust-local-waker))))
    (home-page "https://github.com/actix/actix-net.git")
    (synopsis
      "A non-threadsafe multi-producer, single-consumer, futures-aware, FIFO queue")
    (description
      "This package provides a non-threadsafe multi-producer, single-consumer,
futures-aware, FIFO queue")
    (license (list license:expat license:asl2.0))))

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

(define rust-num-threads ; required by new rust-time
  (package
    (name "rust-num-threads")
    (version "0.1.6")
    (outputs %rust-library-outputs)
    (source (origin
              (method url-fetch)
              (uri (crate-uri "num_threads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i5vmffsv6g79z869flp1sja69g1gapddjagdw1k3q9f3l2cw698"))))
    (build-system antioxidant-build-system)
    (arguments
     (list
      #:test-options
      #~(list "--test-threads=1"))) ; test parallelism interferes with rust-num-thread's tests
    ;; rust-libc is only needed on macos, ios and freebsd
    (home-page "https://github.com/jhpratt/num_threads")
    (synopsis
     "A minimal library that determines the number of running threads for the current process.")
    (description
     "This package provides a minimal library that determines the number of running
threads for the current process.")
    (license (list license:expat license:asl2.0))))

(define rust-firestorm ; not yet in Guix, required by new rust-actix-router
  (package
    (name "rust-firestorm")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "firestorm" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "162zjw9skavhmcfsjzsywwa6yj492i98ljx8m8m7x99djhn6qprc"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (home-page "https://github.com/That3Percent/firestorm")
    (synopsis "A low overhead intrusive flamegraph profiler.")
    (description
      "This package provides a low overhead intrusive flamegraph profiler.")
    (license license:expat)))

;; TODO: lots of rust-git-... crates, maybe (gnu packages gitoxide)?

(define rust-hdrhistogram ; old hdrhistogram doesn't build against new rust-nom
  (package
    (inherit (p rust-hdrhistogram-6))
    (name "rust-hdrhistogram")
    (version "7.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hdrhistogram" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h0905yk0pxgxfk4kzlfmnglm6ky1ssbrpf4ars4yb5y25q2nrri"))))))

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

(define rust-iso8601 ; required for compatibility with new rust-nom
  (package
    (inherit (p rust-iso8601-0.1))
    (name "rust-iso8601")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "iso8601" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15nfg6d4qlniw4gk7039s5y07lzgr1dp9snsw63lsxarnyz4zfg5"))))))

(define rust-jsonrpc-core ; @14.0 doesn't build against new rust-futures
  (package
    (inherit (p rust-jsonrpc-core-14))
    (name "rust-jsonrpc-core")
    (version "18.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jsonrpc-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sv5m6bxyscdqg8cfzlsm8f3vks3972zc9w475l4h19dxxmggxql"))))))

(define rust-s3handler ; @.7 doesn't build against new rust-hmac
  (package
    (inherit (p rust-s3handler-0.7))
    (name "rust-s3handler")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "s3handler" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0142vqnkknc9ibv8wn3nc0pqhxqgngrrily7v46zzjwyc01pmg4z"))))))


(define rust-postgres-protocol ; old version doesn't build against new rust-hmac
  (package
    (inherit (p rust-postgres-protocol-0.6))
    (name "rust-postgres-protocol")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "postgres-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0365asqd8v2iij8sl0282rrc0ixzkixl0jr0m2day0vfjnznr347"))))))

(define rust-lettre
  (package
    (inherit (p rust-lettre-0.9))
    (name "rust-lettre")
    (version "0.10.0-rc.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lettre" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i1r8mpdrwvq64blzfz8b1g0vcgw81silyyqmfkywqj1skcqfzhg"))))))

(define rust-postgres-types ; old version doesn't build against new rust-time
  (package
    (inherit (p rust-postgres-types-0.2))
    (name "rust-postgres-types")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "postgres-types" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "129xn3q32r92ylwggrlg5l0z2w8hfx6d56z8j291cwws32vyimpb"))))))

(define rust-reqwest ; @0.11.4 fails to build and uses deprecated functions from cookie_store
  (package
   (inherit (p rust-reqwest-0.11))
   (name "rust-reqwest")
   (version "0.11.10")
   (source (origin
            (method url-fetch)
            (uri (crate-uri "reqwest" version))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "1ss1ijakw48dgpxaj5a38pk0r3vmzhdgaj842ssfir9m9ymgg8a6"))))))

(define rust-zip ; @0.5.13 doesn't compile against new rust-time
  (package
    (inherit (p rust-zip-0.5))
    (name "rust-zip")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zip" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "138brxnsknbvdh7h5h4rysfpcgvspp3pa177jsscnlmvfg7mn8mz"))))))

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

(define rust-tokio-tungstenite ; @0.11 doesn't build
  (package
    (inherit (p rust-tokio-tungstenite-0.11))
    (name "rust-tokio-tungstenite")
    (version "0.17.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-tungstenite" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1bi1z1l8392v20mg24gryw5jrm0166wxa155z138qma958is3k86"))))))

(define rust-totp-lite ; @1.0 doesn't build against new rust-digest
  (package
    (inherit (p rust-totp-lite-1))
    (name "rust-totp-lite")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "totp-lite" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yi9s6firixay11rahqshdv07ih8i27fxqqrrshfk3wwbn3rdi2w"))))))

(define rust-tree-magic-mini ; fork with updated dependencies, see <https://github.com/aahancoc/tree_magic/pull/20>
  (package
    (inherit (p rust-tree-magic-0.2))
    (name "rust-tree-magic-mini")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tree-magic-mini" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vdazv3y1iggriwx5ksin72c2ds0xjdhx1yvmd5nxkya0w3gvbci")))))) ; TODO: check license

(define rust-unicode-bom ; required by rust-git-config
  (package
    (name "rust-unicode-bom")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unicode-bom" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cpc54ssrlrwm3x8dm7z1dgwj9r9bxjls620ra1vfxfq87snkv33"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (home-page "https://gitlab.com/philbooth/unicode-bom")
    (synopsis "Unicode byte-order mark detection for files and byte arrays.")
    (description
     "Unicode byte-order mark detection for files and byte arrays.")
    (license license:asl2.0)))

(define rust-calloop ; old version doesn't build against new rust-nix
  (package
    (inherit (p rust-calloop-0.6))
    (name "rust-calloop")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "calloop" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bp0h8d8k7r05ggp7ip3y26anwaspld134mgx46s9s1z913128l4"))))
    (inputs
     (modify-inputs (package-inputs (p rust-calloop-0.6))
		    (prepend
		     (p rust-thiserror-1) ; TODO: where to put macros (native/non-native)?
		     (p rust-vec-map-0.8) rust-futures-util-0.3 rust-slotmap)))))

(define rust-command-group
  (package
    (name "rust-command-group")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "command-group" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b7d9gy9dhw4jqx5x8njzmaifgxqw0nywjry7bgmjjlv81psia7p"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,(p rust-async-trait-0.1))
		       ("rust-nix" ,(p rust-nix-0.22))
                       ("rust-tokio" ,(p rust-tokio-1)))
       #:cargo-development-inputs
       (("rust-tokio" ,(p rust-tokio-1)))))
    (home-page "https://github.com/watchexec/command-group")
    (synopsis "Extension to Command to spawn in a process group")
    (description "Extension to Command to spawn in a process group")
    (license (list license:asl2.0 license:expat))))

(define rust-config ; @0.11 doesn't build against new rust-nom
  (package
    (inherit (p rust-config-0.11))
    (name "rust-config")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06xk2846zsa239h2jr34jbnz9d8hyz4d6m9v9q1bbpvf9fviga9y"))))))

(define rust-crossterm
  (package
    (inherit (p rust-crossterm-0.20)) ; @0.19 and @0.20 and don't build against new dependencies
    (name "rust-crossterm")
    (version "0.23.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossterm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05ygjddfp70961ij7cbvrrwz88r09hghdpcqbf50z4c1yyj2w452"))))))


(define rust-ctrlc ; for compatibility with new rust-nix
  (package
    (inherit (p rust-ctrlc-3))
    (name "rust-ctrlc")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ctrlc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r88w8l4hxc64w43xlwjk5f60vg57vdahnjy3w5f0qb89slflzxk"))))))

(define rust-email ; for compatibility with new rust-base64
  (package
    (inherit (p rust-email-0.0.20))
    (name "rust-email")
    (version "0.0.21")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "email" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gkv0lgs1apmq3w13pj2qr2bxiy42hw3vgi1jsb705l3p01hadk5"))))))

(define rust-email-address ; required by new rust-lettre
  (package
    (name "rust-email-address")
    (outputs %rust-library-outputs)
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "email-address" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13j6s1qz5x13rdc0zrmsjfdqqqdshksrwqmrwnhxyms8rg4vg146"))))
    (build-system antioxidant-build-system)
    (inputs (list (vitaminate/auto (p rust-serde-1))))
    (home-page "https://github.com/johnstonskj/rust-email_address.git")
    (synopsis
     "A Rust crate providing an implementation of an RFC-compliant `EmailAddress` newtype. ")
    (description
     "This package provides a Rust crate providing an implementation of an
RFC-compliant `EmailAddress` newtype. ")
    (license license:expat)))

(define rust-email-encoding ; for compatibility with new rust-lettre
  (package
    (name "rust-email-encoding")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "email-encoding" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06xljszqdicpys16jghyvr5199pqhjh3zjjl0jmmim94dnw1yzl2"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-base64" ,(p rust-base64-0.13))
		       ("rust-memchr" ,(p rust-memchr-2)))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,(p rust-pretty-assertions-0.7)))))
    (home-page "")
    (synopsis "Low level email encoding RFCs implementations")
    (description "Low level email encoding RFCs implementations")
    (license (list license:expat license:asl2.0))))

(define rust-pangocairo
  (package
    (inherit (@ (gnu packages crates-gtk) rust-pangocairo-0.9)) ; make it build
    (name "rust-pangocairo")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pangocairo" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rjk0clrjxah4kc0kybn7l7bxa5m5kpxkihxc2i7a6hx3xfa8xkq"))))))

(define rust-raw-window-handle ; required by new rust-winit@0.26
  (package
    (inherit (@ (gnu packages crates-graphics) rust-raw-window-handle-0.3))
    (name "rust-raw-window-handle")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "raw-window-handle" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hgvrqbr2b62zhq4ryv08h92mwis9v8f7j9pwcgxzlp7nswvw05q"))))))

(define rust-slotmap ; required by new rust-calloop
  (package
    (name "rust-slotmap")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slotmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hhkvsc3x79c7fh97b3padjhssd19hzdyyiv291mr3qf3lk8xq71"))))
    (build-system (@ (guix build-system cargo) cargo-build-system))
    (arguments
     `(#:cargo-inputs (("rust-serde" ,(p rust-serde-1))
		       ("rust-version-check" ,(p rust-version-check-0.9)))
       #:cargo-development-inputs
       (("rust-fxhash" ,(p rust-fxhash-0.2))
	("rust-quickcheck" ,(p rust-quickcheck-0.9))
        ("rust-serde" ,(p rust-serde-1))
        ("rust-serde-derive" ,(p rust-serde-derive-1))
        ("rust-serde-json" ,(p rust-serde-json-1)))))
    (home-page "https://github.com/orlp/slotmap")
    (synopsis "Slotmap data structure")
    (description "Slotmap data structure")
    (license license:zlib)))

(define rust-smithay-clipboard ; @0.6.2 doesn't build against new rust-smithay-client-toolkit
  (package
    (inherit (@ (gnu packages crates-graphics) rust-smithay-clipboard-0.6))
    (name "rust-smithay-clipboard")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smithay-clipboard" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s5hyhbmnk75i0sm14wy4dy7c576a4dyi1chfwdhpbhz1a3mqd0a"))))))

(define rust-smithay-client-toolkit ; @0.12 doesn't build against new rust-calloop
  (package
    (inherit (@ (gnu packages crates-graphics) rust-smithay-client-toolkit-0.12))
    (name "rust-smithay-client-toolkit")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smithay-client-toolkit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0m7l0zhl9s3321yj8z6hf1g0w3l2ay85irgcw2r5wwfj69yw81zk"))))))

(define rust-winit ; for compatibility against new rust-smithay-client-toolkit
  (package
    (inherit (@ (gnu packages crates-graphics) rust-winit-0.24))
    (name "rust-winit")
    (version "0.26.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "winit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fp7cdh7llbqmm6ga8f6bzk9785jmkbyy1w631hr9faq3n9wqhwv"))
	      (patches
	       (list (local-file "rust-winit-Update-smithay-client-toolkit.patch")))))))

(define rust-wl-clipboard-rs ; @0.4.1 incompatible with new rust-tree-magic-mini
  (package
    (inherit (p rust-wl-clipboard-rs-0.4))
    (name "rust-wl-clipboard-rs")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wl-clipboard-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12ydiayklrls2ys8ig2rljl86z21ixg672zhdsprnaiqpz8s6p5y"))))))

(define rust-x11rb ; required for compatibility with new rust-nix
  (package
    (inherit (@ (gnu packages crates-graphics) rust-x11rb-0.8))
    (name "rust-x11rb")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x11rb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12kdvzjfdbv9nkliq9lk6jd70f71q1qaiya24fjy4flbciavx6bf"))))))

(define rust-xml5ever
  (package
    (inherit (p rust-xml5ever-0.16)) ; @0.16 doesn't build against new rust-time
    (name "rust-xml5ever")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xml5ever" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l76v0c228c92sskiflpsy19c0bgc8q7flhlfanm32zrbb8f2d20"))))))

;; Some of these are only used for tests, cause cycles, ???,
;; so remove them.  (TODO: some of them can probably now be removed.)
;; TODO: write a "guix style" thing for doing this.
;;
;; "rust-foo": remove all dependencies named "rust-foo"
;; ("rust-foo" -> "rust-bar"): remove the dependency "rust-bar" when used by rust-foo
;; ("rust-foo" #:for-dependent ,p): remove the dependency "rust-foo when used by a package for which the predicate p returns 'true'

(define %removed-dependencies
  ;; Unconditional dependencies
  `("rust-derive-error-chain" ; doesn't build (even when updated) and unmaintained, avoid it for now
    "rust-crypto-tests" ; test dependency doesn't build against new rust-digest, avoid for now
    "rust-objc" "rust-objc-exception" "rust-objc-foundation" "rust-objc-id" "rust-objc-test-utils" ; requires gcc-objc, but gcc-objc doesn't seem to be used by any Rust crate, so it looks like these objc crates have never worked in the first place in Guix.
    "rust-avro-rs" ; doesn't compile against new rust-digest and longer maintained, replace by apache-avro, let's see if we can avoid it ...
    ("rust-quickcheck"
     ;; Avoid (quickcheck env-logger humantime chrono bincode) cycle
     ;; and some others (TODO: try compiling rust-quickcheck against dependencies
     ;; compiled without tests to break the cycle).
     #:for-dependent
     ,(lambda (dependent)
	(member (package-name dependent)
		'("rust-env-logger" "rust-humantime" "rust-chrono" "rust-bincode" "rust-byteorder"
		  "rust-regex" "rust-time" "rust-itertools" "rust-indexmap"))))
    "rust-pear" "rust-pear-codegen" ; current version in Guix requires non-stable
    "rust-mesalink" ; doesn't build against recent rust-rustls
    "rust-defmt" ; accidentally requires unstable-test?
    "rust-heapsize-plugin" ; makes use of removed features
    "rust-rustc-test" ; doesn't build against recent rust-time
    "rust-serde-hjson" ; doesn't build against new rust-serde
    ("rust-mio-uds" ; doesn't build against new rust-mio, now included in new rust-mio
     #:for-dependent
     ,(lambda (dependent)
	(not (string=? (package-name dependent) "rust-signal-hook-mio+old"))))
    "rust-speculate" ; @0.1.2 doesn't build against recent rust-syn
    "rust-skeptic" ; @0.13.4 doesn't build
    "rust-termbox-sys" ; downloads the library it wraps at compile time, cannot have work in Guix in the first place. 
    "rust-boxxy" ; doesn't build and not supposed to be used ‘in production’
    "rust-macrotest"
    ("rust-mio-extras" ; doesn't build against new rust-mio, so avoid it where possible.  Don't remove it unconditionally, because it's required by rust-notify@4 and rust-notify@4 is required by rust-watchexec
     #:for-dependent
     ,(lambda (dependent)
	(not (or (and (string=? "rust-notify" (package-name dependent))
		      (string-prefix? "4." (package-version dependent)))
		 (string=? "rust-alacritty-terminal" (package-name dependent))))))
    "rust-tokio-tls" ; @0.3.1 doesn't build
    "rust-rust-hawktracer-sys" ; only for tracing (debugging-only), so maybe the build failure can be avoided?
    "rust-ntest" "rust-ntest-test-cases" ; test-only, and @0.3.4 tries using non-exported syn::export
    "rust-afl" ; TODO: move to 'native-inputs'/development-inputs
    "rust-js-sys" ; TODO: guix doesn't support those targets (yet)
    "rust-lettre-email" ; current version doesn't build, and seems to have been merged into rust-lettre itself.
    "rust-cortex-m" ; ARM targets not yet supported for Rust in Guix
    ;;"rust-cc" ;; todo: build.rs, hence move to 'native-inputs'?
    "rust-stdweb" "rust-web-sys" ;; web, js, wasm?
    "rust-gloo-timers" ; web-only crate
    "rust-bencher" ; FTB
    "rust-criterion" "rust-criterion-cycles-per-byte" ;; fails to build because rust-async-log-attributes fails to build
    "rust-femme" ; some dependencies fail to build
    ("rust-ptree" -> "rust-config") ; only required by disabled "config" feature
    ("rust-ptree" -> "rust-directories") ; likewise
    "rust-proptest" "rust-proptest-derive"
    "rust-futures-util-preview" ; futures-util has been updated?
    "rust-iron" ; its dependency rust-hyper-native-tls fails to build
    "rust-rocket" ; its dependency rust-hyper-sync-rustls fails to build
    "rust-nickel" ; fails to build
    "rust-kqueue-sys" "rust-kqueue" "rust-errno-dragonfly" ;; TODO: BSD not supported
    "rust-pdcurses-sys" ; bundles pdcurses, maybe actually unused?, see #56031
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
    "rust-cloudabi" ; doesn't seem like a supported target
    "rust-winapi" "rust-winutil" "rust-winapi-wsapoll" "rust-kernel32-sys" "rust-user32-sys" "rust-winreg" "rust-wepoll-sys" "rust-wepoll-sys-stjepang" "rust-ipconfig" "rust-windows" "rust-windows-x86-64-msvc" "rust-windows-x86-64-gnu" "rust-windows-i686-msvc" "rust-windows-i686-gnu" "rust-windows-aarch64-msvc" "rust-windows-sys" "rust-windows-gen" "rust-windows-macros" ; skip Windows support for now
    "rust-mio-anonymous-pipes" ; also Windows-specific
    "rust-nodrop-union" ; required unstable, and deprecated
    "rust-sleef-sys" ; requires unstable
    "rust-packed-simd" "rust-packed-simd-2" ; requires unstable (TODO: rust-packed-simd-2?)
    "rust-security-framework" "rust-cocoa" "rust-cocoa-foundation" "rust-core-foundation" "rust-core-foundation-sys" "rust-core-text" "rust-fsevent" "rust-fsevent-sys" "rust-core-video-sys" "rust-core-graphics" "rust-core-graphics-types" "rust-objc-foundation" "rust-security-framework-sys" "rust-readkey" "rust-cgl" "rust-dispatch" ; non-Linux, non-Hurd things,
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
    "rust-scratch" ; appears to be incompatible with antioxidant's compilation model without shenanigans
    "rust-tokio-core" ;; doesn't exist in recent tokios
    "rust-tokio-process" ;; doesn't exist in recent tokios
    "rust-tokio-executor" ;; doesn't exist in recent tokios, I think?
    "rust-tokio-io" ;; doesn't exist in recent tokios, I think?
    ("rust-lazy-static" -> "rust-spin") ; rust-spin is only needed in a no-std environment, also somewhat deep dependencies for basic functionality
    "rust-version-sync"
    "rust-trybuild"
    "rust-clang-ast-test-suite" ; empty, doesn't build
    "rust-cxx-test-suite" ; likewise
    "rust-clippy"
    "rust-cargo" "rust-cargo-c" ; rust-cargo doesn't build even when updated to @0.62
    "rust-tokio-mock-task" ; doesn't build
    "rust-tokio-test"
    "rust-rand-xorshift"
    "rust-serde-test"
    "rust-actix-testing" ; doesn't build
    "rust-actix-connect" ; doesn't build and no update available
    ("rust-xml5ever" -> "rust-time") ; new version doesn't use rust-time anymore
    "rust-wasm-bindgen" "rust-wasi"
    "rust-wasm-bindgen-futures" ; ECMAScript-only and doesn't build
    "rust-wasm-bindgen-test"
    ("rust-x11rb" -> "libloading") ; no need for the fragile dlopen

    ("alacritty" -> "rust-time") ; not needed anymore
    ("rust-average" -> "rust-rand-distr") ; test cycle?
    ("rust-cxxbridge-macro" -> "rust-cxx") ; test cycle?
    ;; Maybe a test or example cycle?
    ("rust-bytemuck-derive" -> "rust-bytemuck")
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
    ("rust-actix-web-codegen" -> "rust-actix-web")
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
    ("rust-regex-automata" -> "rust-bstr")

    ("rust-page-size" -> "rust-spin") ; not required because no_std isn't enabled
    ;; TODO: quickcheck with an exception for sequoia-pg

    ;; TODO: untested removals
    "rust-riscv" ; maybe optional, also riscv is not yet supported by Rust in Guix
    ("rust-shadow-rs" -> "rust-git2") ; optional, also unused in Guix (there's no git repo in the build environment)

    ;; These probably only add the non-Rust dependencies because of cargo-build-system limitations,
    ;; so clean them up. (TODO: untested)
    ("drill" -> "openssl")
    ("drill" -> "pkg-config")
    ("exa" -> "pkg-config")
    ("exa" -> "libgit2")
    ("exa" -> "zlib")
    ("fd" -> "jemalloc")
    ("i3status-rust" -> "pkg-config")
    ("ripgrep" -> "pcre2")
    ("ripgrep" -> "pkg-config")
    ("git-interactive-rebase-tool" -> "zlib")
    ("sniffglue" -> "libpcap")
    ("sniffglue" -> "libseccomp") ; TODO: check that the Rust seccomp crate properly loads libseccomp
    ("spotify-tui" -> "pkg-config")
    ("spotify-tui" -> "openssl")
    ("tectonic" -> "pkg-config")
    ("tectonic" -> "fontconfig")
    ("tectonic" -> "freetype")
    ("tectonic" -> "graphite2")
    ("tectonic" -> "harfbuzz")
    ("tectonic" -> "icu4c")
    ("tectonic" -> "libpng")
    ("tectonic" -> "openssl")
    ("tectonic" -> "zlib")
    ("tokei" -> "zlib")
    ("tokei" -> "pkg-config")
    ("tokei" -> "libgit2")
    ("tokei" -> "openssl")
    ("tealdeer" -> "pkg-config")
    ("tealdeer" -> "openssl")
    ("git-absorb" -> "zlib")
    ;; LIkewise
    ("castor" -> "pkg-config")
    ("castor" -> "atk")
    ("castor" -> "cairo")
    ("castor" -> "gdk-pixbuf")
    ("castor" -> "gtk+")
    ("castor" -> "libressl")
    ("castor" -> "pango")
    ("monolith" -> "pkg-config")
    ("monolith" -> "openssl")

    ("rust-vcpkg" ; for Windows, usually not needed elsewhere
     #:for-dependent
     ,(lambda (dependent)
	;; TODO: which packages require this?
	(not (member (package-name dependent) '()))))))

(define (remove-dependency? dependent dependency)
  "Should DEPENDENCY be removed from the dependencies of DEPENDENT (both are package objects)?"
  (unless (package? dependent)
    (error "first argument must be a package"))
  (unless (package? dependency)
    (error "second argument must be a package"))
  (define dependent* (package-name dependent))
  (define dependency* (package-name dependency))
  (define remove-dependency*?
    (match-lambda
      ((? string? unconditional-removal) (string=? dependency* unconditional-removal))
      (((? string? context) '-> (? string? conditional-removal))
       (and (string=? dependent* context)
	    (string=? dependency* conditional-removal)))
      (((? string? conditional-removal?) '#:for-dependent (? procedure? context?))
       (and (string=? dependency* conditional-removal?)
	    (context? dependent)))
      (a (pk 'a a) (error "bogus entry in %removed-dependencies"))))
  (any remove-dependency*? %removed-dependencies))

(define %crate-types ; resolve multiple crate types inside the Cargo.toml.
  `(("rust-hyper" ,#~"rlib")
    ("rust-jsonpath-lib" ,#~"rlib")
    ("rust-neso" ,#~"rlib")
    ("parinfer-rust" ,#~"rlib"))) ; TODO: delete the compiled Rust library, only the binary needed!  Also, test if it actually works.

;; Try keeping things sorted, to avoid rebase/merge conflicts.
(define %features
  `(("rust-arrow2" ,#~'("default" "compute" "io_csv" "io_ipc" "io_parquet" "io_json")) ; compute is required by rust-polars-core; "io_csv", "io_ipc", "io_parquet", "io_json" and by rust-polars-io
    ("rust-arrow-format" ,#~'("default" "ipc")) ; "ipc" required by "ipc" feature of "rust-arrow2"
    ;; rust-swayipcs requires 'spawn_blocking' which is only
    ;; public if "unstable" is enabled.
    ("rust-async-std" ,#~'("default" "unstable"))
    ("rust-actix-tls" ,#~'("default" "uri")) ; rust-awc@3.0.0 requires non-default "uri" feature
    ("rust-actix-http" ,#~'("default" "ws" "http2" "compress-gzip" "compress-zstd")) ; ws, http2, compress-gzip, compress-zstd: required by rust-awc
    ("rust-awc" ,#~'("compress-gzip" "compress-zstd" "cookies")) ; default "compress-brotli" feature requires the "compress-brotli" feature in rust-actix-http but that doesn't build
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
    ("rust-blake3" ,#~'("default" "rayon")) ; new b3sum requires rayon.
    ("rust-bv" ,#~'("serde")) ; there are no default features.  Enable "serde" required by rust-bio@0.33
    ("rust-bigdecimal" ,#~'("serde")) ; "serde" required by rust-nu-protocol
    ("rust-bson" ,#~'("default" "chrono-0_4")) ; chrono-0_4: required by rust-nu-plugin-from-bson@0.44.
    ("rust-bstr" ,#~'("default" "serde1")) ; serde1: required by rust-git-glob
    ;; the default "generic-simd" feature required rust-packed-simd
    ;; which is currently uncompilable.
    ("rust-bytecount" ,#~'())
    ("rust-bzip2" ,#~'("futures")) ; "tokio" requires old tokio-io
    ("rust-cairo-sys-rs"
     ;; "dox" disables actually linking to the cairo library, which breaks the build
     ;; of dependents.
     ,#~'("v1_16" "xlib" "png" "pdf" "svg" "ps" "freetype" "script" "xcb" "use_glib"))
    ;; "alloc" is required by some crates. serde is required by alfis
    ("rust-chrono" ,#~'("default" "alloc" "serde"))
    ("rust-chrono-tz" ,#~'("default" "case-insensitive")) ; doesn't build without "case-insensitive"
    ;; zeroize required by rust-ctr
    ("rust-cipher" ,#~'("alloc" "std" "block-padding" "rand_core" "dev" "zeroize"))
    ;; Don't just support libclang 3.5, also include
    ;; bindings for later versions which rust-bindgen might
    ;; need.  Don't include the "runtime" feature, because
    ;; then libclang.so needs to be in LD_LIBRARY_PATH or such
    ;; to be found.  Don't include the "static" feature for
    ;; the standard reasons against static linking in Guix.
    ("rust-clang-sys" ,#~'("clang_10_0")) ; Guix by default does dynamic linking, not static linking, which would use the "static" feature IIUC
    ("rust-command-group" ,#~'("default" "with-tokio")) ;; with-tokio required by rust-watchexec
    ("rust-config" ,#~'("toml" "json" "yaml" "ini" "ron")) ; default json5 feature required pcackage not in Guix.
    ;; This addresses the build failure
    ;; ‘could not find `collector` in the crate root’
    ;; and ‘cannot find function `pin` in crate `epoch`’
    ("rust-crossbeam-epoch"
     ,#~'("std" "alloc"))
    ;; rust-cipher requires non-default rand_core
    ("rust-crypto-common" ,#~'("std" "rand_core"))
    ("rust-cssparser" ,#~'()) ; don't enable the non-stable "bench" feature
    ("rust-darling-core" ,#~'("strsim")) ; don't enable the unstable "diagnostics" feature
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
    ("rust-firestorm" ,#~'()) ; avoid optional dependencies for probably unused profiling for now
    ;; Avoid extra dependencies by using the C library that
    ;; is used elsewhere anyway.
    ("rust-flate2" ,#~'("zlib"))
    ("rust-futures-core"
     ,#~'("std" "alloc"))
    ("rust-futures-channel"
     ,#~'("std" "alloc" "sink")) ; sink is required by rust-warp@0.3
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
    ("rust-glutin" ,#~'("default" "serde")) ; required by alacritty
    ("rust-gobject-sys" ,#~'("v2_68")) ; likewise
    ("rust-gtk" ,#~'("v3_24_9")) ; likewise (for dox)
    ("rust-gtk-sys" ,#~'("v3_24_11")) ; likewise (for dox)
    ("rust-integer-encoding" ,#~'("futures_async")) ; the features are mutually exclusive, so enable only the futures_async feature required by rust-parquet-format-async-temp.
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
    ("rust-lazy-static" ,#~'()) ; don't enable "spin_no_std", that way we don't have to depend on 'rust-spin' and it's only needed in no-std environments
    ("rust-lexical-util" ,#~'("default" "parse-integers" "write-integers" "floats")) ;; enable features required by various rust-lexical-... crates
    ;; extra-traits is required by rust-nix
    ("rust-libc" ,#~'("std" "extra_traits"))
    ("rust-libnghttp2-sys" ,#~'()) ; don't enable the "vendored" feature
    ("rust-lock-api" ,#~'("default" "nightly")) ; "nightly" is required by rust-yansi
    ;; Required by rust-env-logger.
    ;; kv_unstable is required by rust-kv-log-macro.
    ;; "serde" is required by rust-alacritty
    ("rust-log" ,#~'("std" "kv_unstable" "serde")) ;
    ("rust-lzma-sys" ,#~'()) ; don't enable "static" (TODO: add it to the list in antioxidant?)
    ;; Required by rust-tokio
    ;; TODO remove os-poll, as implied features are implemented.
    ("rust-mio"
     ,(lambda (original-package)
	;; set of available features depends on the version
	(if (string-prefix? "0.6." (package-version original-package))
	    #~'("default")
	    #~'("net" "os-ext" "os-poll"))))
    ("rust-multipart"
     ;; default "iron" feature requires rust-iron which currently fails to build.
     ;; Likewise for "nickel".
     ;; The "hyper" feature is incompatible with rust-hyper>0.11
     ,#~'("client" "mock" "server" "tiny_http"))
    ("rust-nalgebra" ,#~'("default" "rand")) ; rust-statrs@0.13 required "rand"
    ;; The non-default feature "alloc" is required by rust-pure-rust-locales.
    ("rust-nom"
     ,#~'("std" "lexical" "alloc"))
    ("rust-numtoa" ,#~'("std"))
    ;; rust-rsa requires "prime" and "zeroize"
    ("rust-num-bigint-dig" ,#~'("default" "prime" "zeroize"))
    ("rust-num-bigint" ,#~'("default" "serde")) ; "serde" is required by rust-nu-protocol
    ("rust-num-rational" ,#~'("default" "serde")) ; 'serde' is required by rust-mp4@0.9.2
    ;; rust-x509-parser required 'crypto' and 'x509'
    ("rust-oid-registry" ,#~'("default" "crypto" "x509"))
    ("rust-once-cell" ,#~'("default" "race")) ; "race" is required by rust-git-tempfile@2.0.1
    ("rust-openssl-sys" ,#~'()) ;; avoid the 'vendored' feature
    ("rust-os-str-bytes" ,#~'("raw"))
    ("rust-pango" ,#~'("v1_46")) ; "dox" feature requires non-stable
    ("rust-pango-sys" ,#~'("v1_46")) ; likewise
    ;; The 'backtrace' and 'petgraph' dependency has been removed.
    ;; (including petgraph causes a cycle between rust-ahash and rust-hashbrown,
    ;; but it's ‘only’ required for deadlock detection).
    ;;
    ;; 'nightly' is required by rust-yansi's tests.
    ("rust-parking-lot-core" ,#~'("nightly"))
    ;; "quickcheck" features requires removed crate "quickcheck"
    ("rust-partial-io" ,#~'("futures03" "tokio1"))
    ("rust-polars-core" ,#~'("default" "dtype-time" "dtype-date" "dtype-datetime" ; "dtype-time", "dtype-datetime" and "dtype-date" are required by rust-polars-io's csv support
			     "performant" ; requested by rust-nu-protocol via rust-polars, and the downside (panic in case of incorrect usage) seems mild
			     "pivot" "downsample" "is_in" "rolling_window" "random" ; required by rust-nu-command
			     "object" "checked_arithmetic" ; indirectly required by rust-nu-protocol
			     "serde" "strings" ; required by rust-nu-protocol
			     "lazy" "private" "zip_with")) ; required by rust-polars-lazy
    ("rust-phf-shared" ,#~'("default" "uncased")) ; uncased required by rust-phf's uncased feature
    ("rust-phf" ,#~'("default" "macros" "uncased")) ; "macros" is required by rust-cssparser@0.28, "uncased" is required by a dependency of "tokei"
    ;; Required by 'sniffglue'
    ("rust-pktparse" ,#~'("serde"))
    ("rust-plotters-svg" ,#~'()) ; "debug" feature causes a build failure
    ("rust-postgres-types" ,#~'("derive" "with-time-0_3" "with-uuid-0_8" "geo-types-0_7")) ; not all  dependencies of all features have been packaged yet.
    ("rust-proc-macro2"
     ;; span-locations is required by rust-cxx-gen@0.7.49
     ,#~'("default" "span-locations"))
    ("rust-ptree"
     ,#~'("petgraph" "ansi" "value")) ; default "config" feature doesn't build
    ;; Without "getrandom" or "alloc", it fails to build (TODO upstream?).
    ;; small_rngs is required by rust-phf-generator.
    ("rust-rand"
     ,#~'("std" "std_rng" "getrandom"
	  "alloc" "small_rng"))
    ("rust-raw-window-handle" ,#~'()) ; don't enable nightly "nightly-docs"
    ("rust-reqwest" ,#~'("default" "blocking" "cookies" "json")) ; tealdeer@1.4.1 requires "blocking" to build, drill requires cookies, rbw requires json
    ;; The 'inline-asm' feature requires non-stable
    ("rust-riscv" ,#~'())
    ("rust-rusqlite" ,#~'()) ; some features are only for bundling or require extra configure flags(?) for sqlite.  For now, don't enable anything. 
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
    ("rust-serde" ,#~'("std" "alloc" "derive"
		       "rc")) ; rc: required by rust-nu-protocol
    ("rust-servo-fontconfig-sys" ,#~'("force_system_lib")) ; be extra sure the bundled copy isn't used
    ("rust-shadow-rs" ,#~'()) ; no need for the 'git2' feature in Guix, which brings in lots of dependents
    ;; Avoid "digest_trait" which requires old rust-digest@0.9.0
    ("rust-sha1collisiondetection" ,#~'("std" "structopt"))
    ("rust-similar" ,#~'("default" "text" "inline"))
    ("rust-text-size" ,#~'("serde")) ; Has no default features. Enable "serde", which is required by rust-rowan@1.15.2
    ("rust-derive-builder" ,#~'()) ; for now don't build the non-building test features
    ("rust-safe-arch" ,#~'("bytemuck")) ; there are no default features.  Enable "bytemuck", requires by rust-wide@0.6.5
    ("rust-selectors" ,#~'()) ; "bench" feature requires non-stable
    ("rust-serde-json" ,#~'("default"
			    "preserve_order" ; presumably comes with a small time or space cost, but it seems safe and potentially unsafe to disable, depending on the dependent
			    "float_roundtrip" ; seems safe, albiet with & 2× performance cost according to the documentation. We don't know which dependents need it and which ones don't, so just enable it to be safe.
			    "arbitrary_precision" ; seems safe (at worst a compile error in a dependent?), and we don't know which dependents need it and which ones don't.
			    "unbounded_depth")) ; add a method disable_recursion_limit that is used by rust-cargo-metadata
    ;; "nested-values" is required by the "nested-values" feature of rust-slog-term
    ("rust-slog" ,#~'("default" "nested-values"))
    ("rust-smallvec" ,#~'("serde" "write")) ; "serde", "serde": required by rust-git-object
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
    ("rust-libssh2-sys" ,#~'()) ;; Setting zlib-ng-compat will make build.rs complain because apparently it could (on other systems) indirectly cause both a bundled and a non-bundled libssl to be loaded.  But we don't do bundling in Guix.  Anyway, in antioxidant, setting zlib-ng-compat only changes error reporting in build.rs, no runtime behaviour changes.  (In Cargo, it would cause a variant of the zlib library to be used)
    ("rust-libgit2-sys" ,#~'("ssh" "https")) ; don't enable vendoring
    ("rust-lsp-types" ,#~'("default" "proposed")) ; "proposed" is required by kak-lsp
    ("rust-page-size" ,#~'()) ; no_std feature required removed feature
    ("rust-smithay-client-toolkit" ,#~'("calloop")) ; don't enable the "dlopen" feature because directly linking works fine and is less fragile.
    ("rust-snafu-derive" ,#~'()) ; unstable-backtraces-impl-std requires unstable
    ("rust-tower" ,#~'("default"
		       ;; features used by rust-tonic
		       "balance" "buffer" "discover" "limit" "load" "make" "timeout" "util"))
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
    ("rust-tracing-subscriber"
     ,#~'("default" "env-filter")) ; env-filter is required by rust-chalk-solve@0.75
    ("rust-tree-magic-mini" ,#~'()) ; Don't enable "with-gpl-data". We don't mind GPL in Guix, but enabling this feature causes rust-tree-magic-mini to look for a bundling crate, which will not be packaged in Guix.  Instead, disable that feature and adjust the ‘runtime’ code to look at the data with a baked-in reference to freedesktop's magic data.
    ;; dns-over-openssl is required by rust-trust-dns-openssl.
    ;; dns-over-native-tls is required by rust-trust-dns-native-tls.
    ;; dns-over-rustls is required by rust-trust-dns-rustls.
    ;; dns-over-https is required by rust-trust-dns-https.
    ("rust-trust-dns-proto"
     ,#~'("default" "dns-over-openssl" "dns-over-native-tls" "dns-over-rustls"
	  "dns-over-https"))
    ("rust-tui" ,#~'("default" "crossterm")) ; required by rust-nu-plugin-chart
    ;; For now avoid optional dependencies
    ("rust-typenum" ,#~'())
    ("rust-url" ,#~'("default" "serde")) ; serde is required by rust-lsp-types@0.80
    ("rust-uuid" ,#~'("default" "serde" "v4")) ; v4,serde required by alfis
    ("rust-value-bag" ,#~'("std"))
    ("rust-v-frame" ,#~'("serialize")) ; wasm doesn't build, tracing seems unnecessary
    ("rust-wayland-protocols" ,#~'("client" "server" "unstable_protocols" "staging_protocols")) ; unstable-protocols is required by (TODO: forgot which one).  staging_protocols is required by rust_winit.  TODO: bundles wayland protocol things.
    ("rust-wayland-sys" ,#~'("client" "cursor" "egl" "server")) ; don't enable the dlopen feature, dlopen(...) is somewhat fragile and RUNPATH works just fine
    ("rust-wayland-client" ,#~'("use_system_lib")) ; don't use the dlopen feature, which is fragile and unneeded in Guix.  system_lib is required by rust-wayland-egl.
    ("rust-webpki" ,#~'("std" "alloc"))
    ("rust-winit" ,#~'("x11" "wayland" "serde")) ; don't enable the default wayland-dlopen feature, because it's not necessary and fragile in Guix. "serde" is required by alacritty
    ("rust-xcb" ,#~'("thread" "xfixes")) ; not all features build, for now only enable features required by rust-x11-clipboard.
    ("rust-xz2" ,#~'("futures")) ; ???
    ("rust-x11" ,#~'("dpms" "xcursor" "xf86vmode" "xft" "xinerama" "xinput" "xlib" "xlib_xcb" "xmu" "xrandr" "xrecord" "xrender" "xss" "xt" "xtest" "xtst")) ; disable the "dox" feature, which causes build.rs to not actually look for the x11 library.  TODO: glx requires gl.pc, maybe some dependent requires that ...
    ("rust-x11rb" ,#~'("allow-unsafe-code" "cursor" "image" "resource_manager" "all-extensions")) ; disable libloading/dlopen, which is fragile and unnecessary
    ;; rust-rcgen requires "time". While at it, enable other
    ;; features as well.
    ("rust-yasna" ,#~'("default" "time" "bit-vec" "bigint" "std"))
    ;; rust-num-bigint-dig's zeroize feature requires the "derive"
    ;; feature of rust-zeroize
    ("rust-zeroize" ,#~'("default" "derive"))
    ("rust-zip" ,#~'("bzip2" "deflate" "time" "zstd")) ; avoid default "aes-crypto" feature, which requiers an old rust-aes (and encrypted zips aren't used often anyways)
    ("rust-zstd-safe" ,#~'("default" "std")))) ; std is reaquired by rust-zstd@0.9.0

(define %replacements
  `(("rust-approx" ,(p rust-approx-0.5)) ; resolve version conflict
    ("rust-alacritty-terminal" ,rust-alacritty-terminal) ; resolve build failure
    ("rust-atk-sys" ,(@ (gnu packages crates-gtk) rust-atk-sys-0.14)) ; @0.10 doesn't build
    ("rust-aho-corasick" ,(p rust-aho-corasick-0.7)) ; avoid version conflict
    ("rust-average" ,(p rust-average-0.13)) ; avoid complication due to multiple versions
    ("rust-blake3" ,(@ (gnu packages crypto) rust-blake3-1)) ; @0.3.8 doesn't build against new crypto libraries
    ("rust-buffering" ,(p rust-buffering-0.4)) ; @0.3 doesn't build
    ("rust-buffering-nocopy-macro" ,(p rust-buffering-nocopy-macro-0.2)) ; @0.1 doesn't build
    ("rust-calloop" ,rust-calloop)
    ("rust-cairo-rs" ,(@ (gnu packages crates-gtk) rust-cairo-rs-0.14)) ; @0.8.1 doesn't build
    ("rust-cbindgen" ,(package-with-extra-patches
		       rust-cbindgen-0.19
		       ;; Replace Cargo-specific assumptions by antioxidant-specific
		       ;; assumptions (we cannot run "cargo metadata"!).
		       ;;
		       ;; rust-cbindgen tries to run "cargo metadata" to generate a JSON
		       ;; file with some information on the package and its dependencies,
		       ;; but fails, resulting in an unclear error message.
		       ;;
		       ;; As antioxidant isn't cargo, we have to replace this logic by
		       ;; some other logic, generating the metadata ourselves.  TODO:
		       ;; actually implement that generate-cbindgen-metadata phase.
		      (list (local-file "rust-cbindgen-0.19-antioxidant-compatibility.patch"))))
    ("rust-comfy-table" ,(p rust-comfy-table-4)) ; @1 doesn't build against new dependencies
    ("rust-config" ,rust-config)
    ("rust-crossterm" ,rust-crossterm) ; @0.19 doesn't build against new rust-signal-hook
    ("rust-ctrlc" ,rust-ctrlc)
    ("rust-darling-core" ,(p rust-darling-core-0.13)) ; @0.9 incompatible with new rust-syn
    ("rust-derive-builder" ,(p rust-derive-builder-0.10) ; @0.7.2 has failing dependencies
     #:for-dependent
     ,(lambda (dependent)
	(not (member (package-name dependent) '("skim"))))) ; needs @0.9
    ("rust-derive-builder" ,(p rust-derive-builder-0.9)
     #:for-dependent
     ,(lambda (dependent)
	(member (package-name dependent) '("skim")))) ; needs @0.9
    ("rust-dirs" ,(p rust-dirs-3)) ; avoid version conflict in tectonic
    ("rust-emacs-macros" ,rust-emacs-macros)
    ("rust-email" ,rust-email)
    ("rust-gio" ,(@ (gnu packages crates-gtk) rust-gio-0.14)) ; @0.8.1 doesn't build
    ("rust-dlib" ,rust-dlib) ; old rust-dlib and new rust-smithay-client-toolkit are incompatible
    ("rust-gtk-sys" ,(@ (gnu packages crates-gtk) rust-gtk-sys-0.14)) ; @0.10 doesn't build
    ("rust-getrandom" ,(p rust-getrandom-0.2)) ; avoid multiple versions
    ("rust-hdrhistogram" ,rust-hdrhistogram)
    ("rust-hmac-sha1" ,(package-with-extra-patches
			(p rust-hmac-sha1-0.1)
			(list (local-file "rust-hmac-sha1-update-dependencies.patch")))) ; compatibility with new dependencies
    ("rust-hostname" ,(p rust-hostname-0.3)) ; rust-lettre requires new version
    ("rust-h2" ,rust-h2)
    ("rust-rand-core" ,(p rust-rand-core-0.6)) ; avoid multiple versions
    ("rust-blake2" ,rust-blake2)
    ("rust-actix" ,rust-actix)
    ("rust-actix-codec" ,rust-actix-codec)
    ("rust-actix-http" ,rust-actix-http)
    ("rust-actix-utils" ,rust-actix-utils)
    ("rust-actix-router" ,rust-actix-router)
    ("rust-actix-service" ,rust-actix-service)
    ("rust-awc" ,rust-awc)
    ("rust-chacha20poly1305" ,rust-chacha20poly1305)
    ("rust-lettre" ,rust-lettre)
    ("rust-libsqlite3-sys" ,(p rust-libsqlite3-sys-0.23)) ; custom phase assumes != @0.20
    ("rust-libgit2-sys" ,(p rust-libgit2-sys-0.12)) ; old version doesn't build.
    ("rust-markup5ever" ,(p rust-markup5ever-0.9)) ; @0.9 doesn't build against new rust-phf-... without patches, but we still need it because monolith doesn't support the new rust-markup5ever@0.10 yet
    ("rust-meval" ,(package-with-extra-patches
		    (package
		     (inherit (p rust-meval-0.2))
		     ;; Update to latest git such that the patch applies.
		     (source
		      (origin
		       (method git-fetch)
		       (uri
			(git-reference
			 (url "https://github.com/rekka/meval-rs")
			 (commit "ac9586fb19e1d6fb505425dbbc9598f372122130")))
		       (sha256 "18554xrhdl0lyga408l01yjhilh69qxkjyyss6mlpxypdwy6cf7w"))))
		    (list (local-file "rust-meval-update-dependencies.patch"))))
    ("rust-miniz-oxide" ,(p rust-miniz-oxide-0.4) ; avoid multiple versions
     #:for-dependent
     ,(lambda (dependent)
	(not (equal? (list (package-name dependent) (package-version dependent))
		     '("rust-png" "0.16.8"))))) ; doesn't build rust-miniz-oxide@0.4
    ("rust-newtype-derive" ; TODO: can be merged in upstream Guix
     ,(package-with-extra-patches (p rust-newtype-derive-0.1)
				  (list (local-file "rust-newtype-derive-Update-dependencies.patch"))))
    ("rust-num-bigint" ,(p rust-num-bigint-0.4)) ; avoid multiple versions
    ("rust-num" ,(p rust-num-0.4)) ; avoid multiple versions (TODO: let the CI test if it doesn't cause build failures)
    ("rust-num-complex" ,(p rust-num-complex-0.4)) ; avoid multiple versions (TODO: let the CI test if it doesn't cause build failures)
    ("rust-polars-core"
     ,(package-with-extra-patches
       (p rust-polars-core-0.17)
       (list (local-file "rust-polars-core-Update-rand.patch"))))
    ("rust-arrayvec" ,(p rust-arrayvec-0.7) ; avoid multiple versions
     #:for-dependent
     ,(lambda (dependent)
	(not (string=? (package-name dependent) "rust-vte"))))
    ("rust-arrayvec" ,(package-with-rust-features (p rust-arrayvec-0.5)
						  #~'("default"))
     #:for-dependent
     ,(lambda (dependent)
	(string=? (package-name dependent) "rust-vte"))) ; still required old rust-arrayvec
    ("rust-bitstream-io" ,(p rust-bitstream-io-1)) ; avoid multiple versions
    ("rust-bytestring" ,rust-bytestring)
    ("rust-avif-serialize" ,rust-avif-serialize)
    ("rust-nasm-rs" ,rust-nasm-rs)
    ("rust-notify" ,(p rust-notify-5) ; use new version where possible, and the old where still required
     #:for-dependent
     ,(lambda (dependent)
	(not (member (package-name dependent) '("rust-watchexec" "alacritty")))))
    ("rust-notify" ,(p rust-notify-4)
     #:for-dependent
     ,(lambda (dependent)
	(member (package-name dependent) '("rust-watchexec" "alacritty"))))
    ("rust-num-rational" ,(p rust-num-rational-0.4)) ; @0.1 doesn't build when "serde" is enabled
    ("rust-ivf" ,rust-ivf)
    ("rust-idna" ,(p rust-idna-0.2)) ; avoid multiple versions
    ("rust-siphasher" ,(p rust-siphasher-0.3)) ; avoid multiple versions
    ("rust-statrs" ,(p rust-statrs-0.14)) ; @0.13 doesn't build
    ("rust-syslog" ,rust-syslog)
    ("rust-clap-derive" ,rust-clap-derive)
    ("rust-askama-shared" ,rust-askama-shared)
    ("rust-askama-derive" ,rust-askama-derive)
    ("rust-zstd" ,(p rust-zstd-0.9)) ; @0.6 doesn't build a dependency failing to build
    ("rust-rand-distr" ,(p rust-rand-distr-0.4)) ; avoid complications due to multiple versions
    ("rust-reqwest" ,rust-reqwest) ; @0.10 has
    ("rust-s3handler" ,rust-s3handler)
    ("rust-cookie-store" ,rust-cookie-store) ; fix failing build by updating
    ("rust-cookie-store-15" ,rust-cookie-store)
    ("rust-signal-hook-mio"
     ,(package-with-rust-features rust-signal-hook-mio
				  #~'("support-v0_8")
				  #:name "rust-signal-hook-mio")
     #:for-dependent
     ,(lambda (dependent)
	(not (member (package-name dependent) '("rust-alacritty-terminal")))))
    ("rust-signal-hook-mio"
     ,(package-with-rust-features rust-signal-hook-mio
				  #~'("support-v0_6")
				  #:name "rust-signal-hook-mio+old"
				  #:rust-metadata "guix-variant=support-v0_6")
     #:for-dependent ; rust-alacritty-terminal needs the old rust-mio for now
     ,(lambda (dependent)
	(member (package-name dependent) '("rust-alacritty-terminal"))))
    ("rust-structopt" ,(p rust-structopt-0.3))
    ("rust-structopt-derive" ,(p rust-structopt-derive-0.4)) ; @0.2.18 doesn't build
    ("rust-tectonic-bridge-core" ,(p rust-tectonic-bridge-core-0.3)) ; to keep custom phases simple, only use a single version
    ("rust-tectonic-status-base" ,(p rust-tectonic-status-base-0.2)) ; resolve version conflict
    ("rust-tectonic-errors" ,(p rust-tectonic-errors-0.2)) ; resolve version conflict
    ("rust-tectonic-io-base" ,(p rust-tectonic-io-base-0.4)) ; resolve version conflict
    ("rust-totp-lite" ,rust-totp-lite)
    ("rust-tree-magic" ,rust-tree-magic-mini) ; for compatibility with new rust-nom
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
    ("rust-postgres-protocol" ,rust-postgres-protocol)
    ("rust-postgres-types" ,rust-postgres-types)
    ("rust-regex" ,(p rust-regex-1)) ; old version doesn't build against new rust-aho-corasick
    ("rust-regex-syntax" ,(p rust-regex-syntax-0.6)) ; multiple version
    ;; swayhide requires non-async to build
    ("rust-swayipc" ,(package-with-rust-features (p rust-swayipc-2)
						 #~'()
						 #:name "rust-swayipc+sync"
						 #:rust-metadata "guix-variant=sync")
     #:for-dependent
     ,(lambda (dependent)
	(string=? "swayhide" (package-name dependent))))
    ;; Use the newest version of rust-mio where possible,
    ;; except for packages that still require an old rust-mio
    ;; and where updating the package is difficult for now.
    ("rust-mio" ,rust-mio
     #:for-dependent
     ,(lambda (dependent)
	(not (or (string=? (package-name dependent) "rust-mio-extras")
		 (string=? (package-name dependent) "rust-mio-uds")
		 (string=? (package-name dependent) "rust-alacritty-terminal")
		 (string=? (package-name dependent) "rust-signal-hook-mio+old")
		 (and (string=? (package-name dependent) "rust-notify")
		      (string-prefix? "4." (package-version dependent)))))))
    ("rust-mio" ,(package-with-rust-features (p rust-mio-0.6)
					     #~'("default")) ; not used, see %features
     #:for-dependent
     ,(lambda (dependent)
	(or (string=? (package-name dependent) "rust-mio-extras")
	    (string=? (package-name dependent) "rust-mio-uds") ; requires old rust-mio
	    (string=? (package-name dependent) "rust-alacritty-terminal")
	    (string=? (package-name dependent) "rust-signal-hook-mio+old")
	    (and (string=? (package-name dependent) "rust-notify")
		 (string-prefix? "4." (package-version dependent))))))
    ("rust-signal-hook-mio" ,rust-signal-hook-mio)
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
    ("rust-semver" ,(package-with-extra-patches
		     (p rust-semver-1)
		     (list (local-file "rust-semver-Add-increment-foo-again.patch"))))
    ("rust-rustc-version" ,(p rust-rustc-version-0.4)) ; @0.2.3 doesn't build against rust-semver@1
    ("rust-dotenv" ,(p rust-dotenv-0.15)) ; @0.10 doesn't build
    ("rust-quickcheck-macros" ,(p rust-quickcheck-macros-1)) ; 0.9 doesn't build against rust-syn@1
    ("rust-quick-xml" ,(p rust-quick-xml-0.22) ; resolve version conflict
     #:for-dependent
     ,(lambda (dependent)
	(not (member (package-name dependent) '("rust-calamine")))))
    ("rust-quick-xml"
     ,(package-with-rust-features (p rust-quick-xml-0.22)
				  #~'("default" "encoding") ; calamine required the "encoding" feature and tectonic required the absence.
				  #:name "rust-quick-xml+encoding"
				  #:rust-metadata "guix-variant=+encoding")
     #:for-dependent
     ,(lambda (dependent)
	(member (package-name dependent) '("rust-calamine"))))
    ("rust-glib-sys" ,(@ (gnu packages crates-gtk) rust-glib-sys-0.14)) ; @0.10 doesn't build
    ("rust-glib" ,(@ (gnu packages crates-gtk) rust-glib-0.14)) ; @0.9 doesn't build
    ("rust-gobject-sys" ,(@ (gnu packages crates-gtk) rust-gobject-sys-0.14)) ; @0.10 doesn't build
    ("rust-gio-sys" ,(@ (gnu packages crates-gtk) rust-gio-sys-0.14)) ; @0.10 doesn't build
    ("rust-gdk-pixbuf" ,(@ (gnu packages crates-gtk) rust-gdk-pixbuf-0.14)) ; @0.8 doesn't build
    ("rust-gdk-pixbuf-sys" ,(@ (gnu packages crates-gtk) rust-gdk-pixbuf-sys-0.14)) ; @0.10 doesn't build
    ("rust-gdk-sys" ,(@ (gnu packages crates-gtk) rust-gdk-sys-0.14)) ; no need for old versions
    ("rust-gdk" ,(@ (gnu packages crates-gtk) rust-gdk-0.14)) ; no need for old versions
    ("rust-cairo-sys-rs" ,(@ (gnu packages crates-gtk) rust-cairo-sys-rs-0.14)) ; avoid version conflicts
    ("rust-pango-sys" ,(@ (gnu packages crates-gtk) rust-pango-sys-0.14)) ; likewise
    ("rust-pangocairo" ,rust-pangocairo) ; old version doesn't build
    ("rust-gtk" ,(@ (gnu packages crates-gtk) rust-gtk-0.14)) ; avoid potential problems
    ("rust-system-deps" ,rust-system-deps)
    ("rust-version-compare" ,rust-version-compare)
    ("rust-input-buffer" ,rust-input-buffer)
    ("rust-iso8601" ,rust-iso8601)
    ("rust-enum-to-u8-slice-derive" ,rust-enum-to-u8-slice-derive)
    ("rust-cookie" ,rust-cookie)
    ("rust-aead" ,(p rust-aead-0.4)) ; resolve ‘found two different crates with name `aead`’ errors
    ("rust-aead-4" ,(p rust-aead-0.4)) ; likewise
    ("rust-actix-threadpool" ,(p rust-actix-threadpool-0.3)) ; old rust-actix-threadpool requires old rust-futures
    ("rust-aes-gcm" ,rust-aes-gcm)
    ("rust-chacha20" ,rust-chacha20)
    ("rust-unicase" ,(p rust-unicase-2)) ; @1 doesn't build because of removed features
    ("rust-url" ,(p rust-url-2)) ; @1 doesn't build
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
    ("rust-tokio-postgres" ,rust-tokio-postgres) ; @0.7.2 doesn't build against new rust-phf
    ("rust-tokio-tungstenite" ,rust-tokio-tungstenite) ; @0.11 doesn't build
    ("rust-bindgen"
     ;; In the old version 'runtime' cannot be
     ;; disabled.
     ,(p rust-bindgen-0.59))
    ("rust-headers" ,rust-headers)
    ("rust-heck" ,(p rust-heck-0.4)) ; 0.3 too old for rust-strum-macros@0.24
    ("rust-peg" ,(p rust-peg-0.6)) ; 0.5 misses dependency information
    ("rust-actix-web-codegen" ,rust-actix-web-codegen)
    ("rust-actix-web" ,rust-actix-web) ; @0.3 doesn't build
    ;; rust-atcix-derive@0.4.0,0.5.0 don't build against new
    ;; rust-syn@1.0.82 (Literal has been renamed to Lit)
    ("rust-actix-derive" ,rust-actix-derive)
    ("rust-actix-server" ,rust-actix-server)
    ("rust-typenum" ,rust-typenum)
    ("rust-serial-test-derive" ,(p rust-serial-test-derive-0.5)) ; @0.1 doesn't build
    ("rust-syscallz" ,rust-syscallz)
    ("rust-strum" ,rust-strum)
    ("rust-strum-macros" ,rust-strum-macros)
    ("rust-actix" ,rust-actix) ; @0.10 doesn't build
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
    ("rust-string-cache-codegen" ,(p rust-string-cache-codegen-0.5)) ; @0.4 doesn't build against new rust-phf-... crates.
    ("rust-string-cache" ,(p rust-string-cache-0.8)) ; old version doesn't build against new rust-string-cache-codegen
    ("rust-time" ; resolve version conflict
     ,rust-time)
    ("rust-instant" ; 0.1.4 doesn't build against rust-time@0.3
     ,rust-instant)
    ;; 0.3 requires unstable
    ("rust-hex" ,(p rust-hex-0.4))
    ("rust-sha3" ,rust-sha3)
    ("rust-h2" ,(p rust-h2-0.3)) ; @0.2 doesn't build
    ("rust-scrypt" ,rust-scrypt)
    ("rust-password-hash" ,rust-password-hash)
    ("rust-phf" ,(p rust-phf-0.10)) ; a dependency of @0.7 fails to build against new rust-syn
    ("rust-phf-shared" ,(p rust-phf-shared-0.10)) ; avoid multiple conflicting versions
    ("rust-phf-generator" ,(p rust-phf-generator-0.10)) ; avoid multiple conflicting versions
    ("rust-phf-codegen" ,(p rust-phf-codegen-0.10)) ; avoid build failure in dependency
    ("rust-block-modes" ,rust-block-modes)
    ("rust-ctr" ,rust-ctr)
    ("rust-salsa20" ,rust-salsa20)
    ("rust-cipher" ,rust-cipher)
    ("rust-block-padding" ,rust-block-padding)
    ("rust-streebog" ,(p rust-streebog-0.10))
    ("rust-pango" ,(@ (gnu packages crates-gtk) rust-pango-0.14)) ; @0.8 doesn't build
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
     ,(p rust-nom-7)
     #:for-dependent
     ,(lambda (p)
	(not (member (package-name p) '("rust-terminfo" "rust-ansi-parser"))))) ; needs old rust-nom@5 (or older, in case of rust-ansi-parser) and no update available
    ("rust-nom" ,(p rust-nom-5)
     #:for-dependent
     ,(lambda (p)
	(member (package-name p) '("rust-terminfo")))) ; needs old rust-nom@5 and no update available
    ("rust-ndarray" ,(p rust-ndarray-0.15)) ; old versions don't build
    ;; rust-pktparse@0.5 doesn't build against nom@7
    ("rust-pktparse" ,rust-pktparse)
    ("rust-rusticata-macros" ; old version doesn't build against nom@7
     ,(p rust-rusticata-macros-4))
    ("rust-pure-rust-locales" ; old version doesn't build against nom@7
     ,rust-pure-rust-locales)
    ("rust-itoa" ,(p rust-itoa-1))
    ("rust-sct" ,(p rust-sct-0.7))
    ("rust-quote" ,(p rust-quote-1))
    ("rust-lexical-core" ,(p rust-lexical-core-0.8)) ; @0.7 doesn't build
    ;; 0.3.0 fails to build against new rust-serde
    ("rust-linked-hash-map"
     ,(p rust-linked-hash-map-0.5))
    ("rust-rust-decimal" ,rust-rust-decimal) ; old rust-decimal incompatible with new rust-arrayvec
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
    ("rust-hyper-tls" ,(p rust-hyper-tls-0.5)) ; @0.4 fails to build, @0.5 succeeds
    ("rust-rustls"
     ;; Remove old (not-building) and potentially unsecure versions
     ;; Also, rust-tokio-rustls requires a newer rust-rustls.
     ,rust-rustls-0.20)
    ("rust-tokio-rustls" ;0.10.3 doesn't build
     ,rust-tokio-rustls-0.23)
    ("rust-warp" ,rust-warp)
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
    ("rust-git2" ,(p rust-git2-0.13)) ; @0.9.1 doesn't build
    ("rust-http" ; 0.1 doesn't build and @0.2.1 doesn't have a const HeaderValue from_static
     ,rust-http)
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
     ,(p rust-rand-0.8)) ; tests of some dependencies of rust-rand@0.6 fail, so no need to try adding test-only exceptions for some dependents
    ("rust-lock-api" ; 0.3, 0.2, 0.1
     ,(p rust-lock-api-0.4))
    ("rust-sysctl" ; 0.1 does not compile (type errors)
     ,(p rust-sysctl-0.4))
    ;; The (now deprecated) rust-tempdir doesn't build
    ;; against current rust-rand, use the new rust-tempfile
    ;; instead as upstream recommends.
    ("rust-tempdir"
     ,(p rust-tempfile-3))
    ("rust-version-check"
     ,(p rust-version-check-0.9) ; rust-email@0.0.21 needs a new rust-version-check
     #:for-dependent
     ,(lambda (dependent)
	;; old rust-nom needs an old rust-version-check
	(not (and (string=? (package-name dependent) "rust-nom")
		  (string=? (package-version dependent) "4.2.3")))))
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
    ("rust-miniz-oxide" ,rust-miniz-oxide ; TODO: there are two replacements for this, which one actually had effect?
     #:for-dependent
     ,(lambda (dependent)
	(not (equal? (list (package-name dependent) (package-version dependent))
		     '("rust-png" "0.16.8")))))
    ("rust-deflate" ,rust-deflate)
    ("rust-gl-generator" ,(@ (gnu packages crates-graphics) rust-gl-generator-0.14)) ; resolve version conflict
    ("rust-png" ,rust-png
     #:for-dependent
     ,(lambda (dependent)
	(not (string=? (package-name dependent) "alacritty")))) ; still needs old rust-png ...
    ("rust-tiff" ,rust-tiff)
    ("rust-jpeg-decoder" ,rust-jpeg-decoder)
    ("rust-image" ,rust-image)
    ("rust-jsonrpc-core" ,rust-jsonrpc-core)
    ("rust-lebe" ,rust-lebe)
    ("rust-exr" ,rust-exr)
    ("rust-tikv-jemalloc-sys" ,(p rust-jemalloc-sys-0.3)) ; the tikv fork does bundling (https://issues.guix.gnu.org/56157), and apparently the fork is now mainstream (just a different name), see https://github.com/tikv/jemallocator/pull/25, so no need for two separate packages  (TODO: upstream rust-jemalloc-sys, TODO: update rust-jemalloc-sys)
    ("rust-tikv-jemallocator" ,(p rust-jemallocator-0.3)) ; likewise
    ("rust-lalrpop" ,rust-lalrpop)
    ("rust-lalrpop-util" ,rust-lalrpop-util)
    ("rust-nalgebra" ,(p rust-nalgebra-0.26)) ; replace by building version
    ("rust-nettle-sys" ,rust-nettle-sys-2)
    ("rust-nettle" ,rust-nettle-7)
    ;; 0.4.30 fails to build.
    ("rust-proc-macro2" ,(p rust-proc-macro2-1))
    ("rust-quickcheck" ,(p rust-quickcheck-1)) ; old versions of rust-quickcheck don't build against new rust-rand
    ("rust-raw-window-handle" ,rust-raw-window-handle)
    ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit)
    ("rust-smithay-clipboard" ,rust-smithay-clipboard)
    ("rust-log" ,(p rust-log-0.4))
    ("rust-uuid" ,(p rust-uuid-0.8)) ; @0.5.1 doesn't build
    ("rust-watchexec"
     ,(package-with-extra-patches
       (p rust-watchexec-1)
       (list (local-file "rust-watchexec-nix-compatibility.patch")))) ; for compatibiliy with new rust-nix
    ("rust-wayland-commons" ,rust-wayland-commons) ; for compatibility with new rust-nix
    ("rust-wayland-client" ,rust-wayland-client) ; for Debug traits required by new rust-smithay-client-toolkit
    ("rust-wayland-cursor" ,rust-wayland-cursor) ; ditto
    ("rust-wayland-protocols" ,rust-wayland-protocols) ; ditto
    ("rust-winit" ,rust-winit) ; for compatibility against new dependencies
    ("rust-wl-clipboard-rs" ,rust-wl-clipboard-rs)
    ("rust-x11rb" ,rust-x11rb)
    ("rust-xml5ever" ,rust-xml5ever)
    ("rust-zip" ,rust-zip)))

;; Needed to support multiple versions of the same crate in the same result.
(define %automatic-metadata ; TODO: make automatic metadata the default!
  '("rust-arrayvec"
    "rust-mio"
    "rust-nom"
    "rust-percent-encoding"
    "rust-derive-builder"
    "rust-ordered-float")) ; rust-bio needs the old version, some other crates want the new version

;; TODO: add these (upstream) or teach "guix style" to add them
(define %extra-inputs
  `(("rust-shared-child"
     (("python-minimal" ,(@ (gnu packages python) python-minimal-wrapper)))) ; for tests
    ("rust-structopt" ; for paw feature
     (("rust-paw" ,(p rust-paw-1))))
    ("rust-sysinfo" ; for tests (TODO: maybe native-inputs)
     (("rust-tempfile" ,(p rust-tempfile-3))))
    ("rust-arboard"
     (("rust-log" ,(p rust-log-0.4))
      ("rust-once-cell" ,(p rust-once-cell-1))
      ("rust-parking-lot" ,(p rust-parking-lot-0.11))))
    ("rust-alacritty-terminal"
     (("rust-signal-hook-mio" ,(p rust-signal-hook-mio-0.2)) ; new dep for new version
      ("rust-serde-json" ,(p rust-serde-json-1)))) ; for test
    ("alacritty" ; likewise
     (("rust-structopt" ,(p rust-structopt-0.3))))
    ("rust-aom-sys"
     (("rust-system-deps" ,(p rust-system-deps-3)))) ; missing input (TODO: native-input)
    ("rust-blake3"
     (("rust-blake3-reference-impl" ,rust-blake3-reference-impl))) ; missing test input
    ("rust-buffering-nocopy-macro" ; for new phase
     (("rust-proc-macro2" ,(p rust-proc-macro2-1))))
    ("circtools" ; missing input (TODO: actually a native-input)
     (("rust-cc" ,(p rust-cc-1))
      ("rust-cmake" ,(p rust-cmake-0.1))))
    ("rust-raw-window-handle" ; new input for new dependency
     (("rust-cty" ,(p rust-cty-0.2))))
    ("rust-smithay-client-toolkit" ; new inputs for new version
     (("rust-pkg-config" ,(p rust-pkg-config-0.3)) ; TODO: actually a native-input
      ("libxkbcommon" ,(@ (gnu packages xdisorg) libxkbcommon))))
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
    ("rust-test-cert-gen"
     (("openssl" ,(@ (gnu packages tls) openssl-3.0))))
    ("rust-tree-magic-mini" ; new inputs for new version
     (("rust-bytecount" ,(p rust-bytecount-0.6))
      ("rust-once-cell" ,(p rust-once-cell-1))
      ("shared-mime-info" ,(@ (gnu packages gnome) shared-mime-info))))
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
    ("rust-actix-http"
     (("rust-ahash" ,(p rust-ahash-0.7))
      ("rust-zstd" ,(p rust-zstd-0.9))
      ("rust-bytestring" ,(p rust-bytestring-0.1))
      ("rust-local-channel" ,rust-local-channel) ; for ws feature
      ("rust-futures-core" ,rust-futures-core-0.3) ; for ws feature
      ("rust-httpdate" ,(p rust-httpdate-1))
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))
      ("rust-smallvec" ,(p rust-smallvec-1))))
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
      ("rust-http" ,rust-http) ; for "uri" feature
      ("rust-actix-rt" ,rust-actix-rt)
      ("rust-log" ,(p rust-log-0.4))
      ("rust-tokio-util" ,(p rust-tokio-util-0.3))))
    ("rust-actix-router" ; likewise
     (("rust-firestorm" ,rust-firestorm)))
    ("rust-actix-web" ;likewise (TODO doesn't build yet)
     (("rust-actix-router" ,(p rust-actix-router-0.2))
      ("rust-bytestring" ,(p rust-bytestring-0.1))
      ("rust-smallvec" ,(p rust-smallvec-1))
      ("rust-actix-web-codegen" ,rust-actix-web-codegen)
      ("rust-cfg-if" ,(p rust-cfg-if-1))
      ("rust-itoa" ,(p rust-itoa-1))
      ("rust-language-tags" ,(p rust-language-tags-0.2))
      ("rust-once-cell" ,(p rust-once-cell-1))
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))
      ("rust-cookie" ,(p rust-cookie-0.15))
      ("rust-ahash" ,(p rust-ahash-0.7))))
    ("rust-actix-web-codegen" ; likewise
     (("rust-actix-router" ,(p rust-actix-router-0.2))))
    ("rust-actix" ; likewise
     (("rust-futures-core" ,rust-futures-core-0.3)
      ("rust-futures-sink" ,rust-futures-sink-0.3)
      ("rust-futures-task" ,rust-futures-task-0.3)
      ("rust-futures-util" ,rust-futures-util-0.3)
      ("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))))
    ("rust-awc" ; new dependencies for new version
     (("rust-pin-project-lite" ,(p rust-pin-project-lite-0.2))
      ("rust-actix-tls" ,rust-actix-tls)
      ("rust-actix-utils" ,rust-actix-utils)
      ("rust-ahash" ,(p rust-ahash-0.7))
      ("rust-cookie" ,(p rust-cookie-0.15))
      ("rust-futures-util" ,rust-futures-util-0.3)
      ("rust-h2" ,(p rust-h2-0.3))
      ("rust-itoa" ,(p rust-itoa-1))
      ("rust-http" ,(p rust-http-0.2))
      ("rust-tokio" ,rust-tokio)))
    ("rust-enquote" ; missing input
     (("rust-thiserror" ,(p rust-thiserror-1))))
    ("rust-freetype-sys"
     (("freetype" ,(@ (gnu packages fontutils) freetype))))
    ;; No need to avoid Rust dependencies.
    ("rust-flate2"
     ,(list (list "zlib" (@ (gnu packages compression) zlib))))
    ("rust-chrono-tz" ; missing input
     (("rust-uncased" ,(p rust-uncased-0.9))))
    ("rust-cmake"
     ,(list (list "cmake" (@ (gnu packages cmake) cmake-minimal))))
    ("rust-clang-sys"
     ;; TODO needs more work for
     ,(list (list "clang" (@ (gnu packages llvm) clang-13))))
    ("rust-config" ; new inputs for new verison
     (("rust-async-trait" ,(p rust-async-trait-0.1))
      ("rust-ron" ,(p rust-ron-0.6))
      ("rust-pathdiff" ,(p rust-pathdiff-0.2))))
    ("rust-dashmap" ; new inputs forn new version
     (("rust-hashbrown" ,(p rust-hashbrown-0.11))
      ("rust-parking-lot-core" ,(p rust-parking-lot-core-0.8))
      ("rust-lock-api" ,(p rust-lock-api-0.4))))
    ;; for "pem" feature
    ("rust-der"
     (("rust-pem-rfc7468" ,(@ (gnu packages crates-io) rust-pem-rfc7468-0.2))))
    ("rust-emacs-macros"
     (("rust-proc-macro2" ,(p rust-proc-macro2-1)))) ; new input for new version
    ("rust-embed-resource"
     (("rust-cc" ,(p rust-cc-1)))) ;; TODO: native-input
    ("rust-glib"
     (("rust-futures-core" ,(p rust-futures-core-0.3)) ; for tests
      ("rust-futures-util" ,(p rust-futures-util-0.3)) ; for tests
      ("rust-tempfile" ,(p rust-tempfile-3)))) ; for tests
    ;; for "pem" and "alloc" feature
    ("rust-pkcs1"
     (("rust-pkcs8" ,(@ (gnu packages crates-io) rust-pkcs8-0.7))))
    ;; for "cbc" feature
    ("rust-pkcs5"
     (("rust-cbc" ,rust-cbc)
      ("rust-sha1" ,rust-sha1))) ; missing dep (for pbes2)
    ("rust-reqwest" ; new inputs for new version
     (("rust-h2" ,rust-h2)))
    ("rust-sha1" (("rust-digest" ,rust-digest)
		  ("rust-cfg-if" ,(p rust-cfg-if-1)) ;missing dep
		  ("rust-cpufeatures" ,(p rust-cpufeatures-0.2))))
    ;; for "sha1" and "sha2" features
    ("rust-spki" (("rust-sha1" ,rust-sha1)
		  ("rust-base64ct" ,(p rust-base64ct-1)) ; missing dep
		  ("rust-sha2" ,(@ (gnu packages crates-io) rust-sha2-0.10))))
    ("rust-image" (("rust-exr" ,rust-exr)))
    ("rust-lettre"
     (("rust-socket2" ,(p rust-socket2-0.4))
      ("rust-fastrand" ,(p rust-fastrand-1))
      ("rust-email-address" ,rust-email-address)
      ("rust-email-encoding" ,rust-email-encoding)
      ("rust-idna" ,(p rust-idna-0.2))
      ("rust-mime" ,(p rust-mime-0.3))
      ("rust-httpdate" ,(p rust-httpdate-1))
      ("rust-quoted-printable" ,(p rust-quoted-printable-0.4))))
    ("rust-libsqlite3-sys" (("sqlite" ,(@ (gnu packages sqlite) sqlite)))) ; missing dependencies (the old versions of rust-libsqlite3-sys acurately add this dependency!),see #56032
    ;; contains pkg-config files
    ("rust-mysqlclient-sys" (("mariadb:dev" ,(@ (gnu packages databases) mariadb) "dev")))
    ;; possibly only required by new version
    #;("rust-boxxy" (("rust-anyhow" ,(@ (gnu packages crates-io) rust-anyhow-1)))) ; TODO: currently useless because in %removed-dependencies, revisit when tests are supported
    ("rust-petgraph" (("rust-indexmap" ,(@ (gnu packages crates-io) rust-indexmap-1))))
    ("sniffglue" (("rust-bstr" ,(@ (gnu packages crates-io) rust-bstr-0.2))))
    ("rust-jsonrpc-core" ; new inputs for new version
     (("rust-futures-executor" ,rust-futures-executor-0.3)
      ("rust-futures-util" ,rust-futures-util-0.3)))
    ("rust-lalrpop" (("rust-tiny-keccak" ,(p rust-tiny-keccak-2))
                     ("rust-pico-args" ,rust-pico-args)))
    ("rust-merge-derive" (("rust-syn" ,(p rust-syn-1)) ; missing inputs
			  ("rust-quote" ,(p rust-quote-1))
			  ("rust-proc-macro2" ,(p rust-proc-macro2-1))
			  ("rust-proc-macro-error" ,(p rust-proc-macro-error-1))))
    ("rust-ncurses" (("ncurses" ,(@ (gnu packages ncurses) ncurses)))) ; missing input
    ("rust-nitrokey-sys" (("libnitrokey" ,(@ (gnu packages security-token) libnitrokey))
			  ("gcc:lib" ,(@ (gnu packages gcc) gcc) "lib") ; for stdbool.h (TODO?)
			  ("rust-bindgen" ,(p rust-bindgen-0.59)) ; TODO: actually a native-input
			  ("rust-cc" ,(p rust-cc-1)))) ; TODO: actually a native-input
    ("rust-nix" (("rust-assert-impl" ,(p rust-assert-impl-0.1)))) ; required by tests (TODO: maybe native-inputs)
    ("rust-nu-ansi-term" (("rust-regex" ,(p rust-regex-1)) ; for tests
			  ("rust-serde-json" ,(p rust-serde-json-1)))) ; for tests
    ;; TODO: is this sufficient?
    ("rust-futures-core-preview"
     (("rust-futures-core" ,rust-futures-core-0.3)))
    ("rust-http-body" ; at least for 0.4
     (("rust-pin-project-lite" ,(@ (gnu packages crates-io) rust-pin-project-lite-0.2))))
    ("rust-headers"
     (("rust-httpdate" ,(p rust-httpdate-1)))) ; new dependency
    ("rust-hex"
     (("rust-pretty-assertions" ,(p rust-pretty-assertions-1)))) ; for tests
    ("rust-s3handler" ; new dependency for new version
     (("rust-thiserror" ,(p rust-thiserror-1))))
    ("rust-tectonic-bridge-core"
     ;; TODO: native-input
     ;; Required in rust-tectonic-bridge-core@0.3 for extra 'regenerate-cbindgen-things'
     ;; phase.
     (("rust-cbindgen" ,rust-cbindgen-0.19)))
    ("rust-tectonic-bridge-freetype2"
     (("freetype" ,(@ (gnu packages fontutils) freetype))))
    ("rust-tectonic-bridge-graphite2"
     (("graphite2" ,(@ (gnu packages fontutils) graphite2))))
    ("rust-tectonic-bridge-harfbuzz"
     (("freetype" ,(@ (gnu packages fontutils) freetype))
      ("harfbuzz" ,(@ (gnu packages gtk) harfbuzz))))
    ("rust-tectonic-bridge-icu"
     (("icu4c" ,(@ (gnu packages icu4c) icu4c))))
    ("rust-tectonic-xetex-layout" ; missing input
     (("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
      ("harfbuzz" ,(@ (gnu packages gtk) harfbuzz)))) ; missing input, .pc points to a subdirectory while it shouldn't or #include <harfbuzz/hb.h> needs to be replaced by #include <harfbuzz/hb.h>
    ("rust-tectonic-engine-xetex" ; missing input (TODO: maybe detect (upstream) in rust-tectonic-xetex-layout to add to the list?)
     (("fontconfig" ,(@ (gnu packages fontutils) fontconfig))
      ("harfbuzz" ,(@ (gnu packages gtk) harfbuzz-3))))
    ("rust-time" ; new inputs for new version
     (("rust-num-threads" ,rust-num-threads)))
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
     (("rust-tracing" ,(p rust-tracing-0.1)))) ; missing dependency
    ("rust-x11" ; missing inputs
     (("libx11" ,(@ (gnu packages xorg) libx11))
      ("libxcursor" ,(@ (gnu packages xorg) libxcursor))
      ("libxext" ,(@ (gnu packages xorg) libxext))
      ("libxft" ,(@ (gnu packages xorg) libxft))
      ("libxi" ,(@ (gnu packages xorg) libxi))
      ("libxinerama" ,(@ (gnu packages xorg) libxinerama))
      ("libxmu" ,(@ (gnu packages xorg) libxmu))
      ("libxrandr" ,(@ (gnu packages xorg) libxrandr))
      ("libxscrnsaver" ,(@ (gnu packages xorg) libxscrnsaver))
      ("libxt" ,(@ (gnu packages xorg) libxt))
      ("libxtst" ,(@ (gnu packages xorg) libxtst))
      ("libxxf86vm" ,(@ (gnu packages xorg) libxxf86vm))))
    ("rust-x11rb" ; missing inputs
     (("libxcb" ,(@ (gnu packages xorg) libxcb))))
    ("rust-warp" ; new dependencies for new version
     (("rust-futures-channel" ,(p rust-futures-channel-0.3))
      ("rust-futures-util" ,(p rust-futures-util-0.3))
      ("rust-tokio-util" ,rust-tokio-util-0.7)
      ("rust-rustls-pemfile" ,(p rust-rustls-pemfile-0.2))
      ("rust-percent-encoding" ,(p rust-percent-encoding-2))))
    ("rust-wayland-cursor" ; new dependencies for new version
     (("rust-xcursor" ,(p rust-xcursor-0.3))
      ("rust-wayland-client" ,(@ (gnu packages crates-graphics) rust-wayland-client-0.28))))
    ("rust-yansi"
     (("rust-parking-lot" ,(p rust-parking-lot-0.11)))) ; test input
    ("rust-zip" ; new inputs for new version
     (("rust-zstd" ,(p rust-zstd-0.9))))))

(define %test-options
  '(("rust-ansi-term"
     "--exact"
     "--skip=debug::test::long_and_detailed") ; fails because the new rust-serde-json outputs things a little different from what rust-ansi-term is used to, seems harmless
    ("rust-arrayref"
     "--exact"
     ;; reported upstream: https://github.com/droundy/arrayref/issues/22
     "--skip=test::check_array_mut_ref_7"
     "--skip=test::check_array_ref_5")
    ("rust-calloop"
     "--exact"
     "--skip=loop_logic::tests::insert_source_no_interest") ; known test failure, reported at <https://github.com/Smithay/calloop/issues/96>
    ("rust-glob"
     "--exact"
     "--skip=test::test_iteration_errors") ; /root does not exist in the build environment
    ("rust-rustls"
     "--skip=msgs::message_test::test_read_fuzz_corpus" ; some test files are missing
     ;; It doesn't find some issuers, probably just
     ;; <https://github.com/rustls/rustls/pull/896> which would be fixed in later versions.
     ;; Additionally, the certificate tests are time bombs in that they expire:
     ;; <https://github.com/rustls/rustls/pull/71>, so disable them.
     "--skip=verifybench::test_")
    ("rust-sysinfo"
     "--exact"
     "--skip=test::check_uid_gid" ; there's no root in the build environment
     "--skip=test::check_system_info"))) ; there's no /etc/os-release or /etc/lsb-release (TODO: maybe we could patch SystemExt.name to just return Guix?).

;; Packages for which tests are disabled.
;; The second part of the pair is a 'reason' for disabling them.
;;
;;   * removed-dependency
;;   * fails (not proper because defeats the point of tests but will do for now).
;;   * build-environment: tests assume things about the build environment that are false
;;     (e.g., rust-atty assumes stdin/out/err is a tty)
;;   * missing-files: some files used by tests are missing from the source code
;;     (fix is probably to switch to VC like done for PyPi)
;;   * version: tests do not compile against the versions of dependencies
;;
;; (the reason symbol can be used by 'guix style' to automatically add a comment
;; to #:tests? #false)
(define %disable-tests
  '(("rust-adler32" . version) ; rust-rand
    ("rust-ahash" . removed-dependency) ; rust-hex
    ("rust-atty" . build-environment) ; assumes fd 0/1/2 are ttys
    ("rust-base64" . version) ; rust-rand
    ("rust-bincode" . removed-dependency) ; quickcheck
    ("rust-byteorder" . removed-dependency) ; quickcheck
    ("rust-chrono" . removed-dependency) ; quickcheck
    ("rust-deflate" . other) ; something about Rust 2018 and use statements having changed?
    ("rust-duct" . removed-dependency) ; rust-tempdir
    ("rust-env-logger" . removed-dependency) ; quickcheck
    ("rust-erased-serde" . removed-dependency) ; rust-serde-json
    ("rust-fst" . version) ; rust-quickcheck
    ("rust-hashbrown" . version) ; rust-rand?
    ("rust-heapless" . version) ; rust-scoped-threadpool?
    ("rust-humantime" . removed-dependency) ; quickcheck
    ("rust-indexmap" . removed-dependency) ; quickcheck
    ("rust-inotify" . version ) ; rust-rand
    ("rust-itertools" . removed-dependency) ; quickcheck
    ("rust-log" . removed-dependency) ; rust-serde-test
    ("rust-memchr" . removed-dependency ) ; quickcheck
    ("rust-memmap" . removed-dependency) ; rust-tempdir
    ("rust-memmap2" . removed-dependency) ; rust-tempdir
    ("rust-nom" . removed-dependency) ; rust-proptest
    ("rust-png" . missing-files)
    ("rust-rand" . removed-dependency) ; rand-pcg
    ("rust-rayon" . removed-dependency) ; rand-xorshift
    ("rust-rayon-core" . removed-dependency) ; rand-rand-xorshift
    ("rust-regex" . removed-dependency) ; quickcheck
    ("rust-rustc-serialize" . fails) ; fails to compile
    ("rust-serde-fmt" . removed-dependency) ; rust-serde-derive
    ("rust-sval" . removed-dependency) ; quickcheck
    ("rust-system-deps" . version) ; rust-pkg-config
    ("rust-terminal-size" . build-environment) ; /dev/stderr is not a tty
    ("rust-time" . missing-files) ; the tests module, and quickcheck
    ("rust-tokio" . removed-dependency) ; rust-tokio<->rust-tokio-test cycle
    ("rust-winres" . fails))) ; only for compiling to Windows, which isn't supported yet

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

(define (looks-like-library? name)
  (and (string-prefix? "rust-" name)
       (not (member name '("rust-analyzer" "rust-cargo-c")))))
(define (outputs-for-package-name name)
  ;; If it looks like just a library, separate libs, binaries, etc,
  ;; because usually binaries aren't needed.  (closure size considerations)
  (if (looks-like-library? name)
      %rust-library-outputs
      ;; If it looks like a leaf package, separate "out" and "lib",
      ;; because things are sort-of statically linked.
      '("out" "lib")))

(define (append-filter-map f list)
  (concatenate (filter-map f list)))

;; todo: ‘stub‘ rust-rustc-version to reduce deps?
;; grrr rust-backtrace
(define (vitaminate/auto* pack)
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 cargo-build-flags ; TODO: investigate later
		 vendor-dir ; not needed in antioxidant
		 (rust-metadata #false) ; #false: automatically determine
		 modules ; TODO: handle #:modules
		 install-source? ; not used by antioxidant-build-system
		 (features #~'("default")))
	 (unless (or (eq? phases '%standard-phases)
		     (not (is-cargo-toml-phases? phases)))
	   (error "phases?"))
	 (define fix-input
	   (match-lambda
	     ((label dependency . maybe-output)
	      (and (not (remove-dependency? pack dependency))
		   (let ((vitaminated-input
			  (vitaminate/auto
			   ;; Resolve version conflicts, choose newer versions,
			   ;; etc.
			   (or (find-replacement pack dependency) dependency))))
		     ;; These are actually test inputs! (TODO guix)
		     ;; (TODO: this isn't build from source)
		     ;;(not (equal? (package-name pack) "rust-pure-rust-locales"))
		     #;(pk 'p pack dependency)
		     (cond ((not (eq? (package-build-system vitaminated-input)
				      antioxidant-build-system))
			    (list (cons* label vitaminated-input maybe-output)))
			   ;; For cbindgen, sometimes the binary, sometimes the lib
			   ;; is needed.  Add both.
			   ((string=? label "rust-cbindgen")
			    (list (list label vitaminated-input "lib")
				  (list label vitaminated-input "out")))
			   ((string-prefix? "rust-nu-plugin" label)
			    (list (list label vitaminated-input "out"))) ; for wrap-program
			   ;; TODO: in some cases, "out" is needed!
			   ;; For library crates, we usually just need the lib output.
			   ((looks-like-library? label)
			    ;; For some packages, we don't have separate outputs
			    ;; (e.g. if they don't have tests, binaries, etc.).
			    ;; In that case, just do the default output.
			    (let ((output (if (member "lib" (package-outputs vitaminated-input))
					      "lib"
					      "out")))
			      (list (list label vitaminated-input output))))
			   (#true (list (cons* label vitaminated-input maybe-output)))))))))
	 ;; Detect cycles early by unthunking
	 (define i
 	   (append-filter-map
	    fix-input
	    (append (match (assoc-ref %extra-inputs (package-name pack))
		      ((association-list) association-list)
		      (#false '())) ; no extra inputs
		    cargo-inputs
		    (package-inputs pack))))
	 (define n-i (append-filter-map
		      fix-input
		      (append cargo-development-inputs
			      ;; TODO: move zlib of rust-libz-sys-1 from
			      ;; native-inputs to inputs.
			      (package-native-inputs pack)
			      (match (package-name pack)
				("rust-backtrace"
				 `(("rust-cc" ,(p rust-cc-1)))) ; missing dep
				(_ '())))))
	 (define p-i (append-filter-map
		      fix-input (package-propagated-inputs pack)))
	 (package
	  (inherit pack)
	  (build-system antioxidant-build-system)
	  (outputs (outputs-for-package-name (package-name pack)))
	  (source
	   (match (package-name pack)
	     ("rust-libnghttp2-sys"
	      (origin
	       (inherit (package-source pack))
	       ;; original unbundling code doesn't work for antioxidant
	       ;; (the library directory -L is not recorded, only -l was).
	       (patches (list (local-file "rust-libnghttp2-unbundle.patch")))
	       (snippet #~(begin
			    (delete-file-recursively "nghttp2")
			    (rename-file "Cargo.toml.orig" "Cargo.toml")))))
	     ("rust-itoa"
	      (origin
	       (inherit (package-source pack))
	       ;; TODO: for compatibility with rust-http
	       (patches (list (local-file "rust-itoa-Reintroduce-fmt.patch")))))
	     (_ (package-source pack))))
	  (arguments (list #:rust-metadata ; the same crate can be linked to multiple times with different versions as long a the metadata is different. (it's not simply set to X.Y.Z, to allow for grafting later)
			   (cond (rust-metadata rust-metadata)
				 ((member (package-name pack) %automatic-metadata)
				  ;; TODO: maybe make this the default (in antioxidant.scm?)
				  (string-append "version="
						 (if (string-prefix? "0." (package-version pack))
						     (version-major+minor (package-version pack))
						     (version-major (package-version pack)))))
				 (#true ""))
			   #:rust-crate-type
			   (match (assoc (package-name pack) %crate-types)
			     ((_ value) value)
			     (#false #~#false)) ; use Cargo.toml
			   #:tests?
			   (not (assoc-ref %disable-tests (package-name pack)))
			   ;; TODO: what's the keyword for cargo-build-system?
			   ;; Maybe the old values can be copied.
			   #:test-options
			   #~'#$(or (assoc-ref %test-options (package-name pack)) '())
			   #:features
			   ;; TODO: can some now be removed now that default features
			   ;; are enabled by default?  And maybe the features can be moved
			   ;; to Guix upstream?
			   (match (assoc (package-name pack) %features)
			     ((_ value)
			      (match value
				((? procedure? procedure) (procedure pack))
				((? gexp? value) value)))
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
		   "rust-system-deps"
		   "rust-tectonic-dep-support")
	       (package-search-paths (@ (gnu packages pkg-config) pkg-config)))
	      (_ '()))
	    (package-search-paths pack)))
	  (native-search-paths
	   (append
	    (match (package-name pack)
	      ;; Make sure that PKG_CONFIG_PATH is available to build.rs.
	      ((or "rust-pkg-config"
		   "rust-system-deps"
		   "rust-tectonic-dep-support")
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
     (pk "oops, a cycle?")
     (throw 'antioxidant-cycle)) ; caught in antioxidant-ci.scm
   (parameterize ((vitamination-stack (cons pack (vitamination-stack))))
     (vitaminate/auto* pack))))

;; Self-checks
(define (check-removed-extra-inputs)
  "Verify that %extra-inputs is not in contradiction with
%removed-dependencies"
  ;; List packages in %removed-dependencies which have an entry in
  ;; %extra-inputs defined (useless, unless it's a leaf package)
  (define (check-removed-dependency entry)
    (match entry
      ((? string? name)
       (when (assoc name %extra-inputs)
	 (pk name "in %removed-dependencies and %extra-inputs (probably useless)")
	 (throw 'oops)))
      (((? string? left) '-> (? string? right))
       (when (member right %removed-dependencies)
	 (pk right "(a -> b) entry in %removed-dependencies and b in %removed-dependencies (redundant)")
	 (throw 'oops))
       (when (member left %removed-dependencies)
	 (pk left "(a -> b) entry in %removed-dependencies and a in %removed-dependencies (useless)")
	 (throw 'oops)))
      (((? string? dependency-name) #:for-dependent context?)
       (when (member dependency-name %removed-dependencies)
	 (pk dependency-name "a #:for-dependent context? entry in %removed-dependencies and a in %removed-dependencies (redundant")
	 (throw 'oops)))
      (a (pk 'a a)
	 (error "bogus entry in %removed-dependencies"))))
  ;; list names listed as an extra-input for some package and also in
  ;; %removed-dependencies (confusing, because they would have to be both
  ;; added and removed).
  (define check-extra-input
    (match-lambda
      (((? string? name) (? list? dependencies))
       (let ((check-dependency
	      (match-lambda ((dependency-name _ . rest)
			     (when (member dependency-name %removed-dependencies)
			       (pk "extra-input " dependency-name " of " name "in %removed-dependencies (contradictory)")
			       (throw 'oops))
			     (when (member (list name '-> dependency-name) %removed-dependencies)
			       (pk name "->" dependency-name "both in %removed-dependencies and %extra-inputs (contradictory)")
			       (throw 'oops))))))
	 (for-each check-dependency dependencies)))))
  (for-each check-removed-dependency %removed-dependencies)
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
(define-public antioxidated-drill
  (package
   (inherit (public-test-package (vitaminate/auto drill)))
   (source
    (origin
     (inherit (package-source drill))
     (patches (list (local-file "drill-update-dependencies.patch")))
     (snippet #~(rename-file "Cargo.toml.orig" "Cargo.toml"))))))
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
(define-public antioxidated-newsboat
  ;; "newsboat" has a Makefile that uses Cargo and Cargo workspaces,
  ;; which is currently unsupported by antioxidant.
  (let* ((base (@ (gnu packages syndication) newsboat))
	 (base/antioxidant (vitaminate/auto base))
	 (base/internal ;; TODO: hidden
	  (package
	   (inherit base/antioxidant)
	   (synopsis "Internal library of newsboat")
	   (description "This is an internal library of newsboat")
	   (properties '((hidden? . #t)))))
	 (arguments/chdir
	  (lambda (dir)
	    (list #:phases
		  #~(modify-phases %standard-antioxidant-phases
		      (add-after 'unpack 'goto-rust-code
		        (lambda  _
			  (chdir #$dir))))))))
    (define rust-newsboat-regex-rs
      (package
       (inherit base/internal)
       (name "rust-newsboat-regex-rs")
       (arguments (arguments/chdir "rust/regex-rs"))
       (inputs (modify-inputs
		(package-inputs base/antioxidant)
		(prepend rust-newsboat-strprintf)))))
    (define rust-newsboat-strprintf
      (package
       (inherit base/internal)
       (name "rust-strprintf")
       (arguments (arguments/chdir "rust/strprintf"))))
    ;; TODO: gettext-system feature of the gettext crate
    ;; TODO: building this as a static library is sufficient even once antioxidant
    ;; supports shared libraries
    (define rust-libnewsboat
      (package
       (inherit base/internal)
       (name "rust-libnewsboat")
       (arguments (arguments/chdir "rust/libnewsboat"))
       (inputs (modify-inputs
		(package-inputs base/antioxidant)
		(prepend rust-newsboat-regex-rs rust-newsboat-strprintf)))))
    (define rust-libnewsboat-ffi
      (package
       (inherit base/internal)
       (name "rust-libnewsboat-ffi")
       (arguments (append
		   (list #:rust-crate-type "staticlib") ; TODO: non-static, for grafts.  Also huge (77.4 MiB)!  Is there a mechanism for static libraries that _doesn't_ include copies of the dependencies?
		   ;; TODO: investigate contents
		   (substitute-keyword-arguments (arguments/chdir "rust/libnewsboat-ffi")
		     ((#:phases old-phases)
		      #~(modify-phases #$(pk 'o old-phases)
			  (add-before 'unpack 'set-rust-cxx-output
			      (lambda _
				(mkdir-p (string-append #$output "/lib/newsboat-ffi-things"))
				(setenv "RUST_CXX_BUILD_OUTPUT" (string-append #$output "/lib/newsboat-ffi-things")))))))))
       (inputs (modify-inputs
		(package-inputs base/antioxidant)
		(prepend rust-libnewsboat)))))
    (public-test-package ; TODO solve build failures (cannot find -lnghttp2)
     (package
      (inherit base/antioxidant)
      (build-system (@ (guix build-system gnu) gnu-build-system))
      (arguments
       (list #:make-flags #~(list (string-append "prefix=" #$output)
				  (string-append "CARGO=echo 'do not use cargo for:'"))
	     #:imported-modules (cons '(antioxidant) %gnu-build-system-modules)
	     #:modules
	     '((guix build gnu-build-system)
	       (antioxidant)
	       (guix build utils))
	     #:tests? #false ;; fails with ‘Error opening terminal: unknown’.  Also, the unantixidated variant did not run tests’ --> TODO: what
	     #:phases
	     #~(modify-phases %standard-phases
		 ;; required by (antioxidant)
		 #$@(with-extensions (list guile-json-4) #~())
		 (delete 'configure)
		 ;; TODO: maybe create a symlink forest of the generated
		 ;; headers by default (in 'target', where the Makefile
		 ;; expects it?)
		 (add-after 'unpack 'find-ffi-things
		   (lambda* (#:key inputs #:allow-other-keys)
		     (substitute* "Makefile"
		       (("\\$\\(relative_cargo_target_dir\\)/cxxbridge")
			(search-input-directory inputs "lib/newsboat-ffi-things/cxxbridge/include"))
		       ;; todo: find dependency -lzlib etc. (--> now solved? to be verified)
		       (("\\$\\(CARGO_TARGET_DIR\\)/\\$\\(BUILD_TYPE\\)/libnewsboat.a")
			(search-input-file inputs "lib/guixcrate/libnewsboat.a"))
		       (("-L\\$\\(CARGO_TARGET_DIR\\)/\\$\\(BUILD_TYPE\\)")
			(let ((crates (find-directly-available-crates inputs))
			      (required (list (make-crate-mapping "libnewsboat-ffi" "libnewsboat-ffi"))))
			  (string-append "-L"
					 (dirname (search-input-file inputs "lib/guixcrate/libnewsboat.a"))
					 " "
					 (string-join (linker-arguments/non-rustc crates required))))))))
		 (add-after 'unpack 'replace-cargo
		   (lambda _ ; TODO: finish
		     (substitute* "config.sh"
		       (("fail \"cargo\"") ":")))))))
      (inputs (modify-inputs (package-inputs base/antioxidant)
			     (append rust-libnewsboat-ffi)))))))

;; Make a shared library and link to it
(define-public test-lib
  (package
   (source (local-file "shared-lib" #:recursive? #true))
   (name "test-sharedlib")
   (version "0.0")
   (build-system antioxidant-build-system)
   (home-page #f)
   (synopsis "Basic example of C-style shared libraries implemented in Rust")
   (arguments
    (list #:rust-dynamic-library-arguments
	  #~'("-C" "link-args=-rdynamic")
	  #:phases
	  #~(modify-phases %standard-antioxidant-phases
	      (replace 'build-binaries
		(lambda _
		  (mkdir (string-append #$output "/bin"))
		  (invoke "gcc" "-L" (string-append #$output "/lib")
			  "-lsharedlib"
			  "example.c"
			  "-o" (string-append #$output "/bin/test")))))))
   ;; /gnu/store/[...]/bin/test  || echo $? ---> 42
   (description #f)
   (license #f)))


;; Update from <https://issues.guix.gnu.org/54299#53>.
;;
;; Note: the copyright info on the top is incomplete, this
;; copies some code from the package definition, will be solved
;; once antioxidant is merged into Guix.
;;
;; TODO: keep the phase that adds all the nice man pages etc.

(define-public antioxidated-alacritty
  (public-test-package
   (vitaminate/auto
    (package
     (inherit (@ (gnu packages terminals) alacritty))
     (name "alacritty")
     (version "0.10.1")
     (source
      (origin
       ;; XXX: The crate at "crates.io" has limited contents.  In particular,
       ;; it does not contain "extra" directory with completions, icon, etc.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwilm/alacritty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
	(base32 "1s38gyx0ifcf1vcr6n8gzbk7rg1apxrz7js8cp8x5k1s0m3abys3"))))))))

(define-public antioxidated-b3sum ; new version required for compatibility with new rust-blake3. Not updated to @1.3.1, because that requires updating non-trivial dependencies.
  (public-test-package
   (vitaminate/auto
    (package
      (inherit (@ (gnu packages crypto) b3sum))
      (name "b3sum")
      (version "1.0.0")
      (source (origin
		(method url-fetch)
		(uri (crate-uri "b3sum" version))
		(file-name (string-append name "-" version ".tar.gz"))
		(sha256
		 (base32
                  "1rq0yqqzrxwqi2c90pzl4v9g6a2gcbvirp8knbgyq38jb0cshvfr"))))))))


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
      antioxidated-tealdeer
      test-lib)

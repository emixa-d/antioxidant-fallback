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
(define-module (antioxidant)
  #:export (find-crates crate-directory extract-crate-name extern-arguments
			L-arguments compile-rust compile-rust-library
			compile-rust-binary compile-cargo
			read-dependency-environment-variables
			%standard-antioxidant-phases)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (json))

;;;
;;; Reading Cargo.toml files.
;;;

(define (or-constant constant)
  (lambda (proc)
    (lambda (foo)
      (if (unspecified? foo)
	  constant
	  (proc foo)))))

(define or-false (or-constant #false))
(define or-empty (or-constant '()))
(define or-false* ((or-constant #false) identity))
(define or-true* ((or-constant #false) identity))
(define or-emptystring* ((or-constant "") identity))

;; rust-libc does not compile with edition=2018
(define %default-edition "2015")
(define or-default-edition* ((or-constant %default-edition) identity))

(define-json-mapping <package> make-package package?
  %json->package <=> %package->json <=> scm->package <=> package->scm
  (autobins package-autobins "autobins" or-true*) ; boolean
  (autoexamples package-autoexamples "autoexamples" or-true*) ; boolean
  (autotests package-autotests "autotests" or-true*) ; boolean
  (autobenches package-autobenches "autobenches" or-true*) ; boolean
  (version package-version "version" or-emptystring*) ; string
  (authors package-authors "authors" (or-empty vector->list)) ; vector of strings
  (categories package-categories "categories" (or-empty vector->list)) ; vector of strings
  (name package-name) ; string
  (description package-description "description" or-emptystring*) ; string
  (homepage package-homepage "homepage" or-emptystring*) ; string
  (repository package-repository "repository" or-emptystring*) ; string
  (license package-license "license" or-emptystring*) ; string
  (license-file package-license-file "license-file" or-emptystring*) ; string
  (edition package-edition "edition" or-default-edition*) ; string
  (build package-build "build" or-false*)
  (links package-links "links" or-false*)) ; string, despite the s suffix

;; TODO: not yet used.  Maybe in the future we could check for
;; version incompatibilities?
(define-json-mapping <dependency> make-dependency dependency?
  %json->dependency <=> %package->dependency <=> scm->dependency <=> package->dependency
  ;; String.  Not actually part of the JSON / TOML.
  (name dependency-name)
  (optional %dependency-optional) ; boolean
  (path %dependency-path) ; string | #false
  (version %dependency-version) ; string | #false
  (git %dependency-git) ; string | #false
  (branch %dependency-branch) ; string | #false
  (default-features %dependency-default-features) ; boolean
  (registry %dependency-registry)) ; string | #false

(define (scm->dependency-list scm)
  (define f
    (match-lambda
      ((key . value)
       (match value
	 ((? string? version)
	  (scm->dependency `(("name" . ,key) ("version" . ,version))))
	 ((? list?) (scm->dependency `(("name" . ,key) ,@value)))))))
  (map f scm))

;;
;; <https://doc.rust-lang.org/cargo/reference/cargo-targets.html#configuring-a-target>
;;
;; For a [lib], [[bin]], [[example]], [[test]] or [[bench]] section.
;;
(define-json-mapping <target> make-target target?
  %json->target <=> %target->json <=> scm->target <=> target->scm
  (name target-name "target" or-false*)
  (path target-path "path" or-false*)
  (test %target-test)
  (doctest %target-doctest)
  (bench %target-bench)
  (doc %target-doc)
  (plugin %target-plugin)
  (proc-macro %target-proc-macro)
  (proc_macro %target-proc_macro)
  (harness %target-harness)
  (edition target-edition "edition" or-false*)
  (crate-type %target-crate-type)
  ;; NA for [lib]
  (required-features target-required-features "required-features"
		     (or-empty vector->list)))

(define (target-proc-macro target)
  ;; TODO: which one is it?  (For rust-derive-arbitrary,
  ;; it is proc_macro)
  (match (list (%target-proc-macro target) (%target-proc_macro target))
    (((? boolean? x) _) x)
    (((? unspecified?) (? boolean? x)) x)
    (((? unspecified?) (? unspecified?)) #false)))

(define (scm->target-list s)
  (map scm->target (vector->list s)))

(define-json-mapping <target-specific> make-target-specific? target-specific?
  %json->target-specific <=> %manifest->target-specific <=> scm->target-specific <=> target-specific->scm
  (target %target-specific-target) ; string, not actually part of the json
  (dependencies target-specific-dependencies "dependencies" (or-empty scm->dependency-list))
  ;; For tests, examples and benchmarks
  (dev-dependencies target-specific-dev-dependencies "dev-dependencies" (or-empty scm->dependency-list))
  ;; For build scripts
  (build-dependencies target-specific-build-dependencies "build-dependencies" (or-empty scm->dependency-list)))

(define-json-mapping <manifest> make-manifest manifest?
  %json->manifest <=> %manifest->json <=> scm->manifest <=> manifest->scm
  (package manifest-package "package" scm->package)
  (lib manifest-lib "lib" (or-false scm->target))
  (bin manifest-bin "bin" (or-empty scm->target-list))
  (bench manifest-bench "bench" (or-empty scm->target-list))
  (example manifest-example "example" (or-empty scm->target-list))
  (test manifest-test "test" (or-empty scm->target-list))
  (features manifest-features "features" (or-empty identity))
  (dependencies manifest-dependencies "dependencies" (or-empty scm->dependency-list))
  ;; For tests, examples and benchmarks
  (dev-dependencies manifest-dev-dependencies "dev-dependencies" (or-empty scm->dependency-list))
  ;; For build scripts
  (build-dependencies manifest-build-dependencies "build-dependencies" (or-empty scm->dependency-list))
  (target manifest-target-specific "target"
	  ;; list of <target-specific>
	  (or-empty
	   (lambda (s)
	     (map (match-lambda
		    ((key . value)
		     (scm->target-specific
		      `(("target" . ,key) ,@value))))
		  s)))))

(define (convert-toml->json from to)
  (invoke "python3" "-c"
	  "import sys, toml, json
here = sys.argv[1]; there = sys.argv[2];
t = toml.load(here);
with open(there, \"w\") as out_file:
	json.dump(t, out_file);"
	  from to))

(define (open-manifest toml json)
  (convert-toml->json toml json)
  (define parsed
    (call-with-input-file json
      (lambda (port)
	(json->scm port))
      #:encoding "UTF-8"))
  (scm->manifest parsed))



;;
;; State.
;;

;; Set in the 'choose-features' phase.  Can be extended in later
;; (package-specific) phases, until the 'make-feature-closure'
;; (TODO build.rs) phase.
(define *features* '())
(define *configuration* '()) ;; set by 'configure'
(define *extra-arguments* '()) ; likewise (TODO doc)

;; TODO: inputs/native-inputs distinction
(define *c-libraries* '())
(define *c-library-directories* '())

;; Initialised by the 'load-manifest' phase.
(define *manifest* #false)



(define (normalise-crate-name name)
  (string-replace-substring name "-" "_"))

(define (crate-directory store-item)
  (string-append store-item "/lib/guixcrate"))

(define* (library-destination crate-name type #:key outputs #:allow-other-keys)
  (string-append
   (crate-directory (or (assoc-ref outputs "lib")
			(assoc-ref outputs "out")))
   "/lib" crate-name "." type))
  
(define (find-crates inputs)
  (append-map (lambda (store-item)
		(if (file-exists? (crate-directory store-item))
		    ;; rlib: Rust's static library format, currently the default
		    ;; so: shared library, used for proc-macro
		    (find-files (crate-directory store-item) "\\.(rlib|so)$")
		    '()))
	      ;; Delete duplicates that can happen when compiling natively, to avoid
	      ;; E0519.
	      (delete-duplicates (map cdr inputs) string=?)))

(define (extract-crate-name lib)
  (string-drop
   (string-drop-right (basename lib)
		      (cond ((string-suffix? ".rlib" lib)
			     (string-length ".rlib"))
			    ((string-suffix? ".so" lib)
			     (string-length ".so"))
			    (#true
			     (format #t "Unrecognised: ~a~%" lib))))
   (string-length "lib")))

(define (extern-arguments crates allowed-crates)
  (filter-map (lambda (crate)
		(define name (extract-crate-name crate))
		(and (member name allowed-crates)
		     (string-append "--extern=" (extract-crate-name crate)
				    "=" crate)))
	      crates))

(define (L-arguments crates)
  (delete-duplicates
   (map (lambda (crate)
	  (string-append "-L" (dirname crate)))
	crates)
   string=?))

(define (configuration-arguments configuration)
  (append-map (lambda (cfg)
		(list "--cfg" cfg))
	      configuration))

;; TODO: support static libraries instead of only .so/dylib
(define (l-arguments c-libraries)
  (append-map (lambda (l) (list "-l" l)) c-libraries))

(define* (compile-rust source destination extra-arguments
		       #:key inputs native-inputs outputs
		       (configuration '())
		       (self-crates? #false)
		       (c-libraries *c-libraries*)
		       ;; TODO: don't use 'extra-arguments' for this
		       (c-library-directories *c-library-directories*)
		       (extern-crates '())
		       #:allow-other-keys)
  (define crates (find-crates (append (if self-crates?
					  outputs
					  '())
				      inputs (or native-inputs '()))))
  (mkdir-p (dirname destination))
  (apply invoke
	 "rustc" "--verbose"
	 ;; Cargo adds '--extern=proc_macro' by default,
	 ;; see <https://github.com/rust-lang/cargo/pull/7700>.
	 ;; Make sure that it will be used.
	 "--extern=proc_macro"
	 "--cap-lints" "warn" ;; ignore #[deny(warnings)], it's too noisy
	 "-C" "prefer-dynamic" ;; for C dependencies & grafting and such?
	 source "-o" destination
	 (append (extern-arguments crates extern-crates)
		 (L-arguments (append crates
				      (map (cut string-append <> "/remove-me")
					   c-library-directories)))
		 (configuration-arguments configuration)
		 (l-arguments c-libraries)
		 extra-arguments)))

(define* (compile-rust-library source destination crate-name extra-arguments
			       #:key (crate-type "rlib")
			       #:allow-other-keys
			       #:rest arguments)
  ;; TODO: why rlib?  Because that works.  Maybe dylib works too?
  (apply compile-rust source destination
	 (append (list (string-append "--crate-name=" crate-name)
		       (string-append "--crate-type=" crate-type))
		 extra-arguments)
	 arguments))

(define* (compile-rust-binary source destination extra-arguments
			      #:key outputs #:allow-other-keys
			      #:rest arguments)
  (apply compile-rust source destination
	 (append (list "--crate-type=bin")
		 extra-arguments)
	 #:self-crates? #true ; required by hexyl
	 arguments))



;;;
;;; Features.
;;;
(define (features-closure features features-section)
  "Include features and the features implied by those features and so on."
  (define new-features
    (delete-duplicates
     ;; lists are not sets, and the order is irrelevant here, so
     ;; pick some fixed arbitrary order.
     (sort-list
      (append-map (lambda (feature)
		    (define extra
		      (vector->list
		       (or (assoc-ref features-section feature) #())))
		    (cons feature extra))
		  features)
      string<?)))
  (if (equal? features new-features)
      ;; fixpoint has been reached
      features
      (features-closure new-features features-section)))

(define (feature->config feature)
  ;; TODO: escapes?
  (string-append "feature=\"" feature "\""))

(define* (choose-features #:key (features '("default")) #:allow-other-keys)
  "Initialise *features* according to #:features.  By default, this enables
the \"default\" feature, and the later 'make-feature-closure' will enable all
default features implied by the \"default\" feature."
  (format #t "Using the features ~a and their implied features.~%" features)
  (set! *features* (append features *features*)))

(define (make-features-closure . _)
  (set! *features* (features-closure *features* (manifest-features *manifest*)))
  (format #t "The following features will be used: ~a~%." *features*))



;; If too many crates are included in --extern, errors like
;; error[E0659]: `time` is ambiguous (name vs any other name during import resolution)
;; are possible.  Avoid them!
(define (manifest-all-dependencies manifest)
  "Return a list of Crate names that are dependencies"
  ;; TODO: split dev, build & dependencies
  ;; For now ignore which target a dependency is for.
  (define (all-target-specific-dependencies target-specific)
    (append (target-specific-dependencies target-specific)
	    (target-specific-dev-dependencies target-specific)
	    (target-specific-build-dependencies target-specific)))
  (define dependencies
    (append (manifest-dependencies manifest)
	    (manifest-dev-dependencies manifest)
	    (manifest-build-dependencies manifest)
	    (append-map all-target-specific-dependencies
			(manifest-target-specific manifest))))
  (map (compose normalise-crate-name dependency-name) dependencies))

;; Some cargo:??? lines from build.rs are ‘propagated’ to dependencies
;; as environment variables, see
;; <https://doc.rust-lang.org/cargo/reference/build-script-examples.html>.
(define* (read-dependency-environment-variables
	  #:key (inputs '())
	  (native-inputs '())
	  #:allow-other-keys)
  (define (setenv* x y)
    (format #t "setting ~a to ~a~%" x y)
    (setenv x y))
  (define (do* stuff c-libraries?)
    (format #t "reading extra environment variables from ~a~%" stuff)
    (for-each
     (match-lambda
       ((x y) (setenv*
	       (string-replace-substring
		(string-upcase
		 (string-append
		  "DEP_"
		  (string-drop-right
		   (basename stuff) ; the 'link' name
		   (string-length ".propagated-environment"))
		  "_"
		  x))
		"-"
		"_")
	       y)
	;; Currently, shared libraries are not supported, and static libraries
	;; do not appear to have an equivalent to ELF's NEEDED, so we have to
	;; mannually ‘propagate’ the -l and -L flags.
	(match x
	  ("rustc-link-lib"
	   (set! *c-libraries* (cons y *c-libraries*)))
	  ("rustc-link-search"
	   (unless (string-prefix? "/tmp" y) ;; TODO: don't include /tmp/guix-build things in propagated-environment?
	     (set! *c-library-directories* (cons y *c-library-directories*))))
	  (_ #false))))
     (call-with-input-file stuff read #:encoding "UTF-8")))
  (define* (do stuff #:optional (c-libraries? #false))
    (match stuff
      ((name . input)
       (define where (string-append input "/lib/guixlinks"))
       ;; Rustc knows how to find glibc anyway, and including
       ;; these in -L cause ‘undefined reference to symbol '__tls_get_addr@@GLIBC_2.3'.
       ;;
       ;; Including libc:static causes Rust to try use the static library,
       ;; which causes relocation errors.
       (when (and (not (member name '("libc:static" "libc")))
		  (directory-exists? (string-append input "/lib")))
	 ;; TODO: only if there are .so or .a, to reduce command-line
	 ;; length?  TODO: why does rustc not recognise LIBRARY_PATH?
	 (set! *c-library-directories*
	       (cons (string-append input "/lib") *c-library-directories*)))
       (when (file-exists? where)
	 (for-each (cut do* <> c-libraries?)
		   (find-files where "\\.propagated-environment$"))))))
  (define (do/with-libraries input) (do input #true))
  (for-each do native-inputs) ;; TODO: make propagated c libraries available to build.rs, _if_ they are native-inputs
  (set! *c-library-directories* (append *c-library-directories*))
  (for-each do/with-libraries inputs)
  (set! *c-library-directories* (delete-duplicates *c-library-directories*))
  (set! *c-libraries* (delete-duplicates *c-libraries*)))

(define* (save-environment-variables link-name saved-settings
				     #:key outputs #:allow-other-keys)
  (define where (string-append (or (assoc-ref outputs "env")
				   (assoc-ref outputs "lib")
				   (assoc-ref outputs "out")) ;; maybe switch the last two?
			       "/lib/guixlinks/" link-name ".propagated-environment"))
  (unless (null? saved-settings)
    ;; TODO: maybe filter out uninteresting things like core-rerun-if-changed?
    (format #t "Saving gathered environment variables to ~a~%" where)
    (mkdir-p (dirname where))
    (call-with-output-file where
      (lambda (o) (write saved-settings o))
      #:encoding "UTF-8")))

(define* (configure #:key target build optimisation-level #:allow-other-keys #:rest arguments)
  (define saved-settings '())
  (define extra-configuration '()) ; --cfg options, computed by build.rs
  (define (handle-line line)
    (when (string-prefix? "cargo:" line)
      (let* ((rest (string-drop line (string-length "cargo:")))
	     (=-index (string-index rest #\=)))
	(if =-index
	    (let ((this (substring rest 0 =-index))
		  (that (substring rest (+ 1 =-index))))
	      (set! saved-settings (cons (list this that) saved-settings)))
	    (begin
	      (pk 'l rest)
	      (error "cargo: line doesn't look right, = missing?")))))
    (cond ((string-prefix? "cargo:rustc-cfg=" line)
	   (format #t "Building with --cfg ~a~%" line) ;; todo invalid
	   (set! extra-configuration
		 (cons (string-drop line (string-length "cargo:rustc-cfg="))
		       extra-configuration)))
	  ((string-prefix? "cargo:rustc-link-lib=" line)
	   (let ((c-library (string-drop line (string-length "cargo:rustc-link-lib="))))
	     (format #t "Building with C library ~a~%" c-library)
	     (set! *c-libraries* (cons c-library *c-libraries*))))
	  ((string-prefix? "cargo:rustc-link-search=" line)
	   (set! *extra-arguments*
		 `("-L" ,(string-drop line (string-length "cargo:rustc-link-search="))
		   ,@*extra-arguments*)))
	  ((string-prefix? "cargo:rustc-env=" line)
	   (putenv (string-drop line (string-length "cargo:rustc-env="))))
	  ((string-prefix? "cargo:warning=" line)
	   (format (current-error-port)
		   "configuration script: warning: ~a~%"
		   (string-drop line (string-length "cargo:warning="))))
	  ((string-prefix? "cargo:" line)
	   (pk 'l line)
	   (format #t "warning: ~a: unrecognised build.rs instruction~%" line)
	   (format #t "hint: maybe the crate is just saving an environment variable for dependencies, maybe nothing needs to be changed.\n"))
	  ;; Some build.rs (e.g. the one of rust-pico-sys)
	  ;; print strings like "TARGET = Some(\"TARGET\")". Maybe
	  ;; they are just debugging information that can be ignored
	  ;; by cargo -- err, antioxidant.
	  (#true
	   (format #t "info from build.rs: ~a~%" line))))

  (setenv "CARGO_MANIFEST_DIR" (getcwd)) ; directory containing the Cargo.toml
  (define package (manifest-package *manifest*))
  (define build.rs
    (or (package-build package)
	;; E.g, rust-proc-macros2 doesn't set 'build'
	;; even though it has a configure script.
	(and (file-exists? "build.rs") "build.rs")))
  (when build.rs
    (format #t "building configuration script~%")
    (apply
     compile-rust-binary build.rs "configuration-script"
     (list (string-append "--edition=" (package-edition package)))
     (append arguments
	     (list #:extern-crates (manifest-all-dependencies *manifest*) ;; TODO: only build dependencies?
		   #:configuration (map feature->config *features*))))
    ;; Expected by rust-const-fn's build.rs
    (setenv "OUT_DIR" (getcwd))
    ;; Expected by rust-libm's build.rs
    (setenv "OPT_LEVEL" (if (number? optimisation-level)
			    (number->string optimisation-level)
			    optimisation-level))
    ;; Expected by some configuration scripts, e.g. rust-libc
    (setenv "RUSTC" (which "rustc"))
    ;; This improves error messages
    (setenv "RUST_BACKTRACE" "full")
    ;; rust-indexmap expectes this to be set (TODO: this is rather ad-hoc)
    (setenv "CARGO_FEATURE_STD" "")
    (setenv "TARGET" target) ; used by rust-proc-macro2's build.rs
    (setenv "HOST" build) ; used by rust-pico-sys
    ;; TODO: use pipes
    (format #t "running configuration script~%")
    (unless (= 0 (system "./configuration-script > .guix-config"))
      (error "configuration script failed"))
    (call-with-input-file ".guix-config"
      (lambda (port)
	(let loop ((r (get-line port)))
	  (match r
	    ((? string? line) (handle-line line) (loop (get-line port)))
	    ((? eof-object? line) (values)))))))
  (set! *configuration* (append extra-configuration (map feature->config *features*)))
  (let ((link (package-links package)))
    (when link
      (apply save-environment-variables link saved-settings arguments)))
  (format #t "Building with configuration options: ~a~%" *configuration*))

(define* (build . arguments)
  "Build the Rust crates (library) described in Cargo.toml."
  ;; Tested for: rust-cfg-il, rust-libc (TODO: more)
  (let* ((package (manifest-package *manifest*))
	 (extern-crates (manifest-all-dependencies *manifest*)) ;; todo only ??? crates
	 (crate-name (normalise-crate-name (package-name package)))
	 (edition (package-edition package))
	 (lib (manifest-lib *manifest*))
	 ;; Location of the crate source code to compile.
	 ;; The default location is src/lib.rs, some packages put
	 ;; the code elsewhere.
	 (lib-path (or (and=> lib target-path) "src/lib.rs"))
	 ;; TODO: which one is it?  (For rust-derive-arbitrary,
	 ;; it is proc_macro)
	 (lib-procedural-macro? (and=> lib target-proc-macro)))
    ;; TODO: implement proper library/binary autodiscovery as described in
    ;; <https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery>.
    (apply compile-rust-library lib-path
	   (apply library-destination crate-name
		  (if lib-procedural-macro?
		      "so"
		      "rlib")
		  arguments)
	   (normalise-crate-name (package-name package))
	   ;; Version of the Rust language (cf. -std=c11)
	   ;; -- required by rust-proc-macro2
	   (list (string-append "--edition=" (package-edition package))
		 ;; Some build.rs put libraries in the current directory
		 ;; (or, at least, in OUT_DIR or something like that).
		 ;; TODO: can be done tidier.
		 (string-append "-Lnative=" (getcwd)))
	   #:crate-type (if lib-procedural-macro?
			    "proc-macro"
			    "rlib")
	   #:extern-crates extern-crates
	   ;; TODO: does the order matter?
	   (append arguments (list #:configuration *configuration*)))))

(define* (build-binaries #:key outputs #:allow-other-keys #:rest arguments)
  "Compile the Rust binaries described in Cargo.toml"
  (define package (manifest-package *manifest*))
  (define files-visited '())
  (define extern-crates (manifest-all-dependencies *manifest*)) ;; TODO: only ??? dependencies
  (define (binary-location binary)
    (string-append (or (assoc-ref outputs "bin")
		       (assoc-ref outputs "out"))
		   "/bin/" binary))
  (define* (cb source binary edition)
    (apply compile-rust-binary source
	   (binary-location binary)
	   (list (string-append "--edition=" edition)
		 (string-append "-Lnative=" (getcwd)))
	   ;; A program can use its own crate without declaring it.
	   ;; At least, hexyl tries to do so.
	   #:extern-crates (cons (normalise-crate-name (package-name package))
				 extern-crates)
	   ;; TODO: figure out how to override things
	   (append
	    arguments
	    (list #:configuration *configuration*))))
  ;; TODO: respect required-features.
  (define (compile-bin-target target)
    (define source (or (target-path target) "src/main.rs"))
    (set! files-visited (cons source files-visited))
    (if (lset<= string=? (target-required-features target) *features*)
	(begin
	  (format #t "Compiling ~a~%" source)
	  (cb source (or (target-name target) (package-name package))
	      (or (target-edition target) (package-edition package))))
	(format #t "not compiling ~a, because the following features are missing: ~a~%"
		source (lset-difference string=?
					(target-required-features target)
					*features*))))
  (for-each compile-bin-target (manifest-bin *manifest*))
  (when (package-autobins package)
    (for-each ;; TODO: support [[bin]]
     (lambda (file)
       (when (and (string-suffix? ".rs" file)
		  ;; Possibly the binary was already in [[bin]]
		  ;; and hence is pointless to compile again.
		  (not (member file files-visited)))
	 (cb file (string-drop-right (basename file)
				     (string-length ".rs"))
	     (package-edition package))))
     (append
      (if (file-exists? "src/main.rs") '("src/main.rs") '())
      (find-files "src/bin")))))

(define* (load-manifest . rest)
  "Parse Cargo.toml and save it in @code{*manifest*}."
  (set! *manifest* (open-manifest "Cargo.toml" "Cargo.json")))

;; Set some variables that Cargo can set and that might
;; be expected by build.rs.  A (full?) list is avialable
;; at <https://doc.rust-lang.org/cargo/reference/environment-variables.html>.
;; When something does not appear in the Cargo.toml or such, according to
;; that documentation, the environment variable needs to be set to the empty
;; string.
(define (set-platform-independent-manifest-variables . _)
  (define package (manifest-package *manifest*))
  ;; Used by rust-cmake.  TODO: actually set the various profile flags,
  ;; optimisation levels, ...
  (setenv "PROFILE" "release")
  (setenv "DEBUG" "true")
  (let ((set-version-environment-variables
	 (lambda (major minor patch pre)
	   (setenv "CARGO_PKG_VERSION_MAJOR" major)
	   (setenv "CARGO_PKG_VERSION_MINOR" minor)
	   (setenv "CARGO_PKG_VERSION_PATCH" patch)
	   (setenv "CARGO_PKG_VERSION_PRE" pre))))
    (match (string-split (package-version package) #\.)
      ((major minor patch pre)
       (set-version-environment-variables major minor patch pre))
      ((major minor patch)
       (set-version-environment-variables major minor patch ""))
      ((major minor)
       (set-version-environment-variables major minor "" ""))
      ((major)
       (set-version-environment-variables major "" "" ""))
      (() ; not set in Cargo.toml
       (set-version-environment-variables "" "" "" ""))))
  (setenv "CARGO_PKG_VERSION" (package-version package))
  (setenv "CARGO_PKG_AUTHORS" (string-join (package-authors package) ":"))
  (setenv "CARGO_PKG_NAME" (package-name package))
  (setenv "CARGO_PKG_DESCRIPTION" (package-description package))
  (setenv "CARGO_PKG_HOMEPAGE" (package-homepage package))
  (setenv "CARGO_PKG_REPOSITORY" (package-repository package))
  (setenv "CARGO_PKG_LICENSE" (package-license package))
  (setenv "CARGO_PKG_LICENSE_FILE" (package-license-file package)))

(define* (set-platform-dependent-variables #:key cargo-env-variables
					   #:allow-other-keys)
  "Set environment variables like CARGO_CFG_TARGET_POINTER_WIDTH and
CARGO_CFG_TARGET_ARCH."
  (for-each (match-lambda ((name . value) (setenv name value)))
	    cargo-env-variables)) ; TODO: maybe move more things inside

(define* (install-pwd-libraries #:key outputs #:allow-other-keys)
  ;; TODO: necessary for rust-sct@0.7?
  (define p (file-name-predicate "\\.(a|so)$"))
  (for-each (cute install-file <>
		 (string-append (or (assoc-ref outputs "lib")
				    (assoc-ref outputs "out"))
				"/lib"))
	    ((@ (ice-9 ftw) scandir) "." (cut p <> 'unused))))

(define %standard-antioxidant-phases
  (modify-phases %standard-phases
    ;; TODO: before configure?
    (add-after 'unpack 'make-features-closure make-features-closure)
    (add-after 'unpack 'choose-features choose-features)
    (add-after 'unpack 'read-dependency-environment-variables read-dependency-environment-variables)
    (add-after 'unpack 'set-platform-independent-manifest-variables
	       set-platform-independent-manifest-variables)
    (add-after 'unpack 'set-platform-dependent-variables set-platform-dependent-variables)
    (add-after 'unpack 'load-manifest load-manifest)
    (replace 'configure configure)
    (replace 'build build)
    (add-after 'build 'build-binaries build-binaries)
    (delete 'check) ; TODO
    (delete 'install) ; TODO?
    (add-after 'install 'install-pwd-libraries install-pwd-libraries))) ; TODO: necessary?

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
  #:export (find-directly-available-crates
	    crate-directory extract-crate-name extern-arguments
	    L-arguments/non-rustc
	    l-arguments/non-rustc
	    linker-arguments/non-rustc
	    *manifest*
	    L-arguments compile-rust compile-rust-library
	    compile-rust-binary compile-cargo
	    read-dependency-environment-variables
	    determine-crate-type
	    %standard-antioxidant-phases
	    %default-crate-type

	    make-manifest manifest? scm->manifest manifest->scm
	    manifest-lib
	    manifest-bin
	    manifest-bench
	    manifest-example
	    manifest-test
	    manifest-features
	    manifest-dependencies
	    manifest-dev-dependencies
	    manifest-build-dependencies
	    manifest-target-specific

	    crate-mapping?
	    make-crate-mapping
	    crate-mapping-dependency-name
	    crate-mapping-local-name

	    elaborate-target
	    elaborate-target/skip
	    elaborated-target?
	    find-rust-binaries
	    compile-binary-target

	    rust-tests-check
	    rust-tests-check/xorg)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34) ; or is the RNRS preferred?
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:declarative? #false) ;; allow @@ if required

;; The default crate type (TODO: switch to dylib?)
;; TODO: why rlib?  Because that works.  Maybe dylib works too?
(define %default-crate-type "rlib")

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
(define or-true* ((or-constant #true) identity))
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
  ;; 'name' is the name of the crate, inside the current Rust project.
  ;; By default, the name inside the crate is the name ooutside the crate.
  ;; However, a crate can choose to use a crate that names itself 'foo'
  ;; but use it as-if it was named 'bar', by setting 'name' to "bar"
  ;; and 'package' to "foo".
  ;;
  ;; 'name' is not actually part of the JSON / TOML.
  (name dependency-name) ; string
  (package dependency-package "package" or-false*) ; string | #false
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
  (name target-name "name" or-false*)
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
  (crate-type target-crate-type
              "crate-type"
              ((or-constant (list %default-crate-type))
		 (lambda (x)
		  (if (string? x)
		      (list x)
		      (vector->list x)))))
  ;; NA for [lib]
  (required-features target-required-features "required-features"
		     (or-empty vector->list)))

(define (elaborated-target? target)
  (and (target-name target)
       (target-path target)
       (target-edition target)))

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
  %json->target-specific <=> %manifest->target-specific <=> %scm->target-specific <=> target-specific->scm
  (target %target-specific-target) ; string, not actually part of the json
  (dependencies target-specific-dependencies "dependencies" (or-empty scm->dependency-list))
  ;; For tests, examples and benchmarks
  (dev-dependencies target-specific-dev-dependencies "dev-dependencies" (or-empty scm->dependency-list))
  ;; For build scripts
  (build-dependencies target-specific-build-dependencies "build-dependencies" (or-empty scm->dependency-list)))

(define (fixup-section-names scm)
  ;; Some packages, e.g. rust-smallvec, use dev_dependencies instead of dev-dependencies.
  ;; (TODO: maybe this is the same thing as the proc-macro / proc_macro?)
  ;; (That was in the Cargo.toml.orig and not in the Cargo.toml, but packages don't have
  ;; to be from crates.io)
  (define fixup-section-name
    (match-lambda
     (("dev_dependencies" . things) (cons "dev-dependencies" things))
     (("build_dependencies" . things) (cons "build-dependencies" things))
     (foo foo)))
  (map fixup-section-name scm))

(define scm->target-specific (compose %scm->target-specific fixup-section-names))

(define-json-mapping <manifest> make-manifest manifest?
  %json->manifest <=> %manifest->json <=> %scm->manifest <=> manifest->scm
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

(define scm->manifest (compose %scm->manifest fixup-section-names))

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

;; Packages to test when modifying these two procedures:
;;  * rust-clang-sys
;;  * rust-seccomp-sys
;;  * rust-bindgen
;;  * mayb other -sys crates
(define* (add-c-library! library)
  "Link the crate to be compiled against C-LIBRARY -- i.e., do the rust
equivalent of adding \"-lLIBRARY ...\" to the invocation of \"gcc\"."
  (let ((corrected-library
	 (cond ((string-suffix? ".so" library) ; happens for rust-jemalloc-sys@0.3
		(format #t "note: the build script explicitly included a .so suffix (~a) for the shared library. We cannot pass that to the linker, so the suffix is removed.~%" library)
		(string-drop-right library (string-length ".so")))
	       ((string-suffix? ".a" library) ; not yet encountered in practice
		(format #t "note: the build script explicitly included a .a suffix (~a) for the shared library. We cannot pass that to the linker, so the suffix is removed.~%" library)
		(string-drop-right library (string-length ".a")))
	       ;; TODO: .a case?
	       (#true library))))
    (set! *c-libraries* (cons corrected-library *c-libraries*))))

(define* (add-c-library-directory! library-directory)
  "Search for non-Rust libraries in LIBRARY-DIRECTORY -- i.e., do the rust
equivalent of adding \"-LLIBRARY_DIRECTORY\" to the invocation of \"gcc\"."
  (set! *c-library-directories* (cons library-directory *c-library-directories*)))



;;
;; Information on how to use a crate.
;;

;; <crate-information> loaded with 'load-crate-information' can be compared with eq?.
;; By default, it is assumed <crate-information> is loaded with that.
(define-json-mapping <crate-information> make-crate-information crate-information?
  json->crate-information <=> crate-information->json <=>
  scm->crate-information <=> crate-information->scm
  ;; The following two fields are usually but not always the same:
  ;; for rust-debug-unreachable, the first in "debug_unreachable"
  ;; and the second is "new_debug_unreachable".
  (name crate-information-name) ; string, name of the crate (normalised)
  (dependency-name crate-information-dependency-name) ; string, name of the crate put as listed in the dependency information
  (link crate-information-link) ; string
  ;; Where is the crate (as .rlib or .so or such) located in the file system?
  ;; (TODO: check that it's absolute)
  (location crate-information-location) ; string
  ;; Extra libraries to add (as -l arguments) to compile depending crates.
  ;; static= prefixes are allowed.
  (libraries crate-information-libraries "libraries" vector->list list->vector)
  ;; List of directory names to search for the libraries -- without native=
  ;; prefixes or such!
  ;; TODO: check that they are absolute.
  (library-directories crate-information-library-directories "library-directories" vector->list list->vector)
  ;; List of file names of the (non-test, non-build, non-dev) dependencies of
  ;; this crate -- the file names point to a <crate-information> JSON.
  (dependencies crate-information-dependencies "dependencies" vector->list list->vector)
  (environment crate-information-environment)) ;; TODO

(define *known-crate-information* (make-hash-table)) ; file name -> <crate-information>
(define *crate-information->file-name* (make-hash-table))
(define (load-crate-information location)
  (match (hash-ref *known-crate-information* location)
    (#f (let ((parsed
	       (scm->crate-information
		(call-with-input-file location
		  json->scm
		  #:encoding "UTF-8"))))
	  (hash-set! *known-crate-information* location parsed)
	  (hashq-set! *crate-information->file-name* parsed location)
	  parsed))
    ((? crate-information? info) info)))
(define (crate-information->file-name crate-info)
  (or (hashq-ref *crate-information->file-name* crate-info)
      (error (pk 'crate-info crate-info "unknown crate info"))))

;; Crate names are normalised by the constructor.
(define-record-type (<crate-mapping> %make-crate-mapping crate-mapping?)
  ;; From which crate package does the crate come?  This is usually, but
  ;; not always, the same as the name of the crate.
  ;; For 'rust-debug-unreachable', this is "new_debug_unreachable".
  (fields (immutable dependency-name crate-mapping-dependency-name) ; string
	  ;; What does the crate that is using this crate
	  ;; expect as name (for 'extern ...')?  If #false,
	  ;; default to the crate name (for rust-debug-unreachable,
	  ;; that is "debug_unreachable").
	  (immutable local-name %crate-mapping-local-name) ; string | #false
	  ))

(define crate-mapping-local-name
  (case-lambda
    ((crate-mapping)
     (or (%crate-mapping-local-name crate-mapping)
	 (error "desired name of crate unknown, pass a <crate-information> to elaborate")))
    ((crate-mapping crate)
     (unless (crate-mapping? crate-mapping)
       (error "argument not a <crate-mapping>"))
     (unless (crate-information? crate)
       (error "argument not a <crate-information>"))
     (or (%crate-mapping-local-name crate-mapping)
	 (crate-information-name crate)))))

(define (make-crate-mapping dependency-name local-name)
  (%make-crate-mapping (normalise-crate-name dependency-name)
		       (and=> local-name normalise-crate-name)))

(define (normalise-crate-name name)
  (string-replace-substring name "-" "_"))

(define (crate-name-of-manifest manifest)
  "Return the crate name of the crate specified in MANIFEST."
  ;; The 'rust-new-debug-unreachable' crate uses the name
  ;; 'debug_unreachable' and not 'new_debug_unreachable'.
  ;; So when available, use (target-name lib), otherwise
  ;; the build of rust-string-cache@0.8.0 fails.
  (let ((package (manifest-package *manifest*))
	(lib (manifest-lib *manifest*)))
    (or (and=> lib target-name)
	(normalise-crate-name (package-name package)))))

(define (partition-crates available-crates crate-mappings)
  ;; First return value: direct dependencies
  ;; Second return value: indirect dependencies (can contain things not in available-crates!)
  ;; Third return value: all things in available-crates not in the previous.
  ;;
  ;; Direct and indirect dependencies can overlap (e.g.: rust-syn@1.0.82)
  (define direct
    (filter (lambda (crate-information)
	      (any (cut match? crate-information <>) crate-mappings))
	    available-crates))
  (define (find-indirect from append-to)
    (define (f crate-information)
      (map load-crate-information
	   (crate-information-dependencies crate-information)))
    (delete-duplicates (append (append-map f from) append-to) eq?))
  (let loop ((indirect (find-indirect direct '())))
    (let ((next (find-indirect indirect indirect)))
      (if (equal? indirect next) ; fixpoint reached
	  (values direct indirect
		  (lset-difference eq? available-crates
				   (lset-union eq? direct indirect)))
	  (loop next)))))

(define (filter-used-crates available-crates crate-mappings)
  (let* ((direct indirect rest (partition-crates available-crates crate-mappings)))
    (append direct indirect)))

(define (find-directly-available-crates inputs)
  (append-map (match-lambda
		((_ . input)
		 (let ((dir (string-append input "/lib/guixcrate")))
		   (if (directory-exists? dir)
		       (map load-crate-information
			    (find-files dir "\\.crate-info"))
		       '()))))
	      inputs))



(define (crate-directory store-item)
  (string-append store-item "/lib/guixcrate"))

(define* (crate-library-destination crate-name type #:key outputs #:allow-other-keys)
  (string-append
   (crate-directory (or (assoc-ref outputs "lib")
			(assoc-ref outputs "out")))
   "/lib" crate-name "." type))

(define* (c-library-destination crate-name type #:key outputs #:allow-other-keys)
  (string-append
   (or (assoc-ref outputs "lib")
       (assoc-ref outputs "out"))
   "/lib/lib" crate-name "." type)) ; type = ".a" / ".so"

(define (extract-crate-name lib)
  (string-drop
   (string-drop-right (basename lib)
		      (cond ((string-suffix? ".rlib" lib)
			     (string-length ".rlib"))
			    ((string-suffix? ".so" lib)
			     (string-length ".so"))
			    ((string-suffix? ".a" lib)
			     (string-length ".a"))
			    (#true
			     (format #t "Unrecognised: ~a~%" lib))))
   (string-length "lib")))

(define (match? crate-information crate-mapping)
  (string=? (crate-mapping-dependency-name crate-mapping)
	    (crate-information-dependency-name crate-information)))

(define (extern-arguments available-crates crate-mappings)
  (define (process-mapping crate-mapping)
    (define (do crate)
      (string-append "--extern=" (crate-mapping-local-name crate-mapping crate)
		     "=" (crate-information-location crate)))
    ;; Search for a matchin crate
    (match (filter (cut match? <> crate-mapping) available-crates)
      (()
       (format (current-error-port)
	       "warning: ~a not found in the available crates -- this might cause the build to fail!~%"
	       crate-mapping)
       #f)
      ((x) (do x))
      ((x y . rest)
       (format (current-error-port)
	       "warning: multiple candidates for ~a (~a, ~a) in the available crates -- this will probably cause the build to fail!~%"
	       crate-mapping x y)
       (do x))))
  ;; "rustc" will sort out duplicates in crate-mappings (by emitting an error)(?)
  (filter-map process-mapping crate-mappings))

(define* (L-arguments available-crates crate-mappings #:optional
		      (extra-library-directories '()))
  (let* ((direct-dependencies indirect-dependencies rest
			      (partition-crates available-crates crate-mappings))
	 (indirect-crate->argument
	  (lambda (crate-information)
	    (string-append "-Ldependency="
			   (dirname (crate-information-location crate-information)))))
	 ;; No need for -Lcrate, as the full file name is passed to --extern=.
	 (indirect-crate-arguments
	  (map indirect-crate->argument indirect-dependencies))
	 (make-Lnative-argument
	  (lambda (directory)
	    ;; native means something different in rustc than Guix.
	    ;; In Rust, 'native' means non-Rust compiled libraries.
	    (string-append "-Lnative=" directory)))
	 (make-Lnative-arguments*
	  (lambda (crate-information)
	    (map make-Lnative-argument
		 (crate-information-library-directories crate-information))))
	 (Lnative-arguments
	  (append (map make-Lnative-argument extra-library-directories)
		  ;; Only use crates that are actually (indirectly) requested.
		  (append-map make-Lnative-arguments*
			      (append direct-dependencies indirect-dependencies)))))
    ;; Delete duplicates to shrink the invocation of 'rustc' a bit.
    (append (delete-duplicates Lnative-arguments string=?)
	    indirect-crate-arguments))) ; shouldn't contain duplicates

(define (configuration-arguments configuration)
  (append-map (lambda (cfg)
		(list "--cfg" cfg))
	      configuration))

(define* (l-arguments available-crates crate-mappings #:optional
		      (extra-nonrust-libraries '()))
  ;; Only involve crates that are actually requested.
  ;; Result: a list of -lopenssl, -lstatic=ring-test, ..., arguments.
  (let* ((used-dependencies (filter-used-crates available-crates crate-mappings))
	 (library->argument
	  (lambda (library)
	    (string-append "-l" library)))
	 (crate->l-arguments
	  (lambda (crate-information)
	    (map library->argument
		 (crate-information-libraries crate-information)))))
    (delete-duplicates ; shrink invocation of 'rustc'
     (append (map library->argument extra-nonrust-libraries)
	     (append-map crate->l-arguments used-dependencies))
     string=?)))

;; TODO: untested, for newsboat
(define* (L-arguments/non-rustc available-crates crate-mappings)
  "Return a list of -L arguments to be passed to a compiler like gcc to link
to the crates in CRATE-MAPPINGS."
  ;; gcc doesn't make a -Lnative / -Ldependency / -Lcrate distinction
  (let* ((used-dependencies (filter-used-crates available-crates crate-mappings))
	 (make-L-argument
	  (lambda (directory)
	    (string-append "-L" directory)))
	 (compiled-crate-argument ; for linking to the compiled crate itself (.rlib|so|a|...)
	  (lambda (crate-information)
	    (make-L-argument
	     (dirname (crate-information-location crate-information)))))
	 (compiled-crate-arguments
	  (map compiled-crate-argument used-dependencies))
	 (nonrust-library-arguments*
	  (lambda (crate-information)
	    (map make-L-argument
		 (crate-information-library-directories crate-information))))
	 (nonrust-library-arguments
	  ;; Only use crates that are actually (indirectly) requested.
	  (append-map nonrust-library-arguments* used-dependencies)))
    ;; Delete duplicates to shrink the invocation of the C compiler a bit.
    (delete-duplicates (append compiled-crate-arguments nonrust-library-arguments))))

;; TODO: likewise untested!
;; TODO: for cdylib/dylib/staticlib crates, maybe this should include
;;       the crate itself as well in -l?
(define* (l-arguments/non-rustc available-crates crate-mappings)
  "Return a list of -l arguments to be passed to a compiler like gcc to link
to the crates in CRATE-MAPPINGS."
  (define (derustify argument)
    (string-append "-l"
      (string-drop argument
		   (cond ((string-prefix? "-lstatic=" argument)
			  (string-length "-lstatic="))
			 ((string-prefix? "-ldylib=" argument)
			  (string-length "-ldylib="))
			 ((string-prefix? "-lframework=" argument)
			  (error "frameworks not supported"))
			 ((string-prefix? "-l" argument)
			  (string-length "-l"))
			 (#true
			  (pk 'unrecognised argument)
			  (error "unrecognised library argument"))))))
  (delete-duplicates
   (map derustify (l-arguments available-crates crate-mappings))))

(define (linker-arguments/non-rustc available-crates crate-mappings)
  (append (L-arguments/non-rustc available-crates crate-mappings)
	  (l-arguments/non-rustc available-crates crate-mappings)))

(define* (compile-rust source destination extra-arguments
		       #:key inputs native-inputs outputs
		       target
		       (optimisation-level "1")
		       (debuginfo-level "1")
		       (rust-metadata "")
		       (configuration '())
		       (available-crates '())
		       (crate-mappings '())
		       (extra-libraries *c-libraries*)
		       (extra-library-directories *c-library-directories*)
		       #:allow-other-keys)
  (mkdir-p (dirname destination))
  (apply invoke
	 "rustc" "--verbose"
	 "-Z" "macro-backtrace" ; enable backtraces in macros during compilation, can help with debugging.
	 (string-append "--target=" target)
	 "-C" (string-append "opt-level=" optimisation-level)
	 "-C" (string-append "debuginfo=" debuginfo-level)
	 ;; Cargo adds '--extern=proc_macro' by default,
	 ;; see <https://github.com/rust-lang/cargo/pull/7700>.
	 ;; Make sure that it will be used.
	 "--extern=proc_macro"
	 "--cap-lints" "warn" ;; ignore #[deny(warnings)], it's too noisy
	 "-C" "prefer-dynamic" ;; for C dependencies & grafting and such?
	 "-C" (string-append "metadata=" rust-metadata) ;; two crates with the same name can only be used in the same binary if they have different metadata, so allow changing the metadata.
	 source "-o" destination
	 (append (extern-arguments available-crates crate-mappings)
		 (L-arguments available-crates crate-mappings extra-library-directories)
		 (configuration-arguments configuration)
		 (l-arguments available-crates crate-mappings extra-libraries)
		 extra-arguments)))

(define* (compile-rust-library source destination crate-name extra-arguments
			       #:key (crate-type %default-crate-type)
			       (rust-dynamic-library-arguments #f)
			       #:allow-other-keys
			       #:rest arguments)
  (apply compile-rust source destination
	 (append (list (string-append "--crate-name=" crate-name)
		       (string-append "--crate-type=" crate-type))
		 (if (string=? crate-type "cdylib")
		     (or rust-dynamic-library-arguments
			 (error "I don't know what symbols to export or the version of the library, please set #:rust-dynamic-library-arguments"))
		     '())
		 (if (string=? crate-type "dylib") ; TODO: untested!
		     (or rust-dynamic-library-arguments '())
		     '())
		 extra-arguments)
	 arguments))

(define* (compile-rust-binary source destination extra-arguments
			      #:key outputs #:allow-other-keys
			      #:rest arguments)
  (apply compile-rust source destination
	 (append (list "--crate-type=bin")
		 extra-arguments)
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
		      (append
		       (vector->list
			(or (assoc-ref features-section feature) #()))
		       ;; "package-name/feature-name" is used for enabling
		       ;; optional dependencies.  Apparently, when enabling
		       ;; optional dependencies, some crates expect the
		       ;; "package-name" feature to be enabled as well?
		       ;; (at least rust-pkcs1@0.3.3)
		       (match (string-index feature #\/)
			 ((? integer? k)
			  (list (substring feature 0 k)))
			 (#false '()))))
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
  (define maybe-car
    (match-lambda
      (("nightly" . _) #false) ; unlikely to work in Guix, e.g. rust-lock-api@0.4
      (("unstable" . _) #false) ; likewise, e.g. rust-fallible-collections@0.4.2
      (("vendored" . _) #false) ; not desired in Guix (e.g.: rust-libnghttp2-sys)
      (("vendor" . _) #false) ; plausible alternate spelling for same concept
      (("bundle" . _) #false) ; likewise
      ((x . y) x)))
  (match (list (->bool (member "default" features))
	       (->bool (assoc "default" (manifest-features *manifest*))))
    ((#t #f)
     ;; See: https://doc.rust-lang.org/cargo/reference/features.html,
     ;; ‘the default feature’.
     (format #t "The default features are requested but the defaults are not
chosen, enabling all features like Cargo does (except nightly).~%")
     (set! *features* (append (filter-map maybe-car (manifest-features *manifest*))
			      features
			      *features*)))
    ((#f _)
     (format #t "warning: not enabling the default features!~%")
     (format #t "Using the features ~a and their implied features.~%" features)
     (set! *features* (append features *features*)))
    (_
     (format #t "Using the features ~a and their implied features.~%" features)
     (set! *features* (append features *features*))))
  (set! *features* (delete-duplicates *features*)))

(define* (make-features-closure #:key (features '()) #:allow-other-keys)
  (define (forbid-vendoring feature)
    (when (member feature *features*)
      (unless (member feature features)
	(format (current-error-port)
		"The vendoring feature ~a was implicitly enabled, but vendoring is usually considered unacceptable due to reasons, so the build is halted.  To vendor anyway, explicitly enable the feature.~%")
	(exit 1))))
  (set! *features* (features-closure *features* (manifest-features *manifest*)))
  (forbid-vendoring "vendored")
  (forbid-vendoring "vendor")
  (forbid-vendoring "bundle")
  (format #t "The following features will be used: ~a~%." *features*))



;; Fake cargo crates that antioxidant doesn't need
(define %rustc-std-workspace-crates
  (map normalise-crate-name
       '("rustc-std-workspace-std"
	 "rustc-std-workspace-core"
	 "rustc-std-workspace-alloc")))

;; If too many crates are included in --extern, errors like
;; error[E0659]: `time` is ambiguous (name vs any other name during import resolution)
;; are possible.  Avoid them!
(define* (manifest-all-dependencies manifest #:optional (kinds '(dependency dev build)))
  "Return a list of crates that are dependencies, as <crate> records."
  ;; For now ignore which target a dependency is for.
  (define (the-target-specific-dependencies target-specific)
    (append (if (memq 'dependency kinds)
		(target-specific-dependencies target-specific)
		'())
	    (if (memq 'dev kinds)
		(target-specific-dev-dependencies target-specific)
		'())
	    (if (memq 'build kinds)
		(target-specific-build-dependencies target-specific)
		'())))
  (define dependencies
    (append (if (memq 'dependency kinds)
		(manifest-dependencies manifest)
		'())
	    (if (memq 'dev kinds)
		(manifest-dev-dependencies manifest)
		'())
	    (if (memq 'build kinds)
		(manifest-build-dependencies manifest)
		'())
	    (append-map the-target-specific-dependencies
			(manifest-target-specific manifest))))
  (define (construct-crate dependency)
    (make-crate-mapping (or (dependency-package dependency)
			    (dependency-name dependency))
			(and (dependency-package dependency) ; <-- first clause required for rust-new-debug-unreachable / rust-string-cache@0.8.0
			     (dependency-name dependency))))
  (define (fake? mapping) ;; avoid warnings about fake crates being missing
    (member (crate-mapping-dependency-name mapping) %rustc-std-workspace-crates))
  (filter (negate fake?) (map construct-crate dependencies)))

;; Some cargo:??? lines from build.rs are ‘propagated’ to dependencies
;; as environment variables, see
;; <https://doc.rust-lang.org/cargo/reference/build-script-examples.html>.
(define* (read-dependency-environment-variables
	  #:key (inputs '())
	  (native-inputs '())
	  #:allow-other-keys)
  ;; TODO: also for indirect dependencies?
  (define (setenv* x y)
    (format #t "setting ~a to ~a~%" x y)
    (setenv x y))
  (define (drop-native=-prefix directory)
    ;; Strip native= and all= prefixes from 'directory'
    (cond ((string-prefix? "native=" directory)
	   (string-drop directory (string-length "native=")))
	  ((string-prefix? "all=" directory)
	   (string-drop directory (string-length "all=")))
	  (#t directory)))
  (define (do crate-info)
    (unless (null? (crate-information-environment crate-info))
      ;; Don't spam the build log with do-nothing messages
      ;; if there are no actual environment variables to set.
      (format #t "setting extra environment variables in ~a~%"
	      (crate-information->file-name crate-info)))
    (for-each
     (match-lambda
       ((x . y) (setenv*
		 (string-replace-substring
		  (string-upcase
		   (string-append
		    "DEP_"
		    (crate-information-link crate-info)
		    "_"
		    x))
		  "-"
		  "_")
		 y)))
     (crate-information-environment crate-info)))
  (for-each do
	    (find-directly-available-crates (delete-duplicates (append native-inputs inputs)))))

(define* (save-crate-info link-name saved-settings library-destination
			  #:key inputs outputs #:allow-other-keys)
  (define where (string-append (or (assoc-ref outputs "env")
				   (assoc-ref outputs "lib")
				   (assoc-ref outputs "out")) ;; maybe switch the last two?
			       "/lib/guixcrate/" link-name ".crate-info"))
  (define available-crates (find-directly-available-crates inputs))
  (define crate-mappings (manifest-all-dependencies *manifest* '(dependency)))
  (format #t "Saving crate information in ~a~%" where)
  (mkdir-p (dirname where))
  ;; /tmp/guix-build-... directories won't exist after the build is finished,
  ;; so including them is pointless.
  (define (directory-without-prefix dir)
    (cond ((string-prefix? "native=" dir)
	   (string-drop dir (string-length "native=")))
	  ((string-prefix? "all=" dir)
	   (string-drop dir (string-length "all=")))
	  (#t dir)))
  (define (local-directory? dir)
    (string-prefix? (getcwd) (directory-without-prefix dir)))
  ;; If the build.rs compiled a C library and linked it into the crate,
  ;; then at least for cases known at writing, rustc will link the local
  ;; C library into the rlib (rust-sha2-asm@0.6.1), so including them in
  ;; -l later is pointless, especially given that they won't be found later.
  (define (locally-compiled-c-library? foo)
    (let* ((name (if (string-prefix? "static=" foo)
		     (string-drop foo (string-length "static="))
		     foo))
	   (basename (format #f "lib~a.a" name)))
      (define (match? c-library-directory)
	(and (local-directory? c-library-directory)
	     (file-exists? (in-vicinity
			    (directory-without-prefix c-library-directory)
			    basename))))
      ;; rust-sha2-asm doesn't add the current directory to c-library-directories
      ;; even though it adds a static library there.
      (any match? (cons (getcwd) *c-library-directories*))))
  (define filtered-c-libraries
    (filter (negate locally-compiled-c-library?) *c-libraries*))
  (define filtered-library-directories
    (filter (negate local-directory?) *c-library-directories*))
  (call-with-output-file where
    (lambda (o)
      (scm->json
       (crate-information->scm
	(make-crate-information (crate-name-of-manifest *manifest*)
				;; TODO: should the dependency name be normalised?
				(normalise-crate-name (package-name (manifest-package *manifest*)))
				link-name
				*library-destination*
				filtered-c-libraries
				filtered-library-directories
				;; direct dependencies
				(map crate-information->file-name
				     (partition-crates available-crates crate-mappings))
				;; TODO: maybe filter out uninteresting things like
				;; core-rerun-if-changed?
				saved-settings))
       o))
    #:encoding "UTF-8"))

;; To avoid cluttering the .crate-info and to reduce the number of environment
;; variables set, exclude these variables which aren't used by dependents.
;; Not exhaustive.
(define %excluded-keys
  ;; 'include' is used by rust-tectonic-engine-bibtex@0.1.1
  '("rerun-if-env-changed" "rerun-if-changed" "rustc-link-search" "rustc-link-lib"
    "rustc-cfg" "warning"))

(define *save* #false) ;; TODO: less impure
(define* (configure #:key inputs native-inputs target build optimisation-level
		    #:allow-other-keys #:rest arguments)
  (define saved-settings '())
  (define (save! key value)
    "Add a KEY=VALUE mapping to the saved settings, unless it is excluded
by %excluded-keys."
    (unless (member key %excluded-keys)
      (set! saved-settings (cons (cons key value) saved-settings))))
  (define extra-configuration '()) ; --cfg options, computed by build.rs
  (define (handle-line line)
    (when (string-prefix? "cargo:" line)
      (let* ((rest (string-drop line (string-length "cargo:")))
	     (=-index (string-index rest #\=)))
	(if =-index
	    (let ((this (substring rest 0 =-index))
		  (that (substring rest (+ 1 =-index))))
	      (save! this that))
	    (begin
	      (pk 'l rest)
	      (error "cargo: line doesn't look right, = missing?")))))
    (cond ((string-prefix? "cargo:rustc-cfg=" line)
	   (format #t "Building with --cfg ~a~%" line) ;; todo invalid
	   (set! extra-configuration
		 (cons (string-drop line (string-length "cargo:rustc-cfg="))
		       extra-configuration)))
	  ;; The rustc-link-lib and rustc-link-search will be added to the <crate-information>.
	  ((string-prefix? "cargo:rustc-link-lib=" line)
	   (let ((c-library (string-drop line (string-length "cargo:rustc-link-lib="))))
	     (format #t "Building with C library ~a~%" c-library)
	     (add-c-library! c-library)))
	  ((string-prefix? "cargo:rustc-link-search=" line)
	   (let ((KIND=PATH (string-drop line (string-length "cargo:rustc-link-search="))))
	     (cond ((string-prefix? "framework=" KIND=PATH)
		    (error "framework not yet supported"))
		   ((string-prefix? "native=" KIND=PATH)
		    (add-c-library-directory! (string-drop KIND=PATH (string-length "native="))))
		   ((string-prefix? "all=" KIND=PATH)
		    ;; Note (Cargo incompatibility?): technically the build.rs could ask us
		    ;; here to search for crates in some arbitrary directories (instead of
		    ;; only C-style libraries), but no crate(™) does that (so far ...)
		    (add-c-library-directory! (string-drop KIND=PATH (string-length "=all"))))
		   ((or (string-prefix? "crate=" KIND=PATH)
			(string-prefix? "dependency=" KIND=PATH))
		    (error "The build script is not supposed to ask to look into arbitrary locations for crates."))
		   (#true
		    (add-c-library-directory! KIND=PATH)))))
	  ((string-prefix? "cargo:rustc-env=" line)
	   (putenv (string-drop line (string-length "cargo:rustc-env="))))
	  ((string-prefix? "cargo:warning=" line)
	   (format (current-error-port)
		   "configuration script: warning: ~a~%"
		   (string-drop line (string-length "cargo:warning="))))
	  ((or (string-prefix? "cargo:rerun-if-changed=" line)
	       (string-prefix? "cargo:rerun-if-env-changed=" line))
	   (values)) ; nothing to do for antioxidant, no need for a warning
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
  (define (set-feature-environment-variable! feature)
    ;; Some crates, e.g. rust-indexmap and rust-wayland-protocols
    ;; expect CARGO_FEATURE_... environment variables to be set.  See:
    ;; <https://doc.rust-lang.org/cargo/reference/features.html#build-scripts>.
    (setenv (string-append "CARGO_FEATURE_"
			   (string-replace-substring
			    (string-upcase feature) "-" "_"))
	    "1"))
  (when build.rs
    (format #t "building configuration script~%")
    (apply
     compile-rust-binary build.rs "configuration-script"
     (list (string-append "--edition=" (package-edition package)))
     (append arguments
	     ;; In Cargo, the build script _does not_ have access to dependencies
	     ;; in 'dependencies' or 'dev-dependencies', only 'build-dependencies',
	     ;; see
	     ;; <https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html>.
	     (list #:crate-mappings (manifest-all-dependencies *manifest* '(build))
		   #:available-crates (find-directly-available-crates native-inputs)
		   ;; Build for the machine the configuration script will be run
		   ;; on.
		   #:target build ; todo: correct terminology?
		   #:configuration (map feature->config *features*))))
    ;; Expected by rust-const-fn's build.rs
    (setenv "OUT_DIR" (getcwd))
    ;; Expected by rust-libm's build.rs
    (setenv "OPT_LEVEL" optimisation-level)
    ;; Expected by some configuration scripts, e.g. rust-libc
    (setenv "RUSTC" (which "rustc"))
    (for-each set-feature-environment-variable! *features*)
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
  (set! *save*
    (lambda (library-destination)
      (apply save-crate-info (or (package-links package)
				 (package-name package))
	     saved-settings library-destination
	     arguments)))
  (format #t "Building with configuration options: ~a~%" *configuration*))

(define *library-destination* #f)

(define* (determine-crate-type manifest #:key rust-crate-type #:allow-other-keys #:rest arguments)
  "Return the crate type to build this rust crate as."
  (define lib (manifest-lib manifest))
  (cond (rust-crate-type rust-crate-type) ; override
	((not lib) %default-crate-type)
	;; TODO: which one is it?  (For rust-derive-arbitrary,
	;; it is proc_macro)
	((target-proc-macro lib) ; proc-macro
	 "proc-macro")
	(#true
	 (match (target-crate-type lib)
	   (() (error "There must be at least one crate type."))
	   ((x) x)
	   ((? list? rest)
	    (pk 'types rest 'in manifest)
	    (error "antioxidant only supports a single crate type, override Cargo.toml with #:rust-crate-type"))))))

(define* (build #:key rust-crate-type inputs tests? #:allow-other-keys #:rest arguments)
  "Build the Rust crates (library) described in Cargo.toml.  If tests are enabled,
also compile the tests using the mechanism described in
<https://doc.rust-lang.org/rustc/tests/index.html> and put the test binary in the
\"tests\" output (or \"bin\" or \"out\")."
  ;; TODO: maybe allow _not_ putting them in an output?
  ;; Also, putting them in "bin" or "out" is potentially confusing.

  ;; Tested for: rust-cfg-il, rust-libc (TODO: more)
  (let* ((package (manifest-package *manifest*))
	 (crate-mappings (manifest-all-dependencies *manifest* '(dependency)))
	 (lib (manifest-lib *manifest*))
	 (crate-name (crate-name-of-manifest *manifest*))
	 (edition (package-edition package))
	 ;; Location of the crate source code to compile.
	 ;; The default location is src/lib.rs, some packages put
	 ;; the code elsewhere.
	 (lib-path (or (and=> lib target-path)
		       (and (file-exists? "src/lib.rs") "src/lib.rs")))
	 (crate-type (apply determine-crate-type *manifest* arguments)))
    (unless (member crate-type '("bin" "lib" "rlib" "dylib" "cdylib" "staticlib" "proc-macro"))
      ;; Note: not all of these crate types have been tested.
      (pk 'c crate-type)
      (error  "unrecognised crate type"))
    (when (and (string=? crate-type "staticlib")
	       (not rust-crate-type))
      (error "The Cargo.toml has asked for a staticlib, but Rust staticlibs include all their dependencies (in contrast to C static libraries) and hence don't play well with grafts, so this needs to be confirmed by setting #:rust-crate-type explicitly"))
    ;; TODO: implement proper library/binary autodiscovery as described in
    ;; <https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery>.
    (when lib-path
      (set! *library-destination*
	(apply (if (member crate-type '("cdylib")) ; TODO: maybe also for 'dylib'?
		   c-library-destination
		   crate-library-destination)
	       crate-name
	       (cond ((member crate-type '("cdylib" "dylib" "proc-macro"))
		      "so")
		     ((member crate-type '("staticlib")) ; used by newsboat-ffi
		      "a")
		     ((member crate-type '("rlib" "lib"))
		      "rlib")
		     (#true
		      (pk 'c crate-type)
		      (error "bogus crate type -- should be unreachable")))
	       arguments)) ;; TODO: less impure
      (*save* *library-destination*)
      (apply compile-rust-library lib-path *library-destination*
	     crate-name
	     ;; Version of the Rust language (cf. -std=c11)
	     ;; -- required by rust-proc-macro2
	     (list (string-append "--edition=" (package-edition package))
		   ;; Some build.rs put libraries in the current directory
		   ;; (or, at least, in OUT_DIR or something like that).
		   ;; TODO: can be done tidier.
		   ;; TODO: is this still necessary, now we interpret
		   ;; rustc-link-search and such?
		   (string-append "-Lnative=" (getcwd)))
	     #:crate-type crate-type
	     #:available-crates (find-directly-available-crates inputs)
	     #:crate-mappings crate-mappings
	     ;; TODO: does the order matter?
	     (append arguments (list #:configuration *configuration*)))
      (when tests?
	;; Compile the tests
	(apply compile-binary-target
	       (elaborate-target
		*manifest*
		(scm->target
		 `(("name" . ,(string-append crate-name "-embedded-tests"))
		   ("path" . ,lib-path))))
	       crate-name
	       #:family 'test
	       arguments)))))

;; See <https://doc.rust-lang.org/cargo/guide/project-layout.html>
;; for how source locations are inferred.
(define (infer-binary-source target)
  "Guess the Rust source code location of TARGET, a <target> record.  If not found,
return false instead."
  (define inferred-source0
    (and (target-name target)
	 (format #f "src/bin/~a.rs" (target-name target))
	 ;; TODO: for 100% paranoia, check that inferred-source0
	 ;; doesn't contain #\nul, slashes or .. components.
	 ))
  ;; default executable (TODO: is this code path actually ever used?) (probably not)
  (define inferred-source1 "src/main.rs")
  (or (target-path target) ; explicit
      (and inferred-source0 (file-exists? inferred-source0) inferred-source0)
      (and (file-exists? inferred-source1) inferred-source1)))

(define* (compile-binary-target target/elaborated crate-name
				#:key (destination 'auto)
				(family 'bin)
				inputs
				outputs
				#:allow-other-keys
				#:rest arguments)
  "Compile an elaborated target @var{target/elaborated}.

If 'destination' is a file name, the binary will be saved there.
If it is the symbol 'auto', an appropriate file name will be chosen
according to the 'target-name' or @var{target/elaborated} and @var{family}.
In that case, the binary will have the target-name as 'base name' and will
be put in the 'bin' subdirectory of one of the outputs.

If the file already exists, bail out.

The directory where the binary is saved in will automatically be created if
required.

The output is based on the symbol 'family' -- if this output does not exist in the list
of outputs, this procedure fallbacks to \"bin\" and then \"bin\":

@begin itemize
@item bin: a regular binary, for the \"bin\" output
@item example: an example (corresponding to an [[example]] section in the
Cargo.toml terminology or a file in the 'examples' subdirectory), for
the \"examples\" output.
@item benchmark: a benchmark (corresponding to a [[bench]] section or a file in the
'benches' directory)
@item test: a test (corresponding to a [[test]] section or a file in the 'tests' directory or the tests
embedded in the main source code)
@end itemize"
  (unless (elaborated-target? target/elaborated)
    (error "The first argument to 'compile-binary-target' must be an elaborated target"))
  (define %family->output
    '((bin . "bin")
      (example . "examples")
      (benchmark . "benchmarks")
      (test . "tests")))
  (define binary-location
    (match destination
      ((? string? where)
       (if (absolute-file-name? where)
	   where
	   (error "The file name passed to 'compile-binary-target' must be absolute.")))
      ('auto
       (match (assoc family %family->output)
	 ((_ . output)
	  (string-append (or (assoc-ref outputs output)
			     (assoc-ref outputs "bin")
			     (assoc-ref outputs "out")
			     (error "'compile-binary-target' expects the \"out\" output to exist."))
			 "/bin/"
			 (target-name target/elaborated)))
	 (#false
	  (if (symbol? family)
	      (error "the family passed to 'compile-bin-target' is unrecognised")
	      (error "the family passed to 'compile-bin-target' is expected to be a symbol")))))))
  (when (file-exists? binary-location)
    ;; This identified a miscompilation of rust-os-pipe.
    (error (format #f "~a already exists when building ~a, refusing to build to avoid overwrite~%"
		   binary-location target/elaborated)))
  (format #t "Compiling ~a to ~a~%" (target-path target/elaborated) binary-location)
  (apply compile-rust-binary
	 (target-path target/elaborated)
	 binary-location
	 (append
	  (if (eq? family 'test)
	      ;; TODO: does this work for [[tests]] and integration tests?
	      (list "--test" ; let the tests be run instead of the main function
		    ;; Some packages, e.g. rust-unsafe-unwrap, expect this to be
		    ;; enabled for tests.
		    "-C" "debug-assertions=on")
	      '())
	  (if crate-name
	      (list (string-append "--crate-name=" crate-name))
	      '())
	  (list (string-append "--edition=" (target-edition target/elaborated))
		(string-append "-Lnative=" (getcwd)))) ; TODO: is this still required, now there's better support for configure scripts?
	 ;; A program can use its own crate without declaring it.
	 ;; At least, hexyl tries to do so.  For a more complicated
	 ;; example, see 'rust-xml-rs@0.8.3', which has "xml_rs" as
	 ;; package name and "xml" as --extern name.
	 ;;
	 ;; TODO: there were ‘could not find crate FOO’ warnings, does this
	 ;; still have any effect?
	 #:crate-mappings
	 (append (if (eq? family 'test)
		     ;; When compiling tests, we are at the same time compiling
		     ;; the library.  Linking to a library when a variant of it
		     ;; is being compiled can cause import ambiguities (e.g. in
		     ;; case of rust-glib@0.14.8), so don't do that.
		     '()
		     (list (make-crate-mapping (package-name (manifest-package *manifest*))
					       (crate-name-of-manifest *manifest*))))
		 (manifest-all-dependencies (pk 'm *manifest*)
					    (if (eq? family 'test)
						'(dependency dev)
						'(dependency))))
	 ;; Binaries can use their own crates!
	 ;; TODO: for tests, also native-inputs?
	 #:available-crates
	 (find-directly-available-crates (append outputs inputs))
	 ;; TODO: figure out how to override things
	 (append
	  arguments
	  (list #:configuration *configuration*))))

(define-condition-type &missing-target-source-code &error
  missing-target-source-code?
  (target missing-target-source-code-target))

(define (elaborate-target manifest target)
  (define package (manifest-package manifest))
  (set-fields target
	      ((target-name)
	       (or (target-name target) (package-name package)))
	      ((target-path)
	       (or (target-path target)
		   (infer-binary-source target)
		   (raise
		    (condition (&missing-target-source-code
				(target target))))))
	      ((target-edition)
	       (or (target-edition target)
		   (package-edition package)))))

(define (elaborate-target/skip manifest target)
  ;; Return the <target> on success, #false otherwise.
  ;; #false: source code is missing.
  ;;
  ;; Maybe the file has been removed due to being non-free,
  ;; requiring dependencies not packaged in Guix, or requiring
  ;; a non-stable rust.  This skipping used to be required for
  ;; rust-phf-generator back when required-features wasn't expected
  ;; and hence gen_hash_test.rs had to be removed in a phase.
  (guard (c
	  ((missing-target-source-code? c)
	   (format #t "warning: source code of ~a could not be found, skipping.~%"
		   (missing-target-source-code-target c))
	   #false))
    (elaborate-target manifest target)))

(define* (find-rust-binaries . arguments) ; TODO: extend to [[benches]], [[tests]], [[examples]]
  ;; This implements autobins, as desribed in
  ;; <https://doc.rust-lang.org/cargo/guide/project-layout.html>.
  ;; As a side-effect, targets are automatically elaborated.
  ;; If the source code of a [[bin]] section is missing, it is ignored
  ;; (with a warning).
  ;;
  ;; First look in [[bin]] sections
  ;;;
  ;; Packages to test after modifications:
  ;;  * rust-os-pipe
  ;;  * ???
  (let* ((autobins? (package-autobins (manifest-package *manifest*)))
	 (elaborate-target/skip* (cut elaborate-target/skip *manifest* <>))
	 (explicit-binaries (map elaborate-target/skip* (manifest-bin *manifest*)))
	 (implicit-primary-main-binary
	  (and autobins?
	       (file-exists? "src/main.rs")
	       (elaborate-target/skip* (scm->target `(("path" . "src/main.rs"))))))
	 (implicit-other-main-binaries
	  (and autobins?
	       (directory-exists? "src/bin")
	       (filter-map
		(match-lambda
		  ((file-name . _)
		   (let ((entry-file-name (string-append "src/bin/" file-name)))
		     ;; Is it a file or a directory?
		     (match (stat:type (lstat entry-file-name))
		       ('regular
			;; If it is a rust file, use it!  The binary will have the same name
			;; as the source file name, except for extension.
			(and (string-suffix? ".rs" file-name)
			     (scm->target `(("name" . ,(string-drop-right file-name 3))
					    ("path" . ,entry-file-name)))))
		       ('directory
			;; If it contains a 'main.rs' file, use it!
			(let ((main (string-append entry-file-name "/main.rs")))
			  (and (file-exists? main)
			       (eq? 'regular (stat:type (stat main)) )
			       (scm->target `(("path" . ,main)
					      ("name" . ,file-name)))))) ; Cargo documentation says: ‘The name of the executable will be the directory name’
		       (_ #false))))) ; something else (e.g., pipe), not something we can build.
		(scandir* "src/bin"))))
	 (implicit-targets
	  (map elaborate-target/skip*
	       (append (or (and=> implicit-primary-main-binary list)
			   '())
		       (or implicit-other-main-binaries '()))))
	 ;; If it's already compiled in the explicit-binaries, don't double compile.
	 ;; (We needed to elaborate-target, because we use the file name
	 ;; which is not always listed.).  Likewise for the target name.
	 (already-used?
	  (lambda (target)
	    (or (member (target-path target) (map target-path explicit-binaries))
		(member (target-name target) (map target-name explicit-binaries)))))
	 (filtered-implicit-targets
	  (filter (negate already-used?) implicit-targets)))
    (append explicit-binaries filtered-implicit-targets)))

(define* (build-binaries #:rest arguments)
  "Compile the Rust binaries described in Cargo.toml (but not examples, tests and benchmarks)."
  (define (compile-binary-target* target)
    ;; Check required-features.
    (if (lset<= string=? (target-required-features target) *features*)
	(apply compile-binary-target target
	       #false ; maybe TODO?
	       #:family 'bin arguments)
	(format #t "not compiling ~a, because the following features are missing: ~a~%"
		target
		(lset-difference string=?
				 (target-required-features target)
				 *features*))))
  (for-each compile-binary-target* (apply find-rust-binaries arguments)))

;; TODO: build-examples, build-benches, build-tests.

(define* (load-manifest . rest)
  "Parse Cargo.toml and save it in @code{*manifest*}."
  (set! *manifest* (open-manifest "Cargo.toml" "Cargo.json")))

;; rust-bzip2-sys has a 0.1.9+1.0.8 version string.
;; Presumably CARGO_PKG_VERSION_MAJOR/MINOR/PATCH must be 0, 1, 9.
;; TODO: what does PRE mean?
(define (without-plus version)
  (match (string-split version #\+)
    ((first . rest) first)))

;; Set some variables that Cargo can set and that might
;; be expected by build.rs.  A (full?) list is avialable
;; at <https://doc.rust-lang.org/cargo/reference/environment-variables.html>.
;; When something does not appear in the Cargo.toml or such, according to
;; that documentation, the environment variable needs to be set to the empty
;; string.
(define* (set-platform-independent-manifest-variables
	  #:key (cargo-target-directory #false) #:allow-other-keys)
  (define package (manifest-package *manifest*))
  ;; Used by rust-cmake.  TODO: actually set the various profile flags,
  ;; optimisation levels, ...
  (setenv "PROFILE" "release")
  (setenv "DEBUG" "true")
  (setenv "NUM_JOBS" (number->string (parallel-job-count)))
  (let ((set-version-environment-variables
	 (lambda (major minor patch pre)
	   (setenv "CARGO_PKG_VERSION_MAJOR" major)
	   (setenv "CARGO_PKG_VERSION_MINOR" minor)
	   (setenv "CARGO_PKG_VERSION_PATCH" patch)
	   (setenv "CARGO_PKG_VERSION_PRE" pre))))
    (match (string-split (without-plus (package-version package)) #\.)
      ((major minor patch pre . rest) ; rest: unusual (non-existent?), but antioxidant doesn't care
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
  (setenv "CARGO_PKG_LICENSE_FILE" (package-license-file package))
  ;; According to Cargo, this is the directory for all ‘generated artifacts
  ;; and intermediate files’ and defaults to a directory "target" in the working
  ;; directory.  However, in Guix, we want to install things in /gnu/store.
  ;; It is also unclear what the file hierarchy is and which artifacts
  ;; should be preserved in the store item and which should be removed.
  ;;
  ;; As such, don't set CARGO_TARGET_DIR by default and instead leave it
  ;; to the packager to decide whether a cwd / store CARGO_TARGET_DIR is
  ;; reasonable and what to preserve / remove.
  ;;
  ;; As an example, rust-cxx-build and newsboat make use of CARGO_TARGET_DIR.
  (when cargo-target-directory
    (let ((cargo-target-directory
	   (if (absolute-file-name? cargo-target-directory)
	       cargo-target-directory
	       (in-vicinity (getcwd) cargo-target-directory))))
      (mkdir-p cargo-target-directory)
      (setenv "CARGO_TARGET_DIR" cargo-target-directory))))

(define* (set-rust-environment-variables
	  #:key rust-environment-variables
	  #:allow-other-keys)
  "Set environment variables like CARGO_CFG_TARGET_POINTER_WIDTH,
CARGO_CFG_TARGET_ARCH and RUSTC_BOOTSTRAP for which we do not need
package-specific information."
  (for-each (match-lambda ((name . value) (setenv name value)))
	    rust-environment-variables)) ; TODO: maybe move more things inside

;; Otherwise it looks for TARGET-strip even when compiling natively,
;; due to how cross-compilation has been set up.
(define* (fixed-strip #:key target build #:allow-other-keys #:rest arguments)
  (if (string=? target build)
      (apply (assoc-ref %standard-phases 'strip)
	     (append arguments
		     (list #:target #false)))
      (apply (assoc-ref %standard-phases 'strip) arguments)))

;; Make sure there are not empty outputs (which can happen if, say,
;; the crate doesn't come with benchmarks and for whatever reason
;; no license file was installed.)
(define* (create-all-outputs #:key outputs #:allow-other-keys)
  (define create-output
    (match-lambda
      ((label . file-name)
       (unless (file-exists? file-name)
	 (mkdir file-name)))))
  (for-each create-output outputs))

(define* (rust-tests-check #:key outputs tests?
			   (test-runner invoke)
			   (test-options '())
			   #:allow-other-keys)
  "Look for tests in the 'tests' output and run them."
  (when tests?
    ;; rust-autocfg@1.0.1 wants a TESTS_TARGET_DIR.  Can't directly
    ;; find out what for.
    (mkdir ".test-target-dir")
    (setenv "TESTS_TARGET_DIR" (in-vicinity (getcwd) ".test-target-dir"))
    (and=> (assoc-ref outputs "tests")
	   (lambda (output)
	     (for-each
	      (lambda (test)
		(apply test-runner test test-options)) ; TODO: look for reasonable defaults
	      (find-files (in-vicinity output "bin")))))))

(define (rust-tests-check/xorg . arguments)
  "Run tests inside an environment with an X display server.  This is often
required for graphical software."
  (define (invoke/xorg . arguments)
    (apply invoke "xvfb-run" "--" arguments))
  (apply rust-tests-check (append arguments (list #:test-runner invoke/xorg))))

(define %standard-antioxidant-phases
  (modify-phases %standard-phases
    ;; TODO: before configure?
    (add-after 'unpack 'make-features-closure make-features-closure)
    (add-after 'unpack 'choose-features choose-features)
    (add-after 'unpack 'read-dependency-environment-variables read-dependency-environment-variables)
    (add-after 'unpack 'set-platform-independent-manifest-variables
	       set-platform-independent-manifest-variables)
    (add-after 'unpack 'set-rust-environment-variables set-rust-environment-variables)
    (add-after 'unpack 'load-manifest load-manifest)
    (replace 'configure configure)
    (replace 'build build)
    (add-after 'build 'build-binaries build-binaries)
    (delete 'check)
    (add-after 'install 'create-all-outputs create-all-outputs)
    (replace 'strip fixed-strip)
    ;; Some Rust packages (e.g. rust-os-pipe@0.9.2) want to access its binaries
    ;; in the check phase.
    (add-after 'strip 'check rust-tests-check)
    (delete 'install))) ; TODO?

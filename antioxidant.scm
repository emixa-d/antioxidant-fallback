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
			read-dependency-environment-variables)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (json))

(define (normalise-crate-name name)
  (string-replace-substring name "-" "_"))

(define (convert-toml->json from to)
  (invoke "python3" "-c"
	  "import sys, toml, json
here = sys.argv[1]; there = sys.argv[2];
t = toml.load(here);
with open(there, \"w\") as out_file:
	json.dump(t, out_file);"
	  from to))

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
		       #:key inputs native-inputs (configuration '())
		       (c-libraries '()) (extern-crates '())
		       #:allow-other-keys)
  (define crates (find-crates (append inputs (or native-inputs '()))))
  (mkdir-p (dirname destination))
  (apply invoke
	 "rustc" "--verbose"
	 ;; Cargo adds '--extern=proc_macro' by default,
	 ;; see <https://github.com/rust-lang/cargo/pull/7700>.
	 ;; Make sure that it will be used.
	 "--extern=proc_macro"
	 "--cap-lints" "warn" ;; ignore #[deny(warnings)], it's too noisy
	 source "-o" destination
	 (append (extern-arguments crates extern-crates)
		 (L-arguments crates)
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
  (define self-crates (find-crates outputs)) ; required by 'hexyl'
  (apply compile-rust source destination
	 (append (list "--crate-type=bin")
		 extra-arguments)
	 arguments))

(define (features-closure features features-section)
  "Include features and the features implied by those features and so on."
  (define new-features
    (delete-duplicates
     ;; lists are not sets, and the order is irrelevant here, so
     ;; pick some fixed arbitrary order.
     (sort-list!
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

;; If too many crates are included in --extern, errors like
;; error[E0659]: `time` is ambiguous (name vs any other name during import resolution)
;; are possible.  Avoid them!
(define (toml-dependencies toml)
  "Return a list of Crate names that are dependencies"
  (define (dependencies-sections1 toml)
    (append (or (assoc-ref toml "dependencies") '())
	    (or (assoc-ref toml "dev-dependencies") '())
	    (or (assoc-ref toml "build-dependencies") '())))
  ;; For now ignore which target a dependency is for.
  (define toml-list
    (cons toml (map cdr (or (assoc-ref toml "target") '()))))
  (define sections (append-map dependencies-sections1 toml-list))
  (map normalise-crate-name (map car sections)))

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
  (define (do* stuff)
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
	       y)))
     (call-with-input-file stuff read #:encoding "UTF-8")))
  (define do
    (match-lambda
      ((_ . input)
       (define where (string-append input "/lib/guixlinks"))
       (when (file-exists? where)
	 (for-each do* (find-files where "\\.propagated-environment$"))))))
  (for-each do native-inputs)
  (for-each do inputs))

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

(define* (compile-cargo #:key name features outputs
			target build
			(optimisation-level 0)
			(cargo-env-variables '())
			#:allow-other-keys #:rest arguments)
  "Compile and install things described in Cargo.toml."
  (for-each (match-lambda ((name . value) (setenv name value)))
	    cargo-env-variables) ; TODO: maybe move more things inside
  (convert-toml->json "Cargo.toml" "Cargo.json")
  (define parsed
    (call-with-input-file "Cargo.json"
      (lambda (port)
	(json->scm port))
      #:encoding "UTF-8"))
  (pk 'pp parsed)
  ;; Tested for: rust-cfg-il, rust-libc (TODO: more)
  (let* ((package (pk 'pack (assoc-ref parsed "package")))
	 (toml-features (assoc-ref parsed "features"))
	 (extern-crates (toml-dependencies parsed))
	 (default-features
	   (vector->list
	    (or (and toml-features
		     (assoc-ref toml-features "default"))
		#())))
	 (extra-configuration '()) ; --cfg options, computed by build.rs
	 (crate-version (or (assoc-ref package "version") ""))
	 (crate-authors (or (assoc-ref package "authors") #()))
	 (crate-name (normalise-crate-name (assoc-ref package "name")))
	 (crate-description (or (assoc-ref package "description") ""))
	 (crate-homepage (or (assoc-ref package "homepage") ""))
	 (crate-repository (or (assoc-ref package "repository") ""))
	 (crate-license (or (assoc-ref package "license") ""))
	 (crate-license-file (or (assoc-ref package "license-file") ""))
	 ;; rust-libc does not compile with edition=2018
	 (edition (or (assoc-ref package "edition") "2015"))
	 (build.rs (or (assoc-ref package "build")
		       ;; E.g, rust-proc-macros2 doesn't set 'build'
		       ;; even though it has a configure script.
		       (and (file-exists? "build.rs") "build.rs")))
	 (lib (or (assoc-ref parsed "lib")))
	 ;; Location of the crate source code to compile.
	 ;; The default location is src/lib.rs, some packages put
	 ;; the code elsewhere.
	 (lib-path (or (and lib (assoc-ref lib "path"))
		       "src/lib.rs"))
	 ;; TODO: which one is it?  (For rust-derive-arbitrary,
	 ;; it is proc_macro)
	 (lib-procedural-macro? (and lib (or (assoc-ref lib "proc-macro")
					     (assoc-ref lib "proc_macro"))))
	 (c-libraries '())
	 (saved-settings '())
	 (link (assoc-ref package "links")) ; optional
	 (extra-arguments '())) ; TODO: ad-hoc
    (when (eq? features 'default)
      (set! features default-features)
      (format #t "Using features listed in Cargo.toml: ~a~%" features)
      (set! features (features-closure features toml-features))
      (format #t "With closure: ~a~%" features))
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
	       (set! c-libraries (cons c-library c-libraries))))
	    ((string-prefix? "cargo:rustc-link-search=" line)
	     (set! extra-arguments
		   `("-L" ,(string-drop line (string-length "cargo:rustc-link-search="))
		     ,@extra-arguments)))
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

    ;; Set some variables that Cargo can set and that might
    ;; be expected by build.rs.  A (full?) list is avialable
    ;; at <https://doc.rust-lang.org/cargo/reference/environment-variables.html>.
    ;; When something does not appear in the Cargo.toml or such, according to
    ;; that documentation, the environment variable needs to be set to the empty
    ;; string.
    (setenv "CARGO_MANIFEST_DIR" (getcwd)) ; directory containing the Cargo.toml
    (setenv "CARGO_PKG_VERSION" crate-version)
    (let ((set-version-environment-variables
	   (lambda (major minor patch pre)
	     (setenv "CARGO_PKG_VERSION_MAJOR" major)
	     (setenv "CARGO_PKG_VERSION_MINOR" minor)
	     (setenv "CARGO_PKG_VERSION_PATCH" patch)
	     (setenv "CARGO_PKG_VERSION_PRE" pre))))
      (match (string-split crate-version #\.)
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
    (setenv "CARGO_PKG_AUTHORS"
	    (string-join (vector->list crate-authors) ":"))
    (setenv "CARGO_PKG_NAME" crate-name)
    (setenv "CARGO_PKG_DESCRIPTION" crate-description)
    (setenv "CARGO_PKG_HOMEPAGE" crate-homepage)
    (setenv "CARGO_PKG_REPOSITORY" crate-repository)
    (setenv "CARGO_PKG_LICENSE" crate-license)
    (setenv "CARGO_PKG_LICENSE_FILE" crate-license-file)
    (define configuration (append extra-configuration (map feature->config features)))
    (when build.rs
      (format #t "building configuration script~%")
      (apply
       compile-rust-binary build.rs "configuration-script"
       (list (string-append "--edition=" edition))
       (append arguments
	       (list #:extern-crates extern-crates
		     #:configuration configuration))) ; TODO: do something less impure
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
    (set! configuration (append extra-configuration (map feature->config features)))
    (when link
      (apply save-environment-variables link saved-settings arguments))
    (format #t "Building with configuration options: ~a~%" configuration)
    ;; TODO: implement proper library/binary autodiscovery as described in
    ;; <https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery>.
    (apply compile-rust-library lib-path
	   (apply library-destination crate-name
		  (if lib-procedural-macro?
		      "so"
		      "rlib")
		  arguments)
	   crate-name
	   ;; Version of the Rust language (cf. -std=c11)
	   ;; -- required by rust-proc-macro2
	   (list (string-append "--edition=" edition)
		 ;; Some build.rs put libraries in the current directory
		 ;; (or, at least, in OUT_DIR or something like that).
		 ;; TODO: can be done tidier.
		 (string-append "-Lnative=" (getcwd)))
	   ;; TODO: figure out how to override things
	   #:crate-type (if lib-procedural-macro?
			    "proc-macro"
			    "rlib")
	   #:extern-crates extern-crates
	   (append
	    arguments
	    (list #:configuration configuration)))
    ;; Compile binaries
    (define (cb source binary)
      (apply compile-rust-binary source
	     (string-append (or (assoc-ref outputs "bin")
				(assoc-ref outputs "out"))
			    "/bin/"
			    binary)
	     (list (string-append "--edition=" edition)
		   (string-append "-Lnative=" (getcwd)))
	     #:extern-crates extern-crates
	     ;; TODO: figure out how to override things
	     (append
	      arguments
	      (list #:configuration configuration))))
    (for-each
     (lambda (file)
       (when (string-suffix? ".rs" file)
	 (cb file (string-drop-right (basename file)
				     (string-length ".rs")))))
     (find-files "src/bin"))))

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
			compile-rust-binary compile-cargo)
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

(define (extern-arguments crates)
  (map (lambda (crate)
	 (string-append "--extern=" (extract-crate-name crate)
			"=" crate))
       crates))

(define (L-arguments crates)
  (delete-duplicates
   (map (lambda (crate)
	  (string-append "-L" (dirname crate)))
	crates)
   string=?))

(define (features-arguments features)
  (append-map (lambda (feature)
		(list "--cfg" feature)) ; or feature="foo"?
	      features))

;; TODO: support static libraries instead of only .so/dylib
(define (l-arguments c-libraries)
  (append-map (lambda (l) (list "-l" l)) c-libraries))

(define* (compile-rust source destination extra-arguments
		       #:key inputs native-inputs (features '())
		       (c-libraries '())
		       #:allow-other-keys)
  (define crates (find-crates (append inputs (or native-inputs '()))))
  (mkdir-p (dirname destination))
  (apply invoke
	 "rustc" "--verbose"
	 "--cap-lints" "warn" ;; ignore #[deny(warnings)], it's too noisy
	 source "-o" destination
	 (append (extern-arguments crates)
		 (L-arguments crates)
		 (features-arguments features)
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
		 (extern-arguments self-crates)
		 (L-arguments self-crates)
		 extra-arguments)
	 arguments))

(define* (compile-cargo #:key name features outputs target
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
	 (default-features
	   (vector->list
	    (or (and toml-features
		     (assoc-ref toml-features "default"))
		#())))
	 (crate-name (normalise-crate-name (assoc-ref package "name")))
	 (crate-version (assoc-ref package "version"))
	 (crate-description (assoc-ref package "description"))
	 ;; rust-libc does not compile with edition=2018
	 (edition (or (assoc-ref package "edition") "2015"))
	 (build (or (assoc-ref package "build")
		    ;; E.g, rust-proc-macros2 doesn't set 'build'
		    ;; even though it has a configure script.
		    (and (file-exists? "build.rs") "build.rs")))
	 (lib (or (assoc-ref parsed "lib")))
	 ;; Location of the crate source code to compile.
	 ;; The default location is src/lib.rs, some packages put
	 ;; the code elsewhere.
	 (lib-path (or (and lib (assoc-ref lib "path"))
		       "src/lib.rs"))
	 (lib-procedural-macro? (and lib (assoc-ref lib "proc-macro")))
	 (c-libraries '())
	 (extra-arguments '())) ; TODO: ad-hoc
    (when (eq? features 'default)
      ;; TODO: this confuses features and configuration options
      (set! features (map (lambda (f)
			    (string-append "feature=\"" f "\"")) default-features))
      (format #t "Using features listed in Cargo.toml: ~a~%" features))
    (define (handle-line line)
      (cond ((string-prefix? "cargo:rustc-cfg=" line)
	     (format #t "Building with --cfg ~a~%" line) ;; todo invalid
	     (set! features
	       (cons (string-drop line (string-length "cargo:rustc-cfg="))
		     features)))
	    ((string-prefix? "cargo:rustc-link-lib=" line)
	     (let ((c-library (string-drop line (string-length "cargo:rustc-link-lib="))))
	       (format #t "Building with C library ~a~%" c-library)
	       (set! c-libraries (cons c-library c-libraries))))
	    ((string-prefix? "cargo:rustc-link-search=" line)
	     (set! extra-arguments
		   `("-L" ,(string-drop line (string-length "cargo:rustc-link-search="))
		     ,@extra-arguments)))
	    ((string-prefix? "cargo:rerun-if-" line)
	     (values)) ; not important for us
	    (#true (pk 'l line)
		   (error "unrecognised output line"))))
    ;; Used by hexyl
    (setenv "CARGO_PKG_NAME" crate-name)
    (when crate-version
      (setenv "CARGO_PKG_VERSION" crate-version))
    (when crate-description
      (setenv "CARGO_PKG_DESCRIPTION" crate-description))
    (when build
      (format #t "building configuration script~%")
      (apply
       compile-rust-binary build "configuration-script"
       (list (string-append "--edition=" edition))
       (append arguments
	       (list #:features features))) ; TODO: do something less impure
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
    (format #t "Building with features: ~a~%" features)
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
	   (list (string-append "--edition=" edition))
	   ;; TODO: figure out how to override things
	   #:crate-type (if lib-procedural-macro?
			    "proc-macro"
			    "rlib")
	   (append
	    (list #:features features)
	    arguments
	    (list #:features features)))
    ;; Compile binaries
    (define (cb source binary)
      (apply compile-rust-binary source
	     (string-append (or (assoc-ref outputs "bin")
				(assoc-ref outputs "out"))
			    "/bin/"
			    binary)
	     (list (string-append "--edition=" edition))
	     ;; TODO: figure out how to override things
	     (append
	      (list #:features features)
	      arguments
	      (list #:features features))))
    (for-each
     (lambda (file)
       (when (string-suffix? ".rs" file)
	 (cb file (string-drop-right (basename file)
				     (string-length ".rs")))))
     (find-files "src/bin"))))

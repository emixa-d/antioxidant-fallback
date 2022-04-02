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
	     (gnu packages compression)
	     (guix search-paths) (gnu packages rust) (gnu packages base))

(define* (antioxidant-build name inputs #:key system target source search-paths outputs
			    crate-name source-file binary-name (type 'library))
  (pk 'hi)
  (define builder
    (with-imported-modules
	(source-module-closure '((guix build utils) (guix build gnu-build-system)))
      #~(begin
	  (use-modules (guix build utils) (guix build gnu-build-system)
		       (srfi srfi-1) (ice-9 match))
	  (define (crate-directory store-item)
	    (string-append store-item "/lib/guixcrate"))
	  (define (extract-crate-name rlib)
	    (string-drop (string-drop-right (basename rlib) (string-length ".rlib"))
			 (string-length "lib")))
	  (define (find-crates inputs)
	    (append-map (match-lambda
			  ((name . store-item)
			   (if (file-exists? store-item)
			       (find-files (crate-directory store-item) "\\.rlib$")
			       '())))
			inputs))
	  (define (extern-arguments crates)
	    (map (lambda (crate)
		   (string-append "--extern=" (extract-crate-name crate)
				  "=" crate))
		 crates))
	  (define (L-arguments crates)
	    (map (lambda (crate)
		   (string-append "-L" (dirname crate)))
		 crates))
	  (define* (build-the-crate #:key inputs native-inputs
				    #:allow-other-keys)
	    (define destination
	      (case '#$type
		((library)
		 (string-append (crate-directory #$output) "/lib" #$crate-name ".rlib"))
		((binary)
		 (string-append #$output "/bin/" #$binary-name))))
	    (define all-inputs (append inputs (or native-inputs '())))
	    (define crates (find-crates all-inputs))
	    (mkdir-p (dirname destination))
	    ;; TODO: why rlib?  Because that works.  Maybe dylib works too?
	    (apply invoke
		   "rustc" "--verbose"
		   #$(case type
		       ((library) "--crate-type=rlib")
		       ((binary) "--crate-type=bin")
		       (else (error "bogus")))
		   #$@(if (eq? type 'library) 
			  (list (string-append "--crate-name=" crate-name))
			  '())
		   #$source-file
		   "-o"
		   destination
		   (append (extern-arguments crates)
			   (L-arguments crates))))
	  (gnu-build #:name #$name
		     #:source #+source
		     #:system #$system ;;#:target #$target
		     #:outputs #$(outputs->gexp outputs)
		     #:inputs #$(input-tuples->gexp inputs)
		     #:native-inputs #$(input-tuples->gexp inputs)
		     #:search-paths '#$(map search-path-specification->sexp
					    search-paths)
		     #:phases (modify-phases %standard-phases
				(delete 'configure)
				(replace 'build build-the-crate)
				(delete 'check)
				(delete 'install))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target crate-name source-file
		type binary-name
		#:rest arguments)
  (define private-keywords
    '(#:inputs #:native-inputs #:outputs))
  (pk 'hi)
  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(("source" ,source)
		    ("rust" ,rust)
		    ("tar" ,tar)
		    ("gzip" ,gzip)
		    ,@native-inputs))
    (host-inputs inputs)
    (build (if target antioxidant-cross-build antioxidant-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define antioxidant-build-system
  (build-system
   (name 'antioxidant)
   (description "Build software written in Rust, without cargo")
   (lower lower)))

;; A rust (macro) library
(define-public rust-cfg-if
  (package
   (name "rust-cfg-if")
   (version "1.0.0")
   (source (package-source (@ (gnu packages crates-io) rust-cfg-if-1)))
   (build-system antioxidant-build-system)
   (arguments (list #:crate-name "cfg_if"
		    #:source-file "src/lib.rs"))
   (synopsis #f)
   (description #f)
   (home-page #f)
   (license #f)))

(define-public rust-hello
  (package
    (name "rust-hello")
    (version "1.0.0")
    (source (local-file "libhello" #:recursive? #true))
    (build-system antioxidant-build-system)
    (arguments (list #:crate-name "hello"
		     #:source-file "hello.rs"))
    ;; Or would this need to be native-inputs, because it's a macro?
    ;; For now, put everything in 'inputs'.
    (propagated-inputs (list rust-cfg-if))
    (synopsis #f)
    (description #f)
    (home-page #f)
    (license #f)))

(define-public hello-oxygen
  (package
    (name "hello-oxygen")
    (version "1.0.0")
    (source (local-file "hello-app" #:recursive? #true))
    (build-system antioxidant-build-system)
    (arguments (list #:type 'binary
		     #:binary-name "hello"
		     #:source-file "main.rs"))
    (inputs (list rust-hello))
    (synopsis #f)
    (description #f)
    (home-page #f)
    (license #f)))

(pk 'h hello-oxygen)

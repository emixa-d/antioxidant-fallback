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
	     (gnu packages guile)
	     (guix search-paths) (gnu packages rust) (gnu packages base))

(define* (antioxidant-build name inputs #:key system target source search-paths outputs
			    crate-name source-file binary-name (type 'library)
			    (features #~'()))
  (define builder
    (with-extensions (list guile-json-4)
    (with-imported-modules
	(cons '(antioxidant)
	      (source-module-closure '((guix build utils) (guix build gnu-build-system)
				       (antioxidant))))
      #~(begin
	  (use-modules (guix build utils) (guix build gnu-build-system)
		       (srfi srfi-1) (ice-9 match) (antioxidant))
	  (define (build-the-crate . arguments)
	    (cond ((eq? '#$type 'auto)
		   (apply compile-cargo arguments))
		  ;; TODO: remove these cases
		  ((eq? '#$type 'binary)
		   (apply compile-rust-binary
			  #$source-file
			  (string-append #$output "/bin/" #$binary-name)
			  '()
			  arguments))
		  (#true
		   (apply compile-rust-library #$source-file
			  (string-append (crate-directory #$output)
					 "/lib" #$crate-name ".rlib")
			  #$crate-name
			  '()
			  arguments))))
	  (gnu-build #:name #$name
		     #:source #+source
		     #:system #$system ;;#:target #$target
		     #:outputs #$(outputs->gexp outputs)
		     #:inputs #$(input-tuples->gexp inputs)
		     #:native-inputs #$(input-tuples->gexp inputs)
		     #:search-paths '#$(map search-path-specification->sexp
					    search-paths)
		     #:features #$features
		     #:phases (modify-phases %standard-phases
				(delete 'configure)
				(replace 'build build-the-crate)
				(delete 'check)
				(delete 'install)))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target crate-name source-file
		type binary-name (features #~'())
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
				       #:key (features #~'()))
  (package
    (inherit crate-package)
    (build-system antioxidant-build-system)
    (arguments (list #:type 'auto
		     #:features features))))

;; A rust (macro) library
(define rust-cfg-if
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-cfg-if-1)))

(define rust-unicode-xid
  (package
    (inherit (@ (gnu packages crates-io) rust-unicode-xid-0.2))
    (build-system antioxidant-build-system)
    ;; TODO tests
    (arguments (list #:type 'auto))))

(define rust-proc-macro2
  (package
    (inherit (@ (gnu packages crates-io) rust-proc-macro2-1))
    (build-system antioxidant-build-system)
    ;; TODO tests
    (arguments (list #:type 'auto))
    (propagated-inputs (list rust-unicode-xid))))

(define rust-quote
  (package
    (inherit (@ (gnu packages crates-io) rust-quote-1))
    (build-system antioxidant-build-system)
    ;; TODO tests
    (arguments (list #:type 'auto))
    (propagated-inputs (list rust-proc-macro2))))

(define rust-syn
  (package
    (inherit (@ (gnu packages crates-io) rust-syn-1))
    (build-system antioxidant-build-system)
    ;; TODO tests
    (arguments (list #:type 'auto))
    (propagated-inputs (list rust-proc-macro2
			     rust-quote
			     rust-unicode-xid))))

;; TODO: rust-libc building?
#;
(define-public rust-rustc-std-workspace-core
  (package
    (inherit (@ (gnu packages crates-io) rust-rustc-std-workspace-core-1))
    (build-system antioxidant-build-system)
    (arguments (list #:type 'auto))))

(define rust-libc
  ;; TODO tests
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-libc-0.2)))

(define rust-atty
  (package
    (inherit
     (vitaminate-library/no-inputs (@ (gnu packages crates-io) rust-atty-0.2)))
    ;; TODO tests
    (propagated-inputs (list rust-libc))))

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

(define rust-autocfg
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-autocfg-1)))

(define rust-bitflags
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-bitflags-1)))

(define rust-hashbrown
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-hashbrown-0.11)
   ;; rust-indexmap requires this feature
   #:features #~'("feature=\"raw\"")))

(define rust-indexmap
  (package
    (inherit (vitaminate-library/no-inputs
	      (@ (gnu packages crates-io) rust-indexmap-1)))
    (propagated-inputs (list rust-hashbrown))
    (native-inputs
     (list rust-autocfg)))) ; required by build.rs

(define rust-os-str-bytes
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-os-str-bytes-2)
   ;; rust-clap requires this feature
   #:features #~'("feature=\"raw\"")))

(define rust-unicode-width
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-unicode-width-0.1)))

(define rust-textwrap
  (package
    (inherit
     (vitaminate-library/no-inputs
      (@ (gnu packages crates-io) rust-textwrap-0.12)))
    (propagated-inputs
     (list rust-unicode-width))))

(define rust-vec-map
  (vitaminate-library/no-inputs
   (@ (gnu packages crates-io) rust-vec-map-0.8)))

;; TODO: fails to build (missing deps)
(define rust-clap
  (package
    (inherit
     (vitaminate-library/no-inputs
      (@ (gnu packages crates-io) rust-clap-3)
      ;; TODO: maybe add this by default?
      #:features #~'("feature=\"std\"")))
    (propagated-inputs
     (list rust-atty rust-bitflags rust-indexmap rust-os-str-bytes
	   rust-unicode-width rust-textwrap rust-vec-map))))

hello-oxygen
rust-unicode-xid
rust-proc-macro2
rust-quote
rust-syn
rust-atty
rust-clap

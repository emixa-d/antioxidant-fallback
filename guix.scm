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
			    crate-name source-file)
  (define builder
    (with-imported-modules
	(source-module-closure '((guix build utils) (guix build gnu-build-system)))
      #~(begin
	  (use-modules (guix build utils) (guix build gnu-build-system))
	  (define (build-the-crate . _)
	    (define destination
	      (string-append #$output "/lib/crate/lib" #$crate-name ".rlib"))
	    (mkdir-p (dirname destination))
	    ;; TODO: why rlib?  Because that works.  Maybe dylib works too?
	    (invoke "rustc" "--verbose" "--crate-type=rlib"
		    #$(string-append "--crate-name=" crate-name)
		    #$source-file
		    "-o"
		    destination))
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
		    ,@native-inputs))
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

;; next step: libhello
rust-cfg-if

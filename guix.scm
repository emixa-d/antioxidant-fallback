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

(define* (antioxidant-build name inputs #:key system target source search-paths outputs
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
	  (gnu-build #:name #$name
		     #:source #+source
		     #:system #$system
		     ;; used even when building natively
		     #:target #$(or target
				    (nix-system->gnu-triplet system))
		     #:outputs #$(outputs->gexp outputs)
		     #:inputs #$(input-tuples->gexp inputs)
		     #:native-inputs #$(input-tuples->gexp inputs)
		     #:search-paths '#$(map search-path-specification->sexp
					    search-paths)
		     #:features #$features
		     #:phases (modify-phases %standard-phases
				(delete 'configure)
				(replace 'build compile-cargo)
				(delete 'check)
				(delete 'install)))))))
  ;; TODO graft stuff, package->derivation guile-for-build
  (gexp->derivation name builder #:system system #:target target #:graft? #f))

(define* (lower name #:key system source inputs native-inputs outputs target
		(features #~'())
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
    (arguments (list #:features features))))

(define c 0)

(define (is-cargo-toml-phases? phases)
  ;; This probably just relaxes versions, so no need to keep this phase
  ;; anymore.
  (match (if (gexp? phases) (gexp->approximate-sexp phases) phases)
    (('modify-phases '%standard-phases _
		     (_ _ _ (_ _ ('substitute* "Cargo.toml" . _))))
     #t)
    (_ #false)))

(define (vitaminate/auto* pack)
  (set! c (+ 1 c))
  (when (> c 200)
    (error "ooops, is this a cycle?"))
  (if (eq? (package-build-system pack) (@ (guix build-system cargo) cargo-build-system))
      (apply
       (lambda* (#:key (cargo-development-inputs '()) (cargo-inputs '())
		 (phases '%standard-phases)
		 ;; TODO: cargo test flags
		 skip-build? cargo-test-flags tests?
		 (features #~'()))
	 (unless (or (eq? phases '%standard-phases)
		     (not (is-cargo-toml-phases? phases)))
	   (error "phases?"))
	 (define fix-input
	   (match-lambda
	     ((_ dependency)
	      ;; Some of these are only used for tests, cause cycles, ???
	      (and (not (member (package-name dependency)
				'("rust-rustc-std-workspace-std"
				  "rust-rustc-std-workspace-core"
				  "rust-compiler-builtins" "rust-winapi"
				  "rust-serde-json" "rust-doc-comment"
				  "rust-regex" "rust-hermit-abi"
				  "rust-lazy-static" "rust-version-sync"
				  "rust-rustversion" "rust-trybuild"
				  "rust-clippy"
				  "rust-rand" "rust-rand-xorshift"
				  "rust-walkdir" "rust-yaml-rust"
				  "rust-serde-test")))
		   ;; Avoid cycle!
		   (or (string=? (package-name pack) "rust-serde-bytes")
		       (not (string=? (package-name dependency) "rust-serde")))
		   (or (string=? (package-name pack) "rust-serde")
		       (not (string=? (package-name dependency) "rust-serde-derive")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-bytes" "rust-bincode")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-serde-bytes" "rust-bincode")))
		   (not (equal? (list (package-name pack) (package-name dependency))
				(list "rust-proc-macro2" "rust-quote")))
		   (pk 'p pack dependency #t)
		   (vitaminate/auto dependency)))))
	 (package
	  (inherit (vitaminate-library/no-inputs pack))
	  (arguments (list #:features
			   (match (package-name pack)
			     ;; TODO: use default features from Cargo.toml
			     ;; rust-serde-bytes requires the 'parsing' feature
			     ("rust-syn"
			      #~'("feature=\"derive\"" "feature=\"parsing\"" "feature=\"printing\"" "feature=\"clone-impls\""
				  "feature=\"proc-macro\""
				  "feature=\"full\""))
			     ("rust-proc-macro2"
			      ;; Required by rust-serde-bytes via rust-syn.  If
			      ;; absent, this causes errors like
			      ;; <<https://github.com/google/cargo-raze/issues/159>.
			      #~'("feature=\"proc-macro\""))
			     ;; TODO: move into Guix proper?
			     ((or "rust-hashbrown" "rust-os-str-bytes")
			      #~'("feature=\"raw\""))
			     ;; TODO: is unstable-locales ok, or does it
			     ;; need to be converted to feature="unstable-locales"?
			     (_ features))))
	  (native-inputs (filter-map fix-input cargo-development-inputs))
	  (propagated-inputs (append (filter-map fix-input cargo-inputs)
				     (package-propagated-inputs pack)))))
       (package-arguments pack))
      pack))

(define vitaminate/auto
  ((@ (guix memoization) mlambda) (pack) (vitaminate/auto* pack)))

(vitaminate/auto (@ (gnu packages rust-apps) hexyl))
(vitaminate/auto (@ (gnu packages crates-io) rust-serde-bytes-0.11))
#;(vitaminate/auto (@ (gnu packages rust-apps) sniffglue))

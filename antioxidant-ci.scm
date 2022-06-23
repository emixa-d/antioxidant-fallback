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
;;; along with Antioxidant.  If not, see <http://www.gnu.org/licenses/>.
(define-module (antioxidant-ci)
  #:use-module ((antioxidant-packages) #:select (vitaminate/auto public-test-package
								 antioxidant-build-system))
  #:use-module ((guix build-system cargo) #:select (cargo-build-system))
  #:use-module ((guix profiles) #:select (packages->manifest))
  #:use-module ((guix packages) #:select (package? package-direct-inputs))
  #:use-module ((gnu packages) #:select (fold-packages))
  #:use-module ((rnrs exceptions) #:select (guard))
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module ((guix packages) #:select (package-build-system package-name))
  #:autoload (guix ui) (warning G_)
  #:export (all-packages))


(define %fails-to-build-with-cargo ; these are bugs but don't appear to be bugs introduced by antioxidant (TODO: bug reports, etc)
  '("git-interactive-rebase-tool"))

(define (is-acceptable-leaf-cargo-rust-package? package)
  (and (eq? cargo-build-system (package-build-system package))
       (or (not (string-prefix? "rust-" (package-name package)))
	   (member (package-name package)
		   '("rust-analyzer" "rust-cargo-c"))) ; not libraries
       (not (member (package-name package) %fails-to-build-with-cargo))))

(define (all-packages)
  "Return a list of all antioxidated leaf packages (not guaranteed to build yet)"
  (define (manually-antioxidated-variant package)
    ;; Some leaf package are updated or patched.  In that case, vitaminate/auto
    ;; will _not_ choose the updated or patched version.  However, a ‘manually’
    ;; antioxidated will be defined in (antioxidant-packages).
    (and=> (module-variable
	    (resolve-interface '(antioxidant-packages))
	    (string->symbol (string-append "antioxidated-" (package-name package))))
	   variable-ref))
  (define (add foo list)
    (guard (c ((eq? (exception-kind c) 'antioxidant-cycle)
	       (warning (G_ "skipping ~a for now because of cycle~%") (package-name foo))
	       list)
	      ((eq? (exception-kind c) 'keyword-argument-error)
	       (warning (G_ "skipping ~a for now because of ~a~%") (package-name foo) c)
	       list))
      (cons (or (manually-antioxidated-variant foo)
		(public-test-package (vitaminate/auto foo))) list)))
  (fold-packages add '() #:select? is-acceptable-leaf-cargo-rust-package?))

(define (package-closure/vhash todo seen descend?)
  ;; Compute the closure, depth-first
  ;; todo: list of packages
  ;; seen: vhash -> #true of packages
  (define (only-package input)
    (second input))
  (define (proc new-package seen)
    (cond ((vhash-assq new-package seen) ; already visited
	   seen)
	  ((descend? new-package)
	   (package-closure/vhash
	    (map only-package (package-direct-inputs new-package))
	    (vhash-consq new-package #true seen)
	    descend?))
	  (#true
	   (vhash-consq new-package #true seen))))
  (fold proc seen todo))

(define (rusty-package-closure packages)
  (define (vhash->key-list vhash)
    (define (cons-it key _ rest)
      (cons key rest))
    (vhash-fold cons-it '() vhash))
  (define (descend? package)
    (and (package? package) ; could be an <origin>
	 (or (string-prefix? "antioxidated-" (package-name package))
	     (string-prefix? "rust-" (package-name package))
	     (eq? (package-build-system package) antioxidant-build-system))))
  (vhash->key-list (package-closure/vhash packages vlist-null descend?)))

;; The idea is to build all packages in (all-packages) by the CI infrastructure.
;; Apparently returning a manifest is convenient for the CI infrastructure, see
;; see <https://github.com/emixa-d/antioxidant-fallback/pull/1>.
;;
;; By using rusty-package-closure, the intermediate packages are shown as well.
(packages->manifest (rusty-package-closure (all-packages)))

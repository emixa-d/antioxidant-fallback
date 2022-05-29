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
  #:use-module ((antioxidant-packages) #:select (vitaminate/auto public-test-package))
  #:use-module ((guix build-system cargo) #:select (cargo-build-system))
  #:use-module ((guix profiles) #:select (packages->manifest))
  #:use-module ((gnu packages) #:select (fold-packages))
  #:use-module ((rnrs exceptions) #:select (guard))
  #:use-module ((guix packages) #:select (package-build-system package-name))
  #:autoload (guix ui) (warning G_)
  #:export (all-packages))

(define (is-leaf-cargo-rust-package? package)
  (and (eq? cargo-build-system (package-build-system package))
       (not (string-prefix? "rust-" (package-name package)))))

(define (all-packages)
  "Return a list of all antioxidated leaf packages (not guaranteed to build yet)"
  (define (add foo list)
    (guard (c ((eq? (exception-kind c) 'antioxidant-cycle)
	       (warning (G_ "skipping ~a for now because of cycle~%") (package-name foo))
	       list)
	      ((eq? (exception-kind c) 'keyword-argument-error)
	       (warning (G_ "skipping ~a for now because of ~a~%") (package-name foo) c)
	       list))
      (cons (public-test-package (vitaminate/auto foo)) list)))
  (fold-packages add '() #:select? is-leaf-cargo-rust-package?))

;; The idea is to build all packages in (all-packages) by the CI infrastructure.
(packages->manifest (all-packages))

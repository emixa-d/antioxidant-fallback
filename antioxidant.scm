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
  #:use-module (json))

(define (normalise-crate-name name)
  (string-replace-substring name "-" "_"))

(define (convert-toml->json from to)
  (invoke "python3" "-c"
	  "import sys; import toml; import json;
here = sys.argv[1]; there = sys.argv[2];
t = toml.load(here);
print(t)
out_file = open(there, \"w\");
json.dump(t, out_file);
out_file.close();"
	  from to))

(define (crate-directory store-item)
  (string-append store-item "/lib/guixcrate"))

(define* (library-destination crate-name #:key outputs #:allow-other-keys)
  (string-append
   (crate-directory (pk 'out (or (assoc-ref outputs "lib")
			(assoc-ref outputs "out"))))
   "/lib" (pk 'cname crate-name) ".rlib"))
  
(define (find-crates inputs)
  (append-map (match-lambda
		((name . store-item)
		 (if (file-exists? store-item)
		     (find-files (crate-directory store-item) "\\.rlib$")
		     '())))
	      inputs))

(define (extract-crate-name rlib)
  (string-drop (string-drop-right (basename rlib) (string-length ".rlib"))
	       (string-length "lib")))

(define (extern-arguments crates)
  (map (lambda (crate)
	 (string-append "--extern=" (extract-crate-name crate)
			"=" crate))
       crates))

(define (L-arguments crates)
  (map (lambda (crate)
	 (string-append "-L" (dirname crate)))
       crates))

(define* (compile-rust source destination extra-arguments
		       #:key inputs native-inputs #:allow-other-keys)
  (define crates (find-crates (append inputs (or native-inputs '()))))
  (mkdir-p (dirname destination))
  (apply invoke
	 "rustc" "--verbose" source "-o" destination
	 (append (extern-arguments crates)
		 (L-arguments crates)
		 extra-arguments)))

(define (compile-rust-library source destination crate-name extra-arguments
			       . arguments)
  ;; TODO: why rlib?  Because that works.  Maybe dylib works too?
  (apply compile-rust source destination
	 (append (list (string-append "--crate-name=" crate-name)
		       "--crate-type=rlib")
		 extra-arguments)
	 arguments))

(define* (compile-rust-binary source destination extra-arguments
			      . arguments)
  (apply compile-rust source destination '("--crate-type=bin") arguments))

(define* (compile-cargo . arguments)
  "Compile and install things described in Cargo.toml."
  (convert-toml->json "Cargo.toml" "Cargo.json")
  (define parsed
    (call-with-input-file "Cargo.json"
      (lambda (port)
	(json->scm port))
      #:encoding "UTF-8"))
  (pk 'pp parsed)
  ;; Tested for: rust-cfg-il (TODO: more)
  (let* ((package (pk 'pack (assoc-ref parsed "package")))
	 (crate-name (normalise-crate-name (assoc-ref package "name")))
	 (edition (or (assoc-ref package "edition") "2018")))
    ;; TODO: how does Cargo determine where the source is located?
    (apply compile-rust-library "src/lib.rs"
	   (apply library-destination crate-name arguments)
	   crate-name
	   '() ; TODO edition
	   arguments)))

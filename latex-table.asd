(defpackage #:latex-table-asd
  (:use :cl :asdf))

(in-package :latex-table-asd)

(defsystem latex-table
  :description "Pretty latex tables from Lisp matrices and vectors."
  :author "Tamas K Papp"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "formatting" :depends-on ("package"))
	       (:file "latex-table" :depends-on ("formatting")))
  :depends-on (:iterate :metabang-bind :cl-utilities))

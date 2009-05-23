(in-package #:latex-table-asd)

(defpackage latex-table
  (:use :common-lisp :iterate :metabang-bind :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; formatting.lisp
   
   make-float-formatter make-simple-formatter

   ;; latex-table.lisp
   
   multicolumn aligned raw-tabular lines-to-vector labeled-matrix
   labeled-vector-horizontal labeled-vector-vertical with-table
   math-inline

   ))

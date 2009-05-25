(in-package #:latex-table-asd)

(defpackage latex-table
  (:use :common-lisp :iterate :metabang-bind :cl-utilities)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; latex-table.lisp
   
   raw-tabular *default-special-values* *default-significant-digits*
   labeled-matrix labeled-vector-horizontal labeled-vector-vertical
   with-table math-inline

   ))

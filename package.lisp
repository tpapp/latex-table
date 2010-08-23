(in-package #:latex-table-asd)

(defpackage latex-table
  (:use :common-lisp :iterate :metabang-bind :alexandria :anaphora
        :cl-num-utils)
  (:shadowing-import-from :cl-num-utils :mean :xor) ; also in alexandria
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; formatting.lisp
   
   format-options *default-format-options* format-value

   ;; latex-table.lisp
   
   multicolumn aligned raw-tabular lines-to-vector labeled-matrix
   labeled-vector-horizontal labeled-vector-vertical with-table

   ))

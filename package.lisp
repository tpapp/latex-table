(defpackage #:latex-table
  (:use #:common-lisp #:iterate #:let-plus #:alexandria #:anaphora
        #:cl-num-utils)
  (:shadowing-import-from #:cl-num-utils #:mean #:variance #:xor #:median) ; also in alexandria
  (:export
   ;; formatting.lisp
   #:format-options
   #:*default-format-options*
   #:format-value
   ;; latex-table.lisp
   #:multicolumn
   #:aligned
   #:raw-tabular
   #:lines-to-vector
   #:labeled-matrix
   #:labeled-vector-horizontal
   #:labeled-vector-vertical
   #:with-table
   ))

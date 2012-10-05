(defpackage #:latex-table
  (:use #:cl #:alexandria #:anaphora #:let-plus)
  (:nicknames #:lxt)
  ;; (:shadowing-import-from #:cl-num-utils #:mean #:variance #:median ; also in alexandria
  ;;                         #:sum ; also in iterate
  ;;                         )
  (:export
   ;; ;; formatting.lisp
   ;; #:format-options
   ;; #:*default-format-options*
   ;; #:format-value
   ;; ;; latex-table.lisp
   ;; #:multicolumn
   ;; #:aligned
   ;; #:raw-tabular
   ;; #:lines-to-vector
   ;; #:labeled-matrix
   ;; #:labeled-vector-horizontal
   ;; #:labeled-vector-vertical
   ;; #:with-table
   )
  (:export
   #:alignment
   #:aligned
   #:align
   #:multicolumn
   #:numprint
   #:*digits-before-decimal*
   #:*digits-after-decimal*
   #:column-type
   #:rule
   #:cells
   #:column-types
   #:rules
   #:table
   #:expand-to-vector
   #:format-content
   #:write-latex
   #:write-ascii
   #:labeled-vertical))

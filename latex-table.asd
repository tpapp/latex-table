(defsystem #:latex-table
  :description "Pretty latex tables from Lisp matrices and vectors."
  :author "Tamas K Papp"
  :license "Boost Software License, Version 1.0."
  :serial t
  :components ((:file "package")
	       (:file "latex-table"))
  :depends-on (#:alexandria #:anaphora #:array-operations #:let-plus))

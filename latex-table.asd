(defsystem #:latex-table
  :description "Pretty latex tables from Lisp matrices and vectors."
  :author "Tamas K Papp"
  :license "Boost Software License, Version 1.0."
  :serial t
  :components ((:file "package")
	       (:file "formatting")
	       (:file "latex-table"))
  :depends-on (#:alexandria #:anaphora #:let-plus))

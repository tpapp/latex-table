(asdf:oos 'asdf:load-op :latex-table)

(defpackage #:latex-table-example
  (:use :cl :latex-table))

(in-package #:latex-table-example)

;;;;  Create a labeled horizontal vector.  Note :significant-digits.
;;;;
(with-open-file (stream "/tmp/horizontal.table" :direction :output
			:if-exists :supersede :if-does-not-exist :create)
  (labeled-vector-horizontal stream 
			     (vector "foo" pi -42)
			     (vector "\\alpha" "bar" "baz")
			     :significant-digits 3))

;;;;  Create a labeled matrix.  Note specification for horizontal lines:
;;;;  1 2 means "double line in position 1", -1 1 means "single line
;;;;  in last position", negative numbers count back from last one
(with-open-file (stream "/tmp/matrix.table" :direction :output
			:if-exists :supersede :if-does-not-exist :create)
  (labeled-matrix stream #2A((11.1 112.55 3)
			     (9 1345.7912122 14.72))
		  (vector "foo" "bar" "baz")
		  (vector "first" "second")
		  :hlines '(1 2 -1 1)))

;;;;  raw tabular interface.
(with-open-file (stream "/tmp/raw.table" :direction :output
			:if-exists :supersede :if-does-not-exist :create)
  (raw-tabular stream #2A((3)
			  (13.14)
			  (0.125)
			  ((:left "left"))
			  ((:right "right"))
			  ((:center "center")))
	       #((:d "." "." 3))	; column types
	       #(0 0)			; vertical lines
	       #(1 2 0 0 0 0 3)))	; horizontal lines
      

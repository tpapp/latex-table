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
			     (vector "$\\alpha$" "bar" "baz")))

;;;;  Create a labeled matrix.  Note specification for horizontal lines:
;;;;  1 2 means "double line in position 1", -1 1 means "single line
;;;;  in last position", negative numbers count back from last one
(with-open-file (stream "/tmp/matrix.table" :direction :output
			:if-exists :supersede :if-does-not-exist :create)
  (labeled-matrix stream #2A((11.1 112.55 3)
			     (9 1345.79 14.72))
		  (vector "foo" "bar" "baz")
		  (vector "first" "second")
		  :hlines '(1 2 -1 1)))

;;;;  raw tabular interface.  You can't use position-type pair
;;;;  specifications here, have to give the whole vector for both
;;;;  horizontal and vertical lines, and also the column types.  This
;;;;  interface does not number->string conversion, that should be
;;;;  done by the caller.
(with-open-file (stream "/tmp/raw.table" :direction :output
			:if-exists :supersede :if-does-not-exist :create)
  (raw-tabular stream #2A(((:align "foo" ""))
			  ((:align "bar" "baz"))
			  ((:align "" ".19"))
			  ((:left "left"))
			  ((:right "right"))
			  ((:center "center")))
	       #(:align)		; column types
	       #(0 0)			; vertical lines
	       #(1 2 0 0 0 0 3)))	; horizontal lines
      


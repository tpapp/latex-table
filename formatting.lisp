(in-package :latex-table)

(defun make-float-formatter (fraction-digits)
  "Return a function that converts its argument into two values: an
integer and a fractional part, both returned as strings.  The latter
contains a decimal dot and exactly the specified number of digits,
padded with zeroes if necessary, or it is empty if fraction-digits
is 0.

Examples:

 (funcall (make-float-formatter 3) pi)      ; => \"3\", \".142\"
 (funcall (make-float-formatter 3) (- pi))  ; => \"-3\", \".142\"
 (funcall (make-float-formatter 0) pi)      ; => \"-3\", \"\"
 (funcall (make-float-formatter 2) -0.1)    ; => \"-0\", \".10\"
"
  (assert (integerp fraction-digits))
  (cond
    ((zerop fraction-digits)
     (lambda (value)
       (values (format nil "~d" (round value)) "")))
    ((plusp fraction-digits)
       (lambda (value)
	 (let* ((string (format nil "~,vf" fraction-digits value))
		(pos (position #\. string)))
        (values (subseq string 0 pos)
		;; !! end argument: fixes possible format bug, investigate
		(subseq string pos (+ pos 1 fraction-digits))))))
    (t (error "fraction-digits must be nonnegative"))))

(defun make-simple-formatter (fraction-digits special-values)
  "Return a formatter function with returns a formatted cell
corresponding to its argument, according to the following rules:

- if the argument is #'equal to one of the special values (a list of
  value-string pairs), the corresponding string is returned

- if the argument is a float, it is formatted with the given number of
  significant digits

- if the argument is an integer, it is formatted without a decimal dot

- otherwise, it is formatted with the directive ~a and centered

Example: (make-simple-formatter 2 '((nil \"n/a\")))
"
  (let ((float-formatter (make-float-formatter fraction-digits)))
    (lambda (value)
      	(let* ((special (find value special-values :key #'car :test #'equal)))
	  (cond
	    (special (list :center (second special)))
	    ((integerp value) (list :aligned (format nil "~d" value) ""))
	    ((floatp value)
	     (bind (((:values int frac) (funcall float-formatter value)))
	       (list :aligned int frac)))
	    (t (list :center (format nil "~a" value))))))))


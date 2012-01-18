(in-package #:latex-table)

(defun exp10 (power)
  "10^power."
  (expt 10 power))

(defun exact-log10 (number)
  "Return the largest E such that (<= (expt 10 e) number).  The second value
is (/ number (expt 10 e)).  The algorithm uses rationals, and is expected to
terminate in the first step, unless round-off errors occur, when it might take
more steps, but they are always corrected.  Both arguments returned are
rationals."
  (let* ((exponent (floor (log number 10)))
         (number (rational number))
         (lower-limit (exp10 exponent)))
    (loop
      (if (< number lower-limit)
          (setf lower-limit (/ lower-limit 10)
                exponent (1- exponent))
          (let ((upper-limit (* 10 lower-limit)))
            (if (< number upper-limit)
                (return-from exact-log10
                  (values exponent (/ number lower-limit)))
                (setf lower-limit upper-limit
                      exponent (1+ exponent))))))))

(defclass format-options ()
  ((min-exponent :accessor min-exponent :initarg :min-exponent :documentation
                 "Floats below this value are printed in standard form.")
   (max-exponent :accessor max-exponent :initarg :max-exponent :documentation
                 "Floats (and maybe integers, see STANDARDIZE-INTEGER?) above
                 this value are printed in standard form.")
   (digits :accessor digits :initarg :digits :initform 4
           :documentation "Number of digits after the decimal dot.")
   (special-values :accessor special-values :initarg :special-values
                   :initform '((nil "n/a")) :documentation
                   "Lists of (value output).  When a cell is EQUAL to VALUE,
                   OUTPUT will be used in the table.")
   (standardize-integer? :accessor standardize-integer? :initform t :initarg
                         :standardize-integer? :documentation
                         "When non-nil, integers above MAX-EXPONENT will be
   treated as floats and standardized.")))

(defmethod initialize-instance :after ((format-options format-options)
                                       &key &allow-other-keys)
  (let+ (((&slots min-exponent max-exponent digits special-values)
          format-options))
    (check-type digits (integer 0))
    (check-type special-values list)
    (if (slot-boundp format-options 'min-exponent)
        (check-type min-exponent real)
        (setf min-exponent (exp10 (- digits))))
    (if (slot-boundp format-options 'max-exponent)
        (check-type max-exponent real)
        (setf max-exponent (exp10 (1- digits))))))

(defparameter *default-format-options* (make-instance 'format-options)
  "Default options for formatting cells.")

(defun math-inline (&rest arguments)
  "Convenience function for formatting arguments (using ~A) in math mode.  No
separator is included."
  (format nil "$~{~a~}$" arguments))

(defun format-base10 (rational digits)
  "Return two string values, containing the formatted integer and fractional
parts of a (positive) rational."
  (assert (<= 0 rational))
  (let+ (((&values int frac) (floor rational)))
    (values (format nil "~d" int)
            (format nil ".~v,'0d" digits (round frac (exp10 (- digits)))))))

(defun format-float (float format-options)
  "Format float for LaTeX.  Return (list int-string frac-string), where the
latter may include the exponent if float is outside min-exponent and
max-exponent.  Requires math mode in LaTeX."
  (let+ (((&slots-r/o digits min-exponent max-exponent) format-options)
         (rational (rationalize float))
         ((&values abs negative?) (if (minusp rational)
                                      (values (- rational) t)
                                      (values rational nil)))
         ((&values int-string frac-string)
          (if (<= min-exponent abs max-exponent)
              (format-base10 abs digits)
              (let+ (((&values exponent mantissa) (exact-log10 abs))
                     ((&values int-string frac-string)
                      (format-base10 mantissa digits)))
                (values int-string
                        (format nil "~A{\\times}10^{~A}"
                                frac-string exponent))))))
    `(:align ,(math-inline (if negative? "-" "") int-string)
             ,(math-inline frac-string))))

(defun format-value (value format-options)
  "Format VALUE according to FORMAT-OPTIONS.  Return a value that is suitable
for a cell in an array passed to RAW-TABULAR."
  (let+ (((&slots-r/o max-exponent standardize-integer?) format-options))
    (acond
      ((find value (special-values format-options) :key #'car :test #'equal)
       (second it))
      ;; passed through
      ((listp value) value)
      ((stringp value) value)
      ;; formatted as number
      ((integerp value)
       (if (and standardize-integer? (<= max-exponent value))
           (format-float value format-options)
           `(:align ,(math-inline value) "")))
      ((floatp value) (format-float value format-options))
      ((complexp value) (error "complex numbers not implemented yet"))
      ((rationalp value) `(:align (math-inline (numerator value))
                                  (math-inline "/" (denominator value))))
      ;; written as a string
      (t (format nil "~A" value)))))

(defun make-format-options (object ncol)
  "Make a vector FORMAT-OPTIONS from objects from an non-vector (used
repeatedly) or a vector.  Each element (or the atom) may be already be such an
object (passed through), a LIST (used with MAKE-INSTANCE), or NIL (using the
default).

For internal use, in LABELED-* macros.  If NCOL is non-nil, a vector argument
is returned (using identical elements, if OBJECT is not a vector), if it is
NIL, then the value is always a single element which can be used directly."
  (flet ((process (object)
           (etypecase object
             (null *default-format-options*)
             (format-options object)
             (list (apply #'make-instance 'format-options object)))))
    (cond
      ((vectorp object)
       (assert ncol () "Expecting a single element as FORMAT-OPTIONS.")
       (assert (= (length object) ncol) ()
               "Incompatible length for FORMAT-OPTIONS.")
       (map 'vector #'process object))
      (ncol (make-array ncol :initial-element (process object)))
      (t (process object)))))

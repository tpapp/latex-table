(in-package :latex-table)

;;;; default parameters used by high-level functions

(defparameter *default-special-values* '((nil . "n/a")))
(defparameter *default-significant-digits* 3)

;;;;  parse-line-placement 
;;;;
;;;;

(defun parse-position-type-pairs (n position-type-pairs type-default)
  "There are two ways to specify horizontal/vertical lines for tables:
one by giving a vector which is passed directly to raw-tabular, the
other is by giving a _flat_ list line/type pairs, which is processed
by this function in raw-tabular.  The syntax is the following:

line-type-pairs is (list pos1 type1 pos2 type2 ...)

where pos is either a number for the line position, which can also be
negative (then it is counted from the end, ie -1 is the last one), or
you can also give

:default type

and

:offset position

pairs, one of each at maximum, determining the default and the
offset (a number added to the position).  Offset will only affect
positive positions.  positions that occur multiple times are not an
error, only the latest counts.

Vectors are just checked for the correct length and returned.

Examples:

 (parse-position-type-pairs 5 '(1 2 -3 1) 0)        => #(0 2 1 0 0)
 (parse-position-type-pairs 5 '(:default 1 2 0) 0)  => #(1 1 0 1 1 1)
 (parse-position-type-pairs 5 '(:offset 1 2 1) 0)   => #(0 0 0 1 0)
"
  (let (default offset)
    ;; sweep for offset and default
    (iter
      (for (line type) :on position-type-pairs :by #'cddr)
      (case line
	(:default
	 (if default
	     (error "default is set twice")
	     (setf default type)))
	(:offset
	 (if offset
	     (error "offset is set twice")
	     (setf offset type)))))
    ;; if not found, set default values
    (unless default (setf default type-default))
    (unless offset (setf offset 0))
    ;; sweep again for pairs
    (let ((lines (make-array n :element-type t :initial-element default)))
      ;; set line & type pairs, extract default
      (iter
	(for (line type) :on position-type-pairs :by #'cddr)
	(cond
	  ((null type) (error "odd number of elements or nil in ~
position-type-pairs"))
	     ((find line '(:default :offset))) ; silently ignore
	     (t (setf (aref lines (if (minusp line)
				      (+ n line)
				      (+ line offset)))
		      type))))
	 ;; return lines
	 lines)))

(defun raw-tabular (stream matrix coltypes vlines hlines &key (position :top)
		    (environment "tabular"))
  "Output matrix in a latex tabular environment (you can also specify
another environment).  The elements of matrix are either atoms, or
lists of the following format:

 (:left string) -- flushed left
 (:right string) -- flushed right
 (:center string) -- centered
 (:multicolumn cols pos string) -- LaTeX multicolumn, subsequent cols-1 elements
    are ignored, pos is one of :left, :right, :center.

The first three are implemented via multicolumn.

The following specifications are valid for column types:

 character or string -- will be sent to stream directly
 :left, :center, :right -- corresponds to l, c, and r
 (:d sep.tex sep.dvi digits)
 (:d sep.tex sep.dvi digits.left digits.right)

For the last two, you need version >=1.03 of David Carlisle's dcolumn
package.  coltypes can be a list, which will be parsed by parsed by
parse-position-type-pairs (with :center as the default).

vlines and hlines need to be vectors one element longer than the
number of columns/rows, respectively, or lists, which are parsed by
parse-line-placement.  They can contain small nonnegative integers 0,
1, 2 and 3 (mapping to the right number of lines), and for vlines,
also arbitrary strings (eg \"|\", \"@{\\ }\") and characters (#\|,
etc).  The latter two are not checked for correctness in LaTeX.

position corresponds to LaTeX tabular's pos argument for vertical
position, and can be either :top or :bottom.
"
  (assert (and (arrayp matrix) (= (array-rank matrix) 2)))
  (labels ((multicolumn (cols pos text)
	     "Send a multicolumn formatting spec to stream."
	     (format stream "\\multicolumn{~a}{~a}{~a}"
		     cols 
		     (ecase pos 
		       (:left "l") (:right "r") (:center "c"))
		     text))
	   (repeat (string n)
	     ;; Send string to stream n times.
	     (dotimes (i n)
	       (princ string stream)))
	   (parse-spec (n spec default varname)
	     ;; perse spec into a vector, or check consistency (length = n)
	     (cond
	       ((vectorp spec) (if (= (length spec) n)
				   spec
				   (error "if ~a is a vector, ~
                                           it should have length ~a" varname n)))
	       ((listp spec) (parse-position-type-pairs n spec default))
	       (t (error "can't parse ~a" varname))))
	   (vline (type)
	     "Return a string for each vline specification."
	     (etypecase type
	       ((integer 0) (repeat "|" type))
	       (character (princ (string type) stream))
	       (string (princ type stream))))
	   (hline (type separator)
	     (check-type type (integer 0))
	     (repeat "\\hline" type)
	     (when (plusp type)
	       (princ separator stream))))
    ;; check consistency of parameters, parse when necessary
    (bind (((nrow ncol) (array-dimensions matrix))
	   (coltypes (parse-spec ncol coltypes :center 'coltypes))
	   (vlines (parse-spec (1+ ncol) vlines 0 'vlines))
	   (hlines (parse-spec (1+ nrow) hlines 0 'hlines)))
      ;; header
      (format stream "\\begin{~a}[~a]{" environment
	      (ecase position
		(:top "t")
		(:bottom "b")))
      (iter
	(for coltype :in-vector coltypes)
	(for vline :in-vector vlines)
	(vline vline)
	(princ (cond
		 ((eq coltype :left) #\l)
		 ((eq coltype :right) #\r)
		 ((eq coltype :center) #\c)
		 ((and (listp coltype) (eq (car coltype) :d))
		  (bind (((nil sep.tex sep.dvi left.digits &optional right.digits)
			  coltype))
		    (if right.digits
			(format nil "D{~a}{~a}{~a.~a}"
				sep.tex sep.dvi left.digits right.digits)
			(format nil "D{~a}{~a}{~a}"
				sep.tex sep.dvi left.digits))))
		 (t (error "unable to parse column type ~a" coltype)))
	       stream))
      ;; last column and closing
      (vline (aref vlines (1- (length vlines))))
      (format stream "}~%" )
      ;; matrix
      (dotimes (i nrow)
	;; hline spec, before each row
	(hline (aref hlines i) " ")
	;; cells in each row
	(let ((multicol-countdown 0))
	  (dotimes (j ncol)
	    (if (plusp multicol-countdown)
		;; do not even parse cell, just skip and ignore
		(decf multicol-countdown)
		;; parse cell
		(let ((cell (aref matrix i j)))
		  ;; here we verify correctness of all forms, and
		  ;; separate type from parameters
		  (if (atom cell)
		      ;; print it out to stream
		      (princ cell stream)
		      ;; not atom, identify various keywords
		      (let ((type (car cell))
			    (params (cdr cell)))
			(cond
			  ((and (member type '(:left :right :center))
				(not (cdr params)))
			   (multicolumn 1 type (car params)))
			  ((and (eq type :multicolumn) (= (length params) 1))
			   (bind (((col pos a) params))
			     ;; we skip the next (1- col) cells
			     (setf multicol-countdown (1- col))
			     (multicolumn col pos a)))
			  (t (error "can't parse cell ~a" cell)))))))
	    ;; close with & or \\
	    (if (= (1- ncol) j)
		;; technically, a nonzero multicol-countdown would lead
		;; to malformed LaTeX code here, but that can never
		;; ("should not") occur
		(format stream "\\\\~%")
		(when (zerop multicol-countdown)
		  (format stream " & "))))))
      ;; hline spec, for last row
      (hline (aref hlines nrow) #\newline)
      ;; closing
      (format stream "\\end{~a}~%" environment))))

(defun format-cell (x special-values digits)
  "If x is find in the assoc-list special-values, return the
  corresponding value.  Otherwise, floats are converted to string and
  rounded."
  (let ((entry (assoc x special-values :test #'equal)))
    (cond
      (entry (cdr entry))
      ((typep x 'float) (format nil "~,vf" digits x))
      (t x))))

(defun expand-significant-digits (significant-digits ncol coltype)
  "Expand significant digits if an atom is given, otherwise just
  parse.  Also construct column types: if coltype is given, it will be
  used, otherwise the dcolumn type is used with digits.
  return (values significant-digits coltypes)."
  (let* ((significant-digits 
	  (parse-position-type-pairs ncol
				     (if (atom significant-digits)
					 (list :default significant-digits)
					 significant-digits)
				     *default-significant-digits*))
	 (coltypes (map 'vector (lambda (sd)
				  (if coltype
				      coltype
				      `(:d "." "." ,sd))) significant-digits)))
    (values significant-digits coltypes)))


(defun labeled-matrix (stream matrix column-labels row-labels &key
		       (significant-digits *default-significant-digits*)
		       (special-values *default-special-values*)
		       (hlines '(1 1)) (vlines '(1 1)) (corner-cell "")
		       (column-label-alignment :left)
		       (coltypes nil))
  "Output matrix as a simple table, with given row and column labels.
If an element is equal to one of the special values, the corresponding
string will appear in the table.

row-labels and column-labels can be either lists or vectors.
"
  (bind (((nrow ncol) (array-dimensions matrix))
	 (m (make-array (list (1+ nrow) (1+ ncol))))
	 (row-labels (coerce row-labels 'vector))
	 (column-labels (coerce column-labels 'vector))
	 ((:values significant-digits coltypes)
	  (expand-significant-digits significant-digits ncol coltypes)))
    ;; corner, row and column labels
    (dotimes (i nrow)
      (setf (aref m (1+ i) 0) (aref row-labels i)))
    (dotimes (j ncol)
      (setf (aref m 0 (1+ j)) (aref column-labels j)))
    (setf (aref m 0 0) corner-cell)
    ;; cells
    (dotimes (i nrow)
      (dotimes (j ncol)
	(setf (aref m (1+ i) (1+ j))
	      (format-cell (aref matrix i j) special-values
			   (aref significant-digits j)))))
    ;; output
    (raw-tabular stream m (concatenate 'vector (vector column-label-alignment)
				       coltypes)
		 vlines hlines)
    (values)))

(defun labeled-vector-horizontal (stream vector labels &key
				  (significant-digits *default-significant-digits*)
				  (special-values *default-special-values*)
				  (hlines (vector 0 1 0))
				  (vlines '(1 1))
				  (coltypes nil))
  "Output vector as a horizontal table.  Significant-digits are
expanded by expand-significant-digits."
  (bind ((vector (coerce vector 'vector))
	 (labels (coerce labels 'vector))
	 (n (length vector))
	 ((:values significant-digits coltypes)
	  (expand-significant-digits significant-digits n coltypes)))
    (assert (= n (length labels)))
    (let ((m (make-array (list 2 n))))
      (dotimes (i n)
	(setf (aref m 0 i) (aref labels i)
	      (aref m 1 i) (format-cell (aref vector i) special-values
					(aref significant-digits i))))
      (raw-tabular stream m coltypes vlines hlines))))

(defun labeled-vector-vertical (stream vector labels &key
				(significant-digits *default-significant-digits*)
				(special-values *default-special-values*)
				(coltypes (list 0 :left :default 
						`(:d "." "." ,significant-digits)))
				(hlines '(1 1))
				(vlines (vector 0 1 0)))
  "Output vector as a vertical table."
  (let* ((vector (coerce vector 'vector))
	 (labels (coerce labels 'vector))
	 (n (length vector)))
    (assert (= n (length labels)))
    (let ((m (make-array (list n 2))))
      (dotimes (i n)
	(setf (aref m i 0) (aref labels i)
	      (aref m i 1) (format-cell (aref vector i) special-values
					significant-digits)))
      (raw-tabular stream m coltypes vlines hlines))))

(defmacro with-table ((stream &key caption label (placement "htbp")) &body body)
  "Put a table environment (with optional caption, label and
placement) around body."
  (once-only (stream caption label placement)
    `(progn
       (format ,stream "\\begin{table}")
       (if ,placement
	   (format ,stream "[~a]" ,placement))
       (terpri ,stream)
       ,@body
       (if ,label
	   (format ,stream "\\label{~a}" ,label))
       (if ,caption
	   (format ,stream "\\caption{~a}" ,caption))
       (format ,stream "\\end{table}~%"))))

(defun math-inline (string)
  "Convenience function for formatting things in math mode."
  (format nil "$~a$" string))

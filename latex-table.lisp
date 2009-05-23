(in-package :latex-table)

;; exported interface for mc/aligned cells, specification in
;; raw-tabular can be used directly, should be stable

(defun multicolumn (cols pos string)
  (list :multicolumn cols pos string))

(defun aligned (x y)
  (list :aligned x y))

(defun raw-tabular (stream matrix coltypes vlines hlines &key (position :top)
		    (environment "tabular"))
  (declare (optimize (debug 3)))
  "Output matrix in a latex tabular environment (you can also specify
another environment).  Matrix can have the following element types:

 string -- aligned according to coltypes (centered for :aligned column type)
 (:left string) -- flushed left
 (:right string) -- flushed right
 (:center string) -- centered
 (:aligned string string) -- aligned pair
 (:aligned string) -- aligned with second part missing
 (:multicolumn cols pos string) -- LaTeX multicolumn, subsequent cols-1 elements
    are ignored

The following column types are
possible: :left, :right, :center, :aligned.  Aligned pairs can only be
accommodated in a column of type :aligned.

vlines and hlines need to be vectors one element longer than the
number of columns/rows, respectively.  They can contain small
nonnegative integers 0, 1, 2 and 3 (mapping to the right number of
lines), and for vlines, also arbitrary strings (eg \"|\", \"@{\\ }\")
and characters (#\|, etc).  The latter two are not checked for
correctness in LaTeX.

position corresponds to LaTeX tabular's pos argument for vertical
position, and can be either :top or :bottom.
"
  ;; Implementation notes: a column of type :aligned is actually two
  ;; columns, separated by @{}.  This is very useful for aligning
  ;; numbers on the decimal dot and similar arrangements, and thus I
  ;; feel it is worth dealing with the extra complexity here so that
  ;; functions calling raw-tabular don't have to.
  (assert (and (arrayp matrix) (= (array-rank matrix) 2)))
  (labels ((multicolumn (cols pos text)
	     (format stream "\\multicolumn{~a}{~a}{~a}"
		     cols 
		     (ecase pos 
		       (:left "l") (:right "r") (:center "c"))
		     text))
	   (vline (type)
	     "Return a string for each vline specification.  Currently
only 0 (no line) and small integers are allowed."
	     (etypecase type
	       ((integer 0) (ecase type	; number of |'s
			      (0 "")
			      (1 "|")
			      (2 "||")
			      (3 "|||")))
	       (character (string type)) ; character to be converted
	       (string type)))		 ; explicit string
	   (hline (type separator)
	     "Output a string for each hline specification _to
stream_.  Currently only 0 (no line) and small integers are allowed.
If the string is nonempty, it is closed with separator."
	     (let ((string (ecase type
			     (0 "")
			     (1 "\\hline")
			     (2 "\\hline\\hline")
			     (3 "\\hline\\hline\\\hline"))))
	       (princ string stream)
	       (princ (if (string= string "") "" separator) stream))))
    (bind (((nrow ncol) (array-dimensions matrix)))
      (assert (and (vectorp coltypes) (= (length coltypes) ncol)
		   (vectorp vlines) (= (length vlines) (1+ ncol))
		   (vectorp hlines) (= (length hlines) (1+ nrow))))
      ;; header
      (format stream "\\begin{~a}[~a]{" environment
	      (ecase position
		(:top "t")
		(:bottom "b")))
      (iter
	(for coltype :in-vector coltypes)
	(for vline :in-vector vlines)
	(format stream "~a~a" 
		(vline vline)
		(ecase coltype
		  (:left "l")
		  (:right "r")
		  (:center "c")
		  (:aligned "r@{}l"))))
      (format stream "~a}~%" (vline (aref vlines (1- (length vlines)))))
      ;; matrix
      (dotimes (i nrow)
	;; hline spec, before each row
	(hline (aref hlines i) " ")
	(let ((multicol-countdown 0))
	  ;; cells in each row
	  (dotimes (j ncol)
	  (if (plusp multicol-countdown)
	      ;; do not even parse cell, just skip and ignore
	      (decf multicol-countdown)
	      ;; parse cell
	      (bind ((cell (aref matrix i j))
		     (coltype (aref coltypes j))
		     ((:values type first second third)
		      ;; here we verify correctness of all forms, and
		      ;; separate type from parameters
		      (if (atom cell)
			;; an atom has type :any
			(values :any cell)
			;; identify various keywords
			(let ((type (car cell))
			      (params (cdr cell)))
			  (ecase type
			    ;; one of the aligned types
			    ((:left :right :center)
			     (bind (((x) params))
			       (when (symbolp x)
				 (error "~a is a symbol" x))
			       (values type x)))
			    ;; aligned pair
			    (:aligned
			     (bind (((a b) params))
			       (values :aligned a b)))
			    ;; multicolumn
			    (:multicolumn
			     (bind (((col pos a) params))
			       ;; we skip the next (1- col) cells
			       (setf multicol-countdown (1- col))
			       ;; calculate the actual number of
			       ;; columns seen by LaTeX, taking
			       ;; :aligned columns into account
			       (let ((total-col
				      (iter
					(for i :from 0 :below col)
					(for coltype :in-vector coltypes :from j)
					(summing (if (eq coltype :aligned) 2 1)))))
				 (values :multicolumn total-col pos a)))))))))
		;; output cell to stream
		(ecase coltype
		  (:aligned
		   (ecase type
		     (:aligned		; aligned pair
		      (format stream "~a & ~a" first second))
		     (:multicolumn	; have already accounted for
					; extra columns above
		      (multicolumn first second third))
		     ((:left :right :center)
		      (multicolumn 2 type first))
		     ((:any)		; any defaults to :center in a mc setting
		      (multicolumn 2 :center first))))
		  ((:left :right :center)
		   (ecase type
		     ((:left :right :center :any)
		      (if (or (eq type coltype) (eq type :any))
			  ;; same type as column, or no type
			  (format stream "~a" first)
			  ;; different type, has to use multicolumn to align
			  (multicolumn 1 type first)))
		     (:multicolumn
		      (multicolumn first second third))
		     (:aligned
		      (error "can't put an aligned cell in a ~
non-aligned column")))))))
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

(defun lines-to-vector (n line-type-pairs)
  "There are two ways to specify horizontal/vertical lines for tables:
one by giving a vector which is passed directly to raw-tabular, the
other is by giving a _flat_ list line/type pairs, which is processed
by this function before being passed to raw-tabular.  The syntax is
the following:

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

 (lines-to-vector 5 '(1 2 -3 1))        => #(0 2 0 1 0 0)
 (lines-to-vector 5 '(:default 1 2 0))  => #(1 1 0 1 1 1)
 (lines-to-vector 5 '(:offset 1 2 1))   => #(0 0 0 1 0 0)
"
  (cond
    ((vectorp line-type-pairs)
     (if (= (1+ n) (length line-type-pairs))
	 line-type-pairs
	 (error "vector does not have the correct length")))
    ((and (listp line-type-pairs) (list-length line-type-pairs))
     (let (default	offset)
       ;; sweep for offset and default
       (iter
	 (for (line type) :on line-type-pairs :by #'cddr)
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
       (unless default (setf default 0))
       (unless offset (setf offset 0))
       ;; sweep again for pairs
       (let ((lines (make-array (1+ n) :element-type t :initial-element default)))
	 ;; set line & type pairs, extract default
	 (iter
	   (for (line type) :on line-type-pairs :by #'cddr)
	   (cond
	     ((null type) (error "odd number of elements or nil in line-type-pairs"))
	     ((find line '(:default :offset))) ; silently ignore
	     (t (setf (aref lines (if (minusp line)
				      (+ n 1 line)
				      (+ line offset)))
		      type))))
	 ;; return lines
	 lines)))
    (t (error "line-type-pairs has to be a vector or a proper list"))))

(defun labeled-matrix (stream matrix column-labels row-labels &key
		       (significant-digits 3) (special-values '((nil "n/a")))
		       (hlines '(1 1)) (vlines '(1 1)) (corner-cell ""))
  "Output matrix as a simple table, with given row and column labels.
The elements of matrix are expected to be numeric, and formatted using
significant-digits.  Exceptions are given in special values, if an
element is equal to one of these, the corresponding string will appear
in the table.

row-labels and column-labels can be either lists or vectors.
"
  (bind (((nrow ncol) (array-dimensions matrix))
	 (m (make-array (list (1+ nrow) (1+ ncol))))
	 (hlines (lines-to-vector (1+ nrow) hlines))
	 (vlines (lines-to-vector (1+ ncol) vlines))
 	 (coltypes (make-array (1+ ncol) :initial-element :aligned))
	 (simple-formatters (cond
			      ((vectorp significant-digits)
			       (assert (= (length significant-digits) ncol))
			       (map 'vector 
				    (lambda (sd)
				      (make-simple-formatter 
				       sd
				       special-values))
				    significant-digits))
			      ((integerp significant-digits)
			       (make-array ncol
					   :initial-element
					   (make-simple-formatter 
					    significant-digits
					    special-values)))
			      (t (error "significant-digits needs to
			      be a vector or an a vector of
			      integers."))))
	 (row-labels (coerce row-labels 'vector))
	 (column-labels (coerce column-labels 'vector)))
    (setf (aref coltypes 0) :left)
    ;; corner, row and column labels
    (dotimes (i nrow)
      (setf (aref m (1+ i) 0) (aref row-labels i)))
    (dotimes (j ncol)
      (setf (aref m 0 (1+ j)) (aref column-labels j)))
    (setf (aref m 0 0) corner-cell)
    (setf (aref coltypes 0) :left)
    ;; cells
    (dotimes (i nrow)
      (dotimes (j ncol)
	(setf (aref m (1+ i) (1+ j))
	      (funcall (aref simple-formatters j) (aref matrix i j)))))
    ;; output
    (raw-tabular stream m coltypes vlines hlines)
    (values)))

(defun labeled-vector-horizontal (stream vector labels &key
				  (significant-digits 3) 
				  (special-values '((nil "n/a")))
				  (hlines (vector 0 1 0))
				  (vlines '(1 1)))
  "Output vector as a horizontal table."
  (let* ((vector (coerce vector 'vector))
	 (labels (coerce labels 'vector))
	 (n (length vector)))
    (assert (= n (length labels)))
    (let* ((m (make-array (list 2 n)))
	   (vlines (lines-to-vector n vlines))
	   (simple-formatter (make-simple-formatter significant-digits
						    special-values))
	   (coltypes (make-array n :initial-element :aligned)))
      (dotimes (i n)
	(setf (aref m 0 i) (aref labels i)
	      (aref m 1 i) (funcall simple-formatter (aref vector i))))
      (raw-tabular stream m coltypes vlines hlines))))

(defun labeled-vector-vertical (stream vector labels &key
				(significant-digits 3)
				(special-values '((nil "n/a")))
				(hlines '(1 1))
				(vlines (vector 0 1 0)))
  "Output vector as a vertical table."
  (let* ((vector (coerce vector 'vector))
	 (labels (coerce labels 'vector))
	 (n (length vector)))
    (assert (= n (length labels)))
    (let* ((m (make-array (list n 2)))
	   (hlines (lines-to-vector n hlines))
	   (simple-formatter (make-simple-formatter significant-digits
						    special-values))
	   (coltypes (make-array 2 :initial-element :aligned)))
      (dotimes (i n)
	(setf (aref m i 0) (aref labels i)
	      (aref m i 1) (funcall simple-formatter (aref vector i))))
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

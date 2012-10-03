(in-package :latex-table)




;;; Telling latex-table about orientation

(defstruct content-mixin content)

(defun content (cell) (content-mixin-content cell))

(defstruct (left (:constructor left (content)) (:include content-mixin)))

(defstruct (right (:constructor right (content)) (:include content-mixin)))

(defstruct (center (:constructor center (content)) (:include content-mixin)))

(deftype justified ()
  '(or left right center))

(defstruct (multicolumn (:constructor multicolumn (content number))
                        (:include content-mixin (content nil :type justified)))
  (number nil :type (integer 1)))




(defgeneric format-content (content)
  (:documentation "Return a string that is understood by LaTeX.")
  (:method ((integer integer))
    (format nil "~d" integer))
  (:method ((real real))
    (format nil "~f" real))
  (:method ((string string))
    string))

(defgeneric format-cell (cell)
  (:method (cell)
    (format-content cell))
  (:method ((cell left))
    (left (format-content (content-mixin-content cell))))
  (:method ((cell right))
    (right (format-content (content-mixin-content cell))))
  (:method ((cell center))
    (center (format-content (content-mixin-content cell))))
  (:method ((cell multicolumn))
    (let+ (((&structure-r/o multicolumn- content number) cell))
      (multicolumn (format-cell content) number))))



(defparameter *output* *standard-output*)

(defmacro with-output ((filespec-or-stream) &body body)
  (once-only (filespec-or-stream)
    (with-unique-names (body-lambda)
      `(flet ((,body-lambda () ,@body))
         (if (streamp ,filespec-or-stream)
             (let ((*output* ,filespec-or-stream))
               (,body-lambda))
             (with-open-file (*output* ,filespec-or-stream
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
               (,body-lambda)))))))

(defun fresh ()
  (fresh-line *output*))

(defun dump (string)
  (princ string *output*))

(defun column-type-string (column-type)
  (ecase column-type
    (left "l") (right "r") (center "c")))

(defun dump-multicolumn (number column-type string)
  (check-type number (integer 1))
  (format *output* "\\multicolumn{~d}{~a}{~a}"
          number
          (column-type-string column-type)
          string))

(defgeneric dump-cell (column-type cell)
  (:method (column-type (cell null)))
  (:method (column-type (cell multicolumn))
    (let+ (((&structure-r/o multicolumn- content number) cell))
      (dump-multicolumn number (type-of content)
                        (content-mixin-content content))))
  (:method (column-type cell)
    (let+ ((cell-type (when (typep cell 'content-mixin)
                        (type-of cell)))
           (string (if cell-type
                       (content-mixin-content cell)
                       cell)))
      (check-type string string)
      (if (and cell-type (not (eq cell-type column-type)))
          (dump-multicolumn 1 cell-type string)
          (dump string)))))



(defclass latex-table ()
  ((column-types :initarg :column-types)
   (cells :initarg :cells)
   (rules :initarg :rules)))

(defun latex-table (cells
                    &key (column-types (make-array (array-dimension cells 1)
                                                   :initial-element 'center)))
  (make-instance 'latex-table :cells cells
                              :column-types column-types))

(defclass latex-tabular ()
  ((column-types :initarg :column-types)
   (cells :initarg :cells)
   (rules :initarg :rules)))

(defun latex-table-to-tabular (latex-table)
  (let+ (((&slots-r/o column-types cells) latex-table)
         ((nrow ncol) (array-dimensions cells))
         (tabular (make-array (list nrow ncol))))
    (loop for row-index below nrow
          do (fresh)
             (loop with multi-left = 0
                   for col-index below ncol
                   for column-type across column-types
                   do (setf (aref tabular row-index col-index)
                            (if (zerop multi-left)
                                (let ((cell (aref cells row-index col-index)))
                                  (when (typep cell 'multicolumn)
                                    (setf multi-left
                                          (1- (multicolumn-number cell))))
                                  (format-cell cell))
                                (prog1 nil
                                  (decf multi-left))))))
    (make-instance 'latex-tabular :column-types column-types :cells tabular)))

(defun write-tabular (filespec latex-tabular)
  (let+ (((&slots-r/o column-types cells) latex-tabular)
         ((nrow ncol) (array-dimensions cells)))
    (with-output (filespec)
      (fresh)
      (dump "\\begin{tabular}{")
      (loop for column-type across column-types
            do (dump (column-type-string column-type)))
      (dump "}")
      (loop for row-index below nrow
            do (fresh)
               (loop for col-index below ncol
                     for column-type across column-types
                     do  (dump-cell column-type
                                    (aref cells row-index col-index))
                        (if (= col-index (1- ncol))
                            (dump " \\\\")
                            (dump " & "))))
      (fresh)
      (dump "\\end{tabular}")
      (fresh))))

(defparameter *l* (latex-table (make-array '(3 2)
                                           :initial-contents `((1 2)
                                                               (3 ,(right 4))
                                                               (,(multicolumn (center 9) 2) foo)))))
(defparameter *t* (latex-table-to-tabular *l*))

(write-tabular *standard-output* *t*)
(write-tabular #P "/tmp/foo.table" *l*)







;; exported interface for mc/aligned cells, specification in
;; raw-tabular can be used directly, should be stable

(defun raw-tabular (stream matrix coltypes vlines hlines &key (position :top)
                                                              (environment "tabular"))
  (declare (optimize (debug 3)))
  "Output matrix in a latex tabular environment (you can also specify another
environment).

Elements of MATRIX have to be one of the following:

 STRING
   aligned according to coltypes (centered for :align column type)

 (:LEFT STRING), (:RIGHT STRING), (:CENTER STRING)
   flushed left/right, or centered

 (:ALIGN STRING STRING)
    aligned pair
 (:ALIGN STRING)
    aligned with second part missing
 (:MULTICOLUMN COLS POS STRING)
   LaTeX multicolumn, subsequent (1- COLS) elements are ignored

The following column types are possible: :LEFT, :RIGHT, :CENTER, :ALIGN.
Aligned pairs can only be accommodated in a column of type :align.

VLINES and HLINES need to be vectors one element longer than the number of
columns/rows, respectively.  They can contain small nonnegative integers 0, 1,
2 and 3 (mapping to the right number of lines), and for vlines, also arbitrary
strings (eg \"|\", \"@{\\ }\") and characters (#\|, etc).  The latter two are
not checked for correctness in LaTeX.

HLINES can also contain :TOP, :MID, and :BOTTOM, which are treated as \toprule
\midrule and \bottomrule.

POSITION corresponds to LaTeX tabular's pos argument for vertical
position, and can be either :TOP or :BOTTOM."
  ;; Implementation notes: a column of type :align is actually two
  ;; columns, separated by @{}.  This is very useful for aligning
  ;; numbers on the decimal dot and similar arrangements, and thus I
  ;; feel it is worth dealing with the extra complexity here so that
  ;; functions calling raw-tabular don't have to.
  (assert (and (arrayp matrix) (= (array-rank matrix) 2)))
  (let ((column-positions
          (iter
            (with cumulative-position := 1) ; LaTeX counts from 0
            (for coltype :in-vector coltypes)
            (when (first-iteration-p)
              (collect cumulative-position :result-type vector))
            (incf cumulative-position (if (eq coltype :align) 2 1))
            (collect cumulative-position :result-type vector))))
    (labels ((multicolumn (cols pos text)
               (format stream "\\multicolumn{~a}{~a}{~a}"
                       cols
                       (ecase pos
                         (:left "l") (:right "r") (:center "c"))
                       text))
             (vline (type)
               ;; Return a string for each vline specification.  Currently only 0
               ;; (no line) and small integers are allowed.
               (etypecase type
                 ((integer 0) (ecase type ; number of |'s
                                (0 "")
                                (1 "|")
                                (2 "||")
                                (3 "|||")))
                 (character (string type)) ; character to be converted
                 (string type)))           ; explicit string
             (process-hline (specification)
               (let+ (((&flet process-list (list)
                         (ecase (car list)
                           ((:top :mid :bottom)
                            (let+ (((head &optional width) list))
                              (princ (ecase head
                                       (:top "\\toprule")
                                       (:mid "\\midrule")
                                       (:bottom "\\bottomrule"))
                                     stream)
                              (when width
                                (format stream "[~A]" width))))
                           (:cmid
                            (let+ (((a b &key trim width) (cdr specification)))
                              (princ "\\cmidrule" stream)
                              (when width
                                (format stream "[~A]" width))
                              (when trim
                                (format stream "(~A)" trim))
                              (format stream "{~A-~A}"
                                      (sub column-positions a)
                                      (let ((b-adj (sub column-positions b)))
                                        (when (eq (aref coltypes b) :align)
                                          (incf b-adj))
                                        b-adj))))))))
                 (etypecase specification
                   (fixnum (unless (zerop specification)
                             (dotimes (i specification)
                               (princ "\\hline" stream))))
                   (keyword (process-list (list specification)))
                   (list (process-list specification))
                   (vector (map nil #'process-hline specification))))))
      (let+ (((nrow ncol) (array-dimensions matrix)))
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
                    (:align "r@{}l"))))
        (format stream "~a}~%" (vline (aref vlines (1- (length vlines)))))
        ;; matrix
        (dotimes (i nrow)
          ;; hline spec, before each row
          (process-hline (aref hlines i))
          (princ #\space stream)
          (let ((multicol-countdown 0))
            ;; cells in each row
            (dotimes (j ncol)
              (if (plusp multicol-countdown)
                  ;; do not even parse cell, just skip and ignore
                  (decf multicol-countdown)
                  ;; parse cell
                  (let+ ((cell (aref matrix i j))
                         (coltype (aref coltypes j))
                         ((&values type first second third)
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
                                   (let+ (((x) params))
                                     (when (symbolp x)
                                       (error "~a is a symbol" x))
                                     (values type x)))
                                  ;; aligned pair
                                  (:align
                                   (let+ (((a b) params))
                                     (values :align a b)))
                                  ;; multicolumn
                                  (:multicolumn
                                   (let+ (((col pos a) params))
                                     ;; we skip the next (1- col) cells
                                     (setf multicol-countdown (1- col))
                                     ;; calculate the actual number of
                                     ;; columns seen by LaTeX, taking
                                     ;; :align columns into account
                                     (let ((total-col
                                             (iter
                                               (for i :from 0 :below col)
                                               (for coltype :in-vector coltypes :from j)
                                               (summing (if (eq coltype :align) 2 1)))))
                                       (values :multicolumn total-col pos a)))))))))
                    ;; output cell to stream
                    (ecase coltype
                      (:align
                       (ecase type
                         (:align        ; aligned pair
                          (format stream "~a & ~a" first second))
                         (:multicolumn  ; have already accounted for
					; extra columns above
                          (multicolumn first second third))
                         ((:left :right :center)
                          (multicolumn 2 type first))
                         ((:any)     ; any defaults to :center in a mc setting
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
                         (:align        ; aligned pair, just center
                          (multicolumn 1 :center (concatenate 'string first second))))))))
              ;; close with & or \\
              (if (= (1- ncol) j)
                  ;; technically, a nonzero multicol-countdown would lead
                  ;; to malformed LaTeX code here, but that can never
                  ;; ("should not") occur
                  (format stream "\\\\~%")
                  (when (zerop multicol-countdown)
                    (format stream " & "))))))
        ;; hline spec, for last row
        (process-hline (aref hlines nrow))
        (princ #\newline stream)
        ;; closing
        (format stream "\\end{~a}~%" environment)))))

(defun lines-to-vector (n line-type-pairs)
  "There are two ways to specify horizontal/vertical lines for tables: one by
giving a vector which is passed directly to raw-tabular, the other is by
giving a _flat_ list line/type pairs, which is processed by this function
before being passed to raw-tabular.  The syntax is the following:

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

(defun labeled-matrix (stream matrix column-labels row-labels
                       &key
                         format-options (hlines '(0 :top 1 :mid -1 :bottom)) vlines
                         (corner-cell ""))
  "Output matrix as a simple table, with given row and column labels.
The elements of matrix are expected to be numeric, and formatted using
FORMAT-OPTIONS.  ROW-LABELS and COLUMN-LABELS are sequences."
  (let+ (((nrow ncol) (array-dimensions matrix))
	 (m (make-array (list (1+ nrow) (1+ ncol))))
	 (hlines (lines-to-vector (1+ nrow) hlines))
	 (vlines (lines-to-vector (1+ ncol) vlines))
 	 (coltypes (make-array (1+ ncol) :initial-element :align))
         (format-options (make-format-options format-options ncol))
	 (row-labels (coerce row-labels 'vector))
	 (column-labels (coerce column-labels 'vector)))
    (setf (aref coltypes 0) :left)
    ;; corner, row and column labels
    (setf (sub m (cons 1 nil) 0) row-labels
          (sub m 0 (cons 1 nil)) column-labels
          (aref m 0 0) corner-cell
          (aref coltypes 0) :left)
    ;; cells
    (dotimes (i nrow)
      (dotimes (j ncol)
	(setf (aref m (1+ i) (1+ j))
              (format-value (aref matrix i j) (aref format-options j)))))
    ;; output
    (raw-tabular stream m coltypes vlines hlines)
    (values)))

(defun labeled-vector-horizontal (stream vector labels &key
                                  format-options
                                  (hlines (vector 0 1 0))
                                  vlines)
  "Output vector as a horizontal table."
  (let* ((vector (coerce vector 'vector))
         (labels (coerce labels 'vector))
         (n (length vector)))
    (assert (= n (length labels)))
    (let* ((m (make-array (list 2 n)))
           (vlines (lines-to-vector n vlines))
           (format-options (make-format-options format-options n))
           (coltypes (make-array n :initial-element :align)))
      (setf (sub m 0 t) labels)
      (setf (sub m 1 t) (map 'vector #'format-value vector format-options))
      (raw-tabular stream m coltypes vlines hlines))))

(defun labeled-vector-vertical (stream vector labels &key
                                format-options
				hlines
				(vlines (vector 0 1 0)))
  "Output vector as a vertical table."
  (let* ((vector (coerce vector 'vector))
	 (labels (coerce labels 'vector))
	 (n (length vector)))
    (assert (= n (length labels)))
    (let* ((m (make-array (list n 2)))
	   (hlines (lines-to-vector n hlines))
           (format-options (make-format-options format-options nil))
	   (coltypes (make-array 2 :initial-element :align)))
      (setf (sub m t 0) labels)
      (setf (sub m t 1) (map 'vector (lambda (v) (format-value v format-options))
                             vector))
      (raw-tabular stream m coltypes vlines hlines))))

(defmacro with-table ((stream &key caption label (placement "htbp")) &body body)
  "Put a table environment (with optional caption, label and placement) around
 body."
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

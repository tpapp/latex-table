;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:latex-table)

;;; cell wrappers
;;;
;;; Cell wrappers modify the placement of cell content.  They do not affect
;;; formatting.

(deftype alignment ()
  "Allowed types of alignment."
  '(member :left :right :center))

(defstruct (aligned (:constructor align (alignment content)))
  "Wrapper structure for aligned content."
  (alignment nil :type alignment)
  (content))

(define-structure-let+ (aligned) alignment content)

(defstruct (multicolumn (:constructor multicolumn (alignment content number)))
  "Wrapper structure for multicolumn content, spanning NUMBER of columns,
aligned according to ALIGNMENT."
  (alignment nil :type alignment)
  (content)
  (number nil :type (integer 1)))

(define-structure-let+ (multicolumn) alignment content number)

;;; column types
;;;
;;; Column types provide default alignment for cell content (which may be
;;; modified by cell wrappers above) and may also influence formatting.

(defstruct (numprint (:constructor numprint))
  "Numprint n{before}{after} column type."
  (digits-before-decimal *digits-before-decimal* :type (integer 1))
  (digits-after-decimal *digits-after-decimal* :type (integer 0)))

(define-structure-let+ (numprint) digits-before-decimal digits-after-decimal)

(deftype column-type ()
  "Column types recognized by tables."
  `(or numprint (member :left :right :center)))

;;; rules

(deftype rule ()
  "Horizontal rule types recognized by tables."
  '(member :top :bottom :middle))

;;; table representations

(defclass table-mixin ()
  ((cells :initarg :cells :type (array * (* *)))
   (column-types :initarg :column-types :type vector)
   (rules :initarg :rules :type vector))
  (:documentation "Mixin class for table of values, stored in CELLS.
COLUMN-TYPES specify the column types, and RULES the rules (you one more than
the rows in CELLS)"))

(defun check-table-consistency (cells column-types rules)
  "Check that the dimensions of the table definition are consistent."
  (check-type cells (array * (* *)))
  (check-type column-types vector)
  (check-type rules vector)
  (let+ (((nrow ncol) (array-dimensions cells)))
    (assert (length= column-types ncol))
    (assert (length= rules (1+ nrow)))
    ;; FIXME check types of elements in these two vectors
    ))

(defmethod initialize-instance :after
    ((table table-mixin) &key cells column-types rules &allow-other-keys)
  (check-table-consistency cells column-types rules))

(defclass raw-table (table-mixin)
  ()
  (:documentation "Internal representation of a table.  CELLS may only contain
 - strings,
 - ALIGNED elements with string content,
 - MULTICOLUMN elements with string content,
 - the appropriate number of NILs after MULTICOLUMN, which are skipped.

This format is for internal use only, and RAW-TABLE is not exported."))

(defclass table (table-mixin)
  ()
  (:documentation "User-constructed representation of a table.  Cells are
formatted and their alignment is resolved according to the column types.
Contents of cells that coincide with multicolumn tables are ignored."))

(defun expand-to-vector (length position-value-pairs
                         &optional initial-element)
  "Construct a vector of given LENTH from (position . value) pairs.  Negative
positions are counted from the end of the vector (eg -1 is for the last
element), while a position T sets all elements."
  (aprog1 (make-array length :initial-element initial-element)
    (loop for (position . value) in position-value-pairs
          do (etypecase position
               ((eql t) (fill it value))
               (integer (setf (aref it (if (minusp position)
                                           (+ length position)
                                           position))
                              value))))))

(defun ensure-vector (length object &optional initial-element)
  "Return OBJECT as a VECTOR of length, constructing it using EXPAND-TO-VECTOR
if necessary, using the given INITIAL-ELEMENT.  For internal use only, not
exported."
  (aetypecase object
    (vector (assert (length= it length)) it)
    (list (expand-to-vector length it initial-element))
    (t (expand-to-vector length nil object))))


(defun table (cells
                    &key (column-types :right)
                         (rules '((0 . :top) (-1 . :bottom))))
  "Construct a table with the given CELLS, column-types and rules.

RULES and COLUMN-TYPES may be lists of pairs, in which case they are passed on
to EXPAND-TO-VECTOR.  Values which are neither vectors nor lists are
recycled to give a vector of the desired length."
  (let+ (((nrow ncol) (array-dimensions cells)))
    (make-instance 'table
                   :cells cells
                   :column-types (ensure-vector ncol column-types :center)
                   :rules (ensure-vector (1+ nrow) rules nil))))

;;; formatting floats

(defparameter *digits-before-decimal* 4
  "Default number of digits before the decimal point.")

(defparameter *digits-after-decimal* 2
  "Default number of digits after the decimal point.")

(defun format-float (number
                     &key (digits-before-decimal *digits-before-decimal*)
                          (digits-after-decimal *digits-after-decimal*))
  (declare (ignore digits-before-decimal))
  (format nil "~,vf" digits-after-decimal number))



(defgeneric format-content (content)
  (:documentation "Return a string.")
  (:method ((integer integer))
    (format nil "~d" integer))
  (:method ((real real))
    (format-float real))
  (:method ((string string))
    string))

(defgeneric format-cell (cell column-type)
  (:method ((cell multicolumn) column-type)
    ;; multicolumn overrides all column types
    (let+ (((&multicolumn alignment content number) cell))
      (multicolumn alignment (format-content content) number)))
  (:method ((cell aligned) column-type)
    (let+ (((&aligned alignment content) cell)
           (formatted (format-content content)))
      (etypecase column-type
        (alignment (if (eq column-type alignment)
                       formatted
                       (align alignment formatted)))
        (numprint (align alignment formatted)))))
  (:method ((cell real) (numprint numprint))
    (let+ (((&numprint &ign digits-after-decimal) numprint))
      (format-float cell :digits-after-decimal digits-after-decimal)))
  (:method ((cell real) column-type)
    (format-float cell))
  (:method (cell column-type)
    (format-content cell)))



;;; table conversion to `raw' format

(defun table-to-raw (table)
  "Convert table to the `raw' format.  Users should not use the raw format as it
may change without notice, tables should only be constructed using TABLE.  Not
exported, for internal use only."
  (let+ (((&slots-r/o column-types cells rules) table)
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
                                  (format-cell cell column-type))
                                (prog1 nil
                                  (decf multi-left))))))
    (make-instance 'raw-table :column-types column-types :cells tabular
                              :rules rules)))



;;; writing to a stream
;;;
;;;

(defparameter *output* *standard-output*
  "Variable holding the stream output commands write to.")

(defmacro with-output ((filespec-or-stream) &body body)
  "Make output commands write to the given target for the scope of BODY.
Filespecs are opened, streams are used as is."
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
  "Fresh line in output."
  (fresh-line *output*))

(defun dump (string)
  "Write STRING to output."
  (check-type string string)
  (princ string *output*))



;;; LaTeX output
;;;
;;; Produces LaTeX code that relies on standard LaTeX2e + dcolumn (if rules
;;; are used, they are enabled by default) and numprint (for columns of the
;;; numprint type).  The user should load these in LaTeX when including the
;;; tables.

(defun latex-column-type-string (column-type)
  "Return the LaTeX fragment designating a given column type, as a string."
  (etypecase column-type
    ((eql :left) "l")
    ((eql :right) "r")
    ((eql :center) "c")
    (numprint (let+ (((&numprint before after) column-type))
                (format nil "n{~d}{~d}" before after)))))

(defun latex-multicolumn (number column-type string)
  "Write a LaTeX \\multicolumn fragment."
  (check-type number (integer 1))
  (check-type string string)
  (format *output* "\\multicolumn{~d}{~a}{~a}"
          number
          (latex-column-type-string column-type)
          string))

(defgeneric latex-cell (cell)
  (:documentation "Write a cell in LaTeX format.")
  (:method ((cell null)))
  (:method ((cell multicolumn))
    (let+ (((&multicolumn alignment content number) cell))
      (latex-multicolumn number alignment content)))
  (:method ((cell aligned))
    (let+ (((&aligned alignment content) cell))
      (latex-multicolumn 1 alignment content)))
  (:method ((cell string))
    (dump cell)))

(defgeneric write-latex (filespec-or-stream table)
  (:documentation "Write TABLE to FILESPEC-OR-STREAM for LaTeX.")
  (:method (filespec-or-stream (raw-table raw-table))
    (let+ (((&slots-r/o column-types cells) raw-table)
           ((nrow ncol) (array-dimensions cells)))
      (with-output (filespec-or-stream)
        (fresh)
        (dump "\\begin{tabular}{")
        (loop for column-type across column-types
              do (dump (latex-column-type-string column-type)))
        (dump "}")
        (loop for row-index below nrow
              do (fresh)
                 (loop for col-index below ncol
                       for column-type across column-types
                       do (let ((cell (aref cells row-index col-index)))
                            (when cell
                              (unless (zerop col-index)
                                (dump " & "))
                              (latex-cell cell)))
                          (when (= col-index (1- ncol))
                            (dump " \\\\"))))
        (fresh)
        (dump "\\end{tabular}")
        (fresh))))
  (:method (filespec-or-stream (table table))
    (write-latex filespec-or-stream (table-to-raw table))))

;;; ASCII output

(defun decimal-position (string)
  "Return the position of the decimal dot in STRING.  If no decimal dot is
present, return the length of the string, which implies that the string will
be aligned left of the decimal dot in numprint columns."
  (let ((position (position #\. string)))
    (aif position it (length string))))

(defun numprint-widths (cell)
  "Break the contents of cell into two parts around the decimal dot, return
the length of each as a cons."
  (let+ (((&flet width2 (string)
            (let ((position (decimal-position string)))
              (cons position (- (length string) position))))))
    (aetypecase cell
      (null (cons 0 0))
      (string (width2 it))
      (aligned (width2 (aligned-content it)))
      (multicolumn (cons 0 0)))))

(defun ascii-column-widths (raw-table)
  "Traverse columns and calculate the column widths, returned as a vector.
Each column with is either an integer or a cons of two integers, the first the
column width, the second the width of the first part (before the decimal
dot)."
  (let+ (((&slots-r/o column-types cells) raw-table)
         ((nrow ncol) (array-dimensions cells))
         (column-widths (map 'vector
                             (lambda (column-type)
                               (etypecase column-type
                                 (alignment 0)
                                 (numprint (cons 0 0))))
                             column-types))
         ((&flet cell-width (cell)
            (aetypecase cell
              (null 0)
              (string (length it))
              (aligned (length (aligned-content it)))
              (multicolumn 0)))))
    (loop for row-index below nrow
          do (loop for col-index below ncol
                   for column-type across column-types
                   do (let ((cell (aref cells row-index col-index)))
                        (if (typep column-type 'numprint)
                            (let+ (((left . right) (numprint-widths cell))
                                   (column-width (aref column-widths col-index)))
                              (maxf (car column-width) left)
                              (maxf (cdr column-width) right))
                            (maxf (aref column-widths col-index)
                                  (cell-width cell))))))
    (map 'vector
         (lambda (w)
           (if (listp w)
               (let+ (((left . right) w))
                 (cons (+ left right) left))
               w))
         column-widths)))

(defun ascii-absolute-positions (column-widths separator-width)
  "Return two values: the first a vector containing the absolute positions in
the format (LIST START END &OPTIONAL OFFSET) where offset is the width of the
part before the decimal dot; the second value the total width of the table."
  (let* ((total-width separator-width)
         (positions (make-array (length column-widths))))
    (loop for column-width across column-widths
          for index from 0
          do (let+ (((&values width offset) (if (consp column-width)
                                                (values (car column-width)
                                                        (cdr column-width))
                                                column-width))
                    (end (+ total-width width)))
               (setf (aref positions index)
                     (if offset
                         (list total-width end offset)
                         (list total-width end))
                     total-width (+ end separator-width))))
    (values positions total-width)))

(defun ascii-buffer-write (buffer start end string offset)
  "Write STRING in BUFFER as if STRING started at (+ START OFFSET) but not
modifying any other elements except the ones between START and END."
  (let* ((length (length string))
         (start2 (+ start offset))
         (end2 (+ start2 length))
         (trim-start (max 0 (- start start2))))
    (replace buffer string :start1 (max start start2) :end1 (min end end2)
             :start2 trim-start)))

(defun ascii-buffer-write-aligned (buffer start end string alignment)
  "Like ASCII-BUFFER-WRITE, but also accepting alignment keywords and
automatically aligning string."
  (let ((width (- end start))
        (length (length string)))
    (ascii-buffer-write buffer start end string
                        (if (integerp alignment)
                            alignment
                            (ecase alignment
                              (:left 0)
                              (:right (- width length))
                              (:center (ceiling (- width length) 2)))))))

(defun ascii-rule (absolute-positions total-width rule)
  "Expand rule into a string."
  (declare (ignore absolute-positions))
  (etypecase rule
    (keyword (make-string total-width
                          :initial-element (ecase rule
                                             ((:top :bottom) #\=)
                                             ((:middle) #\-))))))

(defun ascii-row (absolute-positions total-width column-types cells row-index)
  "Render ROW in ASCII."
  (let+ ((buffer (make-string total-width :initial-element #\space))
         ((&flet write-aligned (string alignment start-index
                                       &optional (end-index start-index))
            (let* ((start-positions (aref absolute-positions start-index))
                   (start (first start-positions)))
              (ascii-buffer-write-aligned buffer start
                                          (second (aref absolute-positions
                                                        end-index))
                                          string
                                          (if (numberp alignment)
                                              (- (third start-positions)
                                                 alignment)
                                              alignment))))))
    (loop for col-index from 0
          for column-type across column-types
          do (aetypecase (aref cells row-index col-index)
               (null)
               (string (if (typep column-type 'numprint)
                           (write-aligned it (car (numprint-widths it)) col-index)
                           (write-aligned it column-type col-index)))
               (aligned (let+ (((&aligned alignment content) it))
                          (write-aligned content alignment col-index)))
               (multicolumn (let+ (((&multicolumn alignment content number) it))
                              (write-aligned content alignment
                                             col-index
                                             (+ col-index number -1))))))
    buffer))

(defvar *ascii-column-separator* "  "
  "The default separator for columns in ASCII output.")

(defgeneric write-ascii (filespec-or-stream table &key column-separator)
  (:method (filespec-or-stream (raw-table raw-table)
            &key (column-separator *ascii-column-separator*))
    (let+ ((column-widths (ascii-column-widths raw-table))
           ((&values absolute-positions total-width)
            (ascii-absolute-positions column-widths (length column-separator)))
           ((&slots-r/o column-types cells rules) raw-table)
           ((&flet write-rule (index)
              (awhen (aref rules index)
                (fresh)
                (dump (ascii-rule absolute-positions total-width it))))))
      (with-output (filespec-or-stream)
        (write-rule 0)
        (loop for row-index below (array-dimension cells 0)
              do (fresh)
                 (dump (ascii-row absolute-positions total-width column-types
                                  cells row-index))
                 (write-rule (1+ row-index))))))
  (:method (filespec-or-stream (table table)
            &key (column-separator *ascii-column-separator*))
    (write-ascii filespec-or-stream (table-to-raw table)
                 :column-separator column-separator)))




;; (defparameter *t* (latex-table-to-raw *l*))

;; (write-ascii *standard-output* *t*)

;; (write-raw-latex *standard-output* *t*)
;; (write-raw-latex #P"/tmp/foo.table" *t*)

;; (defun labeled-matrix (stream matrix column-labels row-labels
;;                        &key
;;                          format-options (hlines '(0 :top 1 :mid -1 :bottom)) vlines
;;                          (corner-cell ""))
;;   "Output matrix as a simple table, with given row and column labels.
;; The elements of matrix are expected to be numeric, and formatted using
;; FORMAT-OPTIONS.  ROW-LABELS and COLUMN-LABELS are sequences."
;;   (let+ (((nrow ncol) (array-dimensions matrix))
;; 	 (m (make-array (list (1+ nrow) (1+ ncol))))
;; 	 (hlines (lines-to-vector (1+ nrow) hlines))
;; 	 (vlines (lines-to-vector (1+ ncol) vlines))
;;  	 (coltypes (make-array (1+ ncol) :initial-element :align))
;;          (format-options (make-format-options format-options ncol))
;; 	 (row-labels (coerce row-labels 'vector))
;; 	 (column-labels (coerce column-labels 'vector)))
;;     (setf (aref coltypes 0) :left)
;;     ;; corner, row and column labels
;;     (setf (sub m (cons 1 nil) 0) row-labels
;;           (sub m 0 (cons 1 nil)) column-labels
;;           (aref m 0 0) corner-cell
;;           (aref coltypes 0) :left)
;;     ;; cells
;;     (dotimes (i nrow)
;;       (dotimes (j ncol)
;; 	(setf (aref m (1+ i) (1+ j))
;;               (format-value (aref matrix i j) (aref format-options j)))))
;;     ;; output
;;     (raw-tabular stream m coltypes vlines hlines)
;;     (values)))

;; (defun labeled-vector-horizontal (stream vector labels &key
;;                                   format-options
;;                                   (hlines (vector 0 1 0))
;;                                   vlines)
;;   "Output vector as a horizontal table."
;;   (let* ((vector (coerce vector 'vector))
;;          (labels (coerce labels 'vector))
;;          (n (length vector)))
;;     (assert (= n (length labels)))
;;     (let* ((m (make-array (list 2 n)))
;;            (vlines (lines-to-vector n vlines))
;;            (format-options (make-format-options format-options n))
;;            (coltypes (make-array n :initial-element :align)))
;;       (setf (sub m 0 t) labels)
;;       (setf (sub m 1 t) (map 'vector #'format-value vector format-options))
;;       (raw-tabular stream m coltypes vlines hlines))))

;; (defun labeled-vector-vertical (stream vector labels &key
;;                                 format-options
;; 				hlines
;; 				(vlines (vector 0 1 0)))
;;   "Output vector as a vertical table."
;;   (let* ((vector (coerce vector 'vector))
;; 	 (labels (coerce labels 'vector))
;; 	 (n (length vector)))
;;     (assert (= n (length labels)))
;;     (let* ((m (make-array (list n 2)))
;; 	   (hlines (lines-to-vector n hlines))
;;            (format-options (make-format-options format-options nil))
;; 	   (coltypes (make-array 2 :initial-element :align)))
;;       (setf (sub m t 0) labels)
;;       (setf (sub m t 1) (map 'vector (lambda (v) (format-value v format-options))
;;                              vector))
;;       (raw-tabular stream m coltypes vlines hlines))))

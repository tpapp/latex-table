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

(defparameter *digits-before-decimal* 4
  "Default number of digits before the decimal point.")

(defparameter *digits-after-decimal* 2
  "Default number of digits after the decimal point.")

(defstruct (numprint
            (:constructor numprint (digits-after-decimal
                                    &optional digits-before-decimal)))
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

(defun ensure-matrix (object)
  "Return OBJECT as a 2D array if possible, otherwise signal an error."
  (flet ((error% ()
           (error "Could not convert ~A to a 2D array." object)))
    (atypecase object
      ((array * (* *)) it)
      (sequence (let ((first (elt object 0)))
                  (if (typep first 'sequence)
                      (make-array (list (length it) (length first))
                                  :initial-contents it)
                      (error%))))
      (t (error%)))))


(defun table (cells &key (column-types :right)
                         (rules '((0 . :top) (-1 . :bottom))))
  "Construct a table with the given CELLS, COLUMN-TYPES and RULES.

CELLS may be a 2D array or a sequence of sequences, converted to a 2D array.

RULES and COLUMN-TYPES may be lists of pairs, in which case they are passed on
to EXPAND-TO-VECTOR.  Values which are neither vectors nor lists are recycled
to give a vector of the desired length."
  (let+ ((cells (ensure-matrix cells))
         ((nrow ncol) (array-dimensions cells)))
    (make-instance 'table
                   :cells cells
                   :column-types (ensure-vector ncol column-types :center)
                   :rules (ensure-vector (1+ nrow) rules nil))))

;;; formatting floats

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
    string)
  (:method ((symbol symbol))
    (symbol-name symbol)))

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
      (aligned (width2 (aligned-content it))) ; FIXME this is incorrect, alignment happens after
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

(defun ascii-row (absolute-positions total-width column-types row)
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
          for element across row
          for column-type across column-types
          do (aetypecase element
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
              for row across (ao:split cells 1)
              do (fresh)
                 (dump (ascii-row absolute-positions total-width column-types
                                  row))
                 (write-rule (1+ row-index))))))
  (:method (filespec-or-stream (table table)
            &key (column-separator *ascii-column-separator*))
    (write-ascii filespec-or-stream (table-to-raw table)
                 :column-separator column-separator)))




;;; convenience functions for constructing tables

(defun alignf (cells row-index col-index &optional (alignment :center))
  "Wrap cell at indexes as aligned."
  (setf (aref cells row-index col-index)
        (align alignment (aref cells row-index col-index))))

(defun multicolumnf (cells row-index col-index number
                     &optional (alignment :center))
  "Wrap cell at indexes as multicolumn."
  (setf (aref cells row-index col-index)
        (multicolumn alignment (aref cells row-index col-index) number)))

(defun labeled-vertical (labels values &key (labels-column :left)
                          (values-column :right) header?)
  "Create a table labeling a vector as a vertical column.  When HEADER? is
set, the top cells are treated as headers and centered, except when HEADER? is
'MULTICOLUMN, which centers it across the two columns."
  (let* ((cells (ao:stack 1 (ao:reshape labels '(t 1)) (ao:reshape values '(t 1))))
         (rules '((0 . :top) (-1 . :bottom))))
    (when header?
      (if (eq header? 'multicolumn)
          (multicolumnf cells 0 0 2)
          (progn
            (alignf cells 0 0)
            (alignf cells 0 1)))
      (push '(1 . :middle) rules))
    (table cells
           :rules rules :column-types (vector labels-column values-column))))

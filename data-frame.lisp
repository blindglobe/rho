;;;; -*- Mode: Lisp -*-

;;;; data-frame.lisp --
;;;;
;;;; See the file COPYING for licencing and copyright information.

(in-package "RHO")

;;(eval-when (:load-toplevel :compile-toplevel :execute)
;;  (shadow '(cl:length)))
;; renamed length to lengthv, eliminating the above

(defstruct (strand
            (:constructor %make-strand (name
                                        data
                                        element-type)))
  "The Strand Structure.

A 'strand' is a 'named column' in R.

It consists of a NAME (currently a symbol, but probably better as a string?),
a vector containing the data (element type and values not preset),
the element type (where T is the universal type).

You cannot rely on UPGRADED-ARRAY-ELEMENT-TYPE (U-A-E-T in the sequel)
because most implementations are lazy."

  (name nil :type symbol)
  (data #() :type vector) 
  (element-type T))


(defun make-strand (name
                    data
                    &optional
                    (element-type (array-element-type data)))
  "Params:
NAME:         symbol,
DATA:         vector
ELEMENT-TYPE: type  (opt)."
  (declare (type symbol name)
           (type vector data))
  (assert (every (lambda (d) (typep d element-type)) data))
  (%make-strand name data element-type))


(defmethod print-object ((s strand) stream)
  (let ((set (strand-element-type s)))
    (print-unreadable-object (s stream)
      (format stream "VAR ~A~:[ [~A]~;~*~] ->~{ ~S~}"
              (strand-name s)
              (eq T set)
              set
              (coerce (strand-data s) 'list)))))

(defgeneric lengthv (x)
  (:method ((x strand))
   (cl:length (strand-data x)))
  (:method ((x vector))
   (cl:length x))
  (:method ((x list))
   (cl:list-length x)))

(defstruct (data-frame
	     (:constructor %make-data-frame (columns)))
  (columns #() :type (vector strand)) ; You cannot be more precise than this.

  ;; MA sez: add hash tables a gogo if you want to address things by
  ;; 'name'...

  ;; AR sez: so we need possibly a hash table for variables, and a
  ;; hash table for observations
  )


(defmethod print-object ((df data-frame) stream)
  (let ((cs (data-frame-columns df)))
    (print-unreadable-object (df stream)
      (format stream "DATA-FRAME [~D]~{ ~S~}"
              (lengthv cs)
              (coerce cs 'list)))))


(defun make-data-frame (&rest cols-specs)
  "Simple version, YMMV.

 Each element of COLS-SPEC is one of:

 1 - a triple       : (name data element-type)
 2 - a pair         : (name data)
 3 - a NAMED-COLUMN : (ie. a STRAND).

TODO: allow for generic naming if there is no name given (but this
could be done at the strand level, as well, so needs to be thought
over a bit)."

  (flet ((process-col-spec (col-spec)
           (etypecase col-spec
             (strand col-spec)
             (list (destructuring-bind (name data &optional (et t et-supplied-p))
                       col-spec
		     
                     (assert (typep data 'sequence))

                     ;; The MAP-INTO may be an overkill, but this lets
                     ;; you have DATA as a LIST.
                     (let ((et (cond (et-supplied-p et)
                                     ((vectorp data)
                                      (array-element-type data))
                                     (t et))))
                       (make-strand
                        name
                        (map-into (make-array (lengthv data)
                                              :element-type et
                                              ;; Note that the above is
                                              ;; useless most of the
                                              ;; times, due to
                                              ;; U-A-E-T.
					      ;; U-A-element-type
					      ;; behavior.
                                              )
                                  #'identity
                                  data)
                        et)))))))
    
    (loop for col-spec in cols-specs
          collect (process-col-spec col-spec) into cols
          finally
          (assert (apply #'= (mapcar #'lengthv cols))) ; Ok. This is limiting!
          (return (%make-data-frame (coerce cols '(vector strand)))))))


(defun pprint-data-frame (df &optional (out *standard-output*))
  ;; This can be installed with PRINT-OBJECT and extended etc etc

  (let* ((cols (data-frame-columns df))
         (col-names (map 'list #'strand-name cols))
         (col-data-vs (map 'list #'strand-data cols))
         (field-width 10) ; Fixed FTTB.
         (nr (lengthv (first col-data-vs)))
         (rn-width (lengthv (format nil "~D" nr))))

    ;; The FORMATs below can be made smarter.

    ;; blank line, to make STDOUT cleaner in edge cases, this could be optional
    (format out "~%")

    ;; Headers
    (format out "~V,@A~:{~V,@S~}~%"
            rn-width #\Space
            (mapcar (lambda (n) (list field-width n)) col-names))

    ;; Should we add a type here?

    ;; Rows
    (loop for rn from 0 below (lengthv (first col-data-vs))
          for row = (mapcar (lambda (c) (aref c rn)) col-data-vs)
          do (format out "~V,@A~:{~V,@S~}~%"
                     rn-width rn
                     (mapcar (lambda (e) (list field-width e)) row)))))


(defun data-frame-column-names (df)
  (declare (type data-frame df))
  (map 'list #'strand-name (data-frame-columns df)))


(defun data-frame-column-types (df)
  (declare (type data-frame df))
  (map 'list #'strand-element-type (data-frame-columns df)))


(defun data-frame-as-lisp-array (df)
  (let ((cols (map 'list #'strand-data (data-frame-columns df))))
    (make-array (list (lengthv (first cols)) (lengthv cols))
                :initial-contents cols)))


;;; strand and df element accessor:


(defgeneric ref$ (item ref &rest refs))

(defmethod ref$ ((df data-frame) (ref fixnum) &rest refs)
  (let ((e (strand-data (aref (data-frame-columns df) ref))))
    (if refs
        (apply #'ref$ e refs)
        e)))


(defmethod ref$ ((df data-frame) (ref symbol) &rest refs)
  (let ((e (strand-data (find ref (data-frame-columns df)
                              :test #'eq
                              :key #'strand-name))))
    (if refs
        (apply #'ref$ e refs)
        e)))


(defmethod ref$ ((a array) (ref fixnum) &rest refs)
  (let* ((ar (array-rank a))
         (rl (lengthv refs))
         (next-indices (butlast refs (- rl (1- ar))))
         (rest-indices (nthcdr (1- ar) refs))
         (e (apply #'aref a ref next-indices))
         )
    (if rest-indices
        (apply #'ref$ e (first rest-indices) (rest rest-indices))
        e)))


(defmethod ref$ ((v vector) (ref fixnum) &rest refs)
  (let ((e (aref v ref)))
    (if refs
        (apply #'ref$ e refs)
        e)))


(defmethod ref$ ((s sequence) (ref fixnum) &rest refs)
  (let ((e (elt s ref)))
    (if refs
        (apply #'ref$ e refs)
        e)))

;;; if a strand, extract vector and use the vector method...
(defmethod ref$ ((s strand) (ref fixnum) &rest refs)
  (ref$ (strand-data s) ref)) ;; sequences and strands MUST have only 1 index




;;;; SETF methods...

(defgeneric (setf ref$) (v item ref &rest refs))

(defmethod (setf ref$) (v (df data-frame) (ref fixnum) &rest refs)
  (let* ((col (aref (data-frame-columns df) ref))
         (col-type (strand-element-type col))
         (col-data (strand-data col))
        )
    (cond ((null refs)
           (assert (typep v `(vector ,col-type)))
           (setf (strand-data col) v))
          ((= (lengthv refs) 1)
           (assert (typep v col-type))
           (setf (aref col-data (first refs)) v))
          (t
           (setf (apply #'ref$ (aref col-data (first refs)))
                 (rest refs))))))


(defmethod (setf ref$) (v (df data-frame) (ref symbol) &rest refs)
  (let* ((col (find ref (data-frame-columns df)
                    :key #'strand-name
                    :test #'eq))
         (col-type (strand-element-type col))
         (col-data (strand-data col)))
    (cond ((null refs)
           (assert (typep v `(vector ,col-type)))
           (setf (strand-data col) v))
          ((= (lengthv refs) 1)
           (assert (typep v col-type))
           (setf (aref col-data (first refs)) v))
          (t
           (setf (apply #'ref$ (aref col-data (first refs)))
                 (rest refs))))))


(defmethod (setf ref$) (v (vec vector) (ref fixnum) &rest refs)
  (if (null refs)
      (setf (aref vec ref) v)
      (setf (apply #'ref$ (aref vec ref) refs) v)))

;;; You know the drill for the other (SETF REF$)
;;; ...




;;; To find a particular vector/strand (mix between R and LispStat)

(defun find-all-strands-and-dataframes (&key (package *package*))
  "Bad.  "
  (let ((lst (list 'a)))
    (do-symbols (s package) 
      (if (typep s 'strand) 
	  (push s lst))
      (if (typep s 'data-frame)
	  (push (data-frame-column-names s) lst)))
    lst))


#| 

 (defparameter s1 (make-strand 'bzr #(1 2 3) 'fixnum))
 s1

  (let ((lst ()))
    (do-symbols (s package) 
      (if (typep s 'strand) 


 (find-all-strands-and-dataframes :package *package*)
 (find-all-strands-and-dataframes)


|#



	  

;;;; end of file -- data-frame.lisp --

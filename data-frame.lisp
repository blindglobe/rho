;;;; -*- Mode: Lisp -*-

;;;; data-frame.lisp --
;;;;
;;;; See the file COPYING for licencing and copyright information.

(in-package :rho)

;;(eval-when (:load-toplevel :compile-toplevel :execute)
;;  (shadow '(cl:length)))

;; renamed length to lengthv, eliminating the shadow given above.

(defstruct (strand
            (:constructor %make-strand (name
                                        data
                                        element-type)))
  "The Strand Structure.

A 'strand' is a 'named column' in R.

It consists of:
1. a NAME (currently a symbol, but probably better as a string?),
2. DATA, a vector containing the data (element type and values not preset),
3. the ELEMENT-TYPE (where T is the universal type).

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
  "Data model: a vector of strands.

MA sez: add hash tables a gogo if you want to address things by 'name'...

COLUMNS: an vector consisting of vectors and strands.  You cannot be
more precise than this.
"
  (columns #() :type (vector strand)))


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

;; MA's comment regarding requiring equal length columns for variables
;; (vectors|strands) in a variable is a mis-nomer.  At this point, we
;; want to ensure that rows refer to the same observation.  It is just
;; that if data is missing, we need to include a data structure for
;; missingness.  However, that challenge should be handled external to
;; this, not internal to this.  What we might want (related) is a
;; means of having a key to identify variables from the same observation
;; thereby not necessarily allowing a mixture of variable lengths.  

;; The question is not whether such a structure is useful, it is more
;; whether that would be more efficient than the direct assumption
;; (ie. would the additional indirection be valuable?)

(defun pprint-data-frame (df &optional (out *standard-output*))
  ;; This can be installed with PRINT-OBJECT and extended etc etc

  (let* ((cols (data-frame-columns df))
         (col-names (map 'list #'strand-name cols))
	 (col-types (map 'list #'strand-element-type cols))
         (col-data-vs (map 'list #'strand-data cols))
         (field-width 20) ; Fixed FTTB.  Originally 10.
         (nr (lengthv (first col-data-vs)))
         (rn-width (lengthv (format nil "~D" nr))))

    ;; The FORMATs below can be made smarter.

    ;; blank line, to make STDOUT cleaner in edge cases, this could be optional
    (format out "~%")

    ;; Variable names
    (format out "~V,@A~:{~V,@S~}~%"
            rn-width #\Space
            (mapcar (lambda (n) (list field-width n)) col-names))

    ;; Variable types: could be optional
    (format out "~V,@A~:{~V,@S~}~%"
            rn-width #\Space
            (mapcar (lambda (n) (list field-width n)) col-types))

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




;;;; Setf methods...

(defgeneric (setf ref$) (v item ref &rest refs))

(defmethod (setf ref$) (v (df data-frame) (ref fixnum) &rest refs)
  (let* ((col (aref (data-frame-columns df) ref))
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

(defmethod (setf ref$) (v (a array) (ref fixnum) &rest refs)
  (error "implement ARRAY setting, called with ~S ~S" ref refs))



(defmethod (setf ref$) (v (vec vector) (ref fixnum) &rest refs)
  (if (null refs)
      (setf (aref vec ref) v)
      (setf (apply #'ref$ (aref vec ref) refs) v)))

(defmethod (setf ref$) (v (seq sequence) (ref fixnum) &rest refs)
  (error "implement SEQUENCE setting, called with ~S ~S" ref refs))

(defmethod (setf ref$) (v (str strand) (ref fixnum) &rest refs)
  (error "implement STRAND setting, called with ~S ~S" ref refs))


;;; You know the drill for the other (SETF REF$)
;;; ...




;;; To find a particular vector/strand (mix between R and LispStat)


;;; Pascal clarifies for me:

;; Variables don't have types. [*] Values have types. So searching for all 
;; variables of certain types is not very meaningful. You may rather want 
;; to search for all values of certain types. 

;; You cannot do this out of the box. However, with structs and classes, 
;; you can define constructors or initializers that store the created 
;; objects in tables. (If these are weak tables, like weak hash tables, you 
;; don't even prevent them from being garbage collected.) 

;; Pascal 

;; [*] You can declare/declaim types for variables, but this may be ignored 
;; by a Common Lisp implementation, and is not recorded in a way that could 
;; be queried, at least not in a portable way. If you want to be able to 
;; query variable types, you may have to implement your own mechanism for 
;; registering such types, but they are then not enforced by the Common 
;; Lisp implementation. 

;;; And Zach gives what I'm looking for... 

;;    (when (and (boundp s) (typep (symbol-value s) target-type)) 
;;      ...) 

(defun every-strand (&key (package *package*))
  "Bad.  What I want is a function that finds all variables in the
PACKAGE of type STRAND or DATA-FRAME, for use in finding things.  One
idea was to cycle through all symbols and record those which are of
the particular type."
  (let ((lst ()))
    (do-symbols (s package) 
      (if (and (boundp s) (typep (symbol-value s) 'STRAND))
	  (push s lst)))
    lst))

(defun every-data-frame (&key (package *package*))
  "Bad.  What I want is a function that finds all variables in the
PACKAGE of type STRAND or DATA-FRAME, for use in finding things.  One
idea was to cycle through all symbols and record those which are of
the particular type."
  (let ((lst ()))
    (do-symbols (s package) 
      (if (and (boundp s) (typep (symbol-value s) 'DATA-FRAME))
	  (push s lst)))
    lst))

#|

(defun every-strand-in-a-data-frame (&key (package *package*))
  "Bad.  What I want is a function that finds all variables in the
PACKAGE of type STRAND or DATA-FRAME, for use in finding things.  One
idea was to cycle through all symbols and record those which are of
the particular type."
  (let ((lst ()))
    (do-symbols (df? package) 
      (if (and (boundp df?) (typep (symbol-value df?) 'DATA-FRAME))
	  ;;(push df? lst) and (push (data-frame-column-names df?) lst) 
	  ))
    lst))

|#







	  

;;;; end of file -- data-frame.lisp --

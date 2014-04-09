;;;; -*- Mode: Lisp -*-

;;;; data-frame.lisp --
;;;;
;;;; See the file COPYING for licencing and copyright information.

(in-package "RHO")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (shadow '(cl:length))
  )


(defstruct (strand
            (:constructor %make-strand (name
                                        data
                                        element-type)))
  "The Strand Structure.

A 'strand' is a 'named colum' in R."
  (name nil :type symbol)
  (data #() :type vector) ; You don't know what element type can go in here.
  (element-type T) ; You cannot rely on UPGRADED-ARRAY-ELEMENT-TYPE
                   ; because most implementations are lazy.
  )


(defun make-strand (name
                    data
                    &optional
                    (element-type (array-element-type data)))
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


(defgeneric length (x)
  (:method ((x strand))
   (cl:length (strand-data x)))
  (:method ((x vector))
   (cl:length x))
  (:method ((x list))
   (cl:list-length x)))


(defstruct (data-frame
            (:constructor %make-data-frame (columns))) 
   (columns #() :type (vector strand)) ; You cannot be more precise than this.

  ;; add hash tables a gogo if you want to address things by 'name'... 
  ) 


(defmethod print-object ((df data-frame) stream)
  (let ((cs (data-frame-columns df)))
    (print-unreadable-object (df stream)
      (format stream "DATA-FRAME [~D]~{ ~S~}"
              (length cs)
              (coerce cs 'list)))))


(defun make-data-frame (&rest cols-specs)
  ;; Simple version, YMMV.
  ;; Each element of COLS-SPEC is:
  ;;
  ;; 1 - a triple
  ;;
  ;;   (name data element-type)
  ;;
  ;; 2 - a pair
  ;;
  ;;   (name data)
  ;;
  ;; 3 - a NAMED-COLUMN

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
                                     (t et)))
                           )
                       (make-strand
                        name
                        (map-into (make-array (length data)
                                              :element-type et
                                              ;; Not that the above is
                                              ;; useless most of the
                                              ;; times, due to
                                              ;; U-A-E-T.
                                              )
                                  #'identity
                                  data)
                        et
                        )
                       )))))
         )
    
    (loop for col-spec in cols-specs
          collect (process-col-spec col-spec) into cols
          finally
          (assert (apply #'= (mapcar #'length cols))) ; Ok. This is limiting!
          (return (%make-data-frame (coerce cols '(vector strand)))))))


(defun pprint-data-frame (df &optional (out *standard-output*))
  ;; This can be installed with PRINT-OBJECT and extended etc etc

  (let* ((cols (data-frame-columns df))
         (col-names (map 'list #'strand-name cols))
         (col-data-vs (map 'list #'strand-data cols))
         (field-width 10) ; Fixed FTTB.
         (nr (length (first col-data-vs)))
         (rn-width (length (format nil "~D" nr)))
         )

    ;; The FORMATs below can be made smarter.

    ;; Headers
    (format out "~V,@A~:{~V,@S~}~%"
            rn-width #\Space
            (mapcar (lambda (n) (list field-width n)) col-names))

    ;; Rows
    (loop for rn from 0 below (length (first col-data-vs))
          for row = (mapcar (lambda (c) (aref c rn)) col-data-vs)
          do (format out "~V,@A~:{~V,@S~}~%"
                     rn-width rn
                     (mapcar (lambda (e) (list field-width e)) row))
    
          )))


(defun data-frame-column-names (df)
  (declare (type data-frame df))
  (map 'list #'strand-name (data-frame-columns df)))


(defun data-frame-column-types (df)
  (declare (type data-frame df))
  (map 'list #'strand-element-type (data-frame-columns df)))


(defun data-frame-as-matrix (df)
  (let ((cols (map 'list #'strand-data (data-frame-columns df))))
    (make-array (list (length (first cols)) (length cols))
                :initial-contents cols)))


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


(defmethod ref$ ((v vector) (ref fixnum) &rest refs)
  (let ((e (aref v ref)))
    (if refs
        (apply #'ref$ e refs)
        e)))


(defmethod ref$ ((s sequence) (ref fixnum) &rest refs)
  (let ((e (nth ref s)))
    (if refs
        (apply #'ref$ e refs)
        e)))


(defmethod ref$ ((a array) (ref fixnum) &rest refs)
  (let* ((ar (array-rank a))
         (rl (length refs))
         (next-indices (butlast refs (- rl (1- ar))))
         (rest-indices (nthcdr (1- ar) refs))
         (e (apply #'aref a ref next-indices))
         )
    (if rest-indices
        (apply #'ref$ e (first rest-indices) (rest rest-indices))
        e)))


;;;; SETF methods...

(defmethod (setf ref$) (v (df data-frame) (ref fixnum) &rest refs)
  (let* ((col (aref (data-frame-columns df) ref))
         (col-type (strand-element-type col))
         (col-data (strand-data col))
        )
    (cond ((null refs)
           (assert (typep v `(vector ,col-type)))
           (setf (strand-data col) v))
          ((= (length refs) 1)
           (assert (typep v col-type))
           (setf (aref col-data (first refs)) v))
          (t
           (setf (apply #'ref$ (aref col-data (first refs)))
                 (rest refs)))
          )))


(defmethod (setf ref$) (v (df data-frame) (ref symbol) &rest refs)
  (let* ((col (find ref (data-frame-columns df)
                    :key #'strand-name
                    :test #'eq))
         (col-type (strand-element-type col))
         (col-data (strand-data col)))
    (cond ((null refs)
           (assert (typep v `(vector ,col-type)))
           (setf (strand-data col) v))
          ((= (length refs) 1)
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



;;;; end of file -- data-frame.lisp --

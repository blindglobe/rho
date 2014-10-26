
(ql:quickload :rho :verbose T)

;; See rho-package.lisp and use RHO-USER as an example for how to use
;; RHO within another package.

(in-package :rho-user)

;; Simple data-frame

(defparameter df-1 (make-data-frame '(foo #(1 2 3)) 
                         '(bar ("a" "s" "d") string) 
                         '(baz (100 102 97) (integer 90 110))))

(pprint-data-frame df-1)

(defparameter df-1-second-column
  (list "a" "s" "d"))

(defparameter df-1-first-row
  (list 1 "a" 100))

(equal (list (ref$ df-1 'bar 0)
	     (ref$ df-1 'bar 1)
	     (ref$ df-1 'bar 2))
       df-1-second-column)

(equal (list (ref$ df-1 1 0)
	     (ref$ df-1 1 1)
	     (ref$ df-1 1 2))
       df-1-second-column)

(equal (list (ref$ df-1 0 0)
	     (ref$ df-1 1 0)
	     (ref$ df-1 2 0))
       df-1-first-row)



(data-frame-column-types df-1)
(data-frame-column-names df-1)
(data-frame-as-lisp-array df-1)

;;; Error, 42 is not a string:  we need to catch this condition, and continue
(ignore-errors
  (setf (ref$ df-1 'bar 2) 42))

;; works, since that is a string
(setf (ref$ df-1 'bar 2) "Works!")

(typep (ref$ df-1 2 1) (ref$ (data-frame-column-types df-1) 2))

df-1

(pprint-data-frame df-1)



#|
  (let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
    (ref$ s 1))
      
  (let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
    (loop
       for i from 0 to 4
       do (assert-equal (ref$ s i) i)))


  (let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
    (loop
       for i from 0 to 4
       do (assert-equal (ref$ 'test-strand i) i)))
|#



;;; simplee but needed example of making a variable of a particular
;;; user-spec'd type.

(defstruct pointSTR (x 0.0 :type float) (y 0.0 :type float))
(defparameter p1 (make-pointSTR :x 2.2 :y 2.3))
p1



(defclass pointCLOS ()
  ((x :type float :initarg :x :initform 0.1)
   (y :type float :initarg :y :initform 0.2))
  (:documentation "silly point class for illustration"))
(defparameter p2 (make-instance 'pointCLOS))
(defparameter p2a (make-instance 'pointCLOS :x 1.0 :y 2.0))
p2
p2a
(list (slot-value p2 'x) (slot-value p2 'y))
(list (slot-value p2a 'x) (slot-value p2a 'y))







(defparameter df-2a
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   (make-strand 'bzr
				(vector (make-pointSTR :x 1.0 :y 2.0)
					(make-pointSTR :x 2.0 :y 2.0)
					(make-pointSTR :x 3.0 :y 2.0))
				'pointSTR)))



df-2a
(pprint df-2a)
(pprint-data-frame df-2a)

(data-frame-column-types df-2a)

;; and we want 

(ref$ df-2a 2 1) ; => 102
(ref$ df-2a 2 2) ; => 97
(ref$ df-2a 3 2) ; => #S(POINTSTR :X 3.0 :Y 2.0)



(vector (make-pointSTR :x 1.0 :y 2.0)
	(make-pointSTR :x 2.0 :y 2.0)
	(make-pointSTR :x 3.0 :y 2.0))


(defparameter df-2
  (make-data-frame '(foo #(1 2 3)) 
		   '(foo2 #(1 2 3) fixnum) 

		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))

		   (make-strand 'bzr
				(vector (make-pointSTR :x 1.0 :y 2.0)
					(make-pointSTR :x 2.0 :y 2.0)
					(make-pointSTR :x 3.0 :y 2.0))
				'pointSTR)

		   ;; (***) The commented out region below was to fit
		   ;; in right here!
		   
		   ))


#|
  These would belong above (see comment (***))
  These seem to fail, but as we have an example solution above, no issues...

		   '(bzr2 (vector (make-pointSTR :x 1.0 :y 2.0)
		    	         (make-pointSTR :x 2.0 :y 2.0)
  		    	         (make-pointSTR :x 3.0 :y 2.0)) pointSTR)
		   '(bzr3
		     (vector (make-pointSTR :x 1.0 :y 2.0)
		      (make-pointSTR :x 2.0 :y 2.0)
		      (make-pointSTR :x 3.0 :y 2.0))
		     pointSTR)

		   '(bzr4 #((make-pointSTR :x 1.0 :y 2.0)
			    (make-pointSTR :x 2.0 :y 2.0)
			    (make-pointSTR :x 3.0 :y 2.0))
		     pointSTR)
|#


(data-frame-column-types df-2)

;;; works
(defparameter bzr-strand-1
  (make-strand 'bzr1
	       #((make-pointSTR :x 1.0 :y 2.0)
		 (make-pointSTR :x 2.0 :y 2.0)
		 (make-pointSTR :x 3.0 :y 2.0))))
(defparameter bzr-strand-2
  (make-strand 'bzr2
	       #((make-pointSTR :x 1.0 :y 2.0)
		 (make-pointSTR :x 2.0 :y 2.0)
		 (make-pointSTR :x 3.0 :y 2.0))
	       ;; pointSTR ; what to put here?
	       ))


(length (strand-data bzr-strand-1))
bzr-strand-1

(defparameter df-4
  (make-data-frame bzr-strand-1))

df-4
(pprint df-4)
(pprint-data-frame df-4)

(defparameter df-4a
  (make-data-frame bzr-strand-1
		   bzr-strand-1))

df-4a
(pprint df-4a)
(pprint-data-frame df-4a)

(setf (ref$ df-4a 'bzr1 2) 3)

(ignore-errors
  (setf (ref$ df-4a 'bzr 2) 3))

;; note that because of places (reference not copy), the 3rd
;; observation in both are set!  We need to make a copy in order to
;; have separate variables.




;;; but above has type T when we'd like type POINTSTR.
;;; so we try to specify, but it currently fails.
(defparameter my/strand (make-strand 'bzr
				     (vector (make-pointSTR :x 1.0 :y 2.0)
					     (make-pointSTR :x 2.0 :y 2.0)
					     (make-pointSTR :x 3.0 :y 2.0))
				     'pointSTR))

(typep my/strand 'STRAND)
(typep my/strand 'STRING)



(defparameter s1 (make-strand 'bzr #(1 2 3) 'fixnum))
s1

(every-strand)
(every-data-frame)

;;; Typing examples for the naive.

(typep #((make-pointSTR :x 1.0 :y 2.0)
	 (make-pointSTR :x 2.0 :y 2.0)
	 (make-pointSTR :x 3.0 :y 2.0))
       'vector) ;; => T

(typep #((make-pointSTR :x 1.0 :y 2.0)
	 (make-pointSTR :x 2.0 :y 2.0)
	 (make-pointSTR :x 3.0 :y 2.0))
       'list) ;; => NIL

(typep #((make-pointSTR :x 1.0 :y 2.0)
	 (make-pointSTR :x 2.0 :y 2.0)
	 (make-pointSTR :x 3.0 :y 2.0))
       '(vector pointSTR *))  ;; => T

(typep #((make-pointSTR :x 1.0 :y 2.0)
	 (make-pointSTR :x 2.0 :y 2.0)
	 (make-pointSTR :x 3.0 :y 2.0))
       '(vector * 3))  ;; => T

(typep #((make-pointSTR :x 1.0 :y 2.0)
	 (make-pointSTR :x 2.0 :y 2.0)
	 (make-pointSTR :x 3.0 :y 2.0))
       '(vector pointSTR 3))  ;; => T











(defparameter df-2
  (make-data-frame
   
   '(foo #(1 2 3)) 

   '(bar  ("a" "s" "d") string) 
#|
   ;;; fixme - need to make this generic-proof via auto-directory location
   (make-strand
    'bar3
    (concatenate 'vector
		 (nth 0
		      (fare-csv:read-csv-file "test-list.txt")))
    'string)
|#
   '(baz  (100 102 97) (integer 90 110))

   (make-strand 'bzr
		(vector (make-pointSTR :x 1.0 :y 2.0)
			(make-pointSTR :x 2.0 :y 2.0)
			(make-pointSTR :x 3.0 :y 2.0))
		'pointSTR)
   '(foo2 #(1 2 3) fixnum)))


#|
 (ref$ df-2 'bar3 0)
 (ref$ df-2 'bar3 1)
 (ref$ df-2 'bar3 2)
 (ref$ df-2 'bar3 3)
|#

(ref$ df-2 'baz 0)
(ref$ df-2 'baz 1)


(data-frame-column-names df-2)
(data-frame-column-types df-2)

;;; FIXME!!
(ignore-errors 
  (data-frame-as-lisp-array df-2))


(pprint-data-frame df-2)

;; Get vector of columns
(data-frame-columns df-2)
;; get second column -- type strand
(aref  (data-frame-columns df-2) 1)
;; get data from the strand, type vector
(strand-data (aref  (data-frame-columns df-2) 1))
;; get the desired element in the strand
(aref (strand-data (aref  (data-frame-columns df-2) 1)) 1)
(ref$ (aref (data-frame-columns df-2) 1) 1)


(ignore-errors 
  (ref$ (symbol (nth 1 (data-frame-column-names df-2))) 1)) ;;FIXME


;;; To get a row vector, from case 2

(case$ df-2 2)


(case$ df-2 0)

(case$ df-2 1)

(case$ df-2 0 1)
(case$ df-2 1 0 1)



;;; READING CSV files into RHO data structures.

(defparameter *rho-installation-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :rho)))
  "Value considered \"home\" for the installation.  Requires the use
  of ASDF to find out where we are.  But ASDF is useful (required?) no
  matter what.  This is used to dummy-proof the location of the
  examples by dynamically determining them.")



(macrolet ((rho-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *rho-installation-home-dir*) ,root-str)))

	   (rho-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (rho-dir ,root-str))))
  (rho-defdir *rho-example-dir* "example/"))




;;; playing with prevalence and serialization

;;; (ql:quickload :cl-prevalence :verbose T)
;;; http://common-lisp.net/project/cl-prevalence/


;;; (ql:quickload :de.setf.resource :verbose T)
;;; https://github.com/lisp/de.setf.resource

;;; (ql:quickload :wilbur :verbose T)
;;; https://github.com/lisp/de.setf.resource


(ql:quickload :datafly)




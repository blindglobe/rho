
(ql:quickload :rho :verbose T)

;; See rho-package.lisp and use RHO-USER as an example for how to use
;; RHO within another package.

(in-package :rho-user)

;; Simple data-frame

;; the basic API consists of creating, and element-wise getting and
;; setting.

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



(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (strand-element-type s))

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (strand-data s))

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (ref$ (strand-data s) 1))

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (type-of s ))

#|

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (ref$ s 1))

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
  (list (ref$ s 1) (strand-element-type s)))

(let ((s (make-strand 'test-strand (vector 0 1 2 3 4) '(integer 0 10))))
  (list (ref$ s 1) (strand-element-type s)))
|#


#|
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

(typep 3.4 'number)


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


;; (ignore-errors 
;;   (ref$ (symbol (nth 1 (data-frame-column-names df-2))) 1)) ;;FIXME


;;; To get a row vector, from case 2

(case$ df-2 2)


(case$ df-2 0)

(case$ df-2 1)

(case$ df-2 0 1)
(case$ df-2 1 0 1)



;;; READING CSV files into RHO data structures.

(ql:quickload :fare-csv)
(ql:quickload :listoflist)

(defparameter *rho-home-dir*
  (directory-namestring
   (truename (asdf:system-definition-pathname :rho)))
  "Value considered \"home\" for the installation.  Requires the use
  of ASDF to find out where we are.  But ASDF is useful (required?) no
  matter what.  This is used to dummy-proof the location of the
  examples by dynamically determining them.")


(macrolet ((rho-dir (root-str)
	     `(pathname (concatenate 'string
				     (namestring *rho-home-dir*) ,root-str)))

	   (rho-defdir (target-dir-var  root-str)
	     `(defvar ,target-dir-var (rho-dir ,root-str))))
  (rho-defdir *rho-example-dir* "example/"))


(defparameter data-file-to-strand
  (make-strand 'bar3
	       (concatenate 
		'vector
		(nth 0
		     (fare-csv:read-csv-file
		      (concatenate 'string
				   (namestring *rho-example-dir*)
				   "test-list.txt"))))
	       'string))

data-file-to-strand

;;; This doesn-t work, but the solution could be to embed it 
(pprint-data-frame data-file-to-strand)

;;;; FIXME: reading CSV file into DATA-FRAME

(defparameter csv-file-contents 
  (fare-csv:read-csv-file
   (concatenate 'string
		(namestring *rho-example-dir*)
		"test-df.csv")))

(defparameter csv-var-labels/names (nth 0 csv-file-contents))

(ql:quickload :listoflist)

;; csv-file-contents is in row-list-form
(defparameter csv-column-list-form (listoflist:transpose-listoflist csv-file-contents))




#|
(defparameter dataframe-from-csvfile
  (make-dataframe
   

   var1
   var2
   var3))



'bar3
   (concatenate 
    'vector
    (nth 0
	 (fare-csv:read-csv-file
	  (concatenate 'string
		       (namestring *rho-example-dir*)
		       "test-list.txt"))))
   'string))


|#

#|
(car (list 0 1 2 3 4)) ;-> 0    (varname)
(cdr (list 0 1 2 3 4)) ;-> rest (data)

(values (list 1 2 3))
(values (list (list 1) (list 2 3)))
(values '(1 2 3))
(values 1 2 3)

|#





;;; playing with prevalence and serialization

;;; (ql:quickload :cl-prevalence :verbose T)
;;; http://common-lisp.net/project/cl-prevalence/


;;; (ql:quickload :de.setf.resource :verbose T)
;;; https://github.com/lisp/de.setf.resource

;;; (ql:quickload :wilbur :verbose T)
;;; https://github.com/lisp/de.setf.resource


;;; (ql:quickload :datafly)

;;; Statistical classing for STRANDs
;;;
;;; The idea is to include a means for summarizing or describing based
;;; on the contents.  We have to have a means for singular values
;;; (rounding or contrast to a fixed point), and kinetics structures
;;; (time-series, longitudinal measurements) as well as "spatial"
;;; structures such as networks.  Additional specialized structures
;;; such as expression arrays, sequences, and matrix-like measures
;;; also need descriptives.  There are generic structure builders such
;;; as replicates which work by getting centrality (means, medians)
;;; and extremal (quantiles, min/max, standard deviation,
;;; inter-quartile range) measures for the replicated values.
;;;
;;; This is implemented by adding 2 summary functions, an element-wise
;;; and a collection-wise (column-wise) summary

(defun censored-type-p (x)
  (if (or (equal x "CENSORED")
	  (equal x "EXACT"))
      T
      NIL))

(deftype censored-type ()
  '(and string (satisfies censored-type-p)))

(defparameter test-cens "CENSORED")
(type-of test-cens)
(typep test-cens 'censored-type)

(defclass censored-number ()
  ((value :type number)
   (censored :type censored-type))
  (:documentation "holds a number and an indicator of censoring."))

;;; Element-wise summary functions


;;; -- Common lisp has a few built-ins, so we can use:
;;; identity, round, floor, ceiling, truncate
;;; -- for values which are intended to be integers,
;;; fround, ffloor, fceiling, ftruncate
(defun plus-5 (x) (+ x 5))
;; Rounding for complex numbers?
(defun round-complex-down (x) x)
(defun round-complex-up (x) x)
(defun round-complex (x) x)
(defun signed-distance-from-x (x fixed-point) (- x fixed-point)
(defun absolution-distance-from-x (x fixed-point) (abs (- x fixed-point)))



;;; Collection-wise 
(defun center (v type)
  (funcall type v)))
(defun mean-arithmetic (v)
  (alexandria:mean v))
(defun mean-geometric (v)
  v)
(defun mean-harmonic (v)
  v)
(defun median (v)
  (alexandria:median v))
(defun mode-discrete (v)
  v)
(defun mode-smooth (v smooth)
  (funcall smooth v))

(defun disperson (v type)
  (funcall type v))
(defun variance (v)
  (alexandria:variance v))
(defun standard-deviation-unbiased (v)
  (alexandria:variance v :biased nil))
(defun standard-deviation-biased (v)
  (alexandria:variance v :biased T))

(defun quartile (v q)
  (funcall q v))
(defun interquartile-range (v)
  v)


(defun kurtosis (v)
  v)


(defun moment-nth (v n)
  "for first 3 moments, should equal mean, variance, kurtosis."
  (list n v))

(defun density-function (v type &rest parameters)
  "Type could be a parametric distribution, in which case we return
  the function in the first parameter, and in the second and third
  parameters, the probability family and parameters for the member of
  the family which best fits the data.

  It could also be a infinite-parameter smooth, ie kernel or spline,
  or an empirical density function (point-mass with 1/n weights), in
  which case we return a function of 1 parameter (sequence oe single
  value) for computing values.

  If type that the user wants is not found, then it should be passed
  in within the parameters as a function (named or lambda) of V
  returning an evaluable function."

  (ecase type
    (empirical (funcall parameters v))
    (parametric (funcall parameters v))
    (user-defined (funcall type v parameters)))
  nil)


(defun distribution-function (v type &rest parameters)
  "Type could be a parametric distribution, in which case we return
  the function in the first parameter, and in the second and third
  parameters, the probability family and parameters for the member of
  the family which best fits the data.

  It could also be a infinite-parameter smooth, ie kernel or spline,
  or an empirical distribution function (step-function with 1/n
  jumps), in which case we return a function of 1 parameter (sequence
  oe single value) for computing values.

  If type that the user wants is not found, then it should be passed
  in within the parameters as a function (named or lambda) of V
  returning an evaluable function."

  (ecase type
    (empirical (funcall parameters v))
    (parametric (funcall parameters v))
    (user-defined (funcall type v parameters)))
  nil)
			    


;;; Primary summary computations

(defgeneric summarize-element (x list-of-summaries)
  (:documentation "use type information to provide possible summarizes
  which can be applied.")
  (:method ((x number) (list-of-summaries list))
   (list #'identity 
	  #'round #'round-down #'round-up 
		"contrast from X")
    (list )
    nil
    )
  (:method ((x censored-number) (list-of-summaries list))
    nil)
  (:method ((x string) (list-of-summaries list))
    nil))

(defgeneric summarize-collection (x )
  (:documentation "use type information to provide possible summarizes
  which can be applied, to sequences and superclasses.")
  (:method ((x strand))
    (ctypecase (strand-element-type x)
      (number nil)
      (censored-number nil)
      (null   nil)))
  (:method ((x sequence))
    nil))

(defgeneric summarize-df (x)
  (:documentation "nest the element summaries with the collection summaries.")
  ;; for each strand, summarize the collection across the element summarises.
  (:method ((x data-frame)) nil)
  (:method ((x array)) nil))


;;; Testing to build it all together.

(defparameter list-of-summary-functions
  (list #'identity #'plus-5))

(defparameter test-strand (make-strand 'bcaus #(1 2 3 4 5 6) 'fixnum))
(summarize-element test-strand :summary-with list-of-summary-functions)


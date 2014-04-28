;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Time-stamp: <2014-04-28 16:09:13 tony>
;;; Creation:   <2014-04-14 11:18:02 tony>
;;; File:       unittests.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2014--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Unit testing and examples for MarcoA's rho package (dataframes) 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


;;; To use this as a standalone file, quickload RHO (most likely from
;;; your local directory) and load the rest of the file.  The
;;; commented out section at the end is useful for developing new
;;; unittests as well as processing existing ones.

;; (ql:quickload :rho)

(in-package :rho-test)

;;; Suites: 
(defsuite rho ())
(defsuite rho-strand (rho))
(defsuite rho-df (rho))
(defsuite rho-ref$ (rho-strand rho-df))

;;; data and data structures (to become fixtures)

(defstruct pointSTR (x 0.0 :type float) (y 0.0 :type float))

(defclass pointCLOS ()
  ((x :type float :initarg :x :initform 0.1)
   (y :type float :initarg :y :initform 0.2))
  (:documentation "silly point class for illustration"))





(deffixture rho-strand (@body)
  (let ((strand-struct-without-type
	 (make-strand 'bzr1-1
		      (vector (make-pointSTR :x 1.0 :y 2.0)
			      (make-pointSTR :x 2.0 :y 2.0)
			      (make-pointSTR :x 3.0 :y 2.0))))
	(strand-struct-with-type
	 (make-strand 'bzr1-2
		      (vector (make-pointSTR :x 1.0 :y 2.0)
			      (make-pointSTR :x 2.0 :y 2.0)
			      (make-pointSTR :x 3.0 :y 2.0))
		      'pointSTR))
	(strand-clos-without-type
	 (make-strand 'bzr2-1
		      (vector (make-instance 'pointCLOS :x 1.0 :y 1.0)
			      (make-instance 'pointCLOS :x 2.0 :y 2.0)
			      (make-instance 'pointCLOS :x 3.0 :y 3.0))))
	(strand-clos-with-type
	 (make-strand 'bzr2-2
		      (vector (make-instance 'pointCLOS :x 1.0 :y 1.0)
			      (make-instance 'pointCLOS :x 2.0 :y 2.0)
			      (make-instance 'pointCLOS :x 3.0 :y 3.0))
		      'pointCLOS))
	(strand-string-without-type
	 (make-strand 'bzr3-1
		      (vector "one" "two" "three")))
	(strand-string-with-type
	 (make-strand 'bzr3-2
		      (vector "one" "two" "three")
		      'string))
	(strand-int-without-type
	 (make-strand 'bzr4-1
		      (vector 100 200 300)))
	(strand-int-with-type
	 (make-strand 'bzr4-2
		      (vector 100 200 300)
		      'fixnum))
	(strand-float-without-type
	 (make-strand 'bzr5-1
		      (vector 150.50d0 250.50d0 350.0d0)))
	(strand-float-with-type
	 (make-strand 'bzr5-1
		      (vector 150.50d0 250.50d0 350.0d0)
		      'float)))

    @body))


(deffixture rho-df (@body)
  (let ((df-1
	 (make-data-frame '(foo #(1 2 3)) 
			  '(bar ("a" "s" "d") string) 
			  '(baz (100 102 97) (integer 90 110))
			  (make-strand 'bzr
				       (vector (make-pointSTR :x 1.0 :y 2.0)
					       (make-pointSTR :x 2.0 :y 2.0)
					       (make-pointSTR :x 3.0 :y 2.0)))))
	(df-2
	 (make-data-frame '(foo #(1 2 3)) 
			  '(bar ("a" "s" "d") string) 
			  '(baz (100 102 97) (integer 90 110)))))
    @body))


;;; Tests

(deftest strands-user/def/type (rho-strand)
  (assert-true (= (length (strand-data strand-struct-without-type))
		  (length (strand-data strand-struct-with-type))
		  (length (strand-data strand-clos-with-type))
		  (length (strand-data strand-clos-with-type))
		  3)))


(deftest strands-basic (rho-strand)
    (assert-true (= (length (strand-data strand-string-without-type))
		    (length (strand-data strand-string-with-type))
		    (length (strand-data strand-int-without-type))
		    (length (strand-data strand-int-with-type))
		    (length (strand-data strand-float-without-type))
		    (length (strand-data strand-float-with-type))
		    3)))


	
(deftest indexing-df (rho-df)
  (assert-true (typep (ref$ df-1 2 1) (ref$ (data-frame-column-types df-1) 2))))


;;
;;	(assert-true (typep (ref$ df-1 2 2) (ref$ (data-frame-column-types df-1) 3)))
;;	(assert-true (typep (ref$ df-1 1 1) (ref$ (data-frame-column-types df-1) 1)))
;;    (assert-equal (data-frame-column-types df2) (list of proper types)



#|
	(df-2
	 (make-data-frame '(foo #(1 2 3)) 
			  '(foo2 #(1 2 3) fixnum) 
			  
			  '(bar ("a" "s" "d") string) 
			  '(baz (100 102 97) (integer 90 110))
			  
			  (make-strand 'bzr
				       (vector (make-pointSTR :x 1.0 :y 2.0)
					       (make-pointSTR :x 2.0 :y 2.0)
					       (make-pointSTR :x 3.0 :y 2.0))
				       'pointSTR)

;;	    '(bzr2 (vector (make-pointSTR :x 1.0 :y 2.0)
;;	    (make-pointSTR :x 2.0 :y 2.0)
;;	    (make-pointSTR :x 3.0 :y 2.0)) pointSTR)


;;  These seem to fail, but as we have an example solution above, no issues...
;;		   '(bzr4 #((make-pointSTR :x 1.0 :y 2.0)
;;			    (make-pointSTR :x 2.0 :y 2.0)
;;			    (make-pointSTR :x 3.0 :y 2.0))
;;		     pointSTR)


			  '(bzr3
			    (vector (make-pointSTR :x 1.0 :y 2.0)
			     (make-pointSTR :x 2.0 :y 2.0)
			     (make-pointSTR :x 3.0 :y 2.0))
			    pointSTR))

|#

(deftest ref$-vector (rho) 
  (let ((v (vector 0 1 2 3 4)))
    (loop
       for i from 0 to 4
       do (assert-equal (ref$ v i) i))))


(deftest ref$-strand (rho) 
  (let ((s (make-strand 'test-strand (vector 0 1 2 3 4) 'fixnum)))
    (loop
       for i from 0 to 4
       do (assert-equal (ref$ s i) i))
    (loop
       for i from 0 to 4
       do (assert-equal (ref$ 'test-strand i) i))))



(deftest ref$-dataframe (rho) 
  (let ((df (vector 0 1 2 3 4)))
    (loop
       for i from 0 to 4
       for j from 0 to 4
       collect (assert-equal (ref$ df i j) (+ i j)))))


#|
;; what is the best way to check DF indexing?
    (loop
       for i from 0 to 4
       for j from 0 to 4
       collect (list i j))

 (let ((my-list (list 100 200 )))
  (dotimes (i 4 my-list)
    (dotimes (j 4 my-list)
      (append my-list (list i j)))))
|#


;;; Running tests

#|


;;; We don't evaluate these during compile or load time!  Just useful
;;; for testing new and runnning old tests.

(setf clunit:*clunit-report-format* :default) 

;;; Main call for testing
(run-suite 'rho
	   :use-debugger NIL
	   :report-progress T)

(run-test 'strands-user/def/type
	  :use-debugger T
	  :report-progress T)

(run-test 'ref$-strand
	  :use-debugger T
	  :report-progress T)

(run-test 'dataframes
	  :use-debugger T
	  :report-progress T)


(run-suite 'rho :report-progress nil)
(rerun-failed-tests :use-debugger NIL)

|#

;;; End of File

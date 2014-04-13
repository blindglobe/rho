(ql:quickload :rho)

(in-package :rho-user)

(defparameter df (make-data-frame '(foo #(1 2 3)) 
                         '(bar ("a" "s" "d") string) 
                         '(baz (100 102 97) (integer 90 110)))) 

(pprint-data-frame df) 

(setf (ref$ df 'bar 2) 42) 

(data-frame-column-types df) 

(setf (ref$ df 'bar 2) "Works!") 

(pprint-data-frame df) 

(typep (ref$ df 2 1) (ref$ (data-frame-column-types df) 2)) 

df


;;; example of making a variable of a defstruct'd type.

(defstruct pointSTR (x 0.0 :type float) (y 0.0 :type float))
(defparameter p1 (make-pointSTR :x 2.2 :y 2.3))
p1

(defclass pointCLOS ()
  ((x :type float :initarg :x :initform 0.1)
   (y :type float :initarg :y :initform 0.1))
  (:documentation "silly point class for illustration"))

(defparameter p2 (make-instance 'pointCLOS))
p2
(list (slot-value p2 'x) (slot-value p2 'y))




(defparameter df-2a
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   (make-strand 'bzr
				(vector (make-pointSTR :x 1.0 :y 2.0)
					(make-pointSTR :x 2.0 :y 2.0)
					(make-pointSTR :x 3.0 :y 2.0)))))


df-2a
(pprint-data-frame df-2a)

(data-frame-column-types df-2a)








;; and we want 

(ref$ df-2a 2 1) ; => 97
(ref$ df-2a 2 2) ; => 97
(ref$ df-2a 3 2) ; => 97

(eval (ref$ df-2a 3 2)) ; => 97

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
#|
		   '(bzr2 (vector (make-pointSTR :x 1.0 :y 2.0)
		    	         (make-pointSTR :x 2.0 :y 2.0)
  		    	         (make-pointSTR :x 3.0 :y 2.0)) pointSTR)
|#
		   '(bzr3
		     (vector (make-pointSTR :x 1.0 :y 2.0)
		       (make-pointSTR :x 2.0 :y 2.0)
		       (make-pointSTR :x 3.0 :y 2.0))
		     pointSTR)

#|  These seem to fail, but as we have an example solution above, no issues...
		   '(bzr4 #((make-pointSTR :x 1.0 :y 2.0)
			    (make-pointSTR :x 2.0 :y 2.0)
			    (make-pointSTR :x 3.0 :y 2.0))
		     pointSTR)
|#
		   ))

(data-frame-column-types df-2)

;;; works
(defparameter bzr-strand-1
  (make-strand 'bzr
	       #((make-pointSTR :x 1.0 :y 2.0)
		 (make-pointSTR :x 2.0 :y 2.0)
		 (make-pointSTR :x 3.0 :y 2.0))))
(length (strand-data bzr-strand-1))

;;; but above has type T when we'd like type POINTSTR.
;;; so we try to specify, but it currently fails.
(make-strand 'bzr
	     (vector (make-pointSTR :x 1.0 :y 2.0)
		     (make-pointSTR :x 2.0 :y 2.0)
		     (make-pointSTR :x 3.0 :y 2.0))
	     'pointSTR)


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



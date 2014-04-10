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

(defstruct point (x 0.0 :type float) (y 0.0 :type float))

(defparameter p1 (make-point :x 2.2 :y 2.3))

p1

(defparameter df-2
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   '(bzr ((make-point :x 1.0 :y 2.0)
			  (make-point :x 2.0 :y 2.0)
			  (make-point :x 3.0 :y 2.0)
		     point))))


(make-strand 'bzr
	     #((make-point :x 1.0 :y 2.0)
	       (make-point :x 2.0 :y 2.0)
	       (make-point :x 3.0 :y 2.0))
	     'point)



(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       'vector)

(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       '(vector point *))

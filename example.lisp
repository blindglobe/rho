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

(defparameter df-2a
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   (make-strand 'bzr
				#((make-point :x 1.0 :y 2.0)
				  (make-point :x 2.0 :y 2.0)
				  (make-point :x 3.0 :y 2.0)))))

df-2a

(pprint-data-frame df-2a)

(data-frame-column-types df-2a)


(defparameter df-2
  (make-data-frame '(foo #(1 2 3)) 
		   '(bar ("a" "s" "d") string) 
		   '(baz (100 102 97) (integer 90 110))
		   '(bzr #((make-point :x 1.0 :y 2.0)
			  (make-point :x 2.0 :y 2.0)
			  (make-point :x 3.0 :y 2.0))
		     point)
		   '(bzr2 ((make-point :x 1.0 :y 2.0)
			   (make-point :x 2.0 :y 2.0)
			   (make-point :x 3.0 :y 2.0))
		     point)))

;;; works
(make-strand 'bzr
	     #((make-point :x 1.0 :y 2.0)
	       (make-point :x 2.0 :y 2.0)
	       (make-point :x 3.0 :y 2.0)))

;;; but above has type T when we'd like type POINT.
;;; so we try to specify, but it currently fails.
(make-strand 'bzr
	     #((make-point :x 1.0 :y 2.0)
	       (make-point :x 2.0 :y 2.0)
	       (make-point :x 3.0 :y 2.0))
	     'point)


(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       'vector) ;; => T

(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       'list) ;; => NIL

(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       '(vector point *))  ;; => T

(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       '(vector * 3))  ;; => T

(typep #((make-point :x 1.0 :y 2.0)
	 (make-point :x 2.0 :y 2.0)
	 (make-point :x 3.0 :y 2.0))
       '(vector point 3))  ;; => T



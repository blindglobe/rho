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


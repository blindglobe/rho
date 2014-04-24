;;;; -*- Mode: Lisp -*-

;;;; rho-package.lisp --

;;;; See the file COPYING for licencing and copyright information.

(defpackage "IT.UNIMIB.DISCO.MA.RHO" (:use "CL")
  (:nicknames "RHO")

  (:export
   "MAKE-DATA-FRAME"
   ;; Add other...
   )

  (:export
   "MAKE-STRAND"
   strand-data       ; exported for debugging only, I think.  Normally
		     ; ref$ should be the only means of accessing data
   ;; Add other...
   )

  (:export
   "DATA-FRAME-COLUMN-NAMES"
   "DATA-FRAME-COLUMN-TYPES"
   "DATA-FRAME-AS-LISP-ARRAY"
   )

  (:export
   "REF$"
   )

  (:export :pprint-data-frame))


(defpackage :rho-user
  (:documentation "package for experiments and use demonstration.")
  (:use :cl
	:rho))

(defpackage :rho-test
  (:documentation "package for experiments and use demonstration.")
  (:use :cl
	:clunit
	:rho))


;;;; end of file -- rho-package.lisp --

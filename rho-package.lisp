;;;; -*- Mode: Lisp -*-

;;;; rho-package.lisp --

;;;; See the file COPYING for licencing and copyright information.

(in-package :cl-user)

(defpackage "IT.UNIMIB.DISCO.MA.RHO"
  (:use "CL")
  (:nicknames "RHO")

  (:export

   make-data-frame

   strand
   make-strand
   strand-name
   strand-data       ; exported for debugging only, I think.  Normally
		     ; ref$ should be the only means of accessing data
   strand-element-type

   "DATA-FRAME-COLUMN-NAMES"
   "DATA-FRAME-COLUMN-TYPES"
   "DATA-FRAME-AS-LISP-ARRAY"
   pprint-data-frame

   ref$ case$

   data-frame-columns


   ;;  lengthv ;; maybe, maybe not

   every-strand
   every-data-frame

   ))


(defpackage :rho-user
  (:documentation "package for experiments and use demonstration.")
  (:use :cl
	:rho))

(defpackage :rho-test
  (:documentation "unittests are run in this package.")
  (:use :cl
	:clunit
	:rho))

  ;; TODO:(?) export from this package, a function to run unittests?



;;;; end of file -- rho-package.lisp --

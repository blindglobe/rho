;;;; -*- Mode: Lisp -*-

;;;; rho-package.lisp --

;;;; See the file COPYING for licencing and copyright information.

(in-package :cl-user)

(defpackage "IT.UNIMIB.DISCO.MA.RHO"
  (:use :cl :alexandria)
  (:nicknames "RHO")

  (:export

   data-frame
   make-data-frame


   strand
   make-strand
   strand-name
   strand-data       ; exported for debugging only, I think.  Normally
		     ; ref$ should be the only means of accessing data
   strand-element-type

   data-frame-column-names
   data-frame-column-types
   data-frame-as-lisp-array
   pprint-data-frame

   ref$ case$

   data-frame-columns

   every-strand
   every-data-frame

   ;;  lengthv ;; maybe, maybe not
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

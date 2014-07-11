;;;; -*- Mode: Lisp -*-

;;;; rho.asd --

;;;; See the file COPYING for licencing and copyright information.

(asdf:defsystem :rho
  :depends-on (:clunit :fare-csv)  ;; later is only for a test -- we should make it conditional here.
  :components ((:file "rho-package")
               (:file "data-frame" :depends-on ("rho-package"))
               (:file "unittests" :depends-on ("data-frame"))))

;;;; end of file -- rho.asd --

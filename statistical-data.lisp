;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Time-stamp: <2014-10-26 10:43:58 tony>
;;; Creation:   <2014-10-18 13:44:18 tony>
;;; File:       statistical-data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2014--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Template header file

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".


;; For types which have statistical properties, either naturally
;; (numbers) or imposed (complex types such as kinetics or locations
;; or similar), we want a means to properly summarize.  We can do the
;; summary by way of dispatch based on class, or generic functions, or similar. 

;; What we-d really like is some form of typed base dispatch, and we
;; can do something like that through generic functions, though with
;; potential performance costs.

;; STATISTICAL-SUMMARY operates on single objects, vectors and
;; strands, data-frames, and aggregated objects.  It should return a
;; SUMMARIZED object which can be used as input into modeling and
;; testing.

(defclass summarized ())

(defgeneric statistical-summary (object
				 list-of-std-actions-to-use
				 list-of-new-actions)
  (:documentation "Generic method for generating a SUMMARIZED, which
  can be used both as itself to describe the statistical properties of
  a collection of objects, or as the parameters in a modeling
  exercise, joint with the raw unprocessed data."))

;;; Examples:

;; The statistical summary of standard numbers should be the standard
;; summary (min, max, quartiles, mean).  Others could be added or
;; defined, and included or not included through the list of
;; parameters (list of names, or list of functions).

;; The idea is that we should be able to compute how to summarize the
;; vector of data, both the (complex) data values themselves, as well
;; as a collection of them.  The collection could be ordered or
;; unordered, depends on essential characteristics of the data
;; (ordering could be restricted in order to assure that it properly
;; pairs with another collection -- note that in the example of a
;; bootstrap for correlation, only under the assumption of observed
;; order being "proper" is the assumption that the original order is
;; the truth, is valid).



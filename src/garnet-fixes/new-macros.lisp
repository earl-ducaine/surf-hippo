;;; -*- mode: lisp; syntax: common-lisp; package: kr; base: 10 -*-

#|

====================================================================================================
			       the surf-hippo neuron simulator system
====================================================================================================

this code was written as part of the surf-hippo project, originally at the center for biological
information processing, department of brain and cognitive sciences, massachusetts institute of
technology, and currently at the neurophysiology of visual computation laboratory, cnrs.

permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. the surf-hippo
project makes no representations about the suitability of this software for any purpose. it is
provided "as is" without express or implied warranty.

if you are using this code or any part of surf-hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.

copyright (c) 1989 - 2003, lyle j. graham

|#

;; this file is modified from garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         the garnet user interface development environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this code was written as part of the garnet project at          ;;;
;;; carnegie mellon university, and has been placed in the public   ;;;
;;; domain.  if you are using this code or any part of garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; garnet-fixes source file: new-macros.lisp

(in-package :kr)

(defmacro fn-gv (object slot)
  `(the fixnum (gv ,object ,slot)))

;; (defmacro fn-gv (object slot)
;;   `(the fixnum (gv ,object ,slot)))

;; (defmacro sf-gv (object slot) `(the single-float (gv ,object ,slot)))
;; (defmacro sf-gv (object slot) `(the single-float (gv ,object ,slot)))

(defmacro fixnum-max (num1 num2)
  `(let ((arg1 ,num1)
	 (arg2 ,num2))
     (if (> (the fixnum arg1) (the fixnum arg2)) arg1 arg2)))

(defmacro fixnum-min (num1 num2)
  `(let ((arg1 ,num1)
	 (arg2 ,num2))
     (if (< (the fixnum arg1) (the fixnum arg2)) arg1 arg2)))

(defmacro fixnum-bound (min num max) `(fixnum-max ,min (fixnum-min ,max ,num)))

(defmacro s-values (schema &rest slot-syms)
  ;; (s-values foo a (b z) c) => (progn (s-value foo :a a) (s-value foo :b z) (s-value foo :c c))
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~a" (if (consp slot-sym) (car slot-sym) slot-sym)))
				    ,(if (consp slot-sym) (cadr slot-sym) slot-sym)))))

(defmacro s-values-s-flt (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~a" (if (consp slot-sym) (car slot-sym) slot-sym)))
			    (coerce ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 'single-float)))))

(defmacro s-values-s-flt-or-0 (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~a" (if (consp slot-sym) (car slot-sym) slot-sym)))
			    (coerce (or ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 0) 'single-float)))))

(defmacro s-values-s-flt-if-non-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(when ,(if (consp slot-sym) (cadr slot-sym) slot-sym)
			    (s-value ,schema ,(read-from-string (format nil ":~a" (if (consp slot-sym) (car slot-sym) slot-sym)))
			     (coerce ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 'single-float))))))

(defmacro s-values-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~a" slot-sym)) nil))))

(defmacro s-values-if-non-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(when ,(if (consp slot-sym) (cadr slot-sym) slot-sym)
			    (s-value ,schema ,(read-from-string (format nil ":~a" (if (consp slot-sym) (car slot-sym) slot-sym)))
			     ,(if (consp slot-sym) (cadr slot-sym) slot-sym))))))

(defmacro s-value-list (schemae slot value) `(mapcar #'(lambda (schema) (s-value schema ,slot ,value)) (coerce-to-list ,schemae)))

(export '(
;;	  fixnum-max
;;	  fixnum-min
;;	  fixnum-bound
;;	  fn-gv
	  sf-gv
;;	  S-VALUES-S-FLT-If-NON-NIL
	  S-VALUES-S-FLT
;;	  S-VALUES-NIL
;;	  s-values
;;	  S-VALUES-S-FLT-OR-0
;;	  s-values-IF-NON-NIL
	  s-value-list
	  ))

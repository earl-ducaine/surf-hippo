;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: KR; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GARNET-FIXES Source file: new-macros.lisp

(in-package "KR")

(defmacro fn-gv (object slot) `(the fixnum (gv ,object ,slot)))
(defmacro fn-gv (object slot) `(the fixnum (gv ,object ,slot)))
  
(defmacro sf-gv (object slot) `(the single-float (gv ,object ,slot)))
(defmacro sf-gv (object slot) `(the single-float (gv ,object ,slot)))

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
  ;; (s-values foo a (b z) c) => (PROGN (S-VALUE FOO :A A) (S-VALUE FOO :B Z) (S-VALUE FOO :C C))
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~A" (if (consp slot-sym) (car slot-sym) slot-sym)))
				    ,(if (consp slot-sym) (cadr slot-sym) slot-sym)))))

(defmacro s-values-s-flt (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~A" (if (consp slot-sym) (car slot-sym) slot-sym)))
			    (coerce ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 'single-float)))))

(defmacro s-values-s-flt-or-0 (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~A" (if (consp slot-sym) (car slot-sym) slot-sym)))
			    (coerce (or ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 0) 'single-float)))))

(defmacro s-values-s-flt-if-non-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(when ,(if (consp slot-sym) (cadr slot-sym) slot-sym)
			    (s-value ,schema ,(read-from-string (format nil ":~A" (if (consp slot-sym) (car slot-sym) slot-sym)))
			     (coerce ,(if (consp slot-sym) (cadr slot-sym) slot-sym) 'single-float))))))

(defmacro s-values-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(s-value ,schema ,(read-from-string (format nil ":~A" slot-sym)) nil))))

(defmacro s-values-if-non-nil (schema &rest slot-syms)
  `(progn ,@(loop for slot-sym in slot-syms
		  collect `(when ,(if (consp slot-sym) (cadr slot-sym) slot-sym)
			    (s-value ,schema ,(read-from-string (format nil ":~A" (if (consp slot-sym) (car slot-sym) slot-sym)))
			     ,(if (consp slot-sym) (cadr slot-sym) slot-sym))))))

(defmacro s-value-list (schemae slot value) `(mapcar #'(lambda (schema) (s-value schema ,slot ,value)) (coerce-to-list ,schemae)))

(export '(fixnum-max fixnum-min fixnum-bound fn-gv fn-gv sf-gv sf-gv
	  S-VALUES-S-FLT-If-NON-NIL
	  S-VALUES-S-FLT
	  S-VALUES-NIL
	  s-values
	  S-VALUES-S-FLT-OR-0
	  s-values-IF-NON-NIL
	  s-value-list
	  ))



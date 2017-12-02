;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

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


;;; PARAMETERS Source file: isource.lisp

;; Default loaded isource type

(in-package "SURF-HIPPO")

(defun plg (variable gain threshold)
  ;; Piece-wise linear gain function
  (let ((shifted-var (- variable threshold)))
    (if (< shifted-var 0) 0 (* gain shifted-var))))

(isource-type-def
 '(plg-voltage-driven
   (class . :driven)
   (activation-function . (lambda (src) (s-flt (plg (element-data-value-fast (isource-cell-element src) 'voltage) 0 1))))))

(isource-type-def
 `(light-driven
   (class . :driven)
  ;; (light-stimulus-function nil)
   (rf-function . (lambda (src) (let ((x (where-x (isource-cell-element src)))
				      (y (where-y (isource-cell-element src)))
				      (light-stimulus-function (element-parameter (isource-type src) 'light-stimulus-function)))
				  (if light-stimulus-function
				  (funcall light-stimulus-function x y (*input-time*))
				  0.0))))
   (activation-function . (lambda (src)
			    (s-flt
			     (funcall (isource-type-rf-function (isource-type src)) src))))))


(isource-type-def
 '(autonomous
   (class . :autonomous)))


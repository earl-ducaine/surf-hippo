;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: declare-2.lisp

;; ****************************************
;;;
;;; Various declarations which require that declare.lisp, create-models.lisp and models-2.lisp be loaded.
;;;
;; ****************************************


(in-package "SURF-HIPPO")

(deftype circuit-object-type () "The union of all circuit object types." `(or ,@*model-names*))

(defun circuit-object-type-p (thing) (typep thing 'circuit-object-type))

(macrolet ((frob ()
	     `(progn ,@(mapcar
			#'(lambda (plot-list-info)
			    `(defvars-w-value
			       (,(plot-list-info-names plot-list-info) nil)
			       (,(plot-list-info-enable-var plot-list-info) nil)
			       (,(plot-list-info-structures  plot-list-info) nil)))
			*plot-lists-info*))))
  (frob))


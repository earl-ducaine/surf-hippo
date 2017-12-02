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


;;; SYS Source file: models-2.lisp
					
(in-package "SURF-HIPPO")

;; ****************************************
;;
;; Various declarations which require that create-models.lisp and structures.lisp be loaded.
;;
;; ****************************************

;; Every abstracted entity that should have direct user access. For every symbol in this list there is a model and an instance hash table. 
(defvar *model-names* (mapcar 'model-name (models)))

;; *CIRCUIT-ELEMENT-TYPE-MODEL-NAMES* and *CIRCUIT-ELEMENT-MODEL-NAMES* include the structure types of all the circuit
;; elements and element types that normally are explicitly manipulated by a simulation script.

(defvar *circuit-element-type-model-names* (loop for model in (models) when (model-child-structure-type model) collect (model-name model)))
(defvar *circuit-element-model-names* (loop for model in (models) unless (model-child-structure-type model) collect (model-name model)))

(defun model-name-p (thing) (true-p (find thing *model-names*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This must be loaded after the element type accessor functions (e.g. for the CELL and CHANNEL structure :TYPE slot accessor
;; functions, CELL-TYPE and CHANNEL-TYPE, respectively) have been loaded.

#|
(defun element-collector (collected-model-type &optional (reference-elt nil reference-elt-supplied-p))
  (macrolet ((match-type-case-macro
		 ()
	       `(case collected-model-type
		 ,@(no-nils (mapcar #'(lambda (model)
					(let ((type-sym-type-accessor (read-from-string (format nil "~A-type" (model-name model)))))
					  (when (fboundp type-sym-type-accessor)
					    `(,(model-name model) (eq (,type-sym-type-accessor element) reference-element)))))
				    (models))))))
    (let* ((collected-model-type-model (get collected-model-type 'model))
	   (collected-model-type-parent (model-parent-structure-type collected-model-type-model))
	   (reference-element (element reference-elt))
	   (reference-element-is-parent-type-of-collected-model-type-p (eq collected-model-type-parent (type-of reference-element)))
	   (reference-element-cell-element (unless (cell-p reference-element) (element-cell-element-core reference-element))))
      (if reference-element-cell-element
	  (cell-element-elements reference-element-cell-element collected-model-type)
	  (let ((all-elements (all-model-instances collected-model-type)))
	    (if (cell-p reference-element)
		(loop for elt in all-elements when (eq reference-element (element-cell elt)) collect elt)
		(if reference-element-is-parent-type-of-collected-model-type-p
		    (loop for element in all-elements when (match-type-case-macro) collect element)
		    (unless reference-elt-supplied-p all-elements))))))))
|#

(defun element-collector (collected-model-type &optional (reference-elt nil reference-elt-supplied-p))
  (macrolet ((match-type-case-macro
		 ()
	       `(case collected-model-type
		 ,@(no-nils (mapcar #'(lambda (model)
					(let ((type-sym-type-accessor (read-from-string (format nil "~A-type" (model-name model)))))
					  (when (fboundp type-sym-type-accessor)
					    `(,(model-name model) (eq (,type-sym-type-accessor element) reference-element)))))
				    (models))))))
    (let* ((collected-model-type-model (get collected-model-type 'model))
	   (collected-model-type-parent (model-parent-structure-type collected-model-type-model))
	   (reference-elements (elements reference-elt))
	   (all-elements (all-model-instances collected-model-type)))
      (if reference-elements
	  (loop for reference-element in reference-elements nconc
		(let* ((reference-element-is-parent-type-of-collected-model-type-p (eq collected-model-type-parent (type-of reference-element)))
		       (reference-element-cell-element (unless (cell-p reference-element) (element-cell-element-core reference-element))))
		  (if reference-element-cell-element
		      (cell-element-elements reference-element-cell-element collected-model-type)
		      (if (cell-p reference-element)
			  (loop for elt in all-elements when (eq reference-element (element-cell elt)) collect elt)
			  (when reference-element-is-parent-type-of-collected-model-type-p
			    (loop for element in all-elements when (match-type-case-macro) collect element))))))
	  (unless reference-elt-supplied-p all-elements)))))

			  




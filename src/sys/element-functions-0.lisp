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


;;; SYS Source file: element-functions-0.lisp

(in-package "SURF-HIPPO")

(defun get-model (thing)
  (if (model-p thing)
    thing
    (get (if (symbolp thing)
	   thing
	   (type-of thing))
	 'model)))

(defun model-slot-p (model-name slot) (setfable-p (read-from-string (format nil "~A-~A" model-name slot))))

(defun get-model-hash-table (thing)
  (let ((model (get-model thing)))
    (when model (model-hash-table model))))

(defun get-hash-table (thing) (get-model-hash-table thing))

(defun all-model-instances (model-type) (hash-table-list (get-model-hash-table model-type)))

(defun all-cell-elements ()
  ;; All somas and segments.
  (concatenate 'list
	       (all-model-instances 'segment)
	       (all-model-instances 'soma)))

(defun name-element (name &optional model-type)
  ;; Return an element with name NAME, constrained to MODEL-TYPE, if included. Priority is implied by ordering in the list of
  ;; models returned by MODELS.
  (when name
    (let ((model-type (if (eq model-type 'electrode) 'segment model-type)) elt)
      (loop for model in (models)
	    do (setq elt (gethash name (model-hash-table model)))
	    when (and elt (or (not model-type) (equal model-type (type-of elt)))) do (return elt)))))

(defun element-core (elt-reference &optional model-type fast)
  (if fast 
    elt-reference
    (if (and (circuit-object-type-p elt-reference) (or (not model-type) (eq (type-of elt-reference) (if (eq model-type 'electrode) 'segment model-type))))
      elt-reference
      (cond ((eq elt-reference 'cell-element) (all-cell-elements))
	    ((model-name-p elt-reference)
	     (when (or (not model-type) (eq elt-reference model-type)) (hash-table-list (get-model-hash-table elt-reference))))
	    (t (name-element elt-reference model-type))))))

(defun element (elt-reference &optional model-type fast)
  "Return either the single structure object \(as an atom\) or objects \(as a list\) that are associated, if any, with ELT-REFERENCE. If none,
then ELEMENT returns NIL. ELT-REFERENCE is either a structure object, a structure name, or a (mixed) list of same. In the case of a name
\(which may be either a string, a symbol, or an integer\), since two or more structure objects of different model-types may have the same name
the search priority of structure model-types is given by *MODEL-NAMES*."
  (typecase elt-reference
    (cons (flatten-no-nils-list (loop for elt-reference-atom in (flatten-list elt-reference) collect (element-core elt-reference-atom model-type fast))))
    ;; Have this explicit to avoid consing in most cases.
    (t (element-core elt-reference model-type fast))))

(defun elements (elt-reference &optional model-type fast) (coerce-to-list (element elt-reference model-type fast)))

(defun element-hash-table (element &optional model-type)
  (or (get-model-hash-table element) (get-model-hash-table (type-of (element element model-type)))))
  
(defun type-name-format-input (type-name)
  ;; Provides the value for a FORMAT ~A directive, that includes an explicit quote when TYPE-NAME is a symbol.
  (typecase type-name
    (string (format nil "~s" type-name))
    (symbol (format nil "`~A" type-name))
    (number type-name)))

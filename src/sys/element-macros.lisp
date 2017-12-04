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


;;; SYS Source file: element-macros.lisp

(in-package "SURF-HIPPO")

;; Must be loaded after structures.lisp, and create-models.lisp (which defines *MODEL-NAME*).

(defmacro element-wrapper ((element initial-type &optional prompt ignore-supplied-element create-it) &body forms)
  ;; This macro supplies a list of elements to be processed by FORMS, which should refer to the varible ELT and
  ;; INTERNAL-TYPE. ELEMENT can be either an atom or a list, composed of object pointers and/or object names. If INITIAL-TYPE is
  ;; supplied, then the elements given to FORMS only consist of those associated with the original ELEMENT arg that are of type
  ;; INITIAL-TYPE. If ELEMENT is NIL and PROMPT is a string, the a menu which can access all loaded elements is generated, included
  ;; the PROMPT string.
  (declare (ignore prompt))
  `(let (internal-type)
     (flatten-if-list-greater-than-1
      (loop for element in (if ,element (coerce-to-list ,element) (when (and (not ,ignore-supplied-element) ,initial-type) (namelist-of-all-things ,initial-type)))
	    nconc (loop for elt in (coerce-to-list (if ,create-it (create-element element ,initial-type) (element element ,initial-type)))
			do (setq internal-type (if (electrode-p elt t) 'electrode (type-of elt)))
			collect (progn . ,forms))
	    into out
	    finally (return (atomize-list (no-nils out)))))))

(defvar *element-slot-t-typecase-error-p* t) ;; If T then ELEMENT-SLOT-CORE will signal an error if the element does not have the
					     ;; indicated structure slot. Otherwise, ELEMENT-SLOT-CORE will return NIL.


(defmacro element-slot-core (element slot-sym &optional (value nil value-supplied-p) update)
  ;; Note that the only way we are able to do this is because the
  ;; structure types stored in *MODEL-NAMES* are known at compile
  ;; time.
  (let* ((elt (gensym))
	 (enable-set-value (gensym))
	 (new-value (gensym))
	 (returned-value (gensym))
	 (typecase-clauses
	  (no-nils
	   (append
	    (mapcar
	     #'(lambda (object-type-symbol)
		 (let* ((accessor-function-string
			 (format nil "~A-~A" object-type-symbol slot-sym))
			(accessor-function
			 (read-from-string accessor-function-string))
			(full-slot-accesssor `(,accessor-function ,elt)))
		   ;; (print 'hello ) (print accessor-function-string)
		   ;; (print 'goodby)
		   (when (setfable-p accessor-function)
		     `(,object-type-symbol
		       (if ,enable-set-value
			   (setf ,full-slot-accesssor ,new-value)
			   ,full-slot-accesssor)))))
	     *model-names*)
	    `((t
	       (when (and *element-slot-t-typecase-error-p* ,elt)
		 (sim-error
		  (format
		   nil
		   (str "ELEMENT-SLOT-CORE macro error: ~A ~A "
			"does not have a ~s structure slot")
		   (type-of ,elt) ,elt ,slot-sym)))))))))
    (when typecase-clauses
      `(let* ,(no-nils (list `(,elt ,element)
			     `(,enable-set-value ,value-supplied-p)
			     `(,new-value ,value)
			     `(,returned-value (typecase
						   ,elt
						 ,@typecase-clauses))))
	 ; To avoid "Note: Variable NEW-VALUE defined but never used."
	 ; etc when there are
	 (null ,new-value) (null ,enable-set-value)
	 ;; no non-T typecase-clauses.
	 (when ,update (update-element ,elt))
	 ,returned-value))))

(defmacro element-slot (element slot-sym &optional (value nil value-supplied-p) update)
  ;; Note that the only way we are able to do this is because the
  ;; structure types stored in *MODEL-NAMES* are known at compile
  ;; time.
  (if value-supplied-p
    `(element-slot-core (element-core ,element) ,slot-sym ,value ,update)
    `(element-slot-core (element-core ,element) ,slot-sym)))

(defmacro element-slot-value-supplied-p (element slot-sym value value-supplied-p update)
  `(if ,value-supplied-p
    (element-slot-core (element-core ,element) ,slot-sym ,value ,update)
    (element-slot-core (element-core ,element) ,slot-sym)))

(defmacro element-slot-fast (element slot-sym &optional (value nil value-supplied-p) update)
  (if value-supplied-p
    `(element-slot-core ,element ,slot-sym ,value ,update)
    `(element-slot-core ,element ,slot-sym)))

(defun element-slot-function (element slot-sym &optional (value nil value-supplied-p) update)
  (if value-supplied-p
    (element-slot-function-internal (element element) slot-sym value update)
    (element-slot-function-internal (element element) slot-sym)))

(defun element-slot-function-internal (element slot-sym &optional (value nil value-supplied-p) update)
  (let ((accessor-function (read-from-string (format nil "~A-~A" (type-of element) slot-sym)))
	returned-value)
    (when (setfable-p accessor-function)
      (setq returned-value
	    (if value-supplied-p
	      (eval `(setf (,accessor-function ,element) (quote ,value)))
	      (cond ((macro-function accessor-function) (eval (macroexpand (list accessor-function element))))
		    ((fboundp accessor-function) (funcall accessor-function element)))))
      (when update (update-element element))
      returned-value)))

(defun element-slot-or-parameter-function (element key-sym &optional (value nil value-supplied-p) update)
  (or (if value-supplied-p (element-slot-function element key-sym value update) (element-slot-function element key-sym))
      (if value-supplied-p (element-parameter element key-sym value update) (element-parameter element key-sym))))

(defun element-parameter-or-slot-function (element key-sym &optional (value nil value-supplied-p) update)
  (or (if value-supplied-p (element-parameter element key-sym value update) (element-parameter element key-sym))
      (if value-supplied-p (element-slot-function element key-sym value update) (element-slot-function element key-sym))))

#|
(defmacro element-TYPE-DO ((elt elt-type) &rest body)
  (let ((element-types (gensym)))
    `(let ((,element-types (coerce-to-list ,elt-type)))
       (dolist (type ,element-types)
	 (if (element-slot-p type :first-element)
	   (do ((,elt ,(macroexpand `(element-slot type :first-element))
		      ,(macroexpand `(element-slot ,elt :next-element))))
	       ((null ,elt))
	     ,@body))))))

(defmacro element-TYPE-DO ((elt elt-type) &rest body)
  `(dolist (type (coerce-to-list ,elt-type))
    (if (element-slot-p type :first-element)
	(do ((,elt ,(macroexpand `(element-slot type :first-element))
		   ,(macroexpand `(element-slot ,elt :next-element))))
	    ((null ,elt))
	  ,@body))))
|#

(defmacro element-TYPE-DO ((elt elt-type) &rest body)
  `(dolist (type (coerce-to-list (element-type ,elt-type)))
    (if (element-slot-p type :first-element)
	(do ((,elt ,(macroexpand `(element-slot type :first-element))
		   ,(macroexpand `(element-slot ,elt :next-element))))
	    ((null ,elt))
	  ,@body))))

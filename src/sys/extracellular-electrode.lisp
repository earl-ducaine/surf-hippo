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


;;; SYS Source file: extracellular-electrode.lisp

;;;
;;; All the circuit element structure definitions, with some slot macros.
;;;

(in-package "SURF-HIPPO")

(defun EXTRACELLULAR-ELECTRODE-FIELD-POTENTIAL-VALUE (electrode)
  (let ((electrode (element electrode 'extracellular-electrode)))
    (when electrode (eval-extracellular-electrode electrode))))

(defun save-extracellular-electrode-data ()
  (loop for name in *plot-field-potentials* do
	(let ((electrode (element name 'extracellular-electrode)))
	  (push-element-parameter electrode :field-potential (eval-extracellular-electrode electrode)))))

(defun update-extracellular-electrode-output-data-flags ()
  (loop for name in *plot-field-potentials* do
	(element-parameter (element name 'extracellular-electrode) 'plot-field-potential t)))

(defun clear-extracellular-electrode-output-data-flags (extracellular-electrode) (element-parameter extracellular-electrode 'plot-field-potential))
    
(defun clear-extracellular-electrode-output-data (extracellular-electrode) (element-parameter extracellular-electrode :field-potential))

(defun print-extracellular-electrode (extracellular-electrode)
  (format t "~A located at ~A~%"
	  (MASSAGE-ELEMENT-PLOT-LABEL extracellular-electrode nil "Extracellular electrode")
	  (extracellular-electrode-absolute-location extracellular-electrode)))  

(defun document-extracellular-electrode (extracellular-electrode) (declare (ignore extracellular-electrode)))

(defun edit-extracellular-electrode (extracellular-electrode)
  (multiple-value-bind (dummy1 dummy2 dummy3)
      (values-list (extracellular-electrode-absolute-location extracellular-electrode))
    (choose-variable-values
     '((dummy1 "X:" :float) (dummy2 "Y:" :float) (dummy3 "Z:" :float))
     :label (format nil "Edit location for extracellular electrode ~A" (element-name extracellular-electrode)))
    (setf (extracellular-electrode-absolute-location extracellular-electrode) (list dummy1 dummy2 dummy3))
    (setup-extracellular-electrode extracellular-electrode)
    extracellular-electrode))
  
(defun create-extracellular-electrode (name x y z)
  (let ((electrode (element name 'extracellular-electrode)))
    (unless electrode
      (setq electrode (make-extracellular-electrode :name name))
      (setf (EXTRACELLULAR-ELECTRODE-HASH-TABLE name) electrode))
    (setf (extracellular-electrode-absolute-location electrode) (s-flt-list (list x y z)))
    (setup-extracellular-electrode electrode)
    electrode))

(defun eval-extracellular-electrode (extracellular-electrode)
  (loop for cell-element-distance in
	(get-element-parameter-fast 'cell-elements-and-distance (extracellular-electrode-parameters extracellular-electrode))
	sum (/ (the df (node-current (element-node (car cell-element-distance))))
	       (the sf (cadr cell-element-distance)))
	into weighted-current double-float
	finally (return (/ (* *r-extracellular* weighted-current) (* 4 pi-single)))))

(defun element-membrane-current (element)
  (let ((element (coerce-to-list (element-cell-element element))))
    (loop for node in (element-node element)
	  sum (node-current node) into current double-float
	  finally (return current))))

(defun setup-all-extracellular-electrodes ()
  (loop for extracellular-electrode being the hash-value of (EXTRACELLULAR-ELECTRODE-HASH-TABLE) 
	do (setup-extracellular-electrode extracellular-electrode)))

(defun setup-extracellular-electrode (extracellular-electrode)
  (setf (extracellular-electrode-absolute-location extracellular-electrode) (s-flt-list (extracellular-electrode-absolute-location extracellular-electrode)))
  (element-parameter
   extracellular-electrode
   'cell-elements-and-distance
   (loop for cell-element in (cell-elements) collect (list (element-name cell-element) (as-the-crow-flies cell-element extracellular-electrode))))
  nil)


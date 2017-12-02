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


;;; SYS Source file: buffer.lisp
;
; The model to integrate ion buffers.
;


(in-package "SURF-HIPPO")


(defun create-buffer-type (type-symbol &optional actual-type-symbol update-parameters)
  (let* ((buffer-type (unless actual-type-symbol
			(if (buffer-type-p type-symbol)
			  type-symbol
			  (BUFFER-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (buffer-type-p type-symbol) (buffer-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'buffer-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and buffer-type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about conc buffer type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless buffer-type
  	(setq buffer-type (if parent-type-symbol
			    (create-BUFFER-TYPE parent-type-symbol type-symbol update-parameters)
			    (make-BUFFER-TYPE :name type-symbol))))
      (when (and update-parameters parent-type-symbol)
	(update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) buffer-type))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters buffer-type))    
      (when parent-type-symbol (push-element-parameter buffer-type 'parent-types parent-type-symbol))
      (setf (buffer-type-class buffer-type) (get-a-value 'class original-parameters))
      (case (buffer-type-class buffer-type)
	(:mm)
	(:FIRST-ORDER-TAU-V)
	(:first-order))
      (cond-every
       ((or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))
	(setf (buffer-type-q10 buffer-type) (s-flt (cdr (or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))))))
       ((assoc 'reference-temp original-parameters)
	(setf (buffer-type-reference-temp buffer-type)
	      (coerce (cdr (assoc 'reference-temp original-parameters)) 'single-float))))
      (unless (member buffer-type *make-needed-v-buffer-arrays*) (push buffer-type *make-needed-v-buffer-arrays*))
      (setf (BUFFER-TYPE-HASH-TABLE type-symbol) buffer-type))
    (setq *buffer-type* buffer-type)
    buffer-type))

(defun get-buffer-simple-name ()
  (loop for candidate from (max 1 *buffer-simple-name-counter*)
	until (not (buffer-hash-table candidate))
	finally (return (setf *buffer-simple-name-counter* candidate))))

(defun rename-buffers-simple (&optional (buffers (buffers)))
  "Rename BUFFERS [default all buffers in circuit] with simple integer names."
  (loop for buffer in (coerce-to-list buffers) do (set-element-name buffer (get-buffer-simple-name) 'buffer)))

(defun total-conc-full-eq (free-conc buffer-total-conc kd)
  (* free-conc (+ 1 (/ buffer-total-conc (+ kd free-conc)))))

(defun total-conc-beta (free-conc buffer-total-conc kd)
  (* free-conc (* free-conc (+ 1 (/ buffer-total-conc kd)))))

#|
(let ((buffer-total-conc 100)
      (label-list '())
      (data-list '()))
  (loop for kd in '(0.01 0.1 1.0)
	do
	(push (loop for free-conc from 0.01 to .5 by 0.01 collect (total-conc-beta free-conc buffer-total-conc kd))
	      data-list)
	(push (format nil "kd: ~A (beta)" kd) label-list)
	(push (loop for free-conc from 0.01 to .5 by 0.01 collect (total-conc-full-eq free-conc buffer-total-conc kd))
	      data-list)
	(push (format nil "kd: ~A (full)" kd) label-list)
	finally (plot-timed-data
		 data-list
		 label-list
		 (loop for free-conc from 0.01 to .5 by 0.01 collect free-conc))))

|#

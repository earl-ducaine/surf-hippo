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

;;; SYS Source file: sparse-data.lisp

(IN-PACKAGE "SURF")

(defun element-sparse-data-keys (element &optional data-type)
  (let* ((element (element element))
	 (model (type-symbol-model (type-of element)))
	 (data-type (or data-type (default-data-type model))))
    (values (model-output-sparse-data-key model data-type) (model-output-ordered-sparse-data-key model data-type))))

(defun element-sparse-data-key (element &optional data-type)
  (let ((element (element element))
	(data-type (or data-type (default-data-type element)))
	(model (element-model element)))
    (model-output-sparse-data-key model data-type)))

(defun element-ordered-sparse-data-key (element &optional data-type)
  (let ((element (element element))
	(data-type (or data-type (default-data-type element)))
	(model (element-model element)))
    (model-output-ordered-sparse-data-key model data-type)))



(defun update-sparse-data (&optional (elements :cell-elements) (data-type 'voltage))
  (push *real-time* *reverse-sparse-data-times*)
  (case elements
    (:cell-elements
     (loop for elt being the hash-value of (segment-hash-table) do (store-element-sparse-data elt data-type))
     (loop for elt being the hash-value of (soma-hash-table) do (store-element-sparse-data elt data-type)))
    (t (loop for elt in (elements elements) do (store-element-sparse-data elt data-type))))
  nil)

(defun store-element-sparse-data (elt &optional data-type values)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((params (element-parameters elt)))
    (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
	(element-sparse-data-keys elt data-type)
      ;; If pushing new data, any ordered data is no longer valid.
      (set-element-parameter-fast elt ordered-sparse-data-key nil params)
      (if values
	  (let ((values-sf values))
	    (set-element-parameter-fast elt sparse-data-key (reverse values-sf) params)
	    (set-element-parameter-fast elt ordered-sparse-data-key values-sf params))
	   (push-element-parameter elt sparse-data-key (the sf (element-current-value-internal elt data-type)) params)
	  )))
  nil)

(defun store-element-sparse-data-core (elt sparse-data-key ordered-sparse-data-key params value)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  ;; If pushing new data, any ordered data is no longer valid.
  (set-element-parameter-fast elt ordered-sparse-data-key nil params)
  (push-element-parameter elt sparse-data-key
			  (the sf value)
			  params)

  nil)

(defun update-sparse-data-fast-core (model-symbol)
  (let* ((model (type-symbol-model model-symbol))
	 (data-type (default-data-type model))
	 (sparse-data-key (model-output-sparse-data-key model data-type))
	 (ordered-sparse-data-key (model-output-ordered-sparse-data-key model data-type)))
    (loop for elt being the hash-value of (model-hash-table model) do
	  (let ((params (element-slot elt :parameters))
		(value (element-current-value-internal elt data-type)))
	    (store-element-sparse-data-core  elt sparse-data-key ordered-sparse-data-key params value)))))
    
(defun update-sparse-data-fast ()
  (push *real-time* *reverse-sparse-data-times*)
  (update-sparse-data-fast-core 'segment)
  (update-sparse-data-fast-core 'soma)
  nil)

(defun clear-sparse-data (&optional (elements (cell-elements)) (data-type 'voltage)) (clear-element-sparse-data elements data-type))

(defun clear-element-sparse-data (element &optional data-type) (access-element-sparse-data element data-type :clear))

(defun length-sparse-data (&optional (elements (cell-elements)) (data-type 'voltage)) (length-element-sparse-data elements data-type))

(defun length-element-sparse-data (element &optional data-type) (access-element-sparse-data element data-type :length))

(defun access-element-sparse-data (element &optional data-type (action :clear))
  (loop for elt in (coerce-to-list element) sum
	(let* ((data-type (or data-type (default-data-type elt)))
	       (model (element-model elt))
	       (key (model-output-data-info model data-type 'sparse-data-param-key))
	       (ordered-sparse-data-key (model-output-data-info model data-type 'ordered-sparse-data-param-key)))
	  (when key
	    (case action
	      (:clear
	       (element-parameter elt key nil)
	       (element-parameter elt ordered-sparse-data-key nil)
	       0)
	      (:length (+ (length (element-parameter elt key))
			  (length (element-parameter elt ordered-sparse-data-key)))))))
	into out
	finally (return (case action
			  (:length out)))))

(defun d-flt-sparse-data (elements)
  (loop for elt in elements do
	(element-parameter elt :SPARSE-VOLTAGE-DATA (mapcar #'(lambda (val) (coerce val 'double-float)) (element-parameter elt :SPARSE-VOLTAGE-DATA))))
  nil)

(defun integrate-sparse-data (element &optional data-type start stop (offset 0.0))
  ;; If START and STOP not supplied, integrate over all times in (CURRENT-SPARSE-DATA-TIMES).
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((offset (s-flt offset)))
    (when (current-sparse-data-times)
      (time-integral-float (element-sparse-data element data-type) (current-sparse-data-times) offset start stop))))

(defun retrieve-sparse-data (element target-time &optional data-type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (current-sparse-data-times)
    (let* ((target-time (s-flt target-time))
	   (current-sparse-data-times (current-sparse-data-times))
	   (last-current-sparse-data-time (car (last current-sparse-data-times))))
      (loop with last-data = nil
	    with last-time = nil
	    for time single-float in current-sparse-data-times
	    for data in (element-sparse-data element data-type)
	    when (< time target-time) do (setq last-time time last-data data)
	    else when (> time target-time)
	    do (return (values (interpolate-data data time (or last-data data) (or last-time target-time) target-time)
			       target-time))
	    when (or (= time last-current-sparse-data-time)
		     (= time target-time))
	    do (return (values data time))))))

(proclaim '(inline get-ordered-element-sparse-data))
(defun get-ordered-element-sparse-data (element data-key ordered-data-key)
  (let* ((element (element element))
	 (params (typecase element
		   (segment (segment-parameters element))
		   (soma (soma-parameters element)))))
    (or (get-a-value ordered-data-key params)
	(set-element-parameter-fast element ordered-data-key (reverse (get-a-value data-key params)) params))))

(defun read-element-sparse-data (&key (data-type 'voltage) data-filename)
  "Read sparse data of DATA-TYPE from file DATA-FILENAME."
  (with-open-stream (stream (open data-filename :direction :input))
    (let ((data (read stream)))
      ; (format t "read ~A ~%car ~A~%" data (car data))
      (loop for elt-values in data do (store-element-sparse-data (car elt-values) data-type (cadr elt-values)))
      (set-sparse-data-times (cadr (read stream))))))

(defun set-sparse-data-times (times)
  (setq *sparse-data-times* times
	*reverse-sparse-data-times* (reverse times)))

(defun write-element-sparse-data (&key (elements (cell-elements)) (data-type 'voltage) data-filename)
  "Write sparse data of DATA-TYPE associated with ELEMENTS to file DATA-FILENAME."
  (write-lisp-header data-filename)
  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
    (format stream "(~%")
    (loop for element in (coerce-to-list (element elements)) do
	  (format stream "  (~s~%" (element-name element))
	  (format-list (float-list (element-sparse-data element)) 10 stream 6 6 " " t t)
	  (format stream "  )~%"))
        (format stream ")~%~%~%")
    ;; Now write out the time list.
    (format stream "(current-sparse-data-times~%")
    (format-list (current-sparse-data-times) 10 stream 6 6 " " t t)
    (format stream "  )~%")
    (format t "File ~a written~%" data-filename)))

(defun plot-element-sparse-data (elements &key (data-type 'voltage) (y-label "mV"))
  "Plot sparse data of DATA-TYPE associated with ELEMENTS."
  (let ((elements (coerce-to-list (element elements))))
    (plot-timed-data
     (mapcar 'element-sparse-data elements)
     (element-name elements)
     (current-sparse-data-times)
     :x-min 0
     :x-label "ms"
     :y-label y-label
     :title (format nil "~A: Sparse ~A data" *simulation-name* data-type))))
   
(defun loaded-sparse-data-p (&optional (elements (cell-elements)) data-type)
  (loop for elt in elements thereis (true-p (element-sparse-data elt data-type))))

(defun element-sparse-data (element &optional data-type)
  "Return a list of sparse data of DATA-TYPE for ELEMENT. If ELEMENT is a cell, then refer to that cell's soma."
  (let ((element (element element)))
    (when (cell-p element) (setq element (cell-soma element)))
    (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
	(element-sparse-data-keys element data-type)
      (get-ordered-element-sparse-data element sparse-data-key ordered-sparse-data-key))))

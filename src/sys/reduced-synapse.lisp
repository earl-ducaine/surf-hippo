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


;;; SYS Source file: reduced-synapse.lisp
;
; functions specific to the reduced synapse model
;


;; in development....


(in-package "SURF-HIPPO")

;; ************************************************
;; ****** SYNAPSE REDUCED MODEL FUNCTIONS *********
;; ************************************************


;; The reduced model parameters are stored in an array referenced in :SYNAPSE-PARAMETERS by
;; `REDUCED-MODEL-PARAMETERS.

(defmacro synapse-gn1 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 0))
(defmacro synapse-gn2 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 1))
(defmacro synapse-d1 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 2))
(defmacro synapse-gd2 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 3))
(defmacro synapse-in1 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 4))
(defmacro synapse-id2 (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 5))
(defmacro synapse-target-v-leak (syn)
  `(aref (the (simple-array single-float *) (get-synapse-param ,syn `reduced-model-parameters)) 6))

(defmacro aref-synapse-gn1 (array)
  `(aref (the (simple-array single-float *) ,array) 0))
(defmacro aref-synapse-gn2 (array)
  `(aref (the (simple-array single-float *) ,array) 1))
(defmacro aref-synapse-d1 (array)
  `(aref (the (simple-array single-float *) ,array) 2))
(defmacro aref-synapse-gd2 (array)
  `(aref (the (simple-array single-float *) ,array) 3))
(defmacro aref-synapse-in1 (array)
  `(aref (the (simple-array single-float *) ,array) 4))
(defmacro aref-synapse-id2 (array)
  `(aref (the (simple-array single-float *) ,array) 5))
(defmacro aref-synapse-target-v-leak (array)
  `(aref (the (simple-array single-float *) ,array) 6))

(defun reduce-synapse-tree (&optional (cell (car (list-of-all-things 'cell))))
  (setq cell (element cell 'cell))
  (z-cable-in-cell cell)		;To update the 'z-cable-in entry in seg params
  (loop for synapse in (list-of-all-things 'synapse)
	when (and (eq (type-of (element-cell-element synapse)) 'segment)
		  (eq cell (element-cell synapse)))
	do (set-synapse-reduction-coefficients synapse (element-cell-element synapse) nil)))

(defun toggle-reduced-synapses (&optional (syns (list-of-all-things 'synapse)) (enable-message t))
  (loop for syn in syns
	when (eq (type-of (element-cell-element syn)) 'segment)
	do
	(when enable-message
	  (if (synapse-use-reduced-model (element syn 'synapse))
	      (format t "Disabling reduced synapses...~%")
	      (format t "Enabling reduced synapses...~%"))
	  (setq  enable-message nil))
	(setf (synapse-use-reduced-model (element syn 'synapse))
	      (not (synapse-use-reduced-model (element syn 'synapse))))))

(defun look-at-reduced-syn-params (synapse)
  (setq synapse (element synapse))
  (format t "Synapse ~a: gn1: ~,2e gn2: ~,2e d1: ~,2e gd2: ~,2e in1: ~,2e id2: ~,2e trget v-l: ~,2e~%"
	  (synapse-name synapse)
	  (synapse-gn1 synapse)
	  (synapse-gn2 synapse)
	  (synapse-d1 synapse)
	  (synapse-gd2 synapse)
	  (synapse-in1 synapse)
	  (synapse-id2 synapse)
	  (synapse-target-v-leak synapse)))

(defun set-synapse-reduction-coefficients (synapse current-segment last-segment)
  (let (gn1-prev gn2-prev d1-prev gd2-prev in1-prev id2-prev gn1 gn2 d1 gd2 in1 id2 segment-load)
    (unless last-segment (format t "~a:~%" (synapse-name synapse)))
    (if (not last-segment)		

	;; JUST STARTING
	(setq segment-load (segment-load current-segment)
	      gn1 (segment-g-axial current-segment) ; beta-0
	      gn2 (* (segment-g-axial current-segment) segment-load)
	      d1 1.0
	      gd2 (+ (segment-g-axial current-segment) segment-load)
	      in1 (* (- (synapse-e-rev synapse)
			(* (element-slot (element-cell-element synapse) :v-leak)))
		     (segment-g-axial current-segment))
	      id2 gd2)

	;; NOT THE FIRST SEGMENT

	(setq segment-load (segment-load current-segment last-segment)
	      gn1-prev (get-synapse-param synapse 'gn1)
	      gn2-prev (get-synapse-param synapse 'gn2)
	      d1-prev (get-synapse-param synapse 'd1)
	      gd2-prev (get-synapse-param synapse 'gd2)
	      in1-prev (get-synapse-param synapse 'in1)
	      id2-prev (get-synapse-param synapse 'id2)
	      gn1 (+ (* gn1-prev (segment-g-axial current-segment))
		     (* d1-prev (segment-g-axial current-segment) segment-load))
	      gn2 (+ (* gn2-prev (segment-g-axial current-segment))
		     (* gd2-prev (segment-g-axial current-segment) segment-load))
	      d1 (+ gn1-prev
		    (* d1-prev (+ (segment-g-axial current-segment) segment-load)))
	      gd2 (+ gn2-prev
		     (* gd2-prev (+ (segment-g-axial current-segment) segment-load)))
	      in1 (* in1-prev (segment-g-axial current-segment))
	      id2 (* d1-prev (+ (segment-g-axial current-segment) segment-load))))
    
     (format t " cur seg ~a gn1: ~,2e gn2: ~,2e d1: ~,2e gd2: ~,2e in1: ~,2e id2: ~,2e seg-load ~,2e~% "
	      (segment-name current-segment)  gn1 gn2 d1 gd2 in1 id2 segment-load)
    
    (REplace-SYNAPSE-PARAM-ACONS synapse 'gN1 gn1)
    (REplace-SYNAPSE-PARAM-ACONS synapse 'gN2 gn2)
    (REplace-SYNAPSE-PARAM-ACONS synapse 'd1 d1)
    (REplace-SYNAPSE-PARAM-ACONS synapse 'gd2 gd2)
    (REplace-SYNAPSE-PARAM-ACONS synapse 'iN1 in1)
    (REplace-SYNAPSE-PARAM-ACONS synapse 'id2 id2)
    (if (proximal-segment current-segment)
	;; STILL MORE TO DO BEFORE HITTING THE SOMA.
	(set-synapse-reduction-coefficients synapse (proximal-segment current-segment) current-segment)

	;; REACHED FINAL PROXIMAL SEG, SO DONE
	(REplace-SYNAPSE-PARAM-ACONS synapse 'reduced-model-parameters
				     (list-to-array (list gn1 gn2 d1 gd2 in1 id2
							  (soma-v-leak (cell-soma (element-cell synapse)))))))))






(defun segment-load (segment &optional exclude-distal-segment)
  (+ (segment-g-leak segment)
     (loop for distal-seg in (distal-segments segment)
	   when (not (eq distal-seg exclude-distal-segment))
	   summing (/ 1.0 (get-segment-param distal-seg 'z-cable-in)))))

(proclaim '(notinline get-reduced-synapse-current get-reduced-synapse-conductance))

(defun get-reduced-synapse-conductance (actual-conductance reduced-synapse-parameters)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float actual-conductance))
  (- 
   (/
   
    (+
     (* actual-conductance
	(aref-synapse-gn1 reduced-synapse-parameters))
     (aref-synapse-gn2 reduced-synapse-parameters))
    (+     (* actual-conductance
	      (aref-synapse-d1 reduced-synapse-parameters))
	   (aref-synapse-gd2 reduced-synapse-parameters)))

   (/ (aref-synapse-gn2 reduced-synapse-parameters)
      (aref-synapse-gd2 reduced-synapse-parameters))))
      


(defun get-reduced-synapse-current (actual-conductance reduced-synapse-parameters)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float actual-conductance))
  (the single-float
       (+
	(the single-float
	     (* 
	      (the single-float (aref-synapse-target-v-leak reduced-synapse-parameters))
	      (the single-float (get-reduced-synapse-conductance (the single-float actual-conductance)
								 reduced-synapse-parameters))))
	(the single-float
	     (/ (the single-float (* (the single-float actual-conductance)
				     (the single-float (aref-synapse-in1 reduced-synapse-parameters))))
		(the single-float (+ (the single-float
					  (* (the single-float actual-conductance)
					     (the single-float (aref-synapse-d1 reduced-synapse-parameters))))
				     (the single-float (aref-synapse-id2 reduced-synapse-parameters)))))))))




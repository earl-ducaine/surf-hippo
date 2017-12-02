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


(in-package "SURF-HIPPO")

;;; Functions using Cholinergic rabbit retina starburst amacrine cell
;;; described in src/rabbit/star-amacrine.lisp.

(defun starburst-network (cell-spacing cell-per-horizontal-side &optional cell-per-vertical-side)
  (unless cell-per-vertical-side (setq cell-per-vertical-side cell-per-horizontal-side))
  (let* ((*add-cell-name-to-segs* t)
	 (vertical-start (* -0.5 (* cell-spacing (1- cell-per-vertical-side))))
	 (vertical-end (- vertical-start))
	 (horizontal-start (* -0.5 (* cell-spacing (1- cell-per-horizontal-side))))
	 (horizontal-end (- horizontal-start)))
    (setq *print-linear-analysis* nil
	  *enable-light* t *enable-synapses* t
	  *user-stop-time* 300.0  *fast-rf-bar* t *light-speed* 4.0
	  *bar-length* 500.0 *bar-width* 50.0 *light-stimulus* :moving-bar
	  *light-theta* (/ pi-single 2.0)
	  ;; T (nil) => movement is 90 degrees ahead (behind) of *light-theta
	  *light-direction* nil
	  *light-stimulus-start-time* 0.0 ; Time to start bar moving, milliseconds
	  *light-stimulus-stop-time* 100000.0
	  *light-start-position-x* -500.0 ; Point of center of stimulus at *motion-start-time in microns
	  *light-start-position-y* 0.0)	  
    (loop for x from horizontal-start to horizontal-end by cell-spacing do
	  (loop for y from vertical-start to vertical-end by cell-spacing do
		(let ((cell-name (format nil "DS-tip-~d-~d" (round (+ 3 (/ x 100))) (round (+ 3 (/ y 100))))))
		  (loop for name in '("soma" ;"222111B"
				      "2212212"
				      "32211" "223222B" )
			do (let ((elt-name (format nil "~a-~a" cell-name name)))
			     (push elt-name *plot-node-elements*)))
					;                  (if (and (= x -300)(= y 0))
					;                      (setq *synapse-names-to-do-first (list (format nil "~a-~a" cell-name "222111B"))))
		  (star-amacrine cell-name :origin (list x y 0.0))))))) 





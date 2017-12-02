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

(defun HIPPO-net (cell-spacing cells-per-horizontal-side &optional cells-per-vertical-side randoms all-all)
  (let* ((*add-cell-name-to-segs* t)
	 (*PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* nil)
	 (cells-per-vertical-side (round (or cells-per-vertical-side cells-per-horizontal-side)))
	 (cells-per-horizontal-side (round cells-per-horizontal-side))
	 (vertical-start (coerce (* -0.5 (* cell-spacing (1- cells-per-vertical-side))) 'single-float))
	 (vertical-end (- vertical-start))
	 (horizontal-start (coerce (* -0.5 (* cell-spacing (1- cells-per-horizontal-side))) 'single-float))
	 (horizontal-end (- horizontal-start))
	 (cell-array (make-array (list cells-per-horizontal-side cells-per-horizontal-side)))
	 (cell-spacing (coerce cell-spacing 'single-float)))
    (declare (optimize (safety 0) (speed 3) (space 1))
	     (single-float vertical-start vertical-end horizontal-start horizontal-end)
	     (fixnum cells-per-vertical-side cells-per-horizontal-side))
    (setq *enable-light* t *enable-synapses* t)
    (do ((x horizontal-start (the single-float (+ x cell-spacing)))
	 (x-index 0 (the fixnum (1+ x-index))))
	((> x horizontal-end))
      (do ((y vertical-start (the single-float (+ y cell-spacing)))
	   (y-index 0 (the fixnum (1+ y-index))))
	  ((> y vertical-end))
	(declare (single-float y x)(fixnum x-index y-index))
	(let ((cell (hippo :name (format nil "Hippo-~d-~d" (round x) (round y)) :origin (list x 0 y))))
	  (setf (aref cell-array x-index y-index) cell)
	  (when (and (= x horizontal-start) (= y vertical-start))
	    (add-pulse-list (add-isource (cell-soma cell)) '((10.0 20.0 2.0)))))))
    (when randoms (add-some-random-synapses cell-array))
    (when all-all (add-all-all-synapses cell-array))
					;    (setq *plot-soma-nodes* (namelist-of-all-things 'soma)
					;	  *plot-synapse-events* (namelist-of-all-things 'synapse))
    (loop for syn in (synapses-of-control 'voltage) do (setf (synapse-delay syn) (random 50.0)))
    (set-type-graphics 'fast-ex 'yellow)
    nil))

(defun add-some-random-synapses (cell-array)
  (let ((cells-per-horizontal-side (array-dimension cell-array 0))
	(cells-per-vertical-side (array-dimension cell-array 1)))
    (do ((x 0 (the fixnum (1+ x))))
	((= x cells-per-horizontal-side))
      (do ((y 0 (the fixnum (1+ y))))
	  ((= y cells-per-vertical-side))
	(declare (fixnum y x))
	(create-synapse (nth (random 5)
			     (cell-segments (aref cell-array
						  (if (> cells-per-horizontal-side 1)
						      (random (1- cells-per-horizontal-side))
						      0)
						  (if (> cells-per-vertical-side 1)
						      (random (1- cells-per-vertical-side))
						      0))))
			'fast-ex
			(cell-soma (aref cell-array x y)))
				
	(create-synapse (cell-soma (aref cell-array x y)) 'fast-ex
			(cell-soma (aref cell-array
					 (if (> cells-per-horizontal-side 1)
					     (random (1- cells-per-horizontal-side))
					     0)
					 (if (> cells-per-vertical-side 1)
					     (random (1- cells-per-vertical-side))
					     0))))
	(create-synapse (cell-soma (aref cell-array x y)) 'l-ex-fac)))))

(defun add-all-all-synapses (cell-array)
  (let ((cells-per-horizontal-side (array-dimension cell-array 0))
	(cells-per-vertical-side (array-dimension cell-array 1)))
    (do ((x 0 (the fixnum (1+ x))))
	((= x cells-per-horizontal-side))
      (do ((y 0 (the fixnum (1+ y))))
	  ((= y cells-per-vertical-side))
	(declare (fixnum y x))

	(do ((xdest 0 (the fixnum (1+ xdest))))
	    ((= xdest cells-per-horizontal-side))
	  (do ((ydest 0 (the fixnum (1+ ydest))))
	      ((= ydest cells-per-vertical-side))
	    (declare (fixnum ydest xdest))

	    (unless (and (= x xdest) (+ y ydest))
	      (create-synapse (cell-soma (aref cell-array xdest ydest))
			      'fast-ex
			      (cell-soma (aref cell-array x y))))))))))

#|
(progn (profile:unprofile)
       (profile:profile make-channel create-particle make-conc-particle create-synapse
			parse-conc-int-info-for-element create-conc-int
			random			create-segment create-soma)
       (time (topload 'hippo-net-50))
       (profile:report-time)
       (profile:unprofile))
|#

(defun HIPPO-net-50 ()
  (let ((*USE-SIMPLE-NAMES* t))
    (hippo-net 200 50)
    (get-some-somas-and-synapses)))

(defun HIPPO-net-3 () (hippo-net 200 3))

(defun HIPPO-net-1 () (hippo-net 200 1))

(defun HIPPO-net-10-all ()
  (let ((*USE-SIMPLE-NAMES* t))
    (hippo-net 200 10 10 nil t)))

(defun get-some-somas-and-synapses ()
  (setq *group-plot-data-by-cell* nil)
  (setq *plot-soma-nodes* (loop for name in (namelist-of-all-things 'soma)
				for count from 0 to 20
				collect name)
	*plot-synapse-events-p t
	*plot-synapse-events* 
	(loop for syn in (synapses-of-control 'voltage)
	      when
	      (= (first (element-absolute-location syn))
		 (third (element-absolute-location syn)))
	      collect (synapse-name syn)))
  nil)


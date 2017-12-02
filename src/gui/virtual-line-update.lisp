;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SON-OF-PLOT-HACK; Base: 10 -*-

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

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "SON-OF-PLOT-HACK")

;;(proclaim '(inline xlib::draw-line-fast))
;;  Assumes one dimensional gob :array-size. This should handle (at least) segments and axons.
(defun update-virtual-line (gob update-info bbox-1 bbox-2  &optional (total-p NIL))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((dummy (gv gob :dummy-item))
	 (draw-function (GV DUMMY :DRAW-FUNCTION))
	 item-bbox 
	 (invalid-object (gv gob :invalid-object))
	 (dirty-p (opal::update-info-dirty-p update-info))
	 (agg-bbox (opal::update-info-old-bbox update-info))
	 (bbox-array (gv gob :bbox-array))
	 (item-array (gv gob :item-array))
	 (a-window (gv gob :window))
	 (colorize (gv a-window :colorize))
	 (lock-color (and ; (gv a-window :colorize)
		      (gv a-window :lock-color)))
	 (display-info (gv a-window :display-info))
	 (root-window  (opal::display-info-root-window display-info))
	 (drawable (gem::the-drawable a-window))
	 (drawable-display (xlib::drawable-display drawable))
	 (function (get draw-function :x-draw-function))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (and t ; (not lock-color)
	       (or dirty-p
		   total-p
		   (and (opal::bbox-valid-p agg-bbox)
			(opal::bbox-intersects-either-p agg-bbox bbox-1 bbox-2))))
      (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		 (opal::bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(opal::bbox-to-clip-mask-components agg-bbox (gv gob :left) (gv gob :top) (gv gob :width) (gv gob :height)))
      (let ((default-graphics-color (gv a-window :default-graphics-color))
	    (color-shading (gv a-window :segment-color-shading))
	    (default-line-style (gv a-window :default-line-style)))
	(dotimes (n (the fixnum (gv gob :next-available-rank)))
	  (declare (fixnum n))
	  (setq item-bbox (aref bbox-array n))
	  (when (and (opal::bbox-valid-p item-bbox)
		     (or (and bbox-1 (opal::bbox-intersect-p bbox-1 item-bbox))
			 (and bbox-2 (opal::bbox-intersect-p bbox-2 item-bbox))))
	    (let* ((item-values (the simple-array (aref (the simple-array item-array) n)))
		   (lstyle (virtual-line-line-style item-values))
		   (x1 (virtual-line-x1 item-values))
		   (y1 (virtual-line-y1 item-values))
		   (x2 (virtual-line-x2 item-values))
		   (y2 (virtual-line-y2 item-values)))
	      (gem::x-draw-line-fast a-window x1 y1 x2 y2 function lstyle
				     display-info root-window line-style-gc xlib-gc-line
				     drawable drawable-display))))))
    (setf (opal::bbox-valid-p (opal::update-info-old-bbox (the opal::UPDATE-INFO (gv invalid-object :update-info)))) nil)
    (if dirty-p (setf (opal::update-info-dirty-p update-info) NIL))))

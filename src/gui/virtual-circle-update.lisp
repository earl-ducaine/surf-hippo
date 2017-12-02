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


;; DRAW-VIRTUAL-CIRCLE, UPDATE-VIRTUAL-CIRCLE, UPDATE-VIRTUAL-DOT

(in-package "SON-OF-PLOT-HACK")

;;  Assumes one dimensional gob :array-size.
(proclaim '(inline draw-virtual-circle))
(defun draw-virtual-circle (line-style-gc filling-style-gc drawable root-window left top diameter lstyle fstyle x-draw-fn)
;  (declare (optimize (speed 3) (safety 1)))
  (let* ((xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	 (xlib-gc-fill (opal::opal-gc-gcontext filling-style-gc))
	 (thickness 1)
	 (fill-diameter (the fixnum (- diameter 2))))
    (declare (fixnum left top thickness diameter fill-diameter))
    (when (plusp diameter)		;don't draw anything unless diameter > 0
      (if (not (plusp fill-diameter))	; if circle is too small, just draw black circle
	(xlib:with-gcontext (xlib-gc-line :fill-style :solid :function x-draw-fn)
			    (xlib:draw-arc drawable xlib-gc-line left top diameter diameter 0.0 opal::*twopi* t ))
	(let ((d-mod-2 (mod diameter 2))
	      (t-mod-2 (mod thickness 2)))
	  (declare (fixnum d-mod-2 t-mod-2))
	  (when fstyle
	    (gem::set-filling-style fstyle filling-style-gc xlib-gc-fill root-window x-draw-fn)
	    (xlib:draw-arc drawable xlib-gc-fill (+ left thickness) (+ top thickness) fill-diameter fill-diameter 0.0 opal::*twopi* t))
	  (when lstyle
	    (gem::set-line-style lstyle line-style-gc xlib-gc-line root-window x-draw-fn)
	    (xlib:draw-arc drawable
			   xlib-gc-line
			   (+ left (the fn (aref (the (simple-array fixnum (2)) opal::*left-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			   (+ top  (the fn (aref (the (simple-array fixnum (2)) opal::*top-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			   (- diameter (+ thickness (the fn (aref (the (simple-array fixnum (2)) opal::*width-adjustment*) d-mod-2 d-mod-2 t-mod-2))))
			   (- diameter (+ thickness (the fn (aref (the (simple-array fixnum (2)) opal::*height-adjustment*) d-mod-2 d-mod-2 t-mod-2))))
			   0.0 opal::*twopi* nil)))))))

(defun update-virtual-circle (gob update-info bbox-1 bbox-2 &optional (total-p NIL))
;  (declare (optimize (safety 1) (speed 3) (space 1)))
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
	 (default-graphics-filling-style (gv a-window :default-graphics-filling-style))
	 (x-draw-fn (get draw-function :x-draw-function))
	 (filling-style-gc (opal::display-info-line-style-gc display-info))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (and t ; (not lock-color)
	       (or dirty-p total-p
		   (and (opal::bbox-valid-p agg-bbox)
			(opal::bbox-intersects-either-p agg-bbox bbox-1 bbox-2))))
      (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		 (opal::bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(opal::bbox-to-clip-mask-components agg-bbox (gv gob :left) (gv gob :top) (gv gob :width) (gv gob :height)))
      (dotimes (n (the fixnum (gv gob :next-available-rank)))
	(declare (fixnum n))
	(setq item-bbox (aref bbox-array n))
	(when (and (opal::bbox-valid-p item-bbox)
		   (or (and bbox-1 (opal::bbox-intersect-p bbox-1 item-bbox)) (and bbox-2 (opal::bbox-intersect-p bbox-2 item-bbox))))
	  (let* ((item-values (aref (the simple-array item-array) n))
		 (line-style (or (gv gob :line-style) (virtual-circle-line-style item-values)))
		 (filling-style (or (gv gob :filling-style) (virtual-circle-filling-style item-values) default-graphics-filling-style)))
	    (draw-virtual-circle
	     line-style-gc filling-style-gc drawable root-window
	     (virtual-circle-left item-values) (virtual-circle-top item-values) (virtual-circle-width item-values)
	     line-style filling-style x-draw-fn))))
      (setf (opal::bbox-valid-p (opal::update-info-old-bbox (the opal::UPDATE-INFO (gv invalid-object :update-info)))) nil)
      (if dirty-p (setf (opal::update-info-dirty-p update-info) NIL)))))

(defun update-virtual-dot (gob update-info bbox-1 bbox-2 &optional (total-p NIL))
;  (declare (optimize (safety 1) (speed 3) (space 1)))
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
	 (display-info (gv a-window :display-info))
	 (root-window  (opal::display-info-root-window display-info))
	 (drawable (gem::the-drawable a-window))
	 (drawable-display (xlib::drawable-display drawable))
	 (default-graphics-filling-style (gv a-window :default-graphics-filling-style))
	 (x-draw-fn (get draw-function :x-draw-function))
	 (filling-style-gc (opal::display-info-line-style-gc display-info))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (or dirty-p total-p
	      (and (opal::bbox-valid-p agg-bbox)
		   (opal::bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		 (opal::bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(opal::bbox-to-clip-mask-components agg-bbox (gv gob :left) (gv gob :top) (gv gob :width) (gv gob :height)))
      (when opal::*debug-UPDATE-METHOD-VIRTUAL-AGGREGATE*
	    (format t "UPDATE-VIRTUAL-DOT: gob ~A, update-info ~A~%   bbox-1 ~A, bbox-2 ~A, total-p ~A~%" gob update-info bbox-1 bbox-2 total-p)
	    (format t "                   gob :next-available-rank ~A~%" (gv gob :next-available-rank)))
      (dotimes (n (the fixnum (gv gob :next-available-rank)))
	(declare (fixnum n))
	(setq item-bbox (aref bbox-array n))
	(when (and (opal::bbox-valid-p item-bbox)
		   (or (and bbox-1 (opal::bbox-intersect-p bbox-1 item-bbox)) (and bbox-2 (opal::bbox-intersect-p bbox-2 item-bbox))))
	  (let ((item-values (aref (the simple-array item-array) n)))
	    (draw-virtual-circle
	     line-style-gc filling-style-gc drawable root-window
	     (virtual-gob-x-left item-values) (virtual-gob-y-top item-values)
	     (- (virtual-gob-y-bottom item-values) (virtual-gob-y-top item-values))
	     (or (gv dummy :line-style) (virtual-gob-line-style item-values)) 
	     (or (gv dummy :filling-style) (virtual-gob-filling-style item-values) ; default-graphics-filling-style
		 )
	     x-draw-fn)))))
    (setf (opal::bbox-valid-p (opal::update-info-old-bbox (the opal::UPDATE-INFO (gv invalid-object :update-info)))) nil)
    (if dirty-p (setf (opal::update-info-dirty-p update-info) NIL))))

(export '(draw-virtual-circle update-virtual-circle update-virtual-dot))



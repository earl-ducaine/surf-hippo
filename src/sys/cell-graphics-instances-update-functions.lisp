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


;;; SYS Source file: cell-graphics-instances-update-functions.lisp

;; This file is modified from Garnet source code.

(IN-PACKAGE "SURF-HIPPO")

;; UPDATE-VIRTUAL-SOMA and UPDATE-VIRTUAL-SEGMENT are derived from UPDATE-VIRTUAL-CIRCLE and UPDATE-VIRTUAL-LINE to handle somas and segments.

(defvar *debug-virtual-cell-elements* nil)

(defun update-virtual-soma (gob update-info bbox-1 bbox-2 &optional (total-p NIL))
;;  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((dummy (gv gob :dummy-item))
	 (draw-function (GV DUMMY :DRAW-FUNCTION))
	 item-bbox
	 (invalid-object (gv gob :invalid-object))
	 (dirty-p (opal::update-info-dirty-p update-info))
	 (agg-bbox (opal::update-info-old-bbox update-info))
	 (bbox-array (gv gob :bbox-array))
	 (item-array (gv gob :item-array))
	 (a-window (gv gob :window))
	 (soma-histology-fixed-diameter-p (gv a-window :soma-histology-fixed-diameter-p))
	 (soma-histology-fixed-diameter-pixels (or (gv a-window :soma-histology-fixed-diameter-pixels) 0))
	 (soma-histology-fixed-radius-pixels (round (/ soma-histology-fixed-diameter-pixels 2))) ; This should be in pixels
	 (soma-outline-p (gv a-window :soma-outline-p))
	 (colorize (gv a-window :colorize))
	 (lock-color (gv a-window :lock-color))
	 (display-info (gv a-window :display-info))
	 (root-window  (opal::display-info-root-window display-info))
	 (drawable (gem::the-drawable a-window))
	 (drawable-display (xlib::drawable-display drawable))
	 (default-graphics-filling-style (gv a-window :default-graphics-filling-style))
	 (x-draw-fn (get draw-function :x-draw-function))
	 (filling-style-gc (opal::display-info-line-style-gc display-info))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (declare (fixnum soma-histology-fixed-radius-pixels soma-histology-fixed-diameter-pixels))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (or dirty-p total-p
	      (and (opal::bbox-valid-p agg-bbox)
		   (opal::bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) (opal::bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(opal::bbox-to-clip-mask-components agg-bbox (gv gob :left) (gv gob :top) (gv gob :width) (gv gob :height)))
      (dotimes (n (the fixnum (gv gob :next-available-rank)))
	(declare (fixnum n))
	(setq item-bbox (aref bbox-array n))
	(when nil		       ; *debug-virtual-cell-elements*
	  (format t "update-virtual-soma: agg-bbox ~A valid-p ~A, bbox-1 ~A valid-p ~A, bbox-2 ~A valid-p ~A, item-bbox ~a valid-p ~A~%"
		  (true-p agg-bbox) (when agg-bbox (opal::bbox-valid-p agg-bbox))
		  (true-p bbox-1) (when bbox-1 (opal::bbox-valid-p bbox-1))
		  (true-p bbox-2) (when bbox-2 (opal::bbox-valid-p bbox-2))
		  (true-p item-bbox) (when item-bbox (opal::bbox-valid-p item-bbox))))
	(when (and (opal::bbox-valid-p item-bbox) (or (and bbox-1 (opal::bbox-intersect-p bbox-1 item-bbox)) (and bbox-2 (opal::bbox-intersect-p bbox-2 item-bbox))))
	  (let* ((item-values (aref (the simple-array item-array) n))
		 (soma (virtual-soma-soma item-values))
		 (circle-width (if soma-histology-fixed-diameter-p soma-histology-fixed-diameter-pixels (virtual-circle-width item-values)))
		 (circle-top (if soma-histology-fixed-diameter-p
				 (the fn (- (virtual-circle-center-y item-values) soma-histology-fixed-radius-pixels))
				 (virtual-circle-top item-values)))
		 (circle-left (if soma-histology-fixed-diameter-p
				  (the fn (- (virtual-circle-center-x item-values) soma-histology-fixed-radius-pixels))
				  (virtual-circle-left item-values)))
		 (filling-style (if colorize
				    (if lock-color
					(aref *cell-fill-styles* (virtual-soma-color-index item-values)) 
					(access-*fill-styles*-for-soma-voltage soma))
				    (or (virtual-circle-filling-style item-values) default-graphics-filling-style)))
		 (line-style (when soma-outline-p
			       (or (virtual-circle-line-style item-values)
				   (access-*line-styles-array*-for-segments-fast 0 (gv filling-style :FOREGROUND-COLOR) 100.0 nil)))))
	    ;;	    (print x-draw-fn)
	    (draw-virtual-circle
	     line-style-gc filling-style-gc drawable root-window
	     circle-left circle-top circle-width
	     line-style
	     filling-style x-draw-fn)))
	(setf (opal::bbox-valid-p (opal::update-info-old-bbox (the opal::UPDATE-INFO (gv invalid-object :update-info)))) nil)
	(if dirty-p (setf (opal::update-info-dirty-p update-info) NIL))))))

(defun update-virtual-segment (gob update-info bbox-1 bbox-2  &optional (total-p NIL))
;;  (declare (optimize (safety 1) (speed 3) (space 1)))
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
	 (lock-color (gv a-window :lock-color))
	 (display-info (gv a-window :display-info))
	 (root-window  (opal::display-info-root-window display-info))
	 (drawable (gem::the-drawable a-window))
	 (drawable-display (xlib::drawable-display drawable))
	 (function (get draw-function :x-draw-function))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (declare (ignore root-window drawable-display xlib-gc-line))
    ;;    (print dummy)    (print function)    (print draw-function)    (break)
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (or dirty-p total-p (and (opal::bbox-valid-p agg-bbox) (opal::bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		 (opal::bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(opal::bbox-to-clip-mask-components agg-bbox (gv gob :left) (gv gob :top) (gv gob :width) (gv gob :height)))
      (let ((default-graphics-color (gv a-window :default-graphics-color))
	    (color-shading (gv a-window :segment-color-shading))
	    ;; (default-line-style (gv a-window :default-line-style))
	    )
	(dotimes (n (the fixnum (gv gob :next-available-rank)))
	  (declare (fixnum n))
	  (setq item-bbox (aref bbox-array n))
#|
	  (when nil ; *debug-virtual-cell-elements*
	    (format t "update-virtual-segment: agg-bbox ~A valid-p ~A, bbox-1 ~A valid-p ~A, bbox-2 ~A valid-p ~A, item-bbox ~a valid-p ~A~%"
		    (true-p agg-bbox) (when agg-bbox (opal::bbox-valid-p agg-bbox))
		    (true-p bbox-1) (when bbox-1 (opal::bbox-valid-p bbox-1))
		    (true-p bbox-2) (when bbox-2 (opal::bbox-valid-p bbox-2))
		    (true-p item-bbox) (when item-bbox (opal::bbox-valid-p item-bbox))))
|#
	  ;; Need to fix this someday??
	  (when (and (opal::bbox-valid-p item-bbox)
		     (or (and bbox-1 (opal::bbox-intersect-p bbox-1 item-bbox))
			 (and bbox-2 (opal::bbox-intersect-p bbox-2 item-bbox))))
#|
	    (when nil ; *debug-virtual-cell-elements*
	      (format t "Update n: ~d~%" n))
|#
	    (let* ((item-values (the simple-array (aref (the simple-array item-array) n)))
		   (seg (unless lock-color (virtual-segment-segment item-values)))
		   (thickness (virtual-segment-thickness item-values))
		   (lstyle (if colorize
			     (if lock-color
			       (aref *cell-color-line-styles*
				     (min thickness (the fn *cell-color-line-styles-thickness-dimension-max-index*))
				     (virtual-segment-color-index item-values))
			       (access-*line-styles-array*-for-segment-voltage thickness seg))
			     (or (virtual-line-line-style item-values)
				 (access-*line-styles-array*-for-segments-fast
				  thickness default-graphics-color color-shading
				  (typecase seg
				    ; (segment (node-color-index (segment-node-2 seg)))
				    (t nil))))))
		   (x1 (virtual-line-x1 item-values))
		   (y1 (virtual-line-y1 item-values))
		   (x2 (virtual-line-x2 item-values))
		   (y2 (virtual-line-y2 item-values)))
;; LG fix blech - replace function arg with explicit :copy 26.08.2016
	      (gem::x-draw-line a-window x1 y1 x2 y2 :copy lstyle drawable)
;	      (gem::x-draw-line a-window x1 y1 x2 y2 function lstyle drawable)
;	      (gem::x-draw-line-fast a-window x1 y1 x2 y2 function lstyle
;				     display-info root-window line-style-gc xlib-gc-line
;				     drawable drawable-display)
	      )))))
    (setf (opal::bbox-valid-p (opal::update-info-old-bbox (the opal::UPDATE-INFO (gv invalid-object :update-info)))) nil)
    (if dirty-p (setf (opal::update-info-dirty-p update-info) NIL))))


;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

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

;; This file is modified from Garnet source code.

;; ;;; SYS Source file: ps-object-methods.lisp

(in-package "OPAL")

(defvar *use-draw-arc-for-opal-circle* nil)

;; LBG Aug. 2 2001
;; Somehow,  borderless dots are getting magnified in the postscript rendering. Here, temporarily force a border of the same color
;; as the fill to avoid this.
;; Now disabled...?

;; LG June 16, 2002 Add :soma-histology-fixed-diameter-p check, and virtual-soma-color-index reference for filling
;; style. Also, moving this to sys directory, since virtual soma macros need to be defined first.

(define-method :ps-object OPAL:CIRCLE (obj)
  (when (or (g-value obj :line-style) (g-value obj :filling-style))
    (let* ((window (gv obj :parent :window))
	   (item-values (gv obj :item-values))
	   (left (g-value obj :left))
	   (opal-top (g-value obj :top))
	   (width (min (g-value obj :width) (g-value obj :height)))
	   (radius (float (/ width 2)))
	   (center-x (+ left radius))
	   (center-y (convert-y (+ opal-top radius)))
	   (virtual-soma-p (is-a-p OPAL::OBJ sh::virtual-soma)))
      (when (and virtual-soma-p (gv window :soma-histology-fixed-diameter-p))
	(setq width (gv window :soma-histology-fixed-diameter-pixels)
	      radius (float (/ width 2))
	      left (- center-x radius)
	      opal-top (- center-y radius)))
      (if *use-draw-arc-for-opal-circle*
	  (progn
	    (format T "~S ~S ~S ~S ~S ~S " center-x center-y radius radius 0 360)
	    (print-graphic-qualities obj)
	    (format T "DrawArc~%"))
	  (progn
	    ;; Parameters: center-x, center-y, radius-x, radius-y, angle1, angle2,
	    ;;             line-thickness, line-halftone, fill-halftone
	    (format T "~S ~S ~S ~S 0 360 " center-x center-y radius radius)
	    (let ((dummy-line-style (unless t ; (g-value obj :line-style)
				      (wh::access-*line-styles-array*-fast 1 (g-value obj :filling-style :foreground-color)))))
;;	      (break)
	      (when (and virtual-soma-p (gv window :colorize))
		(s-value obj :filling-style
			 (if t ; (gv window :lock-color)
			     (let ((fs (aref sh::*cell-fill-styles* (sh::virtual-soma-color-index item-values)) ))
			       ; (format t "fs ~A ~A~%" (sh::virtual-soma-soma item-values) fs)
			       fs)
				       (sh::access-*fill-styles*-for-soma-voltage (sh::virtual-soma-soma item-values)))))
	      (s-value obj :line-style dummy-line-style)
	      (print-graphic-qualities obj)
	      (when dummy-line-style (s-value obj :line-style nil)))
	    (format T "DrawEllipse~%"))))))



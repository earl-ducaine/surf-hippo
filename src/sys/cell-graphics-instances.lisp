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

;;; SYS Source file: cell-graphics-instances.lisp


(IN-PACKAGE "SURF-HIPPO")

(create-instance 'GRAPES opal:aggregadget
		 ;; Given a list of N colors, this aggregadget constructs a radially symmetric (mod N) collection of circles whose
		 ;; center of gravity is given by (CENTER-X, CENTER-Y) in pixels. The diameter of each grape is given by the
		 ;; :GRAPE-SIZE slot in the parent window, or the global variable *GRAPE-SIZE* if this value is NIL. The grapes
		 ;; will have borders if the parent window slot :SUPPRESS-GRAPE-BORDERS is NIL. The line style of the
		 ;; borders is given by the parent window slot :DEFAULT-LINE-STYLE.
		 (:grape-colors '(red))
		 (:center-x 0) (:center-y 0)
		 (:window nil)
		 (:grape-size (o-formula (the fn (or (and (gvl :window) (gvl :window :grape-size)) *GRAPE-SIZE* 10))))
		 (:borders-p (o-formula (if (gvl :window) (not (gvl :window :suppress-grape-borders)) t)))
		 (:line-style (o-formula (when (and (gvl :borders-p) (gvl :window)) (gvl :window :default-line-style))))
		 (:parts (o-formula (let* ((grape-size (gvl :grape-size))
					   (half-grape (round (/ grape-size 2)))
					   (color-length (length (gvl :grape-colors)))
					   (angle-inc (/ 360.0 color-length)))
				      (declare (fixnum half-grape))
				      (loop for i fixnum from 0
					    for angle single-float from 0.0 by (the sf angle-inc)
					    for color in (gvl :grape-colors)
					    collect `(:grapes ,opal:circle
							      (:width ,grape-size)
							      (:height ,grape-size)
							      (:line-style ,(o-formula (gvl :parent :line-style)))
							      (:filling-style ,(color-to-fill color))
							      (:left ,(+ (the fn (gvl :center-x)) (- half-grape)
									 (the fn (if (= color-length 0)
										   0 
										   (round (* (the sf (cos-degrees angle)) half-grape))))))
							      (:top ,(+ (the fn (gvl :center-y)) (- half-grape)
									(the fn (if (= color-length 0)
										  0 
										  (round (* (the sf (sin-degrees angle)) half-grape))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'virtual-cell-element-marker virtual-circle
		 (:line-style (o-formula (or (gvl :parent :window :default-cell-element-marker-line-style) thin-line))))

(create-instance 'virtual-cell-element-marker-no-border virtual-cell-element-marker (:line-style nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-SEGMENT :item-values inherit those of VIRTUAL-LINE, and add thickness, segment and colorized index entries ->
;;     #(x1 y1 x2 y2 line-style thickness segment color-index)
;;
(defmacro virtual-segment-thickness (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 5)))
(defmacro virtual-segment-segment (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 6))
(defmacro virtual-segment-color-index (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 7))

(defun make-virtual-segment-item-values-array (x1 y1 x2 y2 line-style thickness segment &optional (color-index 0))
  (collect-to-array x1 y1 x2 y2 line-style thickness segment color-index))

(create-instance 'virtual-segment virtual-line
		 (:line-style (o-formula (if (and (gvl :parent :window) (gvl :parent :window :colorize))
					   (access-*line-styles-array*-for-segment-voltage (virtual-segment-thickness) (virtual-segment-segment))
					   (or (virtual-line-line-style)
					       (and (gvl :parent :window)
						    (access-*line-styles-array*-fast
						     (virtual-segment-thickness)
						     (gvl :parent :window :default-line-style :foreground-color)))))))
		 (:update-function 'update-virtual-segment))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-SOMA :item-values inherit those of VIRTUAL-CIRCLE, and add a soma and colorized index entries ->
;;       #(left top width height filling-style line-style center-x center-y soma color-index)
;;
(defmacro virtual-soma-soma (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 8))
(defmacro virtual-soma-color-index (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 9))

(defun make-virtual-soma-item-values-array (left top width height filling-style line-style center-x center-y soma &optional (color-index 0))
  (collect-to-array left top width height filling-style line-style center-x center-y soma color-index))

(create-instance 'virtual-soma virtual-circle
		 (:line-style (o-formula (or (virtual-circle-line-style)
					     (access-*line-styles-array*-for-segments-fast 0 (gvl :filling-style :FOREGROUND-COLOR) 100.0 nil))))
		 (:filling-style (o-formula (and (gvl :parent :window)
						 (gvl :parent :window :colorize)
						 (virtual-soma-soma)
						 (access-*fill-styles*-for-soma-voltage (virtual-soma-soma)))))
		 (:update-function 'update-virtual-soma))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defmacro generate-update-slots-function (virtual-thing)
  #'(lambda (dummy dummy-update-slots-values)
      `(let ((slots (g-value ,virtual-thing :update-slots)))
	,@(loop for slot in slots
	   for dummy-vals-indx fixnum from 0
	   collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx) (g-value dummy ,slot))))))

(s-value virtual-segment :generate-update-slots-function (GENERATE-UPDATE-SLOTS-FUNCTION virtual-segment))

(s-value virtual-segment :generate-update-slots-function
	 #'(LAMBDA (DUMMY DUMMY-UPDATE-SLOTS-VALUES)
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 0) (G-VALUE DUMMY :VISIBLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 1) (G-VALUE DUMMY :FAST-REDRAW-P))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 2) (G-VALUE DUMMY :X1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 3) (G-VALUE DUMMY :X2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 4) (G-VALUE DUMMY :Y1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 5) (G-VALUE DUMMY :Y2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 6) (G-VALUE DUMMY :LINE-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 7) (G-VALUE DUMMY :FILLING-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 8) (G-VALUE DUMMY :DRAW-FUNCTION))
	     ))
|#



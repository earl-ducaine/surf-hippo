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


;;; SYS Source file: cell-graphics-setup.lisp

;;; Variables, instances and some function definitions for cell graphics.

(IN-PACKAGE "SURF-HIPPO")

(defvar *histology-window-min-height* 1) ; pixels
(defvar *histology-window-min-width* 1) ; pixels
(defvar *minimum-cell-histo-x-span* 400.0) ; microns
(defvar *minimum-cell-histo-y-span* 400.0) ; microns

(defvar *override-screen-max* nil)

(defvar *histology-scale-bar-left* 20) ; pixels
(defvar *histology-scale-bar-bottom* 45) ; pixels
(defvar *histology-scale-bar-um-length* 100) ; pixels

;;; This is for histology graphics. All zoom windows will be bounded by *histology-zoom-window-max-width*
;;; and *histology-zoom-window-max-height*, keeping the aspect ration of the zoom interactor.

(defvar *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA* 120)
(defvar *histology-zoom-window-max-width* 450)
(defvar *histology-zoom-window-max-height* 450)

(proclaim '(type fixnum
	    *histology-scale-bar-left*
	    *histology-scale-bar-bottom*
	    *histology-scale-bar-um-length*
	    *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA*
	    *histology-zoom-window-max-width*
	    *histology-zoom-window-max-height*))

(defvar *GRAPE-SIZE* 10) ; pixels
(defvar *GRAPE-SIZE-microns* 10.0)

(defvar *default-histology-window-background-color* 'black
  "Default background color for histology windows (e.g. 'BLACK 'RED 'GREEN 'BLUE 'ORANGE 'CYAN 'PURPLE 'YELLOW 'WHITE).")

(defvar *chosen-one-color* 'blue)
(defvar *CHOSEN-ONE-SHADING* 50)
(defvar *marked-node-fill* (get-opal-color-to-fill 'black 15))
(defvar *plotted-node-fill* (get-opal-color-to-fill 'black 35))

;; Voltage colorizing variables and code.

;; These must be single floats.
(defvar *colorizing-arrays-color-min* -80.0)		;mV
(defvar *colorizing-arrays-color-max* 0.0)		;mV


(defvar *colorizing-arrays-color-dimension* 100) ; fixnum
(defvar *colorizing-arrays-color-step* (/ (- *colorizing-arrays-color-max* *colorizing-arrays-color-min*) *colorizing-arrays-color-dimension*)) ; mV
(defvar *colorizing-arrays-color-step-df* (coerce *colorizing-arrays-color-step* 'double-float))
(defvar *colorizing-arrays-color-max-/step* (round (/ *colorizing-arrays-color-max* *colorizing-arrays-color-step*)))
(defvar *colorizing-arrays-color-min-/step* (round (/ *colorizing-arrays-color-min* *colorizing-arrays-color-step*)))

(proclaim '(single-float *colorizing-arrays-color-min* *colorizing-arrays-color-max* *colorizing-arrays-color-step*))
(proclaim '(double-float *colorizing-arrays-color-step-df*))
(proclaim '(fixnum *colorizing-arrays-color-dimension* *colorizing-arrays-color-max-/step* *colorizing-arrays-color-min-/step*))

(defun update-colorizing-scale-parameters ()
  (setq *colorizing-arrays-color-step* (s-flt (/ (- *colorizing-arrays-color-max* *colorizing-arrays-color-min*) *colorizing-arrays-color-dimension*)) ; mV
  	*colorizing-arrays-color-step-df* (coerce *colorizing-arrays-color-step* 'double-float)
	*colorizing-arrays-color-max-/step* (round (/ *colorizing-arrays-color-max* *colorizing-arrays-color-step*))
        *colorizing-arrays-color-min-/step* (round (/ *colorizing-arrays-color-min* *colorizing-arrays-color-step*))))

(defvar *minimum-segment-thickness* 0)	; pixels
(defvar *maximum-segment-thickness* 99)	; pixels
(defvar *segment-thickness-dimension* (+ 1 (- *maximum-segment-thickness* *minimum-segment-thickness*)))


(proclaim '(fixnum *segment-thickness-dimension* *minimum-segment-thickness* *maximum-segment-thickness*))

(defvar *cell-color-line-styles* (make-array (list *segment-thickness-dimension* *colorizing-arrays-color-dimension*)))
(defvar *cell-fill-styles* (make-array (list *colorizing-arrays-color-dimension*)))
(defvar *colorizing-colors* (make-array (list *colorizing-arrays-color-dimension*)))

(proclaim '(type (simple-array schema (* *)) *cell-color-line-styles*))
(proclaim '(type (simple-array schema (*)) *cell-fill-styles* *colorizing-colors*))

(defvar *cell-color-line-styles-thickness-dimension-max-index* (1- (car (ARRAY-DIMENSIONS *cell-color-line-styles*))))
(defvar *cell-color-line-styles-voltage-dimension-max-index* (1- (cadr (ARRAY-DIMENSIONS *cell-color-line-styles*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Kill this limiting for now....

(defconstant *histology-pixels-min-limit* -2000)
(defconstant *histology-pixels-max-limit* 2000)

(proclaim '(inline histology-pix-from-xy-vals-limit))
#|
(defun histology-pix-from-xy-vals-limit (pixel-value)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (the (signed-byte 32) pixel-value))
|#

;; LBG 8.19.99 Actually, keeping it seems to guarantee that the result is (INTEGER -2000 2000) given the constants above, and this
;; contraint propogates so that X-Y-HISTOLOGY-WIN compiles with 0 notes instead of 8. Thus VIRTUAL-SEGMENT-CORE, for example,
;; conses half as much.

(defun histology-pix-from-xy-vals-limit (pixel-value) (fixnum-max *histology-pixels-min-limit* (fixnum-min *histology-pixels-max-limit* pixel-value)))
	    
;;; X-Y-HISTOLOGY-WIN - Translates from data coordinates to histology window coordinates in which the origin is at the center of
;;; the window.  Separate transforms are needed for x and y since the origin of an opal:window is at the upper left hand corner,
;;; thus requiring a flipping of the y values. The scale factor is in units of microns per pixel, and the x and y args are in
;;; microns (assuming that the window :scale slot is in microns per pixel).

;;; X-Y-HISTOLOGY-WIN returns a list of (x y), in window coordinates, appropriately shifted and rotated according to the current
;;; shift and rotate slots in the window.

;;; X-Y-HISTOLOGY-WIN uses the 3x3 transformation matrix (:current-xfrm) to handle rotations and shifts, but scaling is done with
;;; :scale parameter. Transformation matrix is derived from Symbolics graphics transformation method.

;; 7/4/02 LG XLIB:DRAW-LINE expects (UNSIGNED-BYTE 16) type arguments. Thus, change all type of values returned by the XY value graphics conversion
;; routines accordingly.

(proclaim '(inline rotated-histo-x-from-xy))
(defun rotated-histo-x-from-xy (half-win-width x-scaled y-scaled win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float x-scaled y-scaled half-win-width)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (+ half-win-width
				(+ (* x-scaled (the sf (gv win :current-xfrm-0-0)))
				   (* y-scaled (the sf (gv win :current-xfrm-1-0)))
				   (the sf (gv win :current-xfrm-2-0))))))))

(proclaim '(inline rotated-histo-y-from-xy))
(defun rotated-histo-y-from-xy (half-win-height x-scaled y-scaled win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float x-scaled y-scaled half-win-height)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (- half-win-height
				(+ (* x-scaled (the sf (gv win :current-xfrm-0-1)))
				   (* y-scaled (the sf (gv win :current-xfrm-1-1)))
				   (the sf (gv win :current-xfrm-2-1))))))))

(proclaim '(inline non-rotated-histo-x-from-x))
(defun non-rotated-histo-x-from-x (half-win-width x-scaled win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float x-scaled half-win-width)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (+ half-win-width (+ x-scaled (the sf (gv win :current-xfrm-2-0))))))))
   
(proclaim '(inline non-rotated-histo-y-from-y))
(defun non-rotated-histo-y-from-y (half-win-height y-scaled win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float y-scaled half-win-height)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (- half-win-height (+ y-scaled (the sf (gv win :current-xfrm-2-1))))))))

(proclaim '(inline x-y-histology-win-values-with-dims))
(defun x-y-histology-win-values-with-dims (x y half-win-width half-win-height win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float x y half-win-width half-win-height)
	   (type KR::SCHEMA win))
  (let ((x-scaled (/ x (the sf (gv win :scale))))
	(y-scaled (/ y (the sf (gv win :scale)))))
    (declare (single-float x-scaled y-scaled))
    (if (gv win :current-xfrm-rotates)
	(values (rotated-histo-x-from-xy half-win-width x-scaled y-scaled win)
		(rotated-histo-y-from-xy half-win-height x-scaled y-scaled win))
	(values (non-rotated-histo-x-from-x half-win-width x-scaled win)
		(non-rotated-histo-y-from-y half-win-height y-scaled win)))))

(proclaim '(inline x-y-histology-win-values))
(defun x-y-histology-win-values (x y win)
  ;; The same as X-Y-HISTOLOGY-WIN but returns X Y as VALUES. 
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (values fixnum)
	   (type single-float x y))
  (x-y-histology-win-values-with-dims x y (schema-half-width win) (schema-half-height win) win))

(proclaim '(inline x-y-histology-win))
(defun x-y-histology-win (x y win)
  (multiple-value-list (x-y-histology-win-values x y win)))

(proclaim '(inline x-y-cons-histology-win))
(defun x-y-cons-histology-win (x-y win)
  (x-y-histology-win (car x-y) (cadr x-y) win))

;;***************;***************;***************;***************;***************;***************

(defun x-y-histology-win-inv-values (x-pix y-pix win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (declare (type fixnum x-pix y-pix)
	   (type KR::SCHEMA win))
  (let ((xfrm (gv win :current-xfrm))
	(scale (gv win :scale)))
    (declare (type (simple-array single-float (* *)) xfrm)
	     (single-float scale))
    (let* ((half-height (schema-half-height-fn win))
	   (half-width (schema-half-width-fn win)) 
	   (x (/
	       (+
		(* (- (- half-height (aref xfrm 2 1)) y-pix)
		   (/ (aref xfrm 1 0)
		      (* (aref xfrm 1 1) (aref xfrm 0 0))))
		(/ (- (+ half-width (aref xfrm 2 0)) x-pix)
		   (aref xfrm 0 0)))
	       (+ -1 (/ (* (aref xfrm 0 1) (aref xfrm 1 0))
			(* (aref xfrm 1 1) (aref xfrm 0 0))))))
	   (y (- (/ (- (the (signed-byte 32) (- half-height y-pix)) (aref xfrm 2 1))
		    (aref xfrm 1 1))
		 (* x (/ (aref xfrm 0 1) (aref xfrm 1 1))))))
      (values (* x scale) (* y scale)))))

(defun x-y-histology-win-inv (x-pix y-pix win) (multiple-value-list (x-y-histology-win-inv-values x-pix y-pix win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COORDINATE TRANSLATION (3d to 2d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GET-VIEW-PLANE-X Family - Getting the window X pixel value.
(proclaim '(inline GET-VIEW-PLANE-X-from-x-y-z))
(defun get-view-plane-x-from-x-y-z (x y z cos-viewing-phi sin-viewing-phi)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (values single-float)
	   (ignore y)
	   (single-float x z cos-viewing-phi sin-viewing-phi))
  (+ (* x cos-viewing-phi)
     (* -1.0 z sin-viewing-phi)))

(proclaim '(inline GET-VIEW-PLANE-X))
(defun get-view-plane-x (3D-coords cos-viewing-phi sin-viewing-phi)
  ;; With args cos-viewing-phi sin-viewing-phi, where PHI is the angle of the X'Y' viewing plane rotated counter-clockwise along
  ;; the Y axis, ie when PHI = 90deg, X' = -Z.
  (get-view-plane-x-from-x-y-z (first 3D-coords) (second 3D-coords) (the sf (caddr 3D-coords)) cos-viewing-phi sin-viewing-phi))

(proclaim '(inline GET-win-VIEW-PLANE-x-from-x-y-z))
(defun get-win-view-plane-x-from-x-y-z (x y z win)
  (get-view-plane-x-from-x-y-z x y z (gv win :cos-phi) (gv win :sin-phi)))

(proclaim '(inline  GET-win-VIEW-PLANE-X))
(defun get-win-view-plane-x (3D-coords win)
  (get-view-plane-x 3D-coords (the sf (gv win :cos-phi)) (the sf (gv win :sin-phi))))

;;; GET-VIEW-PLANE-Y Family - Getting the window Y pixel value.
(proclaim '(inline GET-VIEW-PLANE-y-from-x-y-z))
(defun get-view-plane-y-from-x-y-z (x y z sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (values single-float)
	   (single-float x y z sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta))
  (+ (* -1.0 x sin-viewing-phi*sin-viewing-theta)
     (* y cos-viewing-theta)
     (* -1.0 z cos-viewing-phi*sin-viewing-theta)))

(proclaim '(inline GET-VIEW-PLANE-y))
(defun get-view-plane-y (3D-coords sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta)
  (get-view-plane-y-from-x-y-z (first 3D-coords) (second 3D-coords) (third 3D-coords) sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta))

(proclaim '(inline GET-win-VIEW-PLANE-y-from-x-y-z))
(defun get-win-view-plane-y-from-x-y-z (x y z win)
  ;; With args sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta, where PHI is the angle of
  ;; the X'Y' viewing plane rotated counter-clockwise along the Y axis, ie when PHI = 90deg, X' = -Z, and THETA is the angle of
  ;; the viewing plane perpendicular, with respect to the XZ plane.
  (get-view-plane-y-from-x-y-z
   x y z (gv win :sin-phi*sin-theta) (gv win :cos-theta) (gv win :cos-phi*sin-theta)))

(proclaim '(inline GET-win-VIEW-PLANE-y))
(defun get-win-view-plane-y (3D-coords win) (get-view-plane-y 3D-coords (gv win :sin-phi*sin-theta) (gv win :cos-theta) (gv win :cos-phi*sin-theta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some combined translations (X and Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-win-view-plane-x-y (3D-coords win) (list (get-win-view-plane-x 3D-coords win) (get-win-view-plane-y 3D-coords win)))
  
(defun get-win-view-plane-x-y-from-x-y-z (x y z win) (list (get-win-view-plane-x-from-x-y-z x y z win) (get-win-view-plane-y-from-x-y-z x y z win)))  

(proclaim '(inline x-y-histology-win-from-view))
(defun x-y-histology-win-from-view (data-location win)
  (x-y-histology-win
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   win))

(proclaim '(inline x-y-histology-win-from-view-values))
(defun x-y-histology-win-from-view-values (data-location win)
  (x-y-histology-win-values
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   win))

(proclaim '(inline x-y-histology-win-from-view-values-with-dims))
(defun x-y-histology-win-from-view-values-with-dims (data-location half-win-width half-win-height win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float half-win-width half-win-height))
  (x-y-histology-win-values-with-dims
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   half-win-width half-win-height
   win))

(proclaim '(inline x-y-histology-win-from-view-x-y-z-values))
(defun x-y-histology-win-from-view-x-y-z-values (x y z win)
  (x-y-histology-win-values
   (get-win-view-plane-x-from-x-y-z x y z win) ; start-x
   (get-win-view-plane-y-from-x-y-z x y z win) ; start-y
   win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(proclaim '(inline color-styles-voltage-index))
(defun color-styles-voltage-index (voltage)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (double-float voltage))
  (if (>= voltage *colorizing-arrays-color-max*)
      (1- (- *colorizing-arrays-color-max-/STEP* *colorizing-arrays-color-min-/STEP*))
      (if (<= voltage (+ *colorizing-arrays-color-step-df* *colorizing-arrays-color-min*))
	  0
	  (1- (- (the fn (round (/ voltage *colorizing-arrays-color-step-df*))) *colorizing-arrays-color-min-/step*)))))

(defun fill-*cell-fill-styles* (&key (variable-color-min *colorizing-arrays-color-min*)
				(variable-color-max *colorizing-arrays-color-max*)
				(variable-color-step *colorizing-arrays-color-step*)
				(colors-array *colorizing-colors*)
				(color-fill-styles-array *cell-fill-styles*)
				(color-line-styles-array *cell-color-line-styles*))
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array * (*)) colors-array color-fill-styles-array)
	   (type (simple-array * (* *)) color-line-styles-array))
  (let ((variable-color-min (s-flt variable-color-min))
	(variable-color-max (s-flt variable-color-max))
	(variable-color-step (s-flt variable-color-step)))
    (loop for voltage single-float from variable-color-min by variable-color-step
	  for color-index fixnum from 0 to (1- (the fn (cadr (ARRAY-DIMENSIONS color-line-styles-array))))
	  do (let ((color (get-opal-variable-color voltage :max-variable variable-color-max :min-variable variable-color-min)))
	       (loop for thickness fixnum from *minimum-segment-thickness* to *maximum-segment-thickness* do
		     (when (= 0 thickness)
		       (setf (aref color-fill-styles-array color-index) (get-opal-color-to-fill color))
		       (setf (aref colors-array color-index) color))			
		     (setf (aref color-line-styles-array thickness color-index)
			   (create-instance nil opal:line-style (:constant nil) (:foreground-color color) (:line-thickness thickness))))))))

(eval-when (load) (fill-*cell-fill-styles*))

(proclaim '(notinline access-*line-styles-array*-for-segments))
(defun access-*line-styles-array*-for-segments (thickness-arg color-or-color-index &optional (shading 100.0) dash thickness-arg-is-a-fix)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (access-*line-styles-array* thickness-arg
			      (typecase color-or-color-index
				(number (aref *colorizing-colors* color-or-color-index))
				(t color-or-color-index))
			      shading dash thickness-arg-is-a-fix))

(proclaim '(notinline access-*line-styles-array*-for-segments-fast))
(defun access-*line-styles-array*-for-segments-fast (thickness-arg color shading-argument color-index)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (values schema))
  (access-*line-styles-array*-fast thickness-arg color shading-argument color-index))

(defvar *debug-access-*line-styles-array*-for-segment-voltage* nil)

(proclaim '(inline access-*line-styles-array*-for-segment-voltage))
(defun access-*line-styles-array*-for-segment-voltage (&optional (thickness-arg 0) segment)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum thickness-arg))
  (let ((ok-voltage-arg (color-styles-voltage-index (the df (if segment (get-segment-voltage-2 segment) 0.0d0)))))
    (if	(<= 0 thickness-arg (the fn *CELL-COLOR-LINE-STYLES-THICKNESS-DIMENSION-MAX-INDEX*))
      (aref *cell-color-line-styles* thickness-arg ok-voltage-arg)
      (let ((color (not-inline-get-opal-variable-color-segment segment)))
	(create-instance nil opal:line-style
			 (:join-style :round)
			 (:line-thickness (max 0 thickness-arg))
			 (:line-style :SOLID)
			 (:color color)
			 (:foreground-color color))))))

(defun not-inline-get-opal-variable-color-segment (segment) (get-opal-variable-color (s-flt (if segment (get-segment-voltage-2 segment) 0.0d0))))

(proclaim '(inline access-*line-styles-array*-for-soma-voltage))
(defun access-*line-styles-array*-for-soma-voltage (soma thickness-arg)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  opal:default-line-style)

(proclaim '(notinline access-*fill-styles*-for-soma-voltage))
(defun access-*fill-styles*-for-soma-voltage (soma)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (aref *cell-fill-styles* (color-styles-voltage-index (get-soma-voltage-df soma))))



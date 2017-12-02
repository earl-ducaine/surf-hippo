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



;;; SYS Source file: 3dplot.lisp

;;; The graphics file for 3d plots. Eventually want to make this independent of SURF-HIPPO.

(IN-PACKAGE "SURF-HIPPO")

(defun 3dplot (array &key (theta 0.0) (phi 0.0) (scale 3.0) (gain 1.0) (aspect 1.0)
	       (x-offset 0.0) (y-offset 0.0) (grid-size 100.0) grid-size-x grid-size-y
	       (width 500) (height 500) comment (title "3D Plot") win (color 'black) filled (halftone-percent 20))
  "Plot a 2 dimensional ARRAY of single floats. Viewing angles THETA and PHI are in degrees."
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (ignore width height ))
  (let* ((grid-size-x (s-flt (or grid-size-x grid-size)))
	 (grid-size-y (s-flt (or grid-size-y grid-size)))
	 (scale (s-flt scale))
	 (theta (s-flt theta))
	 (phi (s-flt phi))
	 (x-offset (s-flt x-offset))
	 (y-offset (s-flt y-offset))
	   
	 (win (setup-3dplot-win win title array grid-size-x grid-size-y grid-size theta phi scale))
	 (amplitude (- (the sf (2d-array-max array)) (the sf (2d-array-min array))))
	 (max-x-y-sides (the sf (max (* (the sf grid-size-x) (array-dimension array 0))
				     (* (the sf grid-size-y) (array-dimension array 1)))))
	 (gain (s-flt (case (g-value win :vertical-scale-constraint)
			(:gain gain)
			(:aspect (/ (* (the sf aspect) max-x-y-sides) amplitude))
			(t 0.0))))
	 (aspect (s-flt (case (g-value win :vertical-scale-constraint)
			  (:gain (/ (* gain amplitude) max-x-y-sides))
			  (:aspect aspect)
			  (t 0.0))))
	 (temp-array (make-array (list (array-dimension array 0) (array-dimension array 1))))
	 (line-style (pick-thickness 1))
	 (filling-style (color-to-fill color (cond (halftone-percent halftone-percent)
						   (filled 100)
						   (t 0))))
	 (x-dimension (array-dimension array 0))
	 (y-dimension (array-dimension array 1)))
    (declare (single-float amplitude grid-size-x grid-size-y)
	     (fixnum x-dimension y-dimension)
	     (single-float grid-size x-offset y-offset scale aspect gain))
    (s-value win :gain gain)
    (s-value win :aspect aspect)
    (set-3dplot-temp-array win array temp-array x-dimension y-dimension x-offset y-offset grid-size-x grid-size-y scale gain)

    (s-value win :visible t) ; Need this here to make virtual agg work right.
					;    (opal:update win)
    (let ((v-agg (make-v-agg virtual-polyline
			     (load-virtual-polyline-item-array array temp-array line-style filling-style win)
			     nil)))
      (virtual-agg-finishing v-agg (g-value win :aggregate))
      (s-value win :visible t) ; Need this here to make virtual agg work right.
      (draw-3d-axises win)
      (when comment (add-comment win comment :update nil)))
    (resurrect-opal-win win)
    win))

(defun set-3dplot-temp-array (win array temp-array x-dimension y-dimension x-offset y-offset grid-size-x grid-size-y scale gain)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum x-dimension y-dimension)
	   (single-float x-offset y-offset scale gain grid-size-x grid-size-y))
  (let ((x 0) (y 0))
    (declare (fixnum x y))
    (loop for xy fixnum from 0 to (1- (the fn (* x-dimension y-dimension))) do
	  (setq x (mod xy x-dimension)
		y (floor (/ xy x-dimension)))
	  do (setf (aref temp-array x y) (get-win-view-plane-x-y-from-x-y-z (+ x-offset (* grid-size-x x))
									    (* gain (the sf (aref array x y)))
									    (+ y-offset (* grid-size-y y)) win))

	  maximize (the sf (car (aref temp-array x y))) into x-max single-float
	  minimize (the sf (car (aref temp-array x y))) into x-min single-float 
	  maximize (the sf (cadr (aref temp-array x y))) into y-max single-float
	  minimize (the sf (cadr (aref temp-array x y))) into y-min single-float

	  finally (reset-histology-xfrm win (* 0.5 (+ x-max x-min)) (* 0.5 (+ y-max y-min))
					(* 1.3 (max (the sf *minimum-cell-histo-x-span*) (- x-max x-min)))
					(+ (* *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA* scale) ; a little extra height
					   (* 1.3 (max (the sf *minimum-cell-histo-y-span*) (- y-max y-min))))
					scale))))
(defun 3dplot-menu (win)
  (let* ((dummy1 (float (rad-to-deg (g-value win :phi))))
	 (dummy2 (float (rad-to-deg (g-value win :theta))))
	 (dummy3 (g-value win :aspect))
	 (dummy4 (g-value win :grid-size-x))
	 (dummy5 (g-value win :grid-size-y))
	 (dummy6 (g-value win :gain))
	 (dummy7 (or (g-value win :vertical-scale-constraint) :aspect))
	 (dummy8 (g-value win :scale)) (dummy12 (g-value win :title)) dummy20
	 dummy27)
    (choose-variable-values
     '((dummy2 "Viewing angle theta [degrees]" :number)
       (dummy1 "Viewing angle phi [degrees]" :number)
       (dummy8 "Drawing scale (microns/pixel):" :number)
       (dummy7 "Relative vertical/horizontal scale constrained by:" :choose (:aspect :gain))
       (dummy3 "Vertical/Horizontal aspect ratio" :number)
       (dummy6 "Vertical scale gain" :number)
       (dummy4 "Grid size, X [microns]" :number)
       (dummy5 "Grid size, Y [microns]" :number)
       (dummy12 "Title" :string) (dummy27 "Edit miscellaneous parameters" :boolean)
       (dummy20 "CANCEL (Do not replot)" :boolean))
     :label (format nil "Plot Menu for ~A" (g-value win :title)))
    (s-value win :title dummy12)
    (unless dummy20
      (s-value win :vertical-scale-constraint dummy7)
      (when dummy27
	(let ((dummy1 nil) (dummy2 nil) (dummy22 nil) dummy17 dummy26
					; (dummy10 (g-value win :x-label))	 (dummy11 (g-value win :y-label))
	      )
	  (choose-variable-values
	   '(				; (dummy10 "X axis label" :string) (dummy11 "Y axis label" :string)
	     
	     (dummy22 "Change label font" :boolean)
	     )
	   :label (format nil "More Plot Parameters for ~A" (g-value win :title)))
					;	  (s-value win :x-label dummy10) (s-value win :y-label dummy11)
	  (cond-every
	   (dummy22 (s-value win :font (s-value win :plot-axis-font (font-menu (g-value win :plot-axis-font) (format nil "Plot axis font for ~A" (g-value win :title)))))))))
      (s-value win :comment-font (g-value win :plot-axis-font))
      (3dplot (g-value win :2darray) :phi dummy1 :theta dummy2 :scale dummy8
	      :aspect dummy3
	      :gain dummy6 :title dummy12
	      :grid-size-x dummy4
	      :grid-size-y dummy5
	      :win win))))

(defun setup-3dplot-win (win title array grid-size-x grid-size-y grid-size theta phi scale)
  (unless win
    (let ((temp (opal-obj-exists *standard-graphics-output*)) *circuit-drawn*)
					; (setq win (get-histology-window 'histology title))
      (setq win (get-plot-window :3dplot title nil :mode :3dplot))
      (setq *standard-graphics-output* temp)))
  (set-histology-window-angle-scale-parameters win (deg-to-rad theta) (deg-to-rad phi) scale)
  (remove-all-cross-hairs win)
  (remove-virtual-aggs win)    
  (add-temp-comment win "")
  (s-value win :plot-axis-font *plot-axis-font*)
  (s-value win :grid-size grid-size)
  (s-value win :grid-size-x grid-size-x)
  (s-value win :grid-size-y grid-size-y)
  (s-value win :2darray array)
  (s-value win :mode :3dplot)
  (unless (g-value win :vertical-scale-constraint) (s-value win :vertical-scale-constraint :aspect))
  (create-instance nil window-menu-Interactor (:Window win) (:final-function #'ph::plot-window-menu-inter-function))
  win)

(defun load-virtual-polyline-item-array (array temp-array line-style filling-style win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (loop for x fixnum from 0 to (the fn (1- (array-dimension array 0))) do
	(loop for y fixnum from 0 to (the fn (1- (array-dimension array 1))) do
	      (setf (aref temp-array x y) (x-y-cons-histology-win (aref temp-array x y) win))))
  (list-to-array
   (loop for y fixnum from 0 to (the fn (- (array-dimension array 1) 2)) by 1 nconcing 
	 (loop for x fixnum from 0 to (the fn (- (array-dimension array 0) 2)) by 1 
	       collect (list-to-array `((,(aref temp-array x y) ,(aref temp-array (1+ x) y) ,(aref temp-array (1+ x) (1+ y)) ,(aref temp-array x (1+ y)) ,(aref temp-array x y))
					,line-style ,filling-style))))))

(defun load-virtual-polyline-item-array (array temp-array line-style filling-style win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (loop for x fixnum from 0 to (the fn (1- (array-dimension array 0))) do
	(loop for y fixnum from 0 to (the fn (1- (array-dimension array 1))) do
	      (setf (aref temp-array x y) (x-y-cons-histology-win (aref temp-array x y) win))))
  (list-to-array
   (loop for y fixnum from 0 to (the fn (- (array-dimension array 1) 2)) by 1 nconcing 
	 (loop for x fixnum from 0 to (the fn (- (array-dimension array 0) 2)) by 1 
	       collect (list-to-array (list `(,(aref temp-array x y)
					      ,(aref temp-array (1+ x) y)
					      ,(aref temp-array (1+ x) (1+ y))
					      ,(aref temp-array x (1+ y))
					      ,(aref temp-array x y))
					    line-style filling-style))))))

(defun load-virtual-polyline-item-array (array temp-array line-style filling-style win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (loop for x fixnum from 0 to (the fn (1- (array-dimension array 0))) do
	(loop for y fixnum from 0 to (the fn (1- (array-dimension array 1))) do
	      (setf (aref temp-array x y) (x-y-cons-histology-win (aref temp-array x y) win))))
  (list-to-array
   (loop for y fixnum from 0 to (the fn (- (array-dimension array 1) 2)) by 1 nconcing 
	 (loop for x fixnum from 0 to (the fn (- (array-dimension array 0) 2)) by 1 
	       collect (list-to-array (list (concatenate 'list
							 (aref temp-array x y)
							 (aref temp-array (1+ x) y)
							 (aref temp-array (1+ x) (1+ y))
							 (aref temp-array x (1+ y))
							 (aref temp-array x y))
					    line-style filling-style))))))

(defun draw-3d-axises (&optional win)
  (when win 
    (let* ((data-array (g-value win :2darray))
	   (agg (clear-and-add-plot-agg win `axises :add t :where :front))
	   (gain (g-value win :gain))
	   (max (* gain (array-max data-array)))
	   (min (* gain (array-min data-array)))
	   (grid-size-x (g-value win :grid-size-x))
	   (grid-size-y (g-value win :grid-size-y))
	   (grid-dimension-x (1- (array-dimension data-array 0)))
	   (grid-dimension-y (1- (array-dimension data-array 1)))
	   (grid-max-corners (list (list 0.0 max 0.0)
				   (list 0.0 max (* grid-size-y grid-dimension-y))
				   (list (* grid-size-x grid-dimension-x) max (* grid-size-y grid-dimension-y))
				   (list (* grid-size-x grid-dimension-x) max 0.0)))
	   (grid-min-corners (list (list 0.0 min 0.0)
				   (list 0.0 min (* grid-size-y grid-dimension-y))
				   (list (* grid-size-x grid-dimension-x) min (* grid-size-y grid-dimension-y))
				   (list (* grid-size-x grid-dimension-x) min 0.0))))
      (loop for grid-max-corner in grid-max-corners
	    for grid-min-corner in grid-min-corners
	    do (add-line (get-win-view-plane-x grid-max-corner win) (get-win-view-plane-y grid-max-corner win)
			 (get-win-view-plane-x grid-min-corner win) (get-win-view-plane-y grid-min-corner win)
			 agg :thickness 1.0 :color 'red :where :front))
      (loop for corners in (list grid-max-corners grid-min-corners) do
	    (do ((corner (cons (car (last corners)) corners) (cdr corner)))
		((null (cdr  corner)))
	      (add-line (get-win-view-plane-x (car corner) win)	(get-win-view-plane-y (car corner) win)
			(get-win-view-plane-x (cadr corner) win) (get-win-view-plane-y (cadr corner) win)
			agg :thickness 1.0 :color 'red :where :front))))))

(defun 3d-histo-plot (x-data x-incs y-data y-incs &key (base-offset 0) (increment 1)
		      x-min x-max y-min y-max (x-label "X") (y-label "Y")
		      win (title "Histogram") comment width height
		      (theta 30.0) (phi 30.0) (scale 3.0) (gain 1.0) (aspect 1.0)
		      (x-offset 0.0) (y-offset 0.0) (grid-size 100.0) grid-size-x grid-size-y
		      (color 'black) filled (halftone-percent 20))
  (declare (ignore x-label y-label))
  (3dplot (make-2d-histo-array x-data y-data  x-incs y-incs :increment increment
			       :x-max x-max :x-min x-min
			       :y-max y-max :y-min y-min
			       :base-offset base-offset)
	  :win win :width width :height height :title title :comment comment
	  :theta theta :phi phi :scale scale :gain gain :aspect aspect
	  :x-offset x-offset :y-offset y-offset :grid-size grid-size :grid-size-x grid-size-x :grid-size-y grid-size-y
	  :color color :filled filled :halftone-percent halftone-percent))



(defun plot-xy-contours ()
  (loop for score from -10 to 10 by 2 collect
        (loop for x from -10 to 10  by 0.5
	      unless (zerop x) collect x into xs and collect (/ score x) into ys
	      finally (return (list xs ys)))
        into xs-ys
	collect (loop for x from -10 to 10  by 0.5
		      unless (zerop x) collect x into xs and collect (/ score x) into ys
		      finally (return (list ys  xs)))
	into xs-ys
	finally (plot-xy-data xs-ys	; labels
			      '() :label-traces nil
			      :scatter t :connect-data-points nil
			      :line-styles varying-width
			      :width 500 :height 500 :y-max 10 :x-max 10 :y-min -10 :x-min -10)))


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


;;; SYS Source file: cell-graphics-hack-1.lisp
(IN-PACKAGE "SURF")


(defun refresh-resized-histology-window (win)
  (s-value win :adjust-histology-window :fix)
  (just-draw :win win))

(defun reset-valid-histology-p-in-current-histology-windows () (loop for win in (windows-of-mode :histology) do (s-value win :valid-histology-p nil)))

(defun valid-histology-windows () (loop for win in (windows-of-mode :histology) when (gv win :valid-histology-p) collect win))

(create-instance 'histology-window
		 basic-graphics-window
		 (:title "SH Histology") (:icon-title "SH Histology")
		 (:mode :histology)
		 (:background-color (symbol-to-opal-color *default-histology-window-background-color*))
		 (:resize-window-refresh-function 'refresh-resized-histology-window) ; This function must have a single required arg, which is a plot window.
		 (:x-units "um") (:y-units "um")
		 (:valid-histology-p t)	; Reset this for existing windows when a new circuit is loaded.
		 (:soma-outline-p t)
		 (:soma-histology-fixed-diameter-p nil)
		 (:marker-diameter *default-marker-diameter*)
		 (:soma-histology-fixed-diameter-pixels 10)

		 ;; ** Histology graphics slots. **

		 ;; Creating 2D projections from 3D structures and then displaying the projections onto the histology window is a
		 ;; two part process, first determining the 2D projection of the tissue and then placing a translated/rotated
		 ;; version of the projection onto the graphics window.

		 ;; The anatomical information for each cell component is referenced to an XYZ coordinate system, with an implicit
		 ;; origin at (0,0,0). The viewing plane is referred to as the X'Y' plane, and the default orientation of this
		 ;; plane is such that X->X' and Y->Y'. The orientation of the XYZ system with respect a given brain structure is
		 ;; arbitrary, but as a general rule is taken so that the typical anatomic/experimental view is reflected in the
		 ;; default X'Y' projection.  Typically, this means that Z is taken as the depth within tissue, oriented with
		 ;; respect to 2-dimensional sheets in brain. I.e. for retina, the XY plane is congruent with the plane of the
		 ;; retina, with the Z axis aligned along the radial dimension (Z=0 at the ILM [for example] and increasing in the
		 ;; distal direction).  This means that the default X'Y' orientation corresponds to the retinal whole mount, or
		 ;; retinotopic orientation. For flattened cortex, the XY plane is congruent with the surface of the brain. For
		 ;; hippocampal slice, the XY plane is in the plane of the slice, since the slice is typically viewed
		 ;; "en-face". This is also the system for cortical neurons that are typically viewed perpendicular to the
		 ;; cortical surface.

		 ;; Two dimensional projections onto the X'Y' viewing plane are taken as follows.  Assume that the XYZ coordinates
		 ;; are placed with the XZ plane in the horizontal direction, the Z axis emerging from the page and the X axis
		 ;; pointing to the right in the plane of the page. The Y axis points up in the plane of the page.  Start with
		 ;; THETA = PHI = 0, where the X'Y' viewing plane is congruent with the XY plane. As the X'Y' plane is rotated
		 ;; about the Y' axis (now = Y axis), the azimuth angle PHI is the angle between the X and X' axises. Next, the
		 ;; X'Y' plane is rotated around the X' axis, and the elevation angle THETA is the angle between the Y and Y'
		 ;; axises. When a structure is drawn, the THETA and PHI values for the appropriate window are used to generate
		 ;; the 2D representation of each element in the structure.

		 ;; The X'Y' viewing plane projection can then be translated and rotated in a second transformation before the
		 ;; actual drawing. This transformation is done with a 3x3 transformation matrix slot for each window, and allows
		 ;; the SURF-HIPPO drawing functions to refer to data coordinates, ie microns, with the default putting the
		 ;; anatomical origin at the center of the window.

		 (:scale 3.0)		;microns per pixel
		 (:current-xfrm nil)
		 (:current-xfrm-rotates nil) ; so we can save some effort on transforms
		 (:current-xfrm-0-0 0.0) (:current-xfrm-0-1 0.0) (:current-xfrm-0-2 0.0)
		 (:current-xfrm-1-0 0.0) (:current-xfrm-1-1 0.0) (:current-xfrm-1-2 0.0)
		 (:current-xfrm-2-0 0.0) (:current-xfrm-2-1 0.0) (:current-xfrm-2-2 0.0)

		 (:view-angle-comment-p t)

		 (:dummy-xfrm1 nil) (:dummy-xfrm2 nil)

		 (:grape-size-reference :pixel)
		 (:grape-size-microns *GRAPE-SIZE-microns*) (:grape-size-pixels *GRAPE-SIZE*)

		 (:adjust-histology-window :automatic)

		 ;; These values are in the coordinate frame of the window, ie the projection of the
		 ;; possibly rotated contents.
		 (:max-x-contents 0.0)	;Maximum X value (microns) for the contents of the window.
		 (:max-y-contents 0.0)	;Maximum Y value (microns) for the contents of the window.
		 (:min-x-contents 0.0)	;Minimum X value (microns) for the contents of the window.
		 (:min-y-contents 0.0)	;Minimum Y value (microns) for the contents of the window.

		 (:draw-electrodes t)

		 (:use-connection-midpoints t) ; for axons and synapse connections
		 (:where-element-markers-go :front)
		 (:where-somas-go :front) (:draw-anatomical-soma t)
		 (:node-label-background-p t)
		 (:visible-histology-scale-bar-sides t)
		 (:include-histology-scale-bar-label t)
		 (:colorize nil)
		 (:show-time nil)	; Display the value of *real-time* in the upper left corner.
		 (:include-colorizing-scale t))

#|
(s-value histology-window :default-graphics-filling-style (o-formula (if (eql opal::black (gvl :background-color)) opal::white-fill opal::black-fill)))
(s-value wh::window-hack-text :default-line-style (o-formula (or (gvl :parent :window :default-line-style)
								 (let ((color (gvl :default-graphics-color)))
								   (create-instance nil opal:default-line-style (:constant nil) (:foreground-color color))))))
(s-value histology-window :default-graphics-background (o-formula (if (eql opal::black (gvl :background-color))	opal::black-fill opal::white-fill )))
|#

(defun histology-x-graphics-win-function (x win) (round x))
(defun histology-y-graphics-win-function (y win) (round y))

(s-value histology-window :x-graphics-win-function #'histology-x-graphics-win-function)
(s-value histology-window :y-graphics-win-function #'histology-y-graphics-win-function)

(defun transfer-current-xfrm-to-xfrm-slots (win)
  (s-value win :current-xfrm-0-0 (aref (gv win :current-xfrm) 0 0))
  (s-value win :current-xfrm-0-1 (aref (gv win :current-xfrm) 0 1))
  (s-value win :current-xfrm-0-2 (aref (gv win :current-xfrm) 0 2))
  (s-value win :current-xfrm-1-0 (aref (gv win :current-xfrm) 1 0))
  (s-value win :current-xfrm-1-1 (aref (gv win :current-xfrm) 1 1))
  (s-value win :current-xfrm-1-2 (aref (gv win :current-xfrm) 1 2))
  (s-value win :current-xfrm-2-0 (aref (gv win :current-xfrm) 2 0))
  (s-value win :current-xfrm-2-1 (aref (gv win :current-xfrm) 2 1))
  (s-value win :current-xfrm-2-2 (aref (gv win :current-xfrm) 2 2))
  (s-value win :current-xfrm-rotates (not (and (= (gv win :current-xfrm-0-0) 1.0)
					       (= (gv win :current-xfrm-1-0) 0.0)
					       (= (gv win :current-xfrm-0-1) 0.0)
					       (= (gv win :current-xfrm-1-1) 1.0)))))

(defmacro histo-interactor-wrapper (body)
  `(let ((window (if (consp (gv interactor :window))
		     (car (gv interactor :window))
		     (gv interactor :window)))
	 *automatic-run*)
    (unless (INTERACTORS-RUNNING window)
      (s-value window :window-menu-interactor-running t)
      (when (consp window) (loop for win in window when (gv win :mode) do (setq window win)))
      ,body
      (when (opal-obj-exists window) (s-value window :window-menu-interactor-running nil)))))

(defun histology-erase-temp-comment-inter-function (interactor final-obj-over)
  (histo-interactor-wrapper
   (progn
     (wh::erase-temp-comment-inter-function interactor final-obj-over)
     (clear-plot-aggs window `colored-node-agg))))

(defun reset-histology-scale-bar-um-length (win)
  (when (gv win :auto-histology-scale-bar-um-length)
    (s-value win :histology-scale-bar-um-length
	     (let ((um-width (* (gv win :width) (gv win :scale))))
	       (cond
		 ((IN-BTWN 2 um-width 5) 1)
		 ((IN-BTWN 5 um-width 10) 2)
		 ((IN-BTWN 10 um-width 25) 5)
		 ((IN-BTWN 25 um-width 100) 10)
		 ((IN-BTWN 100 um-width 200) 20)
		 ((IN-BTWN 200 um-width 500) 40)
		 ((IN-BTWN 500 um-width 1000) 100)
		 ((IN-BTWN 1000 um-width 5000) 500)
		 ((IN-BTWN 5000 um-width 20000) 2000)
		 (t (truncate (/ um-width 4))))))))

(create-instance 'scale-bar opal:aggregadget
  (:type 'histology-scale-bar)
  (:label t)
  (:visible (o-formula (gvl :window :include-scale-bar)))
  (:side-height-in-pixels 10)
  (:y (o-formula (- (the fn (gvl :window :height)) *histology-scale-bar-bottom*)))
  (:y-top (o-formula (round (- (gvl :y) (* 0.5 (gvl :side-height-in-pixels))))))
  (:y-bottom (o-formula (round (+ (gvl :y) (* 0.5 (gvl :side-height-in-pixels))))))
  (:length-in-pixels (o-formula (round (/ (gvl :parent :window :histology-scale-bar-um-length) (the sf (gvl :parent :window :scale))))))
  (:x-left 20)
  (:x-right (o-formula (+ (gvl :length-in-pixels) (gvl :x-left))))
  (:line-style (o-formula (or (gvl :window :default-line-style) OPAL:DEFAULT-LINE-STYLE)))
  (:parts
   (let ((line-style `(o-formula (gvl :parent :line-style)))
	 (visible-sides `(o-formula (gvl :parent :window :visible-histology-scale-bar-sides)))
	 (y `(o-formula (gvl :parent :y)))
	 (x-right `(o-formula (gvl :parent :x-right)))
	 (x-left `(o-formula (gvl :parent :x-left)))
	 (y-bottom `(o-formula (gvl :parent :y-bottom)))
	 (y-top `(o-formula (gvl :parent :y-top))))
     `((:histology-scale-bar-background ,opal:rectangle
					(:visible t) (:line-style nil)
					(:filling-style ,(o-formula (or (gvl :parent :window :default-graphics-background) opal::white-fill)))
					(:left 10)
					(:height ,(o-formula (- (gvl :parent :histology-scale-bar-label :top) (gvl :parent :y-bottom))))
					(:width ,(o-formula (+ (gvl :parent :length-in-pixels) 20)))
					(:top ,(o-formula (- (the fn (gvl :parent :histology-scale-bar-label :top)) 5))))
       (:histology-scale-bar-body ,opal:line (:visible t) (:line-style ,(eval line-style)) (:x1 ,(eval x-left)) (:x2 ,(eval x-right)) (:y1 ,(eval y)) (:y2 ,(eval y)))
       (:histology-scale-bar-left-side
	,opal:line (:visible ,(eval visible-sides)) (:line-style ,(eval line-style)) (:x1 ,(eval x-left)) (:x2 ,(eval x-left)) (:y1 ,(eval y-top)) (:y2 ,(eval y-bottom)))
       (:histology-scale-bar-right-side
	,opal:line (:visible ,(eval visible-sides)) (:line-style ,(eval line-style)) (:x1 ,(eval x-right)) (:x2 ,(eval x-right)) (:y1 ,(eval y-top)) (:y2 ,(eval y-bottom)))
       (:histology-scale-bar-label ,opal:text
				   (:visible t)
				   (:line-style ,(eval line-style))
				   (:top ,(o-formula (round (- (gvl :parent :y-top) (gvl :height) (* 0.5 (gvl :line-style :line-thickness)) 2))))
				   (:string ,(o-formula (if (gvl :parent :window :include-histology-scale-bar-label)
							    (format nil "~d ~A"
								    (let ((um-length (gvl :parent :window :histology-scale-bar-um-length)))
								      (if (gvl :parent :window	:fix-histology-scale-bar-length)
									  (round um-length) um-length))
								    *histology-scale-bar-unit-string*)
							    "")))
				   (:left ,(o-formula (round (+ (gvl :parent :x-left) (/ (- (gvl :parent :x-right) (gvl :parent :x-left) (gvl :width)) 2)))))
				   (:font ,(o-formula (gvl :parent :window :font))))))))

#|
(let ((line-style `(eval (o-formula (gvl :parent :line-style)))))
  `((:histology-scale-bar-body ,opal:line (:visible t) (:line-style line-style)))))
|#

(defun add-histology-scale-bar (win)
  (reset-histology-scale-bar-um-length win)
  (opal:add-component (gv win :aggregate) (create-instance nil scale-bar) :where :front))

(defun add-rest-of-histology-stuff (win)
  (add-histology-scale-bar win)
  (add-histology-interactors win))

(defun get-histology-window (&optional (histology-pane-type 'histology) name &key width height
				       exclude-auxiliary-type (child-number 0) create-new-win prototype increment-version)
  (let ((win (unless (or prototype create-new-win) (find-histology-window histology-pane-type name :exclude-auxiliary-type exclude-auxiliary-type)))
	(version (if prototype (gv prototype :version) "")))
    (when (and exclude-auxiliary-type win (eq (gv win :auxiliary-type) exclude-auxiliary-type))
      (setq win nil))
    (cond-every (width (setq width (min width (round (* 0.9 gem::*screen-width*)))))
		(height (setq height (min height (round (* 0.9 gem::*screen-height*))))))
    (unless win
      (setq win (create-histology-window :width width :height height :type histology-pane-type :prototype prototype))
      (s-value win :title (let ((base-title (concatenate-strings (string (or name histology-pane-type)) (when (> (length version) 0) "-") version)))
			    (if prototype base-title (GET-win-TITLE-STRING base-title))))
      (setq version (if increment-version (increment-version-letter version) version))
      (s-value win :version version)
      (add-rest-of-histology-stuff win))
    (RESET-HISTOLOGY-SCALE-BAR-UM-LENGTH win)
    (s-value win :child-number child-number)
    (when width (s-value win :width (COERCE-TO-EVEN-INT width)))
    (when height (s-value win :height (COERCE-TO-EVEN-INT height)))
    (setq *circuit-drawn* nil
	  *standard-graphics-output* win)))

(defun find-histology-window (histology-pane-type &optional name &key exclude-auxiliary-type)
  (loop for window in (clean-up-*output-windows*)	; Is there already the right kind of window?
	when (and (not (gv window :locked))
		  (eq :histology (gv window :mode))
		  (and (or (string-equal histology-pane-type (gv window :type))
			   (eq histology-pane-type (gv window :type)))
		       (not (and exclude-auxiliary-type
				 (eq (gv window :auxiliary-type) exclude-auxiliary-type)))
		       (if name (string-equal name (gv window :title)) t)))
	do (return (setq *standard-graphics-output* window))))

(defun create-histology-window (&key width height type prototype background-color)
  (let* ((win (create-instance nil histology-window
			       (:icon-title "SH Histo")
			       (:include-colorizing-scale *enable-colorize-scale*)
			       (:show-time *enable-colorize-time*)
			       (:colorize *colorize-simulation*)
			       (:type type)
			       (:where-synapse-stimulus-goes *WHERE-SYNAPSE-STIMULUS-GOES*)
			       (:background-color (extract-color background-color *default-histology-window-background-color*))
			       (:axon-color 'red)
			       (:axon-color-shading 100.0)
					; (:segment-default-color 'black)
			       (:segment-color-shading 100.0)
			       (:current-xfrm (3-by-3-identity)) ; Load with the identity matrix
			       (:dummy-xfrm1 (make-array (list 3 3) :initial-element 0.0 :element-type 'single-float))
			       (:dummy-xfrm2 (make-array (list 3 3) :initial-element 0.0 :element-type 'single-float)))))
    (when prototype
      (unless width (setq width (gv prototype :width)))
      (unless height (setq height (gv prototype :height))))
    (create-histology-window-setup win width height)
    (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS win)
    (INIT-HISTOLOGY-WINDOW-DRAW-SLOTS win prototype)
    (when prototype
      (s-value win :cells (gv prototype :cells))
      (set-histology-window-angle-scale-parameters win (gv prototype :theta) (gv prototype :phi) (gv prototype :scale)))
    (add-view-angle-comment win)
    (s-value win :last-drawn-width (gv win :width))
    (s-value win :last-drawn-height (gv win :height))
    (update-*Twin*)
    (setq *standard-graphics-output* win)))

(defun create-aux-histology-window (&key width height title (auxiliary-type :auxiliary))
  (let ((win (create-instance nil histology-window (:icon-title "SH Histo-aux"))))
    (create-histology-window-setup win width height)
    (when title (s-value win :title (string title)))
    (s-value win :auxiliary-type auxiliary-type)
    win))

(defun create-histology-window-setup (win width height)
  (cond-every (width (s-value win :width (setq width (min width (round (* 0.9 gem::*screen-width*))))))
	      (height (s-value win :height (setq height (min height (round (* 0.9 gem::*screen-height*)))))))
  (s-value win :aggregate (create-instance nil opal:aggregate
					   (:top (o-formula (gvl :window :top)))
					   (:left (o-formula (gvl :window :left)))
					   (:width (o-formula (gvl :window :width)))
					   (:height (o-formula (gvl :window :height)))))
  (s-value (gv win :aggregate) :window win)
  (initialize-graphics-window win :width width :height height :erase-temp-comment-Interactor-final-function #'histology-erase-temp-comment-inter-function)
  (push win *output-windows*))

(defun find-histology-window-title () (if (> (length *circuit*) 0) (format nil "~A Histology" *circuit*) "Histology"))

(defun get-generic-histology-window () (get-histology-window 'histology (find-histology-window-title) :create-new-win t :exclude-auxiliary-type :keys))

(defun get-histology-child-window (parent &optional parented &key exclude-auxiliary-type)
  (let* ((parent-child-number (gv parent :child-number))
	 (child (GET-HISTOLOGY-WINDOW (gv parent :type)
				      (if parented (format nil "~a-~d" (gv parent :title) (1+ parent-child-number)) (find-histology-window-title))
				      :exclude-auxiliary-type exclude-auxiliary-type :prototype parent :increment-version (not parented))))
    (s-value child :background-color (gv parent :background-color))
    (s-value child :colorize (gv parent :colorize))
    (s-value child :chosen-one (gv parent :chosen-one))
    (s-value child :chosen-ones (gv parent :chosen-ones))
    (s-value child :marked-segments-and-colors (gv parent :marked-segments-and-colors))
    (s-value parent :child-number (if parented (1+ parent-child-number) 0))
    (setq *circuit-drawn* t
	  *standard-graphics-output* child)))

(defun view-angle-comment-p (win) (and (gv win :view-angle-comment-p) (not (and (= 0 (gv win :theta)) (= 0 (gv win :phi))))))

(defun add-view-angle-comment (win)
  (add-comment
   win (when (view-angle-comment-p win)
	 (format nil "Viewing theta/phi: ~a/~a" (round (rad-to-deg (gv win :theta))) (round (rad-to-deg (gv win :phi)))))))

(defun set-histology-window-angle-scale-parameters (win theta phi scale)
  (s-value win :phi (float phi))
  (s-value win :theta (float theta))
  (s-value win :scale (float scale))
  (ADD-VIEW-ANGLE-COMMENT win))

(defun reset-histology-xfrm (win center-x center-y width height scale)
  ;; width and height are in histology units
  (s-value win :scale (s-flt scale))
  (s-value win :center-x (s-flt center-x))
  (s-value win :center-y (s-flt center-y))
  (setf (aref (gv win :current-xfrm) 2 0) (s-flt (/ (- center-x) scale))) ; x-shift in pixels
  (setf (aref (gv win :current-xfrm) 2 1) (s-flt (/ (- center-y) scale))) ; y-shift in pixels
  (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS win)
  (when width
    (s-value win :width (COERCE-TO-EVEN-INT (min (max *histology-window-min-width* (/ width scale))
						 (if *override-screen-max*
						     (max *histology-window-min-width* (/ width scale))
						     (* 0.95 gem::*screen-width*))))))
  (when height
    (s-value win :height (COERCE-TO-EVEN-INT (min (max *histology-window-min-height* (/ height scale))
						  (if *override-screen-max*
						      (max *histology-window-min-height* (/ height scale))
						      (* 0.95 gem::*screen-height*))))))
  (RESET-HISTOLOGY-SCALE-BAR-UM-LENGTH win))

;;; (describe 'opal:get-standard-font) ->
;;; GET-STANDARD-FONT returns a font object.  If this function is called multiple times with the same font specification, the same
;;; object will be returned, thus avoiding wasted objects.
;;;    Allowed values:
;;;    family -- :fixed, :serif, :sans-serif, or NIL (NIL == :fixed)
;;;    face   -- :roman, :italic, :bold, :bold-italic, or NIL (NIL == :roman)
;;;    size   -- :small, :medium, :large, :very-large, or NIL (NIL == :medium)

(defun add-string (string x y agg &key size face family dimensions-in-pixels
			  (justification :LEFT) ; Or :RIGHT to specify right-justified text
			  x-pixel y-pixel ; Using these will supersede x, y, and dimensions-in-pixels.
			  background color background-color ;  opal:white
			  font (x-pixel-offset 0) (y-pixel-offset 0))
  ;; This adds an instance of an opal:text to aggegrate of WIN, but takes useful parameters. Note that X and Y params are in window
  ;; coordinates according to the window's :SCALE slot (e.g. microns per pixel), referenced to the shifted center of WIN, unless
  ;; DIMENSIONS-IN-PIXELS is non-NIL.
  (let* ((win (gv agg :parent :window))
	 (text (graphics-text string win :font font :color color :background-color background-color :background background :size size :face face :family family))
	 (font-height (gv text :font :font-height))
	 (win-scale (or (gv win :scale) 1.0))
	 (center-x (or (gv win :center-x) 0))
	 (center-y (or (gv win :center-y) 0))
	 (x (s-flt x)) (y (s-flt y)))
    (when dimensions-in-pixels (reset-histology-xfrm win 0.0 0.0 nil nil 1.0))
    (multiple-value-bind (x-pixel-temp y-pixel-temp)
	(x-y-histology-win-values x y win)
      (when x-pixel (setq x-pixel-temp x-pixel))
      (when y-pixel (setq y-pixel-temp y-pixel))
      (let ((left (+ x-pixel-offset x-pixel-temp (case justification
						   (:left 0)
						   (:right (- (gv text :width)))
						   (t 0)))))
	(s-value text :left left)
	(s-value text :top (+ y-pixel-offset (round (/ font-height -2)) y-pixel-temp))
	(opal:add-component agg text)))
    (when dimensions-in-pixels (reset-histology-xfrm win center-x center-y nil nil win-scale))
    text))

(defun get-add-line-line-style (color stipple-percent thickness &optional (base-line-style opal::default-line-style))
  (create-instance
   nil
   base-line-style
   (:constant '(:except :foreground-color))
   ;; (:foreground-color (when (or color stipple-percent) (get-opal-color color stipple-percent)))
   (:line-thickness thickness)
   (:stipple
    (if nil				; stipple-percent ; (or (eq style 'grey)(eq style 'blue))
      (create-instance
       nil opal:bitmap
       (:line-thickness thickness)
       (:image (opal:halftone-image stipple-percent))
       (:foreground-color color))))))

(defun add-line (x1 y1 x2 y2 agg &key dimensions-in-pixels style stipple-percent color (thickness 0) (where :front))
  ;; This adds an instance of an opal:line to AGG, but takes useful parameters, and references parameters of the AGG's window.
  ;; Note that X1, X2, Y1, Y2, and thickness params are in window coordinates according to the window's :SCALE slot (e.g. microns
  ;; per pixel), referenced to the center of window [(X,Y) = (0,0) at window center]. Data args must be a single-float.
  (declare (ignore style))
  (let* ((win (gv agg :parent :window))
	 (line-style (get-add-line-line-style color stipple-percent
					      (round (/ thickness (gv agg :parent :window :scale)))
					      (graphics-text-linestyle (when (or color stipple-percent) (get-opal-color color stipple-percent)) win)))
	 (win-scale (or (gv win :scale) 1.0))
	 (center-x (or (gv win :center-x) 0))
	 (center-y (or (gv win :center-y) 0))
	 (x1 (s-flt x1)) (y1 (s-flt y1))
	 (x2 (s-flt x2)) (y2 (s-flt y2)))
    (when dimensions-in-pixels (reset-histology-xfrm win 0.0 0.0 nil nil 1.0))
    (multiple-value-bind (pix-x1 pix-y1)
	(x-y-histology-win-values x1 y1 (gv agg :parent :window))
      (multiple-value-bind (pix-x2 pix-y2)
	  (x-y-histology-win-values x2 y2 (gv agg :parent :window))
	(let ((line (create-instance nil opal:line
		      (:constant '(t))
		      (:x1 pix-x1) (:y1 pix-y1) (:x2 pix-x2) (:y2 pix-y2)
		      (:draw-function :copy) (:line-style line-style))))
	  (opal:add-component agg line :where where)
	  (when dimensions-in-pixels (reset-histology-xfrm win center-x center-y nil nil win-scale))
	  line)))))

(defun add-circle (x y radius agg &key filled color halftone-percent dimensions-in-pixels (line-style opal:thin-line) (where :front) (drawing-function :src))
  ;; This adds an instance of an opal:circle to the aggregate of WIN, but takes useful parameters. Note that X and Y params are in
  ;; window coordinates according to the window's :SCALE slot (e.g. microns per pixel), referenced to the center of WIN [(X,Y) =
  ;; (0,0) at WIN center].
  (let* ((filling-style (color-to-fill color (cond (halftone-percent halftone-percent)
						   (filled 100)
						   (t 0))))
	 (win (gv agg :parent :window))
	 (win-scale (or (gv win :scale) 1.0))
	 (center-x (or (gv win :center-x) 0))
	 (center-y (or (gv win :center-y) 0))
	 (x (s-flt x)) (y (s-flt y))
	 )
    (when dimensions-in-pixels
      (reset-histology-xfrm win 0.0 0.0 nil nil 1.0))
    (multiple-value-bind (pix-x pix-y)
	(x-y-histology-win-values x y (gv agg :parent :window))
      (let* ((radius-in-pixels (round (/ radius (the sf (gv agg :parent :window :scale)))))
	     (circle
	      (opal:add-component
	       agg
	       (create-instance nil opal:circle
		 (:constant t)
		 (:left (- pix-x radius-in-pixels))
		 (:top (- pix-y radius-in-pixels))
		 (:height (+ radius-in-pixels radius-in-pixels))
		 (:width (+ radius-in-pixels radius-in-pixels))
		 (:line-style line-style)
		 (:filling-style filling-style))
	       :where where)))
	(when dimensions-in-pixels
	  (reset-histology-xfrm win center-x center-y nil nil win-scale))
	circle))))

(create-instance 'dynamic-polyline opal:polyline
		 (:static-line-style opal:default-line-style)
		 (:line-style (o-formula (if (and (gvl :parent :window) (gvl :parent :window :colorize) (gvl :element))
					   (ACCESS-*LINE-STYLES-ARRAY*-FOR-SOMA-VOLTAGE (gvl :element) (gvl :static-line-style :line-thickness))
					   (gvl :static-line-style))))
		 (:element nil)
		 (:static-fill (color-to-fill 'black))
		 (:filling-style (o-formula (if (and (gvl :parent :window) (gvl :parent :window :colorize) (gvl :element))
					      (access-*fill-styles*-for-soma-voltage (gvl :element))
					      (gvl :static-fill)))))

(defun add-polyline (x-y-points agg &key filled color halftone-percent element (line-style opal:default-line-style) (where :front)
				(drawing-function :src)
				filling-style)
  ;; This adds an instance of an opal:polyline to the aggregate of WIN, but takes useful parameters. Note that the X and Y points
  ;; are in window coordinates according to the window's :SCALE slot (e.g. microns per pixel), referenced to the center of WIN
  ;; [(X,Y) = (0,0) at WIN center]. The :filling-style if given by FILLING-STYLE, if included, otherwise based on COLOR,
  ;; HALFTONE-PERCENT, and FILLED.
  (let* ((static-fill (or filling-style
			  (color-to-fill color (cond (halftone-percent halftone-percent)
						     (filled 100)
						     (t 0)))))
	 (polyline (if element
		     (create-instance nil dynamic-polyline (:element element) (:static-line-style line-style) (:static-fill static-fill))
		     (create-instance nil opal:polyline (:constant t) (:line-style line-style) (:filling-style static-fill)))))
    (s-value polyline :point-list (let (x y x-y-hist)
				    (loop while x-y-points do
					  (setq x (car x-y-points))
					  (setq x-y-points (cdr x-y-points))
					  (setq y (car x-y-points))
					  (setq x-y-points (cdr x-y-points))
					  (setq x-y-hist (x-y-histology-win (s-flt x) (s-flt y) (gv agg :parent :window)))
					  collect (car x-y-hist)
					  collect (cadr x-y-hist))))
    (opal:add-component agg polyline :where where)))

(defun add-arrow (x y length agg &key (thickness 4) ; (head-length 1.0)
				   color)
  ;; This points up, with the arrow tip at (X,Y). Note that X, Y, and LENGTH are in window coordinates according to the window's
  ;; :SCALE slot (e.g. microns per pixel), referenced to the center of WIN [(X,Y) = (0,0) at WIN center].
  (let ((x (s-flt x)) (y (s-flt y)))
    (multiple-value-bind (pix-x pix-y)
	(x-y-histology-win-values x y (gv agg :parent :window))
      (let* ((arrow (create-instance
			nil gg::arrow-line
		      (:constant nil)
		      (:open-p nil) (:filling-style (if color opal:red-fill  opal:black-fill))
		      (:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness thickness) (:foreground-color (if color opal:red opal:black))))
		      (:x1 x) (:x2 x) (:y2 y) (:y1 (round (+ y (/ length (gv agg :parent :window :scale))))))))
	(opal:add-component agg  arrow)))))

(defun graphics-window-cells (win) (no-nils (loop for cell in (gv win :cells) collect (element cell 'cell))))

(defun closest-cell-element-in-window (x y window just-somas)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((cells (graphics-window-cells window)))
    (let ((candidates (if just-somas (somas cells) (cell-elements cells)))
	 (min-distance 0.0)
	 nearest-element)
    (declare (single-float x y min-distance))
    (loop for element in candidates
       do (let* ((loc (element-absolute-location element))
		 (distance (cartesian-distance (get-win-view-plane-x loc window) (get-win-view-plane-y loc window) x y)))
	    (when (or (not nearest-element) (< distance min-distance))
	      (setq nearest-element element min-distance distance)))
       finally (return nearest-element)))))

#|
(defun builder-pointer-function (interactor obj points)
  (declare (ignore obj))
  (break)
  (histo-interactor-wrapper
   (let ((x-y (x-y-histology-win-inv (nth 2 points) (nth 3 points) window)))
     (find-and-query-nearest-node (car x-y) (cadr x-y) (gv window :segment-list)))))

(create-instance 'builder-pointer inter:move-grow-Interactor (:start-where t) (:start-event :leftdown) (:final-function #'builder-pointer-function))
|#

(defun histology-pointer-function (interactor points &optional just-somas)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (histo-interactor-wrapper
   (let* ((x-y (x-y-histology-win-inv (nth 2 points) (nth 3 points) (gv interactor :window)))
	  (x (car x-y))
	  (y (cadr x-y))
	  (x-label "um") (y-label "um")
	  (chosen-one (closest-cell-element-in-window x y window just-somas)))
     (when chosen-one
       (s-value window :last-xy x-y)
       (s-value window :chosen-one chosen-one)
       (update-running-comment window
			       (concatenate-strings
				(format nil "X: ~a ~a~%Y: ~a ~a~%"
					(tidy-number-format (car x-y) :default-decimals 1) x-label
					(tidy-number-format (cadr x-y) :default-decimals 1) y-label)
				(when chosen-one (format nil "Cell node: ~a" (element-name chosen-one)))))
       (DRAW-CHOSEN-ONE (gv window :chosen-one) window)
       ;; 11/20/94 this stops a refresh of the entire window for some reason.
       ;; (opal:update (gv interactor :window))
       ))))

(defun histology-soma-pointer-function (interactor points) (histology-pointer-function interactor points t))


(create-instance 'histology-pointer inter:two-point-Interactor
		 (:start-where t) (:start-event :leftdown) (:line-p t) (:last-xy nil) (:final-function #'histology-pointer-function))

(create-instance 'histology-soma-pointer histology-pointer
		 (:start-event '(:meta-leftdown	; Backward compatibility
				 :rightdown))
		 (:final-function #'histology-soma-pointer-function))

(create-instance 'histology-element-menu-Interactor inter:button-Interactor (:continuous nil) (:start-where t) (:start-event :shift-control-leftdown))

(defun histology-element-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (histo-interactor-wrapper
   (let ((chosen-one (gv window :chosen-one)))
     (when (and (graphics-window-cells window) chosen-one)
       (overall-element-menu chosen-one window)))))

(defun histology-window-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (histo-interactor-wrapper
   (let ((*standard-graphics-output* window))
     (case (gv window :mode)
       (:3dplot (3dplot-menu window))
       (t (drawing-menu window))))))

(defun histology-coords-running-function (interactor points)
  ;; This is used as a FINAL-FUNCTION for the histology window coords pointer, i.e.:
  ;;   (add-window-coords-pointer win #'histology-coords-pointer-result)
  ;; When middle mouse is released the xy coordinates of the last location of the cross hair is displayed in the units of the window
  ;; data. If there was a previous point so delineated, then the length between the current and last points is shown.
  (histology-coords-pointer-result-core interactor points nil))

(defun histology-coords-pointer-result (interactor points) (histology-coords-pointer-result-core interactor points t))

#|
(defun histology-coords-pointer-result-core (interactor points final)
  (histo-interactor-wrapper
   (progn
     (unless final (call-prototype-method interactor points)) ; Keep default running action.
     (let* ((x-y (x-y-histology-win-inv (nth 2 points) (nth 3 points) window))
	    (x-label "um")
	    (y-label "um")
	    (distance (when (gv window :last-pointer-xy)
			(cartesian-distance-float (car x-y) (cadr x-y) (nth 0 (gv window :last-pointer-xy)) (nth 1 (gv window :last-pointer-xy))))))
       (update-running-comment window
			       (format nil "X: ~a ~a~%Y: ~a ~a~A"
				       (tidy-number-format (car x-y) :default-decimals 1) x-label
				       (tidy-number-format (cadr x-y) :default-decimals 1) y-label
				       (if distance (format nil "~%Length: ~a um" (tidy-number-format distance)) "")))
       (when final
	 (s-value window :last-pointer-xy x-y)
	 (add-marker window points :add-cross-hair t :data-x (car x-y) :data-y (cadr x-y) :data-to-points-function #'X-Y-histology-WIN))))))
|#

(defun histology-coords-pointer-result-core (interactor points final)
  (histo-interactor-wrapper
   (progn
     (unless final (call-prototype-method interactor points)) ; Keep default running action.
     (let* ((x-y (x-y-histology-win-inv (nth 2 points) (nth 3 points) window))
	    (x-label "um")
	    (y-label "um")
	    (distance (when (gv window :last-pointer-xy)
			(cartesian-distance (car x-y) (cadr x-y) (nth 0 (gv window :last-pointer-xy)) (nth 1 (gv window :last-pointer-xy))))))
       (update-running-comment window
			       (format nil "X: ~a ~a~%Y: ~a ~a~A"
				       (tidy-number-format (car x-y) :default-decimals 1) x-label
				       (tidy-number-format (cadr x-y) :default-decimals 1) y-label
				       (if distance (format nil "~%Length: ~a um" (tidy-number-format distance)) "")))
       (when final
	 (s-value window :last-pointer-xy x-y)
	 (add-marker window points :add-cross-hair t :data-x (car x-y) :data-y (cadr x-y) :data-to-points-function #'X-Y-histology-WIN))))))

(defun mark-histology-coords-pointer-result (interactor points)
  (declare (ignore points))
  (histo-interactor-wrapper (mark-histology-coords-pointer-menu window)))

(defun mark-histology-coords-pointer-menu (win) (mark-coords-pointer-menu win))

(defun zoom (interactor point-list)
  (histo-interactor-wrapper
   (when (graphics-window-cells (gv interactor :window))
     (let* ((zoom-left (the fn (first point-list)))
	    (zoom-top (the fn (second point-list)))
	    (zoom-width (the fn (third point-list)))
	    (zoom-height (the fn (fourth point-list)))
	    ;; Need to convert the coordinates from the mouse to data coordinates.
	    (x-y-min (x-y-histology-win-inv zoom-left (+ zoom-top zoom-height) window))
	    (x-y-max (x-y-histology-win-inv (+ zoom-left zoom-width) zoom-top window))
	    ;; these are in histology units
	    (new-width (- (car x-y-max)(car x-y-min)))
	    (new-height (- (cadr x-y-max)(cadr x-y-min)))
	    (new-center-x (+ (car x-y-min) (* 0.5 new-width)))
	    (new-center-y (+ (cadr x-y-min) (* 0.5 new-height)))
	    ;; set the new scale to the minimum expansion implicit in the zoom window dimensions.
	    (new-scale (max (/ new-width *histology-zoom-window-max-width*)
			    (/ new-height *histology-zoom-window-max-height*))))
       (if (and (> new-width 5) (> new-height 5))
	   (let ((z-win (get-histology-child-window window t)))
	     (s-value z-win :adjust-histology-window :fix)
	     (reset-histology-xfrm z-win new-center-x new-center-y new-width new-height new-scale)
	     (add-temp-comment z-win "Zooming..." :update nil)
	     (draw-cells z-win t)
	     (wh::add-zoom-marker interactor)))))))

(defun unzoom (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (histo-interactor-wrapper
   (when (and (graphics-window-cells window) (gv window :last-xfrms))
     (s-value window :current-xfrm (pop (gv window :last-xfrms)))
     (s-value window :width (pop (gv window :last-widths)))
     (s-value window :height (pop (gv window :last-heights)))
     (s-value window :scale (pop (gv window :last-scales)))
     (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS window)
     (add-temp-comment window "UnZooming..." :update nil)
     (RESET-HISTOLOGY-SCALE-BAR-UM-LENGTH window)
     (draw-cells window t))))

(defun add-histology-interactors (win)
  (add-window-zoom win #'zoom :control-leftdown)
  (create-instance nil window-menu-Interactor (:Window win) (:final-function #'histology-window-menu-inter-function))
  (create-instance nil histology-element-menu-Interactor (:Window win) (:final-function #'histology-element-menu-inter-function))
  (create-instance nil raise-all-menus-interactor (:window win))
  (add-window-coords-pointer win #'histology-coords-pointer-result #'mark-histology-coords-pointer-result #'histology-coords-running-function)
  (create-instance nil histology-pointer (:Window win))
  (create-instance nil histology-soma-pointer (:Window win)))

(defun clear-histology-windows () (clear-windows-of-mode :histology))

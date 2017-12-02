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


;;; SYS Source file: colorizing.lisp

(IN-PACKAGE "SURF")

;;;;;;;;;;;;; Colorizing Simulation Histology ;;;;;;;;;;;;;;;;;

#| ;; Debug code
(progn (topload 'dead-hippo)(std-setup)
       (add-waveform *isource* :waveform-spec (sinewave 10 100 1 :step 0.1))
       (setq *user-stop-time* 10	     *use-fixed-step* t	     *user-step* 0.1	     *save-data-step* 1	     *enable-sparse-data* t
	     *enable-colorize-time* t	     *colorize-simulation* t	     *enable-colorize-scale* t)
       (just-draw)  (goferit))

(progn (profile::unprofile)
        (profile::profile update-sparse-data replay-colorized-simulation-internal-xfr-btwn-elements-value-arrays)
	(time (goferit))
	(profile::report-time)(profile::unprofile))

(defun test-replay-colorized ()
  (profile::unprofile)
  ;; (profile::profile update-histology-color colorize-xfr-btwn-elements-values set-node-voltages-from-elements-values-array)
  (profile::profile UPDATE-VIRTUAL-SEGMENT  ; access-*line-styles-array*-fast ; access-*line-styles-array*-for-segment-voltage ; ;
					    ; XLIB::DRAW-LINE-FAST ; gem::x-draw-line-fast ; UPDATE-VIRTUAL-SEGMENT
					    ; XLIB::RADIANS->INT16 
		    )
 (time (replay-colorized-simulation :time-step 1 :repetitions 10))
  (profile::report-time)
  (profile::unprofile))
|#

(defun update-sparse-data-and-colorize-simulation (&optional now)
  ;; This is called from within the main simulation loop.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when (or now
	    (> (the sf *real-time*)
	       (+ (if *last-sparse-data-time* (the sf *last-sparse-data-time*) 0.0)
		  (the sf *sparse-data-step*)))

	    ;; Mystery code.
	    ;; (>= (the sf (float-mod (the sf (*t[n]*)) (the sf *sparse-data-step*)))
	    ;;     (the sf (float-mod (the sf *real-time*) (the sf *sparse-data-step*))))

	    )
    (setq *last-sparse-data-time* *real-time*)
    (when *enable-sparse-data* (UPDATE-SPARSE-DATA-FAST))

    (when *colorize-simulation* (update-histology-color nil t nil *real-time*))))

(defvar *ignore-colorized-animation-component-p* t) ; This disables the test by COLORIZED-ANIMATION-COMPONENT-P, called by OPAL::UPDATE-METHOD-AGGREGATE

(defun colorized-animation-component-p (comp)
  (or *ignore-colorized-animation-component-p*
      (gv comp :colorizable)
      (eq (car (gv comp :is-a)) time-comment)
      (eq (gv comp :type) 'segments)
      (eq (gv comp :type) 'somas)))

(defun collect-elements-values-and-nodes (elements data-type)
  (loop for elt in elements
	collect (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
		    (element-sparse-data-keys elt data-type)
		  (get-ordered-element-sparse-data elt sparse-data-key ordered-sparse-data-key)) into elements-values
	collect (typecase elt (segment (segment-node-2 elt)) (soma (soma-node elt))) into element-nodes
	finally (return (values elements-values element-nodes))))

(defun set-node-voltages-from-elements-values-array (element-nodes elements-value-array last-elements-value-array last-time show-time next-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float) elements-value-array last-elements-value-array))
  (loop for node in element-nodes 
	for i fixnum from 0
	do (setf (node-voltage-n node) (d-flt (interpolate-data (aref elements-value-array i) next-time (aref last-elements-value-array i) last-time show-time))))
  nil)

(defun colorize-xfr-btwn-elements-values (elements-values last-elements-value-array elements-value-array init)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float) elements-value-array last-elements-value-array))
  (do ((i 0 (1+ i))
       (l elements-values (cdr l)))
      ((null (car l)))
    (declare (fixnum i))
    (setf (aref last-elements-value-array i) (if init (the sf (caar l)) (aref elements-value-array i)))
    (setf (aref elements-value-array i) (the sf (caar l)))
    (setf (car l) (cdar l)))
nil)

(defun replay-colorized-simulation-internal
    (wins start-time-internal stop-time-internal time-step-internal elements-values element-nodes elements-value-array last-elements-value-array
     show-time-prefix)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((*colorizing-simulation* t)
	 (time-list (current-sparse-data-times))
	 last-time)
    (declare (type (simple-array single-float) elements-value-array last-elements-value-array)
	     (single-float start-time-internal stop-time-internal time-step-internal))
    ;; List of lists traversal and surgery adapted from the lisp::map1 function.
    (colorize-xfr-btwn-elements-values elements-values last-elements-value-array elements-value-array t)
    (loop for show-time single-float from start-time-internal to stop-time-internal by time-step-internal
	 
       do (loop until (or (null time-list) (<= show-time (the sf (car time-list))))
	     do (colorize-xfr-btwn-elements-values elements-values last-elements-value-array elements-value-array nil)
	       (setq last-time (the sf (car time-list))
		     time-list (cdr time-list)))
       when (car time-list)
       do (set-node-voltages-from-elements-values-array element-nodes elements-value-array last-elements-value-array last-time show-time (the sf (car time-list)))
	 (update-histology-color wins t nil (if show-time-prefix (format nil "~A~%Time: ~d ms" show-time-prefix (round show-time)) show-time))
	 (when t
	 (let ((*restrict-snapshot-to-real-time* t)
	       (*real-time* show-time))
	   (loop for win in wins do (draw-light-stimulus win) 
		; (opal:update win t)
		; (break)
		)))
	 )))


(defun replay-colorized-simulation (&key (start-time 0) (stop-time *user-stop-time*) (time-step 0.1) include-colorizing-scale
				    show-time-prefix
				    win (repetitions 1) (display-time t) (elements (cell-elements)) (data-type 'voltage))
  "Runs colorized animation of stored values of DATA-TYPE for ELEMENTS in WIN [if NIL, will run in all histology windows]."
  (let* ((*enable-colorize-time* display-time)
	 (wins (flatten-list (coerce-to-list (or win (no-nils *colorized-windows*) (windows-of-mode :histology)))))
	 (*ignore-colorized-animation-component-p* nil)
	 (num-elements (the fn (length elements)))
	 (start-time-internal (s-flt start-time))
	 (stop-time-internal (s-flt stop-time))
	 (time-step-internal (s-flt time-step))
	 ;; These are supplied by replay-colorized-simulation-internal will be reused if more than one repetition.
	 (elements-value-array (make-array (list num-elements) :element-type 'single-float))
	 (last-elements-value-array (make-array (list num-elements) :element-type 'single-float)))
    (resurrect-opal-win (windows-of-mode :histology) :update t)
    (when include-colorizing-scale (loop for win in wins do (s-value win :include-colorizing-scale t)))
    (resurrect-opal-win wins :raise t :visible t :deiconify t :update t)
    (loop for trial from 1 by (if repetitions 1 0) do
	  (multiple-value-bind (elements-values element-nodes)
	      ;; ELEMENTS-VALUES is a list of sparse-data lists for all the ELEMENTS
	      (collect-elements-values-and-nodes elements data-type)
	    ;; Use an explicit function for debugging purposes.
	    (replay-colorized-simulation-internal wins start-time-internal stop-time-internal time-step-internal
						  elements-values element-nodes elements-value-array last-elements-value-array
						  show-time-prefix))
	  when (and repetitions (= trial repetitions)) do (return))
    (resurrect-opal-win wins :raise t :visible t :deiconify t :update t)
    nil))

(defun show-sparse-data (&key (integrate nil) integrate-start-time integrate-stop-time integrate-offset
			 (target-time *real-time*) (data-type 'voltage) window (lock-color t))
  "Update colorization in histology WINDOW with DATA-TYPE at TARGET-TIME [ms, default *REAL-TIME*]. If WINDOW is not included, any
windows referenced by the variable *COLORIZED-WINDOWS* will be used, otherwise, if none, then all current histology windows. When
LOCK-COLOR is T (default) the colorizing is locked. When DATA-TYPE is NIL, then don't set element values."
  (let* (				; (data-type (or data-type 'voltage))
	 (windows (flatten-list (or window *colorized-windows* (windows-of-mode :histology))))
	 (integrate-offset (or integrate-offset (gv (car windows) :colorize-integrate-offset) 0.0))
	 (integrate-start-time (or integrate-start-time (gv (car windows) :colorize-integrate-start-time) 0.0))
	 (integrate-stop-time (or integrate-stop-time (gv (car windows) :colorize-integrate-stop-time) *user-stop-time*)))
    (when (current-sparse-data-times)
      (unless (numberp target-time) (when data-type (setq integrate t)))
      (when data-type
	(loop for element in (cell-elements) do
	      (set-element-value element 
				 (if integrate
				     (/ (integrate-sparse-data element nil integrate-start-time integrate-stop-time
							       integrate-offset)
					1 ; (- integrate-stop-time integrate-start-time )
					)
				     (multiple-value-bind (sparse-data dummy-target-time)
					 (retrieve-sparse-data element target-time data-type)
				       (declare (ignore dummy-target-time))
				       sparse-data))
				 data-type)))
      (when integrate
	(loop for window in windows do
	      (s-value window :colorize-integrate-offset integrate-offset)
	      (s-value window :colorize-integrate-start-time integrate-start-time)
	      (s-value window :colorize-integrate-stop-time integrate-stop-time))
	(setq target-time (format nil "Integration from ~a to ~,2e ms"
				  (if (zerop integrate-start-time) "0" (format nil "~,2e" integrate-start-time))
				  integrate-stop-time)))
      (when lock-color
	(loop for window in windows do
	      (lock-color window)
	      (when (numberp target-time) (s-value window :colorize-instant-time target-time))
	      (s-value window :colorize t)))
      (update-histology-color windows t lock-color target-time))))

(defun update-histology-color (&optional wins force (lock nil) displayed-time)
  (loop for win in (flatten-no-nils-list (or wins *colorized-windows* (valid-histology-windows))) do
	(let ((original-colorize (gv win :colorize))
	      (original-lock-color (gv win :lock-color)))
	  (s-value win :displayed-time displayed-time)
	  (when force (s-value win :colorize t))
	  (s-value win :lock-color nil)
	  (s-value win :show-time (or *enable-colorize-time* (gv win :show-time)))
	  (UPDATE-ALL-TIME-COMMENTS)
	  (opal:update win t)
	  (s-value win :lock-color (or lock original-lock-color))
	  (s-value win :colorize original-colorize))))

(defun set-element-value (element value &optional (data-type 'voltage)) (when value (set-element-value-fast (element element) value data-type)))

(defun set-element-value-fast (element value data-type)
  (case data-type
    (voltage
     (typecase element
       (segment (set-segment-voltage element value))
       (soma (set-soma-voltage element value))))))

(defun set-color-map-menu ()
  "Menu for reseting the color mapping used in the colorizing code. Assumes voltage scale."
  (let ((*COLOR-MAP-FUNCTIONS*-strings (mapcar #'(lambda (sym) (string sym)) *COLOR-MAP-FUNCTIONS*))
	(dummy1 (string *default-color-map*))
	(dummy2 *colorizing-arrays-color-max*)
	(dummy3 *colorizing-arrays-color-min*))
    (choose-variable-values
     `((dummy1 "Choose a color map:" :choose ,*COLOR-MAP-FUNCTIONS*-strings)
       (dummy2 "Maximum voltage [mv]" :number)
       (dummy3 "Minimum voltage [mv]" :number))
     :text "Reset Colorizing Map")
    (setq *colorizing-arrays-color-max* (s-flt dummy2)
	  *colorizing-arrays-color-min* (s-flt dummy3))
    (set-color-map :map (read-from-string dummy1) :variable-color-max dummy2 :variable-color-min dummy3)))
  
(defun set-color-map (&key (map *default-color-map*)
		      (variable-color-min *colorizing-arrays-color-min*)
		      (variable-color-max *colorizing-arrays-color-max*)
		      (colors-array *colorizing-colors*) (color-fill-styles-array *cell-fill-styles*) (color-line-styles-array *cell-color-line-styles*))
  "Resets the color mapping used in the colorizing code. MAP can be any symbol given by *COLOR-MAP-FUNCTIONS*, e.g.

     '(HOT-JET-RGB-MAP SH-ORIGINAL-RGB-MAP HOT-RGB-MAP JET-RGB-MAP GRAY-RGB-MAP PINK-RGB-MAP HOT-COLD-RGB-MAP)

Note that if too many color maps are used (for SunOs, more than 3), an XLIB:ALLOC-ERROR will occur. This will necessitate
restarting Lisp."
  (setq *colorizing-arrays-color-min* (s-flt variable-color-min )
	*colorizing-arrays-color-max* (s-flt variable-color-max))
  (UPDATE-colorizing-scale-PARAMETERS)
  (when map (setq *default-color-map* map))
  (fill-*cell-fill-styles*
   :variable-color-min variable-color-min :variable-color-max variable-color-max :variable-color-step *colorizing-arrays-color-step*
   :colors-array colors-array :color-fill-styles-array color-fill-styles-array :color-line-styles-array color-line-styles-array)
  nil)
	
(defun test-color-maps (&optional (maps *color-map-functions*))
  (let ((win-width 200)
	(win-height 200))
    (loop for map in (coerce-to-list maps) do
	  (setq *default-color-map* map)
	  (plot-color-map map 100 win-width win-height)
	  (fill-*cell-fill-styles*)
	  (let (*standard-graphics-output*)
	    (colorizing-scale :win-width win-width :win-height win-height :title map
			      :test t :position-relative-to-window :middle :center-x 0 :center-y 0
			      :labeled-voltages 3 :scale-height (* 0.8 *standard-graphics-height*) :scale-width 160))
	  (lock-windows))))

(defun plot-color-map (map &optional (length 100) (width 200) (height 200))
  (multiple-value-bind (r g b)
      (rgb-map-values map length)
    (plot-timed-data (list r g b) '(red green blue) 1 :title map
		     :width width :height height
		     :x-are-fns t :x-label "%" :x-label-horizontal-position :center
		     :line-styles :thick-dashed
		     :x-max length :x-min 0
		     :y-min 0 :y-max 1.0)))

(defvar *colorizing-arrays-color-scale-border* 0)

#|
(if (> scale-range 20) (format nil "~a ~A" (round scale) units-label) (format nil "~,2f ~A" scale units-label))
				  string-right (+ (* 0.5 (gv font :font-height)) y1) agg
				  :dimensions-in-pixels t :justification :right :font font)

(defun aggregadget-left-formula ()
  (case (gvl :position-relative-to-window)
    ((:lower-middle :middle :upper-middle) (round (- (the sf (/ (the fn (gvl :parent :window :width)) 2.0))
						     (the sf (/ (the fn (gvl :width)) 2.0))
						     (the fn (gvl :border)))))
    ((:lower-right :middle-right :upper-right) (- (the fn (gvl :parent :window :width))
						  (the fn (gvl :width))
						  (the fn (gvl :border))))
    ((:lower-left :middle-left :upper-left) (the fn (gvl :border)))))

(defun aggregadget-top-formula ()
  (case (gvl :position-relative-to-window)
    ((:lower-middle :lower-right :lower-left) (- (the fn (gvl :parent :window :height))
						 (the fn (gvl :height))
						 (the fn (gvl :border))))
    ((:middle :middle-right :middle-left) (round (- (/ (the fn (gvl :parent :window :height)) 2)
						    (/ (the fn (gvl :height)) 2)
						    (the fn (gvl :border)))))
    ((:upper-middle :upper-right :upper-left) (the fn (gvl :border)))))
  
(create-instance 'colorizing-scale opal::aggregadget
		 (:border (o-formula *colorizing-arrays-color-scale-border*)) (:scale-height 100) (:scale-width 40) ; pixels
		 (:width (o-formula (gvl :scale-width)))
		 (:height (o-formula (gvl :scale-height)))
		 (:position-relative-to-window :lower-right)
		 (:max-scale (o-formula (float (or *colorizing-arrays-color-max* 100.0)))) ; float
		 (:min-scale (o-formula (float (or *colorizing-arrays-color-min* -100.0)))) ; float
		 (:scale-range (o-formula (- (gvl :max-scale) (gvl :min-scale)))) ; float
		 (:num-steps (o-formula (fix (or *colorizing-arrays-color-dimension* 100)))) ; fixnum
		 (:scale-increment (o-formula (/ (gvl :scale-range) (or (gvl :num-of-steps) 100)))) ; float
		 (:scale-step-thickness (o-formula (round (/ (or (gvl :scale-height) 100) (1+ (or (gvl :num-of-steps) 100)))))) ; fixnum
		 (:font (o-formula *plot-axis-font*))
		 (:labeled-scales 3)
		 (:scale-top (o-formula (the fn (or (gvl :top) 0))))
		 (:scale-left (o-formula (the fn (or (gvl :left) 0))))
		 (:left (o-formula (case (gvl :position-relative-to-window)
				     ((:lower-middle :middle :upper-middle) (round (- (the sf (/ (the fn (gvl :parent :window :width)) 2.0))
										      (the sf (/ (the fn (gvl :width)) 2.0))
										      (the fn (gvl :border)))))
				     ((:lower-right :middle-right :upper-right) (- (the fn (gvl :parent :window :width))
										   (the fn (gvl :width))
										   (the fn (gvl :border))))
				     ((:lower-left :middle-left :upper-left) (the fn (gvl :border))))))
		 (:top (o-formula (case (gvl :position-relative-to-window)
				      ((:lower-middle :lower-right :lower-left) (- (the fn (gvl :parent :window :height))
										   (the fn (gvl :height))
										   (the fn (gvl :border))))
				      ((:middle :middle-right :middle-left) (round (- (/ (the fn (gvl :parent :window :height)) 2)
										      (/ (the fn (gvl :height)) 2)
										      (the fn (gvl :border)))))
				      ((:upper-middle :upper-right :upper-left) (the fn (gvl :border))))))
		 (:parts
		  (o-formula
		   (let ((out '())
			 (labeled-scales (if (consp (gvl :labeled-scales))
					   (sort (gvl :labeled-scales) '<)
					   (list-of-nums (gvl :labeled-scales) (gvl :min-scale) (/ (gvl ::scale-range) (1- (gvl :labeled-scales)))))))
		     (loop for step single-float from 0.0 to (s-flt (or (gvl :num-of-steps) 100)) do
			   (let* ((y (round (- (+ (gvl :scale-top) (gvl :scale-height)) (* step (gvl :scale-step-thickness)))))
				  (scale-value (+ (* step (gvl :scale-increment)) (gvl :min-scale)))
				  (thickness (max 5 (gvl :scale-step-thickness)))
				  (color (get-opal-variable-color scale-value :max-variable (gvl :max-scale) :min-variable (gvl :min-scale))))
			     (declare (single-float scale-value))
			     (when (>= scale-value (car labeled-scales))
			       (push `(:scale-labels ,opal:text
						     (:left ,(- (gvl :scale-left) (or (gvl :scale-width) 0)))
						     (:top  ,(round (- y (/ (or (gvl :font :height) 10) 2))))
						     (:string ,(if (> (gvl :scale-range) 20)
								 (format nil "~a ~A" (round scale-value) (gvl :units-label))
								 (format nil "~,2f ~A" scale-value (gvl :units-label))))
						     (:font ,(or (gvl :font) (graphics-text-font (gvl :parent :window))))
						     (:line-style ,opal:line-style))
				     out)
			       (setq labeled-scales (cdr labeled-scales)))
			     (push `(:scale-lines ,opal:line (:x1 ,(gvl :scale-left)) (:y1 ,y) (:x2 ,(+ (gvl :scale-left) (gvl :scale-width)))
						  (:y2 ,y) (:draw-function :copy)
						  (:line-style ,(create-instance nil opal:line-style
										 (:foreground-color (get-opal-color color)) (:thickness thickness))))
				   out)))
		     out))))


(defun colorizing-scale (&key (num-of-steps *colorizing-arrays-color-dimension*)
			      (units-label "mV")
			      (max-scale *colorizing-arrays-color-max*) (min-scale *colorizing-arrays-color-min*)
			      (win-width 100) (win-height 120) title (update t) test remove
			      (win (if test
				     (get-plot-window nil 'colorizing-colors nil :name title :create-new-plot-windows nil :width win-width :height win-height)
				     (or (opal-obj-exists *standard-graphics-output*) (get-histology-window 'histology title :exclude-auxiliary-type :keys))))
			      (font *plot-axis-font*) (position-relative-to-window :lower-right) (border *colorizing-arrays-color-scale-border*)
			      (scale-height 100) (scale-width 40) (labeled-scales 3))
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (setq *colorizing-arrays-color-scale-border* border)
	
  (loop for win in (coerce-to-list win) do
	(let ((agg (clear-and-add-plot-agg win 'colorizing-scale :add t)))
	  (opal:add-component
	   agg
	   (create-instance nil
			    colorizing-scale
			    (:scale-height scale-height) (:scale-width scale-width)
			    (:position-relative-to-window position-relative-to-window)
			    (:labeled-scales labeled-scales))))))
|#				     

(defvar *colorizing-scale-units* "mV")

(defun colorizing-scale (&key (num-of-steps *colorizing-arrays-color-dimension*)
			 (units-label *colorizing-scale-units*)
			 (max-scale *colorizing-arrays-color-max*) (min-scale *colorizing-arrays-color-min*)
			 (win-width 100) (win-height 120) title (update t) test remove
			 (win (if test
				  (get-plot-window nil 'colorizing-colors nil :name title :create-new-plot-windows nil :width win-width :height win-height)
				  (or (opal-obj-exists *standard-graphics-output*) (get-histology-window 'histology title :exclude-auxiliary-type :keys))))
			 (font *plot-axis-font*) (position-relative-to-window :lower-right) (border *colorizing-arrays-color-scale-border*)
			 (center-x -40) (center-y 32) ; Scale position relative to the center of the window.
			 (scale-height 50) (scale-width 20) (label-scale-gap 5) (labeled-scales 3))
  ;;  (declare (optimize (safety 1) (speed 3) (space 1)))
  (loop for win in (coerce-to-list win) do
	(let* ((font (or (gv win :font) font))
	       (center-x (round (case position-relative-to-window
				  ((:lower-middle :middle :upper-middle) center-x)
				  ((:lower-right :middle-right :upper-right) (+ center-x (- border) (* 0.5 (gv win :width))))
				  ((:lower-left :middle-left :upper-left) (+ center-x border (* -0.5 (gv win :width)))))))
	       (center-y (round (case position-relative-to-window
				  ((:middle-right :middle-left :middle) center-y)
				  (:lower-right (if (view-angle-comment-p win)
						    (+ border (gv font :font-height) 2 center-y (* -0.5 (gv win :height)))
						    (+ border center-y (* -0.5 (gv win :height)))))
				  ((:lower-left :lower-middle :lower-right) (+ center-y border (* -0.5 (gv win :height))))
				  ((:upper-left :upper-middle :upper-right) (+ center-y (- border) (* 0.5 (gv win :height)))))))
	       (scale-range (s-flt (- max-scale min-scale)))
	       (scale-increment (s-flt (/ scale-range num-of-steps)))
	       (labeled-scales (if (consp labeled-scales)
				   (sort labeled-scales '<)
				   (list-of-nums labeled-scales min-scale (/ (- max-scale min-scale) (1- labeled-scales)))))
	       (label-width (max (the fn (opal:string-width font (format nil "~A ~A" max-scale units-label))) ; estimated-max-label-width
				 (the fn (opal:string-width font (format nil "~A ~A" min-scale units-label)))))
	       (scale-thickness (s-flt (/ scale-height (1+ num-of-steps))))
	       (scale-center-x (+ center-x (* 0.5 label-width)))
	       (scale-center-y center-y)
	       (scale-left (- scale-center-x (* 0.5 scale-width)))
	       (scale-right (+ scale-center-x (* 0.5 scale-width)))
	       (string-right (- scale-left label-scale-gap))
	       (agg (clear-and-add-plot-agg win 'voltage-scale :add (not remove))))
	  (unless remove
	    (loop for step fixnum from 0 to (the fn (1- num-of-steps)) do
		  (let* ((y1 (+ scale-center-y (* step scale-thickness) (* 0.5 scale-thickness) (* -0.5 scale-height)))
			 (y2 y1)
			 (scale (+ (* step scale-increment) min-scale))
			 (color (get-opal-variable-color scale :max-variable max-scale :min-variable min-scale)))
		    (declare (single-float y1 y2 scale))
		    (when (or (>= scale (car labeled-scales))
			      (and (= step (1- num-of-steps))
				   (<= (- scale (car labeled-scales)) scale-increment)))
		      (add-string (format nil "~a ~A" (cond ((zerop (car labeled-scales)) "0")
							    ((> scale-range 20) (format nil "~A" (round (car labeled-scales))))
							    (t (format nil "~,1e" (car labeled-scales))))
					  units-label)
				  string-right (+ (* 0.5 (gv font :font-height)) y1) agg
				  :dimensions-in-pixels t :justification :right :font font)
		      (setf labeled-scales (cdr labeled-scales)))
		    (add-line scale-left y1 scale-right y2 agg :color color :dimensions-in-pixels t :thickness (max 5 scale-thickness)))))
	  (when update (resurrect-opal-win win :show t)))
	collect win into out-wins finally (return out-wins)))

(defun colorize-and-sparse-data-menu (&optional win)
  (let (dummy1
	(dummy2 (or (and win (gv win :colorize-integrate-offset)) 0.0))
	(dummy3 1)
	(dummy4 (or (and win (gv win :colorize-start-time)) 0.0))
	(dummy5 (or (and win (gv win :colorize-stop-time)) *user-stop-time*))
	(dummy6 (or (and win (gv win :colorize-step)) 0.1))
	(dummy9 (or (and win (gv win :colorize-instant-time)) *user-stop-time*))
	(dummy11 (or (and win (gv win :colorize-integrate-start-time)) 0.0))
	(dummy12 (or (and win (gv win :colorize-integrate-stop-time)) *user-stop-time*))
	(dummy8 (if win (gv win :show-time) *enable-colorize-time*))
	(dummy10 (when win (gv win :lock-color)))
	(dummy21 (or (and win (gv win :colorize-integrate-offset)) 0.0))
	(dummy22 (and win (gv win :colorize)))
	(dummy24 (if win (gv win :include-colorizing-scale) *enable-colorize-scale*))
	(loaded-sparse-data-p (loaded-sparse-data-p))
	quit)
    (loop until quit do
	  (let (dummy2 dummy7)
	    (choose-variable-values
	     `((*enable-sparse-data* ,(format nil "Enable sparse data storing for all nodes~%(required for colorization)") :boolean)
	       (*sparse-data-step* "Sparse data collection step [ms]" :float)
	       (*colorize-simulation* "Colorize during simulation" :boolean)
	       (dummy22 "Enable colorization for current wins" :boolean)
	       (dummy24 "When colorizing, include voltage scale" :boolean)
	       (dummy1 ,(format nil "Select windows to colorize ~%(current: ~A)"
				(if win (gv win :title) (if *colorized-windows* (length *colorized-windows*) "All")))
		       :boolean)
	       (dummy7 "Set colormap" :boolean)
	       ,(when loaded-sparse-data-p `(:comment "Sparse Data Animation"))
	       ,(when loaded-sparse-data-p `(dummy2 "Replay loaded sparse data:" :choose (:Sequence :Instant :Integrate) :toggle-p :label-left))
	       ,(when loaded-sparse-data-p `(dummy3 "Repetitions" :integer))
	       ,(when loaded-sparse-data-p `(dummy9 "Instant time [ms]" :float))
	       ,(when loaded-sparse-data-p `(dummy11 "Integrate start time [ms]" :float))
	       ,(when loaded-sparse-data-p `(dummy12 "Integrate stop time [ms]" :float))
	       ,(when loaded-sparse-data-p `(dummy21 "Integrate offset" :float))
	       ,(when loaded-sparse-data-p `(dummy4 "Start time [ms]" :float))
	       ,(when loaded-sparse-data-p `(dummy5 "Stop time [ms]" :float))
	       ,(when loaded-sparse-data-p `(dummy6 "Time step [ms]" :float))
	       ,(when (and win loaded-sparse-data-p) `(dummy10 "Lock color" :boolean)))
	     :text (when win (gv win :title))
	     :label "Colorizing and Sparse Data Menu")
	    (let (update-scale
		  (color-wins (coerce-to-list
			       (if dummy1
				 (setq *colorized-windows* (window-selection-menu "Choose Histology Windows to Colorize"
										  (histology-wins-with-current-cells)
										  (union (list win) *colorized-windows*)))
				 (or win *colorized-windows* (histology-wins-with-current-cells))))))
	      (setq dummy1 nil)
	      (mapcar #'(lambda (win)
			  (setq update-scale (or update-scale (xor dummy24 (gv win :include-colorizing-scale))))
			  ;; (s-value win :enable-colorize-time dummy8)
			  ;; (s-value win :update-colorize (and win (not (equal dummy22 (gv win :colorize)))))
			  (s-value win :colorize dummy22)
			  (s-value win :include-colorizing-scale dummy24)
			  (s-value win :show-time dummy8)
			  (s-value win :colorize-start-time dummy4) 
			  (s-value win :colorize-stop-time dummy5)
			  (s-value win :colorize-step dummy6)
			  (s-value win :colorize-instant-time dummy9))
		      color-wins)
	      (when dummy7 (set-color-map-menu) (setq update-scale t))
	      (when update-scale (loop for win in color-wins do
				       (colorizing-scale :win win :remove (not (gv win :include-colorizing-scale)))
				       (when (or dummy10 (gv win :lock-color)) (lock-color win))))
	      (if *circuit-processed*
		(progn
		  (case dummy2
		    (:sequence
		     (replay-colorized-simulation :start-time dummy4 :stop-time dummy5 :time-step dummy6 :repetitions dummy3 :win color-wins)
		     (mapcar #'(lambda (win) (opal:update win t)) color-wins))
		    (:integrate (show-sparse-data :integrate t
						  :integrate-offset dummy21
						  :integrate-start-time dummy11 :integrate-stop-time dummy12 :window color-wins :lock-color dummy10))
		    (:instant (show-sparse-data :target-time dummy9 :window color-wins :lock-color dummy10))
		    (t (UPDATE-ALL-TIME-COMMENTS)
		     (unless (or update-scale dummy7) (setq quit t))))
		  (if dummy10 (lock-color win) (unlock-color win)))
		(setq quit t)))))))

(defun lock-color (&optional wins)
  (loop for win in (flatten-list (or wins *colorized-windows* (windows-of-mode :histology))) do
	(let ((seg-plot-agg (get-plot-agg win 'segments)))
	  (when (and seg-plot-agg (gv seg-plot-agg :virtual-aggregate))
	    (loop for item-values-ref across (gv seg-plot-agg :virtual-aggregate :item-array)
		  do (setf (virtual-segment-color-index item-values-ref) (color-styles-voltage-index (get-segment-voltage-2 (virtual-segment-segment item-values-ref)))))))
	(let ((soma-plot-agg (get-plot-agg win 'somas)))
	  (when (and soma-plot-agg (gv soma-plot-agg :virtual-aggregate))
	    (loop for item-values-ref across (gv soma-plot-agg :virtual-aggregate :item-array)
		  do (setf (virtual-soma-color-index item-values-ref) (color-styles-voltage-index (get-soma-voltage-df (virtual-soma-soma item-values-ref)))))))
	(s-value win :lock-color t)
	(s-value win :show-time t)
	(s-value win :colorize t)))

(defun unlock-color (&optional wins)
  (loop for win in (flatten-list (or wins *colorized-windows* (windows-of-mode :histology))) do	(s-value win :lock-color nil)))



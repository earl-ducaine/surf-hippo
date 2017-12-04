;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SON-OF-PLOT-HACK; Base: 10 -*-


;; The Surf-Hippo Neuron Simulator System
;;
;; This code was written as part of the Surf-Hippo Project, originally
;; at the Center for Biological Information Processing, Department of
;; Brain and Cognitive Sciences, Massachusetts Institute of
;; Technology, and currently at the Neurophysiology of Visual
;; Computation Laboratory, CNRS.
;;
;; Permission to use, copy, modify, and distribute this software and
;; its documentation for any purpose and without fee is hereby
;; granted, provided that this software is cited in derived published
;; work, and the copyright notice appears in all copies and in
;; supporting documentation. The Surf-Hippo Project makes no
;; representations about the suitability of this software for any
;; purpose. It is provided "as is" without express or implied
;; warranty.
;;
;; If you are using this code or any part of Surf-Hippo, please
;; contact surf-hippo@ai.mit.edu to be put on the mailing list.
;;
;; Copyright (c) 1989 - 2003, Lyle J. Graham
;; GUI Source file: plot-hack.lisp

;;; This contains some basic plotting routines, inspired by the
;;; PLOT-HACK system written by Patrick O'Donnell at the MIT AI Lab
;;; for the Symbolics window system, and built upon the Garnet GUI
;;; toolset from CMU. This system requires that WINDOW-HACK and
;;; MENU-HACK be loaded. See the plotting.doc file for more
;;; information.

(in-package :son-of-plot-hack)

(defun if-black-background-white (&optional default-color)
  (let ((window (or (gvl :window) (and (gvl :parent) (gvl :parent :window)))))
    (if (black-p (when window (gv window :background-color)))
	opal::white
	(or default-color (gvl :reference-color) opal::black))))

(defun if-black-background-white-line (&optional (default-line thin-black-line))
  (if (black-p (and (gvl :parent :window) (gvl :parent :window :background-color)))
      thin-white-line
      default-line))

(defun generic-to-axis (axis generic-slot)
  ;; Generate slot accessor keyword.
  (read-from-string (format nil ":~A-~A" axis generic-slot)))

(defun gv-generic-to-axis (win axis generic-slot) (gv win (generic-to-axis axis generic-slot)))

(create-instance 'plot-line-style opal:line-style
		 (:foreground-color (o-formula
				     (if (and (gvl :reference-color) (not (black-p (gvl :reference-color))))
					 (gvl :reference-color)
					 (if (black-p (or (and (gvl :window) (gvl :window :background-color))
							  (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :background-color))))
					     opal::white
					     (or (gvl :reference-color) opal::black))))))

(create-instance 'plot-filling-style opal:filling-style
		 (:foreground-color (o-formula
				     (if (and (gvl :reference-color) (not (black-p (gvl :reference-color))))
					 (gvl :reference-color)
					 (if (black-p (or (and (gvl :window) (gvl :window :background-color))
							  (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :background-color))))
					     opal::white
					     (or (gvl :reference-color) opal::black))))))

(defun create-plot-line-style (reference-line-style window &optional color)
  (when reference-line-style
    (loop for line-style in (gv plot-line-style :is-a-inv)
	  when (and (eq (gv line-style :line-style) (gv reference-line-style :line-style))
		    (or (eq (gv line-style :foreground-color) (gv reference-line-style :foreground-color))
			(eq (gv line-style :foreground-color) color))
		    (eq (gv line-style :line-thickness) (gv reference-line-style :line-thickness))
		    (eq (gv line-style :dash-pattern) (gv reference-line-style :dash-pattern)))
	  do (return line-style)
	  finally (return (create-instance nil plot-line-style
					   (:window window)
					   (:line-style (gv reference-line-style :line-style))
					   (:reference-color (or color (gv reference-line-style :foreground-color)))
					   (:line-thickness (gv reference-line-style :line-thickness))
					   (:DASH-PATTERN (gv reference-line-style :DASH-PATTERN)))))))

(defun create-plot-filling-style (reference-filling-style window)
  (create-instance nil plot-filling-style
		   (:window window)
		   (:reference-color (gv reference-filling-style :foreground-color))))

(export '(PLOT-FILLING-STYLE CREATE-PLOT-FILLING-STYLE))


(defun window-plot-axis-font (win)
  (if win
      (or (gv win :plot-axis-font) (gv win :font))
      *window-default-font*
      ;;(opal:get-standard-font :serif :bold-italic :medium)
      ))


(defun plot-window-font (win) (window-plot-axis-font win))

(defun window-comment-font (win)
  (if win
      (or (gv win :comment-font) (gv win :font))
      *window-default-font*
      ;; (opal:get-standard-font :serif :bold-italic :medium)
      ))

(defun plot-window-string-width (win string) (opal::string-width (window-plot-axis-font win) string))

(defvar *plot-window-xy-bound-border* 20)

(create-instance 'plot-window
		 basic-graphics-window
		 (:title "SH Plot") (:icon-title "SH Plot")
		 (:mode :standard-plot)
		 (:background-color (symbol-to-opal-color *default-plot-window-background-color*))
		 (:resize-window-refresh-function 'refresh-plot) ; This function must have a single required arg, which is a plot window.
		 (:plot-type :xy)
		 (:data-type nil)	; This will be used to keep track of where to put data.

		 (:min-width 100)
		 (:min-height 100)
		 (:view-angle-comment-p t)

		 (:plot-line-style-family (o-formula *plot-line-style-family*)) (:connect-data-points t)
		 (:scatter-symbol *default-scatter-symbol*) (:fill-scatter t) (:scatter-symbol-borderp t) (:scatter-symbol-units-in-pixels t)
		 (:y-symbol-width *default-scatter-size*) (:x-symbol-width *default-scatter-size*)

		 (:axes-type :standard)
		 (:plot-axis-font (o-formula *plot-axis-font*))
		 (:comment-font (o-formula (gvl :plot-axis-font)))

		 ;; Axes labels
		 (:y-label "") (:x-label "")
		 (:label-height 0)	; this is in pixels
		 (:x-label-v-position :below)
		 (:x-label-h-position :right) ; :left :center :right
		 (:y-label-v-position :two-thirds-up)
		 (:y-label-h-position :left)

		 ;; For simple axes
		 (:x-scale-t% 5) (:y-scale-t% 5) (:x-scale-l% 85)

		 ;; These gaps in pixels frame the plotting area.
		 (:x-plot-left-gap 0)		 (:x-plot-right-gap 0)
		 (:y-plot-bottom-gap 0)		 (:x-plot-right-gap-extra 0)

		 ;;These values are in data coordinates, and should be floats.
		 (:y-max 0.0) (:y-min 0.0) (:y-origin 0.0) (:y-mag 0.0)
		 (:x-max 0.0) (:x-min 0.0) (:x-origin 0.0) (:x-mag 0.0)
		 (:y-inc 0.0) (:x-inc 0.0)

		 (:include-border-points t)
		 (:apply-horizontal-borders nil)		 (:apply-vertical-borders nil)

		 ;; For regular axes
		 (:include-x-tick-at-0 t)		 (:include-y-tick-at-0 t)
		 (:x-axis-p t)		 (:y-axis-p t)
		 (:x-axis-tick-skip 0)		 (:y-axis-tick-skip 0)
		 (:x-axis-tick-mark-skip 0)		 (:y-axis-tick-mark-skip 0)

		 ;; If these are negative, axis tick marks will point away from tick labels.
		 (:x-axis-tick-mark-length *x-axis-tick-mark-length*)		 (:y-axis-tick-mark-length *y-axis-tick-mark-length*)

		 ;; For waterfall plots
		 (:waterfall-trace-label-skip 0)		 (:gap-between-trace-and-waterfall-label 20)		 (:skirt-to-window-border t)

		 ;; These slots (pixel units) are for clipping transformed polylines just outside viewing. LG change 24.02.2017
		 (:y-bound-max (o-formula (+ (gvl :height) *plot-window-xy-bound-border* ;100
					     )))
		 (:y-bound-min (o-formula (- *plot-window-xy-bound-border*)) ; -100
			       )
		 (:x-bound-max (o-formula (+ (gvl :width) *plot-window-xy-bound-border* ;100
					     )))
		 (:x-bound-min (o-formula (- *plot-window-xy-bound-border*)) ; -100
			       )
		 (:min-max-lists '())	; Used for zooming/unzooming.

		 (:current-xfrm (3-by-3-identity)) ; For 3d plots

		 ;; Must be single-floats

		 (:x-trace-offset 0.0) (:y-trace-offset 0.0) ; For waterfall plots
		 (:x-data-offset 0.0) (:y-data-offset 0.0)
		 (:waterfall-base-x-offset 0.0) (:waterfall-base-y-offset 0.0)

		 (:x-data-scale 1.0) (:y-data-scale 1.0))



(create-instance 'axis-text opal:aggregadget
		 (:string "")
		 (:label-position :center) ; Determines horizontal position (:LEFT) of text relative to the :LEFT slot.
		 (:parts
                  `((:label ,window-hack-text
		     (:orientation ,(o-formula (gvl :parent :orientation)))
		     (:text ,(o-formula (gvl :parent :string)))
		     (:left ,(o-formula (- (the fn (gvl :parent :left))
					   (case (gvl :parent :label-position)
					     ((:LEFT-UP :LEFT-DOWN :LEFT) (the fn (gvl :width)))
					     ((:RIGHT-UP :RIGHT-DOWN :RIGHT) 0)
					     ;; :CENTER
					     (t (round (* 0.5 (the fn (gvl :width)))))))))
		     (:top ,(o-formula (the fn (gvl :parent :top))))
		     (:font ,(o-formula (window-plot-axis-font (gvl :window)))))))
		 (:interactors
		  `((:text-inter ,inter:text-interactor
		     (:window ,(o-formula (gv-local :self :operates-on :window)))
		     (:feedback-obj nil)
		     (:start-where ,(o-formula (list :in (gvl :operates-on :label))))
		     (:abort-event :CONTROL-\g)
		     (:stop-event (:leftdown))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Functions for accessing plot windows.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun standard-plot-windows () (windows-of-mode :standard-plot))

(defun all-plot-windows () (windows-of-mode '(:scanner :standard-plot :histogram :2dplot :3dplot)))

(defun find-plot-window (plot-type data-type &optional title (modes-to-search '(:scanner :standard-plot :histogram :2dplot :3dplot)))
  ;; Return a window according to spec if there is one in *OUTPUT-WINDOWS*.
  (unless (or *create-new-plot-windows* *create-new-plot-window-types*)
    (loop for window in (clean-up-*output-windows*) ; Is there already the right kind of window?
	  when (and
		(not (gv window :locked))
		(member (gv window :mode) modes-to-search)
		(or (not plot-type) (equal plot-type (gv window :plot-type)))
		(cond
		  ((and (stringp data-type) (stringp (gv window :data-type)))
		   (string-equal data-type (gv window :data-type)))
		  (data-type (eq data-type (gv window :data-type)))
		  (t t))
		(or (not title)
		    (string-equal title (gv window :title))
		    (string-equal title (gv window :session-name))))
	  do (return window))))

(defun create-plot-window (&key width height mode plot-type data-type prototype)
  (let ((win (create-instance nil plot-window (:icon-title "SH Plot")
			      (:mode mode) (:data-type data-type) (:plot-type plot-type)
			      (:omit-title-bar-p *omit-title-bar*)
			      (:aggregate (create-instance nil opal:aggregate))
			      (:plot-line-style-family *plot-line-style-family*))))
    (when prototype (TRANSFER-basic-plot-window-SLOTS prototype win))
    (initialize-graphics-window win :width width :height height)
    (add-plotting-interactors win)
    (clear-and-add-plot-agg win `data-plot)
    (push win *output-windows*)
    (update-*Twin*)
    win))


(defun get-plot-window (plot-type data-type overlay &key title session-name width height left top (mode :standard-plot)
			(enable-clear-plot-agg t) prototype default-win prompt-for-overlay (save-markers t)
			(create-new-plot-windows *create-new-plot-windows*) preserve-win-dims preserve-plot-layout accomodate-overlays
						      (child-number 0))
    (let* ((win (or default-win (unless create-new-plot-windows (find-plot-window plot-type data-type (if (> (length session-name) 0) session-name title)))))
	 (width (if (and win (or (not width) preserve-win-dims)) (gv win :width) width))
	 (height (if (and win (or (not height) preserve-win-dims)) (gv win :height) height))
	   cancel)
    (when (and prompt-for-overlay win)
      (let ((dummy1 (cond (accomodate-overlays :overlay_and_accomodate_new_data)
			  (overlay :overlay_and_retain_coordinates)
			  (create-new-plot-windows :make_new_window)
			  (preserve-plot-layout :erase_and_retain_coordinates)
			  (t *overlay-plot-default-option*))))
	(choose-variable-values
	 `((dummy1 "" :choose (:overlay_and_accomodate_new_data :overlay_and_retain_coordinates
			       :erase_and_retain_coordinates :make_new_window :erase_old_data :cancel_plot)
	    :vertical :rank-margin 6))
	 :text (format nil "~A~%Plot Overlay Options" (gv win :title)) :label "Plot Overlay Menu")
	(setq *overlay-plot-default-option* dummy1)
	(case dummy1
	  (:cancel_plot (setq cancel t))
	  (:erase_old_data (setq *create-new-plot-windows* nil
				 accomodate-overlays nil
				 preserve-plot-layout nil
				 overlay nil))
	  (:erase_and_retain_coordinates (setq preserve-plot-layout t overlay nil))
	  (:overlay_and_accomodate_new_data (setq *create-new-plot-windows* nil
						  accomodate-overlays t
						  overlay t))
	  (:overlay_and_retain_coordinates (setq *create-new-plot-windows* nil
						 accomodate-overlays nil
						 preserve-plot-layout t
						 overlay t))
	  (:make_new_window (setq *create-new-plot-windows* t
				  accomodate-overlays nil
				  overlay nil)))
	(setq overlay (and win overlay))))
    (unless cancel
      (cond ((or (not win) *create-new-plot-windows*)
	     (setq win (create-plot-window :data-type data-type :mode mode :prototype prototype :plot-type plot-type))
	     (increment-plotting-window-top)
	     (set-window-title win (or title data-type) prototype))
	    ((and enable-clear-plot-agg (not overlay)) ;If no overlay, destroy old agg
	     (clear-and-add-plot-agg win `data-plot)))
      (cond-every (data-type (s-value win :data-type data-type))
		  (session-name (s-value win :session-name  session-name))
		  (left (s-value win :left (round left)))
		  (top  (s-value win :top (round top))))
      (unless overlay
	(when width (s-value win :width width))
	(when height (s-value win :height height)))
      (s-values win
		(data-to-points-function (plot-data-to-points-function win))
		(plot-type plot-type)
		(overlay overlay)
		(child-number child-number)
		(save-markers save-markers)
		(preserve-plot-layout (and (> (length (gv win :x-lists)) 0) (not accomodate-overlays) preserve-plot-layout))
		(overlay (and (> (length (gv win :x-lists)) 0) (or (gv win :overlay) overlay)))
		(accomodate-overlays (and ; (> (number-of-overlays win) 0)
					  accomodate-overlays)))
      (unless (or (gv win :overlay) save-markers) (remove-all-markers win)))
    win))

(defun get-child-plot-window (parent &optional (same-size t))
  (let* ((*create-new-plot-windows* t)
	 (title (format nil "~a-~d" (gv parent :title) (1+ (gv parent :child-number))))
	 (child (create-instance nil parent
				 ; (:name title)
				 (:prototype parent)
				 (:aggregate (create-instance nil opal:aggregate)))))
    (set-window-title child title ; (gv child :name)
		      t)
    (initialize-graphics-window child)
    (add-plotting-interactors child)
    (clear-and-add-plot-agg child `data-plot)
    (push child *output-windows*)
    (update-*Twin*)
    (s-value child :data-to-points-function (plot-data-to-points-function child))
    (s-value child :child-number (1+ (gv parent :child-number)))
    (resurrect-opal-win child)
    child))

(defun find-2dplot-window (plot-type data-type &optional title) (find-plot-window plot-type data-type title '(:2dplot)))

(defun increment-plotting-window-top () (setq *plotting-window-top* (if *output-windows* (max 20 (mod (+ 20 *plotting-window-top*) 500)) 20)))

(defun TRANSFER-basic-plot-window-SLOTS (prototype win)
  (TRANSFER-SCHEMA-SLOTS prototype win
			 '(:x-log :y-log :log-base
			   :y-are-fns :x-are-fns :grid-line-style
			   :data-type :plot-type :mode
			   :preserve-plot-layout :overlay :accomodate-overlays
			   :comment-font
			   :y-label :x-label
			   :x-label-h-position :x-label-v-position
			   :y-label-h-position :y-label-v-position
			   :plot-line-style-family :axes-type
			   :label-height :label-list :label-traces
			   :x-lists :y-lists
			   :x-data-scale :y-data-scale
			   :x-data-offset :y-data-offset :x-trace-offset :y-trace-offset :y-inc :x-inc :y-max :x-max
			   :x-axis-p :x-axis-tick-skip :x-axis-tick-mark-skip
			   :y-axis-tick-skip :y-axis-tick-mark-skip :waterfall-trace-label-skip
			   :reference-ticks-to-origin reference-ticks-to-origin
			   :min-max-lists
			   :x-trace-offset :y-trace-offset
			   :x-scale-t% :y-scale-t% :x-scale-l%
			   :x-plot-right-gap-extra
			   :scatter-symbol :scatter-symbol-units-in-pixels
			   :plot-axis-font :y-symbol-width :x-symbol-width)))

(defun erase-plot-data (window)
  (s-value window :x-lists nil)
  (s-value window :y-lists nil)
  (s-value window :data-erased t))

(defun erase-all-plot-data () (mapcar 'erase-plot-data (windows-of-mode :standard-plot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Coordinate transformation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; X-PLOT-WIN and Y-PLOT-WIN translate from data coordinates to plot window coordinates in such a way as to keep the data contained within a scaled
;; rectangle that in turn is centered in the plot window. A separate function is needed for x and y since the origin of opal:windows is at the upper left
;; hand corner. This requires a flipping of the y values. The returned values are clipped by the :Y-BOUND-MAX, :Y-BOUND-MIN, :X-BOUND-MAX, :X-BOUND-MIN
;; slots in WIN so that we don't get wrap-around from illegal window coordinates.

;; 7/4/02 LG XLIB:DRAW-LINE expects (UNSIGNED-BYTE 16) type arguments. Thus, change all type of values returned by the XY value graphics conversion
;; routines accordingly.

(proclaim '(inline x-plot-win-float-unbounded-w-win-args))
(defun x-plot-win-float-unbounded-w-win-args (x-dat x-plot-left-gap x-min x-mag plot-area-width)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float x-dat x-mag x-min)
	   (FIxnum plot-area-width  x-plot-left-gap)
	   (values (UNSIGNED-BYTE 16)))
  (+ (the fn (round (* (/ (- x-dat x-min) x-mag)
		       plot-area-width)))
     x-plot-left-gap))

(proclaim '(inline x-plot-win-float-unbounded))
(defun x-plot-win-float-unbounded (x-dat win)
  (x-plot-win-float-unbounded-w-win-args x-dat (gv win :x-plot-left-gap) (gv win :x-min) (gv win :x-mag) (fn-gv win :plot-area-width)))

(proclaim '(inline x-plot-win-float-bounded-w-win-args))
(defun x-plot-win-float-bounded-w-win-args (x-dat x-min-limit x-max-limit x-bound-min x-bound-max width x-plot-left-gap x-min x-mag plot-area-width)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float x-dat x-min-limit x-max-limit)
	   (fixnum width x-bound-min x-bound-max)
	   (values (UNSIGNED-BYTE 16)))
  (cond ((> x-dat x-max-limit) (+ (the fn *x-plot-win-maximum-margin*) width))
	((> x-min-limit x-dat) *x-plot-win-minimum-margin*)
	(t (bound-int-val (x-plot-win-float-unbounded-w-win-args x-dat x-plot-left-gap x-min x-mag plot-area-width) x-bound-max x-bound-min))))

(proclaim '(inline x-plot-win-float-bounded))
(defun x-plot-win-float-bounded (x-dat win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values (UNSIGNED-BYTE 16)))
  (cond ((> x-dat (the sf (gv win :x-max-limit))) (+ (the fn *x-plot-win-maximum-margin*) (fn-gv win :width)))
	((> (the sf (gv win :x-min-limit)) x-dat) *x-plot-win-minimum-margin*)
	(t (bound-int-val (x-plot-win-float-unbounded x-dat win) (fn-gv win :x-bound-max) (fn-gv  win :x-bound-min)))))

(proclaim '(inline x-plot-win-float))
(defun x-plot-win-float (x-dat win &optional boundit)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values (UNSIGNED-BYTE 16)))
  (if boundit
      (x-plot-win-float-bounded x-dat win)
      (x-plot-win-float-unbounded x-dat win)))

(proclaim '(inline x-plot-win))
(defun x-plot-win (x-dat win &optional boundit)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (values (UNSIGNED-BYTE 16)))
  (let ((x-dat (typecase x-dat
		 (single-float x-dat)
		 (t (coerce x-dat 'single-float)))))
    (declare (single-float x-dat))
    (x-plot-win-float x-dat win boundit)))

(defun x-plot-win-distance (distance win) (abs (- (x-plot-win distance win nil) (x-plot-win 0.0 win nil))))

(defun x-plot-win-distance-float (distance win)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float distance))
  (abs (- (x-plot-win-float distance win nil) (x-plot-win-float 0.0 win nil))))

(defun x-plot-win-ok (x-dat win)
  (case (type-of (round (* (-  (-  (gv win :width)
				   (gv win :x-plot-right-gap))
			       (gv win :x-plot-left-gap))
			   (/ (- (gv win :x-max) (gv win :x-min))
			      (gv win :x-mag)))))
    ((UNSIGNED-BYTE 16) t)
    (fixnum nil)
    (t nil)))

(defun x-plot-win-inv (x-win win)
  (let ((width (gv win :width))
	(min (gv win :x-min))
	(mag (gv win :x-mag)))
    (+ min
       (* (/ mag (- (- width  (gv win :x-plot-right-gap)) (gv win :x-plot-left-gap)))
	  (- x-win (gv win :x-plot-left-gap))))))

;;;;;;;

(proclaim '(inline y-plot-win-float-unbounded-w-win-args))
(defun y-plot-win-float-unbounded-w-win-args (y-dat y-plot-bottom-gap y-min y-mag plot-area-height height)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float y-dat y-min y-mag)
	   (fixnum y-plot-bottom-gap height plot-area-height)
	   (values (UNSIGNED-BYTE 16)))
  (+ (the fn (round (* (the fn plot-area-height)
		       (/ (- y-min y-dat) y-mag))))
     (- height y-plot-bottom-gap)))

(proclaim '(inline y-plot-win-float-unbounded))
(defun y-plot-win-float-unbounded (y-dat win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (y-plot-win-float-unbounded-w-win-args y-dat (fn-gv win :y-plot-bottom-gap) (gv win :y-min) (gv win :y-mag) (fn-gv win :plot-area-height) (fn-gv win :height)))

(proclaim '(inline y-plot-win-float-bounded))
(defun y-plot-win-float-bounded (y-dat win)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values (UNSIGNED-BYTE 16)))
  (cond ((> y-dat (the sf (gv win :y-max-limit))) *y-plot-win-minimum-margin*)
	((> (the sf (gv win :y-min-limit)) y-dat) (+ *y-plot-win-maximum-margin* (fn-gv win :height)))
	(t (bound-int-val (y-plot-win-float-unbounded y-dat win) (fn-gv win :y-bound-max) (fn-gv win :y-bound-min)))))

(proclaim '(inline y-plot-win-float-bounded-w-win-args))
(defun y-plot-win-float-bounded-w-win-args (y-dat y-min-limit y-max-limit y-bound-min y-bound-max height y-plot-bottom-gap y-min y-mag plot-area-height)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float y-dat y-min-limit y-max-limit)
	   (fixnum y-bound-min y-bound-max height plot-area-height)
	   (values (UNSIGNED-BYTE 16)))
  (cond ((> y-dat y-max-limit) (the fn *y-plot-win-minimum-margin*))
	((> y-min-limit y-dat) (+ *y-plot-win-maximum-margin* height))
	(t (bound-int-val (y-plot-win-float-unbounded-w-win-args y-dat y-plot-bottom-gap y-min y-mag plot-area-height height) y-bound-max y-bound-min))))

(proclaim '(inline y-plot-win-float))
(defun y-plot-win-float (y-dat win &optional boundit)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values (UNSIGNED-BYTE 16)))
  (if boundit
      (y-plot-win-float-bounded y-dat win)
      (y-plot-win-float-unbounded y-dat win)))

(proclaim '(inline y-plot-win))
(defun y-plot-win (y-dat win &optional boundit)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (values (UNSIGNED-BYTE 16)))
  (let ((y-dat (typecase y-dat
		 (single-float y-dat)
		 (t (coerce y-dat 'single-float)))))
    (declare (single-float y-dat))
    (y-plot-win-float y-dat win boundit)))

(defun y-plot-win-distance (distance win) (abs (- (y-plot-win distance win nil) (y-plot-win-float 0.0 win nil))))

(defun y-plot-win-distance-float (distance win)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float distance))
  (abs (- (y-plot-win-float distance win nil) (y-plot-win-float 0.0 win nil))))

(defun y-plot-win-inv (y-win win)
  (let ((height (gv win :height))
	(label-height (gv win :label-height))
	(min (gv win :y-min))
	(mag (gv win :y-mag)))
    (+ min
       (* (/ mag (- (gv win :y-plot-bottom-gap) ; *y-plot-bottom-gap* FIXED, 8/27/94 lbg
		    (- height label-height)))
	  (- y-win (- height (gv win :y-plot-bottom-gap)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xy-plot-win-to-points (x-dat y-dat win) (list (x-plot-win x-dat win t) (y-plot-win y-dat win t)))

(s-value plot-window :x-graphics-win-function #'x-plot-win)
(s-value plot-window :y-graphics-win-function #'y-plot-win)

(defun plot-window-x-number-format (value win &key range decimals) (tidy-number-format value :range (or range (gv win :x-inc)) :decimals decimals))
(defun plot-window-y-number-format (value win &key range decimals) (tidy-number-format value :range (or range (gv win :y-inc)) :decimals decimals))

(defun a-bit-more-sub-domain (y-seqs how-much x-seqs x-min x-max win)
  (when x-min (setq x-min (s-flt x-min)))
  (when x-max (setq x-max (s-flt x-max)))
  (typecase (car y-seqs)
    (cons (loop for y-seq in y-seqs maximize (a-bit-more-sub-domain y-seq how-much x-seqs x-min x-max win)))
    (t (let ((max (if (numberp x-seqs)
		      (loop for x from (or (gv win :delta-t-start) 0.0) by x-seqs
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) maximize y)
		      (loop for x in (car x-seqs)
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) maximize y))))
	 (+ max (abs (* how-much max)))))))

(defun a-bit-less-sub-domain (y-seqs how-much x-seqs x-min x-max win)
  (when x-min (setq x-min (s-flt x-min)))
  (when x-max (setq x-max (s-flt x-max)))
  (typecase (car y-seqs)
    (cons (loop for y-seq in y-seqs minimize (a-bit-less-sub-domain y-seq how-much x-seqs x-min x-max win)))
    (t (let ((min (if (numberp x-seqs)
		      (loop for x from (or (gv win :delta-t-start) 0.0) by x-seqs
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) minimize y)
		      (loop for x in (car x-seqs)
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) minimize y))))
	 (+ min (abs (* how-much min)))))))

(defun check-plot-log-parameters (win)
  ;; Signals an error if a dimension that is to be logarithmically displayed contains a value less than or equal to 0.
  (or
   (and (or (not (gv win :x-log)) (positive-p (gv win :x-overall-data-min)))
	(or (not (gv win :y-log)) (positive-p (gv win :y-overall-data-min))))
   (cond-every
    ((and (gv win :x-log) (<= (gv win :x-data-min) 0.0))
     (let ((dummy1 nil)
	   (dummy2 (* 1.000001 (gv win :x-data-scale) (- (gv win :x-data-offset) (gv win :x-data-min)))))
       (choose-variable-values
	`((:comment ,(format nil "Minimum X value: ~A" (- (gv win :x-data-offset) (gv win :x-data-min))))
	  (dummy2 "X offset" :float)
	  (dummy1 "Cancel X log" :boolean))
	:title (format nil "X Log error in ~A" (gv win :title)))
       (cond ((not dummy1)
	      (s-value win :X-MAX-MIN-SPECIFIED nil)
	      (s-value win :x-data-offset dummy2))
	     (t (s-value win :x-log nil)))))
    ((and (gv win :y-log) (<= (gv win :y-data-min) 0.0))
     (let ((dummy1 nil)
	   (dummy2 (* 1.000001 (gv win :y-data-scale) (- (gv win :y-data-offset) (gv win :y-data-min)))))
       (choose-variable-values
	`((:comment ,(format nil "Minimum Y value: ~A" (- (gv win :y-data-offset) (gv win :y-data-min))))
	  (dummy2 "Y offset" :float)
	  (dummy1 "Cancel Y log" :boolean))
	:title (format nil "Y Log error in ~A" (gv win :title)))
       (cond ((not dummy1)
	      (s-value win :Y-MAX-MIN-SPECIFIED nil)
	      (s-value win :y-data-offset dummy2))
	     (t (s-value win :y-log nil)))))
    (t nil))))

(defun num-curves-per-group (win) (apply 'max (mapcar 'length (gv win :y-lists))))

(defun num-groups (win) (length (gv win :y-lists)))

(defun get-plot-window-trace-order (win) (fix-list (or (gv win :trace-order) (list-of-ints (num-curves-per-group win)))))

(defun parse-plot-y-seqs (win &optional (group-index 0))
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference collect (nth (round trace-reference) (nth group-index (gv win :y-lists)))))

(defun parse-all-plot-x-seqs (win)
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference nconc
	(loop for collection in (gv win :x-lists) collect
	      (if (consp (car collection))
		  (nth (round trace-reference) collection)
		  collection))))

(defun parse-all-plot-y-seqs (win)
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference nconc
	(loop for collection in  (gv win :y-lists) collect
	      (nth (round trace-reference) collection))))

(defun first-delta-t (win)  (or (gv win :delta-t-start) 0))

#|
(defun last-delta-t (win x-seqs)
  (+ (or (gv win :delta-t-start) 0)
     (* (1- (gv win :data-length)) x-seqs)))
|#

#|
This reworking of the math is necessary in order to match the x-sequence algorithm based on delta-t in
GET-PLOT-POINT-LIST. Otherwise, one could get the following
* (progn (format t "count  (+ -20  (* count 9.998)) (+ -20 9.998 ...)~%")
           (loop for factor from 0 to 6 do (format t "~d              ~a       ~a~%"
						   factor (+ -20  (* factor 9.998))
						   (car (last (loop for x from -20 by 9.998 for count from 0 to factor collect x)))
						   )))
count  (+ -20  (* count 9.998)) (+ -20 9.998 ...)
0              -20.0       -20
1              -10.002       -10.002
2              -0.00399971       -0.00399971
3              9.9939995       9.994
4              19.992       19.992
5              29.990002       29.990002
6              39.988       39.988003
NIL
|#

(defun last-delta-t (win x-seqs)
  (loop for time from (or (gv win :delta-t-start) 0) by x-seqs
	for count from 1 to (1- (gv win :data-length))
	finally (return (get-nice-mag time))))

#|
(defun x-data-max (win &optional all)
  (let ((x-seqs (if nil (parse-all-plot-x-seqs win) (car (gv win :x-lists)))))
    (* (gv win :x-data-scale)
       (+ (gv win :x-data-offset)
	  (if (numberp x-seqs)
	      (if (> (gv win :x-data-scale) 0) (last-delta-t win x-seqs) (first-delta-t win))
	      (if (> (gv win :x-data-scale) 0)
		  (a-bit-more x-seqs)	; Just the actual max.
		  (a-bit-less x-seqs)	; Just the actual min if scale is negative.
		  ))))))

(defun x-data-min (win &optional all)
  (let ((x-seqs (if nil (parse-all-plot-x-seqs win) (car (gv win :x-lists)))))
    (* (gv win :x-data-scale)
       (+ (gv win :x-data-offset)
	  (if (numberp x-seqs)
	      (if (> (gv win :x-data-scale) 0) (first-delta-t win) (last-delta-t win x-seqs))
	      (if (> (gv win :x-data-scale) 0)
		  (a-bit-less x-seqs)	; Just the actual min.
		  (a-bit-more x-seqs)	; Just the actual max if the scale is negative
		  ))))))

(defun y-data-max (win display-sub-domain x-min-spec x-max-spec &optional (all (gv win :accomodate-overlays)))
  (let ((x-seqs (car (gv win :x-lists)))
	(y-seqs (if all (parse-all-plot-y-seqs win) (parse-plot-y-seqs win))))
    (* (gv win :y-data-scale)
       (+ (gv win :y-data-offset)
	  (if (> (gv win :y-data-scale) 0)
	      (if display-sub-domain
		  (a-bit-more-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
		  (a-bit-more y-seqs)	; Just the actual max.
		  )
	      (if display-sub-domain
		  (a-bit-less-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
		  (a-bit-less y-seqs)	; Just the actual min.
		  ))))))

(defun y-data-min (win display-sub-domain x-min-spec x-max-spec &optional (all (gv win :accomodate-overlays)))
  (let ((x-seqs (car (gv win :x-lists)))
	(y-seqs (if all (parse-all-plot-y-seqs win) (parse-plot-y-seqs win))))
    (* (gv win :y-data-scale)
       (+ (gv win :y-data-offset)
	  (if (> (gv win :y-data-scale) 0)
	      (if display-sub-domain
		  (a-bit-less-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
		  (a-bit-less y-seqs)	; Just the actual min.
		  )
	      (if display-sub-domain
		  (a-bit-more-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
		  (a-bit-more y-seqs)	; Just the actual max.
		  ))))))
|#

(defun x-data-extremum (win maxp)
  (let* ((x-seqs (car (gv win :x-lists)))
	 (operator (if maxp #'> #'<))
	 (extremum (* (gv win :x-data-scale)
		      (+ (gv win :x-data-offset)
			 (if (numberp x-seqs)
			     (if (funcall operator (gv win :x-data-scale) 0) (last-delta-t win x-seqs) (first-delta-t win))
			     (if (funcall operator (gv win :x-data-scale) 0)
				 (a-bit-more x-seqs) ; Just the actual max.
				 (a-bit-less x-seqs))))))) ; Just the actual min if scale is negative.
    extremum))

(defun x-data-max (win &optional all) (x-data-extremum win t))
(defun x-data-min (win &optional all) (x-data-extremum win nil))

(defun y-data-extremum (win display-sub-domain x-min-spec x-max-spec all maxp)
  (let* ((x-seqs (car (gv win :x-lists)))
	 (y-seqs (if all (parse-all-plot-y-seqs win) (parse-plot-y-seqs win)))
	 (operator (if maxp #'> #'<))
	 (extremum (* (gv win :y-data-scale)
		     (+ (gv win :y-data-offset)
			(if (funcall operator (gv win :y-data-scale) 0)
			    (if display-sub-domain
				(a-bit-more-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
				(a-bit-more y-seqs) ; Just the actual max.
				)
			    (if display-sub-domain
				(a-bit-less-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
				(a-bit-less y-seqs))))))) ; Just the actual min.
    extremum))

(defun y-data-max (win display-sub-domain x-min-spec x-max-spec &optional (all (gv win :accomodate-overlays)))
  (y-data-extremum win display-sub-domain x-min-spec x-max-spec all t))

(defun y-data-min (win display-sub-domain x-min-spec x-max-spec &optional (all (gv win :accomodate-overlays)))
  (y-data-extremum win display-sub-domain x-min-spec x-max-spec all nil))

(defun setup-xy-data-limits (win display-sub-domain timed-data x-min-spec x-max-spec)
  ;; Sets :X-DATA-MAX, :X-DATA-MIN, :Y-DATA-MAX, :Y-DATA-MIN, and if :AUTO-WF-SETUP, :X-TRACE-OFFSET, :Y-TRACE-OFFSET and
  ;; :WATERFALL-LABEL-OFFSET. Returns when LOGS-OK.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when (and (gv win :timed-data) (< (x-data-min win) 0))
    (s-value win :timed-data nil) (setq timed-data nil))
  (let ((display-sub-domain (and display-sub-domain (or (not timed-data) (and x-min-spec (> x-min-spec 0.0)))))
	logs-ok)
    (s-value win :prior-x-data-max (gv win :x-data-max))
    (s-value win :prior-x-data-min (gv win :x-data-min))
    (s-value win :prior-y-data-max (gv win :y-data-max))
    (s-value win :prior-y-data-min (gv win :y-data-min))
    (loop until logs-ok do
	  (s-value win :x-data-max (x-data-max win))
	  (s-value win :x-data-min (x-data-min win))
	  (s-value win :x-overall-data-min (x-data-min win t))
	  (setq display-sub-domain (or display-sub-domain (and x-min-spec (> x-min-spec (gv win :x-data-min)))))
	  (s-value win :y-data-max (y-data-max win display-sub-domain x-min-spec x-max-spec))
	  (s-value win :y-data-min (y-data-min win display-sub-domain x-min-spec x-max-spec))
	  (s-value win :y-overall-data-min (y-data-min win display-sub-domain x-min-spec x-max-spec t))

	  (when (gv win :auto-wf-setup)
	    (when *waterfall-fixed-y-max* (s-value win :y-data-max *waterfall-fixed-y-max*))
	    (when *waterfall-fixed-y-min* (s-value win :y-data-min *waterfall-fixed-y-min*))

	    ;; (s-value win :x-trace-offset *x-trace-offset*)
	    (s-value win :x-trace-offset (* (- 1.0 (bound-val (or (gv win :auto-waterfall-x-trace-overlap) *auto-waterfall-x-trace-overlap*) 1.0 0.0))
					    (- (gv win :x-data-max) (gv win :x-data-min))))
	    (s-value win :y-trace-offset (* (- 1.0 (bound-val (or (gv win :auto-waterfall-y-trace-overlap) *auto-waterfall-y-trace-overlap*) 1.0 0.0))
					    (- (gv win :y-data-max) (gv win :y-data-min))))
					; (s-value win :waterfall-label-offset 0.0)
	    )
	  ;; If log used, test for a bad log domain value - menu if so...
	  (setq logs-ok (check-plot-log-parameters win)))))

(defun set-xy-max/min-spec-and-origins (win replot-win-point-list restore-plot unzoom X-INC X-MAX-SPEC X-MIN-SPEC X-ORIGIN Y-INC Y-MAX-SPEC Y-MIN-SPEC Y-ORIGIN)
  ;; In case of a REPLOT-WIN-POINT-LIST, RESTORE-PLOT, or UNZOOM, the X/Y specified max/mins must be set.
  (s-value win :X-MAX-MIN-SPECIFIED (or x-max-spec x-min-spec))
  (s-value win :Y-MAX-MIN-SPECIFIED (or y-max-spec y-min-spec))
  (cond (replot-win-point-list		; Converted mouse coordinates (left top width height) to data coordinates.
	 (let* ((replot-x-min (x-plot-win-inv (first replot-win-point-list) win))
		(replot-x-max (x-plot-win-inv (+ (first replot-win-point-list) (third replot-win-point-list)) win))
		(replot-y-min (y-plot-win-inv (+ (second replot-win-point-list) (fourth replot-win-point-list)) win))
		(replot-y-max (y-plot-win-inv (second replot-win-point-list) win)))
	   (if (or (= replot-x-min replot-x-max) (= replot-y-min replot-y-max)) ; Avoid over zooming - see below
	       (setq x-min-spec (gv win :x-min) x-max-spec (gv win :x-max)
		     y-min-spec (gv win :y-min) y-max-spec (gv win :y-max))
	       (let* ((x-seqs (car (gv win :x-lists)))
		      (min-x-mag (if (numberp x-seqs) ; This means a :DELTA-T spec
				     x-seqs
				     (/ (- (car (last (car x-seqs))) (first (car x-seqs)))
					(gv win :data-length))))
		      (min-maxs (list (gv win :x-min) (gv win :y-min)
				      (gv win :x-max) (gv win :y-max))))
		 (unless (eq min-maxs (car (gv win :min-max-lists))) ; Save if new values are really new.
		   (push min-maxs (gv win :min-max-lists)))
		 (setq x-min-spec replot-x-min
		       y-min-spec replot-y-min
		       x-max-spec replot-x-max
		       y-max-spec replot-y-max)
		 (when nil		; (< (- x-max-spec x-min-spec) min-x-mag) ; Avoid too small zooming.
		   (if (= x-min-spec (gv win :orig-x-min))
		       (setq x-max-spec (+ x-min-spec min-x-mag))
		       (setq x-min-spec (- x-max-spec min-x-mag))))))))
	((or restore-plot unzoom)
	 (let ((min-maxs (pop (gv win :min-max-lists))))
	   (if (and unzoom min-maxs)
	       (setq x-min-spec (first min-maxs) y-min-spec (second min-maxs)
		     x-max-spec (third min-maxs) y-max-spec (fourth min-maxs))
	       (setq x-min-spec (gv win :orig-x-min) x-max-spec (gv win :orig-x-max)
		     y-min-spec (gv win :orig-y-min) y-max-spec (gv win :orig-y-max)
		     x-origin (gv win :orig-x-origin) x-inc (gv win :orig-x-axis-inc)
		     y-origin (gv win :orig-y-origin) y-inc (gv win :orig-y-axis-inc)))))
	((gv win :auto-wf-setup)
	 (s-value win :y-plot-top-gap-extra *default-y-plot-top-gap-extra-waterfall*)
	 (s-value win :waterfall-base-y-offset 0.0)
	 (s-value win :waterfall-base-x-offset 0.0)
	 (s-value win :x-data-scale 1.0) (s-value win :y-data-scale 1.0)
	 (s-value win :x-data-offset 0.0) (s-value win :y-data-offset 0.0)))
  (values x-min-spec x-max-spec y-min-spec y-max-spec x-origin y-origin))


(defun setup-plot (win &key width height preserve-plot-attributes reference-ticks-to-origin wf-skirt fix-to-unity-mag-if-so
			 x-inc x-min-spec x-max-spec x-log x-label x-label-v-position x-label-h-position default-x-incs
			 y-inc y-min-spec y-max-spec y-log y-label y-label-v-position (y-label-h-position :left)  default-y-incs
			 invert-y-axis-label x-are-fns y-are-fns (include-x-tick-at-0 t) (include-y-tick-at-0 t)
			 (x-origin-tick nil) (y-origin-tick t) x-origin y-origin  x-axis-root y-axis-root
			 (x-trace-offset 0.0) (y-trace-offset 0.0) (x-data-offset 0.0) (y-data-offset 0.0)
			 (consider-labels t) unzoom restore-plot replot-win-point-list revise-plot)
  ;; Sets up the plotting window parameters according to the plotted data.
  ;; This can accept data in one of two formats:
  ;;
  ;;   ** As used by PLOT-XY-DATA **
  ;;
  ;;  data-lists =
  ;;
  ;;  '(((x1 x1 ... x1)(y1 y1 ... y1))
  ;;    ((x2 x2 ... x2)(y2 y2 ... y2))
  ;;    ...
  ;;    ((xn xn ... xn)(yn yn ... yn))
  ;;   )
  ;;
  ;;  time-base = nil
  ;;
  ;; ** As used by PLOT-TIMED-DATA **
  ;;
  ;;  data-lists =
  ;;
  ;;  '((x1 x1 ... x1)
  ;;    (x2 x2 ... x2)
  ;;    ...
  ;;    (xn xn ... xn)
  ;;   )
  ;;
  ;;  time-base = '(t0 t1 t2 ..) or an equivalent simple-array.
  ;;
  ;; Also draws labelled axes.
  ;;
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when (gv win :x-axis-coeff) (s-value win :x-axis-coeff (s-flt (gv win :x-axis-coeff))))
  (when (gv win :y-axis-coeff) (s-value win :y-axis-coeff (s-flt (gv win :y-axis-coeff))))
  (setup-plot-preliminaries win preserve-plot-attributes wf-skirt width height consider-labels
			    x-label-h-position x-label-v-position
			    y-label-h-position y-label-v-position
			    x-label y-label (or restore-plot unzoom replot-win-point-list)
			    x-origin-tick y-origin-tick reference-ticks-to-origin y-trace-offset x-trace-offset x-are-fns y-are-fns
			    include-x-tick-at-0 include-y-tick-at-0 invert-y-axis-label)
  ;;  (format t "original axis-root ~A~%" (gv win :y-axis-root))
  (wh::s-values-IF-NON-NIL win default-x-incs default-y-incs x-axis-root y-axis-root)

  (cond-every ((gv win :auto-x-scaling) (setq x-inc nil x-max-spec nil x-min-spec nil x-origin nil))
	      ((gv win :auto-y-scaling) (setq y-inc nil y-max-spec nil y-min-spec nil y-origin nil)))
  (let* ((replotting (or replot-win-point-list restore-plot unzoom))
	 (y-seqs (parse-plot-y-seqs win))
	 (x-seqs (car (gv win :x-lists)))
	 (accomodate-all (and (> (number-of-overlays win) 1) (gv win :overlay) (gv win :accomodate-overlays))) ; Only OK for windows that have data.
	 display-sub-domain)
;;    (printvars y-seqs x-seqs)
    (s-value win :data-length (apply 'max (mapcar 'length y-seqs)))
    (multiple-value-bind (x-min-spec x-max-spec y-min-spec y-max-spec x-origin y-origin)
	(set-xy-max/min-spec-and-origins win replot-win-point-list restore-plot unzoom X-INC X-MAX-SPEC X-MIN-SPEC X-ORIGIN Y-INC Y-MAX-SPEC Y-MIN-SPEC Y-ORIGIN)
      (unless replotting
	(s-values win x-log y-log)
	(unless (gv win :auto-wf-setup) (s-values win x-data-offset y-data-offset)))
      (when (and (gv win :timed-data) (not (gv win :x-log))) ; This is to make the end of the time a little neater.
	(let ((plot-window-top-x (plot-window-top-x win)))
	  (setq x-max-spec (or x-max-spec plot-window-top-x)
		display-sub-domain (< x-max-spec plot-window-top-x))))
      ;; If X-MIN-SPEC is not specified and the minimum value over the x-sequences is 0, then fix X-MIN-SPEC = 0.
      (unless x-min-spec (if (numberp x-seqs)
			     (setq x-min-spec (or (gv win :delta-t-start) 0))
			     (when (= (a-bit-less x-seqs) 0) (setq x-min-spec 0.0))))
      ;; Sets :X-DATA-MAX, :X-DATA-MIN, :Y-DATA-MAX, :Y-DATA-MIN, and, if :AUTO-WF-SETUP, :X-TRACE-OFFSET, :Y-TRACE-OFFSET and
      ;; :WATERFALL-LABEL-OFFSET. Returns when LOGS-OK.
      (setup-xy-data-limits win display-sub-domain (gv win :timed-data) x-min-spec x-max-spec)
      (let* ((log-base (or (gv win :log-base) e))
	     (number-of-traces (length (or (gv win :trace-order) y-seqs)))
	     (xfrmd-x-data-max (if (gv win :x-log) (log (gv win :x-data-max) log-base) (gv win :x-data-max)))
	     (xfrmd-x-data-min (if (gv win :x-log) (log (gv win :x-data-min) log-base) (gv win :x-data-min)))
	     (xfrmd-x-data-mag (abs (- xfrmd-x-data-max xfrmd-x-data-min)))
	     (a-bit-more-than-the-x (+ (if accomodate-all (max (gv win :x-axis-max) xfrmd-x-data-max) xfrmd-x-data-max)
				       (if (gv win :timed-data) 0.0 (* *plot-data-a-bit-more-than-the-x-or-y-coeff* xfrmd-X-data-mag))))
	     (a-bit-less-than-the-x (- (if accomodate-all (min (gv win :x-axis-min) xfrmd-x-data-min) xfrmd-x-data-min)
				       (if (gv win :timed-data) 0.0 (* *plot-data-a-bit-more-than-the-x-or-y-coeff* xfrmd-X-data-mag))))
	     (x-axis-max (if (and (not (gv win :waterfall)) x-max-spec)
			     x-max-spec
			     (if (gv win :x-are-fns) (ceiling a-bit-more-than-the-x) a-bit-more-than-the-x)))
	     (x-axis-min (if (and (not (gv win :waterfall)) x-min-spec)
			     x-min-spec
			     (if (and (not (gv win :x-log)) (or (numberp x-seqs) (gv win :timed-data)))
				 (or (gv win :delta-t-start) 0)
				 (if (gv win :x-are-fns) (floor a-bit-less-than-the-x) a-bit-less-than-the-x))))
	     (x-max (setup-plot-x-max win x-axis-max number-of-traces))
	     (x-min (setup-plot-x-min win x-axis-min number-of-traces))
	     (xfrmd-y-data-max (if (gv win :y-log) (log (gv win :y-data-max) log-base) (gv win :y-data-max)))
	     (xfrmd-y-data-min (if (gv win :y-log) (log (gv win :y-data-min) log-base) (gv win :y-data-min)))
	     (xfrmd-y-data-mag (abs (- xfrmd-y-data-max xfrmd-y-data-min)))
	     (a-bit-more-than-the-y (+ (if accomodate-all
					   (max xfrmd-y-data-max (if (gv win :prior-flat-y-data) (gv win :prior-y-data-max) (gv win :y-axis-max)))
					   xfrmd-y-data-max)
				       (* *plot-data-a-bit-more-than-the-x-or-y-coeff* xfrmd-y-data-mag)))
	     (a-bit-less-than-the-y (- (if accomodate-all
					   (min xfrmd-y-data-min (if (gv win :prior-flat-y-data) (gv win :prior-y-data-min) (gv win :y-axis-min)))
					   xfrmd-y-data-min)
				       (* *plot-data-a-bit-more-than-the-x-or-y-coeff*  xfrmd-y-data-mag)))
	     (y-axis-max (cond ((and fix-to-unity-mag-if-so (and (= 1.0 xfrmd-y-data-mag) (= 1.0 xfrmd-y-data-max))) 1.0)
			       ((and (not (gv win :waterfall)) y-max-spec) y-max-spec)
			       (t (if (gv win :y-are-fns) (ceiling a-bit-more-than-the-y) a-bit-more-than-the-y))))
	     (y-axis-min (cond ((and fix-to-unity-mag-if-so (and (= 1.0 xfrmd-y-data-mag) (= 0.0 xfrmd-y-data-min))) 0.0)
			       ((and (not (gv win :waterfall)) y-min-spec) y-min-spec)
			       (t (if (gv win :y-are-fns) (floor a-bit-less-than-the-y) a-bit-less-than-the-y))))
	     (y-max (setup-plot-y-max win y-axis-max number-of-traces))
	     (y-min (setup-plot-y-min win y-axis-min number-of-traces)))
	;;	(format t "y-axis-max ~A, y-axis-min ~A~%" y-axis-max y-axis-min)
	(s-values win xfrmd-x-data-min xfrmd-x-data-max xfrmd-y-data-min xfrmd-y-data-max)
	(s-value win :xfrmd-x-data-mag (- xfrmd-x-data-max xfrmd-x-data-min))
	(s-value win :xfrmd-y-data-mag (- xfrmd-y-data-max xfrmd-y-data-min))
	(when (gv win :waterfall)
	  (unless (eq (gv win :axes-type) :none) (s-value win :axes-type :simple))
	  (if (> (gv win :y-trace-offset) 0)
	      (setq y-axis-max y-max)
	      (when (< (gv win :y-trace-offset) 0) (setq y-axis-min y-min)))
	  (if (> (gv win :x-trace-offset) 0)
	      (setq x-axis-max x-max)
	      (when (< (gv win :x-trace-offset) 0) (setq x-axis-min x-min))))
	(multiple-value-bind (x-origin y-origin) ; For flat data near zero these may be altered by SET-PLOT-X-Y-MAX-MIN.
	    (set-plot-x-y-max-min win x-max y-max x-axis-max y-axis-max x-min y-min x-axis-min y-axis-min x-origin y-origin)
	  (set-plot-incs-origin-label-position win x-origin y-origin x-inc y-inc x-label-v-position x-label-h-position))
	(unless (or replotting revise-plot) (set-plot-win-orig-parameters win)) ; This is a first time plot.
	(set-plot-gaps win)
	(draw-all-axes win)
	(s-value win :auto-x-scaling nil) (s-value win :auto-y-scaling nil)))
    (when (or (<= (gv win :x-max) (gv win :x-min))
	      (<= (gv win :y-max) (gv win :y-min)))
      (sim-error (format nil "Plot window ~A was specified with bogus ~A" (gv win :title)
			 (cond ((and (<= (gv win :x-max) (gv win :x-min)) (<= (gv win :y-max) (gv win :y-min)))
				(format nil "X/Y max (~a/~a) min (~A/~a) values.~%" (gv win :x-max) (gv win :y-max) (gv win :x-min) (gv win :y-min)))
			       ((<= (gv win :x-max) (gv win :x-min)) (format nil "X max (~a) min (~a) values.~%" (gv win :x-max) (gv win :x-min)))
			       (T (format nil "Y max (~a) min (~a) values.~%" (gv win :Y-max) (gv win :Y-min)))))))
    (s-value win :prior-flat-y-data (gv win :flat-y-data))
    (s-value win :prior-flat-x-data (gv win :flat-x-data))
    (s-value win :has-been-setup t)))

(defun setup-plot-x-max (win x-axis-max number-of-traces)
  (+ x-axis-max (if (and (gv win :waterfall) (> (gv win :x-trace-offset) 0))
		    (* (gv win :x-trace-offset) (1- number-of-traces))
		    0.0)))

(defun setup-plot-x-min (win x-axis-min number-of-traces)
  (+ x-axis-min (if (and (gv win :waterfall) (< (gv win :x-trace-offset) 0))
		    (* (gv win :x-trace-offset) (1- number-of-traces))
		    0.0)))

(defun setup-plot-y-min (win y-axis-min number-of-traces)
  (+ y-axis-min (if (and (gv win :waterfall) (< (gv win :y-trace-offset) 0))
		    (* (gv win :y-trace-offset) (1- number-of-traces))
		    0.0)))

(defun setup-plot-y-max (win y-axis-max number-of-traces)
  (+ (if (and (gv win :waterfall) (gv win :waterfall-y-data-max) (gv win :use-waterfall-y-data-max))
	 (gv win :waterfall-y-data-max)
	 y-axis-max)
     (if (and (gv win :waterfall) (> (gv win :y-trace-offset) 0))
	 (* (gv win :y-trace-offset) (1- number-of-traces))
	 0.0)))

(defun setup-plot-preliminaries (win preserve-plot-attributes wf-skirt width height
				 consider-labels x-label-h-position x-label-v-position y-label-h-position y-label-v-position
				 x-label y-label dont-reset-labels x-origin-tick y-origin-tick reference-ticks-to-origin
				 y-trace-offset x-trace-offset x-are-fns y-are-fns
				 include-x-tick-at-0 include-y-tick-at-0 invert-y-axis-label)
  (unless preserve-plot-attributes (s-value win :label-traces consider-labels))
  (s-values-if-non-nil win width height x-label-v-position x-label-h-position y-label-v-position y-label-h-position)
  (s-values-s-flt-or-0 win y-trace-offset x-trace-offset)
  (unless dont-reset-labels
    (s-values win
	      (x-label (when x-label (if (stringp x-label) x-label (format nil "~A" x-label))))
	      (y-label (when y-label (if (stringp y-label) y-label (format nil "~A" y-label))))))
  (s-values win
	    wf-skirt reference-ticks-to-origin x-origin-tick y-origin-tick include-x-tick-at-0 include-y-tick-at-0 invert-y-axis-label
	    (waterfall-base-x-offset (s-flt (or (gv win :waterfall-base-x-offset) 0.0)))
	    (waterfall-base-y-offset (s-flt (or (gv win :waterfall-base-y-offset) 0.0)))
	    (x-are-fns (or *force-plot-x-fixnums* x-are-fns))
	    (y-are-fns (or *force-plot-y-fixnums* y-are-fns))))

(defun plot-window-top-x-lists (win) (car (gv win :x-lists)))
(defun plot-window-top-y-lists (win) (car (gv win :y-lists)))

(defun plot-window-top-x (win)
  (let* ((x-seqs (plot-window-top-x-lists win))
	 (y-seqs (plot-window-top-y-lists win))
	 (data-length (1- (loop for y-seq in y-seqs maximize (length y-seq)))))
    (get-nice-mag (if (numberp x-seqs)
		      (+ (or (gv win :delta-t-start) 0) (* data-length x-seqs))
		      (max-of-list (car x-seqs))))))

(defun set-plot-x-y-max-min (win x-max y-max x-axis-max y-axis-max x-min y-min x-axis-min y-axis-min x-origin y-origin)
  ;; Sets :X-MIN, :X-MAX, :X-MAG, :X-MIN-LIMIT, :X-MAX-LIMIT, :Y-MIN, :Y-MAX, :Y-MAG, :Y-MIN-LIMIT, :Y-MAX-LIMIT, :X-AXIS-MIN, :X-AXIS-MAX, :Y-AXIS-MIN,
  ;; :Y-AXIS-MAX. Note that :X-MAX, :X-MIN, :Y-MAX and :Y-MIN are eventually set by the corresponding :*-AXIS-* values. The appropriate values as supplied
  ;; by the function args are only changed when there is flat data. In addition, unless :*-MAX-MIN-SPECIFIED, all of these values are cleaned up with
  ;; GET-NICE-MAG.

  ;; :x-min, :x-max, :y-min, :y-max now track :x-axis-min, :x-axis-max, :y-axis-min, :y-axis-max

  ;; SET-PLOT-X-Y-MAX-MIN returns as values x-origin and y-origin, since these may be changed within and thus need to be sent to SET-PLOT-INCS-ORIGIN-LABEL-POSITION.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((x-max (s-flt x-max))
	(y-max (s-flt y-max))
	(x-axis-max (s-flt x-axis-max))
	(y-axis-max (s-flt y-axis-max))
	(x-min (s-flt x-min))
	(y-min (s-flt y-min))
	(x-axis-min (s-flt x-axis-min))
	(y-axis-min (s-flt y-axis-min)))
    (declare (single-float x-max y-max x-axis-max y-axis-max x-min y-min x-axis-min y-axis-min))
    (if (/= y-max y-min)		; Check for flat data
	(s-value win :flat-y-data nil)
	(let ((y-actual-max y-max)
	      (zero-y (= y-max 0))
	      (positive-y (> y-max 0)))
	  (s-value win :flat-y-data t)
	  (setq y-max (if zero-y 1.0 (+ y-actual-max (* (if positive-y 0.5 -0.5) y-actual-max)))
		y-min (if zero-y -1.0 (+ y-actual-max (* (if positive-y -0.5 0.5) y-actual-max)))
		y-axis-max y-max
		y-axis-min y-min)
	  (if (not zero-y)
	      (s-value win :y-origin (setq y-origin (if positive-y y-min y-max)))
	      (cond ((> *plotted-constant-value-threshold-for-shifting-origin* y-actual-max 0)
		     (setq y-origin (or y-origin (- *plotted-constant-value-threshold-for-shifting-origin*)))
		     (s-values win y-origin))
		    ((< (- *plotted-constant-value-threshold-for-shifting-origin*) y-actual-max 0)
		     (setq y-origin (or y-origin *plotted-constant-value-threshold-for-shifting-origin*))
		     (s-values win y-origin))))))
    (if (/= x-max x-min)
	(s-value win :flat-x-data nil)
	(progn (s-value win :flat-x-data t)
	       (setq x-max (+ 1.0 x-max) x-min (- x-min 1.0) x-axis-max x-max x-axis-min x-min)))
    (s-values win
	      (y-axis-max		; (GET-AXIS-MAX y-origin y-axis-max y-max y-min :y win)
	       (s-flt (if y-origin
			  (max y-origin (if (gv win :Y-MAX-MIN-SPECIFIED) y-axis-max (get-nice-mag y-axis-max (- y-max y-min))))
			  (if (gv win :Y-MAX-MIN-SPECIFIED) y-axis-max (get-nice-mag y-axis-max (- y-max y-min))))))
	      (y-axis-min (s-flt (if y-origin
				     (min y-origin (if (gv win :Y-MAX-MIN-SPECIFIED) y-axis-min (get-nice-mag y-axis-min (- y-max y-min))))
				     (if (gv win :Y-MAX-MIN-SPECIFIED) y-axis-min (get-nice-mag y-axis-min (- y-max y-min))))))
	      (x-axis-max (s-flt (if x-origin
				     (max x-origin (if (gv win :X-MAX-MIN-SPECIFIED) x-axis-max (get-nice-mag x-axis-max (- x-max x-min))))
				     (if (gv win :X-MAX-MIN-SPECIFIED) x-axis-max (get-nice-mag x-axis-max (- x-max x-min))))))
	      (x-axis-min (s-flt (if x-origin
				     (min x-origin (if (gv win :X-MAX-MIN-SPECIFIED) x-axis-min (get-nice-mag x-axis-min (- x-max x-min))))
				     (if (gv win :X-MAX-MIN-SPECIFIED) x-axis-min (get-nice-mag x-axis-min (- x-max x-min))))))
	      (y-max y-axis-max) (y-min y-axis-min) (y-mag (- y-axis-max y-axis-min))

	      (y-min-limit (- y-axis-min 0 ; (gv win :y-mag)
			      ))
	      (y-max-limit (+ y-axis-max 0 ; (gv win :y-mag)
			      ))

	      (x-max x-axis-max) (x-min x-axis-min) (x-mag (- x-axis-max x-axis-min))

	      (x-min-limit (- x-axis-min 0 ; (gv win :x-mag)
			      ))
	      (x-max-limit (+ x-axis-max 0 ; (gv win :x-mag)
			      ))
	      )
    (values x-origin y-origin)))

(defun get-axis-max (origin axis-max max min axis win)
  (let ((origin-slot (gv win (generic-to-axis axis :origin)))
	(MAX-MIN-SPECIFIED-slot (gv win (generic-to-axis axis :MAX-MIN-SPECIFIED))))
    (if origin
      (max origin-slot (if MAX-MIN-SPECIFIED-slot axis-max (get-nice-mag axis-max (- max min))))
      (if MAX-MIN-SPECIFIED-slot axis-max (get-nice-mag axis-max (- max min))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Plot gap/layout functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-plot-gaps (win)
  (set-plot-label-height win)
  (set-plot-left-gap win)
  (set-plot-right-gap win)
  (set-plot-bottom-gap win)
  (set-plot-area-hw win))

(defun set-plot-label-height (win)
  (loop for y-seq in (car (gv win :y-lists))
	for label in (gv win :label-list)
	when (> (length label) 0) collect y-seq into y-seqs
	finally
	(let ((number-of-curves (if (gv win :trace-order)
				    (min (length (gv win :trace-order)) (length y-seqs))
				    (length y-seqs))))
	  (s-value win :label-height ;; :LABEL-HEIGHT has to take into account both the trace labels and the y-axis label,
		   (if (and (gv win :use-fixed-top-gap) (numberp (gv win :fixed-top-gap)))
		       (round (gv win :fixed-top-gap))
		       (+ (or (gv win :y-plot-top-gap-extra) 0)
			  ;; For either the Y axis label or the X axis tick marks
			  (if (gv win :waterfall)
			      0
			      (+ (y-label-near-trace-labels win)
				 (case (gv win :x-label-v-position)
				   ;; For the X axis tick marks
				   (:ABOVE ;   ******************  LBG Check this  may 10 2001
				    ;; only account for x marks if ordinate is near top
				    (if (< (/ (- (gv win :y-origin) (gv win :y-min)) (gv win :y-mag)) 0.8)
					(+ 5 (gv (window-plot-axis-font win) :font-height))
					(+ ; 13
					 *gap-btwn-x-label-and-tick-marks*
					 (* 2 (gv (window-plot-axis-font win) :font-height)))))
				   ;; Just for the Y axis label
				   (t	; :BELOW
				    (+ 5
				       (case (gv win :y-label-v-position)
					 ((:center-left :lower-left :center-center :lower-center :TWO-THIRDS-UP) 0)
					 (t (y-axis-label-height win)))))
				   )))
			  (if (and (gv win :label-traces) (non-null-plot-win-labels win))
			      (+ (gv (window-plot-axis-font win) :font-height)
				 (loop for curve-num from 1 to number-of-curves sum (key-and-label-height win curve-num)))
			      5)))))))

(defun y-axis-label-height (win)
  (* (gv (window-plot-axis-font win) :font-height)
     (+ 1 (NUMBER-OF-NEWLINES (get-plot-axis-label win :y)))))

(defun key-and-label-height (win curve-num)
  (round (+ *trace-key-gap*
	    (max (gv (window-plot-axis-font win) :font-height)
		 (scatter-symbol-height-width win (get-scatter-symbol win curve-num) curve-num)))))

(defun y-axis-labeled-ticks-below-0-width (win)
  (loop for y downfrom (- (gv win :y-origin) (gv win :y-inc))
	to (min (gv win :y-origin) (gv win :y-axis-min))
	by (gv win :y-inc)
	for count from 0
	maximize
	(+ 5 10 5			; for tick mark, gap,  and a little extra
	   (plot-window-string-width win (y-axis-number-string win y)))
	into max
	when (or (<= y (min (gv win :y-origin) (gv win :y-axis-min)))
		 (and (> count 0)
		      (= y (- (gv win :y-origin) (gv win :y-inc)))))
	do (return max)
	finally (return max)))

(defun y-axis-labeled-ticks-above-0-width (win)
  (loop for y upfrom (gv win :y-origin) ; Y axis labeled ticks, above 0.0
	to (max (gv win :y-origin) (gv win :y-axis-max))
	by (gv win :y-inc)
	for count from 0
	maximize
	(+ 5 10 5			; for tick mark, gap,  and a little extra
	   (plot-window-string-width win (y-axis-number-string win y)))
	into max
	when (or (>= y (max (gv win :y-origin) (gv win :y-axis-max)))
		 (and (> count 0) (= y (gv win :y-origin))))
	do (return max)
	finally (return max)))

(defun y-axis-label-width (win) (plot-window-string-width win (get-plot-axis-label win :y)))

(defun y-label-to-the-left-of-axis (win)
  (case (gv win :y-label-v-position)
    ((:upper-left :center-left :lower-left :two-thirds-up) 0)
    ((:center-center :upper-center :lower-center)
     (/ (y-axis-label-width win) 2))
    (t ;; :center-right :upper-right :lower-right
     (+ 10 (y-axis-label-width win)))))

(defun y-label-near-trace-labels (win)
  (case (gv win :y-label-v-position)
    ((:center-right  :lower-right :center-center :center-left  :lower-center :lower-left :two-thirds-up) 0)
    ((:upper-left :upper-center :upper-right)
     (round (/ (gv (window-plot-axis-font win) :font-height) 2)))
    (t 0)))

(defun set-plot-left-gap (win) ;; The distance in pixels of the scaled data rectangle from the left of the window.
  (let ((y-seqs (car (gv win :y-lists))))
    (s-value win :x-plot-left-gap
	     (round
	      (if (and (gv win :use-fixed-left-gap) (numberp (gv win :fixed-left-gap)))
		  (round (gv win :fixed-left-gap))
		  (+ (or (gv win :x-plot-left-gap-extra) 0)
		     (case (gv win :plot-type)
		       (:polar (gv win :label-height))
		       (:waterfall *x-plot-left-gap-waterfall*)
		       (t (+ (case (gv win :y-label-v-position)
			       (:two-thirds-up
				(+ 7 (y-axis-label-width win)))
			       (t ;; :upper-right :center-right :lower-right :upper-left :center-left :lower-left
				0))
			     (max (minimum-x-plot-right-gap win)
				  ;; Go through all the y axis tick mark numbers to find the widest.
				  (- (max (y-axis-labeled-ticks-below-0-width win)
					  (y-axis-labeled-ticks-above-0-width win)
					  (y-label-to-the-left-of-axis win))
				     (* (gv win :width) ; 0
					(/  (- (gv win :x-origin) (gv win :x-min))
					; (max (gv win :x-origin) (gv win :x-min))
					    (gv win :x-mag))))))))))))))

(defun minimum-x-plot-right-gap (win) (min *x-plot-right-gap* (round (* 0.05 (gv win :width)))))

(defun set-plot-right-gap (win)	;; The distance in pixels of the scaled data rectangle from the right side of the window.
  (s-value win :x-plot-right-gap
	   (if (and (gv win :use-fixed-right-gap) (numberp (gv win :fixed-right-gap)))
	       (round (gv win :fixed-right-gap))
	       (+ (or (gv win :x-plot-right-gap-extra) 0)
		  (case (gv win :plot-type)
		    (:polar (gv win :label-height))
		    (:waterfall
		     (+ (if (gv win :label-waterfall)
			    (loop for label in (reverse (gv win :label-list))
				  for curve-num from 0
				  when (= 0 (mod curve-num (1+ (gv win :waterfall-trace-label-skip))))
				  maximize (- (plot-window-string-width win label)
					; (x-plot-win-distance (* curve-num (gv win :x-trace-offset)) win)
					      0))
			    0)
			(minimum-x-plot-right-gap win)
			(if (and (gv win :label-waterfall) (gv win :gap-between-trace-and-waterfall-label))
			    (gv win :gap-between-trace-and-waterfall-label)
			    0)))
		    (t (+ (max (minimum-x-plot-right-gap win)
			       ;; Go through all the y axis tick mark numbers to find the widest.
			       (case (gv win :y-label-v-position)
				 ((:upper-left :upper-center :upper-right) 0)
				 ((:two-thirds-up :center-right :upper-right :lower-right) 0)
				 (t ;; :upper-left :center-left :lower-left
				  0	; (+ 10 (y-axis-label-width win))
				  ))))))))))

(defun set-plot-bottom-gap (win) ;; The distance in pixels of the scaled data rectangle off the bottom of the window.
  (let ((y-seqs (car (gv win :y-lists))))
    (s-value win :y-plot-bottom-gap
	     (if (and (gv win :use-fixed-bottom-gap) (numberp (gv win :fixed-bottom-gap)))
		 (round (gv win :fixed-bottom-gap))
		 (max
		  (+ (gv (window-comment-font win) :font-height) ; Leave room for at least title
		     (case (gv win :x-label-v-position)
		       (:above 0)
		       (t		; :below
			(+ (max 0 (gv win :x-axis-tick-mark-length))
			   (gv (window-plot-axis-font win) :font-height))))
		     10			; fudge
		     )
		  (case (gv win :plot-type)
		    (:polar (gv win :label-height))
		    (t (if (eq :auto (gv win :waterfall))
			   (+ *y-plot-bottom-gap* 10)
			   *y-plot-bottom-gap*))))))))

(defun set-plot-area-hw (win)
  (s-values win
	    (plot-area-width (- (gv win :width) (+ (gv win :x-plot-left-gap) (gv win :x-plot-right-gap))))
	    (plot-area-height (- (gv win :height) (+ (gv win :label-height) (gv win :y-plot-bottom-gap))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun adjust-axis-min-max-for-ints (win axis)
  (let* ((number-of-increments 5)
	 (axis-max (gv win (case axis (:x :x-axis-max) (:y :y-axis-max))))
	 (axis-min (gv win (case axis (:x :x-axis-min) (:y :y-axis-min))))
	 (axis-root (or (gv win (case axis (:x :x-axis-root) (:y :y-axis-root)))
			(if (and (> axis-max 0) (< axis-min 0)) 0
			    (if (> axis-max 0) axis-min axis-max))))
	 (adjusted-increment (ceiling (/ (- axis-max axis-min) number-of-increments)))
	 (distance-from-axis-max (mod (/ (- axis-max axis-root) adjusted-increment) 1))
	 (distance-from-axis-min (mod (/ (- axis-root axis-min) adjusted-increment) 1))
	 (m (* number-of-increments (/ (- axis-max axis-root)  (- axis-max axis-min)))))
;    (format t "m ~a~%" m)

    (s-value win (case axis (:x :x-axis-root) (:y :y-axis-root)) axis-root)
    (if (> distance-from-axis-max distance-from-axis-min)
	(case axis
	  (:x (s-value win :x-axis-max (+ axis-max (ceiling distance-from-axis-max))))
	  (:y (s-value win :y-axis-max (+ axis-max (ceiling distance-from-axis-max)))))
	(case axis
	  (:x (s-value win :x-axis-min (+ axis-min (ceiling distance-from-axis-min))))
	  (:y (s-value win :y-axis-min (+ axis-min (ceiling distance-from-axis-min))))))
    (ceiling (/
     (- (gv win (case axis (:x :x-axis-max) (:y :y-axis-max)))
	(gv win (case axis (:x :x-axis-min) (:y :y-axis-min))))
     number-of-increments))))


(defun set-plot-incs-origin-label-position (win x-origin y-origin x-inc y-inc x-label-v-position x-label-h-position)
  (cond-every ((and (not x-inc) (gv win :x-are-fns)) (setq x-inc (adjust-axis-min-max-for-ints win :x)))
	      ((and (not y-inc) (gv win :y-are-fns)) (setq y-inc (adjust-axis-min-max-for-ints win :y))))
  (s-value win :x-origin (s-flt
			  (or x-origin
			      (get-nice-mag (bound-val 0.0 (gv win :x-axis-max) (gv win :x-axis-min))
					    (- (gv win :x-axis-max) (gv win :x-axis-min))))))
  (s-value win :y-origin (s-flt (or y-origin
				    (if (gv win :TIMED-DATA)
					(progn
					  (when (and (not (gv win :y-axis-root))
						     (> (gv win :y-axis-max) 0) (< (gv win :y-axis-min) 0))
					    (s-value win :y-axis-root 0.0))
					  (if (> (abs (gv win :y-axis-max)) (abs (gv win :y-axis-min)))
					      (gv win :y-axis-min) (gv win :y-axis-max)))
					(get-nice-mag (bound-val 0.0 (gv win :y-axis-max) (gv win :y-axis-min))
						      (- (gv win :y-axis-max) (gv win :y-axis-min)))))))
  ;; Avoid small y-incs which do not advance axis tick positions in draw-full-cartesian-axes
  (s-value win :y-inc (let ((float-inc (max (case (gv win :mode)
					      (:histogram 1)
					      (t (abs (* .01 (gv win :y-mag)))))
					    (if (and y-inc (not (= 0 y-inc)))
						(float y-inc)
						(/ (get-nice-mag
						    (if (gv win :waterfall)
							(gv win :xfrmd-y-data-mag)
							(- (gv win :y-axis-max) (gv win :y-axis-min))))
						   (case (gv win :plot-type)
						     (polar 4.0)
						     (:waterfall 3.0)
						     (t (or (gv win :default-y-incs) 5.0))))))))
			(s-flt (if (gv win :y-are-fns) (max 1 (round float-inc)) float-inc))))
  (s-value win :x-inc (let ((float-inc (case (gv win :plot-type)
					 (:polar (gv win :y-inc))
					 (t (if (and x-inc (not (= 0 x-inc)))
						(float x-inc)
						(/ (get-nice-mag
						    (if (gv win :waterfall)
							(gv win :xfrmd-x-data-mag)
							(- (gv win :x-axis-max) (gv win :x-axis-min))))
						   (or (gv win :default-x-incs) 5.0)))))))
			(s-flt (if (gv win :x-are-fns) (max 1 (round float-inc)) float-inc))))
  (when x-label-h-position (s-value win :x-label-h-position x-label-h-position))
  (s-value win :x-label-v-position (or x-label-v-position
				       (if (> (- (gv win :y-axis-max) (gv win :y-origin))
					      (- (gv win :y-origin) (gv win :y-axis-min)))
					   :below :above))))




#|
;; (gv *twin* :xfrmd-x-data-mag)


;; (* 5 (ceiling (/ 43.0 5)))
|#

(defun UPDATE-FIXED-GAP (win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap use-fixed-right-gap
					fixed-right-gap use-fixed-left-gap fixed-left-gap)
  (s-values win
	    fixed-top-gap fixed-bottom-gap fixed-right-gap fixed-left-gap
	    (use-fixed-top-gap (and use-fixed-top-gap (numberp fixed-top-gap)))
	    (fixed-bottom-gap (and use-fixed-bottom-gap (numberp fixed-bottom-gap)))
	    (use-fixed-right-gap (and use-fixed-right-gap (numberp fixed-right-gap)))
	    (use-fixed-left-gap (and use-fixed-left-gap (numberp fixed-left-gap)))))

(defun set-plot-win-orig-parameters (win)
  ;; These are useful for restoring the plot to its original state.
  (s-values win
	    (orig-x-min (gv win :x-min))
	    (orig-x-max (gv win :x-max))
	    (orig-x-origin (gv win :x-origin))
	    (orig-x-axis-inc (gv win :x-inc))
	    (orig-y-min (gv win :y-min))
	    (orig-y-max (gv win :y-max))
	    (orig-y-origin (gv win :y-origin))
	    (orig-y-axis-inc (gv win :y-inc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plot Axes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-all-axes (win)
  (case (gv win :axes-type)
    (:none (get-plot-agg win 'data-axes t))
    (:simple (draw-simple-axes win :auto-setup (gv win :auto-wf-setup))
	     (when (gv win :draw-grid) (draw-full-cartesian-axes win :draw-axes nil)))
    (:standard (draw-full-cartesian-axes win)))
  (unless (gv win :draw-grid) (get-plot-agg win 'data-grid t))
  nil)

(defun refresh-axes (win)
  (if (gv win :draw-grid)
      (draw-full-cartesian-axes win :draw-axes nil)
      (get-plot-agg win 'data-grid t)))

(defun draw-simple-axes (window &key auto-setup)
  (when auto-setup
    (s-value window :x-scale-t% 90)
    (s-value window :y-scale-t% 90)
    (s-value window :x-scale-l% 85.0))
  (let* ((axes-agg (get-plot-agg window 'data-axes t))
	 (x-body-x1 (round (* (gv window :width) .01 (gv window :x-scale-l%))))
	 (x-body-x2 (+ (round (* (gv window :width) .01 (gv window :x-scale-l%)))
		       (-  (x-plot-win-float (+ (gv window :x-axis-min)
						(or *simple-axis-x* (gv window :x-inc))) window nil)
			   (x-plot-win-float (gv window :x-axis-min) window nil))))
	 (x-body-y1 (round (* (gv window :height) .01 (gv window :x-scale-t%))))
	 (x-body-y2 (round (* (gv window :height) .01 (gv window :x-scale-t%))))
	 (y-body-x1 (round (* (gv window :width) .01 (gv window :x-scale-l%))))
	 (y-body-x2 (round (* (gv window :width) .01 (gv window :x-scale-l%))))

	 (y-body-y1 (round (* (gv window :height) .01 (gv window :y-scale-t%))))
	 (y-body-y2 (+ (round (* (gv window :height) .01 (gv window :y-scale-t%)))
		       (- (y-plot-win-float (+ (gv window :y-axis-min) (or *simple-axis-y* (gv window :y-inc))) window nil)
			  (y-plot-win-float (gv window :y-axis-min) window nil))))

	 (x-label (create-instance nil axis-text
				   (:label-position (o-formula (or (gvl :window :x-label-v-position) :center)))
				   (:left (round (+ (the fn x-body-x1) (* 0.5 (- (the fn x-body-x2) (the fn x-body-x1))))))))

	 (y-label (create-instance nil axis-text
				   (:label-position (o-formula (or (gvl :window :y-label-h-position) :center)))
				   (:left (o-formula (case (gvl :window :y-label-h-position)
						       (:right (the fn (+ (the fn y-body-x1) 10)))
						       (:left (the fn (- (the fn y-body-x1) 5)))
						       (t y-body-x1))))))

	 (gadget (create-instance nil opal:aggregadget
				  (:parts
				   `((:x-body ,opal:line
				      (:line-style ,(o-formula (create-plot-line-style opal::line-style window)))
				      (:visible ,(o-formula (gvl :window :x-axis-p)))
				      (:x1 ,x-body-x1) (:x2 ,x-body-x2) (:y1 ,x-body-y1) (:y2 ,x-body-y2))
				     (:y-body ,opal:line
				      (:line-style ,(o-formula (create-plot-line-style opal::line-style window)))
				      (:visible ,(o-formula (gvl :window :y-axis-p)))
				      (:x1 ,y-body-x1) (:x2 ,y-body-x2) (:y1 ,y-body-y1) (:y2 ,y-body-y2)))))))

    (s-values x-label window)
    (s-values y-label window)

    (opal:add-components axes-agg gadget y-label x-label)
    (s-value y-label :visible (o-formula (gvl :parent :window :y-axis-p)))
    (s-value y-label :string (format nil "~a~A~a"
				     (if (gv window :simple-axis-y-value-p)
					 (y-axis-number-string window (s-flt (or *simple-axis-y* (gv window :y-inc))))
					 "")
				     (if (and (gv window :y-label) (gv window :simple-axis-y-value-p))
					 (format nil "~% ") "")
				     (gv window :y-label)))
    (s-value y-label :top (min (- (the fn y-body-y1) (+ -5 (fn-gv y-label :height))) (the fn y-body-y2)))
    (s-value x-label :string (format nil "~a~A~a"
				     (if (gv window :simple-axis-x-value-p)
					 (x-axis-number-string window (s-flt (or *simple-axis-x* (gv window :x-inc))))
					 "")
				     (if (and (gv window :x-label) (gv window :simple-axis-x-value-p))
					 " " "")
				     (gv window :x-label)))
    (s-value x-label :visible (o-formula (gvl :parent :window :x-axis-p)))
    (s-value x-label :top (+ 5 x-body-y1))

    nil))

(defun parse-plot-win-log-base (win)
  (if (and (numberp (gv win :log-base))
	   (= (round (gv win :log-base)) (gv win :log-base)))
      (round (gv win :log-base))
      (or (gv win :log-base) "E")))

(defun get-plot-axis-label (win axis)
  (let ((log (gv win (generic-to-axis axis 'log)))
	(label (gv win (generic-to-axis axis 'label))))
    (or (if log
	    (format nil "~A[log~A]" label (parse-plot-win-log-base win))
	    label)
	"")))

(defun get-axis-root (win axis &optional AXIS-MAX AXIS-MIN)
  (declare (ignore AXIS-MAX AXIS-MIN))
  (let* ((axis-root-value (gv win (generic-to-axis axis :axis-root)))
	 (axis-origin-value (gv win (generic-to-axis axis :origin)))
	 (derived-axis-root-value (if (numberp axis-root-value)
				    axis-root-value
				    (or (gv win (generic-to-axis axis axis-root-value))
					axis-origin-value))))
    (s-flt (if (gv win :reference-ticks-to-origin) axis-origin-value derived-axis-root-value))))

(defun draw-full-cartesian-axes (win &key (draw-axes t) draw-grid)
  ;; Draw X and Y axis onto plot window WIN so that data will be contained in scaled rectangle centered in window. Also adds labeled tick marks and grid.
  (let* ((draw-grid (or draw-grid (gv win :draw-grid)))
	 ;; (axis-line-style (or (gv win :axis-line-style) *default-axis-line-style*))
	 (grid-line-style (create-plot-line-style (or (gv win :grid-line-style) *default-grid-line-style*) win))
	 (axis-agg (when draw-axes (get-plot-agg win 'data-axes t))) ; Get a data-axis agg and clear it.
	 (grid-agg (when draw-grid (get-plot-agg win 'data-grid t))) ; Get a data-axis agg and clear it.

	 (y-origin (s-flt (gv win :y-origin)))
	 (y-axis-max (s-flt (max y-origin (gv win :y-axis-max))))
	 (y-axis-min (s-flt (min y-origin (gv win :y-axis-min))))
	 (y-axis-root (get-axis-root win :y y-AXIS-MAX Y-AXIS-MIN))
	 (y-axis-origin y-origin)
 	 (y-origin-w (y-plot-win-float-unbounded y-axis-origin win))

	 (x-origin (s-flt (gv win :x-origin)))
	 (x-axis-max (s-flt (max x-origin (gv win :x-axis-max))))
	 (x-axis-min (s-flt (min x-origin (gv win :x-axis-min))))
	 (x-axis-root (get-axis-root win :x x-AXIS-MAX x-AXIS-MIN))
	 (x-axis-origin x-origin)
	 (x-origin-w (x-plot-win-float-unbounded x-axis-origin win))

	 (number-x-tics-below-zero (truncate (/ (- x-axis-root x-axis-min) (gv win :x-inc))))
	 (number-x-tics-above-zero (truncate (/ (- x-axis-max x-axis-root) (gv win :x-inc))))
	 (number-y-tics-below-zero (truncate (/ (- y-axis-root y-axis-min) (gv win :y-inc))))
	 (number-y-tics-above-zero (truncate (/ (- y-axis-max y-axis-root) (gv win :y-inc))))

	 (y-axis-visible-max (when (gv win :consider-y-axis-visible-limit) (gv win :y-axis-visible-max)))
	 (y-axis-visible-min (when (gv win :consider-y-axis-visible-limit) (gv win :y-axis-visible-min)))
	 (x-axis-visible-max (when (gv win :consider-x-axis-visible-limit) (gv win :x-axis-visible-max)))
	 (x-axis-visible-min (when (gv win :consider-x-axis-visible-limit) (gv win :x-axis-visible-min))))
    (declare (single-float y-axis-max y-axis-min y-axis-root x-axis-max x-axis-min x-axis-root))
    (when (or (and draw-axes (gv win :y-axis-p)) draw-grid)
      (when (and draw-axes (gv win :y-axis-p)) ;Y AXIS
	(opal:add-component axis-agg (create-instance nil opal:line
						      (:line-style (o-formula (create-plot-line-style plot-line-style win)))
						      (:x1 x-origin-w) (:x2 x-origin-w)
						      (:y1 (y-plot-win-float-unbounded (or y-axis-visible-min y-axis-min) win))
						      (:y2 (y-plot-win-float-unbounded (or y-axis-visible-max y-axis-max) win))))
	(add-y-axis-label axis-agg win y-axis-max y-axis-min x-origin-w))
      (add-y-ticks win draw-axes axis-agg y-axis-root ; y-axis-origin
		   number-y-tics-above-zero
		   y-axis-visible-min y-axis-visible-max x-axis-visible-min x-axis-visible-max
		   x-origin-w draw-grid grid-agg grid-line-style NUMBER-y-TICS-BELOW-ZERO x-AXIS-MAX x-AXIS-MIN))
    (when (or (and draw-axes (gv win :x-axis-p)) draw-grid)
      (when (and draw-axes (gv win :x-axis-p)) ;X AXIS
	(opal:add-component axis-agg (create-instance nil opal:line
						      (:line-style (o-formula (create-plot-line-style plot-line-style win)))
						      (:x1 (x-plot-win-float-unbounded (or x-axis-visible-min x-axis-min) win))
						      (:x2 (x-plot-win-float-unbounded (or x-axis-visible-max x-axis-max) win))
						      (:y1 y-origin-w) (:y2 y-origin-w)))
	(add-x-axis-label axis-agg win x-axis-max x-axis-min y-origin-w))
      (add-x-ticks win draw-axes axis-agg x-axis-root ; x-axis-origin
		   number-x-tics-above-zero
		   x-axis-visible-min x-axis-visible-max y-axis-visible-min y-axis-visible-max
		   y-origin-w draw-grid grid-agg grid-line-style NUMBER-X-TICS-BELOW-ZERO Y-AXIS-MAX Y-AXIS-MIN))
    (when (and draw-axes (eq (gv win :plot-type) :polar) (gv win :polar-circles-p)) ; Polar circles
      (add-polar-circles win axis-agg y-axis-root y-axis-max)))
  (when (gv win :bitmap) (opal:move-component (get-agg win) (gv win :bitmap) :where :back)))

(defun add-y-axis-label (axis-agg win y-axis-max y-axis-min x-origin-w)
  (let* ((font-height (the fn (Y-AXIS-LABEL-HEIGHT win)))
	 (y-axis-label (get-plot-axis-label win :y))
	 (y-axis-max-pixels (y-plot-win-float-unbounded y-axis-max win))
	 (y-axis-min-pixels (y-plot-win-float-unbounded y-axis-min win))
	 (label (create-instance nil axis-text
				 (:label-position (o-formula (case (gvl :window :y-label-v-position)
							       ((:upper-left :center-left :lower-left :two-thirds-up) :right)
							       ((:center-center :upper-center :lower-center) :center)
							       (t ; :center-right :upper-right :lower-right
								:left)))))))
    (s-value label :window win)
    (opal:add-component axis-agg label)
    (s-value label :string y-axis-label)
    (s-value label :orientation (o-formula (gvl :parent :window :y-label-orientation)))
    (s-value label :left (case (gv win :y-label-v-position)
			   (:two-thirds-up 5)
			   (:center-right (+ x-origin-w 5))
			   ((:upper-center :lower-center :center-center) x-origin-w)
			   ((:upper-right :lower-right) x-origin-w)
			   (t		; :upper-left :lower-left :center-left
			    (+ x-origin-w  (* -1 (fn-gv label :width)) -5))))
    (s-value label :top (case (gv win :y-label-v-position)
			  ((:center-left :center-right)
			   (round (the sf (+ y-axis-min-pixels
					     (* 0.5 (- y-axis-max-pixels y-axis-min-pixels))
					     (* -0.5 font-height)))))
			  (:two-thirds-up
			   (round (the sf (+ (y-plot-win-float-unbounded y-axis-min win)
					     (* 0.67 (- y-axis-max-pixels y-axis-min-pixels))
					     (* -0.5 font-height)))))
			  ((:lower-left :lower-right)
			   (round (- y-axis-min-pixels (* -1.5 font-height))))
			  (t		; :upper-right :upper-left
			   (round (+ y-axis-max-pixels (* -1.5 font-height))))))
    label))

(defun add-x-axis-label (axis-agg win x-axis-max x-axis-min y-origin-w)
  (let* ((font-height (fn-gv (window-plot-axis-font win) :font-height))
	 (x-axis-label (get-plot-axis-label win :x))
	 (x-axis-max-pixels (x-plot-win-float-unbounded x-axis-max win))
	 (x-axis-min-pixels (x-plot-win-float-unbounded x-axis-min win))
	 (x-axis-label-width (the fn (plot-window-string-width win x-axis-label)))
	 (label (create-instance nil axis-text (:label-position :right ; :center
								))))
    (s-value label :window win)		;    (s-value label :string (gv win :x-label))
    (s-value label :string x-axis-label)
    (opal:add-component axis-agg label)
    (s-value label :orientation (o-formula (gvl :parent :window :x-label-orientation)))
    (s-value label :left (case (gv axis-agg :window :x-label-h-position)
			   (:left x-axis-min-pixels)
			   (:center (round (+ x-axis-min-pixels (* 0.5 (- (- x-axis-max-pixels x-axis-min-pixels) x-axis-label-width)))))
			   (t (- x-axis-max-pixels x-axis-label-width)))) ; :right
    (s-value label :top (+ y-origin-w
			   (case (gv axis-agg :window :x-label-v-position)
			     (:above (- (+ *GAP-BTWN-X-LABEL-AND-TICK-MARKS* (+ font-height font-height))))
			     (t		; :below
			      (+ *GAP-BTWN-X-LABEL-AND-TICK-MARKS* font-height)))))
    label))

(defun add-x-ticks (win draw-axes axis-agg x-axis-origin number-x-tics-above-zero
		    x-axis-visible-min x-axis-visible-max y-axis-visible-min y-axis-visible-max
		    y-origin-w draw-grid grid-agg grid-line-style NUMBER-X-TICS-BELOW-ZERO Y-AXIS-MAX Y-AXIS-MIN)
  (declare (optimize (safety 1) (speed 3) (space 0))
  	   (single-float x-axis-origin y-axis-max y-axis-min)
	   (fixnum number-x-tics-above-zero NUMBER-x-TICS-BELOW-ZERO))
  (let ((y-min (or y-axis-visible-min y-axis-min))
	(y-max (or y-axis-visible-max y-axis-max)))
    (labels ((include-label (win tick-count x) (and (= 0 (mod tick-count (1+ (fn-gv win :x-axis-tick-skip)))) (or (not (= x 0)) (gv win :include-x-tick-at-0))))
	     (include-mark (win tick-count x) (and (= 0 (mod tick-count (1+ (fn-gv win :x-axis-tick-mark-skip)))) (or (not (= x 0)) (gv win :include-x-tick-at-0))))
	     (draw-grid-line (grid-agg grid-line-style win x y-min y-max)
	       (opal:add-component grid-agg (create-instance nil opal:line (:line-style grid-line-style)
							     (:x1 (x-plot-win-float-unbounded x win)) (:x2 (x-plot-win-float-unbounded x win))
							     (:y1 (y-plot-win-float-unbounded y-min win)) (:y2 (y-plot-win-float-unbounded y-max win)))))
	     (add-x-tick (win x draw-axes axis-agg draw-grid grid-agg y-origin-w tick-count x-axis-visible-min x-axis-visible-max)
	       (when (and (or (not x-axis-visible-min) (<= (the sf x-axis-visible-min) x))
			  (or (not x-axis-visible-max) (<= x (the sf x-axis-visible-max))))
		 (let ((include-label (include-label win tick-count x))
		       (include-mark (include-mark win tick-count x)))
		   (when (and draw-axes (gv win :x-axis-p))
		     (when include-label (x-axis-tick-label axis-agg y-origin-w x))
		     (unless (or (gv win :polar-circles-p) (not include-mark)) (x-axis-tick-mark axis-agg y-origin-w x include-label)))
		   (when (and include-mark draw-grid) (draw-grid-line grid-agg grid-line-style win x y-min y-max))))))
      (loop for x single-float downfrom (- x-axis-origin (the sf (gv win :x-inc))) by (the sf (gv win :x-inc)) ;; X TICKS, BELOW 0.0
	    for tick-count fixnum from 1 to (max 0 number-x-tics-below-zero)
	    do (add-x-tick win x draw-axes axis-agg draw-grid grid-agg y-origin-w tick-count x-axis-visible-min x-axis-visible-max))
      (loop for x single-float upfrom x-axis-origin by (the sf (gv win :x-inc))	;; X AXIS LABELED TICKS, ABOVE 0.0
	    for tick-count fixnum from 0 to (max 0 number-x-tics-above-zero)
	    do (add-x-tick win x draw-axes axis-agg draw-grid grid-agg y-origin-w tick-count x-axis-visible-min x-axis-visible-max)))))

(defun add-y-ticks (win draw-axes axis-agg y-axis-origin number-y-tics-above-zero
		    y-axis-visible-min y-axis-visible-max x-axis-visible-min x-axis-visible-max
		    x-origin-w draw-grid grid-agg grid-line-style NUMBER-y-TICS-BELOW-ZERO X-AXIS-MAX X-AXIS-MIN)
  (declare (optimize (safety 1) (speed 3) (space 0))
  	   (single-float y-axis-origin x-axis-max x-axis-min)
	   (fixnum number-y-tics-above-zero NUMBER-y-TICS-BELOW-ZERO))
  (let ((x-min (or x-axis-visible-min x-axis-min))
	(x-max (or x-axis-visible-max x-axis-max)))
    (labels ((include-label (win tick-count x) (and (= 0 (mod tick-count (1+ (fn-gv win :y-axis-tick-skip)))) (or (not (= x 0)) (gv win :include-y-tick-at-0))))
	     (include-mark (win tick-count x) (and (= 0 (mod tick-count (1+ (fn-gv win :y-axis-tick-mark-skip)))) (or (not (= x 0)) (gv win :include-y-tick-at-0))))
	     (draw-grid-line (grid-agg grid-line-style win x x-min x-max)
	       (opal:add-component grid-agg (create-instance nil opal:line (:line-style grid-line-style)
							     (:y1 (y-plot-win-float-unbounded x win)) (:y2 (y-plot-win-float-unbounded x win))
							     (:x1 (x-plot-win-float-unbounded x-min win)) (:x2 (x-plot-win-float-unbounded x-max win)))))
	     (add-y-tick (win x draw-axes axis-agg draw-grid grid-agg x-origin-w tick-count y-axis-visible-min y-axis-visible-max)
	       (when (and (or (not y-axis-visible-min) (<= (the sf y-axis-visible-min) x))
			  (or (not y-axis-visible-max) (<= x (the sf y-axis-visible-max))))
		 (let ((include-label (include-label win tick-count x))
		       (include-mark (include-mark win tick-count x)))
		   (when (and draw-axes (gv win :y-axis-p))
		     (when include-label (y-axis-tick-label axis-agg x-origin-w x))
		     (unless (or (gv win :polar-circles-p) (not include-mark)) (y-axis-tick-mark axis-agg x-origin-w x include-label)))
		   (when (and include-mark draw-grid) (draw-grid-line grid-agg grid-line-style win x x-min x-max))))))
      (loop for x single-float downfrom (- y-axis-origin (the sf (gv win :y-inc))) by (the sf (gv win :y-inc)) ;; Y TICKS, BELOW 0.0
	    for tick-count fixnum from 1 to (max 0 number-y-tics-below-zero)
	    do (add-y-tick win x draw-axes axis-agg draw-grid grid-agg x-origin-w tick-count y-axis-visible-min y-axis-visible-max))
      (loop for x single-float upfrom y-axis-origin by (the sf (gv win :y-inc))	;; Y AXIS LABELED TICKS, ABOVE 0.0
	    for tick-count fixnum from 0 to (max 0 number-y-tics-above-zero)
	    do (add-y-tick win x draw-axes axis-agg draw-grid grid-agg x-origin-w tick-count y-axis-visible-min y-axis-visible-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-polar-circles (win axis-agg y-axis-root y-axis-max)
  (loop for y from (+ y-axis-root (gv win :y-inc)) by (gv win :y-inc) to y-axis-max do
	(opal:add-component
	 axis-agg
	 (create-instance nil opal:circle (:visible t)
			  (:line-style (o-formula (create-plot-line-style plot-line-style win))
			  ;; (o-formula (or (gvl :parent :window :axis-line-style) *default-axis-line-style* thick-black-line))
			  )
			  ;; These 1- and +3 are fudges to get better polar circles.
			  (:left (1- (x-plot-win-float-unbounded (- y) win)))
			  (:top (1- (y-plot-win-float-unbounded y win)))
			  (:height (+ 3 (y-plot-win-distance (+ y y) win)))
			  (:width (+ 3 (x-plot-win-distance (+ y y) win)))))))

(defun add-axis-text (string top left window agg &optional (text-position :right))
  (let ((label (create-instance nil axis-text (:label-position text-position))))
    (opal:add-component agg label)
    (s-values label window top string left)
    label))

(defun x-axis-number-string (win x)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   ; (single-float x)
	   )
  (let* ((adjusted-x (s-flt (if (gv win :x-axis-coeff) (* (gv win :x-axis-coeff) x) x)))
	 (x (cond ((gv win :absolute-value-ticks) (abs adjusted-x))
		  (t adjusted-x))))
    (declare (single-float adjusted-x x))
    (if (and (= x (the sf (gv win :x-origin)))
	     (not (and (or (gv win :x-axis-p) (gv win :x-origin-tick))
		       (or (not (and (gv win :y-axis-min) (gv win :y-origin) (gv win :y-axis-max)))
			   (not (< (the sf (gv win :y-axis-min))
				   (the sf (gv win :y-origin))
				   (the sf (gv win :y-axis-max))))))))
	""
	(let ((naked-number-string
	       (cond ((gv win :x-are-fns) (format nil "~D" (round x)))
		     (*use-tidy-number-format-for-plots* (plot-window-x-number-format x win :decimals (gv win :x-tick-decimal) :range (gv win :x-inc)))
		     (t (format nil "~E" x)))))
	  (if (or (gv win :x-axis-prefix) (gv win :x-axis-suffix))
	      (format nil "~a~A~a" (or (gv win :x-axis-prefix) "") naked-number-string (or (gv win :x-axis-suffix) ""))
	      naked-number-string)))))

(defun y-axis-number-string (win y)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   ; (single-float y)
	   )
  (let* ((adjusted-y (s-flt (if (gv win :y-axis-coeff) (* (gv win :y-axis-coeff) y) y)))
	 (y (cond ((gv win :absolute-value-ticks) (abs adjusted-y))
		  ((gv win :invert-y-axis-label) (- adjusted-y))
		  (t adjusted-y))))
    (declare (single-float adjusted-y y))
    (if (and (= y (the sf (gv win :y-origin)))
	     (not (and (gv win :y-origin-tick)
		       (or (not (and (gv win :x-axis-min) (gv win :x-origin) (gv win :x-axis-max)))
			   (not (< (the sf (gv win :x-axis-min))
				   (the sf (gv win :x-origin))
				   (the sf (gv win :x-axis-max))))))))
	""
	(let ((naked-number-string
	       (cond ((gv win :y-are-fns) (format nil "~D" (round y)))
		     (*use-tidy-number-format-for-plots* (plot-window-y-number-format y win :decimals (gv win :y-tick-decimal) :range (gv win :y-inc)))
		     (t (format nil "~E" y)))))
	  (if (or (gv win :y-axis-prefix) (gv win :y-axis-suffix))
	      (format nil "~a~A~a" (or (gv win :y-axis-prefix) "") naked-number-string (or (gv win :y-axis-suffix) ""))
	      naked-number-string)))))

(defun x-axis-tick-label (axis-agg y-origin-w x &optional actual-x-position-w)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum y-origin-w)
	   ; (single-float x)
	   )
  (let* ((text (opal:add-component axis-agg (create-instance nil axis-text (:label-position :center))))
	 (window (gv axis-agg :window))
	 (left (+ (or actual-x-position-w (x-plot-win-float-unbounded x window))
					; (* 0.5 (fn-gv text :width))
		  )))

    (s-value text :top (+ y-origin-w
			  (case (gv axis-agg :window :x-label-v-position)
			    (:below *plot-axis-x-axis-tick-label-gap*)
			    (:above (- (+ *plot-axis-x-axis-tick-label-gap* (fn-gv (window-plot-axis-font window) :font-height))))
			    (t 0))))
    (s-value text :string (x-axis-number-string window x))
    (s-values text window left)
    text))

(defun x-axis-tick-mark (axis-agg y-origin-w x include-label)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum y-origin-w)
	   ; (single-float x)
	   )
  (let ((window (gv axis-agg :window)))
    (opal:add-component
     axis-agg
     (create-instance nil opal:line
		      (:line-style (o-formula (create-plot-line-style plot-line-style window)))
		      (:y1 (+ y-origin-w
			      (round (the sf (* (if include-label 1.0 0.6)
						(case (gv axis-agg :window :x-label-v-position)
						  (:below (gv window :x-axis-tick-mark-length))
						  (:above (- (gv window :x-axis-tick-mark-length)))
						  (t 0)))))))
		      (:y2 (+ y-origin-w
			      (case (gv axis-agg :window :plot-type)
				(:polar (case (gv axis-agg :window :x-label-v-position)
					  (:below (- (gv window :x-axis-tick-mark-length)))
					  (:above (gv window :x-axis-tick-mark-length))
					  (t 0)))
				(t 0))))
		      (:x1 (x-plot-win-float-unbounded x window))
		      (:x2 (x-plot-win-float-unbounded x window))))))

(defun y-axis-tick-mark (axis-agg x-origin-w y include-label)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum x-origin-w)
	   ; (single-float y)
	   )
  (let ((window (gv axis-agg :window)))
    (opal:add-component
     axis-agg
     (create-instance nil opal:line
		      (:line-style (o-formula (create-plot-line-style plot-line-style window)))
		      (:x1 (+ x-origin-w
			      (* (case (gv axis-agg :window :y-label-h-position)
				   (:left -1)
				   (:right 1)
				   (t -1))
				 (the fn (round (the sf (* (if include-label 1.0 0.65)
							   (gv window :y-axis-tick-mark-length))))))))
		      (:x2 (+ x-origin-w
			      (case (gv window :plot-type)
				(:polar (gv window :y-axis-tick-mark-length))
				(t 0))))
		      (:y1 (y-plot-win-float-unbounded y window))
		      (:y2 (y-plot-win-float-unbounded y window))))))

(defun y-axis-tick-label (axis-agg x-origin-w y &optional actual-y-position-w)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum x-origin-w)
	   ; (single-float y)
	   )
  (let ((text (opal:add-component axis-agg (create-instance nil axis-text (:label-position (gv axis-agg :window :y-label-h-position)))))
	(window (gv axis-agg :window)))
    (s-values text window)
    (s-value text :top (round (- (or actual-y-position-w (y-plot-win-float-unbounded y window))
				 (* 0.5 (fn-gv (window-plot-axis-font window) :font-height)))))
    (s-value text :string (y-axis-number-string window y))
    (s-value text :left (+ x-origin-w
			   (* (case (gv axis-agg :window :y-label-h-position)
				(:left -1)
				(:right 1)
				(t -1))
			      (+ 5	; for tick mark
				 (case (gv axis-agg :window :y-label-h-position) ; a little gap
				   (:left *plot-axis-y-axis-tick-label-gap*)
				   (:right *plot-axis-y-axis-tick-label-gap*)
				   (t *plot-axis-y-axis-tick-label-gap*))))))
    text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Point list functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline log-or-not))
(defun log-or-not (data-value enable-log base-p base)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float base data-value))
  (the sf (if enable-log
	      (if base-p
		  (/ (the sf (log data-value)) (the sf (log base)))
		  (the sf (log data-value)))
	      data-value)))

(defun get-plot-point-list (x-base y-seq win &key (x-trace-offset 0.0) (y-trace-offset 0.0) (use-timed-data-x-constraints t) only-visible)
  ;; Note that X-BASE can be a number or a list of single-floats, and Y-SEQ is a list of single-floats. :X-TRACE-OFFSET and
  ;; :Y-TRACE-OFFSET are applied to the data *after*, and :X-DATA-OFFSET and :Y-DATA-OFFSET are applied to the data *before* log, if
  ;; enabled.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((max-min-fudge-factor 1e-5)
	 (x-trace-offset (s-flt (or x-trace-offset 0.0)))
	 (y-trace-offset (s-flt (or y-trace-offset 0.0)))
	 (margin (the fn (round *GET-PLOT-POINT-LIST-margin*)))
	 (base (the sf (or (gv win :log-base) e-single)))
	 (base-p (gv win :log-base))

	 ;; These are in pixels
	 (y-max-pixels (min (1- (fn-gv win :y-bound-max)) (the fn (+ margin (fn-gv win :height)))))
	 (x-max-pixels (min (1- (fn-gv win :x-bound-max)) (the fn (+ margin (fn-gv win :width)))))
	 (y-min-pixels (max (1+ (fn-gv win :y-bound-min)) (- margin)))
	 (x-min-pixels (max (1+ (fn-gv win :x-bound-min)) (- margin)))

	 ;; These are in data units
	 (x-min (* (- 1.0 max-min-fudge-factor) (gv win :x-min))) (x-max (* (+ 1.0 max-min-fudge-factor) (gv win :x-max)))
	 (y-min (* (- 1.0 max-min-fudge-factor) (gv win :y-min))) (y-max (* (+ 1.0 max-min-fudge-factor) (gv win :y-max)))
	 (x-log (gv win :x-log)) (y-log (gv win :y-log))
	 (x-data-scale (s-flt (or (gv win :x-data-scale) 1.0)))
	 (y-data-scale (s-flt (or (gv win :y-data-scale) 1.0)))
	 (x-data-offset (s-flt (or (gv win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (gv win :y-data-offset) 0)))
	 (last-x 0) (last-y 0) (this-x 0) (this-y 0))
    (declare (single-float x-data-offset y-data-offset x-min x-max y-trace-offset x-trace-offset x-data-scale y-data-scale)
	     (fixnum last-x last-y this-x this-y y-max-pixels x-max-pixels y-min-pixels x-min-pixels))
    (cond ((and (listp x-base) (listp y-seq)) ; For now almost all cases are with both x and y seq as lists
	   (get-plot-point-list-from-lists
	    x-base x-log x-min x-max x-min-pixels x-max-pixels x-data-offset x-trace-offset x-data-scale
	    use-timed-data-x-constraints
	    y-seq y-log y-min y-max y-min-pixels y-max-pixels y-data-offset y-trace-offset y-data-scale
	    base base-p win only-visible))
	  ((and (numberp x-base) (listp y-seq))
	   (let ((delta-x-float (float x-base))
		 (x-start (s-flt (or (gv win :delta-t-start) 0)))
		 point-list)
	     (do ((x x-start (the sf (+ x delta-x-float))) ; LAST-DELTA-T copies this algorithm.
		  (y-seq y-seq (cdr y-seq)))
		 ((null y-seq) point-list)
	       (let ((offsetted-x (* x-data-scale (+ x-data-offset (s-flt x))))
		     (offsetted-y (* y-data-scale (+ y-data-offset (s-flt (car y-seq))))))
		 (when (or x-log
			   (not use-timed-data-x-constraints)
			   (<= x-min offsetted-x x-max))
		   (setq this-x (x-plot-win-float (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base))) win t)
			 this-y (y-plot-win-float (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base))) win t))))
	       (when (and (not (and (= this-x last-x) (= this-y last-y)))
			  (<= x-min-pixels this-x x-max-pixels)
			  (<= y-min-pixels this-y y-max-pixels))
		 (push this-y point-list)
		 (push this-x point-list)
		 (setq last-x this-x last-y this-y)))
	     point-list)))))

(defun get-xfrmed-point-list (x-base y-seq win)
  (let* (point-list
	 (x-start (or (gv win :delta-t-start) 0.0))
	 (x-trace-offset (s-flt (or (gv win :x-trace-offset) 0.0)))
	 (y-trace-offset (s-flt (or (gv win :y-trace-offset) 0.0)))
	 (base (the sf (or (gv win :log-base) e-single)))
	 (base-p (gv win :log-base))
	 (x-seq (typecase x-base
		  (cons x-base)
		  (number (let ((delta-x-float (s-flt x-base))
				out)
			    (do ((x x-start (the sf (+ x delta-x-float)))
				 (y-seq y-seq (cdr y-seq)))
				((null y-seq) (reverse out))
			      (push x out))))))
	 (x-min (gv win :x-min))	; In data units
	 (x-max (gv win :x-max))	; In data units
	 (x-log (gv win :x-log)) (y-log (gv win :y-log))
	 (x-data-scale (s-flt (or (gv win :x-data-scale) 1)))
	 (y-data-scale (s-flt (or (gv win :y-data-scale) 1)))
	 (x-data-offset (s-flt (or (gv win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (gv win :y-data-offset) 0)))
	 x-out y-out)
    (do ((x-seq x-seq (cdr x-seq))
	 (y-seq y-seq (cdr y-seq)))
	((null y-seq) point-list)
      (let ((offsetted-x (* x-data-scale (+ x-data-offset (s-flt (car x-seq)))))
	    (offsetted-y (* y-data-scale (+ y-data-offset (s-flt (car y-seq))))))
	(when  (<= x-min offsetted-x x-max)
	  (push (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base))) x-out)
	  (push (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base))) y-out))))
    (values (reverse x-out) (reverse y-out))))


(defun get-plot-point-list-from-lists (x-seq x-log x-min x-max x-off-win-min-pixels x-off-win-max-pixels
				       x-data-offset x-trace-offset x-data-scale
				       use-timed-data-x-constraints
				       y-seq y-log y-min y-max y-off-win-min-pixels y-off-win-max-pixels
				       y-data-offset y-trace-offset y-data-scale
				       base base-p win only-visible)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (ignore use-timed-data-x-constraints)
	   (fixnum x-off-win-min-pixels x-off-win-max-pixels y-off-win-min-pixels y-off-win-max-pixels)
	   (single-float x-trace-offset x-data-offset x-data-scale
			 y-trace-offset y-data-offset y-data-scale
			 base x-min x-max y-min y-max))
  (let* ((x-min-limit (GV WIN :x-min-limit))
	 (x-max-limit (GV WIN :x-max-limit))
	 (x-bound-min (GV WIN :x-bound-min))
	 (x-bound-max (GV WIN :x-bound-max))
	 (width (GV WIN :width))
	 (x-plot-left-gap (GV WIN :x-plot-left-gap))
	 (x-min (GV WIN :x-min))
	 (x-mag (GV WIN :x-mag))
	 (plot-area-width (GV WIN :plot-area-width))
	 (y-min-limit (GV WIN :y-min-limit))
	 (y-max-limit (GV WIN :y-max-limit))
	 (y-bound-min (GV WIN :y-bound-min))
	 (y-bound-max (GV WIN :y-bound-max))
	 (height (GV WIN :height))
	 (y-plot-bottom-gap (GV WIN :y-plot-bottom-gap))
	 (y-min (GV WIN :y-min))
	 (y-mag (GV WIN :y-mag))
	 (plot-area-height (GV WIN :plot-area-height))
	 (last-x-pixels 0) (this-x-pixels 0) (last-y-pixels 0) (this-y-pixels 0)
	 (x-off-plot-min-pixels (x-plot-win-float x-min win))
	 (x-off-plot-max-pixels (x-plot-win-float x-max win))
	 (y-off-plot-min-pixels (y-plot-win-float y-max win))
	 (y-off-plot-max-pixels (y-plot-win-float y-min win))
	 (x-left-border-pixels (if (gv win :apply-horizontal-borders) x-off-plot-min-pixels x-off-win-min-pixels))
	 (x-right-border-pixels (if (gv win :apply-horizontal-borders) x-off-plot-max-pixels x-off-win-max-pixels))
	 (y-left-border-pixels (if (gv win :apply-vertical-borders) y-off-plot-min-pixels y-off-win-min-pixels))
	 (y-right-border-pixels (if (gv win :apply-vertical-borders) y-off-plot-max-pixels y-off-win-max-pixels))
	 (plot-point-skip-mod (the fn (or (when (numberp (gv win :plot-point-skip)) (1+ (fn-gv win :plot-point-skip)))
					  1)))
	 (plot-point-skip-mod-one-p (= plot-point-skip-mod 1))
	 point-list this-point-visible last-point-valid)
    (declare (fixnum last-x-pixels last-y-pixels x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels))
    (do ((x-seq x-seq (cdr x-seq))
	 (y-seq y-seq (cdr y-seq))
	 (count 0 (1+ count)))
	((or (null x-seq) (null y-seq)) point-list)
      (declare (fixnum count))
      (when (or plot-point-skip-mod-one-p (zerop (mod (the (unsigned-byte 29) count) (the (unsigned-byte 29) plot-point-skip-mod))))
	(let* ((offst-x-data-sf (* x-data-scale (+ x-data-offset (the sf (car x-seq)))))
	       (offset-log-x (+ x-trace-offset (log-or-not offst-x-data-sf x-log base-p base)))
	       (offst-y-data-sf (* y-data-scale (+ y-data-offset (the sf (car y-seq)))))
	       (offset-log-y (+ y-trace-offset (log-or-not offst-y-data-sf y-log base-p base)))
	       (last-point-visible this-point-visible)
	       this-x-pixels
	       this-y-pixels)
	  (setq this-x-pixels (x-plot-win-float-bounded-w-win-args
			       offset-log-x x-min-limit x-max-limit x-bound-min x-bound-max width x-plot-left-gap x-min x-mag plot-area-width))
	  (setq this-y-pixels (y-plot-win-float-bounded-w-win-args
			       offset-log-y y-min-limit y-max-limit y-bound-min y-bound-max height y-plot-bottom-gap y-min y-mag plot-area-height))
	  (unless (and (= this-x-pixels last-x-pixels) (= this-y-pixels last-y-pixels))
	    (if  (and (<= x-left-border-pixels this-x-pixels x-right-border-pixels)
		      (<= y-left-border-pixels this-y-pixels y-right-border-pixels))
		 (progn
		   (setq this-point-visible t)
		   (unless last-point-visible
		     (when (and (not only-visible) (gv win :include-border-points) last-point-valid)
		       (multiple-value-bind (border-x-pixels border-y-pixels)
			   (border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
					 x-left-border-pixels x-right-border-pixels y-left-border-pixels y-right-border-pixels
					 t)
			 (push nil point-list)
			 (push border-y-pixels point-list)
			 (push border-x-pixels point-list))))
		   (push this-y-pixels point-list)
		   (push this-x-pixels point-list))
		 (progn
		   (setq this-point-visible nil)
		   (when (and last-point-visible (not only-visible) (gv win :include-border-points))
		     (multiple-value-bind (border-x-pixels border-y-pixels)
			 (border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
				       x-left-border-pixels x-right-border-pixels y-left-border-pixels y-right-border-pixels
				       nil)
		       (push border-y-pixels point-list)
		       (push border-x-pixels point-list)
		       (push nil point-list)))))
	    (setq last-x-pixels this-x-pixels last-y-pixels this-y-pixels last-point-valid t)))))
    point-list))

(defun border-point (last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
		     x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels
		     previous-point-hidden)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
		   x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels))
  (let* ((hidden-x-pixels (if previous-point-hidden last-x-pixels this-x-pixels))
	 (shown-x-pixels (if previous-point-hidden this-x-pixels last-x-pixels))
	 (hidden-y-pixels (if previous-point-hidden last-y-pixels this-y-pixels))
	 (shown-y-pixels (if previous-point-hidden this-y-pixels last-y-pixels))
   	 (diff-vector-x (s-flt (- hidden-x-pixels shown-x-pixels)))
	 (diff-vector-y (s-flt (- hidden-y-pixels shown-y-pixels)))
	 (diff-vector-angle (if (= diff-vector-x 0)
				(* (if (> diff-vector-y 0) 1 -1) (/ pi 2))
				(atan diff-vector-y diff-vector-x)))
	 final-diff-vector-x final-diff-vector-y)
    (when nil				; *debug-plot-border-point*
      (format t "hidden-x-pixels ~A, hidden-y-pixels ~A, shown-x-pixels ~A, shown-y-pixels ~A~%" hidden-x-pixels hidden-y-pixels shown-x-pixels shown-y-pixels)
      (format t "x-off-plot-min-pixels ~A, x-off-plot-max-pixels ~A ~%" x-off-plot-min-pixels x-off-plot-max-pixels)
      (format t "y-off-plot-min-pixels ~A, y-off-plot-max-pixels ~A ~%" y-off-plot-min-pixels y-off-plot-max-pixels)
      (format t "diff-vector-angle ~A, diff-vector-x ~A, diff-vector-y ~A~%" diff-vector-angle diff-vector-x diff-vector-y))
    (cond ( ;; Top border
	   (and (<= hidden-x-pixels (+ x-off-plot-max-pixels (- hidden-y-pixels) y-off-plot-min-pixels))
		(>= hidden-x-pixels (+ hidden-y-pixels (- y-off-plot-min-pixels) x-off-plot-min-pixels))
		(<= hidden-y-pixels y-off-plot-min-pixels))
	   (when *debug-plot-border-point* (format t "over top border  ~%"))
	   (setq final-diff-vector-y y-off-plot-min-pixels)
	   (setq final-diff-vector-x (+ shown-x-pixels (round (/ (- final-diff-vector-y shown-y-pixels) (tan diff-vector-angle))))))
	  ;; Bottom border
	  ((and (>= hidden-x-pixels (+ y-off-plot-max-pixels (- hidden-y-pixels) x-off-plot-min-pixels))
		(<= hidden-x-pixels (+ hidden-y-pixels x-off-plot-max-pixels (- y-off-plot-max-pixels)))
		(>= hidden-y-pixels y-off-plot-max-pixels))
	   (when *debug-plot-border-point* (format t "over bottom border  ~%"))
	   (setq final-diff-vector-y y-off-plot-max-pixels)
	   (setq final-diff-vector-x (+ shown-x-pixels (round (/ (- final-diff-vector-y shown-y-pixels) (tan diff-vector-angle))))))
	  ;; Left border
	  ((and (>= hidden-y-pixels (+ hidden-x-pixels (- x-off-plot-min-pixels) y-off-plot-min-pixels))
		(<= hidden-y-pixels (+ y-off-plot-max-pixels (- hidden-x-pixels) x-off-plot-min-pixels))
		(<= hidden-x-pixels x-off-plot-min-pixels))
	   (when *debug-plot-border-point* (format t "over left border  ~%"))
	   (setq final-diff-vector-x x-off-plot-min-pixels)
	   (setq final-diff-vector-y (round (+ shown-y-pixels (* (- final-diff-vector-x shown-x-pixels) (tan diff-vector-angle))))))
	  ;; Right border
	  ((and (>= hidden-y-pixels (+ x-off-plot-max-pixels (- hidden-x-pixels) y-off-plot-min-pixels))
		(<= hidden-y-pixels (+ hidden-x-pixels (- x-off-plot-max-pixels) y-off-plot-max-pixels))
		(>= hidden-x-pixels x-off-plot-max-pixels))
	   (when *debug-plot-border-point* (format t "over right border  ~%"))
	   (setq final-diff-vector-x x-off-plot-max-pixels)
	   (setq final-diff-vector-y (round (+ shown-y-pixels (* (- final-diff-vector-x shown-x-pixels) (tan diff-vector-angle)))))))
    (unless (and final-diff-vector-x final-diff-vector-y) (break))
    (when *debug-plot-border-point* (format t " final-diff-vector-x ~A, final-diff-vector-y ~A~%" final-diff-vector-x final-diff-vector-y))
    (values final-diff-vector-x final-diff-vector-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Density Plotting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-2d-density-array (win array array-width-x array-width-y left-border top-border rect-width rect-height z-min z-amp color invert)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type (array single-float (* *)) array)
	   (fixnum left-border rect-width top-border rect-height array-width-x array-width-y)
	   (single-float z-min z-amp))
  (let ((v-agg (create-instance
		nil opal:virtual-aggregate
		(:item-prototype virtual-rectangle)
		(:item-array
		 (list-to-array
		  (loop for left fixnum from left-border by rect-width
			for x fixnum from 0 to (the fn (1- array-width-x))
			nconc (loop for y fixnum from 0 to (the fn (1- array-width-y))
				    for top fixnum from top-border by rect-height
				    collect
				    ;;x y w h c
				    (let ((val (the sf (/ (the sf (- (aref array x y) z-min)) z-amp))))
				      (make-virtual-rectangle-item-values-array
				       left top rect-width rect-height (get-number-fill (if invert (- 1 val) val) color)))))))
		(:point-in-item nil))))
    (opal:add-component (gv win :aggregate) v-agg :where :front)
    (resurrect-opal-win win)))

(defun add-density-plot-axes (win x-min x-max y-min y-max x-label-text y-label-text array-width-x array-width-y left-border rect-width top-border rect-height)
  (let* ((axes-agg (get-plot-agg win 'data-axes t)) ; Get a data-axis agg and clear it.
	 ;; (axis-line-style (or (gv win :axis-line-style) *default-axis-line-style*))
	 (x-axis-y-position (round (+ top-border (* rect-height array-width-y) (/ rect-height 2))))
	 (y-axis-x-position (round (- left-border 10 ; (/ rect-width 2)
				      )))
	 (x-inc (s-flt (/ (- x-max x-min) array-width-x)))
	 (y-inc (s-flt (/ (- y-max y-min) array-width-y)))
	 (tick-length 5))
    ;; X axis
    (when (gv win :x-axis-p)
      (let ((x-label
	     (create-instance nil axis-text
			      (:label-position (o-formula (or (gvl :window :x-label-v-position) :center)))
			      (:left (round  (/ (gv win :width) 2)))
			      (:top (o-formula
				     (+ x-axis-y-position
					(if (gvl :window)
					    (gv (window-plot-axis-font (gvl :window)) :font-height) ; Tick Numbers
					    0)
					tick-length
					5))))))
	(s-value x-label :window win)
	(opal:add-component axes-agg x-label)
	(s-value x-label :string x-label-text)

	(loop for x-tick-position from left-border by rect-width
	      for x-value from x-min by x-inc
	      for count from 0 to array-width-x
	      do
	      (when (= 0 (mod count (1+ (gv win :x-axis-tick-skip))))
		(x-axis-tick-label axes-agg x-axis-y-position x-value x-tick-position))
	      (when (= 0 (mod count (1+ (gv win :x-axis-tick-mark-skip))))
		(opal::add-component
		 axes-agg
		 (create-instance nil opal:line
				  (:line-style (o-formula (create-plot-line-style plot-line-style win)))
				  (:x1 x-tick-position)
				  (:x2 x-tick-position)
				  (:y1 x-axis-y-position)
				  (:y2 (+ (if (= 0 (mod count (1+ (gv win :x-axis-tick-skip))))
					      tick-length (- tick-length 2))
					  x-axis-y-position))))))
	(opal::add-component
	 axes-agg
	 (create-instance nil opal:line
			  (:line-style (o-formula (create-plot-line-style plot-line-style win)))
			  (:x1 left-border)
			  (:x2 (+ left-border (* rect-width array-width-x)))
			  (:y1 x-axis-y-position)
			  (:y2 x-axis-y-position)))))
    ;; Y axis
    (when (gv win :y-axis-p)
      (let ((y-label
	     (create-instance nil axis-text
			      (:label-position (o-formula (or (gvl :window :y-label-h-position) :left)))
			      (:left y-axis-x-position)
			      (:top (o-formula
				     (- top-border
					(+ (if (gvl :window)
					       (gv (window-plot-axis-font (gvl :window)) :font-height)
					       0)
					   8)))))))
	(s-value y-label :window win)
	(opal:add-component axes-agg y-label)
	(s-value y-label :string y-label-text)
	(loop for y-tick-position from top-border by rect-height
	      for y-value from y-max by (- y-inc)
	      for count from 0 to array-width-y
	      do
	      (when (= 0 (mod count (1+ (gv win :y-axis-tick-skip))))
		(y-axis-tick-label axes-agg y-axis-x-position y-value y-tick-position))
	      (when (= 0 (mod count (1+ (gv win :y-axis-tick-mark-skip))))
		(opal::add-component
		 axes-agg
		 (create-instance nil opal:line
				  (:line-style (create-plot-line-style plot-line-style win))
				  (:y1 y-tick-position)
				  (:y2 y-tick-position)
				  (:x1 y-axis-x-position)
				  (:x2 (- y-axis-x-position
					  (if (= 0 (mod count (1+ (gv win :y-axis-tick-skip))))
					      tick-length (- tick-length 2))))))))
	(opal::add-component
	 axes-agg
	 (create-instance nil opal:line
			  (:line-style (create-plot-line-style plot-line-style win))
			  (:y1 top-border)
			  (:y2 (+ top-border (* rect-height array-width-y)))
			  (:x1 y-axis-x-position)
			  (:x2 y-axis-x-position)))))))

(defun density-plot-scale (label &key (min 0.0) (max 1.0) (y-incs 10) y-min y-max (z-min 0) (z-max 1.00) (width 100) (height 400) (left-border 60))
  (let ((scale-array (make-array '(1 100) :element-type 'single-float)))
    (loop for val from (s-flt min) to max by (/ (- max min) 100.0)
	  for i from 0 to 99
	  do (setf (aref scale-array 0 i) val))
    (density-plot scale-array :y-label label :width width :height height
		  :element-aspect-ratio 1.0 :vertical-border 50 :right-border 15 :left-border left-border
		  :y-min y-min :y-max y-max :z-min z-min :z-max z-max
		  :x-axis-p  nil :y-are-fns t :y-axis-tick-skip (round (1- (/ 100 y-incs))) :y-axis-tick-mark-skip (round (1- (/ 100 y-incs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Plotlines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-plotline-menu (win &optional edited-plotline)
  (let ((dummy1 (if edited-plotline (gv edited-plotline :x-start) (gv win :x-min)))
	(dummy2 (if edited-plotline (gv edited-plotline :x-stop) (gv win :x-max)))
	(dummy3 (if edited-plotline (gv edited-plotline :y-start) (gv win :y-min)))
	(dummy4 (if edited-plotline (gv edited-plotline :y-stop) (gv win :y-max)))
	dummy5
	dummy6
	(line-style (if edited-plotline (gv edited-plotline :line-style)
			(or (gv win :plotline-line-style) thick-black-line))))
    (choose-variable-values
     `((dummy1 ,(format nil "Start X ~A" (or (gv win :x-label) ""))  :float)
       (dummy2 ,(format nil "Stop X ~A" (or (gv win :x-label) "")) :float)
       (dummy3 ,(format nil "Start Y ~A" (or (gv win :y-label) "")) :float)
       (dummy4 ,(format nil "Stop Y ~A" (or (gv win :y-label) "")) :float)
       (dummy6 ,(format nil "Choose different line-style~%[Default is ~A]" (wh::line-style-nice-name line-style)) :boolean)
       (dummy5 "CANCEL" :boolean))
     :title (format nil "Add line to ~A" (gv win :title)))
    (unless dummy5
      (when edited-plotline (remove-plotlines win (list edited-plotline)))
      (mark-plotline win dummy1 dummy2 dummy3 dummy4
		     :line-style
		     (or (when dummy6
			   (line-style-menu
			    :default-style line-style
			    :label (format nil "Choose a line style for plotline in ~A" (gv win :title))))
			 line-style)))))

(defun mark-plot-wins-at-time (time &key (win (standard-plot-windows)) (line-style thick-black-line))
  "Add a vertical LINE-STYLE line at TIME, from the current :Y-MIN to the current :Y-MAX of each plot window referenced by WIN."
  (loop for win in (coerce-to-list win) when (eq :standard-plot (gv win :mode))
	do (mark-plotline win time time (gv win :y-min) (gv win :y-max) :line-style line-style)))

(defun mark-plotlines (win coordinates &key (update t) (line-style thick-black-line) clear-previous)
  ;; COORDINATES is a list of coordinate values '((X-START X-STOP Y-START Y-STOP) ...)
  (when clear-previous (remove-plotlines win))
  (loop for values in coordinates do (mark-plotline win (first values) (second values) (third values) (fourth values) :update update :line-style line-style)))

(defun mark-plotline (win x-start x-stop y-start y-stop &key (update t) (line-style thick-black-line) clear-previous)
  (when clear-previous (remove-plotlines win))
  (s-value win :plotlines (no-nils (loop for plotline in (gv win :plotlines) when (opal-obj-exists plotline) collect plotline)))
  (let* ((x-start (s-flt x-start))
	 (x-stop (s-flt x-stop))
	 (y-start (s-flt y-start))
	 (y-stop (s-flt y-stop))
	 (previous-one (loop for plotline in (gv win :plotlines)
			     when (and (= x-start (gv plotline :x-start)) (= x-stop (gv plotline :x-stop))
				       (= y-start (gv plotline :y-start)) (= y-stop (gv plotline :y-stop))
				       (eq line-style (gv plotline :line-style)))
			     do (return plotline)))
	 (x-start-pixels (x-plot-win-float x-start win))
	 (x-stop-pixels (x-plot-win-float x-stop win))
	 (y-start-pixels (y-plot-win-float y-start win))
	 (y-stop-pixels (y-plot-win-float y-stop win))
	 (all-ends-in-window-p (and (<= 0 x-start-pixels (gv win :width))
				    (<= 0 x-stop-pixels (gv win :width))
				    (<= 0 y-start-pixels (gv win :height))
				    (<= 0 y-stop-pixels (gv win ::height)))))
    (or previous-one
	(let ((aggad
	       (create-instance
		nil opal:aggregadget
		(:label t) (:x-start x-start) (:x-stop x-stop)
		(:y-start y-start) (:y-stop y-stop)
		(:line-style line-style)
		(:v-bar-width 10) (:h-bar-width 10)
		(:parts
		 (when t		; all-ends-in-window-p
		   `((:line
		      ,opal:line
		      (:line-style ,(create-plot-line-style line-style win))
		      (:known-as :line)
		      (:x1 ,(o-formula (x-plot-win-float (gvl :parent :x-start) (gvl :parent :window))))
		      (:x2 ,(o-formula (x-plot-win-float (gvl :parent :x-stop) (gvl :parent :window))))
		      (:y1 ,(o-formula (y-plot-win-float (gvl :parent :y-start) (gvl :parent :window))))
		      (:y2 ,(o-formula (y-plot-win-float (gvl :parent :y-stop) (gvl :parent :window)))))))))))
	  (s-value win :plotline-line-style line-style)
	  (push (opal:add-component (gv win :aggregate) aggad) (gv win :plotlines))
	  (when update (resurrect-opal-win win))
	  aggad))))

;; Backward comp
(defun mark-baseline (win x-start x-stop y-start y-stop &key (update t) (line-style thick-black-line) clear-previous)
  (mark-plotline win x-start x-stop y-start y-stop :update update :line-style line-style :clear-previous clear-previous))

(defun frame-plot (&optional win (line-style thin-line))
  (loop for win in (coerce-to-list (or win (win-menu "Select Plot to Add Frame" (standard-plot-windows)))) do
	(let ((x-min (gv win :x-min))
	      (x-max (gv win :x-max))
	      (y-min (gv win :y-min))
	      (y-max (gv win :y-max)))
	  (loop for x-start in (list x-min x-min x-min x-max)
		for x-stop  in (list x-max x-max x-min x-max)
		for y-start in (list y-min y-max y-min y-min)
		for y-stop  in (list y-min y-max y-max y-max)
		do (mark-plotline win x-start x-stop y-start y-stop :line-style line-style)))))

(defun frame-plot (&optional win (line-style thin-line))
  (mapcar #'(lambda (win)
	      (let ((x-min (gv win :x-min))
		    (x-max (gv win :x-max))
		    (y-min (gv win :y-min))
		    (y-max (gv win :y-max)))
		(mapcar #'(lambda (x-start x-stop y-start y-stop) (mark-plotline win x-start x-stop y-start y-stop :line-style line-style))
			(list x-min x-min x-min x-max)
			(list x-max x-max x-min x-max)
			(list y-min y-max y-min y-min)
			(list y-min y-max y-max y-max))))
	  (coerce-to-list (or win (win-menu "Select Plot to Add Frame" (standard-plot-windows))))))


(defun mark-plot-origin-axises (win &optional (line-style thin-line))
  (loop for win in (coerce-to-list win) do
	(mark-plotline win  0.0 0.0 (gv win :y-min) (gv win :y-max) :line-style line-style)
	(mark-plotline win (gv win :x-min) (gv win :x-max) 0.0 0.0 :line-style line-style)))

(defun mark-plot-odd-quadrant-diagonal (&optional (win (standard-plot-windows)) &key (line-style dotted-line-2))
  (mapcar #'(lambda (win) (mark-plotline win (gv win :x-min) (gv win :x-max) (gv win :y-min) (gv win :y-max) :line-style line-style)) (coerce-to-list win)))

(defun mark-plot-even-quadrant-diagonal (&optional (win (standard-plot-windows)) &key (line-style dotted-line-2))
  (mapcar #'(lambda (win) (mark-plotline win (gv win :x-min) (gv win :x-max) (gv win :y-max) (gv win :y-min) :line-style line-style)) (coerce-to-list win)))

(defun mark-plot-diagonal (&optional (win (standard-plot-windows)) &key (line-style dotted-line-2)) (mark-plot-odd-quadrant-diagonal win :line-style line-style))

(defun update-plotlines (win)
  (let ((original-points (no-nils (gv win :plotlines))))
    (s-value win :plotlines nil)
    (loop for point in original-points
	  collect (mark-plotline win (gv point :x-start) (gv point :x-stop) (gv point :y-start) (gv point :y-stop)
				 :line-style (when (gv point :line) (gv point :line :line-style)))
	  into new-points
	  do (opal:remove-component (gv win :aggregate) point) (opal:destroy point)
	  finally (s-value win :plotlines new-points))))

(defun plotline-description-string (point win)
  (format nil "Plotline from [~,2f~d, ~,2f~d] to [~,2f~d, ~,2f~d]"
	  (gv point :x-start) (gv win :x-label)
	  (gv point :y-start) (gv win :y-label)
	  (gv point :x-stop)  (gv win :x-label)
	  (gv point :y-stop)  (gv win :y-label)))

(defun remove-plotlines (win &optional (removed-plotlines :all))
  (loop for win in (case win
		     (:all (garnet-debug:windows))
		     (t (coerce-to-list win)))
	do
	(loop for point in (no-nils (gv win :plotlines))
	      when (or (eq removed-plotlines :all)
		       (member point (if (listp removed-plotlines) removed-plotlines (list removed-plotlines))))
	      do (opal:remove-component (gv win :aggregate) point)
	      else collect point into points
	      finally (s-value win :plotlines points))))

(defun choose-plotlines (win menu-label)
  (let ((plotlines (gv win :plotlines)))
    (choose-list-values-from-keys
     (loop for point in plotlines collect (list (plotline-description-string point win) point)) nil :punt-if-only-one-entry nil :label menu-label)))

(defun edit-plotlines (win)
  (remove-plotlines win (choose-plotlines win (format nil "Remove these plotlines from ~A" (gv win :title))))
  (loop for plotline in (choose-plotlines win (format nil "Edit these plotlines in ~A" (gv win :title))) do (mark-plotline-menu win plotline))
  (resurrect-opal-win win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-virtual-aggs (win)
  (loop for comp in (gv win :aggregate :components)
	when (eq  opal:virtual-aggregate (car (gv comp :is-a))) do
	(opal:remove-component (gv win :aggregate) comp)
	(opal:destroy comp)))

(proclaim '(inline find-max-y-out-of-xy-list find-min-y))

(defun find-max-y-out-of-xy-list (point-list)
  ;; This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns ymax.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for point in point-list
	for i from 0
	when (oddp (the fn i)) maximize (the fn point)))

(defun find-min-y-out-of-xy-list (point-list)
  ;; This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns ymin.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for point in point-list
	for i from 0
	when (oddp (the fn i)) minimize (the fn point)))

(defun clear-up-label-list (win &optional labels)
  (let ((label-list (coerce-to-list (or labels (g-value win :label-list))))
	(curves-per-group (num-curves-per-group win)))
    (unless (>= (length labels) curves-per-group)
      (setq label-list
	    (loop for numbered-canonic-label in (get-canonic-labels curves-per-group (g-value win :canonic-label))
	       for index from 0
	       collect (or (nth index label-list) numbered-canonic-label))))
    ;; Make sure that all the labels are strings.
    (when (not (loop for label in label-list always (and (stringp label) (string= "" label))))
      (s-value win :label-list
	       (loop for count from 0 to (1- curves-per-group)
		  collect (let ((label (nth count label-list)))
			    (typecase label
			      (string label)
			      (number (princ-to-string label))
			      (t (if label (format nil "~A" label) "")))))))))

(defun make-time-sequence (window-y-lists delta-t &optional (start-time 0.0))
  ;; For getting a time sequence based on a DELTA-T and the length of the data lists
  (list-of-nums
   (1+ (loop for list in (if (consp (caar window-y-lists)) (car window-y-lists) window-y-lists)
	     maximize (length list)))
   start-time
   delta-t))

;; PARSED-XORY-LISTS (data-lists)
;; Also coerces to single float, and array to list conversion.
;;
;;  DATA-LISTS can be -
;;
;;  array
;;  (...)
;;  (array ... )
;;  (...)
;;  ((...) ... )
;;  (((...) ... ) ((...) ... ) ((...) ... ) ... )
;;
;; We want -
;;
;;  (((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ....)
;;                                           \___________________________/
;;                                               old-lists when overlay

(defun numeric-plot-data-p (data-lists)
  (and data-lists
       (loop for new-data in data-lists
	  when (or (number-sequence-p new-data) (numberp new-data))
	  do (return t)
	  finally (return (numeric-plot-data-p new-data)))))

(defun update-plot-xy-win-xy-data-lists (win xy-data-lists &optional transpose-data) ;; For PLOT-XY-DATA
  (macrolet ((parse-xy-data-lists (index)
	       `(if (consp (caar xy-data-lists))
		    ;; xy-data-lists = '(((x1 x1 ... x1)(y1 y1 ... y1)) ... ((xn xn ... xn)(yn yn ... yn)))
		    (list (loop for xy-data-list in xy-data-lists collect (nth ,index xy-data-list))) ;; (car xy-data-list) = (x1 x1 ... x1) -> (((x1 x1 ... x1) (x2 x2 ... x2)...))
		    ;; xy-data-lists = '((x x ... x)(y y ... y))
		    (list (list (nth ,index xy-data-lists)))))) ;; (car xy-data-lists) = (x x ... x) -> (((x x ... x)))
    (s-value win :x-lists (parsed-xory-lists (parse-xy-data-lists (if transpose-data 1 0))))
    (s-value win :y-lists (parsed-xory-lists (parse-xy-data-lists (if transpose-data 0 1))))))

(defun parsed-xory-lists (data-lists)
  ;; Return ((list1_set1 list2_set1...)
  ;;         (list1_set2 list2_set2...)
  ;;         ....
  ;;        )
  (flet ((parsed-xory-lists-error () (sim-error (format nil "Some plotting function (e.g. PLOT-TIMED-DATA) was given a NIL data list"))))
    (cond
      ((and (listp data-lists) (not (car data-lists))) (parsed-xory-lists-error))
      ((arrayp data-lists)		; data-lists = array ->  (( (array-vals) ))
       (list (list (array-to-float-list data-lists))))
      ((not (or (consp (car data-lists)) (arrayp (car data-lists)))) ; data-lists = (vals) -> (( (vals) ))
       (list (list (float-list data-lists))))
      ((or (arrayp (car data-lists))	; data-lists = (array array array ...) OR ( (vals) (vals)...) - list of arrays, or list of lists
	   (not (consp (caar data-lists))))
       (list (loop for seq in data-lists collect (S-flt-LIST seq)))) ; (( (vals) (vals) (vals) ... )) OR (( (array-vals) (array-vals) (array-vals) ... ))
      (t ; (printvars data-lists)
	 (loop for list in data-lists ;  data-lists = ??
	    when list collect (car (parsed-xory-lists list))
	    else do (parsed-xory-lists-error))))))

;; Both :x-lists and :y-lists have the same structure:
;; # groups = (length (gv win :y-lists)) = (length (gv win :x-lists))
;; For xref= (nth group (gv win :x-lists)), xref is either a number (delta-x) or a list of x value lists
;; For yref= (nth group (gv win :y-lists)), yref is a list of y value lists (i.e. traces)
;;
;;
;; PLOT-TIMED-DATA (load-y-lists win data-sequences)
;; parsed-xor-y-lists
;;        ((list1_set1 list2_set1...)
;;         (list1_set2 list2_set2...)
;;         ....
;;        )
;;
(defun load-y-lists (win data-sequences)
  (loop for new-data-seq in (reverse (parsed-xory-lists data-sequences))
       for count from 0
     do ;; (printvars count new-data-seq)
       (s-value win :y-lists (cons new-data-seq (when (or (> count 0) (gv win :overlay))
						  (gv win :y-lists)))))
					;  (format t ":y-lists ~A~%" (g-value win :y-lists))
  )

#|
If not a number, TIME-BASE can be -
  array
  (...)
  (array ... )
  (...)
  ((...) ... )
  (((...) ... ) ((...) ... ) ((...) ... ) ... )

; We want -
;;
;;  (((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ....)
;;                                           \___________________________/
;;                                               old-lists when overlay
|#

(defun load-x-lists (win time-base)
  (loop for new-data in (if (numberp time-base) (list time-base) (reverse (parsed-xory-lists time-base)))
     do (s-value win :x-lists (cons new-data (when (gv win :overlay) (gv win :x-lists))))))

;;
;;  Format of x and y lists in window slots:
;;
;;  :y-lists -  '( ( (group-1, y-trace-1) (group-1, y-trace-2) ... (group-1, y-trace-n1) )
;;                 ( (group-2, y-trace-1) (group-2, y-trace-2) ... (group-2, y-trace-n2) )
;;                                             .
;;                                             .
;;                                             .
;;                 ( (group-m, y-trace-1) (group-m, y-trace-2) ... (group-m, y-trace-nm) ) )
;;
;;
;;  :x-lists -  '( ( (group-1, x-trace) ) or ( (group-1, x-trace-1) (group-1, x-trace-2) ... (group-1, x-trace-n1) )
;;                 ( (group-2, x-trace) ) or ( (group-2, x-trace-1) (group-2, x-trace-2) ... (group-2, x-trace-n1) )
;;                            .
;;                            .
;;                            .
;;                 ( (group-m, x-trace) ) or ( (group-m, x-trace-1) (group-m, x-trace-2) ... (group-m, x-trace-n1) ) )

(defun get-canonic-labels (num-curves-per-group canonic-label)
  (if (= num-curves-per-group 1)
      '(nil)
      (loop for i from 1 to num-curves-per-group collect (format nil "~a ~a" CANONIC-LABEL i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scatter plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retrieve-scatter-v-aggs (win)
  (loop for comp in (gv (get-plot-agg win 'data-plot) :components)
	when (eq (gv comp :what-is-it) :data-points)
	collect comp))

(defun set-virtual-gob-width-and-height (thing width height)
  (s-value thing :item-array
	   (list-to-array
	    (loop for item-values across (gv thing :item-array)
		  collect (make-virtual-GOB-item-values-array
			   (virtual-gob-x-center item-values)
			   (virtual-gob-y-center item-values)
			   (- (virtual-gob-x-left item-values) (floor (/ width 2)))
			   (+ (virtual-gob-x-right item-values) (ceiling (/ width 2)))
			   (- (virtual-gob-y-top item-values) (floor (/ height 2)))
			   (+ (virtual-gob-y-bottom item-values) (ceiling (/ height 2)))
			   (virtual-gob-line-style item-values)
			   (virtual-gob-filling-style item-values)))))
  (opal::update (gv thing :window) t))

(defun get-scatter-symbol (win curve-num)
  (if (consp (gv win :scatter-symbol))
      (nth (mod curve-num (length (gv win :scatter-symbol))) (gv win :scatter-symbol))
      (case (gv win :scatter-symbol)
	(:all (nth (mod curve-num (length *scatter-symbols*)) *scatter-symbols*))
	(t (gv win :scatter-symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Histo plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-histo-bins (plot-agg points y-base)
  (let* ((win (gv plot-agg :window))
	 x
	 (y-base-pixels (when y-base (y-plot-win-float y-base win)))
	 (stipple-percent (or (gv win :stipple-percent) (unless (gv win :bar-border-p) 50)))
	 (line-style (when (gv win :bar-border-p) thick-line))
	 (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
    (loop for xy in points
	  for count from 0
	  when (evenp count) do (setq x xy)
	  when (oddp count) do (add-histo-bin plot-agg x xy t y-base-pixels line-style filling-style))))

(defun add-histo-bin (plot-agg x y points-are-in-plot-win-coords y-base-pixels line-style filling-style)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((win (gv plot-agg :window))
	 (bin-width (or (gv win :bin-width) 1.0))
	 (x-pixels (the fn (if points-are-in-plot-win-coords (the fn x) (x-plot-win-float x win))))
	 (x-pixels+bin (the fn (if points-are-in-plot-win-coords
				   (+ (the fn x) (x-plot-win-distance bin-width win))
				   (x-plot-win-float (the sf (+ x bin-width)) win))))
	 (y-pixels (the fn (+ 0		; (or y-base-pixels 0)
			      (if points-are-in-plot-win-coords
				  (the fn y)
				  (y-plot-win-float (s-flt y) win))))))
    (when (> (- (the fn (or y-base-pixels (y-plot-win-float 0.0 win))) y-pixels) 0)
      (add-histo-bin-rectangle-to-agg
       plot-agg y-base-pixels line-style filling-style
       x-pixels x-pixels+bin
       y-pixels win))))

(defun add-histo-bin-rectangle-to-agg (plot-agg y-base-pixels line-style filling-style x-pixels x-pixels+bin y-pixels win)
  (opal:add-component
   plot-agg
   (create-instance nil opal:rectangle
		    (:constant t)
		    (:left (- x-pixels 1))
		    (:width (+ 2 (- x-pixels+bin x-pixels)))
		    (:top y-pixels)
		    (:height (- (+ (the fn (or y-base-pixels (y-plot-win-float 0.0 win))) 1) y-pixels))
		    (:line-style (o-formula (create-plot-line-style line-style win)))
		    (:filling-style (o-formula (create-plot-filling-style filling-style win))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Waterfall plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun point-list-w-skirt (point-list win)
  ;; This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns (x0 ymax x0 y0 x1 y1 ... xn yn xn ymax), unless HEM-IS-MAX is nil, in which case
  ;; the result is (x0 ymin x0 y0 x1 y1 ... xn yn xn ymin).
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((last-x (the fn (car (last point-list 2))))
	 (hem-is-max (> (gv win :y-trace-offset) 0.0))
	 (max-y (if (gv win :skirt-to-window-border)
		    (if hem-is-max (gv win :height) 0)
		    (if hem-is-max (find-max-y-out-of-xy-list point-list) (find-min-y-out-of-xy-list point-list)))))
    (cons (car point-list)		; x0
	  (cons max-y (append point-list (list last-x max-y))))))

(defun massage-points (points)
  ;; In case the POINTS list includes NILs, break up into separate lists. POINTS must be fixnums.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (if (null-in-sequence-p points)
      (let (outs out)
	(loop for point in points
	      when point do (push point out)
	      else when out do (push (reverse out) outs) (setq out nil)
	      finally (return (if out (cons (reverse out) outs) outs))))
      (list points)))

(defun new-add-polyline-to-plot (plot-agg points line-style connect-ends)
  (let* ((win (gv plot-agg :window))
	 (x-axis-min-w (x-plot-win-float (gv win :x-axis-min) win))
	 (x-axis-max-w (x-plot-win-float (gv win :x-axis-max) win))
	 (y-axis-min-w (y-plot-win-float (gv win :y-axis-min) win))
	 (y-axis-max-w (y-plot-win-float (gv win :y-axis-max) win))
	 (points (if nil
		     (loop for point in points
			   for count from 0
			   ;; do (format t "Count ~D, Point ~A~%" count point)
			   collect (when point
				     (if (evenp count) ; X value
					 (when (<= x-axis-min-w point x-axis-max-w)
					   point)
					; Y value
					 (when (>= y-axis-min-w point y-axis-max-w)
					   point))))
		     points)))
;    (format t "points ~A~%" points)
    (loop for points in (massage-points points)
	  when (> (length points) 1) do
	  (let* ((point-list (if (not connect-ends)
				 points
				 (let ((first-x (first points))
				       (first-y (second points)))
				   (nconc (nconc points (list first-x)) (list first-y)))))
		 (line-object (create-instance nil (if (gv plot-agg :window :plot-POLYLINE-as-multipoint) opal:multipoint opal:polyline)
					       ;; (:draw-function (o-formula (if-black-then-xor-draw-function)))
					       ;; (:constant t)
					       (:line-style (o-formula (create-plot-line-style line-style win)))
					       (:point-list point-list))))
	   ; (format t "point-list ~A~%" point-list)
	   ; (format t "ls ~A~%" (gv line-object :line-style))
	    (opal:add-component
	     plot-agg line-object
	     :where :back)))))


(defun add-polyline-to-plot (plot-agg win x-list y-list line-style &key connect-ends)
  ;; Just used by add-xy-data-to-plot for plot-xy-data
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type KR::SCHEMA win plot-agg))
  (let* ((x-min (gv win :x-min)) (x-max (gv win :x-max))
	 (y-min (gv win :y-min)) (y-max (gv win :y-max))
	 (x-off-plot-min-pixels (x-plot-win-float x-min win))
	 (x-off-plot-max-pixels (x-plot-win-float x-max win))
	 (y-off-plot-min-pixels (y-plot-win-float y-max win))
	 (y-off-plot-max-pixels (y-plot-win-float y-min win))
	 (x-left-border-pixels (if (gv win :apply-horizontal-borders) x-off-plot-min-pixels 0))
	 (x-right-border-pixels (if (gv win :apply-horizontal-borders) x-off-plot-max-pixels 0))
	 (y-left-border-pixels (if (gv win :apply-vertical-borders) y-off-plot-min-pixels 0))
	 (y-right-border-pixels (if (gv win :apply-vertical-borders) y-off-plot-max-pixels 0))
	 (this-x-pixels 0)
	 (this-y-pixels 0))
    (declare (fixnum x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels
		     x-left-border-pixels x-right-border-pixels y-left-border-pixels y-right-border-pixels this-x-pixels this-y-pixels))
    (opal:add-component
     plot-agg
     (create-instance nil (if (gv plot-agg :window :plot-POLYLINE-as-multipoint) opal:multipoint opal:polyline)
		      ;; (:constant t)
		      ;; (:draw-function (o-formula (if-black-then-xor-draw-function)))
		      (:point-list (loop for x in (if connect-ends (concatenate 'list x-list (list (first x-list))) x-list)
					 for y in (if connect-ends (concatenate 'list y-list (list (first y-list))) y-list)
					 when (and (numberp x) (numberp y))
					 do (setq this-x-pixels (x-plot-win-float (s-flt x) win)
						  this-y-pixels (y-plot-win-float (s-flt y) win))
					 when (and (or (not (gv win :apply-horizontal-borders)) (<= x-left-border-pixels this-x-pixels x-right-border-pixels))
						   (or (not (gv win :apply-vertical-borders)) (<= y-left-border-pixels this-y-pixels y-right-border-pixels)))
					 collect this-x-pixels and collect this-y-pixels))
		      (:line-style (o-formula (create-plot-line-style line-style win)))))))

(defun add-wf-skirt (plot-agg win points)
  (opal:add-component
   plot-agg
   (create-instance nil opal:polyline (:point-list (point-list-w-skirt points win)) (:line-style opal:no-line) (:constant t)
		    (:filling-style (o-formula (create-plot-filling-style opal:white-fill win))))
   :where :back))

(defun add-waterfall-label (plot-agg label x-start-offset y-start-offset)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float x-start-offset y-start-offset)
	   (type kr::schema plot-agg))
  (let ((win (gv plot-agg :window)))
    (unless (gv win :waterfall-label-offset) (s-value win :waterfall-label-offset 0.0))
    (let ((text (opal:add-component plot-agg (create-instance nil axis-text (:label-position :right)))))
      (s-value text :left (the fn (+ (the fn (or (gv win :gap-between-trace-and-waterfall-label) 20))
				     (x-plot-win-float (+ x-start-offset (the sf (gv win :xfrmd-x-data-max))) win))))
      (s-value text :top (the fn (+ -30 (y-plot-win-float (+ (the sf (gv win :y-min))
							     (the sf (gv win :waterfall-label-offset))
							     y-start-offset)
							  win))))
      (s-value text :string label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key and label functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Need to add these with o-formulas
(create-instance 'plot-key-label opal:aggregadget
                 (:top 0)
                 (:left 0)
		 (:parts
		  `((:background ,opal:rectangle
		     (:filling-style ,(o-formula
				       (if (black-p (or (and (gvl :window) (gvl :window :background-color))
							(and (gvl :parent)
							     (gvl :parent :window) (gvl :parent :window :background-color))))
					   opal::black-fill opal::white-fill)))
		     (:left ,(o-formula (gvl :parent :left)))
		     (:top ,(o-formula (gvl :parent :top)))
		     (:width ,(o-formula (+ (the fn (gvl :parent :label :width))
					    (the fn (* 2 *background-border-width*)))))
		     (:height ,(o-formula (+ (the fn (gvl :parent :label :height))
					     (the fn (* 2 *background-border-width*)))))
		     (:box '(0 0 0 0))
		     (:line-style nil))
		    (:label ,axis-text))))

(defun add-key-and-label (plot-agg curve-num line-style label &optional scatter-symbol max-key-width)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type fixnum curve-num))
  (when (and label (> (length (the SIMPLE-BASE-STRING label)) 0))
    (let* ((win (gv plot-agg :parent :window))
	   (include-key (> (length (gv win :label-list)) 1))
	   (label-height (the fn (key-and-label-height win curve-num)))
	   (y (the fn (+ *trace-keys-top* (the fn (* curve-num label-height)))))
	   (y-key (the fn (round (+ y (* 0.5 label-height)))))
	   (text (opal:add-component plot-agg (create-instance nil axis-text (:what-is-it :key-label) (:label-position :right)))))
      (s-value text :string label)
      (s-value text :window (gv plot-agg :window))
      (s-value text :left (if include-key
			      (+ *trace-keys-right*
				 (round (if max-key-width (/ max-key-width 2) 0))
				 10)
			      *trace-keys-left*))
      (s-value text :top (round (- y-key (/ (gv text :height) 2))))
      ;; Background
      (opal:add-component plot-agg (create-instance nil opal:rectangle
						    (:what-is-it :key-label-background)
						    (:filling-style (o-formula
								     (if (black-p (or (and (gvl :window) (gvl :window :background-color))
										      (and (gvl :parent)
											   (gvl :parent :window) (gvl :parent :window :background-color))))
									 opal::black-fill opal::white-fill)))
						    (:left *trace-keys-left*)
						    (:top y)
						    (:width (the fn (+ (+ *trace-keys-right* 10) (fn-gv text :width))))
						    (:height (gv text :height))
						    (:box '(0 0 0 0))
						    (:line-style nil)))
      (opal:move-component plot-agg text :where :front)
      (when include-key
	(let ((key-left (+ (round (if max-key-width (/ max-key-width 2) 0)) *trace-keys-left*)))
	  (when (gv win :scatter)
	    (add-scatter-points-to-plot plot-agg win
					(if (gv win :connect-data-points)
					    (list key-left y-key *trace-keys-right* y-key)
					    (list *trace-keys-middle* y-key))
					line-style scatter-symbol :data-key
					curve-num))
	  (when (gv win :connect-data-points)
	    (opal:add-component		; Data Key
	     plot-agg
	     (create-instance nil opal:line
			      (:line-style (o-formula (create-plot-line-style line-style win)))
			      (:what-is-it :key-label)
			      (:x1 key-left) (:x2 *trace-keys-right*)
			      (:y1 y-key) (:y2 y-key)))))))))

(defun non-null-plot-win-labels (win)
  (when (gv win :label-list)
    (loop for label in (gv win :label-list)
	  thereis (typecase label
		    (string (> (length label) 0))
		    (t label)))))

(defun plot-window-curve-numbers (win number-of-curves)
  (or (gv win :trace-order)
      (loop for count from 0 to (1- number-of-curves) collect count)))

(defun relevant-labels (win number-of-curves)
  (when (non-null-plot-win-labels win)
    (loop for curve-num in (plot-window-curve-numbers win number-of-curves)
	  collect (nth (round curve-num) (gv win :label-list)))))

(defun label-traces (plot-agg number-of-curves &optional line-styles labels)
  (let ((win (gv plot-agg :window))
	(line-styles (or line-styles (get-line-styles))))
    (when (non-null-plot-win-labels win)
      (let ((max-key-width (max-key-width win number-of-curves))
	    (curve-numbers (plot-window-curve-numbers win number-of-curves)))
	(loop for curve-num in curve-numbers
	      for count from 0
	      ;; when (member curve-num (gv win :trace-order)) collect (nth curve-num (gv win :label-list)) into labels
	      collect (get-scatter-symbol win count) into scatter-symbols
	      collect (if (gv win :use-same-line-style)
			  (car line-styles)
			  (nth (mod count (length line-styles)) line-styles))
	      into plot-line-styles
	      finally
	      (loop for curve-num from 0
		    for label in (or labels (relevant-labels win number-of-curves))
		    for scatter-symbol in scatter-symbols
		    for line-style in plot-line-styles
		    do (add-key-and-label plot-agg curve-num line-style label scatter-symbol max-key-width)))))))

(defun max-key-width (win number-of-curves)
  (if (gv win :scatter)
      (round (loop for curve-num from 1 to number-of-curves
		   maximize (multiple-value-bind (height width)
				(scatter-symbol-height-width win (get-scatter-symbol win curve-num) curve-num)
			      width)))
      0))

(defun REFRESH-key-labels-POSITION (win)
  (let ((plot-agg (get-plot-agg win 'data-plot)))
    (loop for comp in (loop for comp in (gv plot-agg :components)
			    when (eq (gv comp :what-is-it) :key-label-background)
			    collect comp)
	  do (opal:move-component plot-agg comp :where :front))
    (loop for comp in (loop for comp in (gv plot-agg :components)
			    when (case (gv comp :what-is-it)
				   ((:key-label :data-key) t))
			    collect comp)
	  do (opal:move-component plot-agg comp :where :front))))

;; These are called by plot-timed-data and plot-xy-data to enable calling setup-plot and for wiping out any old-plot-agg. May 12 2001 LBG, June 8 2001 LBG
(defun run-setup-plot-p (win)
  (or (not (gv win :has-been-setup))
      (and (not (gv win :preserve-plot-layout))
	   (or (not (gv win :overlay))
	       (and (gv win :accomodate-overlays) (gv win :overlay))))))

(defun renew-plot-agg-p (win old-plot-agg)
  (or (not old-plot-agg) (run-setup-plot-p win)
      (and (gv win :overlay) (gv win :accomodate-overlays))))

(defun plot-windows-finishing (win &optional (final-action :resurrect) old-plot-agg)
  (when old-plot-agg (clear-window-aggregate-components win old-plot-agg))
  (update-plotlines win)
  (REFRESH-MARKERs-POSITION win)
  (REFRESH-key-labels-POSITION win)
  (case final-action
    (:resurrect (resurrect-opal-win win))
    (:update (resurrect-opal-win win :update t)))
  win)

(defun plot-linear-regression (x-list y-list win label line-style)
  (multiple-value-bind (xfrmed-time-seq xfrmed-data-list) (GET-XFRMED-POINT-LIST x-list y-list win)
    (multiple-value-bind (slope intercept r) (lin-reg (list xfrmed-time-seq xfrmed-data-list))
      (draw-linear-regression win slope intercept r line-style :label label))))

(defun draw-linear-regression (win slope intercept r line-style &key label)
  (add-comment win (format nil "~ASlp ~,2f~A Itrcpt ~,2f R ~,2f"
			   (if (> (length label) 0) (format nil "~A: " label) "")
			   slope
			   (if (zerop slope) "" (format nil " 1/Slp ~,2f" (/ 1 slope)))
			   intercept r)
	       :append-to-old-comment t)
  (when (and (numberp slope) (numberp intercept))
    (let* ((plot-agg (get-plot-agg win 'data-plot))
	   (y-as-function-of-xmin (+ (* slope (gv win :x-min)) intercept))
	   (y-as-function-of-xmax (+ (* slope (gv win :x-max)) intercept))
	   (x-as-function-of-ymin (unless (zerop slope) (/ (- (gv win :y-min) intercept) slope)))
	   (x-as-function-of-ymax (unless (zerop slope) (/ (- (gv win :y-max) intercept) slope)))
	   (sorted-xys (sort (no-nils (list (list (gv win :x-min) y-as-function-of-xmin)
					    (list (gv win :x-max) y-as-function-of-xmax)
					    (when x-as-function-of-ymin
					      (list x-as-function-of-ymin (gv win :y-min)))
					    (when x-as-function-of-ymax
					      (list x-as-function-of-ymax (gv win :y-max)))))
			     '> :key 'car))
	   (start-point (if x-as-function-of-ymin
			    (nth 1 sorted-xys)
			    (list (gv win :x-min) y-as-function-of-xmin)))
	   (end-point (if x-as-function-of-ymin
			  (nth 2 sorted-xys)
			  (list (gv win :x-max) y-as-function-of-xmax))))
      (opal:add-component plot-agg (create-instance nil opal:line
						    (:line-style (o-formula (create-plot-line-style plot-line-style win)))
						    (:x1 (x-plot-win-float (car start-point) win))
						    (:x2 (x-plot-win-float (car end-point) win))
						    (:y1 (y-plot-win (cadr start-point) win))
						    (:y2 (y-plot-win (cadr end-point) win))
						    (:line-style line-style))))))

;; PLOT-COORDS-POINTER-RESULT This is used as a FINAL-FUNCTION for the plot window coords pointer, i.e.:

;; (add-window-coords-pointer win #'plot-coords-pointer-result))

;; When middle mouse is released the xy coordinates of the last location of the cross hair is
;; displayed in the units of the window data. If there was a previous point so delineated then the
;; slope between that point and the current one is also displayed, again in the units of the data in
;; the window.

(defun event-plotter-point-to-time (win x-point)
  (float (+ (gv win :minimum-event-time)
	    (* (- (gv win :maximum-event-time) (gv win :minimum-event-time))
	       (/ (- x-point (gv win :event-plot-left-border))
		  (gv win :event-plot-width))))))

(defun XY-EVENT-PLOTTER-WIN-TO-POINTS (time dummy-y win)
  (list
   (round (+ (gv win :event-plot-left-border)
	     (* (gv win :event-plot-width)
		(/ (- time (gv win :minimum-event-time))
		   (- (gv win :maximum-event-time) (gv win :minimum-event-time))))))
   0))

;; Have to define this after XY-EVENT-PLOTTER-WIN-TO-POINTS and XY-PLOT-WIN-TO-POINTS.
(defun plot-data-to-points-function (win)
  (or (gv win :data-to-points-function)
      (case (gv win :mode)
	(:2plot #'XY-EVENT-PLOTTER-WIN-TO-POINTS)
	(:3dplot nil)
	(t #'XY-PLOT-WIN-TO-POINTS))))

(defun plot-coords-running-function (interactor points) (plot-coords-pointer-result-core interactor points nil))
(defun plot-coords-pointer-result (interactor points) (plot-coords-pointer-result-core interactor points t))

(defun plot-coords-pointer-result-core (interactor points final)
  (let ((win (gv interactor :window)))
    (unless final (call-prototype-method interactor points)) ; Keep default running action.
    (case (gv interactor :window :mode)
      (:2dplot (cond ((gv win :event-plotter)
		      (let ((time (event-plotter-point-to-time win (nth 2 points))))
			(when (<= (gv win :minimum-event-time) time (gv win :maximum-event-time))
			  (update-running-comment win (concatenate 'string
								   (format nil "Event time: ~,1fms" time)
								   (when (gv win :last-pointer-time)
								     (format nil "~%dt: ~,1fms" (- time (gv win :last-pointer-time)))))))
			(when final
			  (s-value win :last-pointer-time time)
			  (add-marker win points :data-x time :data-to-points-function #'XY-EVENT-PLOTTER-WIN-TO-POINTS))))))
      (:3dplot
       (let ((x (x-plot-win-inv (nth 2 points) win))
	     (y (y-plot-win-inv (nth 3 points) win))
	     *automatic-run*)
	 (when final
	   (s-value win :last-pointer-xy (list x y))
	   (add-marker win points :add-cross-hair t :data-x x :data-y y :data-to-points-function #'XY-PLOT-WIN-TO-POINTS))))
      (t
       (let ((x (x-plot-win-inv (nth 2 points) win))
	     (y (y-plot-win-inv (nth 3 points) win))
	     *automatic-run*)
	 (update-running-comment win (concatenate
				      'string
				      (plot-coords-pointer-current-value-string x y win)
				      (when (gv win :last-pointer-xy) (plot-coords-pointer-last-pointer-value-string x y win))))
	 (when final
	   (s-value win :last-pointer-xy (list x y))
	   (add-marker win points :data-x x :data-y y :data-to-points-function #'XY-PLOT-WIN-TO-POINTS))))))
  nil)

(defun plot-coords-pointer-last-pointer-value-string (x y win)
  (case (gv win :plot-type)
    ((or :waterfall :xy)
     (let ((dy (- y (nth 1 (gv win :last-pointer-xy))))
	   (dx  (- x (nth 0 (gv win :last-pointer-xy)))))
       (format nil "~%dy/dx: ~a/~a" (plot-window-y-number-format dy win) (plot-window-x-number-format dx win))))))

(defun plot-coords-pointer-current-value-string (x y win)
  (let ((x-label (get-plot-axis-label win :x))
	(y-label (get-plot-axis-label win :y)))
    (case (gv win :plot-type)
      (:polar (format nil "Mag: ~a ~a~%Angle: ~a degrees"
		      (tidy-number-format (cartesian-distance 0.0 0.0 x y) :default-decimals 4)
		      x-label
		      (tidy-number-format (rad-to-deg (atan y x)) :default-decimals 2)))
      (t (format nil "X: ~a ~a~%Y: ~a ~a"
		 (plot-window-x-number-format x win) x-label
		 (plot-window-y-number-format y win) y-label)))))

(defun mark-plot-coords-pointer-result (interactor points)
  (declare (ignore points))
  (mark-coords-pointer-menu (gv interactor :window)))

;;; These three functions work on the plot windows produced by PLOT-TIMED-DATA.
(defun reset-plot (win point-list)
  (let* ((left (the fn (first point-list)))
	 (top (the fn (second point-list)))
	 (width (the fn (third point-list)))
	 (height (the fn (fourth point-list)))
	 (x-min (x-plot-win-inv left win))
	 (y-min (y-plot-win-inv (+ top height) win))
	 (x-max (x-plot-win-inv (+ left width) win))
	 (y-max (y-plot-win-inv top win)))
    (setup-plot win :y-min-spec y-min :y-max-spec y-max :x-min-spec x-min :x-max-spec x-max)))

(defun zoom (interactor point-list)
  (let ((win (gv interactor :window))
	*create-new-plot-windows*
	*preserve-plot-layout*
	*automatic-run*)
    (unless (or (not (gv win :y-lists))
		(gv win :data-erased)
		(eq :2dplot (gv win :mode))
		(eq :scanner (gv win :mode))
		(gv win :waterfall))
      (add-temp-comment win "Zooming...")
      (plot-timed-data nil nil nil
		       :win win
		       :preserve-win-attributes t
		       :preserve-plot-attributes t
		       :replot-win-point-list point-list
		       :resurrect nil
		       :upper-right-comment "Zooming..")
      (add-temp-comment win ""))))

(defun find-comment (win))

(defun zoom-to-new-window (interactor point-list)
  (unless (or (gv interactor :window :data-erased)
	      (eq :2dplot (gv interactor :window :mode))
	      (eq :scanner (gv interactor :window :mode))
	      (gv interactor :window :waterfall))
    (let* ((parent-win (gv interactor :window))
	   (win (get-child-plot-window parent-win))
	   (left (the fn (first point-list)))
	   (top (the fn (second point-list)))
	   (width (the fn (third point-list)))
	   (height (the fn (fourth point-list)))
	   ;; Need to convert the coordinates from the mouse to data coordinates.
	   (x-min (x-plot-win-inv left parent-win))
	   (y-min (y-plot-win-inv (+ top height) parent-win))
	   (x-max (x-plot-win-inv (+ left width) parent-win))
	   (y-max (y-plot-win-inv top parent-win))
	   *automatic-run* *create-new-plot-windows*)
      (when win
	(add-temp-comment parent-win "Parenting...")
	(s-value parent-win :child-number (1+ (gv parent-win :child-number)))
	(s-value win :mode (gv parent-win :mode))
	(case (gv interactor :window :mode)
	  (:standard-plot (plot-timed-data
			   nil (gv parent-win :label-list) nil
			   :x-lists (gv parent-win :x-lists) :y-lists (gv parent-win :y-lists)
			   :win win :preserve-win-attributes t :preserve-plot-attributes t
			   :x-are-fns (gv parent-win :x-are-fns) :y-are-fns (gv parent-win :y-are-fns)
			   :draw-grid (gv parent-win :draw-grid)
			   :label-traces (gv parent-win :label-traces)
			   :x-min x-min :x-max x-max :y-min y-min :y-max y-max
			   :x-label (gv parent-win :x-label) :y-label (gv parent-win :y-label)))
	  (:histology nil))		; add something to zoom histology
	(add-temp-comment parent-win "")
	(add-temp-comment win "")))))

(defun unzoom (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (gv interactor :window))
	*preserve-plot-layout* *automatic-run* *create-new-plot-windows*)
    (unless (or (not (gv win :y-lists))
		(gv win :data-erased)
		(eq :2dplot (gv win :mode))
		(gv win :waterfall)
		(not (gv win :min-max-lists)))
      ;; (remove-all-markers win)
      (add-temp-comment win "UnZooming...")
      (plot-timed-data nil nil nil
		       :win win
		       :unzoom t
		       :preserve-win-attributes t
		       :preserve-plot-attributes t		       :resurrect nil
		       :upper-right-comment "UnZooming..")
      (add-temp-comment win ""))))

(defun restore-plot (interactor-or-win &optional final-obj-over)
  (declare (ignore final-obj-over))
  (unless (or (not (gv interactor-or-win :window :y-lists))
	      (eq :2dplot (gv interactor-or-win :window :mode))
	      (gv interactor-or-win :window :data-erased))
    (let ((win (if (eq (car (gv interactor-or-win :is-a)) ph::plot-window)
		   interactor-or-win
		   (gv interactor-or-win :window)))
	  *automatic-run* *create-new-plot-windows*)
      (add-temp-comment win "Restoring...")
      (s-value win :has-been-setup nil)
      (plot-timed-data nil nil nil
		       :win win
		       :restore-plot t
		       :preserve-win-attributes t
		       :resurrect nil
		       :upper-right-comment "Restoring..")
      (add-temp-comment win ""))))

;; (PH::RESTORE-PLOTS :all #'(lambda (win) (s-value win :label-traces nil)))
(defun restore-plots (&optional (windows :all) pre-restore-function)
  "Restore standard plot windows as referenced by WINDOWS [default :ALL], calling PRE-RESTORE-FUNCTION on each window prior to the
restore, if supplied."
  (loop for window in (find-output-windows (coerce-to-list (case windows
							     (:all (windows-of-mode :standard-plot))
							     (t windows))) :standard-plot)
	when pre-restore-function do (funcall pre-restore-function window)
	do (restore-plot window)))

(defun edit-individual-scatter-dimensions (&optional plot-win)
  (let ((plot-win (or plot-win (win-menu "Select Plot to Edit Scatter Dimensions" (standard-plot-windows) nil t)))
	temp-line-styles temp-borderps temp-scatter-symbols)
    (loop for v-agg in (retrieve-scatter-v-aggs plot-win)
	  for curve-num from 0
	  do (let* ((dummy5 (SCATTER-PROTOTYPE-TO-SYMBOL (gv v-agg :item-prototype)))
		    (dummy1 (round (or (car (nth curve-num (gv plot-win :scatter-width-heights)))
				       (gv v-agg :symbol-width)
				       (gv plot-win :x-symbol-width)
				       10)))
		    (dummy2 (round (or (cadr (nth curve-num (gv plot-win :scatter-width-heights)))
				       (gv v-agg :symbol-height)
				       (gv plot-win :y-symbol-width)
				       10)))
		    (line-style (or (get-scatter-line-style plot-win curve-num) width-1-line))
		    (dummy3 (gv line-style :line-thickness))
		    (dummy4 (get-scatter-symbol-borderp plot-win curve-num))
		    (menu-list `((dummy5 "Scatter symbol:" :choose ,*scatter-symbols*)
				 (dummy1 "Symbol width (ignored for dots):" :integer)
				 (dummy2 "Symbol height:" :integer)
				 (dummy4 "Include border" :boolean)
				 (dummy3 ,(format nil "Border thickness (1 to ~d)" (length varying-width)) :integer))))
	       (choose-variable-values menu-list :label (format nil "Edit symbol for the ~:R data list" (1+ curve-num)))
	       (s-value v-agg :item-prototype (scatter-symbol-to-prototype dummy5))
	       (s-value v-agg :symbol-width dummy1)
	       (s-value v-agg :symbol-height dummy2)
	       (set-virtual-gob-width-and-height v-agg dummy1 dummy2)
	       (push dummy5 temp-scatter-symbols)
	       (push (read-from-string (opal::name-for-schema (nth (1- (max 1 (min (length varying-width) dummy3))) varying-width))) temp-line-styles)
	       (push dummy4 temp-borderps))

	  collect (list (gv v-agg :symbol-width) (gv v-agg :symbol-height)) into scatter-width-heights
	  finally
	  (s-value plot-win :scatter-symbol (reverse temp-scatter-symbols))
	  (s-value plot-win :scatter-symbol-borderp (reverse temp-borderps))
	  (s-value plot-win :scatter-width-heights scatter-width-heights)
	  (s-value plot-win :scatter-symbol-line-style (reverse temp-line-styles)))))

(defun plot-line-style-menu (&optional (win (win-menu "Choose plots to assign line styles")))
  (loop for win in (coerce-to-list win) do
	(loop for count from 1
	      for label in (gv win :label-list)
	      do (s-value win :plot-line-style-family (line-style-menu :label (format nil "~:R line style for ~s" count (gv win :title)))))))

(defun 2dplot-menu (win)
  (let* ((dummy1 nil) (dummy2 nil) dummy3 dummy4
	 dummy9
	 dummy12
	 dummy15
	 dummy17
	 dummy22
	 dummy30
	 (menu-list '((dummy22 "Change label font" :boolean))))
    (when (or t				; (gv win :marked-points)
	      (gv win :plotlines))
      (push '(dummy4 "Edit marked points font" :boolean) menu-list))
    (choose-variable-values menu-list :title (format nil "Plot Parameters for ~A" (gv win :title)))
    (cond-every
     (dummy4 (Edit-marked-points-font win))
     (dummy22 (s-value win :font (s-value win :plot-axis-font (font-menu (gv win :plot-axis-font) (format nil "Plot axis font for ~A" (gv win :title))))))))
  (resurrect-opal-win win))

(defun 3dplot-menu (win))

(defun edit-tick-format (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (or (gv win :x-tick-decimal) 0))
	(dummy2 (or (gv win :y-tick-decimal) 0))
	(dummy3 (or (gv win :x-axis-tick-mark-length) *x-axis-tick-mark-length*))
	(dummy4 (or (gv win :y-axis-tick-mark-length) *y-axis-tick-mark-length*)))
    (choose-variable-values
     `((:general-comment
	"If a decimal places spec is 0, and axis values are NOT specified as integers, then the decimal places used is automatically determined.")
       (dummy1 "X tick decimal places" :integer)
       (dummy2 "Y tick decimal places" :integer)
       (:general-comment "If negative, the following will cause axis tick marks to point away from tick labels.")
       (dummy3 "X tick mark length [pixels]" :integer)
       (dummy4 "Y tick mark length [pixels]" :integer))
     :title (gv win :title))
    (s-value revised-win :x-axis-tick-mark-length dummy3)
    (s-value revised-win :y-axis-tick-mark-length dummy4)
    (s-value revised-win :x-tick-decimal (unless (zerop dummy1) dummy1))
    (s-value revised-win :y-tick-decimal (unless (zerop dummy2) dummy2))))

(defun histogram-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let* (dummy1
	 (dummy2 (gv win :x-max))
	 dummy3
	 (dummy4 (round (gv win :y-max)))
	 (dummy5 (gv win :bin-width))
	 (dummy6 (ceiling (gv win :y-inc)))
	 (dummy7 (or (gv win :stipple-percent) 0))
	 (dummy8 (gv win :x-inc))
	 (dummy9 (gv win :x-min))
	 (dummy12 (gv win :title))
	 dummy15
	 dummy16
	 (dummy17 (gv win :width))
	 (dummy19 (gv win :height))
	 dummy20 dummy22 dummy26 )
    (choose-variable-values
     '((dummy6 "Y axis interval" :integer)
       (dummy4 "Y maximum" :integer)
       (dummy9 "X minimum" :number)
       (dummy2 "X maximum" :number)
       (dummy12 "title" :string)
       (dummy1 "Edit axes" :boolean)
       (dummy17 "Window width" :integer)
       (dummy19 "Window height" :integer)
       (dummy7 "Stipple percent" :integer)
       (dummy15 "Edit space around plot" :boolean)
       (dummy22 "Change label font" :boolean)
       (dummy20 "CANCEL" :boolean))
     :title (gv win :title))
    (unless dummy20
      (cond-every
       (dummy1 (axes-menu win))
       (dummy15 (plotting-space-menu win revised-win))
       (dummy22
	(s-value win :comment-font (s-value win :font (s-value win :plot-axis-font (font-menu (gv win :plot-axis-font) (format nil "Plot axis font for ~A" (gv win :title))))))
	;; (s-value win :font (s-value win :plot-axis-font (font-menu (gv win :plot-axis-font) (gv win :title))))
	;; (change-comment-font win (gv win :plot-axis-font))
	))
      (s-value win :title dummy12)
      (s-value win :x-label-h-position dummy28)
      (plot-histogram-data (caar (gv win :y-lists))
			   dummy9
			   dummy2


			   :win win :title dummy12
			   :stipple-percent dummy7
			   :x-axis-tick-skip (gv win :x-axis-tick-skip)
			   :x-are-fns (gv win :x-are-fns)
			   :x-origin dummy9 ; (gv win :x-origin)
					; :bin-width (gv win :bin-width)
			   :x-label (gv win :x-label) :y-label (gv win :y-label)
			   :width dummy17 :height dummy19

			   :y-max dummy4 :y-inc dummy6))))

(defun waterfall-plot-menu (win)
  (let ((dummy2 (gv win :use-waterfall-y-data-max))
	(dummy3 (or (gv win :waterfall-y-data-max) 0))
	(dummy5 (or (gv win :waterfall-trace-label-skip) 0))
	(dummy6 (or (gv win :waterfall-label-offset) 0.0))
	(dummy7 (gv win :auto-wf-setup))
	(dummy8 (round (* 100 (or (gv win :auto-waterfall-x-trace-overlap) *auto-waterfall-x-trace-overlap*))))
	(dummy9 (round (* 100 (or (gv win :auto-waterfall-y-trace-overlap) *auto-waterfall-y-trace-overlap*))))
	(dummy14 (gv win :x-trace-offset))
	(dummy15 (gv win :y-trace-offset))
	(dummy16 (gv win :label-waterfall))
	(dummy17 (or (gv win :waterfall-base-x-offset) 0.0))
	(dummy18 (gv win :wf-skirt))
	(dummy19 (or (gv win :waterfall-base-y-offset) 0.0))
	(dummy20 (or (gv win :gap-between-trace-and-waterfall-label) 20))
	(dummy21 (gv win :skirt-to-window-border))
	dummy1)
    (choose-variable-values
     `((dummy2 "Use WATERFALL-Y-DATA-MAX (for automatic setup or not)" :boolean)
       (dummy3 ,(format nil "WATERFALL-Y-DATA-MAX (~A)" (gv win :y-label)) :float)
       (dummy7 "Use automatic waterfall setup (otherwise use offset values below)" :boolean)
       (dummy8 "Auto waterfall setup, X trace overlap (%)" :float)
       (dummy9 "Auto waterfall setup, Y trace overlap (%)" :float)
       ("Trace and base offsets apply when automatic setup is NOT chosen above" :comment)
       (dummy14 ,(format nil "X trace offset (~A)" (gv win :x-label)) :number)
       (dummy15 ,(format nil "Y trace offset (~A)" (gv win :y-label)) :number)
       (dummy17 ,(format nil "X base offset (~A)" (gv win :x-label)) :float)
       (dummy19 ,(format nil "Y base offset (~A)" (gv win :y-label)) :float)
       ("Trace labels" :comment)
       (dummy16 "Label waterfall" :boolean)
       (dummy5 "Waterfall trace label skip (0=> no skip)" :integer)
       (dummy6 ,(format nil "If adding trace labels alongside traces, label offset [~A]" (gv win :y-label)) :float)
       (dummy20 "Gap between traces and waterfall labels [pixels]" :integer)
       ("Miscellaneous" :comment)
       (dummy1 "Adjust simple axes" :boolean)
       (dummy18 "Add opaque skirt to traces" :boolean)
       (dummy21 "Extend skirt to window border" :boolean)
       )
     :text (format nil "Editing plot ~A"  (gv win :title))
     :title "Waterfall Plot Parameters")
    (when dummy1 (simple-axes-menu win))
    (s-values win
	      (auto-waterfall-x-trace-overlap (/ dummy8 100))
	      (auto-waterfall-y-trace-overlap (/ dummy9 100))
	      (auto-wf-setup dummy7)
	      (use-waterfall-y-data-max dummy2)
	      (waterfall-y-data-max dummy3)
	      (waterfall-base-x-offset dummy17)
	      (waterfall-base-y-offset dummy19)
	      (waterfall-label-offset dummy6)
	      (x-trace-offset dummy14)
	      (y-trace-offset dummy15)
	      (waterfall-trace-label-skip dummy5)
	      (wf-skirt dummy18)
	      (skirt-to-window-border dummy21)
	      (gap-between-trace-and-waterfall-label dummy20)
	      (label-waterfall dummy16))))

(defun overlay-layout-menu (win)
  (let ((dummy1 (gv win :preserve-plot-layout))
	(dummy2 (gv win :overlay))
	(dummy3 (gv win :accomodate-overlays))
	dummy4
	(dummy5 (or (when (numberp (gv win :plot-point-skip)) (round (gv win :plot-point-skip))) 0))
	(dummy6 (string-capitalize (opal-color-to-string (or (gv win :background-color) *default-graphics-window-background-color*)))))
    (choose-variable-values
     `((dummy4 "Edit space around plot" :boolean)
       (dummy5 ,(format nil "Plot point skip~%(this can recover dash patterns for close points)") :integer)
       (:comment "The following apply to any subsequent plots to this window")
       (dummy2  "Overlay data" :boolean)
       (dummy3 "Adjust window dimensions to accomodate all overlays" :boolean)
       (dummy1 "Preserve layout for new data" :boolean)
       (dummy6 "Background color" :choose ("Black" "White") :label-left))
     :text (concatenate-string-list
	    (no-nils (list (when *overlay-plots* (format nil "Global overlay set=> above flag ignored"))
			   (when *accomodate-overlays* (format nil "Global accomodate overlay set=> above flag ignored"))
			   (when *preserve-plot-layout* (format nil "Global preserve layout set=> above flag ignored"))))
	    :string-spacer (format nil "~%"))
     :title (format nil "Overlay and Layout Parameters for ~A" (gv win :title)))
    (s-values win
	      (plot-point-skip (max 0 dummy5))
	      (preserve-plot-layout dummy1)
	      (overlay dummy2)
	      (accomodate-overlays dummy3)
	      (background-color (string-to-opal-color dummy6)))
    (when dummy4 (plotting-space-menu win))))

(defun simple-axes-menu (win)
  (let ((dummy1 (gv win :x-scale-l%))
	(dummy3 (gv win :y-scale-t%))
	(dummy4 (gv win :x-scale-t%))
	(dummy5 (gv win :x-inc))
	(dummy6 (gv win :y-inc))
	(dummy7 (gv win :simple-axis-x-value-p))
	(dummy8 (gv win :simple-axis-y-value-p)))
    (choose-variable-values
     `((dummy1 "Left end position of X scale bar (% window width, from left)" :float)
       (dummy4 "Position of X scale bar (% window height, from top)" :float)
       (dummy3 "Bottom end position of Y scale bar (% window height, from top)" :float)
       (dummy5 ,(format nil "X scale bar length [~A]" (gv win :x-label)) :number)
       (dummy6 ,(format nil "Y scale bar length [~A]" (gv win :y-label)) :number)
       (dummy7 "Include bar length in X label" :boolean)
       (dummy8 "Include bar length in Y label" :boolean))
     :title (format nil "Simple Axes Parameters for ~A" (gv win :title)))
    (s-values win
	      (simple-axis-x-value-p dummy7)
	      (simple-axis-y-value-p dummy8)
	      (x-inc dummy5)
	      (y-inc dummy6)
	      (x-scale-l% dummy1)
	      (y-scale-t% dummy3)
	      (x-scale-t% dummy4))))

(defun data-and-trace-offset-plot-menu (win)
  (let ((dummy14 (gv win :x-trace-offset))
	(dummy15 (gv win :y-trace-offset))
	(dummy1 (or (gv win :x-data-scale) 1.0))
	(dummy2 (or (gv win :y-data-scale) 1.0))
	(dummy21 (gv win :x-data-offset))
	(dummy22 (gv win :y-data-offset)))
    (choose-variable-values
     `((dummy21 ,(format nil "X data offset (~A) (applied before log, if log coordinates)" (gv win :x-label)) :float)
       (dummy22 ,(format nil "Y data offset (~A) (applied before log, if log coordinates)" (gv win :y-label)) :float)
       (dummy1 "X data scale (applied after data offset and before log, if log coordinates)" :float)
       (dummy2 "Y data scale (applied after data offset and before log, if log coordinates)" :float)
       (dummy14 "X trace offset (for waterfall plots)" :number)
       (dummy15 "Y trace offset (for waterfall plots)" :number))
     :title (format nil "Plot Scale, Data and Trace Offsets for ~A" (gv win :title)))
    (s-values win
	      (x-data-scale dummy1)
	      (y-data-scale dummy2)
	      (x-data-offset dummy21)
	      (y-data-offset dummy22)
	      (x-trace-offset dummy14)
	      (y-trace-offset dummy15))))

(defun edit-labels (win &optional revised-win)
  (clear-up-label-list win (gv win :label-list))
  (s-value (or revised-win win) :label-list (EDIT-string-LIST
					     (sequence-to-string-list (gv win :label-list))
					     :label (format nil "Editing labels for ~a" (gv win :title))
					     :entry-string "trace label")))

(defun scatter-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let* ((dummy1 (if (consp (gv win :scatter-symbol)) :all (or (gv win :scatter-symbol) :dot)))
	 (dummy2 (gv win :x-symbol-width))
	 (dummy3 (gv win :y-symbol-width))
	 (dummy4 (if (gv win :scatter-symbol-units-in-pixels) :pixels :plotted_units))
	 (dummy5 (gv win :fill-scatter))
	 dummy6 dummy7 dummy8
	 (dummy9 (gv win :scatter-symbol-borderp)))
    (unless dummy2 (setq dummy2 (if dummy4 *default-scatter-size* (* 0.02 (gv win :x-mag)))))
    (unless dummy3 (setq dummy3 (if dummy4 *default-scatter-size* (* 0.02 (gv win :y-mag)))))
    (choose-variable-values
     `(,(when (consp (gv win :scatter-symbol))
	  `(dummy8 ,(format nil "Symbols are currently ~A: Clear this and use value below" (gv win :scatter-symbol)) :boolean))
       ,(unless (< (length *scatter-symbols*) 2) `(dummy1 "Scatter symbol: " :choose ,(cons :all *scatter-symbols*)))
       (dummy5 "Fill scatter symbols" :boolean)
       (dummy9 "Include scatter symbol borders" :boolean)
       (,(format nil "The minimum width or height (in pixels) with borders is 2, without is 4~%A value of 3 will be changed to 2.") :comment)
       (dummy3 "Symbol height" :number)
       ,(unless (< (length *scatter-symbols*) 2) `(dummy2 "Symbol width (ignored for dots)" :number))
       (dummy4 "Symbol dimensions in:" :choose (:pixels :plotted_units) :label-left)
       ,(when (gv win :scatter-width-heights) `(dummy7 "Clear individual scatter dimensions" :boolean))
       (dummy6 "Edit individual scatter dimensions" :boolean))
     :title (format nil "Scatter Plot Parameters for ~A" (gv win :title)))

    (when dummy7 (s-value win :scatter-width-heights nil))
    (when (eq dummy4 :pixels)		; pixel widths and heights must be at least 2pixels.
      (setq dummy2 (if (= dummy2 3) 2 (max 2 dummy2)) dummy3 (if (= dummy3 3) 2 (max 2 dummy3))))
    (when (or (not (consp (gv win :scatter-symbol))) dummy8)
      (s-value revised-win :scatter-symbol dummy1))
    (s-value win :scatter-symbol-borderp dummy9)
    (s-values revised-win
	      (fill-scatter dummy5)
	      (scatter-symbol-units-in-pixels (eq dummy4 :pixels))
	      (x-symbol-width dummy2)
	      (y-symbol-width dummy3))
    (when dummy6 (EDIT-INDIVIDUAL-SCATTER-DIMENSIONS revised-win))))

(defun edit-tick-skip (win &optional (revised-win win))
  (let ((dummy3 (or (gv win :y-axis-tick-skip) 0))
	(dummy4 (or (gv win :x-axis-tick-skip) 0))
	(dummy17 (or (gv win :y-axis-tick-mark-skip) 0))
	(dummy18 (or (gv win :x-axis-tick-mark-skip) 0)))
    (choose-variable-values
     `((dummy3 "Y tick label skip" :integer)
       (dummy17 "Y tick skip" :integer)
       (dummy4 "X tick label skip" :integer)
       (dummy18 "X tick skip" :integer))
     :text "For tick and tick mark skips, 0=> no skip" :title (gv win :title))
    (s-values revised-win
	      (y-axis-tick-mark-skip dummy17)
	      (x-axis-tick-mark-skip dummy18)
	      (y-axis-tick-skip dummy3)
	      (x-axis-tick-skip dummy4))
    nil))

(defun border-extrapolation-menu (win)
  (let ((dummy1 (gv win :include-border-points))
	(dummy2 (gv win :apply-horizontal-borders))
	(dummy3 (gv win :apply-vertical-borders)))
    (choose-variable-values
     `(					; (dummy1 "Extrapolate off plot lines to axes borders" :boolean)
       (dummy2 "Apply horizontal borders" :boolean)
       (dummy3 "Apply vertical borders" :boolean))
     :title (gv win :title))
    ;; (s-value win :include-border-points dummy1)
    (s-values win
	      (apply-horizontal-borders dummy2)
	      (apply-vertical-borders dummy3))))

(defun axes-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (cond ((and (gv win :x-are-fns) (gv win :y-are-fns)) :x_&_y)
		      ((gv win :x-are-fns) :x)
		      ((gv win :y-are-fns) :y)
		      (t :neither)))
	dummy2

	(dummy7 (if (numberp (gv win :x-axis-root)) :NUMBER (gv win :x-axis-root)))
	(dummy70 (if (numberp (gv win :x-axis-root)) (gv win :x-axis-root) (gv win :x-origin)))

	(dummy8 (if (numberp (gv win :y-axis-root)) :NUMBER (gv win :y-axis-root)))
	(dummy80 (if (numberp (gv win :y-axis-root)) (gv win :y-axis-root) (gv win :y-origin)))

	(dummy10 (or (gv win :x-label) "")) (dummy11 (or (gv win :y-label) ""))
	(dummy12 (not (gv win :reference-ticks-to-origin)))
	(dummy13 (gv win :y-label-v-position))
	dummy14
	dummy15
	(dummy16 (gv win :x-label-v-position)) (dummy17 (gv win :y-label-h-position))
	dummy21
	dummy22
	(dummy28 (gv win :x-label-h-position))
	(dummy29 (gv win :axes-type))
	dummy30)
    (choose-variable-values
     `(
       (dummy1 "Print ticks as integers:" :choose (:x_&_y :x :y :neither) :rank-margin 4 :label-left)
       (dummy29 "Axes type:" :choose (:standard :simple :none) :label-left)
       (:comment)
       (dummy11 "Y axis label" :string)
       (dummy13 "Y axis label vertical position:" :choose ,*y-label-v-positions* :rank-margin 3)
       (dummy17 "Tick label position relative to Y axis:" :choose ,*y-label-h-positions* :label-left)
       (dummy10 "X axis label" :string)
       (dummy28 "X label position relative to X axis:" :choose ,*x-label-h-positions* :label-left)
       (dummy16 "Tick label position relative to X axis:" :choose ,*x-label-v-positions* :label-left)
       (:comment)
       (dummy12 "Use roots below for tick marks (default is origin)" :boolean)
       (dummy7 "Root for X axis tick marks:" :choose (:max :min :number) :label-left)
       (dummy70 "Value for numeric root for X axis tick marks" :float)
       (dummy8 "Root for Y axis tick marks:" :choose (:max :min :number) :label-left)
       (dummy80 "Value for numeric root for Y axis tick marks" :float)
       (:comment)
       (dummy21 "Border extrapolation menu" :boolean)
       (dummy22 "Axes label values menu:" :boolean)
       (dummy14 "Tick format and length menu" :boolean)
       (dummy2 "Tick skip menu" :boolean)
       (dummy15 "Data grid menu" :boolean)
       (dummy30 "Axes visibility menu" :boolean))
     :title (format nil "Axes etc. menu for ~A" (gv win :title)))
    (s-value revised-win :reference-ticks-to-origin (not dummy12))
    (s-value revised-win :y-label-h-position dummy17)
    (s-value revised-win :x-label-v-position dummy16)
    (when dummy22 (axes-tick-values-menu win revised-win))
    (when dummy21 (border-extrapolation-menu revised-win))
    (when dummy14 (edit-tick-format win revised-win))
    (when dummy2 (EDIT-TICK-SKIP win revised-win))
    (when dummy15 (edit-data-grid win revised-win))
    (when dummy30 (axes-visibility-menu win revised-win))
    (case dummy1
      (:x_&_y (s-value revised-win :x-are-fns t) (s-value revised-win :y-are-fns t))
      (:x (s-value revised-win :x-are-fns t) (s-value revised-win :y-are-fns nil))
      (:y (s-value revised-win :y-are-fns t) (s-value revised-win :x-are-fns nil))
      (t (s-value revised-win :y-are-fns nil) (s-value revised-win :x-are-fns nil)))
    (s-value revised-win :axes-type dummy29)
    (s-value revised-win :x-label dummy10) (s-value revised-win :y-label dummy11)
    (s-value revised-win :y-label-v-position dummy13)
    (s-value revised-win :x-label-h-position dummy28)
    (if t				; dummy12
	(progn (s-value revised-win :x-axis-root (if (eq dummy7 :NUMBER) dummy70 dummy7))
	       (s-value revised-win :y-axis-root (if (eq dummy8 :NUMBER) dummy80 dummy8)))
	(progn (s-value revised-win :x-axis-root (gv win :x-origin))
	       (s-value revised-win :y-axis-root (gv win :y-origin))))))

(defun axes-tick-values-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy22 (or (gv win :x-axis-prefix) ""))
	(dummy23 (or (gv win :x-axis-suffix) ""))
	(dummy24 (or (gv win :y-axis-prefix) ""))
	(dummy25 (or (gv win :y-axis-suffix) ""))
	(dummy26 (or (gv win :x-axis-coeff) 1.0))
	(dummy27 (or (gv win :y-axis-coeff) 1.0)))
    (printvars dummy22)
    (choose-variable-values
     `((dummy22 "X axis value prefix" :string)
       (dummy23 "X axis value suffix" :string)
       (dummy26 "X axis number coefficient" :number)
       (dummy24 "Y axis value prefix" :string)
       (dummy25 "Y axis value suffix" :string)
       (dummy27 "Y axis number coefficient" :number))
     :title (format nil "Axes Label Values: ~A" (gv win :title)))
    (s-value revised-win :x-axis-coeff dummy26)
    (s-value revised-win :y-axis-coeff dummy27)
    (s-value revised-win :x-axis-prefix dummy22)
    (s-value revised-win :x-axis-suffix dummy23)
    (s-value revised-win :y-axis-prefix dummy24)
    (s-value revised-win :y-axis-suffix dummy25)))

(defun axes-visibility-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy10 (list (when (gv win :consider-y-axis-visible-limit) :limit-y-axis)
		       (when (gv win :consider-x-axis-visible-limit) :limit-x-axis)))
	(dummy1 (or (gv win :y-axis-visible-max)  (gv win :y-axis-max)))
	(dummy2 (or (gv win :y-axis-visible-min)  (gv win :y-axis-min)))
	(dummy3 (or (gv win :x-axis-visible-max)  (gv win :x-axis-max)))
	(dummy4 (or (gv win :x-axis-visible-min)  (gv win :x-axis-min)))
	(dummy5 (cond ((and (gv win :x-axis-p) (gv win :y-axis-p)) :x_&_Y)
		      ((gv win :x-axis-p) :x)
		      ((gv win :y-axis-p) :y)
		      (t :none))))
    (choose-variable-values
     `((dummy5 "Visible axes:" :choose (:x_&_Y :x :y :none) :rank-margin 4)
       (dummy10 "Limit visibility of X and/or Y axis:" :x-choose  (:limit-x-axis :limit-y-axis))
       (dummy1 "Y axis maximum" :float)
       (dummy2 "Y axis minimum" :float)
       (dummy3 "X axis maximum" :float)
       (dummy4 "X axis minimum" :float))
     :title (gv win :title))
    (case dummy5
      (:none (s-value revised-win :x-axis-p nil) (s-value revised-win :y-axis-p nil))
      (:x_&_Y (s-value revised-win :x-axis-p t) (s-value revised-win :y-axis-p t))
      (:x (s-value revised-win :x-axis-p t) (s-value revised-win :y-axis-p nil))
      (:y (s-value revised-win :x-axis-p nil) (s-value revised-win :y-axis-p t)))
    (s-value revised-win :consider-y-axis-visible-limit (true-p (member :limit-y-axis dummy10)))
    (s-value revised-win :consider-x-axis-visible-limit (true-p (member :limit-x-axis dummy10)))
    (s-value revised-win :y-axis-visible-max (s-flt dummy1))
    (s-value revised-win :y-axis-visible-min (s-flt dummy2))
    (s-value revised-win :x-axis-visible-max (s-flt dummy3))
    (s-value revised-win :x-axis-visible-min (s-flt dummy4))))

(defun plotting-space-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (gv win :use-fixed-top-gap))
	(dummy2 (or (and (gv win :use-fixed-top-gap) (numberp (gv win :fixed-top-gap)) (gv win :fixed-top-gap))
		    (gv win :label-height)
		    0))
	(dummy3 (gv win :use-fixed-bottom-gap))
	(dummy4 (or (and (gv win :use-fixed-bottom-gap) (numberp (gv win :fixed-bottom-gap)) (gv win :fixed-bottom-gap))
		    (gv win :y-plot-bottom-gap)
		    0))
	(dummy5 (gv win :use-fixed-right-gap))
	(dummy6 (or (and (gv win :use-fixed-right-gap) (numberp (gv win :fixed-right-gap)) (gv win :fixed-right-gap))
		    (gv win :x-plot-right-gap)
		    0))
	(dummy7 (gv win :use-fixed-left-gap))
	(dummy8 (or (and (gv win :use-fixed-left-gap) (numberp (gv win :fixed-left-gap)) (gv win :fixed-left-gap))
		    (gv win :x-plot-left-gap)
		    0))
	dummy9
	(dummy14 (gv win :x-plot-right-gap-extra))
	(dummy16 (or (gv win :y-plot-top-gap-extra) 0))
	(dummy18 (or (gv win :x-plot-left-gap-extra) 0)))

    (choose-variable-values
     '((dummy1 "Use fixed top gap" :boolean)
       (dummy2 "Fixed top gap [pixels]" :integer)
       (dummy3 "Use fixed bottom gap" :boolean)
       (dummy4 "Fixed bottom gap [pixels]" :integer)
       (dummy5 "Use fixed right gap" :boolean)
       (dummy6 "Fixed right gap [pixels]" :integer)
       (dummy7 "Use fixed left gap" :boolean)
       (dummy8 "Fixed left gap [pixels]" :integer)
       (dummy9 "Assign via menu the same fixed gaps to other windows" :boolean)
       ("Extra space parameters ignored if associated fixed gap is used" :comment)
       (dummy14 "Extra space on right side [pixels]" :integer)
       (dummy18 "Extra space on left side [pixels]" :integer)
       (dummy16 "Extra space between top of window and traces [pixels]" :integer))
     :title (format nil "Space Around Plot for ~A" (gv revised-win :title)))

    (loop for win in (cons revised-win (when dummy9 (win-menu
						     (format nil "Choose Plot Windows to Set~%Same Fixed Gaps as ~A" (gv revised-win :title))
						     (loop for win in (windows-of-mode (gv revised-win :mode))
							   unless (eq win revised-win) collect win))))
	  when win do
	  (UPDATE-FIXED-GAP win dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8)
	  (s-value win :x-plot-right-gap-extra dummy14)
	  (s-value win :x-plot-left-gap-extra dummy18)
	  (s-value win :y-plot-top-gap-extra dummy16)
	  unless (eq win revised-win) do (plot-timed-data nil nil nil :win win :revise-plot t))))

(defun standard-plot-menu (win)
  (let* ((dummy1 (gv win :x-axis-min)) (dummy2 (gv win :x-axis-max)) (dummy3 (gv win :y-axis-min)) (dummy4 (gv win :y-axis-max))
	 (dummy5 (gv win :x-inc)) (dummy6 (gv win :y-inc)) (dummy7 (gv win :x-origin)) (dummy8 (gv win :y-origin))
	 dummy9 dummy10 (dummy11 (gv win :connect-data-points)) (dummy12 (gv win :scatter))
	 dummy13 (dummy14 (cond ((and (gv win :connect-data-points) (gv win :scatter)) :Connect_&_show)
				((gv win :connect-data-points) :Connect) ((gv win :scatter) :Show)))
	 dummy15 dummy16 dummy17 dummy18
	 *preserve-plot-layout* *create-new-plot-windows* (revised-win win)
	 (only-win-of-mode-p (= (length (windows-of-mode (gv win :mode))) 1))
	 (only-one-win-of-mode-p-or-is-scanner (or (eq (gv win :mode) :scanner) only-win-of-mode-p)))
    (choose-variable-values
     `(,(unless only-one-win-of-mode-p-or-is-scanner `(dummy18 "Match window dimensions to another plot:" :choose (:Width_&_Height :Width :Height) :rank-margin 4 :TOGGLE-P))
       (:comment ,(format nil "X/Y layout precedence is ~AAutomatic, Explicit" (if only-one-win-of-mode-p-or-is-scanner "" "Match, ")))
       ,(unless only-one-win-of-mode-p-or-is-scanner `(dummy9 "Match layout to another plot:" :choose (:X_&_Y :X :Y) :label-left :rank-margin 4 :TOGGLE-P))
       ,(unless (eq (gv win :mode) :scanner) '(dummy15 "Automatic layout:" :choose (:x_&_y :x :y) :label-left :rank-margin 4 :TOGGLE-P))
       (:comment "Explicit layout parameters")
       ,(unless (eq (gv win :mode) :scanner) `(dummy1 "X minimum" :number)) ,(unless (eq (gv win :mode) :scanner) `(dummy2 "X maximum" :number))
       ,(unless (eq (gv win :mode) :scanner) `(dummy3 "Y minimum" :number)) ,(unless (eq (gv win :mode) :scanner) `(dummy4 "Y maximum" :number))
       (dummy5 "X axis interval" :number) (dummy6 "Y axis interval" :number)
       (dummy7 "X origin (Y intercept on X axis)" :number) (dummy8 "Y origin (X intercept on Y axis)" :number)
       (:comment)
       (dummy14 "Plot technique (vis-a-vis points):" :choose (:Connect :Show :Connect_&_show) :horizontal ; :label-left
	)
       ,(unless (eq (gv win :mode) :scanner) `(dummy10 "Create new window for revisions" :boolean))
       ,(unless only-win-of-mode-p `(dummy16 "Match other plot layouts to this one" :boolean))
       (:comment)
       (dummy13 "More edit and analysis options" :boolean)
       (dummy17 "CANCEL plot edit" :boolean))
     :title "Edit Plot" :text (format nil "Editing plot window ~A" (gv win :title)))
    (unless dummy17
      (when dummy10 (setq revised-win (GET-CHILD-PLOT-WINDOW win)))
      (when revised-win
	(case dummy14
	  (:Connect_&_show (s-value revised-win :connect-data-points t) (s-value revised-win :scatter t))
	  (:Connect (s-value revised-win :connect-data-points t) (s-value revised-win :scatter nil))
	  (:Show (s-value revised-win :connect-data-points nil) (s-value revised-win :scatter t)))
	(case dummy15
	  (:x_&_y (s-value revised-win :auto-x-scaling t) (s-value revised-win :auto-y-scaling t))
	  (:x (s-value revised-win :auto-x-scaling t) (s-value revised-win :auto-y-scaling nil))
	  (:y (s-value revised-win :auto-y-scaling t) (s-value revised-win :auto-x-scaling nil)))
	(when dummy18 (match-win-dimensions-menu (or revised-win win) dummy18))
	;; Unfortunately, PLOT-MATCH-WIN-SCALE-MENU has side effects on the symbols DUMMY1 through DUMMY8, so it must go here.
	(when dummy9 (plot-match-win-scale-menu win revised-win dummy9))
	(s-value revised-win :x-min dummy1) (s-value revised-win :x-max dummy2)
	(s-value revised-win :y-min dummy3) (s-value revised-win :y-max dummy4)
	(s-value revised-win :x-axis-min dummy1) (s-value revised-win :x-axis-max dummy2)
	(s-value revised-win :y-axis-min dummy3) (s-value revised-win :y-axis-max dummy4)
	(s-value revised-win :x-inc dummy5) (s-value revised-win :y-inc dummy6)
	(s-value revised-win :x-origin dummy7) (s-value revised-win :y-origin dummy8)
	(when dummy13 (misc-plot-parameters-menu win revised-win))
	(when (eq (gv revised-win :waterfall) t) (waterfall-plot-menu revised-win))
	(let ((label-traces (gv revised-win :label-traces))
	      (scatter (gv revised-win :scatter)) (SCATTER-SYMBOL (gv revised-win :SCATTER-SYMBOL))
	      (fill-scatter (gv revised-win :fill-scatter)) (scatter-symbol-borderp (gv revised-win :scatter-symbol-borderp))
	      (x-symbol-width (gv revised-win :x-symbol-width)) (y-symbol-width (gv revised-win :y-symbol-width))
	      (connect-data-points (gv revised-win :connect-data-points)) (draw-grid (gv win :draw-grid))
	      (simple-axis-x-value-p (gv revised-win :simple-axis-x-value-p))
	      (simple-axis-y-value-p (gv revised-win :simple-axis-y-value-p))
	      (use-same-line-style (gv revised-win :use-same-line-style))
	      (x-label-v-position (gv revised-win :x-label-v-position))
	      (y-label-h-position (gv revised-win :y-label-h-position))
	      (x-min (gv revised-win :x-min)) (x-max (gv revised-win :x-max))
	      (y-min (gv revised-win :y-min)) (y-max (gv revised-win :y-max))
	      (x-inc (gv revised-win :x-inc)) (y-inc (gv revised-win :y-inc))
	      (x-origin (gv revised-win :x-origin)) (y-origin (gv revised-win :y-origin))
	      (reference-ticks-to-origin (gv revised-win :reference-ticks-to-origin))
	      (x-label (gv win :x-label)) (y-label (gv win :y-label))
	      (x-axis-p (gv revised-win :x-axis-p)) (y-axis-p (gv revised-win :y-axis-p))
	      (x-are-fns (gv revised-win :x-are-fns)) (y-are-fns (gv revised-win :y-are-fns)))
	  (if nil			; (gv win :linear-regression)
	      (plot-xy-data (loop for x-list in (car (gv win :x-lists))
				  for y-list in (car (gv win :y-lists))
				  collect (list x-list y-list))
			    (gv win :label-list)
			    :win revised-win :label-traces label-traces :connect-data-points connect-data-points
			    :scatter scatter :SCATTER-SYMBOL SCATTER-SYMBOL :fill-scatter fill-scatter :scatter-symbol-borderp scatter-symbol-borderp
			    :x-symbol-width x-symbol-width :y-symbol-width y-symbol-width
			    :linear-regression t :draw-grid draw-grid
			    :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
			    :use-same-line-style use-same-line-style
			    :x-label-v-position x-label-v-position :y-label-h-position y-label-h-position
			    :x-min x-min :x-max x-max :y-min y-min :y-max y-max :x-inc x-inc :y-inc y-inc
			    :x-origin x-origin :y-origin y-origin :x-label x-label :y-label y-label :x-are-fns x-are-fns :y-are-fns y-are-fns)
	      (plot-timed-data nil ; (when dummy10 (gv win :y-lists)) (when dummy10 (gv win :label-list))
			       (gv win :label-list)
			       nil
			       :y-lists (gv win :y-lists) :x-lists (gv win :x-lists)
			       :win revised-win :delta-t-start (gv revised-win :delta-t-start)
			       :use-bins (gv revised-win :use-bins) :bin-width (gv revised-win :bin-width)
			       :draw-grid draw-grid :label-traces label-traces :revise-plot (not dummy10) :resurrect (not (eq revised-win win))
			       :x-axis-p x-axis-p :y-axis-p y-axis-p
			       :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
			       :consider-y-axis-visible-limit (gv revised-win :consider-y-axis-visible-limit)
			       :y-axis-visible-max (gv revised-win :y-axis-visible-max) :y-axis-visible-min (gv revised-win :y-axis-visible-min)
			       :consider-x-axis-visible-limit (gv revised-win :consider-x-axis-visible-limit)
			       :x-axis-visible-max (gv revised-win :x-axis-visible-max) :x-axis-visible-min (gv revised-win :x-axis-visible-min)
			       :reference-ticks-to-origin reference-ticks-to-origin
			       :x-log (gv revised-win :x-log) :y-log (gv revised-win :y-log) :log-base (gv revised-win :log-base)
			       :x-trace-offset (gv revised-win :x-trace-offset) :y-trace-offset (gv revised-win :y-trace-offset)
			       :x-data-offset (gv revised-win :x-data-offset) :y-data-offset (gv revised-win :y-data-offset)
			       :connect-data-points connect-data-points :scatter scatter
			       :SCATTER-SYMBOL SCATTER-SYMBOL :fill-scatter fill-scatter :scatter-symbol-borderp scatter-symbol-borderp
			       :x-symbol-width x-symbol-width :y-symbol-width y-symbol-width
			       :x-label-v-position x-label-v-position :y-label-h-position y-label-h-position
			       :x-min x-min :x-max x-max :y-min y-min :y-max y-max :x-inc x-inc :y-inc y-inc
			       :x-origin x-origin :y-origin y-origin :x-label x-label :y-label y-label :x-are-fns x-are-fns :y-are-fns y-are-fns
			       :waterfall (gv revised-win :waterfall) :wf-skirt (gv revised-win :wf-skirt)
			       :stipple-percent (gv win :stipple-percent) :use-same-line-style use-same-line-style
			       :waterfall-trace-label-skip (gv revised-win :waterfall-trace-label-skip)
			       :waterfall-label-offset (gv revised-win :waterfall-label-offset)
			       :label-waterfall (gv revised-win :label-waterfall)))
	  (when dummy16 (MATCH-PLOTS-MENU revised-win))
	  (refresh-axes revised-win)))
      revised-win)))


(defun refresh-all-plots (&optional grid)
  ;; The optional GRID can be :DRAW, :ERASE or nil (don't change).
  (loop for win in (standard-plot-windows) do (refresh-plot win grid)))

(defun refresh-plot (win &optional (grid *refresh-plot-default-grid*))
  (case (gv win :mode)
    (:standard-plot (s-value win :draw-grid (case grid
					      (:draw t)
					      (:erase nil)
					      (t (gv win :draw-grid))))
		    (refresh-axes win)
		    (unless (not (gv win :y-lists))
		      (let ((*automatic-run* t))
			(standard-plot-menu win))))))

(defun match-plots-menu (&optional match-win)
  (let* ((match-win (or match-win
			(choose-list-values-from-keys
			 (mapcar #'(lambda (win) (list (gv win :title) win)) (standard-plot-windows)) nil
			 :punt-if-only-one-entry nil :only-one-choice t :label "Choose plot window for setting coordinates")))
	 (wins-to-be-revised (choose-list-values-from-keys
			      (loop for plot-win in (loop for output-win in *output-windows*
							  when (and (not (eq match-win output-win))
								    (eq (gv output-win :mode) :standard-plot))
							  collect output-win)
				    collect (list (gv plot-win :title) plot-win))
			      nil :punt-if-only-one-entry nil :only-one-choice nil
			      :label (format nil "Choose plot windows for setting~%coordinates to ~a" (gv match-win :title)))))
    (loop for revise-win in wins-to-be-revised do
	  (s-value revise-win :scatter-symbol (gv match-win :scatter-symbol))
	  (s-value revise-win :x-symbol-width (gv match-win :x-symbol-width))
	  (s-value revise-win :y-symbol-width (gv match-win :y-symbol-width))
	  (s-value revise-win :scatter-symbol-units-in-pixels (gv match-win :scatter-symbol-units-in-pixels))
	  (s-value revise-win :fill-scatter (gv match-win :fill-scatter))
	  (plot-timed-data
	   nil nil nil
	   :win revise-win :revise-plot t
	   :draw-grid (gv match-win :draw-grid) :label-traces (gv revise-win :label-traces)
	   :x-label (gv revise-win :x-label) :y-label (gv revise-win :y-label)
	   :x-are-fns (gv match-win :x-are-fns) :y-are-fns (gv match-win :y-are-fns)
	   :x-min (gv match-win :x-min) :x-max (gv match-win :x-max) :x-inc (gv match-win :x-inc) :x-origin (gv match-win :x-origin)
	   :y-min (gv match-win :y-min) :y-max (gv match-win :y-max) :y-inc (gv match-win :y-inc) :y-origin (gv match-win :y-origin)))))

(defun plot-match-win-scale-menu (win revised-win scaled-axes)
  (let ((match-win (choose-list-values-from-keys
		    (loop for plot-win in (loop for output-win in *output-windows*
						when (and (not (or (eq revised-win output-win) (eq win output-win)))
							  (eq (gv output-win :mode) :standard-plot))
						collect output-win)
			  collect (list (gv plot-win :title) plot-win))
		    nil
		    :punt-if-only-one-entry nil :only-one-choice t :label "Matching Plot Scales"
		    :text (format nil "Choose plot window for setting ~a axis(es) scale~%of window ~a"
				  (NICE-STRING-FROM-KEYWORD scaled-axes) (gv win :title)))))
    (when match-win
      (case scaled-axes
	(:X_&_Y (setq dummy1 (gv match-win :x-min) dummy2 (gv match-win :x-max)
		      dummy3 (gv match-win :y-min) dummy4 (gv match-win :y-max)
		      dummy5 (gv match-win :x-inc) dummy6 (gv match-win :y-inc)
		      dummy7 (gv match-win :x-origin) dummy8 (gv match-win :y-origin)))
	(:X (setq dummy1 (gv match-win :x-min) dummy2 (gv match-win :x-max)
		  dummy5 (gv match-win :x-inc) dummy7 (gv match-win :x-origin)))
	(:Y (setq dummy3 (gv match-win :y-min) dummy4 (gv match-win :y-max)
		  dummy8 (gv match-win :y-origin) dummy6 (gv match-win :y-inc)))))))

(defun rescale-wins-to-largest-magnitude (wins axis)
  (let ((axis-max-slot (read-from-string (format nil ":~A-axis-max" axis)))
	(axis-min-slot (read-from-string (format nil ":~A-axis-min" axis)))
	(axis-inc-slot (read-from-string (format nil ":~A-inc" axis))))
    (loop for win in wins
	  maximize (gv win axis-inc-slot) into max-inc
	  maximize (gv win axis-max-slot) into max
	  minimize (gv win axis-min-slot) into min
	  finally
	  (loop for win in wins do
		(s-value win axis-inc-slot max-inc)
		(s-value win axis-max-slot max)
		(s-value win axis-min-slot min)
		(refresh-plot win)))))

(defun rescale-wins-to-largest-x-magnitude (wins)
  "Rescale all standard plot windows WINS so that all have the minimum X axis value to the smallest of all, and the maximum X axis
value to the largest of all."
  (rescale-wins-to-largest-magnitude wins :x))

(defun rescale-wins-to-largest-y-magnitude (wins)
  "Rescale all standard plot windows WINS so that all have the minimum Y axis value to the smallest of all, and the maximum Y axis
value to the largest of all."
  (rescale-wins-to-largest-magnitude wins :y))

(defun set-*plot-axis-font*-menu (&optional default-font)
  (setq *plot-axis-font* (font-menu (or default-font *plot-axis-font*) "Set *PLOT-AXIS-FONT* (default plot axis font)")))

(defun plot-axis-and-comment-default-fonts-menu (&optional reference-window)
  (let (dummy1 dummy2 (dummy3 :no_change) dummy4 dummy5)
    (choose-variable-values
     `((dummy1 ,(format nil "Set *PLOT-AXIS-FONT* from menu~%(currently ~s ~s ~s)"
			(gv *plot-axis-font* :family) (gv *plot-axis-font* :face) (gv *plot-axis-font* :size))
	:boolean)
       (dummy2 "Update axis font of current plot windows to *PLOT-AXIS-FONT*" :boolean)
       (dummy3 ,(format nil "Set *COMMENT-FONT* (currently ~s ~s ~s):"
			(gv *comment-font* :family) (gv *comment-font* :face) (gv *comment-font* :size))
	:choose (:from_menu :to_*plot-axis-font*) :toggle-p)
       (dummy5 "Update comment font of current output windows to *COMMENT-FONT*" :boolean))
     :label (if reference-window (format nil "Set Fonts for ~A and Other Windows" (gv reference-window :title)) "Set Window Fonts"))
    (when dummy1 (set-*plot-axis-font*-menu (and reference-window (gv reference-window :plot-axis-font))))
    (when dummy2 (mapcar #'(lambda (win) (s-value win :plot-axis-font *plot-axis-font*)) (all-plot-windows)))
    (case dummy3
      (:to_*plot-axis-font* (setq *comment-font* *plot-axis-font*))
      (:from_menu (set-*comment-font*-menu (and reference-window (gv reference-window :comment-font)))))
    (when dummy5 (mapcar #'(lambda (win) (s-value win :comment-font *comment-font*)) *output-windows*))
    (when (or dummy2 dummy5) (mapcar #'(lambda (win) (unless (eq win reference-window) (refresh-plot win))) (ALL-PLOT-WINDOWS)))))

;; Backward comp
(defun default-window-font-menu (&optional reference-window) (plot-axis-and-comment-default-fonts-menu reference-window))

(defun misc-plot-parameters-menu (win revised-win)
  (let* ((dummy1 nil) (dummy2 nil) dummy3 dummy4
	 (dummy5 (cond ((eq :auto (gv win :waterfall)) :waterfall_[auto_scale])
		       ((gv win :waterfall) :waterfall)
		       (t :regular)))
	 dummy6 dummy7 dummy8 dummy9 dummy15 dummy16 dummy17
	 (dummy19 (gv win :use-same-line-style))
	 dummy20
	 (dummy21 (gv win :label-traces))
	 dummy22
	 (dummy24 (gv win :plot-line-style-family))
	 dummy25 dummy26 dummy30
	 (menu-list
	  `((dummy15 "Reorder/suppress traces" :boolean)
	    ,(when (> (length (gv win :label-list)) 5) '(dummy16 "Invert order of traces" :boolean))
	    (dummy21 "Include trace labels" :boolean)
	    ,(unless (eq (gv win :mode) :scanner) `(dummy5 "Plot layout:" :choose (:regular :waterfall) :label-left :horizontal))
	    (dummy24 "Plot line style:" :choose ,*plot-line-style-families* :rank-margin 2)
	    (dummy19 ,(format nil "Use same line style for all traces~%(always true for waterfalls)") :boolean)

	    (dummy7 "Write traces to file" :boolean)

	    (:comment)
	    (dummy9 "Ticks, Grid and Axes menu" :boolean)
	    (dummy20 ,(format nil "Log plot menu~A" (or (log-parameters-string win) "")) :boolean)
	    (dummy6 "Window overlay and layout menu" :boolean)
	    (dummy1 "Data and trace offset menu" :boolean)
	    	    (dummy8 "Trace analysis menu" :boolean)
	    (:comment)

	    	    (dummy26 "Edit scatter" :boolean)
	    (dummy2 "Edit labels" :boolean)
	    (dummy22 "Edit fonts:" :choose (:all_windows :this_window) :label-left :toggle-p)



	    )))
    (when nil				; (or t ; (gv win :marked-points) (gv win :plotlines))
      (push '(dummy4 "Edit marked points font" :boolean) menu-list))
    (choose-variable-values menu-list :title "Edit Plot" :text (gv revised-win :title))
    (when dummy7 (grab-and-store-plot-data
		  :filename (STRIP-DISPLAYED-HOST-NAME-FROM-TITLE (gv win :title))
		  :win win :force-menu t))
    (when dummy8 (ADD-TRACE-ANALYSIS-TO-PLOT-MENU win))
    (s-value revised-win :waterfall (case dummy5
				      (:waterfall t)
				      (:waterfall_[auto_scale] :auto)
				      (:regular nil)))
    (when dummy6 (overlay-layout-menu win))
    (s-value revised-win :use-same-line-style dummy19)
    (s-value revised-win :label-traces dummy21)
    (s-value revised-win :plot-line-style-family dummy24)
    (case dummy22
      (:this_window (s-value revised-win :comment-font
			     (s-value revised-win :font
				      (s-value revised-win :plot-axis-font
					       (font-menu (gv win :plot-axis-font)
							  (format nil "Plot axis font for ~A" (gv revised-win :title)))))))
      (:all_windows (plot-axis-and-comment-default-fonts-menu revised-win)))
    (cond-every
     (dummy25 (edit-tick-format win revised-win))
     (dummy16 (reorder-plot-trace-menu win t))
     (dummy15 (reorder-plot-trace-menu revised-win))
     (dummy2 (edit-labels revised-win))
     (dummy9 (axes-menu win revised-win))
     (dummy4 (Edit-marked-points-font revised-win))
     (dummy1 (data-and-trace-offset-plot-menu revised-win))
     (dummy26 (scatter-menu win revised-win))
     ((and (eq (gv revised-win :axes-type) :simple) (not (gv revised-win :waterfall))) (SIMPLE-AXES-MENU revised-win))
     (dummy17 (plot-scale-to-trace-menu revised-win))
     (dummy20 (plot-log-parameters-menu win revised-win)))))

(defun plot-scale-to-trace-menu (revised-win)
  (let* ((label-list (gv revised-win :label-list))
	 (dummy1 (car label-list)))
    (choose-variable-values `((dummy1 "Choose trace:" :choose ,(loop for label in label-list collecting label)))
			    :title (format nil "Choose trace for scaling ~A" (gv revised-win :title)))
    (let ((scaling-data (loop for label in label-list
			      for data in (car (gv revised-win :y-lists))
			      when (string= label dummy1) do (return data))))
      (s-value revised-win :y-max (a-bit-more (list scaling-data) 0.05))
      (s-value revised-win :y-min (a-bit-less (list scaling-data) 0.05)))))

(defun log-parameters-string (win)
  (when (or (gv win :x-log) (gv win :y-log))
    (concatenate-strings
     (format nil " (")
     (when (gv win :x-log) (format nil "Log X"))
     (when (and (gv win :x-log) (gv win :y-log)) (format nil " "))
     (when (gv win :y-log) (format nil "Log Y"))
     (format nil ", Base: ~A)" (or (gv win :log-base) "Natural base")))))

(defun plot-log-parameters-menu (win revised-win)
  (let* ((dummy3 (gv win :x-log)) (dummy4 (gv win :y-log))
	 (dummy1 (gv win :log-base)) (dummy2 (not dummy1))
	 (dummy5 (gv win :auto-x-scaling))
	 (dummy6 (gv win :auto-y-scaling))
	 menu-list)
    (choose-variable-values
     `((dummy3 "Log plot for X" :boolean)
       (dummy5 "Auto adjust X layout" :boolean)
       (dummy4 "Log plot for Y" :boolean)
       (dummy6 "Auto adjust Y layout" :boolean)
       (dummy2 "Use natural base" :boolean))
     :title (format nil "Log Plot Parameters for ~A" (gv revised-win :title)))
    (if dummy2
	(s-value revised-win :log-base nil)
	(when (or dummy3 dummy4)
	  (unless (numberp dummy1) (setq dummy1 10.0))
	  (push '(dummy1 "Log base (bad values default to 10)" :float) menu-list)
	  (choose-variable-values menu-list :title (format nil "Log Plot Parameters for ~A" (gv revised-win :title)))
	  (when (or (<= dummy1 0.0) (< (- 1.0 0.01) dummy1 (+ 1.0 0.01)))
	    (setq dummy1 10.0))
	  (s-value revised-win :log-base dummy1)))
    (s-value win :auto-x-scaling dummy5)
    (s-value win :auto-y-scaling dummy6)
    (s-value revised-win :x-log dummy3)
    (s-value revised-win :y-log dummy4)))

(defun reorder-plot-trace-menu (win &optional just-invert)
  (let* ((label-list (gv win :label-list))
	 (all-values (sequence-to-string-list (list-of-nums (length label-list) 1 1)))
	 (trace-order (or (gv win :trace-order) (list-of-ints (length label-list))))
	 (included-list (loop for order in trace-order collect (nth (round order) label-list)))
	 (excluded-list (loop for label in label-list unless (member label included-list :test 'equal) collect label))
	 (menu-list (loop for i from 1
			  for key in (concatenate 'list included-list excluded-list)
			  collect (list key (if (> i (length included-list)) "" (princ-to-string i)))))
	 (new-trace-order (if just-invert
			      (reverse trace-order)
			      (let* ((menu-trace-order-list
				      (XCLUSIVE-CHOOSE-BUTTON menu-list all-values t
							      :rank-margin 6 :label (format nil "Trace Order for ~a" (gv win :title))
							      :text "Unassigned traces will not be displayed"))
				     (sorted-menu-trace-order-list (sort (loop for string-val in menu-trace-order-list
									       when (and (cadr string-val) (> (length (cadr string-val)) 0))
									       collect (list (car string-val) (read-from-string (cadr string-val))))
									 '< :key 'cadr)))
				(loop for label-val in sorted-menu-trace-order-list
				      collect (search (list (car label-val)) label-list :test 'equal))))))
    (s-value win :trace-order (fix-list new-trace-order))))

(defparameter GENERIC-PLOT-WINDOW-SLOTS
  ;; These keywords should correspond to useful slots of plot windows, that will be saved by WRITE-WINDOW-PLOT-FORM. Eventually
  ;; these should be used as key args to PLOT-TIMED-DATA and friends.
  '(:x-tick-decimal :y-tick-decimal
    :x-axis-tick-mark-length :y-axis-tick-mark-length
    :scatter-symbol-line-style
    :use-waterfall-y-data-max :waterfall-y-data-max :waterfall-base-x-offset :waterfall-base-y-offset
    :gap-between-trace-and-waterfall-label :skirt-to-window-border))

(defparameter generic-plot-keywords
  '(:TITLE
    :left :top :WIDTH :HEIGHT
    :delta-t :delta-t-start :timed-data
					; :scale
    :data-type :canonic-label
    :accomodate-overlays
    :X-LABEL :Y-LABEL :X-LABEL-V-POSITION :x-label-h-position
    :invert-y-axis-label :y-label-v-position :y-label-h-position

    :X-ARE-FNS :Y-ARE-FNS :x-axis-coeff :y-axis-coeff
    :x-axis-prefix :x-axis-suffix :y-axis-prefix :y-axis-suffix

    :X-MIN :X-MAX :X-INC :X-ORIGIN :Y-MIN :Y-MAX :Y-INC :Y-ORIGIN :Y-LOG :X-LOG :LOG-BASE :x-axis-root :y-axis-root

    :AXES-TYPE :X-AXIS-P :Y-AXIS-P
    :X-SCALE-L% :X-SCALE-T% :Y-SCALE-T% :SIMPLE-AXIS-X-VALUE-P :SIMPLE-AXIS-Y-VALUE-P
    :consider-y-axis-visible-limit :y-axis-visible-max :y-axis-visible-min
    :consider-x-axis-visible-limit :x-axis-visible-max :x-axis-visible-min

    :linear-regression

    :x-origin-tick :y-origin-tick :reference-ticks-to-origin :include-x-tick-at-0 :include-y-tick-at-0

    :USE-BINS :BIN-WIDTH :STIPPLE-PERCENT :USE-SAME-LINE-STYLE
    :upper-right-comment :comment :comment-position

    :USE-FIXED-TOP-GAP :FIXED-TOP-GAP :USE-FIXED-BOTTOM-GAP :FIXED-BOTTOM-GAP :USE-FIXED-RIGHT-GAP :FIXED-RIGHT-GAP :USE-FIXED-LEFT-GAP :FIXED-LEFT-GAP

    :DRAW-GRID				; :grid-line-style
    :LABEL-TRACES
    :X-DATA-OFFSET :Y-DATA-OFFSET :X-TRACE-OFFSET :Y-TRACE-OFFSET
    :fix-to-unity-mag-if-so :session-name

    :CONNECT-DATA-POINTS :SCATTER :SCATTER-SYMBOL :SCATTER-WIDTH-HEIGHTS :scatter-symbol-borderp :fill-scatter :x-symbol-width :y-symbol-width
    :polar :POLAR-CIRCLES-P
    :LABEL-WATERFALL :WATERFALL :WF-SKIRT :WATERFALL-TRACE-LABEL-SKIP :WATERFALL-LABEL-OFFSET))

(defun fixup-formatted-object (obj)
  (typecase obj
    (cons (format nil "`~s" obj))
    (t    obj)))

(defun print-window-key-and-value (win key &optional (stream t))
  (let ((obj (gv win key)))
    (typecase obj
      (cons (format stream "~% ~s `~s" key obj))
      (t    (format stream "~% ~s ~s" key obj)))))

(defun print-s-value-form-window-key-and-value (win key &optional (stream t))
  (let ((obj (gv win key)))
    (typecase obj
      (cons (format stream "  (s-value win ~s `~s)~%" key obj))
      (t    (format stream "  (s-value win ~s ~s)~%" key obj)))))

#|
(defun write-window-plot-form (win &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (format t ";;; -*- Package: ~a; Mode: LISP -*-~%~%~%~%~%" *default-lisp-data-package-name*)
    (format t "(let ((win ~%  ")
    (format t "(plot-timed-data~%  ")
    (formatted-list-dump (gv win :y-lists))    (format t "~%  ")
    (formatted-list-dump (gv win :label-list))
    (format t "~% NIL ~%  :x-lists~%  ")
    (formatted-list-dump (gv win :x-lists))    (format t "~%")
    (format t "	:update-fixed-gap t ~%")
    (loop for key in GENERIC-PLOT-KEYWORDS do (print-window-key-and-value win key))
    (format t " )))~%~% ")

    (loop for key in GENERIC-PLOT-WINDOW-SLOTS do (print-s-value-form-window-key-and-value win key))

    (WRITE-WINDOW-DRESSING win)
    (format t " (let ((*automatic-run* t)) (standard-plot-menu win))~%")
    (format t " nil)~% ")))
|#

(defun write-window-plot-form (win &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (format t ";;; -*- Package: ~a; Mode: LISP -*-~%~%~%~%~%" *default-lisp-data-package-name*)
    (format t "(let ((win ~%  ")
    (format t "(plot-timed-data~%  ")
    (if *write-window-plot-form-w-explicit-x-and-y-lists*
	(format t "  nil    ; Refer to :y-lists arg below")
	(formatted-list-dump (gv win :y-lists)))
    (format t "~%  ")
    (formatted-list-dump (gv win :label-list))
    (format t "~% NIL ~%")
    (when *write-window-plot-form-w-explicit-x-and-y-lists*
      (format t "  :y-lists~%  ")
      (formatted-list-dump (gv win :y-lists))    (format t "~%"))
    (format t "  :x-lists~%  ")
    (formatted-list-dump (gv win :x-lists))
    (format t "~%")
    (format t "	:update-fixed-gap t ~%")
    (loop for key in GENERIC-PLOT-KEYWORDS do (print-window-key-and-value win key))
    (format t " )))~%~% ")
    (WRITE-WINDOW-DRESSING win)
    (format t " (refresh-plot win)~%")
    (format t " nil)~% ")))

(defun write-window-dressing (win)
  (loop for key in GENERIC-PLOT-WINDOW-SLOTS do (print-s-value-form-window-key-and-value win key))
  (loop for font-slot in '(:window-font :comment-font :plot-axis-font :marked-points-font) do (format-window-font-slot-description win font-slot))
  (write-window-markers win)
  (when (gv win :grid-line-style)
    (format t "  (s-value win :grid-line-style ~A)~%" (read-from-string (opal::name-for-schema (gv win :grid-line-style)))))
  (loop for plotline in (gv win :plotlines) do
	(format t "  (mark-plotline win ~S ~S ~S ~S :line-style ~a)~%"
		(gv plotline :x-start) (gv plotline :x-stop) (gv plotline :y-start) (gv plotline :y-stop)
		(OUTPUT-PARSE-LINE-STYLE (gv plotline :line :line-style) nil))))

(defun format-window-font-slot-description (win slot)
  (when (gv win slot)
    (format t "  (s-value win ~s (opal:get-standard-font ~S ~S ~S))~%" slot (gv win slot :family) (gv win slot :face) (gv win slot :size))))

(defun write-window-markers (win)
  (when (gv win :markers) (format t ";; Markers ~%"))
  (loop for marker in (gv win :markers) do
	(let ((marker-label (marker-label marker)))
	  (format t "  (add-marker win `~S :add-point ~S :add-cross-hair ~S ~%"
		  (marker-points marker) (true-p (marker-point marker)) (true-p (marker-cross marker)))
	  (when (true-p (marker-point marker))
	    (format t "   :point-type ~s :point-width ~a :point-height ~a~%"
		    (marker-point-type marker)
		    (marker-point-width marker)
		    (marker-point-height marker)))
	  (format t "    :data-x ~S :data-y ~S :label ~S :label-position ~S)~%"
		  (marker-data-x marker) (marker-data-y marker)
		  (when marker-label  (gv marker-label :label :text))
		  (when marker-label (gv marker-label :label-position))))))

(defun output-parse-color (color &optional stream)
  (when color
    (format stream " (create-instance nil opal::color (:red ~a) (:blue ~a) (:green ~a))"
	    (gv color :red)
	    (gv color :blue)
	    (gv color :green))))

(defun output-parse-line-style (line-style &optional stream)
  (when line-style
    (format stream "~a"
	    (if (member line-style *all-line-styles*)
		(read-from-string (opal::name-for-schema line-style))
		(concatenate 'string
			     "(create-instance nil opal:line-style "
			     (format nil "(:join-style ~s) (:line-thickness ~A) "
				     (gv line-style :join-style)
				     (gv line-style :line-thickness))
			     (format nil "(:shading ~S) (:line-style ~S)"
				     (gv line-style :shading)
				     (gv line-style :line-style))
			     (format nil " (:dash-pattern ~S) (:color ~a) (:foreground-color ~a))"
				     (fixup-formatted-object (gv line-style :dash-pattern))
				     (gv line-style :color)
				     (output-parse-color (gv line-style :foreground-color))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plot interactors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-plot-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (unless (gv window :data-erased)
     (case (gv window :mode)
					;       (:scanner (scanned-image-menu window))
					;       (:histogram (histogram-menu window))
					;       (:2dplot (2dplot-menu window))
					;       (:3dplot (3dplot-menu window))
       (:standard-plot (refresh-plot window))))))

(defun plot-window-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (unless (gv window :data-erased)
     (case (gv window :mode)
       (:scanner (scanned-image-menu window))
       (:histogram (histogram-menu window))
       (:2dplot (2dplot-menu window))
       (:3dplot (3dplot-menu window))
       (:standard-plot (standard-plot-menu window))))))

(create-instance 'menu-for-grid-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;;		 (:start-event :control-\g)
		 (:start-event '(:control-\g :CONTROL-LATIN_SMALL_LETTER_G))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor
								 (progn (edit-data-grid window)
									(refresh-axes window)
									(opal:update window))))))

(create-instance 'mark-plotline-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_SMALL_LETTER_G instead of :control-\g
		 ;; (:start-event :control-\b)
		 (:start-event '(:CONTROL-LATIN_SMALL_LETTER_B :control-\b))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor (mark-plotline-menu window)))))

(create-instance 'edit-plotlines-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_SMALL_LETTER_G instead of :control-\g
		 ;; 		 (:start-event :control-B)
		 (:start-event '(:CONTROL-LATIN_CAPITAL_LETTER_B :control-B))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor (edit-plotlines window)))))

(defun erase-plot-data-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (if (gv window :DATA-ERASED)
       (NOTIFICATION (format nil "Data already erased from plot ~A!" (gv window :title)))
       (when (go-ahead-menu (format nil "Erase all data from plot ~A?" (gv window :title)))
	 (erase-plot-data window)))))

(create-instance 'erase-plot-data-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\E)
		 (:start-event '(:control-e :CONTROL-LATIN_CAPITAL_LETTER_E))
		 (:final-function #'erase-plot-data-inter-function))

(defun restore-just-recent-traces (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (if (and nil (not (eq :2dplot (gv interactor :window :mode)))
	   (> (length (gv interactor :window :y-lists)) 1))
      (unless (gv interactor :window :data-erased)
	(let ((win (gv interactor :window)) *automatic-run* *create-new-plot-windows*)
					; (remove-all-markers win)
	  (add-temp-comment win "Restoring...")
	  (s-value win :y-lists (list (car (gv win :y-lists))))
	  (s-value win :x-lists (list (car (gv win :x-lists))))
	  (plot-timed-data
	   nil nil nil
	   :win (gv interactor :window)
	   :draw-grid (gv (gv interactor :window) :draw-grid)
	   :overlay t
	   :upper-right-comment "Restoring..")
	  (add-temp-comment win "")))))

(create-instance 'restore-just-recent-traces-window-Interactor inter:button-Interactor
		 (:continuous nil)
		 (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-R)
		 (:start-event '(:control-R :CONTROL-LATIN_CAPITAL_LETTER_R))
		 (:final-function #'restore-just-recent-traces))

(defun grab-and-store-plot-data (&key filename win (TRACE :all) overlay-index
				 force-menu (output-format nil output-format-supplied-p) suppress-comments include-which-comments (if-file-exists :supersede))
  "Writes XY plot data from WIN into FILENAME. Prompts for non-specfied args. Arguments and behaviour follow STORE-XY-DATA. TRACE and OVERLAY-INDEX
keywords determine which traces are retrieved, following EXTRACT-PLOT-WINDOW-DATA. If FILENAME does not include a directory path, then this is taken from
*DATA-DIRECTORY*."
  (let ((win (or win (win-menu "Select Plot Window for Data" (standard-plot-windows) nil t)))
	(trace (if force-menu :all trace))) 	; Backward compatibility
    (when win
      (let* ((number-of-overlays (number-of-overlays win))
	     (overlay-index (or overlay-index
				(if (= number-of-overlays 1)
				    0
				    (let ((dummy1 0))
				      (choose-variable-values
				       '((dummy1 "Extract overlay index:" :integer))
				       :text (format nil "Choose overlay index from 0 to ~d" (1- number-of-overlays)) :title (format nil "Choose overlay for ~a" (gv win :title)))
				      dummy1))))
	     (filename (make-nice-filename (or filename (strip-displayed-host-name-from-title (gv win :title)))))
	     (directory-namestring-no-colons (directory-namestring-no-colons filename))
	     (dummy1 (format nil "~A~A" (if (> (length directory-namestring-no-colons) 0) directory-namestring-no-colons *data-directory*) (remove-dirs-from-path filename)))
	     dummy2
	     (dummy4 suppress-comments))
	(when include-which-comments  (setq *grab-and-store-plot-data-include-comments* include-which-comments))
	(when output-format-supplied-p
	  (setq *grab-and-store-plot-data-output-format* output-format))
	(unless *grab-and-store-plot-data-output-format* (setq *grab-and-store-plot-data-output-format* :lisp))
	(multiple-value-bind (data labels)
	    (extract-plot-window-data win trace overlay-index)
	  (when (and data labels)
	    (when (or force-menu (not (full-pathname-p dummy1)))
	      (choose-variable-values
	       `((dummy1 ,(format nil "Edit filename (for COLUMNS format, this is base name.~%For LISP format, add .lisp extension if desired)") :string)
		 (*grab-and-store-plot-data-output-format* "Output format:" :choose (:lisp :columns))
		 (*grab-and-store-plot-data-include-comments* "Include comments:" :x-choose (:window_comments :window_title))
		 (dummy2 "CANCEL" :boolean))
	       :title (format nil "Write Data From ~A" (gv win :title))))
	    (unless dummy2
	      (store-XY-data data dummy1 *grab-and-store-plot-data-output-format* :labels labels :suppress-comments (not *grab-and-store-plot-data-include-comments*)
			     :if-file-exists if-file-exists
			     :x-units (gv win :x-label) :y-units (gv win :y-label)
			     :extra-comment
			     (concatenate-strings
			      (if (member :window_title *grab-and-store-plot-data-include-comments*) (format nil "~%; **** Data from plot window ~A  ****~%" (gv win :title)) "")
			      (if (member :window_comments *grab-and-store-plot-data-include-comments*)
				  (concatenate-strings (loop for pos in *comment-positions*
							     collect (get-window-comment-text win :position pos)
							     collect (format nil "~%")))))))))))))

(defun grab-and-store-plot-data-interactor-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (gv interactor :window)) *automatic-run*)
    (unless (or (not (eq :standard-plot (gv win :mode))) (gv win :data-erased))
      (grab-and-store-plot-data :win win :force-menu t))))

(create-instance 'grab-and-store-plot-data-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-T)
		 (:start-event '(:control-T :CONTROL-LATIN_CAPITAL_LETTER_T))
		 (:final-function #'grab-and-store-plot-data-interactor-function))

(defun save-graphics-to-lisp-file (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (unless (or (not (eq :standard-plot (gv interactor :window :mode)))
	      (gv interactor :window :data-erased))
    (let ((win (gv interactor :window)) *automatic-run*)
      (dump-plot-to-lisp-file win))))

(create-instance 'save-graphics-to-lisp-file-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL-G instead of :CONTROL-G
		 ;; (:start-event :control-\s)
		 (:start-event '(:control-\s :CONTROL-LATIN_SMALL_LETTER_S))
		 (:final-function #'save-graphics-to-lisp-file))

(create-instance 'toggle-grid-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_SMALL_LETTER_G instead of :control-\g
		 ;; (:start-event :control-\g)
		 (:start-event '(:CONTROL-LATIN_SMALL_LETTER_G :control-\g))
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (let ((win (gv interactor :window)))
					(s-value win :draw-grid (not (gv win :draw-grid)))
					(refresh-axes win)
					(opal:update win)))))

(defun edit-data-grid (win &optional revised-win)
  (let ((dummy1 (gv win :draw-grid))
	dummy2
	(target-win (or revised-win win)))
    (choose-variable-values
     '((dummy1 "Draw grid" :boolean)
       (dummy2 "Edit grid line style:" :choose (:dashed-and-solid-grey :solid-color :dashed-color :dashed-and-solid-black :from_components)))
     :title (format nil "Edit Data Grid For ~A" (gv target-win :title)))
    (s-value target-win :draw-grid dummy1)
    (s-value target-win :grid-line-style
	     (or (and dummy2
		      (line-style-menu :default-style (or (gv win :grid-line-style) *default-grid-line-style*)
				       :text (format nil "Line Style For Data Grid in ~A" (gv target-win :title))
				       :choose-from-components (true-p (member dummy2 '(:from_components)))
				       :choose-from-classes (true-p (member dummy2 '(:dashed-and-solid-grey :solid-color :dashed-color :dashed-and-solid-black)))
				       :style-options (case dummy2
							(:from_components nil)
							(:dashed-and-solid-black 'dashed)
							(:dashed-and-solid-grey 'grey-dashed)
							(:solid-color 'thin-color)
							(:dashed-color 'thin-dashed-1-color))))
		 (gv target-win :grid-line-style)))))

(defun add-plotting-interactors (win)
  ;; This adds a bunch of interactors for windows used for data plotting.
  (case (gv win :mode)
    (:standard-plot
     (add-window-zoom win #'zoom :control-leftdown)
     (add-window-zoom win #'zoom-to-new-window :shift-control-leftdown)
     (add-window-zoom win #'unzoom :rightdown nil)
     (create-instance nil save-graphics-to-lisp-file-Interactor (:Window win))
     (create-instance nil grab-and-store-plot-data-Interactor (:Window win))
     (create-instance nil restore-just-recent-traces-window-Interactor (:Window win))
     (add-window-zoom win #'restore-plot :control-rightdown nil)))
  (case (gv win :mode)
    ((or :standard-plot :scanner :histogram)
     (create-instance nil toggle-grid-window-Interactor (:Window win))
     (create-instance nil menu-for-grid-window-Interactor (:Window win))))
  (case (gv win :mode)
    (:scanner (add-scanner-interactors win)))
  (create-instance nil erase-plot-data-Interactor (:window win))
  (create-instance nil mark-plotline-interactor (:window win))
  (create-instance nil edit-plotlines-interactor (:window win))
  (create-instance nil window-menu-Interactor (:Window win) (:final-function #'plot-window-menu-inter-function))
  (create-instance nil refresh-window-Interactor (:Window win) (:final-function #'refresh-plot-inter-function))
  (add-window-coords-pointer win #'plot-coords-pointer-result #'mark-plot-coords-pointer-result #'plot-coords-running-function)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-plot-windows () (clear-windows-of-mode :standard-plot))

(defun transform-polyline (polyline xfrm-point-list zoom-p) ; This should work on multipoint objects as well.
  ;; When ZOOM-P then XFRM-POINT-LIST is new coords, else XFRM-POINT-LIST is last coords.
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type (unsigned-byte 29)))
  (let* ((new-left (the fn (first xfrm-point-list)))
	 (new-top (the fn (second xfrm-point-list)))
	 (new-width (the fn (third xfrm-point-list)))
	 (new-height (the fn (fourth xfrm-point-list)))
	 (x-coeff (float (/ (the fn (gv polyline :parent :window :width)) new-width)))
	 (y-coeff (float (/ (the fn (gv polyline :parent :window :height)) new-height))))
    (s-value polyline :point-list
	     (loop for point in (gv polyline :point-list)
		   for count from 0
		   if (evenp count) collect (truncate (the sf (if zoom-p
								  (* (- (the fn point) new-left) x-coeff)
								  (+ new-left (* (the fn point) x-coeff)))))
		   else collect (truncate (the sf (if zoom-p
						      (* (- (the fn point) new-top) y-coeff)
						      (+ new-top (* (the fn point) y-coeff)))))))))

(defun scatter-symbol-height-width (plot-win scatter-symbol curve-num)
  ;; POINTS are in pixel units - (x y x y ...) - not rounded!
  (let* ((scatter-symbol (or scatter-symbol (gv plot-win :scatter-symbol)))
	 (scatter-width-heights (gv plot-win :scatter-width-heights))
	 (y-symbol-width (or (cadr (nth curve-num scatter-width-heights))
			     (gv plot-win :y-symbol-width)
			     *default-scatter-size*
			     (* 0.02 (the sf (gv plot-win :y-mag)))))
	 (x-symbol-width (case scatter-symbol
			   (:dot y-symbol-width)
			   (t (or (car (nth curve-num scatter-width-heights))
				  (gv plot-win :x-symbol-width)
				  *default-scatter-size*
				  (* 0.02 (the sf (gv plot-win :x-mag)))))))
	 ;; Following params are in pixels
	 (width (if (gv plot-win :scatter-symbol-units-in-pixels)
		    x-symbol-width
		    (x-plot-win-distance x-symbol-width plot-win)))
	 (height (if (gv plot-win :scatter-symbol-units-in-pixels)
		     y-symbol-width
		     (y-plot-win-distance y-symbol-width plot-win))))
    (values height width)))

(defun get-scatter-line-style (plot-win curve-num &optional (line-style thin-black-line))
  ;; CURVE-NUM ranges from 0 to N-1 for N curves
  (or
   (let ((temp-value (typecase (gv plot-win :scatter-symbol-line-style)
		       (cons (nth curve-num (gv plot-win :scatter-symbol-line-style)))
		       (t (gv plot-win :scatter-symbol-line-style)))))
     (typecase temp-value
       (string (symbol-value (read-from-string temp-value)))
       (symbol (symbol-value temp-value))
       (t temp-value)))
   (create-plot-line-style thin-black-line plot-win (gv line-style :foreground-color))))

(defun get-scatter-symbol-borderp (plot-win curve-num)
  (typecase (gv plot-win :scatter-symbol-borderp)
    (cons (nth curve-num (gv plot-win :scatter-symbol-borderp)))
    (t (gv plot-win :scatter-symbol-borderp))))

(defun add-scatter-points-to-plot (plot-agg plot-win points line-style scatter-symbol what-is-it curve-num)
  ;; LINE-STYLE applies to the line (whether shown or not) that connects the data points, not the line style for the scatter symbol.
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type KR::SCHEMA plot-win plot-agg)
	   ; (cons points)
	   )
  (let* ((color-fill (when (or (nth curve-num (gv plot-win :scatter-symbol-fill)) (gv plot-win :fill-scatter))
		       (color-to-fill (gv line-style :foreground-color))))
	 (scatter-symbol (or scatter-symbol (gv plot-win :scatter-symbol)))
	 (scatter-line-style (when (or (member scatter-symbol *SCATTER-SYMBOLS-OPEN-CURVES*) (get-scatter-symbol-borderp plot-win curve-num))
			       (get-scatter-line-style plot-win curve-num line-style))))
    (multiple-value-bind (height width)
	(scatter-symbol-height-width plot-win scatter-symbol curve-num)
      (let* ((floor-half-width (the fn (floor (/ width 2)))) ; This and the following params are in pixels
	     (ceiling-half-width (the fn (ceiling (/ width 2))))
	     (floor-half-height (the fn (floor (/ height 2))))
	     (ceiling-half-height (the fn (ceiling (/ height 2))))
	     (item-array
	      (add-scatter-points-to-plot-make-item-array
	       points floor-half-height floor-half-width ceiling-half-height ceiling-half-width scatter-line-style color-fill))
	     (v-agg (make-virtual-scatter-aggregate plot-win item-array scatter-symbol what-is-it)))
	(virtual-agg-finishing v-agg plot-agg :front)))))

(defun add-scatter-points-to-plot-make-item-array (points floor-half-height floor-half-width ceiling-half-height ceiling-half-width scatter-line-style color-fill)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let (output-list)
    (do ((points points (cddr points)))
	((null points))
      (declare (fixnum floor-half-height floor-half-width ceiling-half-height ceiling-half-width))
      (let ((x-win (car points))
	    (y-win (cadr points)))
	(declare (fixnum x-win y-win))
	(push
	 (make-virtual-GOB-item-values-array ; x-center y-center x-left x-right y-top y-bottom line-style filling-style
	  x-win y-win
	  (the fn (- x-win ceiling-half-width))  (the fn (+ x-win floor-half-width))
	  (the fn (- y-win ceiling-half-height)) (the fn (+ y-win floor-half-height))
	  scatter-line-style color-fill)
	 output-list)))
    (sequence-to-array output-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual aggs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun frob-win-for-virtual-agg (win)
  ;; This needs to happen before adding the virtual agg. ??
  (when *enable-frob-win-for-virtual-agg*
    (s-value win :visible t)
    (opal:update win)))

(defun virtual-agg-finishing (v-agg parent-agg &optional (where :front))
  (FROB-win-FOR-VIRTUAL-agg (gv parent-agg :window))
  ;; To make the width/height calculation simpler - not clear that this helps
  (gv v-agg :width) (gv v-agg :height)
  (gv v-agg :top) (gv v-agg :left)
  (s-value parent-agg :virtual-aggregate v-agg)	; A direct reference to the virtual agg, assuming that there is only one per parent agg.
  (opal:add-component parent-agg v-agg :where where))

(defun make-v-agg (prototype item-sequence what-is-it &optional (point-in-item-function `#(lambda (virtual-aggregate item-values x y)
											   (declare (ignore virtual-aggregate item-values x y))
											   t)))
  (let ((v-agg (create-instance nil opal:virtual-aggregate
				(:item-prototype prototype)
				(:item-array (SEQUENCE-TO-array-generic item-sequence))
				(:point-in-item point-in-item-function)
				(:what-is-it what-is-it))))
    (opal::recalculate-virtual-aggregate-bboxes v-agg)
    v-agg))


(defun make-virtual-scatter-aggregate (plot-win item-sequence scatter-symbol what-is-it)
  (make-v-agg (scatter-symbol-to-prototype scatter-symbol) item-sequence what-is-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(get-plot-window find-plot-window
	  plot-line-style-menu
	  generic-plot-keywords
	  GENERIC-PLOT-WINDOW-SLOTS
	  make-v-agg
	  virtual-agg-finishing
	  frob-win-for-virtual-agg
	  ERASE-ALL-PLOT-DATA
	  mark-all-plot-wins-at-time
	  plot-window-top-x-lists plot-window-top-y-lists
	  standard-plot-windows
	  ALL-PLOT-WINDOWS standard-plot-menu
	  rescale-wins-to-largest-x-magnitude rescale-wins-to-largest-y-magnitude
	  2d-array-max 2d-array-min
	  clear-plot-windows
	  plot 2dplot
	  3dplot-menu
	  output-parse-line-style
	  output-parse-color
	  mark-plotline mark-baseline remove-plotlines
	  MARK-PLOTLINES
	  MARK-PLOT-DIAGONAL
	  MARK-PLOT-EVEN-QUADRANT-DIAGONAL
	  MARK-PLOT-odd-QUADRANT-DIAGONAL
	  frame-plot
	  mark-plot-origin-axises
	  DEFAULT-WINDOW-FONT-MENU
	  plot-axis-and-comment-default-fonts-menu
	  SET-*PLOT-AXIS-FONT*-MENU
	  restore-plot
	  remove-virtual-aggs
	  REFRESH-PLOT REFRESH-ALL-PLOTS
	  UPDATE-FIXED-GAP
	  plot-window-font
	  window-plot-axis-font
	  window-comment-font
	  DENSITY-PLOT-SCALE RETRIEVE-SCATTER-V-AGGS set-virtual-gob-width-and-height EDIT-INDIVIDUAL-SCATTER-DIMENSIONS
	  ))

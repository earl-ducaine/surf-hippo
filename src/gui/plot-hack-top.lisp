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

;; GUI Source file: plot-hack-top.lisp

;;; This contains the more user-oriented plotting routines.

(in-package "SON-OF-PLOT-HACK")

(defun test-ptd ()
  (loop for time from 0 to (* 2 pi) by 0.1
	collect (sin time) into y
	collect (/ time (* 2 pi)) into x
	finally
	(plot-timed-data y 'sinwave x
			 :draw-grid t
			 :y-origin -1
			 :y-label 'Y
			 :x-label 'X
			 :x-min 0 :x-max 1
			 :y-inc 0.5
			 :y-max 1 :y-min -1)))

(defun test-plot () (test-ptd))

(defun test-histo ()
  (plot-histogram-data (list-of-nums 1000) 0 1e3)
  (plot-histogram (list-of-nums 1000) ))

(export '(test-plot test-histo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-global-plot-comment (win &key (update t)) (add-comment win *global-plot-comment* :position *global-plot-comment-position* :update update))

(defun add-local-and-global-comment (comment comment-position win &key (update t))
  (if (and comment (eq (or *global-plot-comment-position* *default-comment-position*) comment-position))
    (let ((*global-plot-comment* (if (zerop (length *global-plot-comment*)) comment (concatenate 'string *global-plot-comment* (format nil "~%") comment))))
      (add-global-plot-comment win))
    (progn (when comment (add-comment win comment :position comment-position :update update))
	   (add-global-plot-comment win :update update))))

;; This needs to be completed ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MAJOR PLOTTING FUNCTIONS
;;
;; PLOT-XY-DATA, PLOT-TIMED-DATA, PLOT-POINTS, PLOT-SCATTER, PLOT-HISTOGRAM,
;; PLOT-POLAR-DATA, PLOT-POLAR-SIMPLE-ARRAY, PLOT-POLAR-VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PLOT-XY-DATA-non-nil-data-p (XY-DATA-LISTS)
  (not
   (or
    (if (consp (caar xy-data-lists))
      ;; '(((x1 x1 ... x1)(y1 y1 ... y1))
      ;;   ((x2 x2 ... x2)(y2 y2 ... y2))
      ;;   ....
      ;;   ((xn xn ... xn)(yn yn ... yn)))
      (loop for xy-data-list in xy-data-lists unless (car xy-data-list) do (return t))
      (not (car xy-data-lists)))
    (if (consp (caar xy-data-lists))
      (loop for xy-data-list in xy-data-lists unless (cadr xy-data-list) do (return t))
      (not (cadr xy-data-lists))))))

(defun PLOT-XY-DATA-non-nil-data-p (XY-DATA-LISTS)
  (and
   (if (consp (caar xy-data-lists))
       ;; '(((x1 x1 ... x1)(y1 y1 ... y1))
       ;;   ((x2 x2 ... x2)(y2 y2 ... y2))
       ;;   ....
       ;;   ((xn xn ... xn)(yn yn ... yn)))
       (loop for xy-data-list in xy-data-lists when (car xy-data-list) do (return t))
       (caar xy-data-lists))
   (if (consp (caar xy-data-lists))
       (loop for xy-data-list in xy-data-lists when (cadr xy-data-list) do (return t))
       (cadr xy-data-lists))))

(defun parse-interleaved-xy-list (interleaved-xy-list)
  (loop for interleaved-xy in
	(if (consp (car interleaved-xy-list))
	    interleaved-xy-list
	    (list interleaved-xy-list))
	collect
	(loop for val in interleaved-xy
	      for count from 0
	      when (evenp count) collect val into x-list
	      else collect val into y-list
	      finally (return (list x-list y-list)))))

(export 'parse-interleaved-xy-list)

#|
(defun add-xy-data-to-plot-agg (win plot-agg xy-data-lists RELEVANT-LABELS line-styles same-line-style scatter polar connect-data-points connect-ends linear-regression)
  ;; For PLOT-XY-DATA
  (loop for xy-data-list in xy-data-lists
	for curve-num from 0 do
	(let ((scatter-symbol (get-scatter-symbol win curve-num))
	      (label (nth (round curve-num) relevant-labels))
	      (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles))))
	  (when scatter (add-scatter-points-to-plot-from-xy-data-list xy-data-list plot-agg win line-style scatter-symbol :data-points curve-num))
	  (when connect-data-points (add-polyline-to-plot plot-agg win (car xy-data-list) (cadr xy-data-list) line-style :connect-ends (and polar connect-ends)))
	  (when linear-regression
	    (s-value win :linear-regression t)
	    (plot-linear-regression (nth 0 xy-data-list) (nth 1 xy-data-list) win label line-style)))))
(defun add-xy-data-to-plot-agg (win plot-agg xy-data-lists RELEVANT-LABELS line-styles same-line-style scatter polar connect-data-points connect-ends linear-regression)
  (declare (ignore xy-data-lists))
  ;; For PLOT-XY-DATA
  (loop
     for x-list-group in (g-value win :x-lists)
     for y-list-group in (g-value win :y-lists)
     do
       (loop ; for xy-data-list in xy-data-lists
	  for x-list in x-list-group
	  for y-list in y-list-group
	  for curve-num from 0 do
	(let ((scatter-symbol (get-scatter-symbol win curve-num))
	      (label (nth (round curve-num) relevant-labels))
	      (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles))))
	  (when scatter (add-scatter-points-to-plot-from-xy-data-list (list x-list y-list) plot-agg win line-style scatter-symbol :data-points curve-num))
	  (when connect-data-points (add-polyline-to-plot plot-agg win x-list y-list line-style :connect-ends (and polar connect-ends)))
	  (when linear-regression
	    (s-value win :linear-regression t)
	    (plot-linear-regression x-list y-list) win label line-style)))))

(defun add-xy-data-to-plot-agg (win plot-agg RELEVANT-LABELS line-styles same-line-style)
  ;; For PLOT-XY-DATA
  (loop
     for x-list-group in (g-value win :x-lists)
     for y-list-group in (g-value win :y-lists)
     do
       (loop
	  for x-list in x-list-group
	  for y-list in y-list-group
	  for curve-num from 0 do
	    (let ((scatter-symbol (get-scatter-symbol win curve-num))
		  (label (nth (round curve-num) relevant-labels))
		  (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles))))
	      (when (g-value win :scatter) (add-scatter-points-to-plot-from-xy-data-list (list x-list y-list) plot-agg win line-style scatter-symbol :data-points curve-num))
	      (when (g-value win :connect-data-points) (add-polyline-to-plot plot-agg win x-list y-list line-style :connect-ends (and (g-value win :polar) (g-value win :connect-ends))))
	      (when (g-value win :linear-regression) (plot-linear-regression x-list y-list) win label line-style)))))
|#

(defun add-data-to-plot-agg (win plot-agg RELEVANT-LABELS line-styles same-line-style)
  ;; For PLOT-XY-DATA
  (let ((trace-references (fix-list (no-nils (or (g-value win :trace-order) (list-of-nums (num-curves-per-group win)))))))
    (loop
       for data-group fixnum from 0
       for data-group-y-lists in (g-value win :y-lists) ;; for data-group-x-lists in (g-value win :x-lists)
       do
	 (let ((data-group-x-lists (nth (if (= (max-depth (g-value win :x-lists)) (max-depth (g-value win :y-lists))) data-group 0) (g-value win :x-lists))))
	   (loop
	      for trace-reference in trace-references
	      for curve-num from 0 do
		(let* ((scatter-symbol (get-scatter-symbol win curve-num))
		       (label (nth (round curve-num) relevant-labels))
		       (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles)))
		       (curve-num-*-x-offset (* curve-num (g-value win :x-trace-offset)))
		       (curve-num-*-y-offset (* curve-num (g-value win :y-trace-offset)))
		       (y-list (nth trace-reference data-group-y-lists))
		       ;; (x-ref (or (nth trace-reference data-group-x-lists) (time-base-from-data-group-x-lists data-group-x-lists trace-reference)))
		       (x-ref (or (if (or (numberp data-group-x-lists)
					  (not (consp (nth trace-reference data-group-x-lists))))
				      (time-base-from-data-group-x-lists data-group-x-lists trace-reference)
				      (nth trace-reference data-group-x-lists))))
		       (wf-x-offset (when (g-value win :waterfall) (+ (g-value win :waterfall-base-x-offset) curve-num-*-x-offset)))
		       (wf-y-offset (when (g-value win :waterfall) (+ (g-value win :waterfall-base-y-offset) curve-num-*-y-offset)))
		       (points (get-plot-point-list x-ref y-list win :x-trace-offset wf-x-offset :y-trace-offset wf-y-offset :only-visible t)))
		  (cond-every
		   ((g-value win :linear-regression) (plot-linear-regression x-ref y-list win label line-style))
		   (points (cond-every
			    ((g-value win :scatter) (add-scatter-points-to-plot plot-agg win points line-style scatter-symbol :data-points curve-num))
			    ((or (g-value win :waterfall) (g-value win :connect-data-points))
			     (if (g-value win :use-bins)
				 (add-histo-bins plot-agg points (when (g-value win :waterfall) (+ (g-value win :waterfall-base-y-offset) curve-num-*-y-offset)))
				 (new-add-polyline-to-plot plot-agg points line-style (g-value win :polar))))
			    ((and (g-value win :waterfall) (g-value win :wf-skirt)) (add-wf-skirt plot-agg win points))))
		   ;; ((g-value win :connect-data-points) (add-polyline-to-plot plot-agg win x-ref y-list line-style :connect-ends (and (g-value win :polar) (g-value win :connect-ends))))
		   )))))))


#|
(defun add-timed-data-to-plot-agg (win plot-agg RELEVANT-LABELS line-styles same-line-style)
  (let ((trace-references (fix-list (no-nils (or (g-value win :trace-order) (list-of-nums (num-curves-per-group win)))))))
    (loop
       for data-group fixnum from 0
       for data-group-y-lists in (g-value win :y-lists) ;; for data-group-x-lists in (g-value win :x-lists)
       do
	 (let ((data-group-x-lists (nth (if (= (max-depth (g-value win :x-lists)) (max-depth (g-value win :y-lists))) data-group 0) (g-value win :x-lists))))
	   (loop
	      for trace-reference in trace-references
	      for curve-num from 0 do
		(let* ((scatter-symbol (get-scatter-symbol win curve-num))
		       (label (nth (round curve-num) relevant-labels))
		       (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles)))
		       (curve-num-*-x-offset (* curve-num (g-value win :x-trace-offset)))
		       (curve-num-*-y-offset (* curve-num (g-value win :y-trace-offset)))
		       (y-list (nth trace-reference data-group-y-lists))
		       (x-ref (or (if (or (numberp data-group-x-lists)
					  (not (consp (nth trace-reference data-group-x-lists))))
				      (time-base-from-data-group-x-lists data-group-x-lists trace-reference)
				      (nth trace-reference data-group-x-lists))))
		       (wf-x-offset (when (g-value win :waterfall) (+ (g-value win :waterfall-base-x-offset) curve-num-*-x-offset)))
		       (wf-y-offset (when (g-value win :waterfall) (+ (g-value win :waterfall-base-y-offset) curve-num-*-y-offset)))
		       (points (get-plot-point-list x-ref y-list win :x-trace-offset wf-x-offset :y-trace-offset wf-y-offset :only-visible t)))
		  (cond-every
		   ((g-value win :linear-regression) (plot-linear-regression x-ref y-list win label line-style))
		   (points (cond-every
			    ((g-value win :scatter) (add-scatter-points-to-plot plot-agg win points line-style scatter-symbol :data-points curve-num))
			    ((or (g-value win :waterfall) (g-value win :connect-data-points))
			     (if (g-value win :use-bins)
				 (add-histo-bins plot-agg points (when (g-value win :waterfall) (+ (g-value win :waterfall-base-y-offset) curve-num-*-y-offset)))
				 (new-add-polyline-to-plot plot-agg points line-style (g-value win :polar))))
			    ((and (g-value win :waterfall) (g-value win :wf-skirt)) (add-wf-skirt plot-agg win points))))
		   ;; ((g-value win :connect-data-points) (add-polyline-to-plot plot-agg win x-ref y-list line-style :connect-ends (and (g-value win :polar) (g-value win :connect-ends))))
		   )))))))
|#

(defmacro plot-timed-data-and-xy-data-transfer-plotting-vars ()
  `(if preserve-win-attributes
       (setq plot-point-skip (g-value win :plot-point-skip)
	     x-are-fns (g-value win :x-are-fns)
	     y-are-fns (g-value win :y-are-fns)
	     x-label-h-position (g-value win :x-label-h-position)
	     x-label-v-position (g-value win :x-label-v-position)
	     y-label-h-position (g-value win :y-label-h-position)
	     y-label-v-position (g-value win :y-label-v-position)
	     label-traces (g-value win :label-traces)
	     draw-grid (g-value win :draw-grid)
	     x-log (g-value win :x-log)
	     y-log (g-value win :y-log)
	     log-base (g-value win :log-base)
	     use-bins (g-value win :use-bins)
	     waterfall-trace-label-skip (g-value win :waterfall-trace-label-skip)
	     waterfall-label-offset (g-value win :waterfall-label-offset))
       (progn
	 (s-values win
		   reference-ticks-to-origin stipple-percent draw-grid label-waterfall x-axis-p y-axis-p
		   (:absolute-value-ticks polar)
		   (:use-same-line-style (or (g-value win :waterfall) use-same-line-style))
		   (:log-base (when (numberp log-base) (s-flt log-base))))
	 (cond-every
	  (plot-point-skip-supplied-p (s-value win :plot-point-skip plot-point-skip))
	  (axes-type (s-value win :axes-type axes-type))
	  (y-label-h-position (s-value win :y-label-h-position y-label-h-position))
	  (grid-line-style (s-value win :grid-line-style grid-line-style))))))

(defmacro plot-timed-and-xy-data-update-polar-plot-parameters ()
  `(when polar
     (when win (setq polar-circles-p (g-value win :polar-circles-p)))
     (when (and y-inc x-inc) (setq x-inc (max x-inc y-inc) y-inc (max x-inc y-inc)))
     (setq x-label "" x-origin-tick nil y-origin-tick nil
	   x-max (if (and x-max y-max) (max x-max y-max) (or y-max x-max)))
     (unless x-max (error-message "Need to specify x-max or y-max for polar plot"))
     (setq y-max x-max y-min (* -1 x-max) x-min (* -1 x-max))))

(defmacro plot-timed-and-xy-data-update-axis-scatter-label-gap-wf-polar-parameters ()
  `(progn
     (when (or new-win-p (not preserve-win-attributes))
       (cond-every
	(x-axis-coeff (s-value win :x-axis-coeff x-axis-coeff))
	(y-axis-coeff (s-value win :y-axis-coeff y-axis-coeff))
	(x-axis-prefix-supplied-p (s-value win :x-axis-prefix x-axis-prefix))
	(x-axis-suffix-supplied-p (s-value win :x-axis-suffix x-axis-suffix))
	(y-axis-prefix-supplied-p (s-value win :y-axis-prefix y-axis-prefix))
	(y-axis-suffix-supplied-p (s-value win :y-axis-suffix y-axis-suffix))
	(x-axis-tick-mark-skip-suppplied (s-value win :x-axis-tick-mark-skip x-axis-tick-mark-skip))
	(y-axis-tick-mark-skip-suppplied (s-value win :y-axis-tick-mark-skip y-axis-tick-mark-skip))
	(axes-type (s-value win :axes-type axes-type))
	(plot-polyline-as-multipoint-supplied-p (s-value win :plot-polyline-as-multipoint plot-polyline-as-multipoint))
	(scatter-symbol (s-value win :scatter-symbol scatter-symbol))
	(scatter-symbol-fill (s-value win :scatter-symbol-fill scatter-symbol-fill))
	(scatter-width-heights (s-value win :scatter-width-heights scatter-width-heights))
	(x-scale-l% (s-value win :x-scale-l% x-scale-l%))
	(x-scale-t% (s-value win :x-scale-t% x-scale-t%))
	(y-scale-t% (s-value win :y-scale-t% y-scale-t%))
	(y-label-h-position (s-value win :y-label-h-position y-label-h-position))
	(x-label-h-position (s-value win :x-label-h-position x-label-h-position))
	(update-fixed-gap (update-fixed-gap win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap)))
       (s-values win (scatter (or (g-value win :scatter) scatter)) scatter-symbol-borderp fill-scatter connect-data-points
		 x-symbol-width y-symbol-width consider-y-axis-visible-limit y-axis-visible-min y-axis-visible-max consider-x-axis-visible-limit x-axis-visible-min x-axis-visible-max
		 waterfall-trace-label-skip waterfall-label-offset  simple-axis-x-value-p simple-axis-y-value-p x-axis-p y-axis-p (absolute-value-ticks polar))
       (when (eq :waterfall (g-value win :plot-type))
	 (s-value win :waterfall (cond (auto-wf-setup :auto)
				       (waterfall t)))
	 (when (or (eq waterfall :auto) (and auto-wf-setup-supplied-p auto-wf-setup) (g-value win :auto-setup))
	   (s-value win :auto-wf-setup (not (or replot-win-point-list restore-plot unzoom))))
	 (setq auto-wf-setup t
	       label-waterfall t
	       waterfall t wf-skirt t))
       (s-values win
		 (log-base (when (numberp log-base) (s-flt log-base)))
		 use-bins (bin-width (s-flt (or bin-width 0))) draw-grid polar (polar-circles-p (and polar polar-circles-p))
		 consider-y-axis-visible-limit y-axis-visible-min y-axis-visible-max consider-x-axis-visible-limit x-axis-visible-min x-axis-visible-max
		 waterfall-trace-label-skip waterfall-label-offset simple-axis-x-value-p simple-axis-y-value-p))))



;; scatter-lists (((x1...)(y1....)) ((x2...)(y2....)) ...)

(defun scatter-statistics-comment (scatter-lists labels win &key (position :upper-right))
  (add-comment win (loop for x-list-y-list in
			 scatter-lists for label in labels collect (format nil "~D(~A)" (length (nth 0 x-list-y-list)) label)
			 into total-number-strings
			 nconc (loop for n from 0 to 1
				     collect (format nil "~A ~A: Mean(sd) ~,2e(~,2e)"
						     label (if (zerop n) (g-value win :x-label) (g-value win :y-label))
						     (mean (nth n x-list-y-list))
						     (std-dev (nth n x-list-y-list))))
			 into mean-std-strings
			 finally (return (concatenate-string-list (list (format nil "N = ~{~a ~}" total-number-strings) mean-std-strings)
								  :lf-count 1))) :position position))

(defun add-scatter-points-to-plot-from-xy-data-list (xy-data-list plot-agg win line-style symbol what-is-it curve-num)
  (let ((points (loop for x in (car xy-data-list)
		      for y in (cadr xy-data-list)
		      when (and (numberp x) (numberp y)) collect (x-plot-win x win) and collect (y-plot-win y win))))
    (add-scatter-points-to-plot plot-agg win points line-style symbol what-is-it curve-num)))

(defun time-base-from-data-group-x-lists (data-group-x-lists trace-reference)
  (typecase data-group-x-lists
    (number data-group-x-lists)
    (t (nth (if (= (length data-group-x-lists) 1) 0 trace-reference) data-group-x-lists))))

(defun parse-line-styles-for-plot (line-styles win waterfall use-same-line-style num-curves-per-group)
  (let ((temp-styles (cond
		      ((consp line-styles) line-styles)
		      ((consp (g-value win :plot-line-style-family)) (g-value win :plot-line-style-family))
		      (t (coerce-to-list
			  (get-line-styles (or line-styles (g-value win :plot-line-style-family))
					   (unless (or waterfall use-same-line-style) num-curves-per-group)))))))
    (if (or waterfall use-same-line-style)
      (list (car temp-styles))
      temp-styles)))

(defun number-of-overlays (win)
  ;;  should be identical to  (length (g-value win :y-lists))
  (length (g-value win :x-lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                         ************* PLOT-TIMED-DATA *************
;;
;; DATA-SEQUENCES are one or more sets of Y data point sequences; a single set of lists is for adding new data to a window
;; (which may have data already), and multiple sets are for creating overlaid plots from scratch. In the former case,
;; the format is a list of sequences: '((ydata1-seq) (ydata2-seq)...), where each sequence may be a list or an array of
;; Y points. In the latter case, the format is a list of lists of lists: '(((ydata1A-list) (ydata2A-list))
;; ((ydata1B-list) (ydata2B-list))...). In this case the data points must all be in lists. Note that this second format
;; is the way that the y data is stored in the :y-lists slot of the plotting window. The format for the optional
;; TIME-BASE argument is the same, except that it is possible for there to be only one X data point list corresponding
;; to a set of Y data point lists; this may occur, for example, when there is a common time base for a set of Y
;; data. LABELS is an optional list of labels, the number of labels typically corresponding to the number of Y data
;; point sequences is the first set in the DATA-LISTS list of sequences of Y data point sequences.  If TIME-BASE is NIL,
;; then the time base is inferred from the DELTA-T argument (default 1.0). If LABELS is NIL, AND the target window
;; has no label-list, AND :LABEL-TRACES is t (default) then labels of the form "CANONIC-LABEL-i" are used, where i
;; ranges from 1 to the number of data lists, and CANONIC-LABEL is taken from the argument of the same name (default
;; "DATA"). The ACCOMODATE-OVERLAYS argument causes the plot dimensions to change in order to accomodate subsequent
;; overlaid data.  The data and time points *must* be single floats!! (NOT TRUE?)

;; :X-TRACE-OFFSET and :Y-TRACE-OFFSET are applied to the data *after*, and :X-DATA-OFFSET and :Y-DATA-OFFSET are
;; applied to the data *before* log (when :X-LOG and/or :Y-LOG is set).

;; :AXES-TYPE can be either NIL (axes type unchanged for plotting to an old window, with the default :STANDARD) :SIMPLE,
;; :STANDARD or :NONE. You can also enable X or Y axes with the :X-AXIS-P and :Y-AXIS-P arguments (both default to T).

;; When the :AUTO-WF-SETUP argument is non-NIL, then a waterfall plot will be made with the traces stacked vertically
;; (spaced by the largest amplitude trace), the trace labels centered vertically to the right of each trace, and the
;; simple axes set in the lower left corner.

;; If the :ERASE-DATA-AFTER-PLOT key argument is T (default NIL), then data that is normally stored in a (standard) plot
;; window is erased after the plot is done, but not the displayed image. This may be useful if there are a lot of plots
;; and memory is getting tight. While a window whose data has been erased may be printed and examined with the cross
;; hairs, zooming, unzooming, restoring, or other rescaling will not be possible.

(defmacro plot-timed-and-xy-data-setup-plot ()
  `(when (run-setup-plot-p win) ;; run-setup-plot
     (setup-plot win :width width :height height :preserve-plot-attributes preserve-plot-attributes :reference-ticks-to-origin reference-ticks-to-origin
		 :wf-skirt wf-skirt :consider-labels (and label-traces (not waterfall))
		 :x-are-fns x-are-fns :y-are-fns y-are-fns :fix-to-unity-mag-if-so fix-to-unity-mag-if-so
		 :default-x-incs default-x-incs :default-y-incs default-y-incs
		 :x-inc x-inc :x-min-spec x-min :x-max-spec x-max :x-log x-log :y-inc y-inc :y-min-spec y-min :y-max-spec y-max :y-log y-log
		 :x-label x-label :x-label-h-position x-label-h-position :x-label-v-position x-label-v-position
		 :y-label y-label :y-label-h-position y-label-h-position :y-label-v-position y-label-v-position :invert-y-axis-label invert-y-axis-label
		 :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick :x-origin x-origin :y-origin y-origin :x-axis-root x-axis-root :y-axis-root y-axis-root
		 :x-trace-offset x-trace-offset :x-data-offset (if waterfall (g-value win :waterfall-base-x-offset) x-data-offset)
		 :y-trace-offset y-trace-offset :y-data-offset (if waterfall (g-value win :waterfall-base-y-offset) y-data-offset)
		 :unzoom unzoom :restore-plot restore-plot :replot-win-point-list replot-win-point-list :revise-plot revise-plot)))


(defun plot-timed-data (data-sequences
			&optional labels (time-base 1.0)
			&key win
			  (width (and win (g-value win :width))) (height (and win (g-value win :height))) left top ; The win dims and position in pixels.
			  data-type ; Optional. May be used to select plot windows according to some aspect of the data.
			  x-lists y-lists (delta-t-start 0.0) (timed-data t)
			  prompt-for-overlay (overlay *overlay-plots*) (accomodate-overlays *accomodate-overlays*)
			  preserve-plot-attributes ; :label-traces, (via setup-plot -> setup-plot-preliminaries) others
			  (preserve-plot-layout *preserve-plot-layout*) preserve-win-attributes preserve-win-dims ; ??
			  (update t) (resurrect t) (visible t)
			  (plot-point-skip nil plot-point-skip-supplied-p)
			  ;;
			  (label-traces *label-plot-traces*) (CANONIC-LABEL "DATA") (x-label *default-x-label*)  x-label-v-position x-label-h-position
			  (y-label *default-y-label*) y-label-v-position (y-label-h-position :left) invert-y-axis-label
			  (x-label-position nil x-label-position-supplied-p) ; backward comp
			  ;;
			  x-are-fns x-axis-coeff (x-axis-prefix nil x-axis-prefix-supplied-p) (x-axis-suffix nil x-axis-suffix-supplied-p)
			  y-are-fns y-axis-coeff (y-axis-prefix nil y-axis-prefix-supplied-p) (y-axis-suffix nil y-axis-suffix-supplied-p)
			  ;;
			  x-min x-max x-inc x-origin y-min y-max y-inc y-origin x-log y-log log-base
			  (default-x-incs 5) (default-y-incs 5) ; If no explicit value for axis increments
			  axes-type	; (:standard :simple :none)
			  x-axis-root y-axis-root (x-axis-p t) (y-axis-p t)
			  x-scale-l% x-scale-t% y-scale-t% ; for positioning simple axes
			  (simple-axis-x-value-p t) (simple-axis-y-value-p t)
			  consider-x-axis-visible-limit x-axis-visible-max x-axis-visible-min consider-y-axis-visible-limit y-axis-visible-max y-axis-visible-min
			  ;;
			  linear-regression odd-quadrant-diagonal even-quadrant-diagonal
			  ;;
			  (x-origin-tick nil) (y-origin-tick t) reference-ticks-to-origin (include-x-tick-at-0 :follow-window) (include-y-tick-at-0 :follow-window)
			  ;;
			  use-bins bin-width (stipple-percent 100)
			  line-styles use-same-line-style
			  title upper-right-comment comment (comment-position *default-comment-position*) session-name
			  ;;
			  UPDATE-FIXED-GAP use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0) use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
			  ;;
			  (draw-grid *default-plot-grid-p*) grid-line-style
			  (x-data-offset 0.0) (y-data-offset 0.0) (x-trace-offset 0.0) (y-trace-offset 0.0)
			  fix-to-unity-mag-if-so
			  ;;
			  (connect-data-points t) (scatter *default-plot-timed-data-scatter-enable*) scatter-symbol-fill
			  (plot-POLYLINE-as-multipoint nil plot-POLYLINE-as-multipoint-supplied-p)
			  scatter-symbol ; This can be a single symbol or a list, as is *scatter-symbols* (default)
			  scatter-width-heights (scatter-symbol-borderp t) (fill-scatter t)
			  (x-symbol-width *default-scatter-size*) (y-symbol-width *default-scatter-size*)
			  ;;
			  replot-win-point-list restore-plot unzoom revise-plot erase-data-after-plot (save-markers t)
			  polar (polar-circles-p t) connect-ends
			  waterfall label-waterfall wf-skirt (auto-wf-setup nil auto-wf-setup-supplied-p) (waterfall-trace-label-skip 0) (waterfall-label-offset 0.0))
  (declare (ignore visible INCLUDE-X-TICK-AT-0 INCLUDE-Y-TICK-AT-0 CONNECT-ENDS))
  (when (or y-lists (and win (gv win :y-lists)) (numeric-plot-data-p data-sequences) replot-win-point-list restore-plot unzoom revise-plot) ; Verify data to plot. For Y values :y-lists > data-sequences
    (when x-label-position-supplied-p (setq x-label-v-position x-label-position)) ; backward comp
    (let ((plot-type (cond (waterfall :waterfall)
			   (polar (progn (when (and width height) (setq width (max width height) height (max width height))) :polar))
			   (win (g-value win :plot-type))
			   (t :xy)))
	  X-AXIS-TICK-MARK-SKIP X-AXIS-TICK-MARK-SKIP-SUPPPLIED Y-AXIS-TICK-MARK-SKIP Y-AXIS-TICK-MARK-SKIP-SUPPPLIED) ;; For compatibility with plot-xy-data
      (case plot-type
	((:xy :waterfall) (setq polar nil))
	(:polar (setq polar t)))
      (let* ((get-plot-win (get-plot-window plot-type data-type overlay :default-win win :left left :top top :width width :height height
					    :preserve-plot-layout preserve-plot-layout :accomodate-overlays accomodate-overlays :preserve-win-dims preserve-win-dims
					    :title (format nil "~A" (or title (and win (g-value win :title)) "Data Plot"))
					    :prompt-for-overlay prompt-for-overlay :session-name session-name :save-markers save-markers))
	     (new-win-p (not (eq get-plot-win win)))
	     (win get-plot-win))
	(when (and win (not (g-value win :data-erased)))
	  (s-value win :timed-data timed-data)
	  (when line-styles (s-value win :plot-line-style-family line-styles))
	  (plot-timed-and-xy-data-update-axis-scatter-label-gap-wf-polar-parameters)
	  (plot-timed-and-xy-data-update-polar-plot-parameters)
	  (plot-timed-data-and-xy-data-transfer-plotting-vars)
	  (unless preserve-win-attributes (when delta-t-start (s-value win :delta-t-start (s-flt delta-t-start))))
	  (when linear-regression (s-value win :linear-regression t))
	  (when upper-right-comment (add-temp-comment win upper-right-comment))
	  (add-local-and-global-comment comment comment-position win)
	  (if replot-win-point-list
	      (setq restore-plot nil unzoom nil) ; just to be safe.
	      (when restore-plot (setq unzoom nil)))
	  (if (or replot-win-point-list restore-plot unzoom revise-plot)
	      (progn		  ; Extract arguments from the window.
		(s-value win :overlay nil)
		(when (or replot-win-point-list restore-plot unzoom)
		  (setq x-trace-offset (g-value win :x-trace-offset) y-trace-offset (g-value win :y-trace-offset)
			waterfall (eq :waterfall (g-value win :plot-type)) wf-skirt (g-value win :wf-skirt)
			x-label nil y-label nil connect-data-points (g-value win :connect-data-points))))
	      (progn		  ; Otherwise new plot, maybe overlay.
		(s-value win :connect-data-points connect-data-points)
		;; For the Y values, the precedence is :y-lists argument > data-sequences
		(cond (y-lists (s-value win :y-lists y-lists))
		      (data-sequences (load-y-lists win data-sequences)))
		;; For the X values, the precedence is :x-lists argument > time-base
		(unless (or (and (not data-sequences) (g-value win :x-lists)) (sequencep time-base))
		  (s-value win :delta-t time-base))
		(cond (x-lists (s-value win :x-lists x-lists))
		      (time-base (load-x-lists win time-base)))))
	  (s-values win canonic-label)
	  (clear-up-label-list win labels)
	  (let* ((num-curves-per-group (num-curves-per-group win))
		 (old-plot-agg (car (find-plot-agg win 'data-plot)))
		 (renew-plot-agg (renew-plot-agg-p win old-plot-agg))
		 (plot-agg (if renew-plot-agg (add-plot-agg win 'data-plot) old-plot-agg))
		 (line-styles (parse-line-styles-for-plot line-styles win waterfall use-same-line-style num-curves-per-group))
		 (same-line-style (when use-same-line-style (car line-styles)))
		 (relevant-labels (relevant-labels win num-curves-per-group)))
	    (plot-timed-and-xy-data-setup-plot)
	    (add-data-to-plot-agg win plot-agg RELEVANT-LABELS line-styles same-line-style)
	    (cond-every
	     ((g-value win :label-traces) (label-traces plot-agg num-curves-per-group line-styles relevant-labels))
	     (erase-data-after-plot (erase-plot-data win))
	     (odd-quadrant-diagonal (mark-plot-odd-quadrant-diagonal win))
	     (even-quadrant-diagonal (mark-plot-even-quadrant-diagonal win)))
	    (plot-windows-finishing win (cond (resurrect :resurrect)
					      (update :update))
				    (when renew-plot-agg old-plot-agg))))))))

(defun plot-xy-data (xy-data-ref
		     &optional labels
		     &key (title "XY Data") win width height left top ; Pixels.
		       x-lists y-lists interleaved-xy-data
		       (x-axis-tick-mark-skip nil x-axis-tick-mark-skip-suppplied) (y-axis-tick-mark-skip nil y-axis-tick-mark-skip-suppplied)
		       data-type		; Optional. May be used to select plot windows according to some aspect of the data.
		       prompt-for-overlay (overlay *overlay-plots*) (accomodate-overlays *accomodate-overlays*)
		       (preserve-plot-layout *preserve-plot-layout*) preserve-win-attributes preserve-win-dims
		       (update t) (resurrect t) (visible t)
		       (plot-point-skip nil plot-point-skip-supplied-p)
		       ;;
		       (canonic-label "DATA") (x-label "") (y-label "")
		       x-label-h-position x-label-v-position ; :below :above
		       invert-y-axis-label y-label-v-position (y-label-h-position :left)
		       ;;
		       x-are-fns y-are-fns x-axis-coeff y-axis-coeff
		       (x-axis-prefix nil x-axis-prefix-supplied-p) (x-axis-suffix nil x-axis-suffix-supplied-p)
		       (y-axis-prefix nil y-axis-prefix-supplied-p) (y-axis-suffix nil y-axis-suffix-supplied-p)
		       ;;
		       x-min x-max x-inc x-origin y-min y-max y-inc y-origin x-log y-log log-base
		       (default-x-incs 5) (default-y-incs 5)
		       axes-type		; (:standard :simple :none)
		       x-axis-root y-axis-root (x-axis-p t) (y-axis-p t)
		       x-scale-l% x-scale-t% y-scale-t% ; for positioning simple axes
		       (simple-axis-x-value-p t) (simple-axis-y-value-p t)
		       consider-x-axis-visible-limit x-axis-visible-max x-axis-visible-min consider-y-axis-visible-limit y-axis-visible-max y-axis-visible-min
		       ;;
		       linear-regression odd-quadrant-diagonal even-quadrant-diagonal
		       ;;
		       (x-origin-tick nil) (y-origin-tick t) reference-ticks-to-origin (include-x-tick-at-0 :follow-window) (include-y-tick-at-0 :follow-window)
		       ;;
		       use-bins bin-width (stipple-percent 100)
		       line-styles use-same-line-style
		       (upper-right-comment "") comment (comment-position *default-comment-position*)
		       ;;
		       update-fixed-gap use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0) use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
		       ;;
		       (draw-grid *default-plot-grid-p*) grid-line-style
		       (label-traces *label-plot-traces*)
		       (x-data-offset 0.0) (y-data-offset 0.0) (x-trace-offset 0.0) (y-trace-offset 0.0)
		       fix-to-unity-mag-if-so session-name
		       ;;
		       transpose-data (connect-data-points t) scatter scatter-symbol-fill (plot-polyline-as-multipoint nil plot-polyline-as-multipoint-supplied-p)
		       scatter-symbol	; This can be a single symbol or a list, as is *scatter-symbols* (default)
		       scatter-width-heights (scatter-symbol-borderp t) (fill-scatter t) (x-symbol-width *default-scatter-size*) (y-symbol-width *default-scatter-size*)
		       ;;
		       polar (polar-circles-p t) connect-ends
		       waterfall label-waterfall wf-skirt (auto-wf-setup nil auto-wf-setup-supplied-p) (waterfall-trace-label-skip 0) (waterfall-label-offset 0.0)
		       (save-markers t) erase-data-after-plot)
  "Plot data set(s) with explicit x and y coordinates, according to XY-DATA-REF, with optional LABELS being either a string or symbol, or a list of strings or symbols.

Data from files:

XY-DATA-REF = \"pathname\"

            or

              (\"pathname\" \"pathname\" ...)

Where \"pathname\" is an ASCII numeric file, such that each successive number is taken as successive interleaved X and Y values. Note that in this case,
INTERLEAVED-XY-DATA is taken to be T.

Explicit numeric structure:

If INTERLEAVED-XY-DATA is NIL (default) then

XY-DATA-REF = '(((x1 x1 ... x1)(y1 y1 ... y1))
                ((x2 x2 ... x2)(y2 y2 ... y2))
                ....
                ((xn xn ... xn)(yn yn ... yn)))

            or

              '((x x ... x)(y y ... y))

If INTERLEAVED-XY-DATA is T then

XY-DATA-REF = '((x1 y1 x1 y1 ...)
                (x2 y2 x2 y2 ...)
                ...
                (xn yn xn yn ...)
           or
              '(x y x y x y ...)"
  (declare (ignore visible include-x-tick-at-0 include-y-tick-at-0 erase-data-after-plot x-lists y-lists))
  ;; Process input data, whether from numeric files or explicit numeric data.
  (let ((xy-data-lists (cond ((every 'EXISTING-FILENAME-P (coerce-to-list xy-data-ref))
			      (setq interleaved-xy-data t)
			      (loop for filename in (coerce-to-list xy-data-ref) collect (read-number-file filename)))
			     (t xy-data-ref)))
	PRESERVE-PLOT-ATTRIBUTES REVISE-PLOT REPLOT-WIN-POINT-LIST RESTORE-PLOT UNZOOM ;; for plot-timed-data compatibility
	)
    (when (plot-xy-data-non-nil-data-p xy-data-lists) ;; If non-nil data, then go ahead....
      (when interleaved-xy-data (setq xy-data-lists (parse-interleaved-xy-list xy-data-lists)))
      (let ((plot-type (cond (waterfall :waterfall)
			     (polar (progn (when (and width height) (setq width (max width height) height (max width height))) :polar))
			     (win (g-value win :plot-type))
			     (t :xy))))
	(case plot-type
	  ((:xy :waterfall) (setq polar nil))
	  (:polar (setq polar t)))
	(let* ((get-plot-win (get-plot-window
			      plot-type data-type overlay :default-win win :left left :top top :width width :height height
			      :preserve-plot-layout preserve-plot-layout :accomodate-overlays accomodate-overlays :preserve-win-dims preserve-win-dims
			      :title (format nil "~A" (or title (and win (g-value win :title)) "Data Plot"))
			      :prompt-for-overlay prompt-for-overlay :session-name session-name :save-markers save-markers))
	       (new-win-p (not (eq get-plot-win win)))
	       (win get-plot-win))
	  (when (and win (not (g-value win :data-erased)))
	    (plot-timed-and-xy-data-update-polar-plot-parameters)
	    (plot-timed-data-and-xy-data-transfer-plotting-vars)
	    (plot-timed-and-xy-data-update-axis-scatter-label-gap-wf-polar-parameters)
	    (add-temp-comment win upper-right-comment :update nil) (add-local-and-global-comment comment comment-position win :update nil)
	    (update-plot-xy-win-xy-data-lists win xy-data-lists transpose-data)
	    (plot-timed-and-xy-data-setup-plot)
	    (s-values win
		      canonic-label
		      connect-data-points
		      connect-ends
		      polar
		      linear-regression
		      )
	    (clear-up-label-list win labels)
	    (let* ((curves-per-group (num-curves-per-group win))
		   (old-plot-agg (car (wh::find-plot-agg win 'data-plot)))
		   (renew-plot-agg (renew-plot-agg-p win old-plot-agg))
		   (plot-agg (if renew-plot-agg (add-plot-agg win 'data-plot) old-plot-agg))
		   (line-styles (coerce-to-list (or line-styles (get-line-styles (g-value win :plot-line-style-family) (unless use-same-line-style curves-per-group)))))
		   (relevant-labels (relevant-labels win curves-per-group))
		   (same-line-style (when use-same-line-style (car line-styles))))
	      (add-data-to-plot-agg win plot-agg RELEVANT-LABELS line-styles same-line-style)
	      (when (g-value win :label-traces) (label-traces plot-agg curves-per-group line-styles relevant-labels))
	      (cond-every (odd-quadrant-diagonal (mark-plot-odd-quadrant-diagonal win))
			  (even-quadrant-diagonal (mark-plot-even-quadrant-diagonal win)))
	      (plot-windows-finishing win (cond (resurrect :resurrect)
						(update :update))
				      (when renew-plot-agg old-plot-agg)))))))))

(defun x-list-y-list-from-xy-point-list (list-of-point-lists &optional transpose)
  (loop for point-list in (if (consp (caar list-of-point-lists)) list-of-point-lists (list list-of-point-lists))
	collect (loop for point in point-list
		   when (and point transpose) do (setq point (reverse point))
		   when point
		   collect (car point) into x-list and collect (cadr point) into y-list
		      finally (return (no-nils (list x-list y-list))))))

(defun plot-points (list-of-point-lists
		    &optional label-list
		    &key win (title "XY Data") ; (scale 0.8)
		      transpose-points
		      prompt-for-overlay (overlay *overlay-plots*) (accomodate-overlays *accomodate-overlays*)
		      (preserve-plot-layout *preserve-plot-layout*) preserve-win-attributes preserve-win-dims

		      (connect-data-points t) scatter scatter-symbol scatter-width-heights (scatter-symbol-borderp t) (fill-scatter t) plot-POLYLINE-as-multipoint
		      x-symbol-width y-symbol-width
		      even-quadrant-diagonal odd-quadrant-diagonal linear-regression
		      x-axis-prefix x-axis-suffix y-axis-prefix y-axis-suffix
		      (x-label "") x-label-v-position x-label-h-position
		      (y-label "") y-label-v-position (y-label-h-position :left)

		      (draw-grid *default-plot-grid-p*)

		      (x-origin-tick nil) (y-origin-tick t)
		      line-styles

		      x-are-fns y-are-fns
		      axes-type		; (:standard :simple :none)
		      x-scale-l% x-scale-t% y-scale-t%
		      (simple-axis-x-value-p t) (simple-axis-y-value-p t)

		      (label-traces t)
		      update-fixed-gap
		      use-fixed-top-gap (fixed-top-gap 0)
		      use-fixed-bottom-gap (fixed-bottom-gap 0)
		      use-fixed-right-gap (fixed-right-gap 0)
		      use-fixed-left-gap (fixed-left-gap 0)
		      use-same-line-style
		      polar connect-ends waterfall
		      session-name comment (upper-right-comment "") (comment-position *default-comment-position*)
		      left top (width 400) (height 400)
		      x-inc y-inc y-min y-max x-min x-max  x-origin y-origin)
  "For plotting a set or sets of XY points.
The data format of LIST-OF-POINT-LISTS is
        '((x y)(x y)(x y)(x y)...) for one set
or
        '(((x1 y1)(x1 y1)...(x1 y1)) ((x2 y2)(x2 y2)...(x2 y2)) ...) for more than one set.

If TRANSPOSE-POINTS [default NIL], then for each pair (x y) the values are interchanged.
 "
  (plot-xy-data
   (x-list-y-list-from-xy-point-list list-of-point-lists transpose-points)
   label-list
   :win win :title title		; :scale scale
   :overlay overlay :prompt-for-overlay prompt-for-overlay :accomodate-overlays accomodate-overlays
   :preserve-plot-layout preserve-plot-layout :preserve-win-attributes preserve-win-attributes
   :preserve-win-dims preserve-win-dims
   :connect-data-points connect-data-points
   :scatter scatter :scatter-symbol scatter-symbol :plot-POLYLINE-as-multipoint plot-POLYLINE-as-multipoint
   :fill-scatter fill-scatter :scatter-width-heights scatter-width-heights :scatter-symbol-borderp scatter-symbol-borderp
   :even-quadrant-diagonal even-quadrant-diagonal :odd-quadrant-diagonal odd-quadrant-diagonal
   :linear-regression linear-regression
   :x-label x-label :y-label y-label
   :x-label-v-position x-label-v-position :x-label-h-position x-label-h-position
   :y-label-v-position y-label-v-position :y-label-h-position y-label-h-position
   :x-axis-prefix x-axis-prefix :x-axis-suffix x-axis-suffix
   :y-axis-prefix y-axis-prefix :y-axis-suffix y-axis-suffix
   :draw-grid draw-grid
   :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
   :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
   :axes-type axes-type :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
   :update-fixed-gap update-fixed-gap
   :use-fixed-top-gap use-fixed-top-gap :fixed-top-gap fixed-top-gap
   :use-fixed-bottom-gap use-fixed-bottom-gap :fixed-bottom-gap fixed-bottom-gap
   :use-fixed-right-gap use-fixed-right-gap :fixed-right-gap fixed-right-gap
   :use-fixed-left-gap use-fixed-left-gap :fixed-left-gap fixed-left-gap
   :width width :height height :left left :top top
   :line-styles line-styles :use-same-line-style use-same-line-style
   :label-traces label-traces
   :polar polar :connect-ends connect-ends :waterfall waterfall
   :x-symbol-width x-symbol-width :upper-right-comment upper-right-comment :comment comment
   :comment-position comment-position
   :y-symbol-width y-symbol-width :session-name session-name
   :x-are-fns x-are-fns :y-are-fns y-are-fns
   :x-inc x-inc :y-inc y-inc :y-min y-min :y-max y-max :x-min x-min :x-max x-max :x-origin x-origin :y-origin y-origin))

(defun plot-scatter (list-of-point-lists
		     &optional label-list
		     &key win (title "XY Data")	; (scale 0.8)
		     prompt-for-overlay (overlay *overlay-plots*) (accomodate-overlays *accomodate-overlays*)
		     (preserve-plot-layout *preserve-plot-layout*) preserve-win-attributes preserve-win-dims
		     connect-data-points (scatter t) (scatter-symbol :dot) (x-symbol-width *default-scatter-size*)
		     y-symbol-width
		     (scatter-symbol-borderp t) (fill-scatter t)
		     plot-POLYLINE-as-multipoint
		     even-quadrant-diagonal odd-quadrant-diagonal linear-regression
		     (x-label "") x-label-v-position x-label-h-position
		     (y-label "") y-label-v-position (y-label-h-position :left)
		     (draw-grid *default-plot-grid-p*)
		     (x-origin-tick nil)(y-origin-tick t)
		     line-styles
		     x-axis-prefix x-axis-suffix
		     y-axis-prefix y-axis-suffix

		     x-are-fns y-are-fns
		     axes-type		; (:standard :simple :none)
		     x-scale-l% x-scale-t% y-scale-t%
		     (simple-axis-x-value-p t) (simple-axis-y-value-p t)

		     UPDATE-FIXED-GAP
		     use-fixed-top-gap (fixed-top-gap 0)
		     use-fixed-bottom-gap (fixed-bottom-gap 0)
		     use-fixed-right-gap (fixed-right-gap 0)
		     use-fixed-left-gap (fixed-left-gap 0)

		     (label-traces t)
		     use-same-line-style

		     comment (upper-right-comment "") (comment-position *default-comment-position*)
		     session-name
		     left top (width 400) (height 400)
		     x-inc y-inc y-min y-max x-min x-max  x-origin y-origin)
  (plot-points list-of-point-lists label-list
	       :win win :title title ; :scale (or scale (and win (g-value win :scale)))
	       :overlay overlay :prompt-for-overlay prompt-for-overlay :accomodate-overlays accomodate-overlays
	       :preserve-plot-layout preserve-plot-layout :preserve-win-attributes preserve-win-attributes
	       :preserve-win-dims preserve-win-dims

	       :connect-data-points connect-data-points
	       :scatter scatter :scatter-symbol scatter-symbol :x-symbol-width x-symbol-width :y-symbol-width (or y-symbol-width x-symbol-width)
	       :scatter-symbol-borderp scatter-symbol-borderp :fill-scatter fill-scatter
	       :plot-POLYLINE-as-multipoint plot-POLYLINE-as-multipoint
	       :even-quadrant-diagonal even-quadrant-diagonal :odd-quadrant-diagonal odd-quadrant-diagonal
	       :linear-regression linear-regression
	       :x-label x-label :y-label y-label
	       :x-label-v-position x-label-v-position
	       :x-label-h-position x-label-h-position
	       :y-label-v-position y-label-v-position
	       :y-label-h-position y-label-h-position

	       :x-axis-prefix x-axis-prefix :x-axis-suffix x-axis-suffix
	       :y-axis-prefix y-axis-prefix :y-axis-suffix y-axis-suffix

	       :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
	       :line-styles line-styles :use-same-line-style use-same-line-style
	       :label-traces label-traces
	       :x-are-fns x-are-fns :y-are-fns y-are-fns
	       :axes-type axes-type :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
	       :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
	       :draw-grid draw-grid
	       :update-fixed-gap update-fixed-gap
	       :use-fixed-top-gap use-fixed-top-gap :fixed-top-gap fixed-top-gap
	       :use-fixed-bottom-gap use-fixed-bottom-gap :fixed-bottom-gap fixed-bottom-gap
	       :use-fixed-right-gap use-fixed-right-gap :fixed-right-gap fixed-right-gap
	       :use-fixed-left-gap use-fixed-left-gap :fixed-left-gap fixed-left-gap
	       :comment comment :comment-position comment-position :upper-right-comment upper-right-comment
	       :session-name session-name
	       :width width :height height :left left :top top
	       :x-inc x-inc :y-inc y-inc :y-min y-min :y-max y-max :x-min x-min :x-max x-max
	       :x-origin x-origin :y-origin y-origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-histo (list)
  (let* ((max (maximize-val-or-seq list))
	 (histo-array (make-array (list (+ 1 (round max))) :initial-element 0.0)))
    (loop for len in list do (setf (aref histo-array (round len)) (+ 1.0 (aref histo-array (round len)))))
    (plot-xy-data (list (list (list-of-nums (+ 1.0 (float (round max)))) (array-to-list histo-array))) (list "histo")
		  :x-label "" :y-label "")
    (print (/ (loop for len in list summing len) (length list)))
    (print max)))

(defun plot-histogram-data (y-data-list bin-min bin-max
			    &key
			    y-inc (y-min 0.0) y-max x-origin y-origin (x-origin-tick t) (y-origin-tick t) x-are-fns (y-are-fns t)
			    (title "Histogram Data") title-position data-type (x-label "") (y-label "")
			    x-axis-tick-skip x-axis-tick-mark-skip (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
			    x-label-v-position x-label-h-position y-label-v-position y-label-h-position
			    stipple-percent (bar-border-p t)
			    overlay accomodate-overlays prompt-for-overlay
			    axes-type	; (:STANDARD :SIMPLE :NONE)
			    x-scale-l% x-scale-t% y-scale-t%
			    (simple-axis-x-value-p t) (simple-axis-y-value-p t)
			    update-fixed-gap use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0)
			    use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
			    font comment (comment-position *default-comment-position*) (upper-right-comment "")
			    left top (width 400) (height 400)
			    win)
  ;; Y-DATA-LIST = list of the actual histogram values.
  ;; Histogram are constructed with vertical bars whose left edges are defined by the X entries in Y-DATA-LIST, and whose widths
  ;; are defined by BIN-WIDTH. If either X-AXIS-TICK-SKIP or X-AXIS-TICK-MARK-SKIP is NIL [default], then reasonable values will be
  ;; chosen automatically.

  (let ((win (or win (get-plot-window :xy data-type overlay :title title :mode :histogram :left left :top top
				      :accomodate-overlays (unless prompt-for-overlay accomodate-overlays) :prompt-for-overlay prompt-for-overlay))))
    (s-values win simple-axis-x-value-p simple-axis-y-value-p (y-axis-tick-mark-length 5) (x-axis-tick-mark-length 2))
    (cond-every
     (axes-type (s-value win :axes-type axes-type))
     (x-scale-l% (s-value win :x-scale-l% x-scale-l%))
     (x-scale-t% (s-value win :x-scale-t% x-scale-t%))
     (y-scale-t% (s-value win :y-scale-t% y-scale-t%))
     (font (s-values win font (comment-font font) (plot-axis-font font))))
    (when UPDATE-FIXED-GAP (UPDATE-FIXED-GAP win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
					     use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))
    (add-temp-comment win upper-right-comment)
    (add-local-and-global-comment comment comment-position win)
    (when y-label-h-position (s-values win y-label-h-position))
    (unless y-max (setq y-max (float (max-of-list y-data-list))))
    (unless y-inc (setq y-inc (/ y-max 5)))
    (let* ((bins (length y-data-list))
	   (bin-width (/ (- bin-max bin-min) bins))
	   (x-inc bin-width)
	   (x-min bin-min)
	   (x-max bin-max)
	   (x-list (list-of-nums (length y-data-list) x-min bin-width)))
      (s-values win bar-border-p (bin-width (float bin-width)) stipple-percent (x-label-h-position :center))
      (load-x-lists win (list x-list))
      (load-y-lists win (list y-data-list))
      (mapc (lambda (x-list)
	      (let ((bins (round (/ (- (max-of-list x-list) (min-of-list x-list)) x-inc))))
		(s-value win :x-axis-tick-skip (or x-axis-tick-skip (if (< bins 10) 0 (round (/ bins 5)))))
		(s-value win :x-axis-tick-mark-skip (or x-axis-tick-mark-skip (if (< bins 10) 0 (round (/ bins 10)))))))
	    (gv win :x-lists))
      (s-values win y-axis-tick-skip y-axis-tick-mark-skip)
      (unless (and (g-value win :overlay) (not (g-value win :accomodate-overlays)))
	(setup-plot win :width width :height height :consider-labels t
		    :x-origin x-origin :x-origin-tick x-origin-tick :y-origin y-origin :y-origin-tick y-origin-tick
		    :x-label x-label :x-label-v-position x-label-v-position :x-label-h-position x-label-h-position
		    :y-label y-label :y-label-v-position y-label-v-position :y-label-h-position y-label-h-position
		    :x-inc x-inc :x-min-spec x-min :x-max-spec x-max
		    :x-are-fns x-are-fns :y-are-fns y-are-fns
		    :y-inc y-inc :y-min-spec y-min :y-max-spec y-max))
      (let ((plot-agg (get-plot-agg win 'data-plot t))
	    (stipple-percent (or (g-value win :stipple-percent) (unless (g-value win :bar-border-p) 50)))
	    (line-style (when (g-value win :bar-border-p) thick-line))
	    (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
	(mapc (lambda (x-list y-list)
		(mapc (lambda (x y)
			(when (>= (- (gv win :x-max) bin-width) x) (add-histo-bin plot-agg x y nil nil line-style filling-style)))
		      (car x-list) (car y-list)))
	      (gv win :x-lists) (gv win :y-lists))))
    (when title-position (add-title win :position title-position))
    (plot-windows-finishing win)))

(defun histogram-sequence-values-to-array (sequence bins-or-array bin-width min)
  (let* ((array (if (arrayp bins-or-array) bins-or-array (make-array (list bins-or-array))))
	 (bins (length array)))
    (mapc (lambda (val) (let ((bin-index (floor (* bins (/ (- val min) (* bin-width bins))))))
			  (when (and (>= bin-index 0) (< bin-index bins)) (incf (aref array bin-index)))))
	  (sequence-to-list sequence))
    array))

#|
(plot-histogram '(10 10 10 20 100 ) :bins 100)
PRINTVARS: MAX 100.0, MIN 10, BINS 100, BIN-WIDTH 0.909,
#k<KR-DEBUG:PLOT-WINDOW-78280>
* (plot-histogram '(100 100 100 200 1000 ) :bins 100)
PRINTVARS: MAX 100.0, MIN 10, BINS 100, BIN-WIDTH 1,
#k<KR-DEBUG:PLOT-WINDOW-78280>
* (plot-histogram '(100 100 100 200 1000 ) :bins 100 :x-are-fns nil)
PRINTVARS: MAX 1000.0, MIN 100, BINS 100, BIN-WIDTH 10,
#k<KR-DEBUG:PLOT-WINDOW-78280>
* (plot-histogram '(100 100 100 200 1000 ) :bins 100 :x-are-fns nil)
|#



;; A histogram is constructed based on the values in SEQUENCE
(defun plot-histogram (sequence &key font comment win
		       bin-min bin-max bin-width bins
		       limit-sig-figs (x-origin :use-min) bin-at-zero
		       x-are-fns (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
		       (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
		       annotate-with-mean-and-std-dev annotate-all (title "Histogram") title-position width height
		       y-inc (y-min 0.0) y-max y-origin
		       axes-type	; (:standard :simple :none)
		       (stipple-percent 10) x-scale-l% x-scale-t% y-scale-t%
		       (simple-axis-x-value-p t) (simple-axis-y-value-p t)
		       overlay accomodate-overlays prompt-for-overlay
		       x-label-v-position x-label-h-position (x-label "") y-label-v-position y-label-h-position (y-label ""))
  ;; Range of ordinate values:
  ;; Minimum bin given by BIN-MIN or (MIN-OF-LIST LIST), with leftmost bin including minimum value
  ;; Maximum bin given by BIN-MAX or (MAX-OF-LIST LIST)
  ;; BIN-WIDTH (OR BIN-WIDTH (1- (/ (- BIN-MAX BIN-MIN) BINS)))
  ;; BINS (1+ ((- BIN-MAX BIN-MIN) BIN-WIDTH)))
  (let* ((list (flatten-sequence sequence))
	 (include-bin-min-left-border (not bin-min))
	 (data-min (if limit-sig-figs (limit-sig-figs (min-of-list list) 3 :floor) (min-of-list list)))
	 (data-max (if limit-sig-figs (limit-sig-figs (max-of-list list) 3 :ceiling) (max-of-list list)))
	 (x-are-fns (and x-are-fns (> (- data-max data-min) 1)))
	 (bin-width (if x-are-fns (round (or bin-width (/ (- data-max data-min) 10))) (or bin-width (/ (- data-max data-min) 10))))
	 (adjusted-positive-bins (abs (if bin-max (ceiling (/ bin-max bin-width)) (floor (1+ (/ data-max bin-width))))))
	 (adjusted-negative-bins (abs (floor (/ (or bin-min data-min) bin-width)))))
    (cond ((and (< data-min 0) (> data-max 0))
	   (setq bin-at-zero t
		 bins (+ adjusted-positive-bins adjusted-negative-bins)
		 bin-max (or bin-max (* bin-width adjusted-positive-bins))
		 bin-min (or bin-min (- (* bin-width adjusted-negative-bins)))))
	  ((and (>= data-min 0) (> data-max 0))
	   (setq bins (- adjusted-positive-bins adjusted-negative-bins)
		 bin-max (or bin-max (* bin-width adjusted-positive-bins))
		 bin-min (or bin-min (* bin-width adjusted-negative-bins))))
	  (t
	   (setq bins (- adjusted-negative-bins adjusted-positive-bins)
		 bin-max (or bin-max (- (* bin-width adjusted-positive-bins)))
		 bin-min (or bin-min (- (* bin-width adjusted-negative-bins))))))
    (cond ((apply '= list) (format t "The ~D element input list to PLOT-HISTOGRAM-LIST contains identical values of ~A!~%" (length list) (car list)))
	  ((= bin-width 0) (format t "Nothing to Histogram!~%"))
	  (t (let ((plot-win (plot-histogram-data (histogram-sequence-values-to-array list bins bin-width bin-min) bin-min bin-max
						  :font font :win win
						  :comment comment
						  :title title :title-position title-position :width width :height height
						  :x-label-v-position x-label-v-position :x-label-h-position x-label-h-position
						  :y-label-v-position y-label-v-position :y-label-h-position y-label-h-position
						  :x-are-fns x-are-fns :stipple-percent stipple-percent
						  :x-axis-tick-skip x-axis-tick-skip :x-axis-tick-mark-skip x-axis-tick-mark-skip
						  :y-axis-tick-skip y-axis-tick-skip :y-axis-tick-mark-skip y-axis-tick-mark-skip
						  :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
						  :axes-type axes-type :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
						  :x-label x-label :y-label y-label
						  :overlay overlay :prompt-for-overlay prompt-for-overlay :accomodate-overlays accomodate-overlays
						  :y-inc y-inc :y-min y-min :y-max y-max :y-origin y-origin
						  :x-origin (if (equal x-origin :use-min) bin-min x-origin))))
	       (when (or annotate-all annotate-with-mean-and-std-dev)
		 (add-comment plot-win
			      (format nil "~AMean ~,2f, Std-dev ~,2f"
				      (if annotate-all (format nil "N=~D, " (length list)) "") (mean list) (std-dev list)) :position :lower-right))
	       ;; (let ((*automatic-run* t)) (histogram-menu plot-win)) ; bogosity
	       plot-win)))))

(setf (fdefinition 'PLOT-HISTOGRAM-list) (fdefinition 'PLOT-HISTOGRAM)) ; Backward compatibility

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plot-polar-data (rtheta-lists
			&optional label-list &key win (overlay nil) (title "R Theta Data")
			axis-tick-mark-skip
			theta-in-degrees-p ; default assumes radians
			LIne-styles comment (comment-position *default-comment-position*)
			(label-traces t) (polar-circles-p t)
			use-same-line-style
			axes-type (simple-axis-x-value-p t) (simple-axis-y-value-p t)
			x-scale-l% x-scale-t% y-scale-t%
			connect-data-points (scatter t) (r-label "") ; cross-width
			x-label-v-position x-label-h-position
			y-label-v-position (y-label-h-position :left)
			(width 500) (height 500) r-inc r-max)
  ;; rtheta-lists ( ((r r r ...) (t t t ...)) ... )
  (plot-xy-data
   (loop for rtheta-list in rtheta-lists
	 collect
	 (loop for r in (car rtheta-list)
	       for theta in (cadr rtheta-list)
	       collect (* r (if theta-in-degrees-p (cos-degrees theta) (cos theta))) into x-list
	       collect (* r (if theta-in-degrees-p (sin-degrees theta) (sin theta))) into y-list
	       finally (return (list x-list y-list))))
   label-list
   :x-axis-tick-mark-skip axis-tick-mark-skip
   :y-axis-tick-mark-skip axis-tick-mark-skip

   :connect-data-points connect-data-points
   :win win :title title :comment comment :comment-position comment-position
   :width width :height height
   :x-label-v-position x-label-v-position
   :x-label-h-position x-label-h-position
   :y-label-v-position y-label-v-position
   :y-label-h-position y-label-h-position
   :line-styles line-styles :use-same-line-style use-same-line-style
   :label-traces label-traces
   :axes-type axes-type :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
   :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
   :overlay overlay
   :polar t :polar-circles-p polar-circles-p :connect-data-points t :connect-ends t :scatter scatter
   :y-label r-label
   :y-max (or r-max (max-of-list (mapcar #'(lambda (x) (max-of-list (car x))) rtheta-lists)))
   :y-inc r-inc :x-inc r-inc))

(defun plot-polar-simple-array (rtheta-lists label-list
				&key win (overlay nil)(title "R Theta Data")
				x-label-v-position x-label-h-position
				y-label-v-position (y-label-h-position :left)
				(scatter nil) (r-label "") ; cross-width
				(polar-circles-p t)
				(width 500) (height 500) r-inc r-max)
  (plot-xy-data
   (loop for rtheta-list in rtheta-lists
	 collect (loop for theta in (cadr rtheta-list)
		       for r in (car rtheta-list)
		       collect 0.0 into x-list
		       collect (* r (cos-degrees theta)) into x-list
		       collect 0.0 into y-list
		       collect (* r (sin-degrees theta)) into y-list
		       finally (return (list x-list y-list))))
   label-list
   :win win :title title :width width :height height
   :x-label-v-position x-label-v-position
   :x-label-h-position x-label-h-position
   :y-label-v-position y-label-v-position
   :y-label-h-position y-label-h-position

   :overlay overlay
   :polar-circles-p polar-circles-p :polar t :connect-data-points t :connect-ends t :scatter scatter
   :y-label r-label
   :y-max (or r-max (max-of-list (mapcar #'(lambda (x) (max-of-list (car x))) rtheta-lists))) :y-inc r-inc :x-inc r-inc ))

(defun plot-polar-vectors (r-theta-lists label-list
			   &key win title
			   x-label-v-position x-label-h-position
			   y-label-v-position (y-label-h-position :left)
			   (polar-circles-p t) (width 350) (height 350) overlay)
  (let ((new-r-theta-lists
	 (loop for r-theta-list in r-theta-lists
	       collect (list (loop for r in (car r-theta-list)
				   collect r
				   collect 0.0)
			     (loop for theta in (cadr r-theta-list)
				   collect theta
				   collect theta)))))
    (plot-polar-data new-r-theta-lists label-list :win win :title title :width width :height height
		     :x-label-v-position x-label-v-position
		     :x-label-h-position x-label-h-position
		     :y-label-v-position y-label-v-position
		     :y-label-h-position y-label-h-position
		     :overlay overlay :polar-circles-p polar-circles-p :line-styles very-THICK-COLOR)))

(defun add-trace (new-trace new-label &optional win-or-title time-base)
  "This function adds NEW-TRACE with reference NEW-LABEL to an existing :STANDARD-PLOT plot window, using the time base of the window unless a
TIME-BASE is supplied. WIN-OR-TITLE can be the (string) title of an existing window, a window, or NIL. If NIL, then a menu is provided. "
  (let ((win (typecase win-or-title
	       (string (loop for win in (standard-plot-windows)
			     when (string= win-or-title (gv win :title)) return win))
	       (schema (when (eq (gv win-or-title :mode) :standard-plot) win-or-title))
	       (t (WINDOW-SELECTION-MENU "Pick One Window to Add Trace" (standard-plot-windows) nil t))))
	(time-sequence (typecase time-base
			 (number (list-of-nums (length new-trace) 0.0 time-base))
			 (sequence (sequence-to-list time-base)))))
    (unless win (sim-error (format nil "~A does not reference a window!" win-or-title)))
    (plot-timed-data (cons new-trace (car (gv win :y-lists))) (cons new-label (gv win :label-list))
		     (no-nils (cons time-sequence (plot-window-top-x-lists win)))
		     :win win)))

(defun describe-plot-win (&optional (win (let ((*automatic-run* nil)) (win-menu "Select Plot Windows to Describe" (standard-plot-windows)))))
  (atomize-list
   (loop for win in (coerce-to-list win) do
	 (let ((x-list (caar (gv win :x-lists)))
	       (y-list (caar (gv win :y-lists))))
	   (format t "Plot window ~A: Left ~D, Top ~D, Width ~D, Height ~D~%"
		   (gv win :title) (gv win :left) (gv win :top) (gv win :width) (gv win :height))
	   (format t "   Length of X data ~D, max ~A min ~A~%" (length x-list) (max-of-list x-list) (min-of-list x-list))
	   (format t "   Length of Y data ~D, may ~A min ~A~%" (length y-list) (max-of-list y-list) (min-of-list y-list))
	   (format t "   X origin ~A, Y origin ~A~%" (gv win :x-origin) (gv win :y-origin))
	   (format t "   X data min/max ~A/~A, Y min/max ~A/~A~%"
		   (gv win :x-data-min) (gv win :x-data-max) (gv win :y-data-min) (gv win :y-data-max)))
	 collect win)))

;; Functions for saving plots to lisp files.
(defun dump-plot-to-lisp-file (&optional wins filename (directory *plot-code-directory*) force)
  "Given optional WINS \(list or atom\), FILENAME \(string\), and DIRECTORY [default *PLOT-CODE-DIRECTORY*] write lisp files that recreate a plot
window. Menus prompt for missing arguments. Already existing files with same name will be overwritten. FORCE disables all prompts except initial
window menu if needed."
  (loop for win in (coerce-to-list (or wins (win-menu "Select Plot Windows to Dump" (standard-plot-windows))))
	do (let* ((*automatic-run* force)
		  (dummy1 (or filename (format nil "~A.lisp" (make-nice-filename (strip-displayed-host-name-from-title (gv win :title))))))
		  (dummy2 directory)
		  dummy3)
	     (choose-variable-values
	      `((dummy1 "Filename" :string)
		(dummy2 "Directory" :string)
		(dummy3 "CANCEL" :boolean))
	      :title (format nil "Write Plot Window Lisp File of ~A" (gv win :title)))
	     (unless dummy3
	       (let* ((pathname-directory (fixup-pathname-directory dummy2))
		      (filename (format nil "~A~A" pathname-directory dummy1)))
		 (setq *plot-code-directory* pathname-directory)
		 (when (write-file-overwrite-authorization filename)
		   (unix-mkdir pathname-directory)
		   (when (probe-file pathname-directory)
		     (with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
				       (write-window-plot-form win))
		     (format t ";; File ~a written~%" filename))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plot Data Extraction and Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PLOT-WINDOW-OVERLAYS (win) (length (gv win :y-lists)))

(defun add-trace-analysis-to-plot-menu (&optional win)
  (let* ((windows (coerce-to-list (or win (win-menu))))
	 (dummy1 nil)			; append-to-existing-comment
	 (dummy2 nil)			; return-strings
	 (dummy3 0)			; trace
	 (dummy4 0.0)			; y-base
	 (dummy5 :upper-right)		; position
	 (dummy6 0)			; overlay-index
	 dummy7)
    (flatten-no-nils-list
     (loop for win in windows do
	   (choose-variable-values
	    `((dummy1 "Append to existing comment" :boolean)
	      (dummy2 "Return strings" :boolean)
	      (dummy3 "Trace" :integer)
	      (dummy7 "All traces (=> ignore above spec)" :boolean)
	      (dummy4 "Y base for integral" :number)
	      (dummy5 "Comment position" :choose ,*comment-positions*)
	      (dummy6 "Overlay index" :integer))
	    :label (format nil "Analysis of Plot Traces") :text (gv win :title))
	   collect (add-trace-analysis-to-plot win :append-to-existing-comment dummy1
					       :return-strings dummy2
					       :trace (if dummy7 :all dummy3)
					       :y-base dummy4
					       :position dummy5
					       :overlay-index dummy6)))))

(defun add-trace-analysis-to-plot (&optional win &key append-to-existing-comment return-strings trace (y-base 0.0) (position :upper-right) (overlay-index 0))
  "Adds analysis result of traces in plotting WIN [if NIL then a menu is given] at POSITION [default :UPPER-RIGHT], including integral relative to
Y-BASE [default 0], maximum and minimum. Chosen traces are determined by the TRACE [default NIL] and OVERLAY-INDEX [default 0] arguments, as described
for the function EXTRACT-PLOT-WINDOW-DATA. If RETURN-STRINGS is T then a list of all generated strings is returned."
  (loop for win in (or (coerce-to-list win) (win-menu)) collect
	(let* ((overlays (case overlay-index
			   (:all (list-of-nums (plot-window-overlays win) 0 1))
			   (t (coerce-to-list overlay-index))))
	       (strings
		(concatenate-string-list
		 (loop for overlay in overlays nconcing
		       (multiple-value-bind (data labels)
			   (extract-plot-window-data win trace overlay)
			 (loop for xy-lists in data
			       for label in labels
			       when data
			       collect (plot-trace-analysis-string (cadr xy-lists)
								   (when (> (length (gv win :label-list)) 1) label)
								   (car xy-lists) y-base))))
		 :lf-count 1)))
	  (add-comment win strings :append-to-old-comment append-to-existing-comment :position position)
	  strings) into out
	finally (when return-strings (return out))))

(defun plot-trace-analysis-string (data-list label time-base y-base)
  (format nil "~AInt ~,2e~A, Max ~,2e, Min ~,2e"
	  (if label (format nil "~A: " label) "")
	  (integrate-x-y data-list time-base :y-base y-base)
	  (if (and y-base (/= y-base 0)) (format nil "[~,2e]" y-base) "")
	  (max-of-list data-list)
	  (min-of-list data-list)))

(defun integrate-plot-window-data (&key win trace x-max x-min (y-base 0.0) average)
  (values-list
   (loop for xy-lists in (extract-plot-window-data win trace)
	 collect (integrate-x-y (cadr xy-lists) (car xy-lists) :average average :x-max x-max :x-min x-min :y-base y-base))))

(defun extract-plot-window-y-list (win trace overlay-index) (nth trace (nth overlay-index (gv win :y-lists))))

(defun extract-plot-window-x-list (win trace overlay-index)
  (let ((y-list (extract-plot-window-y-list win trace overlay-index)))
    (when y-list
      (let ((overlay-x-lists (nth overlay-index (gv win :x-lists))))
	(if (numberp overlay-x-lists)
	  (list-of-nums (length y-list) (gv win :delta-t-start) overlay-x-lists)
	  (nth (min (1- (length overlay-x-lists)) trace) overlay-x-lists))))))

(defun extract-plot-window-data (&optional win (trace :menu) (overlay-index 0))
  "Extract one or more plot data lists from WIN. Prompts for non-specfied WIN. For overlayed plots, retrieves the overlay according to OVERLAY-INDEX,
default 0, referenced from the last overlay. Returns as values DATA and LABELS. DATA is of the form:

        (((x-list) (y-list)) ... ((x-list) (y-list)))

If TRACE is nil then the first trace is returned. Otherwise, if TRACE is an integer or a list of integers, then the traces corresponding to those
numbers [starting from 1] are returned.  If TRACE is :ALL, then all data lists are included. If TRACE is :MENU, the default, then a menu for the
traces is given."
  (let ((win (or win (win-menu "Select Plot Window for Data" (standard-plot-windows) nil t))))
    (when win
      (let* ((sequence-numbers
	      (case trace
		(:all (list-of-nums (length (car (gv win :y-lists))) 0 1))
		(:menu (coerce-to-list
			(choose-list-values-from-keys
			 (loop for y-seq in (car (gv win :y-lists)) for count from 0
			       collect (list (or (when (> (length (nth count (gv win :label-list))) 0)
						   (nth count (gv win :label-list)))
						 (format nil "Trace ~A" (+ count 1)))
					     (+ 0 count)))
			 nil		; :only-one-choice trace
			 :label (format nil "Choose Trace Data From ~A" (gv win :title)))))
		(t (if trace (coerce-to-list trace) '(0)))))
	     (labels (loop for trace in sequence-numbers
			   collect (nth trace (gv win :label-list))))
	     (y-data (no-nils (loop for trace in sequence-numbers collect (extract-plot-window-y-list win trace overlay-index))))
	     (x-data
	      (no-nils
	       (loop for trace in sequence-numbers
		     collect
		     (let ((x (extract-plot-window-x-list win trace overlay-index)))
		       (typecase x
			 (cons x)
			 (number (list-of-nums (length (extract-plot-window-y-list win trace overlay-index)) 0 x))))))))
	(values (loop for x in x-data for y in y-data
		      collect (list x y))
		(when (>= (length labels) (length x-data))
		  (loop for x in x-data
			for label in labels
			collect label)))))))

(defun density-plot (array
		     &key win (title "2D Plot")
					; (scale 0.8)
		     (overlay nil) (element-aspect-ratio 1.0)
		     (color nil) (x-inc 1) (y-inc 1) (width 400) (height 400)
		     border
		     (vertical-border 50)
		     (side-border 50)
		     left-border
		     right-border
		     (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
		     (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
		     (x-axis-p t) (y-axis-p t)
		     x-are-fns y-are-fns
		     x-min x-max y-min y-max (x-label "") (y-label "") z-max z-min
		     invert)
  "BORDER, WIDTH, HEIGHT are in pixels."
  (declare		   ; (optimize (safety 1) (speed 3) (space 0))
   (type (array single-float (* *)) array)
   (fixnum width height			; x-inc y-inc
	   ))
  (let ((overlay (and overlay (or win (FIND-2dPLOT-WINDOW title))))
	(vertical-border (round (or border vertical-border)))
	(side-border-new (round
			  (if (and left-border right-border)
			      (/ (+ left-border right-border) 2)
			      (or border side-border))))
	(element-aspect-ratio (s-flt element-aspect-ratio)))

    (unless win (setq win (get-plot-window :2dplot nil overlay :title title :mode :2dplot :width width :height height :preserve-win-dims t)))
    (when win
      (s-value win :x-axis-p x-axis-p)
      (s-value win :y-axis-p y-axis-p)
      (s-value win :x-axis-tick-skip x-axis-tick-skip)
      (s-value win :x-axis-tick-mark-skip x-axis-tick-mark-skip)
      (s-value win :y-axis-tick-skip y-axis-tick-skip)
      (s-value win :y-axis-tick-mark-skip y-axis-tick-mark-skip)
      (s-value win :x-are-fns x-are-fns) (s-value win :y-are-fns y-are-fns)
      (let* ((width (fn-gv win :width))
	     (height (fn-gv win :height))
	     (array-width-x (array-dimension array 0))
	     (array-width-y (array-dimension array 1))
	     (z-max (s-flt (or z-max (2d-array-max array))))
	     (z-min (s-flt (or z-min (2d-array-min array))))
	     (z-amp (the sf (- z-max z-min)))
	     (rect-width-ref (/ (- width (* 2 side-border-new)) (the fn array-width-x)))
	     (rect-height-ref (/ (- height (* 2 vertical-border)) (the fn array-width-y)))
	     (rect-width (round (if (> element-aspect-ratio 1)
				    (/ rect-width-ref element-aspect-ratio)
				    rect-width-ref)))
	     (rect-height (round (if (> element-aspect-ratio 1)
				     rect-height-ref
				     (/ rect-height-ref element-aspect-ratio))))
	     (left-border (round (or left-border side-border-new)))
	     (top-border vertical-border) ; In pixels
	     (x-min (or x-min 0))
	     (y-min (or y-min 0))
	     (x-max (or x-max
			(* x-inc array-width-x)))
	     (y-max (or y-max
			(* y-inc array-width-y))))
	(declare (fixnum rect-width rect-height left-border top-border array-width-x array-width-y))
	(when (> z-amp 0)
	  (s-value win :y-origin-tick t)
	  (resurrect-opal-win win :visible t :update t)
	  (remove-virtual-aggs win)
	  (s-value win :width width) (s-value win :height height)
	  (resurrect-opal-win win)
	  (add-density-plot-axes win x-min x-max y-min y-max x-label y-label
				 array-width-x array-width-y
				 left-border rect-width
				 top-border rect-height)
	  (add-2d-density-array win array array-width-x array-width-y left-border top-border
				rect-width rect-height z-min z-amp color invert)
	  win)))))

(defun plot-2d-array (array &key win (title "2-D Data Plot")
					; (scale 0.8)
			    (overlay nil)
			    (x-inc 1) (y-inc 1) (width 400)(height 400)  (border 50)
			    (element-aspect-ratio 1) comment)
  (let ((win (density-plot array :win win :title title ; :scale scale
			   :overlay overlay
			   :x-inc x-inc :y-inc y-inc :width width :height height :border border
			   :element-aspect-ratio element-aspect-ratio)))
    (add-local-and-global-comment comment nil win)))

(defun density-histo-plot (x-data x-incs y-data y-incs
			   &key
			   (dynamic-range 100)
			   (increment 1)
			   (white-is-maximum-p t)
			   x-are-fns y-are-fns
			   (element-aspect-ratio 1)
			   (border 75)
			   print-out-max-mins
			   (title "Histo Plot")
			   z-max x-min x-max y-min y-max
			   (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
			   (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
			   (width 400) (height 400)
			   (x-label "") (y-label ""))
  (density-plot (make-2d-histo-array x-data y-data x-incs y-incs
				     :x-min x-min :x-max x-max :y-min y-min :y-max y-max
				     :increment increment :print-out-max-mins print-out-max-mins)
		:title title :border border :width width :height height
		:invert (not white-is-maximum-p)
		:z-min 0 :z-max z-max :x-min x-min :x-max x-max :y-min y-min :y-max y-max
		:element-aspect-ratio element-aspect-ratio
		:x-axis-tick-skip x-axis-tick-skip :x-axis-tick-mark-skip x-axis-tick-mark-skip
		:y-axis-tick-skip y-axis-tick-skip :y-axis-tick-mark-skip y-axis-tick-mark-skip
		:x-are-fns x-are-fns :y-are-fns y-are-fns
		:x-label x-label :y-label y-label))

(export '(scatter-statistics-comment
	  add-global-plot-comment
	  describe-plot-win
	  NUMBER-OF-OVERLAYS
	  ADD-TRACE-ANALYSIS-TO-PLOT
	  PLOT-TRACE-ANALYSIS-STRING
	  plot-xy-data plot-timed-data
	  PLOT-POINTS
	  plot-scatter
	  BASIC-HISTO
	  plot-histogram-data
	  plot-histogram
	  plot-histogram-list
	  plot-polar-data
	  plot-polar-vectors
	  plot-polar-simple-array
	  ADD-TRACE
	  dump-plot-to-lisp-file
	  extract-plot-window-data
	  extract-plot-window-y-list
	  extract-plot-window-x-list
	  grab-and-store-plot-data
	  integrate-plot-window-data
	  density-plot
	  density-histo-plot
	  plot-2d-array))

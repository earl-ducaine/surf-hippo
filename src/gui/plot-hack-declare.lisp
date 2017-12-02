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

;; GUI Source file: plot-hack-declare.lisp

(in-package "SON-OF-PLOT-HACK")


(defvar *x-label-v-positions* '(:below :above))
(defvar *x-label-h-positions* '(:left :center :right))
(defvar *y-label-h-positions* '(:left :right))
(defvar *y-label-v-positions* '(:two-thirds-up :upper-right :center-right :lower-right :lower-left :center-left :upper-left))

(defvar *global-plot-comment* nil "When set to a string, this comment is added to the window produced by PLOT-XY-DATA, PLOT-TIMED-DATA, PLOT-POINTS,
PLOT-SCATTER, PLOT-HISTOGRAM, PLOT-POLAR-DATA, PLOT-POLAR-SIMPLE-ARRAY and PLOT-POLAR-VECTORS, at the position given by
*GLOBAL-PLOT-COMMENT-POSITION* [default *DEFAULT-COMMENT-POSITION*]. If this position is the same as the comment position argument of these plotting
functions, then any comment supplied to the plotting function is added to the global plot comment.")

(defvar *global-plot-comment-position* nil)
(defvar *default-plot-grid-p* nil "When T functions such as PLOT-TIMED-DATA will draw a grid by default.")

(export '(*DEFAULT-PLOT-GRID-P* *global-plot-comment* *global-plot-comment-position*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *force-plot-x-fixnums* nil "When T, plots will be forced to display X values as fixnums.")
(defvar *force-plot-y-fixnums* nil "When T, plots will be forced to display Y values as fixnums.")
(defvar *label-plot-traces* t "Default enable for trace labels in plots.")
(defvar *gap-btwn-x-label-and-tick-marks* 10) ; pixels

(defvar *plot-axis-y-axis-tick-label-gap* 4)
(defvar *plot-axis-x-axis-tick-label-gap* 4)

(defvar *x-axis-tick-mark-length* -5 "In pixels. If negative, X axis tick marks will point away from tick labels.")
(defvar *y-axis-tick-mark-length* -5 "In pixels. If negative, Y axis tick marks will point away from tick labels.")

(defvar *trace-keys-top* 10)
(defvar *trace-keys-left* 10)
(defvar *trace-keys-middle* 25)
(defvar *trace-keys-right* 40)
;; (defvar *trace-labels-left* 70)
(defvar *trace-key-gap* 0)		; A gap in pixels between each trace label key. Not precisely implemented
(proclaim '(fixnum *trace-key-gap*
	    *trace-keys-top* *trace-keys-middle* *trace-keys-left* *trace-keys-right* ;; *trace-labels-left*
	    ))

(defvar *accomodate-all-overlays* t) ;; shadow *accomodate-overlays*
(defvar *accomodate-overlays* t "When T the layout of existing windows adapt to any new overlaid data.")
(defvar *preserve-plot-layout* nil "When T new plots to existing windows will not change the layout.")
(defvar *overlay-all-plots* nil) ;; shadow *overlay-plots*
(defvar *overlay-plots* nil "When T encourage overlays when plotting data to existing windows.")

(defvar *default-x-label* "")
(defvar *default-y-label* "")

(defvar *plot-scale-bar-horizontal-space* 20)
(defvar *plot-scale-bar-vertical-space* 45)

(defvar *x-plot-left-gap* 65)		; The distance in pixels of the scaled rectangle from the left of the window. (not used??)
(defvar *x-plot-right-gap* 50)		; The distance in pixels of the scaled rectangle from the right of the window.
(proclaim '(fixnum *x-plot-left-gap* *x-plot-right-gap*))

(defvar *y-plot-bottom-gap* 10)		; The distance in pixels of the scaled rectangle off the bottom of the window. Note that
					; the top gap is variable, depending on the number of trace labels required
					; (:label-height). 
(proclaim '(fixnum *y-plot-bottom-gap*))


(defvar *x-plot-fudge* 0.05)
(defvar *y-plot-fudge* 0.05)

(defvar *x-plot-win-minimum-margin* -20)
(defvar *x-plot-win-maximum-margin* 20)
(defvar *y-plot-win-minimum-margin* -20)
(defvar *y-plot-win-maximum-margin* 20)

(defvar *create-new-plot-windows* nil)	; Create a new set of plot windows.
(defvar *create-new-plot-window-types* nil)

(defvar *GET-PLOT-POINT-LIST-margin* 100)

;; For waterfalls
(defvar *default-x-plot-top-gap-extra-waterfall* 20 "In pixels")
(defvar *default-y-plot-top-gap-extra-waterfall* 20 "In pixels")
(defvar *x-trace-offset* 0.0 "In data units") ; used for automatic waterfall setup
(defvar *x-plot-left-gap-waterfall* 30 "In pixels")
(defvar *auto-waterfall-x-trace-overlap* 0.0)
(defvar *auto-waterfall-y-trace-overlap* 0.0)
(defvar *waterfall-fixed-y-max* nil)
(defvar *waterfall-fixed-y-min* nil)
  
(defvar *simple-axis-x* nil)
(defvar *simple-axis-y* nil)

(defvar *plot-line-style-family* :thin-color "Default line styles used for plotting. Possible values are given by the members of *PLOT-LINE-STYLE-FAMILIES*.")
(defvar *plot-line-style-families* '(:Thick-Color :Thin-Color :Thick-dashed :dashed :double-VARYING-WIDTH :VARYING-WIDTH))

;; (defvar *default-plot-line-style* :thin-color)

(defvar *connect-data-points t)		; Connect the dots in the data plots.
(defvar *default-scatter-size* 7)	; pixels
(defvar *default-scatter-symbol* :dot)
(defvar *default-plot-timed-data-scatter-enable* nil) ; Gives the default value for the :SCATTER argument of PLOT-TIMED-DATA.

(defvar *plot-axis-font* *window-default-font* ;; (OPAL:GET-STANDARD-FONT :fixed :roman :medium)
  ;; Default font for plot axes. Set this to font returned by OPAL:GET-STANDARD-FONT. See also SET-*PLOT-AXIS-FONT*-MENU.
)

(defvar *use-tidy-number-format-for-plots* t)

(defvar *default-grid-line-style* (create-instance nil (create-instance nil dashed-line (:constant nil)) (:dash-pattern (list 1 8))))
(defvar *default-axis-line-style* opal:line-1)

(defvar *plot-data-a-bit-more-than-the-x-or-y-coeff* 0.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *overlay-plot-default-option* nil)
;; `(:overlay_and_accomodate_new_data :overlay_and_retain_coordinates :erase_and_retain_coordinates :make_new_window :erase_old_data :cancel_plot)




(export '(*overlay-plot-default-option*))

(defvar *plotted-constant-value-threshold-for-shifting-origin* 0.25) ; For shifting the origin for flat data near 0.

(defvar *debug-plot-border-point* nil)

(defvar *refresh-plot-default-grid* nil)

(defvar *write-window-plot-form-w-explicit-x-and-y-lists* t)

(defvar *grab-and-store-plot-data-include-comments* '(:window_title :window_comments))
(defvar *grab-and-store-plot-data-output-format* nil)

(defvar *enable-frob-win-for-virtual-agg* t)
(export '*enable-frob-win-for-virtual-agg*)

(proclaim '(type fixnum *x-plot-win-minimum-margin* *x-plot-win-maximum-margin*
	    *x-axis-tick-mark-length* *y-axis-tick-mark-length* *y-plot-win-minimum-margin* *y-plot-win-maximum-margin*))

(export '(*REFRESH-PLOT-DEFAULT-GRID*
	  *plot-data-a-bit-more-than-the-x-or-y-coeff*
	  *FORCE-PLOT-Y-FIXNUMS* *FORCE-PLOT-X-FIXNUMS*
	  ;;  *default-plot-line-style*
	  *label-plot-traces*
	  *x-axis-tick-mark-length*
	  *y-axis-tick-mark-length*
	  *GRID-STYLE*
	  *default-scatter-size* *default-scatter-symbol* *default-plot-timed-data-scatter-enable*
	  *default-x-label* *default-y-label*
	  *create-new-plot-windows* *create-new-plot-window-types*
	  *connect-data-points *plot-line-style-family* *PLOT-LINE-STYLE-FAMILIES* *plot-axis-font*
	  *accomodate-all-overlays* *accomodate-overlays* *overlay-all-plots* *overlay-plots* *preserve-plot-layout*
	  
	  *x-trace-offset* *x-plot-right-gap* *X-PLOT-LEFT-GAP-WATERFALL*
	  *auto-waterfall-x-trace-overlap* *default-x-plot-top-gap-extra-waterfall*
	  *auto-waterfall-y-trace-overlap* *default-y-plot-top-gap-extra-waterfall*
	  *simple-axis-x* *simple-axis-y*
	  *WATERFALL-FIXED-Y-MIN* *WATERFALL-FIXED-Y-Max*
	  *USE-TIDY-NUMBER-FORMAT-FOR-PLOTS*
	  *x-plot-win-minimum-margin* *x-plot-win-maximum-margin*
	  *y-plot-win-minimum-margin* *y-plot-win-maximum-margin*))
	  

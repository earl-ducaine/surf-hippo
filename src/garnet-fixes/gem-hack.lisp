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

(in-package "GEM")

;; (defun x-draw-line (window x1 y1 x2 y2 function line-style &optional drawable)
;; ;;  (declare (optimize (safety 0) (speed 3) (space 0)))
;;   (let* ((display-info (g-value window :display-info))
;; 	 (root-window  (opal::display-info-root-window display-info)))
;;     ;; Provide the actual drawable of the window if you want to bypass drawing
;;     ;; into the buffer.  This is used by the gesture-interactor to draw lines
;;     ;; directly into the window, not the buffer.
;;     (unless drawable
;;       (setf drawable (the-drawable window)))
;;     (setf function (get function :x-draw-function))
;;     (if line-style
;; 	(let* ((line-style-gc (opal::display-info-line-style-gc display-info))
;; 	       (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
;; 	  (set-line-style line-style line-style-gc xlib-gc-line
;; 			  root-window function)
;; 	  (xlib:draw-line drawable xlib-gc-line x1 y1 x2 y2)))))


;;(proclaim '(inline x-draw-line-fast))
(defun x-draw-line-fast (window x1 y1 x2 y2
				function
				line-style
				display-info
				root-window
				line-style-gc
				xlib-gc-line
				&optional drawable drawable-display)
  (declare ;; (optimize (safety 0) (speed 3) (space 0))
	   (ignore window display-info))
  (set-line-style line-style line-style-gc xlib-gc-line root-window function)
  (xlib::draw-line-fast drawable xlib-gc-line x1 y1 x2 y2
			nil
			drawable-display)

  nil)


;;(proclaim '(inline x-draw-lines-fast))
;; Used for virtual polylines.
(defun x-draw-lines-fast (window point-list function line-style fill-style)
;;    (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (if fill-style
	(let* ((filling-style-gc (opal::display-info-line-style-gc display-info))
	       (xlib-gc-filling (opal::opal-gc-gcontext filling-style-gc)))
	  (set-filling-style
	   fill-style filling-style-gc xlib-gc-filling root-window function)
	  (xlib:draw-lines drawable xlib-gc-filling point-list :fill-p T)))
    (if line-style
	(let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	       (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	  (set-line-style line-style line-style-gc xlib-gc-line root-window function)
	  (xlib:draw-lines drawable xlib-gc-line point-list)))))


;; Dec 14, 2003
;;
;; Avoid bugs when zooming too far on plots....

#|
(defun x-clear-area (window &optional (x 0) (y 0) width height clear-buffer-p)
;  (break)
  (if clear-buffer-p
    ;; Clear the window's buffer
    (let* ((gc (g-value window :buffer-gcontext))
	   (buffer (g-value window :buffer))
	   (background (xlib:gcontext-background gc))
	   ;; Avoid too large width or height....
	   (width (when width (min width (xlib:drawable-width buffer))))
	   (height (when height (min height (xlib:drawable-height buffer)))))
      ;; (sh::printvars width height) (break)
      (xlib:with-gcontext (gc :function opal::*copy* :foreground background)
	(if x
	  ;; clear only a region
	  (xlib:draw-rectangle buffer gc x y width height t)
	  ;; clear the entire buffer
	  (xlib:draw-rectangle buffer gc 0 0
			       (xlib:drawable-width buffer)
			       (xlib:drawable-height buffer) t))))
    ;; Clear the window itself
    (xlib:clear-area (the-drawable window)
		     :x x :y y :width width :height height
		     :exposures-p NIL)))
|#

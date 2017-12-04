;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

;; The Surf-Hippo Neuron Simulator System
;;
;; This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
;; Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
;; Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
;;
;; Permission to use, copy, modify, and distribute this software and its documentation for any purpose
;; and without fee is hereby granted, provided that this software is cited in derived published work,
;; and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
;; Project makes no representations about the suitability of this software for any purpose. It is
;; provided "as is" without express or implied warranty.
;;
;; If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
;; on the mailing list.
;;
;; Copyright (c) 1989 - 2003, Lyle J. Graham


(in-package :windows-hack)

(defun show-image (image &key (win nil window-supplied) title left top height
			   width (update t) crop image-left image-top
			   (horizontal-side-border 0) (vertical-side-border 0))
  "Returns a window, default WIN, that shows the bitmap IMAGE
   image. IMAGE can be either a Garnet bitmap schema or a filename. If
   IMAGE is nil then browse. Image size is adjusted by
   HORIZONTAL-SIDE-BORDER and VERTICAL-SIDE-BORDER. Window dimensions
   are given by HEIGHT or WIDTH when non-nil, or by the adjusted image
   size if either CROP is non-nil or WIN is nil. Image placement is
   given by IMAGE-LEFT and IMAGE-TOP when either is non-nil, otherwise
   is centered in window. IMAGE-LEFT and IMAGE-TOP can either be
   specified in pixel coordinates, or by the keywords :LEFT or :RIGHT,
   or :TOP or :BOTTOM, respectively, thus justifying the image
   position with respect to the window as indicated. Place window at
   LEFT and TOP position in the screen, if included. Window TITLE
   defaults to IMAGE, unless a supplied WIN already has a
   non-nil :TITLE."
  (let* ((path nil)
	 (bitmap-object
	  (cond ((schema-p image) image)
		(t (setq path (if image
				  (pathname image)
				  (file-browser "Find Image File"
						"/home"
						'("xbm"))))
		   (create-instance nil opal:bitmap
		     (:filling-style (create-instance nil opal:filling-style (:fill-style :stippled)))
		     (:image (opal:read-image (namestring path)))))))
	 (image-name (or (gv bitmap-object :name)
			 (and path (file-namestring path))
			 "Unamed Image")))
    (when bitmap-object
      (let* ((win (or win
		      (create-instance nil inter:interactor-window
			(:title (string (or title image-name)))
			(:aggregate (create-instance nil opal::aggregate)))))
	     (image-width
	      (+ (gv bitmap-object :width)
		 (if HORIZONTAL-SIDE-BORDER (* 2 HORIZONTAL-SIDE-BORDER) 0)))
	     (image-height
	      (+ (gv bitmap-object :height)
		 (if VERTICAL-SIDE-BORDER (* 2 VERTICAL-SIDE-BORDER) 0))))
	(when (or width crop (not window-supplied))
	  (s-value win :width (or width image-width)))
	(when (or height crop (not window-supplied))
	  (s-value win :height (or height image-height)))
	(s-value bitmap-object :image-left image-left)
	(s-value bitmap-object :image-top image-top)
	(s-value bitmap-object
		 :left (o-formula
			(round
			 (case (gvl :image-left)
			   (:left 0)
			   (:right (- (gvl :window :width) (gvl :width)))
			   (t (or (gvl :image-left)
				  (- (/ (gvl :window :width) 2)
				     (/ (gvl :width) 2))))))))
	(s-value bitmap-object
		 :top (o-formula
		       (round
			(case (gvl :image-top)
			  (:top 0)
			  (:bottom (- (gvl :window :height)
				      (+ (or vertical-side-border 0)
					 (gvl :height))))
			  (t (or (gvl :IMAGE-top)
				 (- (/ (gvl :window :height) 2)
				    (/ (gvl :height) 2))))))))
	(opal:add-component (or (gv win :aggregate)
				win)
			    bitmap-object :where :back)
	(s-value win :bitmap bitmap-object)
	(s-value win :visible t)
	(s-value win :left (round (case left
				    (:right (- *screen-width* (gv win :width)))
				    (:left 0)
				    (:center (/ (- *screen-width* (gv win :width)) 2))
				    (t (or left (if window-supplied (gv win :left) 0))))))
	(s-value win :top (round (case top
				   (:top 0)
				   (:bottom (- *screen-height* (gv win :height)))
				   (:center (/ (- *screen-height* (gv win :height)) 2))
				   (t (or top (if window-supplied (gv win :top) 0))))))
	(when update (resurrect-opal-win win :raise t :update t))
	win))))

(export '(show-image))


#|
(let ((left 0) (top 400))
  (loop for file in  (directory "/giga/garnet/garnet-3.0/lib/pixmaps/")
      do (setq left (+ 30 left (gv (show-image (create-instance nil opal::pixmap
		 (:image (opal::read-xpm-file  file)))
		     :left left :top top
		     :title (namestring file)) :width)))
      when (> left 800) do (setq top (+ top 200) left 0)))

|#

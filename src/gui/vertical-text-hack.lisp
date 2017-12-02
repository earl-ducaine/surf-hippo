;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

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

;; GUI Source file: windows-hack.lisp

(IN-PACKAGE "WINDOWS-HACK")

#|
From: Chisheng Huang <cph@chi-square-works.com>
To: lyle graham <lyle@cogni.iaf.cnrs-gif.fr>
Subject: Re: Vertical text hacks for CLX
Date: Saturday, March 16, 2002 11:52 AM

lyle graham <lyle@cogni.iaf.cnrs-gif.fr> writes:

> I would like to implement vertical text (within Garnet), but according
> to the CLX manual, CLX "does not provide any direct support for vertical
> text". Has anyone tackled this, or can supply some useful pointers?
> 
> Thanks in advance,
> 
> Lyle
> 


This was what I hacked many years ago as a proof of concept:
|#


#|
(defun copy-matrix (matrix)
  (let* ((dimensions (array-dimensions matrix))
         (result (make-array dimensions)))
    (dotimes (i (first dimensions))
      (dotimes (j (second dimensions))
        (setf (aref result i j)
          (aref matrix i j))))
    result))

(defun col (matrix j)
  (loop for i from 0 below (first (array-dimensions matrix))
      collect (aref matrix i j)))

(defun row (matrix i)
  (loop for j from 0 below (second (array-dimensions matrix))
      collect (aref matrix i j)))
|#

(defun rotate-matrix (matrix angle)
  (declare (type xlib:Pixarray matrix)
           (Fixnum angle))
  (unless (= (mod angle 90) 0)
    (error "~&angle has to be a multiple of 90; you had angle = ~A.~%" angle))
  (let* ((rad (* pi (/ angle 180)))
         (sin-rad (truncate (sin rad)))
         (cos-rad (truncate (cos rad)))
         (dimensions (array-dimensions matrix))
         (result (make-array (rotated-dimensions dimensions angle) 
                             :element-type (array-element-type matrix)
                             :initial-element (aref matrix 0 0))))
    (declare (Fixnum sin-rad cos-rad)
             (List dimensions)
             (type xlib:Pixarray result))
    (multiple-value-bind
        (delta-i delta-j)
        (calculate-translations dimensions sin-rad cos-rad)
      (declare (Fixnum delta-i delta-j))
      (labels ((rotated-i (i j)
			  (declare (Fixnum i j))
			  (+ delta-i
			     (- (* i cos-rad) (* j sin-rad))))
               (rotated-j (i j)
			  (declare (Fixnum i j))
			  (+ delta-j
			     (+ (* i sin-rad) (* j cos-rad)))))
        (dotimes (i (first dimensions))
          (declare (Fixnum i))
          (dotimes (j (second dimensions))
            (declare (Fixnum j))
            (setf (aref result (rotated-i i j) (rotated-j i j))
		  (aref matrix i j))))))
    result))

(defun rotated-dimensions (dimensions angle)
  (declare (Fixnum angle)
           (List dimensions))
  (if (= (mod angle 180) 0)
    dimensions
    (reverse dimensions)))

(defun calculate-translations (dimensions sin-rad cos-rad)
  (declare (Fixnum sin-rad cos-rad)
           (List dimensions))
  (let ((min-i 0)
        (min-j 0)
        (nrow (1- (first dimensions)))
        (ncol (1- (second dimensions))))
    (declare (Fixnum min-i min-j nrow ncol))
    (setq min-i
	  (min (- (* ncol sin-rad)) 
	       (* nrow cos-rad)
	       (- (* nrow cos-rad) (* ncol sin-rad))))
    (setq min-j
	  (min (* ncol cos-rad)
	       (* nrow sin-rad)
	       (+ (* nrow sin-rad) (* ncol cos-rad))))
    (values (if (< min-i 0)
	      (abs min-i)
              0)
            (if (< min-j 0)
	      (abs min-j)
              0))))

;;=============================================================================

(defvar *vertical-text-win* nil)

(defun do-stop-vertical-text-hack () (xlib:destroy-window *vertical-text-win*))

(defun clear-pixmap (pixmap gc)
  (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
		      (xlib:draw-rectangle pixmap gc
					   0 0 
					   (xlib:drawable-width pixmap) 
					   (xlib:drawable-height pixmap) t)))

(defun do-go-vertical-text-hack (&key (string "This is a test") (action :default)
				      (pattern "9x15") ; "7x13"
				      )
  (let* ((display (ext:open-clx-display))
	 (screen (first (xlib:display-roots display)))
	 (root-window (xlib:screen-root screen))
	 (gc (xlib:create-gcontext :drawable root-window
				   :background (xlib:screen-white-pixel screen)
				   :foreground (xlib:screen-black-pixel screen)))
	 (pixmap (xlib:create-pixmap :width 256
				     :height 256
				     :depth (xlib:drawable-depth root-window)
				     :drawable (xlib:screen-root screen))))

    (setf (xlib:display-after-function display) #'xlib:display-finish-output)
    (setf (xlib:gcontext-font gc) pattern)
    (setf *vertical-text-win* (xlib:create-window :parent root-window
						  :x 0 :y 0
						  :width 256
						  :height 256
						  :background (xlib:screen-white-pixel screen)))
    ;; (setf font (xlib:gcontext-font gc))
    (xlib:map-window *vertical-text-win*)


    (multiple-value-bind (width ascent descent left)
	(xlib:text-extents gc string)
      (case action
	(:default
	 (xlib:draw-glyphs *vertical-text-win* gc left ascent string)
	 (xlib:draw-rectangle *vertical-text-win* gc 0 0 width (+ ascent descent))
	 (format t "~& left = ~A     ascent = ~A   width = ~A~%" left ascent width))
	(:action-1 (clear-pixmap pixmap gc)
		   (xlib:draw-glyphs pixmap gc left ascent string)
		   (let ((image (xlib:get-image pixmap
						:x 0 :y 0 :width width :height (+ ascent descent)
						:format :z-pixmap)))
		     (xlib:put-image *vertical-text-win* gc
				     (xlib:create-image 
				      :data (rotate-matrix (xlib:image-z-pixarray image) 90))
				     :x 30 :y 30)))
	(:action-2 (clear-pixmap pixmap gc)
		   (xlib:draw-glyphs pixmap gc left ascent string)
		   (xlib:copy-area pixmap gc 0 0 width (+ ascent descent) *vertical-text-win* 60 60)
		   (let ((image (xlib:get-image pixmap
						:x 0 :y 0 :width width :height (+ ascent descent)
						:format :z-pixmap)))
		     (xlib:put-image *vertical-text-win* gc
				     (xlib:create-image 
				      :data (rotate-matrix (xlib:image-z-pixarray image) 90))
				     :x 30 :y 30)))
	(:action-3 (xlib:draw-glyphs pixmap gc left ascent string)
		   (let ((image (xlib:get-image pixmap
						:x 0 :y 0 :width width :height (+ ascent descent)
						:format :z-pixmap)))
		     (xlib:put-image *vertical-text-win* gc image :x 30 :y 30)
		     (xlib:put-image *vertical-text-win* gc
				     (xlib:create-image 
				      :data (xlib:image-z-pixarray image))
				     :x 30 :y 60)))))))


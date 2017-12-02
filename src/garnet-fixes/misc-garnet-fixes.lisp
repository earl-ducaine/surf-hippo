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


(in-package "LISP")



#|
*** Since this is a Garnet bug that causes an X server crash, I am forwarding
*** this response to the Garnet bboard.

> I have found the following Garnet error.  When the width or height of a
> roundtangle is less than 5, an X server error is created.  We are running
> Lucid 4.0 on an Hewlett Packard Apollo 9000 Series 700.

Bill,

The error was that a negative width and height was being passed to the
low-level X routines to draw the corners of the roundtangle.  To fix this
problem in Garnet, edit the file opal/roundtangles.lisp, and change

  (defun draw-roundtangle-border (drawable xlib-gc-line L TO W H r th)
    (let ((th/2 (floor th 2))
          (th\2 (ceiling th 2))
          (2*r-th (- (+ r r) th)))

to

  (defun draw-roundtangle-border (drawable xlib-gc-line L TO W H r th)
    (let ((th/2 (floor th 2))
          (th\2 (ceiling th 2))
          (2*r-th (max 0 (- (+ r r) th))))  ; <---  avoid X server crash

After changing the lisp file, you will need to start Garnet and do
(garnet-compile "opal:roundtangles") to generate a new binary.
The new file will be loaded the next time you load Garnet from scratch.

--Andrew Mickish

|#

(in-package "OPAL")

(defun draw-roundtangle-border (drawable xlib-gc-line L TO W H r th)
  (let ((th/2 (floor th 2))
	(th\2 (ceiling th 2))
	(2*r-th (max 0 (- (+ r r) th)))) ; <---  avoid X server crash
    (xlib:draw-segments drawable xlib-gc-line
			(list
			 ;; left side
			 (+ L th/2) (+ TO r (mod th 2))
			 (+ L th/2) (+ TO H (- r))
			 ;; bottom side
			 (+ L r (mod th 2)) (- (+ TO H) th\2)
			 (- (+ L W) r)      (- (+ TO H) th\2)
			 ;; right side
			 (- (+ L W) th\2) (- (+ TO H) r)
			 (- (+ L W) th\2) (+ TO r (mod th 2))
			 ;; top side
			 (- (+ L W) r)      (+ TO th/2)
			 (+ L r (mod th 2)) (+ TO th/2)))
    (xlib:draw-arcs drawable xlib-gc-line
		    (list
		     ;; lower left corner
		     (+ L th\2) (+ TO H th/2 (* -2 r)) 
		     2*r-th 2*r-th pi (/ pi 2)
		     ;; lower right corner
		     (+ L W th/2 (* -2 r)) (+ TO H th/2 (* -2 r))
		     2*r-th 2*r-th (* pi 3/2) (/ pi 2)
		     ;; upper right corner
		     (+ L W th/2 (* -2 r)) (+ TO th\2) 2*r-th 2*r-th
		     0.0 (/ pi 2)
		     ;; upper left corner
		     (+ L th\2) (+ TO th\2) 2*r-th 2*r-th
		     (/ pi 2) (/ pi 2)))))




;;;(create-instance 'gg::BESIDE-BUTTON-TEXT opal:text
;;;                 (:constant '(:actual-heightp))
;;;                 (:left (o-formula (let ((p (kr-path 0 :parent)))
;;;                                     (if (gv p :text-on-left-p)
;;;                                         (gv p :left)
;;;                                         (+ (gv p :left) (gv p :button-unit-width)
;;;                                            (gv p :text-offset))))))
;;;                 (:top (o-formula (- (gv (kr-path 0 :parent) :center-y)
;;;                                     (floor (gvl :height) 2))))
;;;                 (:string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))
;;;                                       (if (stringp s)
;;;                                           s
;;;                                           (REPLACE-UNDERSCORE-WITH-SPACE
;;;                                            (string-capitalize (string-trim ":" s)))))))
;;;                 (:font (o-formula (gv (kr-path 0 :parent) :font))))
;;;

;;;(s-value gg::BESIDE-BUTTON-TEXT
;;;         :string (o-formula (let ((s (gv (kr-path 0 :parent) :string)))
;;;                              (if (stringp s)
;;;                                  s
;;;                                  (REPLACE-UNDERSCORE-WITH-SPACE
;;;                                   (string-capitalize (string-trim ":" s)))))))

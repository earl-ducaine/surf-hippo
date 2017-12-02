;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

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

(in-package "COMMON-LISP-USER")


;; LBG 8/17/99

;; Apparently, bitmap slots that use an o-formula to read in a bitmap file (as opposed to directly
;; reading it in when the shema is created), lose their contents with save-lisp. Thus, when the Lisp
;; image is started, they try to regain their contents by reading the specified file. This loses if
;; SH is starting up on a machine that doesn't have the bitmap file (e.g. does not have Garnet).
;(break)
#|
(s-value opal::ARROW-CURSOR :constant nil)
(s-value opal::ARROW-CURSOR-mask :constant nil)
(s-value opal::ARROW-CURSOR :image (opal::Get-Garnet-Bitmap "garnet.cursor"))
(s-value opal::ARROW-CURSOR-MASK :image (opal::Get-Garnet-Bitmap "garnet.mask"))
(s-value opal::ARROW-CURSOR :constant :image)
(s-value opal::ARROW-CURSOR-mask :constant :image)
|#



;; These hacks are to help this problem out. Also, the entire contents of the Garnet lib directory
;; have now been recreated in surf-hippo/lib. Bogus solution.


; If DIR doesn't exist, default to *SURF-HOME*. This way we can mirror GARNET files that are loaded
; unexpectedly on systems without Garnet. 

(defun Garnet-Pathnames (subdir dir)
  (merge-pathnames
   subdir
   (if (probe-file dir)
       dir
       (progn
	 (format t "~% !!! GARNET-PATHNAMES: Can't find directory ~A for loading ~A.~%"
		 dir subdir)
	 (format t  "                      Looking for ~A in *SURF-HOME* (value is ~A).~%"
		 subdir *surf-home*)
	 *surf-home*))))


; Punt on bitmaps especially.. This assumes that "lib/bitmaps/" is the bitmap subdirectory when
; Garnet is built.

(defun opal::Get-Garnet-Bitmap (bitmapname)
  (opal:read-image (user::garnet-pathnames
		    (concatenate 'string "lib/bitmaps/" bitmapname)
		    user::Your-Garnet-Pathname)))
		    

	       

; Another way is to recreate the offending schema, without the o-formulae.
; These come from src/opal/create-instances2.lisp

#|
(create-instance 'opal::ARROW-CURSOR opal:bitmap
  (:constant :image)
  (:image (opal::Get-Garnet-Bitmap "garnet.cursor")))

(create-instance 'opal::ARROW-CURSOR-MASK opal:bitmap
  (:constant :image)
  (:image (opal::Get-Garnet-Bitmap "garnet.mask")))

(create-instance 'opal::HOURGLASS-CURSOR opal:bitmap
  (:constant :image)
  (:image (opal::Get-Garnet-Bitmap "hourglass.cursor")))

(create-instance 'opal::HOURGLASS-CURSOR-MASK opal:bitmap
  (:constant :image)
  (:image (opal::Get-Garnet-Bitmap "hourglass.mask")))
|#

; Another way is to reset the the offending slots, without the o-formulae. But we need
; KR::WITH-CONSTANTS-DISABLED since :image is declared :constant.

; If we don't KR:DESTROY-SLOT first, then you get - 
;
; *** Warning: you are setting the value of slot :IMAGE of
;  object #k<OPAL:HOURGLASS-CURSOR-MASK>.  This slot contains a formula which was never evaluated.
;  The formula is now valid, but its dependencies are not set up properly.  As
;  a result, the formula will never be evaluated.
;  In order for this formula to work properly, you should have used
;  (g-value #k<OPAL:HOURGLASS-CURSOR-MASK> :IMAGE) before using S-VALUE.  If you want to fix things now,
;  re-install the formula using s-value.

(kr::with-constants-disabled
    
  (kr::destroy-slot opal::ARROW-CURSOR :image) 
  (s-value opal::ARROW-CURSOR 
	   :image (opal::Get-Garnet-Bitmap "garnet.cursor"))

  (kr::destroy-slot opal::ARROW-CURSOR-MASK :image) 
  (s-value opal::ARROW-CURSOR-MASK
	   :image (opal::Get-Garnet-Bitmap "garnet.mask"))

  (kr::destroy-slot opal::HOURGLASS-CURSOR :image) 
  (s-value opal::HOURGLASS-CURSOR 
	   :image (opal::Get-Garnet-Bitmap "hourglass.cursor"))

  (kr::destroy-slot opal::HOURGLASS-CURSOR-MASK :image) 
  (s-value opal::HOURGLASS-CURSOR-MASK 
	   :image (opal::Get-Garnet-Bitmap "hourglass.mask")))


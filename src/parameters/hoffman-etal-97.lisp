;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

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


(in-package "SURF-HIPPO")




;; Channel models derived from:

#|
@ARTICLE{Hof-Mag-Col-Joh-97,
	AUTHOR = {Hoffman, D. A. and Magee, J. C. and Colbert, C. M. and Johnston, D.},
	TITLE = {{K$^+$} channel regulation of signal propagation in dendrties of hippocampal pyramidal neurons},
	JOURNAL = {Nature},
	YEAR = {1997},
	VOLUME = {87},
	PAGES = {869-875},
	MONTH = {June}
}

(and the correction note in Nature 390 page 199, 1997)
|#

(channel-type-quoted-def
 `(na-Hoffman-etal-97
   (gbar-density . ,(* 4.2 0.012e-4))	; pS/um2 <- 0.012 S/cm2,  still to be multiplied by 4.2 (?)
   (e-rev . 58)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-Hoffman-etal-97 3) (nah-Hoffman-etal-97 1)))))

(particle-type-def
 `(nam-Hoffman-etal-97
   (class . :hh)
   (alpha  . (lambda (voltage)
	       (let ((v-shift (+ voltage 32.5))) ; Before, had shift factor = 25?
		 (if (zerop v-shift)
		     4.5
		     (/ (* 0.182 v-shift) 
			(- 1 (exp (/ v-shift -4.5))))))))
   (beta  . (lambda (voltage)
	      (let ((v-shift (+ voltage 32.5)))
		(if (zerop v-shift)
		    4.5
		    (/ (* -0.124 v-shift)
		       (- 1 (exp (/ v-shift 4.5))))))))
   (tau-coefficient . 0.8)))

(particle-type-def
 `(nah-Hoffman-etal-97
   (class . :hh)
   (ss  . (lambda (voltage)
	    (/ 1
	       (+ 1 (exp (/ (+ voltage 58) 5))))))
   (tau . (lambda (voltage)
	    (/ 1
	       (+
		;; Their alpha_h
		(/ (* 0.008 (+ voltage 40))
		   (- 1 (exp (- (/ (+ voltage 40) 3)))))
		;; Their beta_h
		(/ (* -0.0005 (+ voltage 10))
		   (- 1 (exp (/ (+ voltage 10) 5))))))))))

(defun k-Hoffman-etal-97-gbar-density (distance)
  ;; DISTANCE from soma in microns. Returns pS/um2.
  (* 1.0e-4
     (+ 0.007
	(* 0.011 (/ distance 100)))))

#|
(loop for distance from 0 to 1000
		       collect distance into distances
		       collect (k-Hoffman-etal-97-gbar-density distance) into densities
		       finally
		       (plot-timed-data densities nil distances
					:y-label "pS/um2"
					:y-are-fns nil
					:x-label "um"
					:x-are-fns t
					:x-inc 200
					:title 'k-Hoffman-etal-97-gbar-density))
|#

(defun set-k-Hoffman-etal-97-gbar (ch &optional (distance (distance-to-soma ch)))
  (set-element-absolute-iv-relation-ref ch (* (element-area ch) ; um2
				       (k-Hoffman-etal-97-gbar-density distance) ; pS/um2
				       1.0e-3 ; uS/pS
				       )))

(defun create-k-Hoffman-etal-97 (element)
  (let ((distance (distance-to-soma element)))
    (set-k-Hoffman-etal-97-gbar
     (create-element (if (> distance 100) 'k-Hoffman-etal-97-distal 'k-Hoffman-etal-97-proximal) element)
     distance)))

(channel-type-def
 `(k-Hoffman-etal-97-proximal
   (gbar-density . 0.03e-4)		; pS/um2 <- 0.007 + (* 0.011 distance/100) S/cm2, distance from soma measured in microns. 0.03S/cm2 constant compromize
   (e-rev . -80)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((km-Hoffman-etal-97-proximal 4) (kh-Hoffman-etal-97 1)))))

(channel-type-def
 `(k-Hoffman-etal-97-distal
   (gbar-density . 0.03e-4)		; pS/um2 <- 0.007 + (* 0.011 distance/100) S/cm2, distance from soma measured in microns. 0.03S/cm2 constant compromize
   (e-rev . -80)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((km-Hoffman-etal-97-distal 4) (kh-Hoffman-etal-97 1)))))

;; for proximal channels
(defun km-Hoffman-etal-97-alpha-proximal (voltage)
  (/ (* -0.01 (+ voltage 21.3))
     (- (exp (/ (+ voltage 21.3)
		-35))
	1)))

(defun km-Hoffman-etal-97-beta-proximal (voltage)
  (/ (* 0.01 (+ voltage 21.3))
     (- (exp (/ (+ voltage 21.3)
		35))
	1)))

(particle-type-def
 `(km-Hoffman-etal-97-proximal
   (class . :hh)
   (ss . (lambda (voltage)
	   (/ (km-Hoffman-etal-97-alpha-proximal voltage)
	      (+ (km-Hoffman-etal-97-alpha-proximal voltage) (km-Hoffman-etal-97-beta-proximal voltage)))))
   (tau . 2)))

(defun km-Hoffman-etal-97-alpha-distal (voltage)
  (/ (* -0.01 (+ voltage 34.4))
     (- (exp (/ (+ voltage 34.4)
		-21))
	1)))

(defun km-Hoffman-etal-97-beta-distal (voltage)
  (/ (* 0.01 (+ voltage 34.4))
     (- (exp (/ (+ voltage 34.4)
		21))
	1)))

(particle-type-def
 `(km-Hoffman-etal-97-distal
   (class . :hh)
   (ss . (lambda (voltage)
	   (/ (km-Hoffman-etal-97-alpha-distal voltage)
	      (+ (km-Hoffman-etal-97-alpha-distal voltage) (km-Hoffman-etal-97-beta-distal voltage)))))
   (tau . 2)))

(defun kh-Hoffman-etal-97-alpha (voltage)
  (/ (* -0.01
	(+ voltage 58))
     (- (exp (/ (+ voltage 58)
		8.2))
	1)))

(defun kh-Hoffman-etal-97-beta (voltage)
  (/ (* 0.01
	(+ voltage 58))
     (- (exp (/ (+ voltage 58)
		-8.2))
	1)))

(particle-type-def
 `(kh-Hoffman-etal-97
   (class . :hh)
   (ss . (lambda (voltage)
	   (/ (kh-Hoffman-etal-97-alpha voltage)
	      (+ (kh-Hoffman-etal-97-alpha voltage) (kh-Hoffman-etal-97-beta voltage)))))
   (tau . (lambda (voltage)
	    (+ 5.0
	       (if (> voltage -20)
		 (/ (* 2.6 (+ 20 voltage))
		    10)
		 0))))))

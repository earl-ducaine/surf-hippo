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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NA-HUG  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
reproduced from
@ARTICLE{Des-Hug-00,
	AUTHOR = {Destexhe, A., and Huguenard, J. R.},
	TITLE = {Nonlinear thermodynamic models of voltage-dependent currents},
	JOURNAL = {Journal of Computational Neuroscience},
	YEAR = {2000},
	VOLUME = {},
	number = {},
	MONTH = {in press}
}
|#

(channel-type-def
 `(ca-t-hh-Destexhe-Hugenard-00
   (permeability-density . 5.0e-7)
   (use-defined-e-rev . NIL)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1.0)
					;   (reference-temp . 22.0)
   (v-particles . ((ca-tm-hh-Destexhe-Hugenard-00 2)(ca-th-hh-Destexhe-Hugenard-00 1)))))


(particle-type-def
 `(ca-tm-hh-Destexhe-Hugenard-00
   (class . :hh)
   (ss-function  . (lambda (voltage)
		     (/ 1
			(+ 1 (exp (/ (+ voltage 57) -6.2))))))
   (tau-function  . (lambda (voltage)
		      (+ 0.612
			 (/ 1
			    (+ (exp (/ (+ voltage 132) -16.7))
			       (exp (/ (+ voltage 16.8) 18.2)))))))))

(particle-type-def
 `(ca-th-hh-Destexhe-Hugenard-00
   (class . :hh)
   (ss-function  . (lambda (voltage)
		     (/ 1
			(+ 1 (exp (/ (+ voltage 81) 4))))))
   (tau-function  . (lambda (voltage)
		      (cond
		       ((>= voltage -81)
			(+ 28
			   (exp (/ (+ voltage 22) -10.5))))
		       (t (exp (/ (+ voltage 467) 66.6))))))))
			
(channel-type-def
 `(ca-t-hh-ext-Destexhe-Hugenard-00
   (permeability-density . 5.0e-7)
   (use-defined-e-rev . NIL)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1.0)
					;   (reference-temp . 22.0)
   (v-particles . ((ca-tm-hh-ext-Destexhe-Hugenard-00 2)
		   (ca-th-hh-ext-Destexhe-Hugenard-00 1)))))


(particle-type-def
 `(ca-tm-hh-ext-Destexhe-Hugenard-00
   (class . :hh-ext)
   (v-half . -57)			; (v-half . -57.00025)
   (valence . 4.2)			; (valence . 4.169777)
   (base-rate . 0.048)			; 0.048158076)
   (gamma . 0.82)			;  (gamma . 0.8193908)
   (tau-0 . 0.78)			; (tau-0 . 0.7757894)
   ))


(particle-type-def
 `(ca-th-hh-ext-Destexhe-Hugenard-00
   (class . :hh-ext)
   (v-half . -81)			;    (v-half . -81.00064)
   (valence . -6.5)			; (valence . -6.4631557)
   (base-rate . 0.0025)			;     (base-rate . 0.0024879656)
   (gamma . 0.11)			; (gamma . 0.11294473)
   (tau-0 . 41)				;  (tau-0 . 40.86677)))
   ))
 

(defun test-ca-t-Destexhe-Hugenard-00 ()
  (create-cell "Ca-T-cell" :soma-diameter (sphere-diameter-from-area 1))
  (create-element 'ca-t-hh-ext-Destexhe-Hugenard-00
		  'ca-t-hh-Destexhe-Hugenard-00
		  *soma*)
  (std-setup))

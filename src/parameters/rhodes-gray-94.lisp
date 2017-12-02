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

;
; Various Na channels.
;


(in-package "SURF-HIPPO")


;;  Rhodes & Gray, Neural Comp. 1994
(channel-type-def
 '(na-rho
   (gbar-density . 300.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)) )
   (QTEN . 3)
   (reference-temp . 37.0)
   (v-particles . ((nam-rho 2) (nah-rho 1)) )))




;; particule d'activation du canal SODIUM
;;
(particle-type-def
 `(nam-rho 
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((diff (+ voltage 46.9)))
		(/ (* -320.0 diff) (1- (exp (/ diff -4.0)))))))			    
   (beta . (lambda (voltage)
	     (let ((diff (+ voltage 19.9)))
	       (/  (* 280.0 diff)  (1- (exp (/ diff 5.0)))))))))



;; particule d'inactivation du canal SODIUM
(particle-type-def
 `(nah-rho
   (class . :hh)
   (alpha . (lambda (voltage)
	      (* 128.0 (exp (/ (+ voltage 43.0) -18.0)))))
   (beta . (lambda (voltage)
	     (/ 4000.0 (1+ (exp (/ (+ voltage 20.0) -5.0))))))))



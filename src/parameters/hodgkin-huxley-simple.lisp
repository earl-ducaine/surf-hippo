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

;
; Parameters for the canonical Hodgkin-Huxley Na and K squid axon channels. The definitions given
; here are the minimum required by Surf-Hippo - more elaborate definitions are given in the file
; hodgkin-huxley.lisp. 
;

(channel-type-def
 '(NA-HH
   (gbar-density . 1200)		; pS/um2
   (e-rev . 50)			; mV
   (v-particles . ((M-HH 3) (H-HH 1)))	; There are 3 M-HH particles and 1 H-HH particle.
   ))


;; For all the rate functions, the voltage argument is assumed to be in mV, and the functions
;; return rate in 1/ms.

(defun m-hh-alpha (voltage)
  (/ (* -0.1 (- voltage -40))
     (1- (exp (/ (- voltage -40)
		 -10)))))

(defun m-hh-beta (voltage)
  (* 4 (exp (/ (- voltage -65) -18))))

(particle-type-def
 `(M-HH
   (class . :HH)			; This particle is of the canonical HH form.
   (alpha . M-HH-ALPHA)			; The forward rate constant is given by this function.
   (beta .  M-HH-BETA)			; The backward rate constant is given by this function.
   ))


(defun h-hh-alpha (voltage)
  (* 0.07 (exp (/ (- voltage -65) -20))))

(defun h-hh-beta (voltage)
  (/ 1 (1+ (exp (/ (- voltage -35) -10)))))

(particle-type-def
 `(H-HH
   (class . :HH)
   (alpha . H-HH-ALPHA)
   (beta . H-HH-BETA)))


(channel-type-def
 '(DR-HH
   (gbar-density . 360)		; pS/um2
   (e-rev . -77)			; mV
   (v-particles . ((n-hh 4)))		; There are 4 N-HH particles.
   ))

(defun n-hh-alpha (voltage)
  (/ (* -0.01 (- voltage -55))
     (1- (exp (/ (- voltage -55) -10)))))

(defun n-hh-beta (voltage)
  (* 0.125 (exp (/ (- voltage -65) -80))))

(particle-type-def
 `(N-HH
   (class . :HH)
   (alpha . N-HH-ALPHA)
   (beta . N-HH-BETA)))
    


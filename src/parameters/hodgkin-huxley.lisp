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
; The channel models for the canonical Hodgkin-Huxley channels, plus version fitted to the extended HH model.
;

; These are not adjusted to the correct reference temperatures.

(channel-type-def
 '(na-hh
   (gbar-density . 1200)
   (e-rev . 50)
   (ion-permeabilities . ((NA 1)))
   (v-particles . ((m-hh 3) (H-hh 1)))))


;; alpha_m = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -0.1e6    1/(Volts*sec)	= -0.1	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.040    Volts		= -40	mV


;; beta_m = A exp((v-V0)/B)
;; A = 4.0e3     1/sec		= 4.0	1/msec
;; B = -0.018    Volts		= -18	mV
;; V0= -0.065    Volts		= -65	mV

(particle-type-def
 `(m-hh
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-40 (- voltage -40)))
		(/ (* -0.1 v-40)
		   (1- (exp (/ v-40 -10)))))))
   (beta . (lambda (voltage)
	     (* 4 (exp (/ (- voltage -65) -18)))))))


;; alpha_h = A exp((v-V0)/B)
;; A = 70.0      1/sec		= 0.07	1/msec
;; B = -0.020    Volts		= -20	mV
;; V0= -0.065    Volts		= -65	mV

;; beta_h = A / (exp((v-V0)/B) + 1)
;; A = 1.0e3 1/sec		= 1.0	1/msec
;; B = -0.010 Volts		= -10	mV
;; V0= -0.035 Volts		= -35	mV

(particle-type-def
 `(h-hh
   (class . :hh)
   (alpha . (lambda (voltage)
	      (* 0.07 (exp (/ (- voltage -65) -20)))))
   (beta . (lambda (voltage)
	     (/ 1 (1+ (exp (/ (- voltage -35) -10))))))))


(channel-type-def
 '(na-hh-ext
   (gbar-density . 1200)
   (e-rev . 50)
   (ion-permeabilities . ((NA 1)))
   (v-particles . ((m-hh-ext 3) (H-hh-ext 1)))))

(particle-type-def
 '(m-hh-ext
   (class . :hh-ext)
   (VALENCE . 2.7)
   (GAMMA . 0.4)
   (BASE-RATE . 1.2)
   (V-HALF . -40)
   (TAU-0 . 0.07)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))

(particle-type-def
 '(h-hh-ext
   (class . :hh-ext)
   (VALENCE . -3.7)
   (GAMMA . 0.4)
   (BASE-RATE . 0.07)
   (V-HALF . -62)
   (TAU-0 . 0.9)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))





(channel-type-def
 '(dr-hh
   (gbar-density . 360)
   (e-rev . -77)
   (ion-permeabilities . ((K 1)))
   (v-particles . ((n-hh 4)))))


;; alpha_n = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -10.0e3   1/(Volts*sec)	= -0.01	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.055    Volts		= -55	mV


;; beta_n = A exp((v-V0)/B)
;; A = 125.0     1/sec		= 0.125	1/msec
;; B = -0.080    Volts		= -80	mV
;; V0= -0.065    Volts		= -65	mV


(particle-type-def
 `(n-hh
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-55 (- voltage -55)))
		(/ (* -0.01 v-55) (1- (exp (/ v-55 -10)))))))
   (beta . (lambda (voltage)
	     (* 0.125 (exp (/ (- voltage -65) -80)))))))

(channel-type-def
 '(dr-hh-ext
   (gbar-density . 360)
   (e-rev . -77)
   (ion-permeabilities . ((K 1)))
   (v-particles . ((n-hh-ext 4)))))

(particle-type-def
 '(n-hh-ext
   (class . :hh-ext)
   (VALENCE . 1.5)
   (GAMMA . 0.9)
   (BASE-RATE . 0.14)
   (V-HALF . -51)
   (TAU-0 . 1)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))




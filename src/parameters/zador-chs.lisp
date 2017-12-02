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

#|

Derived from -

@PHDTHESIS{Zad-93,
	AUTHOR = {Zador, A. M.},
	TITLE = {Biophysics of computation in single hippocampal neurons},
	SCHOOL = {Yale University},
	YEAR = {1993}
}

see also surf-hippo/src/hippocampus/zador-thesis.lisp

|#

;
; Adjusted from the canonical Hodgkin-Huxley channels.
;

; These are not adjusted to the correct reference temperatures yet.

;; The NA-TZ-CA1-HH (M-TZ-CA1-HH and H-TZ-CA1-HH particles) and DR-TZ-CA1-HH (N-TZ-CA1-HH particle)
;; are for the Chapter 5 (p. 78) model.

;; ps/um2

(channel-type-def
 '(na-tz-ca1-hh
   (gbar-density . 2400.0);; HH is (gbar-density . 1200.0)
   (e-rev . 50.0)			; ??
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((m-tz-ca1-hh 3) (H-tz-ca1-hh 1)))))


;; HH is:
;; alpha_m = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -0.1e6    1/(Volts*sec)	= -0.1	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.040    Volts		= -40	mV

;; TZ is:
;; A = -0.2	1/(mV*msec)
;; B = -10	mV
;; V0= -65	mV

;; HH is:
;; beta_m = A exp((v-V0)/B)
;; A = 4.0e3     1/sec		= 4.0	1/msec
;; B = -0.018    Volts		= -18	mV
;; V0= -0.065    Volts		= -65	mV

;; TZ is:
;; A = 8.0	1/msec
;; B = -18	mV
;; V0 = -80	mV

(particle-type-def
 `(m-tz-ca1-hh
   (class . hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (- voltage -65.0)))
		(/ (* -0.2 v-shift)
		   (1- (exp (/ v-shift -10.0)))))))
   (beta . (lambda (voltage)
	     (* 8.0 (exp (/ (- voltage -80.0) -18.0)))))))

;; HH is:
;; alpha_h = A exp((v-V0)/B)
;; A = 70.0      1/sec		= 0.07	1/msec
;; B = -0.020    Volts		= -20	mV
;; V0= -0.065    Volts		= -65	mV

;; TZ is:
;; A = 0.008	1/msec
;; B = -5	mV
;; V0 = -80	mV

;; HH is:
;; beta_h = A / (exp((v-V0)/B) + 1)
;; A = 1.0e3 1/sec		= 1.0	1/msec
;; B = -0.010 Volts		= -10	mV
;; V0= -0.035 Volts		= -35	mV

;; TZ is:
;; A = 0.05	1/msec
;; B = -5	mV
;; V0= -65	mV

(particle-type-def
 `(h-tz-ca1-hh
   (class . hh)
   (alpha . (lambda (voltage)
	      (* 0.008 (exp (/ (- voltage -80.0) -5)))))
   (beta . (lambda (voltage)
	     (/ 0.05
		(1+ (exp (/ (- voltage -65.0) -5.0))))))))





;; HH DR modified


(channel-type-def
 '(dr-tz-ca1-hh
   (gbar-density . 400.0)		; Thesis says 4000!!!  HH is (gbar-density . 360.0)
   (e-rev . -77.0) (ion-permeabilities . ((K 1.0)))
   (v-particles . ((n-tz-ca1-hh 4)))
   ))

;; HH is:
;; alpha_n = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -10.0e3   1/(Volts*sec)	= -0.01	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.055    Volts		= -55	mV

;; TZ is:
;; A =  -0.01	1/(mV*msec)
;; B = -5 mV
;; V0 = -60 mV

;; HH is:
;; beta_n = A exp((v-V0)/B)
;; A = 125.0     1/sec		= 0.125	1/msec
;; B = -0.080    Volts		= -80	mV
;; V0= -0.065    Volts		= -65	mV

;; TZ is:
;; A = 0.006	1/msec
;; B = -20	mV
;; V0 = -40	mV


(particle-type-def
 `(n-tz-ca1-hh
   (class . hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (- voltage -60.0)))
		(/ (* -0.01 v-shift)
		   (1- (exp (/ v-shift -5.0)))))))
   (beta . (lambda (voltage)
	     (* 0.006 (exp (/ (- voltage -40.0) -20)))))))


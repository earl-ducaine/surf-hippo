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

;; Markovian model of Na channel as described in a preprint version of

#|

@ARTICLE{Cla-98,
	AUTHOR = {Clay, J. R.},
	TITLE = {Excitability of the squid giant axon revisited},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1998},
	VOLUME = {80},
	NUMBER = {2},
	PAGES = {903-913},
	MONTH = {August}
}


Note that the parameters in the published version are different.

This model is modified from

@ARTICLE{Van-Bez-91b,
	Author  = {Vandenberg, C. A. and Bezanilla, F.},
  Title   = {A sodium channel gating model based on single-channel, macroscopic ionic, and gating currents in the squid giant axon},
  Journal = {Biophysical Journal},
  Year    = {1991},
  Volume  = {60},
  Pages   = {1511-1533}
  }

See also vandenberg-bezanilla.lisp
|#

(channel-type-def
 `(na-clay98
   ;; VB model was constant-field
   (gbar-density . 450)		; 45 mS/cm2
   (e-rev . 55)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nax-clay98 1)))))

; nomenclature of VB paper
(defvar vandenberg-bezanilla-na-states      '(C1   C2   C3   C4   C5   I4  I5  I   O)) 
(defvar vandenberg-bezanilla-na-open-states '(O))


;; voltage is in mV, and returns 1/ms
(defun clay98-exponential-rate (voltage &key (a 0.0) (b 0.0) (c 0.0) (d 0.0) (ee 0.0))
  "If c = 0.0, then it and d will be ignored. See Clay, Journal of Neurophysiology, v80:2 pp903-913, 1998,"
  (+ ee
     (/ (* a (exp (* b voltage)))
	(if (zerop c)
	    1.0
	    (+ 1 (exp (* (the sf c) (+ voltage (the sf d)))))))))

;; parameters assuming voltage in mV, rate in 1/ms
(defvar clay98-rate-parameters
  ;; If c = 0.0, then it and d will be ignored in CLAY98-EXPONENTIAL-RATE
  ;; rate constant a b c d ee
  `((y . (19.92 0.0138 0.0 0.0 0.0)) 
    (a . (6.9 0.017 -0.05 40.0 0.0))
    (c . (18.84 0.06 -0.05 40.0 0.0))
    (f . (0.48 0.0 -0.05 0.0 0.5184))
    (g . (0.77 0.0 -0.07 40.0 0.0))
    (z . (1.1652 -0.05 0.0 0.0 0.0)) 
    (b . (5.16 -0.00017 -0.07 40.0 0.0))
    (d . (1.632 -0.02 0.0 0.0 0.0)) 
    (i . (0.0048 -0.038 0.0 0.0 0.0)) 
    (j . (0.00852 -0.038 0.0 0.0 0.0))))

(defmacro clay98-a (param) `(nth 0 (get-a-value ,param clay98-rate-parameters)))
(defmacro clay98-b (param) `(nth 1 (get-a-value ,param clay98-rate-parameters)))
(defmacro clay98-c (param) `(nth 2 (get-a-value ,param clay98-rate-parameters)))
(defmacro clay98-d (param) `(nth 3 (get-a-value ,param clay98-rate-parameters)))
(defmacro clay98-ee (param) `(nth 4 (get-a-value ,param clay98-rate-parameters)))

(defvar CLAY98-STATE-VOLTAGE-TRANSITION-FUNCTIONS
  (loop for from-to-rate in '((c1 c2 y)
			      (c2 c1 z)
			      (c2 c3 y)
			      (c3 c2 z)
			      (c3 c4 y)
			      (c4 c3 z)
			      (c4 i4 g)
			      (i4 c4 j)
			      (c4 c5 a)
			      (c5 c4 b)
			      (c5 o c)
			      (o c5 d)
			      (o i f)
			      (i o i)
			      (i i5 d)
			      (i5 i c)
			      (i5 i4 b)
			      (i4 i5 a))
	collect
	(let ((from (nth 0 from-to-rate))
	      (to (nth 1 from-to-rate))
	      (rate (nth 2 from-to-rate)))
	  (list from to `(clay98-exponential-rate voltage
						  :a ,(clay98-a rate)
						  :b ,(clay98-b rate)
						  :c ,(clay98-c rate)
						  :d ,(clay98-d rate)
						  :ee ,(clay98-ee rate))))))

(particle-type-quoted-def
 `(nax-clay98
   (class . :markov)
   (states . ,vandenberg-bezanilla-na-states)
   (open-states . ,vandenberg-bezanilla-na-open-states)
   (STATE-TRANSITIONS . ,CLAY98-STATE-VOLTAGE-TRANSITION-FUNCTIONS)))

(defun test-na-clay98 ()
  (topload 'dead-hippo)
  (create-element *soma* 'na-clay98)
  (enable-element-plot 'particles))
 

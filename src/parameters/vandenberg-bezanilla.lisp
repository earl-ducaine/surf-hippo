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

;; Markovian model of Na channel as described in

#|

@ARTICLE{Van-Bez-91a,
	Author  = {Vandenberg, C. A. and Bezanilla, F.},
  Title   = {Single-channel, macroscopic, and gating currents from sodium channels in the squid giant axon},
  Journal = {Biophysical Journal},
  Year    = {1991},
  Volume  = {60},
  Pages   = {1499-1510}
  }

@ARTICLE{Van-Bez-91b,
	Author  = {Vandenberg, C. A. and Bezanilla, F.},
  Title   = {A sodium channel gating model based on single-channel, macroscopic ionic, and gating currents in the squid giant axon},
  Journal = {Biophysical Journal},
  Year    = {1991},
  Volume  = {60},
  Pages   = {1511-1533}
  }
|#

;; Model parameters derived from data with temperature = 5C

;; Note that model was derived from recordings with

;;    546Na ASW (no divalent cations)

;; Divalent block derived from Equation 3 in Van-Bez-91a

;; Note that Van-Bez-91a used a value of 10mM Ca++ and  50mM Mg++ in some of their experiments with squid axon. 
(defvar *kd-vb91-na-divalent-block* 150.0) ; mM

;; voltage in mV
(defun vb91-na-divalent-block (voltage)
  (declare (optimize (speed 3) (space 0)) 
	   (single-float voltage))
  (/ 1.0
     (+ 1.0 (* (/ 
		(+ (the sf (default-extracellular-concentration 'ca))
		   (the sf (default-extracellular-concentration 'mg)))
		(* (the sf *kd-vb91-na-divalent-block*)
		   (exp (/ (* 2		; valence of divalent ions
			      0.19	; d
			      faraday
			       (* 1.0e-3 voltage) ; convert to volts
			      )
			   (* gasconstant *temperature*)))))))))


(channel-type-quoted-def
 `(na-vb91
   (permeability-density . ,(* *jaffe94-ghk-gbar-correction* 4.0)) ; matching to HH model current mag
   ;; Equation 3 in Van-Bez-91a 
   (static-voltage-dependence-function . vb91-na-divalent-block)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nax-vb91 1)))))


;; voltage is in mV, and returns 1/ms
(defmacro vb91-exponential-rate (a b c)
  `#'(lambda (voltage)
       (declare (optimize (speed 3) (space 0)) 
		(single-float voltage))
       (let ((voltage (* 1.0e-3 voltage ))) 
	 (* 1.0e-3			; convert result to 1/ms
	    (the sf ,a)
	    (exp (* (the sf ,b) (/ (* (the sf ,c) voltage)
				   (/ (* gasconstant *temperature*) faraday))))))))

; nomenclature of VB paper
(defvar vandenberg-bezanilla-na-states '(C1   C2   C3   C4   C5   I4  I5  I   O)) 
(defvar vandenberg-bezanilla-na-open-states '(O))


;; parameters assuming voltage in mV, rate in 1/s
(defvar vb91-rate-parameters)
(setq vb91-rate-parameters
      ;; rate constant  a b c
      '((y . (16609.0 1.50 0.22))
	(a . (5750.0  0.42 0.99))
	(c . (15669.0 1.91 0.75))
	(f . (432.0   0.91 0.001))
	(g . (770.0   0.91 0.001))
	(z . (971.0  -1.50 0.78))
	(b . (4325.0 -0.42 0.01))
	(d . (1361.0 -1.91 0.25))
	(i . (4.0    -0.91 0.999))))



(defun vb91-a (param) (nth 0 (get-a-value param vb91-rate-parameters)))
(defun vb91-b (param) (nth 1 (get-a-value param vb91-rate-parameters)))
(defun vb91-c (param) (nth 2 (get-a-value param vb91-rate-parameters)))


(defvar VB91-STATE-VOLTAGE-TRANSITION-FUNCTIONS
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
	  (list from to	(case rate
			  (j (vb91-exponential-rate (the sf (/ (* (the sf (vb91-a 'g))
								  (the sf (vb91-a 'i)))
							       (the sf (vb91-a 'f))))
						    (the sf (+ (* (the sf (vb91-b 'g)) (the sf (vb91-c 'g)))
							       (* (the sf (vb91-b 'i)) (the sf (vb91-c 'i)))
							       (* -1.0 (the sf (vb91-b 'f)) (the sf (vb91-c 'f)))))
						    1.0))
			  (t (vb91-exponential-rate (vb91-a rate) (vb91-b rate) (vb91-c rate))))))))

(particle-type-quoted-def
 `(nax-vb91
   (class . :markov)
   (states . ,vandenberg-bezanilla-na-states)
   (open-states . ,vandenberg-bezanilla-na-open-states)
   (STATE-TRANSITIONS . ,VB91-STATE-VOLTAGE-TRANSITION-FUNCTIONS)))


(defun test-na-vandenberg-bezanilla ()
  (topload 'dead-hippo)
  (create-element *soma* 'na-vb91 'dr-hh)
  (setq *user-stop-time* 10)
  (add-pulse-list (add-isource *soma*) '(1 100 1))
  (enable-element-plot (somas))
  (enable-element-plot (channels))
  (enable-element-plot (particles))
  (enable-element-plot (channels) 'reversal-potential))



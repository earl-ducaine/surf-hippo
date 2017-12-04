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


;; Parameters for the original and extended Hodgkin-Huxley version models for hippocampal pyramidal
;; cell channels, as described by Warman, E. N.; Durand, D. M.; and Yuen, G. L. F., Reconstruction
;; of Hippocampal CA1 Pyramidal Cell Electrophysiology by Computer Simulation, J. Neurophy. v71, no
;; 6, June 1994


(in-package "SURF-HIPPO")

;; Na current
(channel-type-def
 '(na-wdy
   (gbar-density . 1500.0)		; 150mS/cm2
   (e-rev . 65.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-wdy 3) (nah-wdy 1)))))


(particle-type-def
 `(nam-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-11 (- voltage 11.0)))
		(/ (* -1.74 v-11)
		   (- (exp (/ v-11 -12.94)) 1)))))
   (beta . (lambda (voltage)
	     (let ((v-5.9 (- voltage 5.9)))
	       (/ (* 0.06 v-5.9)
		  (- (exp (/ v-5.9 4.47)) 1)))))))

(particle-type-def
 `(nah-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (/ 3 (exp (/ (+ voltage 80) 10)))))
   (beta . (lambda (voltage)
	     (/ 12 (+ (exp (/ (- voltage 77) -27)) 1))))))

(channel-type-def
 '(na-wdy-ext
   (gbar-density . 1500.0)		; 150mS/cm2
   (e-rev . 65.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-wdy-ext 3) (nah-wdy-ext 1)))))


(particle-type-def
 `(nam-wdy-ext
   (class . :hh-ext)
   (VALENCE . 2.2)
   (GAMMA . 0.79)
   (BASE-RATE . 2.8)
   (V-HALF . -35.0)
   (TAU-0 . 0.028)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


(particle-type-def
 `(nah-wdy-ext
   (class . :hh-ext)
   (VALENCE . -3.5)
   (GAMMA . .74)
   (BASE-RATE . 0.12)
   (V-HALF . -48.0)
   (TAU-0 . 0.0)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


;; Ca current
;;
;; s kinetics taken from kay and wong 87 (dissociated gp ca1 @22C)


;; E[Ca] = -25.8 ln([Ca_1]/2000)

(channel-type-def
 '(ca-wdy
   (gbar-density . 100.0)		; 10mS/cm2
   (ion-permeabilities . ((cA 1.0)))
   (use-variable-e-rev . t)
   (conc-int-type-params . ((ca-in1-wdy (1 .7))
			    (ca-in2-wdy (1 0.024))))

   (CONC-INT-TYPE-E-REV-PARAMS . ((ca-in1-wdy (1 1))))

   (v-particles . ((cas-wdy 2) (car-wdy 1)))))


(particle-type-def
 `(cas-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((shifted-v (- voltage -26.0)))
		(/ (* -0.16 shifted-v)
		   (- (exp (/ shifted-v -4.5)) 1)))))
   (beta . (lambda (voltage)
	     (let ((shifted-v (- voltage -12.0)))
	       (/ (* 0.04 shifted-v)
		  (- (exp (/ shifted-v 10)) 1)))))))

(particle-type-def
 `(car-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (/ 2 (exp (/ (+ voltage 94) 10)))))
   (beta . (lambda (voltage)
	     (/ 8 (+ (exp (/ (- voltage 68) -27)) 1))))))

(channel-type-def
 '(ca-wdy-ext
   (gbar-density . 100.0)		; 10mS/cm2
   (ion-permeabilities . ((cA 1.0)))
   (use-variable-e-rev . t)
   (conc-int-type-params . ((ca-in1-wdy (1 .7))
			    (ca-in2-wdy (1 0.024))))

   (CONC-INT-TYPE-E-REV-PARAMS . ((ca-in1-wdy (1 1))))
   (v-particles . ((cas-wdy-ext 2) (car-wdy-ext 1)))))


(particle-type-def
 `(cas-wdy-ext
   (class . :hh-ext)
   (v-half . -25.2)
   (valence . 3.8)
   (base-rate . 0.92)
   (gamma . 0.80)
   (tau-0 . 0.17)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


(particle-type-def
 `(car-wdy-ext
   (class . :hh-ext)
   (v-half . -60.0)
   (valence . -3.5)
   (base-rate . 0.069)
   (gamma . 0.73)
   (tau-0 . 0.064)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


;; k channel - CT

(channel-type-def
 '(kct-wdy
   (gbar-density . 1200.0)		; 120mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kctd-wdy 1)))
   (conc-particles . ((kctc-wdy 2)))))


(channel-type-def
 '(kct-wdy-ext
   (gbar-density . 1200.0)		; 120mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kctd-wdy-ext 1)))
   (conc-particles . ((kctc-wdy 2)))))


(particle-type-def
 '(kctd-wdy-ext
   (class . :hh-ext)
   (v-half . -46.0)
   (valence . -3.5)
   (base-rate . 0.038)
   (gamma . 0.74)
   (tau-0 . 0.0)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


(particle-type-def
 `(kctd-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (/ 1.0 (exp (/ (+ voltage 79) 10)))))
   (beta . (lambda (voltage)
	     (/ 4 (+ (exp (/ (- voltage 82) -27)) 1))))))



#|
(defun kctc-wdy-ss (conc-arg voltage &optional return-rate)
  (declare (optimize (speed 3) (space 0))
	   (double-float conc-arg voltage))
  ;; CONC-ARG is in mM, while paper uses uM
  (let* ((v-shift (- (* 40 (the df (log (* conc-arg 10000) 10))) ; the paper uses 1000, but the
		     105))
	 (v-arg (/ (+ voltage v-shift 103) -12))
	 (alpha (if (= (exp v-arg) 1)
		  0.0077d0		; in the paper, the coefficient is -0.0077d0
		  (/ (* 0.0077 v-arg)
		     (- (exp v-arg) 1))))
	 (beta (/ 1.7 (exp (/ (+ voltage v-shift 237) 30)))))
    (case return-rate
      (:beta beta)
      (:alpha alpha)
      (t (/ alpha (+ alpha beta))))))
|#

;; with corrections from Dominique Durand
;; This function is optimised because it is called at every particle evaluation.
(defun kctc-wdy-ss (conc-arg voltage &optional return-rate)
  (declare (optimize (speed 3) (space 0))
	   (double-float voltage conc-arg))
    ;; CONC-ARG is in mM, while paper uses uM
    (let* ((v-shift (- (* 40 (the df (DF-REAL-LOG (the df (* conc-arg 1000)) 10.0d0))) 105))
	   (v-arg (+ voltage v-shift 103))
	   (alpha (if (= (exp v-arg) 1)
		      -0.0077d0
		      (/ (* -0.0077 v-arg)
			 (- (exp (/ v-arg -12)) 1))))
	   (beta (/ 1.7 (exp (/ (+ voltage v-shift 237) 30)))))
      (case return-rate
	(:beta beta)
	(:alpha alpha)
	(t (/ alpha (+ alpha beta))))))

(conc-particle-type-def
 `(kctc-wdy
   (class . :conc-volt)
   (ss . kctc-wdy-ss)
   (tau . 1.1)
   (conc-int-type . ca-in1-wdy)
   (shell . 1)))

;; k ahp channel

(channel-type-def
 '(kahp-wdy
   (gbar-density . 33.0)		; 3.3mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (conc-particles . ((kahpq-wdy 1)))))

;; This function is optimised because it is called at every particle evaluation.
(defun kahpq-wdy-ss (conc-arg voltage)
  (declare (optimize (speed 3) (space 0))
	   (declare (double-float conc-arg))
	   (ignore voltage))
  (let ((log-conc (* 10 (the df (DF-REAL-LOG (* conc-arg 1000) 10.0d0)))))
    (let ((alpha (/ 0.0048 (exp (/ (- log-conc 35) -2))))
	  (beta (/ 0.012 (exp (/ (+ log-conc 100) 5)))))
      (/ alpha (+ alpha beta)))))

(conc-particle-type-def
 `(kahpq-wdy
   (class . :conc-volt)
   (ss . kahpq-wdy-ss)
   (tau . 48)
   (conc-int-type . ca-in2-wdy)
   (shell . 1)))


;; k m channel

(channel-type-def
 '(km-wdy
   (gbar-density . 67.0)		; 6.7mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kmu-wdy 2)))))


(particle-type-def
 `(kmu-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (+ voltage 52.7)))
		(/ 0.016 (exp (/ v-shift -23))))))
   (beta . (lambda (voltage)
	     (let ((v-shift (+ voltage 52.7)))
	       (/ 0.016 (exp (/ v-shift 18.8))))))))


(channel-type-def
 '(km-wdy-ext
   (gbar-density . 67.0)		; 6.7mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kmu-wdy-ext 2)))))


(particle-type-def
 `(kmu-wdy-ext
   (class . :hh-ext)
   (v-half . -52.7)
   (valence . 2.5)
   (base-rate . 0.016)
   (gamma . 0.45)
   (tau-0 . 0.0043)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


;; k a channel

(channel-type-def
 '(ka-wdy
   (gbar-density . 1000.0)		; 100.0mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kaa-wdy 1)(kab-wdy 1)))))


(particle-type-def
 `(kaa-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (+ voltage 20.0)))
		(/ (* -0.05 v-shift) (- (exp (/ v-shift -15)) 1)))))
   (beta . (lambda (voltage)
	     (let ((v-shift (+ voltage 10.0)))
	       (/ (* 0.1 v-shift) (- (exp (/ v-shift 8)) 1)))))))

(particle-type-def
 `(kab-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (+ voltage 18.0)))
		(/ 0.00015 (exp (/ v-shift 15))))))
   (beta . (lambda (voltage)
	     (let ((v-shift (+ voltage 73.0)))
	       (/ 0.06 (+ (exp (/ v-shift -12)) 1)))))))


(channel-type-def
 '(ka-wdy-ext
   (gbar-density . 1000.0)		; 100.0mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kaa-wdy-ext 1)(kab-wdy-ext 1)))))


(particle-type-def
 `(kaa-wdy-ext
   (class . :hh-ext)
   (v-half . -13.5)
   (valence . 2.3)
   (base-rate . 1.25)
   (gamma . 0.3)
   (tau-0 . 0.14)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


(particle-type-def
 `(kab-wdy-ext
   (class . :hh-ext)
   (v-half . -86.4)
   (valence . -3.2)
   (base-rate . 0.031)
   (gamma . 0.48)
   (tau-0 . 16.0)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


;; k dr channel
(channel-type-def
 '(kdr-wdy
   (gbar-density . 80.0)		; 8.0mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kdrn-wdy 4)))))


(particle-type-def
 `(kdrn-wdy
   (class . :hh)
   (alpha . (lambda (voltage)
	      (/ (* -0.018 voltage) (- (exp (/ voltage -25)) 1))))
   (beta . (lambda (voltage)
	     (/ (* 0.0036 (- voltage 10)) (- (exp (/ (- voltage 10) 12)) 1))))))



(channel-type-def
 '(kdr-wdy-ext
   (gbar-density . 80.0)		; 8.0mS/cm2
   (e-rev . -80.0)
   (ion-permeabilities . ((k 1.0)))
   (v-particles . ((kdrn-wdy-ext 4)))))


(particle-type-def
 `(kdrn-wdy-ext
   (class . :hh-ext)
   (v-half . -41.1)
   (valence . 1.2)
   (base-rate . 0.25)
   (gamma . 0.63)
   (tau-0 . 0.84)
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (Fixed-boltzmann-reference-temperature . 35.0)))


;; Ca++ concentration integrators

;; as in the paper
(defun warman94-ca-erev (conc-in conc-out type)
  (declare (ignore conc-out type))
  (* -25.8 (log (/ conc-in 2))))

;; from D. Durand -
#|
1) Although there was an error in the paper, the simulations were done
appropriately using in fact
        RT/ZF= 12.9623
|#


(defun warman94-ca-erev-valence-2 (conc-in conc-out type)
  (declare (ignore conc-out type))
  (* (/ -25.8 2) (log (/ conc-in 2))))



;; comment from D. Durand -
;;	The actual values used in the simulations were
;;  [Ca,1]initial = 0.097442 instead of 0.1
;; [Ca,2] initial = 0.002558 instead of 0.1


(conc-int-type-def
 `(ca-in1-wdy
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; Default
   (tau . 0.9)				; ms
   (juxtamembrane-shell-thickness . 1.0) ; microns
   (core-conc . 0.9744e-4)		; mM
   ;;   (core-conc . 1.0e-4)			; mM
   (parameters . ((e-rev-function . warman94-ca-erev-valence-2)))
   ;; (parameters . ((e-rev-function . warman94-ca-erev)))
   ))

(conc-int-type-def
 `(ca-in2-wdy
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; Default
   (tau . 1000.0)			; ms
   (juxtamembrane-shell-thickness . 1.0) ; microns
   (core-conc . 0.2558e-5)		;mM
;   (core-conc . 1.0e-4)		; mM
))



#|

notes on simulations:

ca params do not support ca only spike

na-only IR gives dual resting potentials




|#

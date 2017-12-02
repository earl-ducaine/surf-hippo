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

;;; Markovian model of Na channel as described in
#|
@ARTICLE{Kuo-Bea-94,
	AUTHOR = {Kuo, C. C. and Bean, P. B.},
	TITLE = {{$Na^+$} channels must deactivate to recover from inactivation},
	JOURNAL = {Neuron},
	YEAR = {1994},
	VOLUME = {12},
	PAGES = {819-829}
}


"..the voltage dependence of na channel gating studied with whole-cell recording is shifted -20 to
-50mV in the hyperpolarizing direction."
|#


(defun test-na-kb94 ()
  (topload 'dead-hippo)(create-channel *soma* 'na-kb94)
  (add-pulse-list (add-isource *soma*) '(0 1.5 1.5))
  (setq *plot-node-elements* (list (soma-name *soma*)))
  (enable-element-plot (car (particles))))
		  
(channel-type-def
 '(na-kb94
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nax-kb94 1))))
 )


(defvar kb-na-states)
(setq kb-na-states       '(C1   C2   C3   C4   C5   O
			   C1B  C2B  C3B  C4B  C5B  OB))
(defvar kb-na-open-states)
(setq kb-na-open-states  '(O))


;; simulating fig 3 (t = 22C)
(defvar *Con-kb94* 0.004)		; 1/ms - with diphenylhydantoin 0.000001
(defvar *Coff-kb94* 4.5)		; 1/ms - with diphenylhydantoin 0.023
(defvar *Oon-kb94* 4.0)			; 1/ms - with diphenylhydantoin 0.001
(defvar *Ooff-kb94* 0.008)		; 1/ms - with diphenylhydantoin 0.000004

(defvar *a-kb94* (expt (/ (/ *Coff-Kb94* *Con-Kb94*) (/ *Ooff-Kb94* *Oon-Kb94*)) 0.125))

(proclaim '(type single-float *Con-kb94* *Coff-kb94* *Oon-kb94* *Ooff-kb94* *a-kb94*))

;; Markov voltage functions to fill arrays must take single floats and return single float rates (1/ms).
;; Voltage in mV
(defun alpha-kb94 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage)) (* 20 (exp (/ voltage 40))))
(defun beta-kb94 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage)) (* 3 (exp (/ voltage -40))))
(defun gamma-kb94 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage)) (* 50 (exp (/ voltage 100))))
(defun delta-kb94 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage)) (* 0.2 (exp (/ voltage -25))))


#|
alpha = 20 exp v/40
beta = 3 exp -v/40
gamma = 50 exp v/100
delta = 0.2 exp -v/25

|#


(defvar *kb94-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)

(setq *kb94-STATE-VOLTAGE-TRANSITION-FUNCTIONS*
      `((c1 c2 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 4 (the sf (alpha-kb94 voltage)))))
	(c1 c1b ,*Con-kb94*)
	(c2 c2b ,(* *Con-kb94* *a-kb94*))
	(c2 c3 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 3 (the sf (alpha-kb94 voltage)))))
	(c3 c3b ,(* *Con-kb94* (the sf (expt *a-kb94* 2))))
	(c3 c4 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 2 (the sf (alpha-kb94 voltage)))))
	(c4 c5 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (the sf (alpha-kb94 voltage))))
	(c4 c4b ,(* *Con-kb94* (the sf (expt *a-kb94* 3))))
	(c5 c5b ,(* *Con-kb94* (the sf (expt *a-kb94* 4))))
	(c5 O (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		(the sf (gamma-kb94 voltage))))
	(O Ob ,*Oon-kb94*)
	(c1b c2b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (* 4 (the sf (alpha-kb94 voltage)) *a-kb94*)))
	(c2b c3b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (* 3 (the sf (alpha-kb94 voltage)) *a-kb94*)))
	(c3b c4b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (* 2 (the sf (alpha-kb94 voltage)) *a-kb94*)))
	(c4b c5b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (* (the sf (alpha-kb94 voltage)) *a-kb94*)))
	(c5b ob (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		  (the sf (gamma-kb94 voltage))))

	(c2 c1 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (the sf (beta-kb94 voltage)) ))
	(c1b c1 ,*Coff-kb94*)
	(c2b c2 ,(/ *Coff-kb94* *a-kb94*))
	(c3 c2 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 2 (the sf (beta-kb94 voltage)) )))
	(c3b c3 ,(/ *Coff-kb94* (the sf (expt *a-kb94* 2))))
	(c4 c3 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 3 (the sf (beta-kb94 voltage)) )))
	(c5 c4 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		 (* 4 (the sf (beta-kb94 voltage)) )))
	(c4b c4 ,(/ *Coff-kb94* (the sf (expt *a-kb94* 3))))
	(c5b c5 ,(/ *Coff-kb94* (the sf (expt *a-kb94* 4))))
	(O c5 (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		(the sf (delta-kb94 voltage))))
	(Ob O ,*Ooff-kb94*)
	(c2b c1b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (/ (the sf (beta-kb94 voltage))  *a-kb94*)))
	(c3b c2b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (/ (* 2 (the sf (beta-kb94 voltage)) ) *a-kb94*)))
	(c4b c3b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (/ (* 3 (the sf (beta-kb94 voltage)) ) *a-kb94*)))
	(c5b c4b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		   (/ (* 4 (the sf (beta-kb94 voltage)) ) *a-kb94*)))
	(ob c5b (lambda (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
		  (the sf (delta-kb94 voltage))))))


(particle-type-quoted-def
 `(nax-kb94
   (class . :markov)
   (states . ,kb-na-states)
   (open-states . ,kb-na-open-states)
   (STATE-TRANSITIONS . ,*kb94-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)))


;; *****************************************
;; Shifted Model
;; *****************************************

(defvar *Con-kb94-v2* 0.004)		; 1/ms - with diphenylhydantoin 0.000001
(defvar *Coff-kb94-v2* 4.5)		; 1/ms - with diphenylhydantoin 0.023
(defvar *Oon-kb94-v2* 4.0)			; 1/ms - with diphenylhydantoin 0.001
(defvar *Ooff-kb94-v2* 0.008)		; 1/ms - with diphenylhydantoin 0.000004
(defvar *a-kb94-v2* (expt (/ (/ *Coff-Kb94-V2* *Con-Kb94-V2*) (/ *Ooff-Kb94-V2* *Oon-Kb94-V2*)) 0.125))
(proclaim '(type single-float *Con-kb94-v2* *Coff-kb94-v2* *Oon-kb94-v2* *Ooff-kb94-v2* *a-kb94-v2*))


;; shifted by 20mV to the *left*, required by making things steeper by factor of 2
(defun alpha-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 20 (exp (/ (- voltage -25) (* 0.5 40)))))
(defun beta-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 3 (exp (/ (- voltage -25) (* 0.5 -40)))))
(defun gamma-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 50 (exp (/ (- voltage -25) (* 0.5 100)))))
(defun delta-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 0.2 (exp (/ (- voltage -25) (* 0.5 -25)))))

(defun alpha-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 20 (exp (/ (- voltage 20) (* 1 40)))))
(defun beta-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 3 (exp (/ (- voltage 20) (* 1 -40)))))
(defun gamma-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 50 (exp (/ (- voltage 20) (* 1 100)))))
(defun delta-kb94-v2 (voltage) (declare (optimize (speed 3) (space 0)) (single-float voltage))
       (* 0.2 (exp (/ (- voltage 20) (* 1 -25)))))


(defvar *kb94-v2-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)
(setq *kb94-v2-STATE-VOLTAGE-TRANSITION-FUNCTIONS*
      `((c1 c2 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 4 (the sf (alpha-kb94-v2 v)))))
	(c1 c1b ,*Con-kb94-v2*)
	(c2 c2b ,(* *Con-kb94-v2* *a-kb94-v2*))
	(c2 c3 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 3 (the sf (alpha-kb94-v2 v)))))
	(c3 c3b ,(* *Con-kb94-v2* (the sf (expt *a-kb94-v2* 2))))
	(c3 c4 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 2 (the sf (alpha-kb94-v2 v)))))
	(c4 c5 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (the sf (alpha-kb94-v2 v))))
	(c4 c4b ,(* *Con-kb94-v2* (the sf (expt *a-kb94-v2* 3))))
	(c5 c5b ,(* *Con-kb94-v2* (the sf (expt *a-kb94-v2* 4))))
	(c5 O (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		(the sf (gamma-kb94-v2 v))))
	(O Ob ,*Oon-kb94-v2*)
	(c1b c2b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (* 4 (the sf (alpha-kb94-v2 v)) *a-kb94-v2*)))
	(c2b c3b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (* 3 (the sf (alpha-kb94-v2 v)) *a-kb94-v2*)))
	(c3b c4b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (* 2 (the sf (alpha-kb94-v2 v)) *a-kb94-v2*)))
	(c4b c5b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (* (the sf (alpha-kb94-v2 v)) *a-kb94-v2*)))
	(c5b ob (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		  (the sf (gamma-kb94-v2 v))))
	(c2 c1 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (the sf (beta-kb94-v2 v)) ))
	(c1b c1 ,*Coff-kb94-v2*)
	(c2b c2 ,(/ *Coff-kb94-v2* *a-kb94-v2*))
	(c3 c2 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 2 (the sf (beta-kb94-v2 v)) )))
	(c3b c3 ,(/ *Coff-kb94-v2* (the sf (expt *a-kb94-v2* 2))))
	(c4 c3 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 3 (the sf (beta-kb94-v2 v)) )))
	(c5 c4 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		 (* 4 (the sf (beta-kb94-v2 v)) )))
	(c4b c4 ,(/ *Coff-kb94-v2* (the sf (expt *a-kb94-v2* 3))))
	(c5b c5 ,(/ *Coff-kb94-v2* (the sf (expt *a-kb94-v2* 4))))
	(O c5 (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		(the sf (delta-kb94-v2 v))))
	(Ob O ,*Ooff-kb94-v2*)
	(c2b c1b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (/ (the sf (beta-kb94-v2 v))  *a-kb94-v2*)))
	(c3b c2b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (/ (* 2 (the sf (beta-kb94-v2 v)) ) *a-kb94-v2*)))
	(c4b c3b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (/ (* 3 (the sf (beta-kb94-v2 v)) ) *a-kb94-v2*)))
	(c5b c4b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		   (/ (* 4 (the sf (beta-kb94-v2 v)) ) *a-kb94-v2*)))
	(ob c5b (lambda (v) (declare (optimize (speed 3) (space 0)) (single-float v))
		  (the sf (delta-kb94-v2 v))))))


(channel-type-def
 '(na-kb94-v2
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nax-kb94-v2 1))))
 )

(particle-type-quoted-def
 `(nax-kb94-v2
   (class . :markov)
   (states . ,kb-na-states)
   (open-states . ,kb-na-open-states)
   (STATE-TRANSITIONS . ,*kb94-v2-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)))


;; *******************;; *******************;; *******************;; *******************
























(defun test-na-kb94-v2 ()
  (topload 'dead-hippo)(create-channel *soma* 'na-kb94-v2)
  (enable-element-plot (car (particles)))
  (element-parameter (car (particles)) 'plot-markov-states t))



#|

peak current curve for activation (from -130mV)


(
 (-0.003042221 -0.003042221 -0.04654789 -0.07384062 -0.23399258 -0.40614414
  -0.79626656 -3.4903624 -11.985724 -28.262518 -48.000484 -63.773872 -72.41494
  -72.93276 -66.70552 -54.249397 -39.549988 -19.999996 -2.0191765 -2.1114044))


peak current curve for inactivation (to 0mV)

((-130.0 -120.0 -110.0 -100.0 -90.0 -80.0 -70.0 -60.0 -50.0 -40.0 -30.0 -20.0)
 (-73.05118 -72.918274 -72.82033 -72.692085 -68.00288 -48.66243 -16.080643
  -2.9574852 -0.75650215 -0.5203259 -0.4696505 -0.41432953))





(plot-timed-data (list (loop for current in `(-73.05118 -72.918274 -72.82033 -72.692085 -68.00288 -48.66243 -16.080643
					      -2.9574852 -0.75650215 -0.5203259 -0.4696505 -0.41432953
					      -0.41432953 -0.41432953 -0.41432953 -0.41432953 -0.41432953 -0.41432953)
			     for voltage in '(-130.0 -120.0 -110.0 -100.0 -90.0 -80.0 -70.0 -60.0 -50.0 -40.0 -30.0 -20.0
					      -10 0 10 20 30 40)
			     collect (/ (- current -0.41432953) -73.05118))
		       (loop for g in '(1.6901228e-5 1.7895418e-5 2.909243e-4 4.922708e-4 0.0016713756 0.0031241856
					0.0066355546 0.031730566 0.119857244 0.31402797 0.60000604 0.9110553
					1.2069156 1.4586552 1.6676381 1.8083133 1.9774994 1.9999996)
			     collect (/ (- g 1.6901228e-5) 1.9999996)))
		 (list "SS-in" "SS-act")
		 '(-130.0 -120.0 -110.0 -100.0 -90.0 -80.0 -70.0 -60.0 -50.0 -40.0 -30.0 -20.0
		   -10.0 0.0 10.0 20.0 30.0 40.0)
		 :title "Steady State Properties of KB94 I_Na Model")



;; these list are over volts (-130.0 -120.0 -110.0 -100.0 -90.0 -80.0 -70.0 -60.0 -50.0 -40.0 -30.0
;; -20.0-10.0 0.0 10.0 20.0 30.0 40.0) 
(defvar *kb-94-ss-inactivation* 
  '(0.9943282 0.9925089 0.99116814 0.9894126 0.92522186 0.66046983 0.21445668
  0.03481334 0.0046840124 0.0014509879 7.5729066e-4 -0.0 -0.0 -0.0 -0.0 -0.0
    -0.0 -0.0))
(defvar *kb-94-ss-activation* 
  '(0.0 4.9709535e-7 1.3701156e-4 2.3768483e-4 8.2723737e-4 0.0015536424
    0.0033093274 0.015856836 0.059920184 0.15700556 0.29999462 0.4555193
    0.60344946 0.7293193 0.83381075 0.90414834 0.9887414 0.99999154))



;; for 25mv shift in exponentials, arg x2
(defvar *kb-94-ss-inactivation-25-2-volts* 
  '(-130.0 -120.0 -110.0 -100.0 -90.0 -80.0 -70.0 -60.0 -50.0 -40.0 -30.0))
(defvar *kb-94-ss-inactivation-25-2*
  (loop for val in 
	'(-168.75652 -169.71024 -170.69829 -171.6248 -172.23273 -171.97237 -159.52426
	  -37.76805 -1.4054661 -0.8291315 -0.7490473)
	collect (/ (- val -0.7490473) -168.75652)))


(defvar *kb-94-ss-activation-25-2-volts* 
  '(-80.0 -70.0 -60.0 -50.0 -40.0 -30.0 -20.0 -10.0 0.0 10.0 20.0 30.0))
(defvar *kb-94-ss-activation-25-2*
  (loop for val in 
	'(1.50112e-5 2.339994e-4 0.009459437 0.18833919 0.9430871 1.9048992 2.6276634
	  3.1474388 3.342247 3.7275193 3.7087286 3.623633)
	collect (/ (- val 1.50112e-5) 3.623633)))











figure 2


(let ((user-stop-time 20) *overlay-plots* (*ACCOMODATE-OVERLAYS* t)
      (*vclamp-default-magnitude -110.0))
  (loop for recovery-magnitude in '(-190.0 -70.0 -50.0) do
	(setq *CREATE-NEW-SIMULATION-PLOTS* t *overlay-plots* nil)
	(loop for recovery-time from 0.1 to 2 by 0.1 do 
	      (add-pulse-list *vsource*
			      (list '(1.0 6.0 20.0)
				    (list 6.0 (+ 6.0 recovery-time) recovery-magnitude)
				    (list (+ 6.0 recovery-time) 10.0 20.0)))
	      (goferit)(setq *overlay-plots* t))))


|#




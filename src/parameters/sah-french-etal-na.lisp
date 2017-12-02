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

;; "The Sodium Current Underlying Action Potentials in Guinea Pig Hippocampal CA1 Neurons" Sah, P,.
;; Gibb, A. J., and Gage P. W., J of General Physiology, v91, 1988 p373-398
;;
;; Dissociated cells, highly variable Rin (100 - 1000Mohm) and Co (5 - 50pF), suggesting a 10-fold
;; range of cell areas.

;; patch electrodes filled with 150mMKF - average  erest -40 - they suggest shunt from cut off processes
;; they estimate rm at 6kohm cm2
;;
;; negative shift of curves possible with -
;;        low temp
;;        high F in  solution
;;
;; they say that a measured peak current of 20-30nA can account for the dvdt of spikes. however, this
;; peak was measured from a holding potential of -100 (cf. Fig 4 and 5), where inactivation will be
;; much more removed than that from the normal erest, according to their curves, which puts the
;; inactivation at ~0.3 @ -70mV
;;
;; Note that the curves shown in fig. 10 are significantly different than the parameters reported in
;; the text for pooled data.
(channel-type-def
 '(na-sgg
   (gbar . 1.2)				;uS, one example fig7 at 22 deg - same cell (?) at 18 deg
					;has gbar = .4uS

   ;; a figure of (gbar-density . 110.0) pS/um2 =  11mS/cm2 is quoted in Fre-Sah-Buc-Gag-90, p 1154
   (e-rev . 65.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.0)				; ??
   (reference-temp . 22.0)
   (v-particles . ((nam-sgg 1) (nah-sgg 1)))))


(channel-type-def
 '(na-sgg-fig10
   (parent-type . na-sgg)
   (v-particles . ((nam-sgg-fig10 1) (naH-sgg-fig10 1)))))


(channel-type-def
 '(na-sgg-power-ext
   (parent-type . na-sgg)
   (v-particles . ((nam-sgg-power-3 3) (nah-sgg-power-2 2)))))


(channel-type-def
 '(na-sgg-shifted-power-ext
   (parent-type . na-sgg)
   (v-particles . ((nam-sgg-shifted-power-3 3) (nah-sgg-shifted-power-2 2)))))	


(channel-type-def
 '(na-sgg-mpower-ext
   (parent-type . na-sgg)
   (v-particles . ((nam-sgg-power-3 3) (nah-sgg 1)))))	


(channel-type-def
 '(na-2-in-comp-sgg
   (parent-type . na-sgg)
   (v-particles . ((nam-sgg 1) (nah-sgg 1) (Nah2-sgg 1)))))	


#|
(defvar sgg-volts (loop for voltage from -100 to 50 by 0.1 collect voltage))

(defvar nam-sgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (/ 1.0 (+ 1 (exp (/ (- -39 voltage) 6.6)))))))

(defvar nam-sgg-ss-m-gsgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (-  (/ 1.0 (+ 1 (exp (/ (- -39 voltage) 6.6))))
				    (* 0.01 (/ 1.0 (+ 1 (exp (/ (- -50 voltage) 2.8)))))))))

(defvar scaled-gsgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (* 0.01 (/ 1.0 (+ 1 (exp (/ (- -50 voltage) 9))))))))
(defvar total-sgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (+ (*
				     (/ 1.0 (+ 1 (exp (/ (- -39 voltage) 6.6))))
				     (/ 1.0 (+ 1 (exp (/ (- voltage -75) 7.7))))

				     (/ 1.0 (+ 1 (exp (/ (- voltage -40) 7.7)))))
				    (* 0.01 (/ 1.0 (+ 1 (exp (/ (- ; -50
								 -45
								 voltage) 2.8)))))))))
(defvar sgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (*
				  (/ 1.0 (+ 1 (exp (/ (- -39 voltage) 6.6))))
				  (/ 1.0 (+ 1 (exp (/ (- voltage -75) 7.7))))

				  (/ 1.0 (+ 1 (exp (/ (- voltage -40) 7.7))))))))


(defvar gsgg-ss-minus-sgg-window
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (/ (- (*  0.016 (/ 1.0 (+ 1 (exp (/ (- -50 voltage) 9)))))
				       (*
					(/ 1.0 (+ 1 (exp (/ (- -39 voltage) 6.6))))
					(/ 1.0 (+ 1 (exp (/ (- voltage -75) 7.7))))
					(/ 1.0 (+ 1 (exp (/ (- voltage -40) 7.7))))))
				    0.015999762))))
;; From Fig 8A, curve of time to peak at 18C
;; If we assume a single exponential then tau ~= 0.333 time-to-peak
(defvar nam-sgg-tau-eye-volts (loop for volts from -50.0 to 40 by 10 collect volts))
(defvar nam-sgg-tau-eye-time-to-peak-18 '(2.0 1.8 1.8 1.3 1.2 .9 .7 .6 .6 .5))
(defvar nam-sgg-tau-eye-time-to-peak-22 '(1.2 .8 .5 .4 .4 .38 .35 .32 .3 .3))
|#

;; fitting paper's boltzman fit to the 3rd power of the hh boltzman
(particle-type-def
 `(nam-sgg-power-3
   (class . :hh-ext)
   (v-half . -50.799202)
   (valence . 3.0503156)
	
   ;; following fitted with fig8a 22degrees used as tau curve directly
   (base-rate . 0.55515987)
   (gamma . 0.86573625)
   (tau-0 . 0.3317309)))



(particle-type-def
 `(nam-sgg-shifted-power-3
   (parent-type . nam-sgg-power-3)
   (v-half . -40.799202)))


;; fitting paper's boltzman fit to the 2nd power of the hh boltzman
(particle-type-def
 `(nah-sgg-power-2
   (class . :hh-ext)
   (v-half . -66.39991)
   (valence . -2.7539392)
   (base-rate . 2.5e-3)
   (gamma . 0.2)
   (tau-0 . 1.0)))

(particle-type-def
 `(nah-sgg-shifted-power-2
   (parent-type . nah-sgg-power-2)
   (v-half . -56.39991)))	

(particle-type-quoted-def
 `(nam-sgg
   (class . :hh-ext)
	
   ;; Average over 8 cells
   (VALENCE . ,(VALENCE-FROM-K 6.6 (+ 273 22))) ; = 3.8516855
   (V-HALF . -39.0)		


   ;; following fitted with fig8a 18 deg used as tau curve * 3
	
					; (base-rate . 1.1)
					; (gamma . 0.52)
					; (tau-0 . 0.22)

   ;; following fitted with fig8a 22degrees used as tau curve directly

   (base-rate . 1.0)
   (gamma . 1.0)
   (tau-0 . .3)
   ;; following fitted with fig8a 18 deg used as tau curve directly
	
   ;;	(base-rate . 0.35)
   ;;	(gamma . 0.52)
   ;;	(tau-0 . 0.66)

   (QTEN . 1.0)
					; (Fixed-boltzmann-reference-temperature . 18.0)
   (reference-temp . 18.0)))

(particle-type-def
 `(nam-sgg-fig10
   (parent-type . nam-sgg)

   (VALENCE . 3.5)			; taken from traub 91
   (V-HALF . -34.0)))




;; From Fig 8B, curve of half decay time at 18C
;; If we assume a single exponential then tau ~= 0.666 half decay time
#|
(defvar nah-sgg-tau-eye-volts (loop for volts from -50.0 to 40 by 10 collect volts))
(defvar nah-sgg-tau-eye-half-decay-time-18 '(7 10 9 6 4 3.2 3 2.7 2.2 2))
(defvar nah-sgg-tau-eye-half-decay-time-22 '(3 8 4 3 2 1.8 1.7 1.5 1.5 1.5))





(defvar nah-sgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (/ 1.0 (+ 1 (exp (/ (- voltage -75) 7.7)))))))

(defvar nah2-sgg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (/ 1.0 (+ 1 (exp (/ (- voltage -40) 7.7)))))))

|#

#|
(plot-xy-data `(((-120 -110 -100 -90 -80 -70 -60 -50 -40) (53  53 53 48 36 19 7 2 .4))
		( ,(loop for voltage from -100 to 50 by 0.1 collect voltage)
		  ,nah-sgg-ss)))
|#

;; hypothetical inactivation partilce to account for the strong peak in the half decay time at -40.
(particle-type-quoted-def
 `(nah2-sgg
   (class . :hh-ext)		
   (VALENCE . ,(VALENCE-FROM-K -8 (+ 273 22)))
	
   (V-HALF . -40.0)

   ;; following fitted with fig8b used as tau curve * 1.67

   (base-rate . 0.1)
   (gamma . 0.6)
   (tau-0 . 1.0)
	
   ;; following fitted with fig8b used as tau curve directly

   ;; (base-rate . 0.065)
   ;; (gamma . 0.56)
   ;; (tau-0 . 1.8)
))


(particle-type-quoted-def
 `(nah2-sgg
   (class . :hh-ext)		
   (VALENCE . ,(/ 25 -3))	
   (V-HALF . -40.0)

   ;; following fitted with fig8b at 22 degrees used as tau curve directly

   (base-rate . 0.08)
   (gamma . 0.6)
   (tau-0 . 1.6)))
	



(particle-type-quoted-def
 `(nah-sgg
   (class . :hh-ext)

   ;; Average over 8 cells
   (VALENCE . ,(VALENCE-FROM-K -7.7 (+ 273 22))) ; -3.3015792
   (V-HALF . -75.0)		

   ;; These set to account for the different time courses of inactivation recovery in fig. 11,
   ;; and to have the tau ~< 10ms at large depolarizations.
   (gamma . 0.0)
   (qten . 1.0)
   (reference-temp . 22.0)
   (base-rate . 0.04)
   (tau-0 . 5.0)))

(particle-type-quoted-def
 `(nah-sgg-fig10
   (parent-type . nah-sgg)

   (VALENCE . ,(VALENCE-FROM-K -9.9 (+ 273 22))) ; -2.567895
   (V-HALF . -89.5)))		


;; fig 11
#|
(let ((user-stop-time 200) *overlay-plots* (*ACCOMODATE-OVERLAYS* t))
  (loop for *vclamp-default-magnitude in '(-100.0) do
	(loop for delay in '(10 30 50 70) do 
	      (add-pulse-list *vsource*
			      (list (list (- 70.01 delay) (+ 100.0 (- 70.0 delay)) 0.0) `(170 180 -20.0)))
	      (goferit)(setq *overlay-plots* t))))

(let ((user-stop-time 300) *overlay-plots* (*ACCOMODATE-OVERLAYS* t))
  (loop for *vclamp-default-magnitude in '(-120.0 -100.0 -80.0) do
	(loop for delay from 160.0 to 270 by 10 do 
	      (add-pulse-list *vsource*
			      (list '(50.0 150.0 0.0) (list delay (+ delay 10) -20.0)))
	      (goferit)(setq *overlay-plots* t))))
|#
;; French et al persistent sodium current
#|
@Article(Fre-Gag-82,
  Author  = {French, C. and Gage, P.},
  Title   = {A threshold sodium current in pyramidal cells in rat hippocampus},
  Journal = {Neuroscience Letters},
  Year    = {1985},
  Volume  = {56},
  Pages   = {289-293}
  )

@ARTICLE{Fre-Sah-Buc-Gag-90,
	  Author  = {French, C. and Sah, P. and Buckett, K. J. and  Gage, P.},
	TITLE = {A Voltage-dependent Persistent Sodium Current in Mammalian Hippocampal Neurons},
	JOURNAL = {Journal of General Physiology},
	YEAR = {1990},
	VOLUME = {95},
	PAGES = {1139-1157}

}

|#

;; there is some inactivation over the ranges -80 to -20, measured with a conditioning pulse of 200ms

(channel-type-def
 '(na-fsbg-slice
   (gbar-density . 3.01)		; pS/um2, areas estimated with cap 1uF/cm2 and for Slice cells gmax 7.8nS
   (e-rev . 30.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.0)
   (reference-temp . 22.0)		; 20-23 C
   (v-particles . ((nam-fsbg-slice 1)))))

(channel-type-def
 '(na-fsbg-diss
   (gbar-density . 1.7)			; pS/um2, calculated for dissociated cells w/gmax = 4.4nS, assuming
					; areas estimated with cap 1uF/cm2. Slice cells gmax 7.8nS
   (e-rev . 30.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.0)
   (reference-temp . 22.0)		; 20-23 C
   (v-particles . ((nam-fsbg-diss 1)))))

(channel-type-def
 '(na-fsbg-minus-window
   (gbar-density . 1.7)			; pS/um2, calculated for dissociated cells w/gmax = 4.4nS, assuming
					; areas estimated with cap 1uF/cm2. Slice cells gmax 7.8nS
   (e-rev . 30.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.0)
   (reference-temp . 22.0)		; 20-23 C
   (v-particles . ((m-fsbg-minus-window 1)))))


(defvar m-fsbg-ss
  (s-flt-array (loop for voltage from -100 to 50 by 0.1 collect
				 (/ 1.0 (+ 1 (exp (/ (- -50 voltage) (/ 25 9))))))))
(particle-type-quoted-def
 `(nam-fsbg-diss
   (class . :hh-ext)
   (VALENCE . ,(/ 25 9.0))		; k = 4.5mV old paper, 5mV slice, 9mV dissociated
   (GAMMA . 0.5)
   (V-HALF . -49.0)			; dissociated cells, -49.0 for slice cells, -49 old paper
   (TAU-0 . 1.0)			; Although earlier paper shows 10ms tau between -45 and -36, the
					; figures in the second paper suggests that this may be amplifier artifact
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . t)
   (BASE-RATE . 1.0)
   (QTEN . 1.0)
   (reference-temp . 22.0)))


(particle-type-quoted-def
 `(nam-fsbg-slice
   (class . :hh-ext)
   (VALENCE . ,(/ 25 5.0))		; k = 4.5mV old paper, 5mV slice, 9mV dissociated
   (GAMMA . 0.5)
   (V-HALF . -49.0)			; dissociated cells, -49.0 for slice cells, -49 old paper
   (TAU-0 . 1.0)			; Although earlier paper shows 10ms tau between -45 and -36, the
					; figures in the second paper suggests that this may be amplifier artifact
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . t)
   (BASE-RATE . 1.0)
   (QTEN . 1.0)
   (reference-temp . 22.0)))


(particle-type-def
 `(m-fsbg-minus-window
   (class . :hh-ext)
   (v-half . -42)
   (valence . 4.8)

   (GAMMA . 0.5)
   (V-HALF . -50.0)			; dissociated cells, -49.0 for slice cells, -49 old paper
   (TAU-0 . 1.0)			; Although earlier paper shows 10ms tau between -45 and -36, the
					; figures in the second paper suggests that this may be amplifier artifact
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . t)
   (BASE-RATE . 1.0)
   (QTEN . 1.0)
   (reference-temp . 22.0)))





;; *******************  *******************  *******************  *******************


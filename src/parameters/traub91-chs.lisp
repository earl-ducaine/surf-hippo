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

;; traub91.channels.lisp
;; from the article :
;; A Model of a CA3 Hippocampal Pyramidal Neuron Incorporating
;;    Voltage-Clamp Data on Intrinsic Conductances
;; RD. Traub, RKS. Wong, R. Miles, H. Michelson
;; J. Neurophysiol. 1991, vol. 66, no. 2, 635-650.
;; ----------------------------------------------------------------
;; kinetics is given in terms of alpha- and beta- functions
;; gbar-density is given in mS per cm2rr
;; => article value is multiplied by 10 to yield pS per um2
;;
;; in this model all channel densities are functions of the compartments
;;
;; na-trb91
;; kdr-trb91
;; ahp-trb91
;; kc-trb91
;; ka-trb91
;; ca-trb91
;;
;; Sodium channel na-trb91
;; data taken from Sah et al. 1988
;;

(channel-type-def
 '(na-trb91
   (gbar-density . 300.0)	        
   (e-rev . 55.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-trb91 2)(nah-trb91 1)))))


(channel-type-def
 '(na-trb91-ext
   (gbar-density . 300.0)	        
   (e-rev . 55.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-trb91-ext 2)(nah-trb91-ext 1)))))



(defun nam-trb91-alpha-function (voltage)
  (let ((d (- 13.1 (+ 60.0 voltage) )))
    (/ (* 0.32 d)
       (- (the sf (exp (/ d 4.0))) 1.0))))

(defun nam-trb91-beta-function (voltage)
  ( let ((d (- (+ 60.0 voltage)  40.1)))
    (/ (* 0.28 d)
       (- (the sf (exp (/ d 5.0))) 1.0))))

(particle-type-def
 `(nam-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . nam-trb91-alpha-function)
   (beta . nam-trb91-beta-function)))

(particle-type-def
 '(nam-trb91-ext
   (class . :hh-ext)
   (VALENCE . 3.5)
   (GAMMA . 0.57)
   (BASE-RATE . 6.1)
   (V-HALF . -34.0)
   (TAU-0 . 0.05)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


;; claim that this follows sah et al 88, but "shifted to the right to account for higher temp" .
;; However, fits to sah et al 88 give - 
;;
;; POOLED DATA
;;
;;   (VALENCE . ,(VALENCE-FROM-K -7.7 (+ 273 22))) ; -3.3015792
;;   (V-HALF . -75.0)
;;
;; FIG 10
;;
;;   (VALENCE . ,(VALENCE-FROM-K -9.9 (+ 273 22))) ; -2.567895
;;   (V-HALF . -89.5)))
;;

(particle-type-def
 '(nah-trb91-ext
   (class . :hh-ext)
   (VALENCE . -6.4)
   (GAMMA . 0.23)
   (BASE-RATE . 0.1)
   (V-HALF . -38.0)
   (TAU-0 . 0.18)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


(defun nah-trb91-alpha-function (voltage)
  (* 0.128 (the sf (exp (/ (- 17.0 (+ 60.0 voltage) ) 18.0)))))

(defun nah-trb91-beta-function (voltage)
  (/ 4.0 (+ 1.0 (the sf (exp (/ (- 40.0 (+ 60.0 voltage) ) 5.0))))))

(particle-type-def
 `(nah-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . nah-trb91-alpha-function)
   (beta . nah-trb91-beta-function)))

;; ------------------------------------------------------------------------
;; Calcium channel ca-trb91
;; data taken from Kay and Wong 1987
;; 

(channel-type-def
 '(ca-trb91
   (gbar-density . 40.0)                        
   (e-rev . 80.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((CA 1.0)))
   (conc-int-type-params . ((ca-in-traub91 (1 1))))
   (v-particles . ((cas-trb91 2)(car-trb91 1)))))    

(channel-type-def
 '(ca-trb91-ext
   (gbar-density . 40.0)                        
   (e-rev . 80.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((CA 1.0)))
   (conc-int-type-params . ((ca-in-traub91 (1 1))))
   (v-particles . ((cas-trb91-ext 2)(car-trb91-ext 1)))))    

(defun cas-trb91-alpha-function (voltage)
  (/ 1.6 
     (+ 1.0 (the sf (exp (* -0.072 (- (+ voltage 60.0) 65.0)))))))

(defun cas-trb91-beta-function (voltage)
  (/ (* 0.02 (- (+ voltage 60.0) 51.1))
     (- (the sf (exp (/ (- (+ voltage 60.0) 51.1) 5.0))) 1.0)))

(particle-type-def
 `(cas-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . cas-trb91-alpha-function)    
   (beta . cas-trb91-beta-function)))

(particle-type-def
 '(cas-trb91-ext
   (class . :hh-ext)
   (VALENCE . 3.3)
   (GAMMA . 0.51)
   (BASE-RATE . 0.33)
   (V-HALF . -20.0)
   (TAU-0 . 0.64)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))

(particle-type-def
 '(car-trb91-ext
   (class . :hh-ext)
   (base-rate-undefined . t)
   (VALENCE . -2.8)
   (GAMMA . 0.5)
   (BASE-RATE . 1.0)
   (V-HALF . -43.0)
   (TAU-0 . 200.0)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))

(defun car-trb91-alpha-function (voltage)
  (if (> (+ voltage 60.0) 0.0)
      (/ (the sf (exp (/ (+ voltage 60.0) -20.0))) 200.0)
      0.005))

(defun car-trb91-beta-function (voltage)
  (if (> (+ voltage 60.0) 0.0)				 
      (- 0.005 (/ (the sf (exp (/ (+ voltage 60.0) -20.0))) 200.0))
      0.0))

(particle-type-def
 `(car-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . car-trb91-alpha-function)    
   (beta . car-trb91-beta-function)))


;; ------------------------------------------------------------------------
;; Potassium channel kdr-trb91
;; delayed rectifier
;; 
;;

(channel-type-def
 '(kdr-trb91
   (gbar-density . 150.0)	            
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kdrn-trb91 1)))))	    


(channel-type-def
 '(kdr-trb91-ext
   (gbar-density . 150.0)	            
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kdrn-trb91-ext 1)))))	    


(particle-type-def
 '(kdrn-trb91-ext
   (class . :hh-ext)
   (VALENCE . 2.5)
   (GAMMA . 0.72)
   (BASE-RATE . 0.17)
   (V-HALF . -16.0)
   (TAU-0 . 0.63)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


(defun kdrn-trb91-alpha-function (voltage)
  (/ (* 0.016 (- 35.1 (+ voltage 60.0))) 
     (- (the sf (exp (/ (- 35.1 (+ voltage 60.0)) 5.0))) 1.0)))

(defun kdrn-trb91-beta-function (voltage)
  (* 0.25 (the sf (exp (/ (- 20.0 (+ voltage 60.0)) 40.0)))))

(particle-type-def
 `(kdrn-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kdrn-trb91-alpha-function)    
   (beta . kdrn-trb91-beta-function)))


;; ------------------------------------------------------------------------

;; Iahp

(channel-type-def
 '(ahp-trb91
   (gbar-density . 2)
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)
   (reference-temp . 30.0)
   (conc-particles . ((ahpq-trb91 1)))))

(conc-particle-type-def
 '(ahpq-trb91
   (class . :nth-order)
   (alpha . 0.2e-4)
   (beta . 0.001)
   (tau-0 . 100.0)
   (power . 1)
   (QTEN . 1)
   (reference-temp . 30.0)
   (conc-int-type . ca-in-traub91)
   (shell . 1)))


;; ------------------------------------------------------------------------
;; C potassium channel kc-trb91
;; data taken from Adams et al 1982 (bullfrog...)
;; no inactivation particle
;;

(channel-type-def
 '(kc-trb91
   (gbar-density . 100.0)	          
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kcc-trb91 1)))
   (conc-particles . ((kcw-trb91 1)))))

;; KCC-TRB91-EXT not a very good fit..
(channel-type-def
 '(kc-trb91-ext
   (gbar-density . 100.0)	          
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kcc-trb91-ext 1)))
   (conc-particles . ((kcw-trb91 1)))))

;; not a very good fit....
(particle-type-def
 '(kcc-trb91-ext
   (class . :hh-ext)
   (VALENCE . 2.5)
   (GAMMA . 0.0)
   (BASE-RATE . 0.11)
   (V-HALF . -18.0)
   (TAU-0 . 0.0)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


(conc-particle-type-quoted-def
 `(kcw-trb91
   (class . :static-linear)
   (QTEN . 1)          	 
   (power . 1)
   (concentration-slope . ,(/ 1.0 250.0))
   (concentration-min . 1.0)
   (conc-int-type . ca-in-traub91)
   (shell . 1)))

(defun alpha-c (voltage)
  (/  (exp (- (/ (- (+ 60.0 voltage)  10.0) 11.0)
	      (/ (- (+ 60.0 voltage)  6.5) 27)))
      18.975))

(defun beta-c (voltage)
  (* 2.0 (the sf (exp (/ (- 6.5 (+ 60.0 voltage))  27.0)))))

(defun kcc-trb91-alpha-function (voltage)
  (if (<= voltage -10.0) 
      (alpha-c voltage)
      (beta-c voltage)))

(defun kcc-trb91-beta-function (voltage)
  (if (< voltage -10.0)
      (- (beta-c voltage) (alpha-c voltage))
      0.0))

(particle-type-def
 `(kcc-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kcc-trb91-alpha-function)    
   (beta . kcc-trb91-beta-function)))

;; ------------------------------------------------------------------------
;; K,A Potassium channel ka-trb91
;; data taken from Numann et al. 1987
;;

(channel-type-def
 '(ka-trb91
   (gbar-density . 40.0)	 
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1)          	 
   (reference-temp . 22.0) 	
   (v-particles . ((kaa-trb91 1)(kab-trb91 1)))))   


(channel-type-def
 '(ka-trb91-ext
   (gbar-density . 40.0)	 
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1)          	 
   (reference-temp . 22.0) 	
   (v-particles . ((kaa-trb91-ext 1)(kab-trb91-ext 1)))))   


(particle-type-def
 '(kaa-trb91-ext
   (class . :hh-ext)
   (VALENCE . 2.1)
   (GAMMA . 0.54)
   (BASE-RATE . 0.53)
   (V-HALF . -35.0)
   (TAU-0 . 0.56)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


(particle-type-def
 '(kab-trb91-ext
   (class . :hh-ext)
   (VALENCE . -6.4)
   (GAMMA . 0.22)
   (BASE-RATE . 0.0013)
   (V-HALF . -68.0)
   (TAU-0 . 18.0)
   (QTEN . 1.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


(defun kaa-trb91-alpha-function (voltage)
  (/ (* 0.02 (- 13.1 (+ voltage 60.0)  )) 
     (- (the sf (exp (/ (- 13.1 (+ voltage 60.0)  ) 10.0))) 1.0)))

(defun kaa-trb91-beta-function (voltage)
  (/ (* 0.0175 (- (+ voltage 60.0) 40.1))
     (- (the sf (exp (/ (- (+ voltage 60.0) 40.1) 10.0))) 1.0)))

(particle-type-def
 `(kaa-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kaa-trb91-alpha-function)    
   (beta . kaa-trb91-beta-function)))


(defun kab-trb91-alpha-function (voltage)
  (* 0.0016 (the sf (exp (/ (- -13.0 (+ voltage 60.0)) 18.0)))))

(defun kab-trb91-beta-function (voltage)
  (/ 0.05 (+ 1.0 (the sf (exp (/ (- 10.1 (+ voltage 60.0)) 5.0))))))

(particle-type-def
 `(kab-trb91
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kab-trb91-alpha-function)    
   (beta . kab-trb91-beta-function)))
    

;; Simple Ca++ concentration integrator.

(conc-int-type-def
 `(ca-in-traub91
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)
   (tau . 13.33)			; ms 
   (juxtamembrane-shell-thickness . 1.0) ; microns - this will be adjusted for each compartment
   (core-conc . 0.0e-5))		; mM
 )








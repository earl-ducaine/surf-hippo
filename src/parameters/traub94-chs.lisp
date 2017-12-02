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
;; traub94-channels.lisp
;; from the article :
@ARTICLE{Tra-Jef-Mil-Whi-Tot-94,
	AUTHOR = {Traub, R. D. and Jefferys, J. G. R. and Miles, R. and Whittington, M. A. and T\'oth, K.},
	TITLE = {A branching dendritic model of a rodent {CA3} pyramidal neurone},
	JOURNAL = {Journal of Physiology},
	YEAR = {1994},
	VOLUME = {481},
	NUMBER = {1}

which in turn references

@ARTICLE{Tra-Won-Mil-Mic-91,
	AUTHOR = {Traub, R. D. and Wong, R. K. S. and Miles, R. and Michelson, H.},
	TITLE = {A model of a {CA3} hippocampal pyramidal neuron incorporating voltage-clamp data on intrinsic conductances},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1991},
	VOLUME = {66},
	NUMBER = {2},
	MONTH = {August}
}
|#

;; kinetics is given in terms of alpha- and beta- functions
;;   probleme a regler avec les concentration de calcium interne.
;;  et pouvoir metre le calcium comme variable...
;; 
;; gbar-density is given in mS per cm2rr
;; => article value is multiplied by 10 to yield pS per um2
;;
;; in this model all channel densities are functions of the compartments
;;
;; na-trb94
;; kdr-trb94
;; ahp-trb94
;; kc-trb94
;; ka-trb94
;; ca-trb94
;;
;;
;; Sodium channel na-trb94 is the same that na-trb91...
;; data taken from Sah et al. 1988
;;
;; the voltage in the original equations is referenced to the "resting potential", which we will
;; take here to be -60.0mV

(channel-type-def
 '(na-trb94
   (gbar-density . 300.0)	        
   (e-rev . 55.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-trb94 2)(nah-trb94 1)))))

(defun nam-trb94-alpha-function (voltage)
  (let ((d (- 13.1 (+ 60.0 voltage) )))
    (/ (* 0.32 d)
       (- (exp (/ d 4.0)) 1.0))))

(defun nam-trb94-beta-function (voltage)
  ( let ((d (- (+ 60.0 voltage)  40.1)))
    (/ (* 0.28 d)
       (- (exp (/ d 5.0)) 1.0))))

(particle-type-def
 `(nam-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . nam-trb94-alpha-function)    
   (beta . nam-trb94-beta-function)))    


(defun nah-trb94-alpha-function (voltage)
  (* 0.128 (exp (/ (- 17.0 (+ 60.0 voltage) ) 18.0))))

(defun nah-trb94-beta-function (voltage)
  (/ 4.0 (+ 1.0 (exp (/ (- 40.0 (+ 60.0 voltage) ) 5.0)))))

(particle-type-def
 `(nah-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . nah-trb94-alpha-function)    
   (beta . nah-trb94-beta-function)))    

;; ------------------------------------------------------------------------
;; sodium channel for the IS and the axon
;;

(channel-type-def
 '(na-ax-trb94
   (gbar-density . 300.0)	        
   (e-rev . 55.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((na-ax-m-trb94 3)(na-ax-h-trb94 1)))))


(defun na-ax-m-trb94-alpha-function (voltage)
  (let ((d (- 17.2 (+ voltage 60.0))))
    (/ (* 0.8 d)
       (- (exp (/ d 4.0)) 1.0))))

(defun na-ax-m-trb94-beta-function (voltage)
  ( let ((d (- (+ voltage 60.0) 42.2)))
    (/ (* 0.7 d)
       (- (exp (/ d 5.0)) 1.0))))

(particle-type-def
 `(na-ax-m-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . na-ax-m-trb94-alpha-function)    
   (beta . na-ax-m-trb94-beta-function)))    


(defun na-ax-h-trb94-alpha-function (voltage)
  (* 0.32 (exp (/ (- 42.0 (+ voltage 60.0)) 18.0))))

(defun na-ax-h-trb94-beta-function (voltage)
  (/ 10.0 (+ 1.0 (exp (/ (- 42.0 (+ voltage 60.0)) 5.0)))))

(particle-type-def
 `(na-ax-h-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . na-ax-h-trb94-alpha-function)    
   (beta . na-ax-h-trb94-beta-function)))


;; ------------------------------------------------------------------------
;; Calcium channel ca-trb94
;; data taken from Kay and Wong 1987
;;  the same that ca-trb91 but without inactivation car-trb94...

(channel-type-def
 '(ca-soma-trb94
   (gbar-density . 40.0)                        
   (e-rev . 80.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((CA 1.0)))
   (conc-int-type-params . ((ca-in-soma-traub94 (1 1))))
   (v-particles . ((cas-trb94 2)))))    


(channel-type-def
 '(ca-dendrite-trb94
   (gbar-density . 40.0)                        
   (e-rev . 80.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((CA 1.0)))
   (conc-int-type-params . ((ca-in-dendrite-traub94 (1 1))))
   (v-particles . ((cas-trb94 2)))))


(defun cas-trb94-alpha-function (voltage)
  (/ 1.6 
     (+ 1.0 (exp (* -0.072 (- (+ 60.0 voltage)  65.0))))))

(defun cas-trb94-beta-function (voltage)
  (/ (* 0.02 (- (+ 60.0 voltage)  51.1))
     (- (exp (/ (- (+ 60.0 voltage)  51.1) 5.0)) 1.0)))

(particle-type-def
 `(cas-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . cas-trb94-alpha-function)    
   (beta . cas-trb94-beta-function)))

;; ------------------------------------------------------------------------
;; Potassium channel kdr-trb94
;; delayed rectifier
;; data taken from Sah et al. 1988 and Numann et al. 1987
;; same that kdr-trb91

(channel-type-def
 '(kdr-trb94
   (gbar-density . 150.0)	            
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kdrn-trb94 2)))))	    


(defun kdrn-trb94-alpha-function (voltage)
  (/ (* 0.016 (- 35.1 (+ 60.0 voltage) )) 
     (- (exp (/ (- 35.1 (+ 60.0 voltage) ) 5.0)) 1.0)))

(defun kdrn-trb94-beta-function (voltage)
  (* 0.25 (exp (/ (- 20.0 (+ 60.0 voltage) ) 40.0))))

(particle-type-def
 `(kdrn-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kdrn-trb94-alpha-function)    
   (beta . kdrn-trb94-beta-function)))
    


;; ------------------------------------------------------------------------
;; Potassium channel kdr-ax-trb94
;; delayed rectifier for the IS and the axon
;; 
;;

(channel-type-def
 '(kdr-ax-trb94
   (gbar-density . 150.0)	            
					; (e-rev . -75.0)
   (e-rev . -85.0)			; traub puts the axonal e-k lower than soma, although he
					; mentions that having the same e-k also works
   (use-defined-e-rev . t)

   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1)          	 
   (v-particles . ((kdrn-ax-trb94 4)))))	    


(defun kdrn-ax-trb94-alpha-function (voltage)
  (/ (* 0.03 (- 17.2 (+ voltage 60.0))) 
     (- (exp (/ (- 17.2 (+ voltage 60.0)) 5.0)) 1.0)))

(defun kdrn-ax-trb94-beta-function (voltage)
  (* 0.45 (exp (/ (- 12.0 (+ voltage 60.0)) 40.0))))

(particle-type-def
 `(kdrn-ax-trb94
   (class . :hh)
   (QTEN . 1)          	 
   (alpha . kdrn-ax-trb94-alpha-function)    
   (beta . kdrn-ax-trb94-beta-function)))


;; ------------------------------------------------------------------------

;; Iahp

(channel-type-def
 '(ahp-soma-trb94
   (gbar-density . 2)
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (reference-temp . 30.0)
   (conc-particles . ((ahpq-soma-trb94 1)))))


(channel-type-def
 '(ahp-dendrite-trb94
   (gbar-density . 2)
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)
   (reference-temp . 30.0)
   (conc-particles . ((ahpq-dendrite-trb94 1)))))


(conc-particle-type-def
 '(ahpq-soma-trb94
   (class . :nth-order)
   (alpha . 0.2e-4)
   (beta . 0.001)
   (tau-0 . 100.0)
   (power . 1)
   (QTEN . 1)
   (reference-temp . 30.0)
   (conc-int-type . ca-in-soma-traub94)
   (shell . 1)))


(conc-particle-type-def
 '(ahpq-dendrite-trb94
   (class . :nth-order)
   (alpha . 0.2e-4)
   (beta . 0.001)
   (tau-0 . 100.0)
   (power . 1)
   (QTEN . 1)
   (reference-temp . 30.0)
   (conc-int-type . ca-in-dendrite-traub94)
   (shell . 1)))


;; ------------------------------------------------------------------------
;; C potassium channel kc-trb94
;; data taken from Adams et al 1982 (bullfrog...)
;; no inactivation particle
;;

(channel-type-def
 '(kc-soma-trb94
   (gbar-density . 100.0)	          
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kcc-trb94 1)))
   (conc-particles . ((kcw-soma-trb94 1)))))


(channel-type-def
 '(kc-dendrite-trb94
   (gbar-density . 100.0)	          
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (QTEN . 1.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((kcc-trb94 1)))
   (conc-particles . ((kcw-dendrite-trb94 1)))))


(defun kcw-conc-dependence (concentration)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (the df (min 1.0d0 (/ (the df concentration) 250.0))))

(conc-particle-type-quoted-def
 `(kcw-soma-trb94
   (class . :static-linear)
   (QTEN . 1.0)
   (power . 1)
   (concentration-slope . ,(/ 1.0 250.0))
   (concentration-min . 1.0)
   (conc-int-type . ca-in-soma-traub94)
   (shell . 1)))

(conc-particle-type-quoted-def
 `(kcw-dendrite-trb94
   (class . :static-linear)
   (power . 1)
   (QTEN . 1.0)
   (concentration-slope . ,(/ 1.0 250.0))
   (concentration-min . 1.0)
   (conc-int-type . ca-in-dendrite-traub94)
   (shell . 1)))


(defun alpha-c (voltage)
  (/ (exp (- (/ (- (+ 60.0 voltage)  10.0) 11.0)
	     (/ (- (+ 60.0 voltage)  6.5) 27)))
     18.975))

(defun beta-c (voltage)
  (* 2.0 (exp (/ (- 6.5 (+ 60.0 voltage))  27.0))))

(defun kcc-trb94-alpha-function (voltage)
  (if (<= voltage -10.0) 
      (alpha-c voltage)
      (beta-c voltage)))

(defun kcc-trb94-beta-function (voltage)
  (if (< voltage -10.0)
      (- (beta-c voltage) (alpha-c voltage))
      0.0))

(particle-type-def
 `(kcc-trb94
   (class . :hh)
   (QTEN . 1.0)
   (alpha . kcc-trb94-alpha-function)    
   (beta . kcc-trb94-beta-function)))


;; ------------------------------------------------------------------------
;; K,A Potassium channel ka-trb94
;; data taken from Numann et al. 1987
;;

(channel-type-def
 '(ka-trb94
   (gbar-density . 40.0)	 
   (e-rev . -75.0)
   (use-defined-e-rev . t)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1)          	 
   (reference-temp . 22.0) 	
   (v-particles . ((kaa-trb94 1)(kab-trb94 1)))))   


(defun kaa-trb94-alpha-function (voltage)
  (/ (* 0.02 (- 13.1 (+ 60.0 voltage) )) 
     (- (exp (/ (- 13.1 (+ 60.0 voltage) ) 10.0)) 1.0)))

(defun kaa-trb94-beta-function (voltage)
  (/ (* 0.0175 (- (+ 60.0 voltage)  40.1))
     (- (exp (/ (- (+ 60.0 voltage)  40.1) 10.0)) 1.0)))

(particle-type-def
 `(kaa-trb94
   (class . :hh)
   (alpha . kaa-trb94-alpha-function)    
   (beta . kaa-trb94-beta-function)))    


(defun kab-trb94-alpha-function (voltage)
  (* 0.0016 (exp (/ (- -13.0 (+ 60.0 voltage) ) 18.0))))

(defun kab-trb94-beta-function (voltage)
  (/ 0.05 (+ 1.0 (exp (/ (- 10.1 (+ 60.0 voltage) ) 5.0)))))

(particle-type-def
 `(kab-trb94
   (class . :hh)
   (QTEN . 1.0)
   (alpha . kab-trb94-alpha-function)    
   (beta . kab-trb94-beta-function)))


;; Simple Ca++ concentration integrator.

(conc-int-type-def
 `(ca-in-soma-traub94
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)
   (tau . 1000.0)			; ms 
   (juxtamembrane-shell-thickness . 1.0) ; microns - this will be adjusted for each compartment
   (core-conc . 0.0e-5))		; mM
 )

(conc-int-type-def
 `(ca-in-dendrite-traub94
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)
   (alpha-s . 1.0)
   (shell-2-p . nil)
   (shell-3-p . nil)
   (tau . 20.0)				; ms
   (juxtamembrane-shell-thickness . 1.0) ; microns - this will be adjusted for each compartment
   (core-conc . 0.0e-5))		; mM
 )


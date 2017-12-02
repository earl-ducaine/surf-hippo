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

;; Channels from :
;;   Computer simulations of a morphologically reconstructed
;;   CA3 hippocampal neuron
;;   M.Migliore, EP. Cook, DB. Jaffe, DA. Turner, D. Johnston
;;   J. Neurophysiol, March 1995, vol. 73, no. 3, 1157-1168
;;
;; ---------------------------------------------------------------
;; cet article recupere bcp de canaux de Traub et al. 1991


;; (load (concatenate 'string *Surf-parameters-dir* "traub91-channels.lisp"))

;; In this paper, some channel densities were varied for various cell anatomies - the figures given
;; in the definitions below are those used for the cell T39 bursting model.

(channel-type-def
 `(ca-l-mig95
   (parent-type . ca-l-jaffe)
   (conc-int-type-params . ((ca-in-mig-approx (1 1))))))

(channel-type-def
 `(ca-n-mig95
   (parent-type . ca-n-jaffe)
   (conc-int-type-params . ((ca-in-mig-approx (1 1))))))

(channel-type-def
 `(ca-t-mig95
   (parent-type . ca-t-jaffe)
   (conc-int-type-params . ((ca-in-mig-approx (1 1))))))



(channel-type-quoted-def
 `(na-mig95

   (gbar-density . ,(* 38		; mS/cm2  - range from 15 to 45
		     1.0e-3		; mS -> S
		     1.0e4))		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
   (e-rev . 50.0)			
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-trb91-ext 3)(nah-trb91-ext 1))) ; traub91 uses m2h
   ;; (v-particles . ((nam-trb91 3)(nah-trb91 1)))
   )
 )


(channel-type-quoted-def
 `(kdr-mig95
   (gbar-density . ,(* 15		; mS/cm2  - range from 7 to 30 (burst), 30 to 60 (non-burst)
		     1.0e-3		; mS -> S
		     1.0e4))		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
   (e-rev . -91.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (v-particles . ((kdrn-mig 3)(kdrl-mig 1)))))



;; Delayed rectifier K,DR channel activation particle
;; 
(particle-type-def
 `(kdrn-mig
   (class . :hh-ext)
   (VALENCE . 5)
   (GAMMA . 0.4)
   (BASE-RATE . 0.03)
   (V-HALF . -32.0)
   (TAU-0 . 0.0)
   (QTEN . 1))				; not given
 )


;; delayed rectifier potassium channel inactivation particle
;;
(particle-type-def
 `(kdrl-mig
   (class . :hh-ext)
   (VALENCE . -2)
   (GAMMA . 1.0)
   (BASE-RATE . 0.001)
   (V-HALF . -61.0)
   (TAU-0 . 0.0)
   (QTEN . 1))				; not given
 )


;;----------------------------------------------------
;; K,A potassium channel
;; data from migliore et al.
;;
(channel-type-quoted-def
 `(ka-mig95
   (gbar-density . ,(* 1		; mS/cm2  - range from ~0 to 5 (burst), 1 to 20 (non-burst)
		     1.0e-3		; mS -> S
		     1.0e4		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
		     ))
   (e-rev . -91.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (v-particles . ((kan-mig 1)(kal-mig 1)))))



;; K,A channel activation particle
;; 
(particle-type-def
 `(kan-mig
   (class . :hh-ext)
   (VALENCE . 3)
   (GAMMA . 0.6)
   (BASE-RATE . 0.02)
   (V-HALF . -33.6)
   (TAU-0 . 0.0)
   (QTEN . 1))				; not given
 )


;; K,A channel inactivation particle
;;
(particle-type-def
 `(kal-mig
   (class . :hh-ext)
   (VALENCE . -4)
   (GAMMA . 1.0)
   (BASE-RATE . 0.08)
   (V-HALF . -83.0)
   (TAU-0 . 0.0)
   (QTEN . 1))				; not given
 )



;;----------------------------------------------------
;; K,M potassium channel
;; data from migliore et al.
;;
(channel-type-quoted-def
 `(km-mig95
   (gbar-density . ,(* .10		; mS/cm2  - range from 0.02 to 0.1 (burst), 0.1 to 0.2 (non-burst)
		     1.0e-3		; mS -> S
		     1.0e4		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
		     ))
   (e-rev . -91.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (v-particles . ((kmm-mig 1)))))



;; K,M channel activation particle
;; 
(particle-type-def
 `(kmm-mig
   (class . :hh-ext)
   (VALENCE . 10)
   (GAMMA . 0.06)
   (BASE-RATE . 0.006)			; in the paper, the base-rate for beta is 0.06, which is
					; probably a typo
   (V-HALF . -55)
   (TAU-0 . 0.0)
   (QTEN . 1))				; not given
 )

(particle-type-def
 `(kmm-mig-paper
   (class . :hh)
   (alpha . (lambda (voltage)
	      (* 0.006 (exp (/ (* 10 0.06 1.0e-3 (- voltage -55.0) faraday) ; mV -> V
			       (* *temperature* gasconstant))))))
   (beta . (lambda (voltage)
	     (* 0.06 (exp (/ (* 10 0.94 1.0e-3 (- voltage -55.0) faraday) ; mV -> V
			     (* *temperature* gasconstant))))))
			  
   (QTEN . 1))				; not given
 )


;; Just to check the published parameters.
(particle-type-def
 `(kmm-mig-paper
   (class . :hh)
   (alpha . (lambda (voltage)
	      (* 0.006 (exp (/ (* 10 0.06 1.0e-3 (- voltage -55.0) faraday) ; mV -> V
			       (* *temperature* gasconstant))))))
   (beta . (lambda (voltage)
	     (* 0.06 (exp (/ (* -1 10 0.94 1.0e-3 (- voltage -55.0) faraday) ; mV -> V
			     (* *temperature* gasconstant))))))
   ;; not given
   (QTEN . 1)))				


(channel-type-quoted-def
 `(kahp-mig95
   ;; p. 1161
   (gbar-density . ,(* 0.4		; mS/cm2   - same for all cells
		     1.0e-3		; mS -> S
		     1.0e4))		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
   (e-rev . -91.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)
   (conc-particles . ((kahpw-mig 1)))))

(conc-particle-type-def
 `(kahpw-mig
   (class . :nth-order)
   (alpha . 1.3e13)
   (power . 4)
   (beta . 0.005)
   (conc-int-type . ca-in-mig-approx)
   ; (conc-int-type . ca-in-jaffe-94)
   (shell . 1)				; For the CA-IN-JAFFE-94 conc integrator type
					; shell 1 corresponds to the entire cell element volume.
   (QTEN . 1)))



;; kc fast ca activated channel, adapted from Moczydlowski and Latorre 1983, via Hines.
(channel-type-quoted-def
 `(kc-mig95
   ;; p. 1161
   (gbar-density . ,(* 0.8		; mS/cm2 - note that for one of their cells this was
					; adjusted to 0.55 mS/cm2
		     1.0e-3		; mS -> S
		     1.0e4		; S/cm2 -> pS/um2 - 1 pS/um2 = 1.0e-4 S/cm2
		     ))
   (e-rev . -91.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (conc-particles . ((kco-mig 1)))))


;; K,C channel activation particle - see description of kco-ml83 in NEURON-channels.lisp.

;; In SURF-HIPPO, we maintain the conventions and notations of the ML paper - thus alpha is the backward rate constant,
;; and beta is the forward rate constant.
(conc-particle-type-def
 `(kco-mig
   (parent-type . kco-ml83)

   ;; mM - in the Migliore paper they call this k1, after Hines. This is a scaling term for the Ca++ dep component
   ;; of the forward rate constant, beta. In the M-L paper, this value ranges from .18 to .37 mM. Note that Hines
   ;; uses 0.18mM.
   (k1-0 .  0.48e-3)			;mM

   ;; mM - in the Migliore paper they call this k2, after Hines. This is a scaling term for the Ca++ dep component
   ;; of the backward rate constant, alpha. In the M-L paper, this value ranges from .011 to .014 mM! Note that Hines
   ;; uses 0.011mM.
   (k4-0 .  0.13e-6)			;mM

   (conc-int-type . ca-in-mig-approx)	; This is an approximation to the incomplete description in
					; the paper
;   (conc-int-type . ca-in-jaffe-94)
   (shell . 1))				; For the CA-IN-JAFFE-94 conc integrator type
					; shell 1 corresponds to the entire cell element volume.
 )

;; Similar to CA-IN-GEN, but with buffer hitting all shells
(conc-int-type-def
 '(CA-IN-mig-approx
   (class . :MULTI-SHELL)
   (species . CA)
   (intra-p . T)
   (shell-2-p . T)
   (shell-3-p . T)
   (juxtamembrane-shell-thickness . 1.0)
   (alpha-s . 0.2)
   (inner-shell-thickness . 0.0)	; shell 3 is rest of volume
   (interdigitation-coefficient . -1.0)
   (diffusion-coefficient . (((1 2) 6.0e-6)
			      ((1 3) 0.0)
			      ((2 3) 6.0e-6)))
   (transmembrane-concentration . 2.0)
   (core-conc . 5.0e-5)
   (PUMP-TYPE-PARAMS . ((CA-JAFFE-94 2)))
   (k-buffer . 1000.0)
   (shells-w-instantaneous-buffer . (1 2 3))))


(setq *D_CA* 0.6e-5)			; cm2/s - they use 0.6 um2/ms. default value at 25c is 0.79e-5


(conc-int-type-def
 `(ca-in-migliore95
   (class . :multi-shell)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; This is the default

   (pump-type-params . ((ca-jaffe-94 1))) ; Pump type CA-JAFFE-94 is associated with (the single)
					  ; compartment 1.

   (K-buffer . 1.0e3)			; Instantaneous buffer, ratio of bound[X]/[X] in
					; a compartment (dimensionless).
   
   (juxtamembrane-shell-thickness . 0.10) ; check


   (transmembrane-concentration . 2.0)	; The calcium out used in the paper 
   
   (core-conc . 5.0e-5)))

(synapse-type-def
 '(mig95-ampa-abs
   (e-rev . 0)
   (waveform-spec . (alpha-array 3 :step 0.2))
   (waveform-time-interval . 0.2)
   (gbar . 0.001)			;adjusted to total g 5nS
   (control . :event)))



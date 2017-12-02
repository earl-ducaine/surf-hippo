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


;; Parameters for the extended Hodgkin-Huxley models for hippocampal pyramidal cell channels, as
;; described (but possibly modified since) in AI TR 1161. These parameters are now superseded by
;; parameters found in working-hpc.lisp.


(in-package "SURF-HIPPO")

;;;; Na channel notes from TR1161:
;;; Original estimates for the parameters of the three conductances are derived from single Na only spike record
;;; (24 degrees C) and the Na only repetitive records (27 degrees C and 32 degrees C). All Qten's are derived
;;; from a reference of 24 degrees C. Gating particle kinetics have a Qten of 5; conductance Qten's are set to
;;; 1.5.




;;; 12/26/93 re-evaluating TR 1161 parameters
;;; ref Sunday, 12/26/93 02:22:40 pm EST
;;; Simulation 'basic-hippo-296592256'

;; revised 1/2/94
(channel-type-def
 '(na1-TR1161
   (gbar-density . 130.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.5)
   (reference-temp . 24.0)
   (v-particles . ((m1-TR1161 2) (H1-TR1161 1)))))


(particle-type-def
 '(m1-TR1161
   (class . :hh-ext-old)
   (VALENCE . 20.0)
   (GAMMA . 0.5)
   (BASE-RATE . 0.3)
   (V-HALF . -46.0)
   (TAU-0 . 0.5)
					;	(QTEN . 5.0)
   (QTEN . 3.0)
   (reference-temp . 24.0)
   (Fixed-boltzmann-reference-temperature . 24.0)
   ))

(particle-type-def
 '(H1-TR1161
   (class . :hh-ext-old)
   (VALENCE . -15.0)
   (GAMMA . 0.7)
   (BASE-RATE . 0.001)
   (V-HALF . -62.0)
   (TAU-0 . 1.0)
					;	(QTEN . 5.0)
   (QTEN . 3.0)
   (reference-temp . 24.0)
   (Fixed-boltzmann-reference-temperature . 24.0)
   ))

;; revised 12/31/93
(channel-type-def
 '(na2-TR1161
   (gbar-density . 1.0) (e-rev . 50.0) (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.5)
   (reference-temp . 24.0)
   (v-particles . ((m2-TR1161 1)))))

(particle-type-def
 '(m2-TR1161
   (class . :hh-ext-old)
   (VALENCE . 5.0)
   (GAMMA . 0.5)
   (BASE-RATE . 0.005)
   (V-HALF . -44.0)
   (TAU-0 . 5)
   (QTEN . 5.0)
   (reference-temp . 24.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))


;; revised 1/2/94
(channel-type-def
 '(na3-TR1161
   (gbar-density . 100.0) (e-rev . 50.0) (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1.5)
   (reference-temp . 24.0)
   (v-particles . ((m3-TR1161 1)(h3-TR1161 1)))))

;; revised 12/31/93
(particle-type-def
 '(m3-TR1161
   (class . :hh-ext-old)
   (VALENCE . 8)
   (GAMMA . 0.5)
   (BASE-RATE . 1.0)
   (V-HALF . -28.0)
   (TAU-0 . 0.5)
					;	(QTEN . 5.0)
   (QTEN . 3.0)
   (reference-temp . 24.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))

;; revised 12/30/93
;; revised 1/2/94
(particle-type-def
 '(H3-TR1161
   (class . :hh-ext-old)
   (VALENCE . -8.0)
   (GAMMA . 0.7)
   (BASE-RATE . 0.025)
   (V-HALF . -42)
   (TAU-0 . 1.0)
					;	(QTEN . 5.0)
   (QTEN . 3.0)
   (reference-temp . 24.0)
   (Fixed-boltzmann-reference-temperature . 24.0)))

(channel-type-def
 '(ca-TR1161
   (gbar-density . 170.0)
   (e-rev . 110.0)
   (ion-permeabilities . ((cA 1.0)))
   (use-variable-e-rev . t)
   (conc-int-type-params . ((ca-in-TR1161 (1 1))
;			    (ca-in-TR1161 (2 1))
			    ))
   (CONC-INT-TYPE-E-REV-PARAMS . ((ca-in-TR1161 (1 1))))
   (QTEN . 1.5)
   (reference-temp . 32.0)
   (v-particles . ((cas-TR1161 2) (caw-TR1161 4)))))

(particle-type-def
 '(cas-TR1161 
   (class . :hh-ext-old)
   (VALENCE . 4)
   (GAMMA . 0.1)
   (BASE-RATE . 0.2)
   (V-HALF . -24.0)
   (TAU-0 . 0.10)
   (QTEN . 3.0)
   (reference-temp . 32.0)
   (Fixed-boltzmann-reference-temperature . 32.0)))

(particle-type-def
 '(caw-TR1161
   (class . :hh-ext-old)
   (VALENCE . -12.0)
   (GAMMA . 0.8)
   (BASE-RATE . 0.001)
   (V-HALF . -35.0)
   (TAU-0 . 5.0)
   (QTEN . 3.0)
   (reference-temp . 32.0)
   (Fixed-boltzmann-reference-temperature . 32.0)))

(channel-type-def
 '(dr-TR1161
   (gbar-density . 40.0)
   (e-rev . -75.0)			; original was -73
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (v-particles . ((drx-TR1161 12)		; original was 3!
		   (dry-TR1161 1)))))

(particle-type-def
 '(drx-TR1161
   (class . :hh-ext-old)
   (VALENCE . 12)
   (GAMMA . 0.9)			; original was 0.95
   (BASE-RATE . 0.002)			; original was 0.008
   (v-HALF . -28.0)
   (TAU-0 . 0.5)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(particle-type-def
 '(dry-TR1161
   (class . :hh-ext-old)
   (VALENCE . -9)
   (GAMMA . 0.8)
   (BASE-RATE . 4.0e-4)
   (V-HALF . -45.0)
   (TAU-0 . 6.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))


(channel-type-def
 '(m-TR1161
   (gbar-density . 10.0) (e-rev . -85.0) (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (v-particles . ((mx-TR1161 1)))))

(particle-type-def
 '(mx-TR1161
   (class . :hh-ext-old)
   (VALENCE . 5)
   (GAMMA . 0.5)
   (BASE-RATE . 0.0015)
   (V-HALF . -45.0)
   (TAU-0 . 0.10)
   (QTEN . 5)
   (reference-temp . 23.0)
   (Fixed-boltzmann-reference-temperature . 23.0)))


(channel-type-def
 '(a-TR1161
   (gbar-density . 120.0) (e-rev . -70.0) (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (v-particles . ((ax-TR1161 3)(ay-TR1161 1)))))

(particle-type-def
 '(ax-TR1161
   (class . :hh-ext-old)
   (VALENCE . 3.5)
   (GAMMA . 0.8)
   (BASE-RATE . 0.2)
   (V-HALF . -52.0)
   (TAU-0 . 1.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(particle-type-def
 '(ay-TR1161
   (class . :hh-ext-old)
   (VALENCE . -7)
   (GAMMA . 0.6)
   (BASE-RATE . 0.0015)
   (V-HALF . -72.0)
   (TAU-0 . 24.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(channel-type-def
 '(c-TR1161
   (gbar-density . 40.0) (e-rev . -85.0) (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (v-particles . ((cx-TR1161 3)(cy-TR1161 1)))
   (conc-particles . ((cw-TR1161 1)))))

(particle-type-def
 '(cx-TR1161
   (class . :hh-ext-old)
   (VALENCE . 25)
   (GAMMA . 0.2)
   (BASE-RATE . 0.007)
   (V-HALF . -65.0)
   (TAU-0 . 0.25)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(particle-type-def
 '(cy-TR1161
   (class . :hh-ext-old)
   (VALENCE . -20)
   (GAMMA . 0.8)
   (BASE-RATE . 0.01)
   (V-HALF . -60.0)
   (TAU-0 . 15.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(conc-particle-type-def
 '(cw-TR1161
   (class . :nth-order)
   (alpha . 200.0)			; original was 10000.0 (mistake in int-type-TR1161 . ca-in-TR1161)
   (beta . 0.125)			; original, probably wrong
   (conc-int-type . ca-in-TR1161)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)
   (shell . 1)))

(channel-type-def
 '(ahp-TR1161
   (gbar-density . 100.0) (e-rev . -85.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((ahpz-TR1161 1)(ahpy-TR1161 2)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)
   (conc-particles . ((ahpw-TR1161 1)))))

(particle-type-def
 '(ahpz-TR1161
   (class . :hh-ext-old)
   (VALENCE . -12)
   (GAMMA . 1.0)
   (BASE-RATE . 1.9999999e-4)
   (V-HALF . -72.0)
   (TAU-0 . 120.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))

(particle-type-def
 '(ahpy-TR1161
   (class . :hh-ext-old)
   (VALENCE . -15)
   (GAMMA . 0.2)
   (BASE-RATE . 0.015)
   (V-HALF . -50.0)
   (TAU-0 . 1.0)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))



(conc-particle-type-def
 '(ahpw-TR1161
   (class . :nth-order)
   (alpha . 1000.0)			; original was 1.0e5
   (beta . 1.0)				; original was 0.005
   (power . 3)
   (conc-int-type . ca-in-TR1161)
   (QTEN . 3)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)
   (shell . 2)))


(channel-type-def
 '(q-TR1161
   (gbar . 0.002)
   (e-rev . -55.0)			; Gro-Hal-90 -49, Hal-Ada-82 between -53 and -59 
;   (ion-permeabilities . ((K 0.70)(na 0.30))) ; check
   (v-particles . ((qx-TR1161 1)))
   (QTEN . 1.5)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))


(particle-type-def
 '(qx-TR1161
   (class . :hh-ext)
   (VALENCE . -15)
   (GAMMA . 1.0)
   (BASE-RATE . 0.0003)
   (V-HALF . -45.0)
   (TAU-0 . 6.0)
   (QTEN . 5)
   (reference-temp . 30.0)
   (Fixed-boltzmann-reference-temperature . 30.0)))




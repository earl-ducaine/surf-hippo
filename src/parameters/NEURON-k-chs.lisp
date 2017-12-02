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
; K channels adapted from NEURON.
;


(in-package "SURF-HIPPO")


;; kc fast ca activated channel, adapted from Moczydlowski and Latorre 1983, via Hines.
(channel-type-def
 '(kc-ml83
   ;; 	gkbar=.01	(mho/cm2)	: Maximum Permeability
   ;;   cai = 1e-3	(mM)
   (gbar-density . 1.0)	
   (e-rev . -77.50)			; Not clear that this is the default value 
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (conc-particles . ((kco-ml83 1)))
   )
 )


;; K,C channel activation particle
;;
;; for comparison, the parameters in Hines are
;;
;;	d1 = .84
;;	d2 = 1.
;;      k1 = .18	(mM)
;;	k2 = .011	(mM)
;;	bbar = .28	(/ms)
;;	abar = .48	(/ms)
;;
;; followed by this comment from Hines, in the file nmodl/examples/cagk.mod -
;;
;; the preceding two numbers were switched on 8/19/92 in response to a bug
;; report by Bartlett Mel. In the paper the kinetic scheme is
;; C <-> CCa (K1)
;; CCa <-> OCa (beta2,alpha2)
;; OCa <-> OCa2 (K4)
;; In this model abar = beta2 and bbar = alpha2 and K4 comes from d2 and k2
;; I was forcing things into a nomenclature where alpha is the rate from
;; closed to open. Unfortunately I didn't switch the numbers.

;; In SURF-HIPPO, we maintain the conventions and notations of the ML paper - thus alpha is the backward rate constant,
;; and beta is the forward rate constant.
(conc-particle-type-def
 '(kco-ml83
   (class . :ml)
	
   ;; Dimensionless. Taken from Hines, who calls this d1. This is a coefficient for the voltage in the exponential
   ;; coefficient for the Ca++ dep component of the forward rate constant, beta.
   (delta-1 . .84)
	
   ;; Dimensionless. Taken from Hines, who calls this d2. This is a coefficient for the voltage in the exponential
   ;; coefficient for the Ca++ dep component of the backward rate constant, alpha.
   (delta-4 . 1.0)

   ;; Valence of the calcium ion
   (valence . 2) 

   ;; mM - Hines calls this k1. This is a scaling term for the Ca++ dep component
   ;; of the forward rate constant, beta. In the M-L paper, this value ranges from .18 to .37 mM. 
   (k1-0 .  0.18)

   ;; mM - Hines calls this k2. This is a scaling term for the Ca++ dep component
   ;; of the backward rate constant, alpha. In the M-L paper, this value ranges from .011 to .014 mM.
   (k4-0 .  0.011)

   (beta-0 . 0.48)			; 1/ms Taken from Hines ("abar"), who follows the value given in M-L.

   (alpha-0 . 0.28)			; 1/ms Taken from Hines ("bbar"), who follows the value given in M-L.
	
   (power . 1)				; [Ca++] power for the rate constants
   (QTEN . 1)				; not given
   (conc-int-type . ca-in-gen)
   (shell . -1))			; Follow the concentration averaged over the entire cell
					; element volume.
 )


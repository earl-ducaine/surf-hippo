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
This file includes mechanisms from the Working model, as described in

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 13, Cortical
Models", edited by P.S. Ulinski, E.G. Jones and A. Peters, Plenum Press, 1998.
|#

(pump-type-def
 `(CA-HPC-MM
   (class . :MM)
   (v-max . 6e-11)
   (kd . 0.01)
   (species . CA)
   (qten . 1)
   (reference-temp . 27)))

(channel-type-def
 '(KM-HPC
   (gbar . 0.4)
   (e-rev . -80)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.9) (NA 0.1)))
   (q10 . 1)
   (reference-temp . 27)
   (v-particles . ((KMU-HPC 2)))))

(particle-type-def
 `(KMU-HPC
   (class . :HH-EXT)
   (valence . 6)
   (gamma . 0.6)
   (base-rate . 0.003)
   (v-half . -45)
   (tau-0 . 8)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 35)
   (qten . 1)))

(channel-type-def
 '(KA-HPC
   (gbar . 2.3)
   (e-rev . -70)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.85) (NA 0.15)))
   (q10 . 1)
   (reference-temp . 35)
   (v-particles . ((KAX-HPC 4)(KAY-HPC 3)))))

(particle-type-def
 `(KAX-HPC
   (class . :HH-EXT)
   (valence . 2.8)
   (gamma . 0.85)
   (base-rate . 0.08)
   (v-half . -41)
   (tau-0 . 1)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 1)))

(particle-type-def
 `(KAY-HPC
   (class . :HH-EXT)
   (valence . -3)
   (gamma . 1)
   (base-rate . 0.04)
   (v-half . -49)
   (tau-0 . 2)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 1)))

(channel-type-def
 '(KAHP-HPC
   (gbar . 0.02)
   (e-rev . -85)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1)
   (reference-temp . 27)
   (conc-particles . ((KAHPO-HPC 2)))))

(conc-particle-type-def
 '(KAHPO-HPC
   (class . :NTH-ORDER)
   (alpha . 2e14)
   (beta . 0.01)
   (tau-0 . 100)
   (power . 4)
   (qten . 1)
   (reference-temp . 30)
   (shell . 2)
   (conc-int-type . CA-IN-HPC)))
                   
(channel-type-def
 '(CA-L-HPC
   (permeability . 3e-9)
   (e-rev . 0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1)
   (reference-temp . 22)
   (conc-int-type-params . ((CA-IN-HPC (1 1))))
   (v-particles . ((CA-LM-HPC 2)))))

(particle-type-def
 `(CA-LM-HPC
   (class . :HH-EXT)
   (valence . 4.6)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -1.2)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 1)))

(channel-type-def
 '(CA-T-HPC
   (permeability . 3e-9)
   (e-rev . 0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1)
   (reference-temp . 22)
   (conc-int-type-params . ((CA-IN-HPC (1 1))))
   (v-particles . ((CA-TM-HPC 2)(CA-TH-HPC 1)))))

(particle-type-def
 `(CA-TM-HPC
   (class . :HH-EXT)
   (valence . 3)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -36)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (qten . 1)))

(particle-type-def
 `(CA-TH-HPC
   (class . :HH-EXT)
   (valence . -5.2)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -68)
   (tau-0 . 10)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (qten . 1)))

(channel-type-def
 '(KDR-HPC
   (gbar . 0.4)
   (e-rev . -70)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1)
   (reference-temp . 24)
   (v-particles . ((KDRX-HPC 1)(KDRY-HPC 1)))))

(particle-type-def
 `(KDRX-HPC
   (class . :HH-EXT)
   (valence . 3)
   (gamma . 0.8)
   (base-rate . 0.17)
   (v-half . -5)
   (tau-0 . 0.8)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 27)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 3)))

(particle-type-def
 `(KDRY-HPC
   (class . :HH-EXT)
   (valence . -1)
   (gamma . 0)
   (base-rate . 0)
   (v-half . -68)
   (tau-0 . 300)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 3)))

(channel-type-def
 '(NA-HPC
   (gbar . 1.2)
   (e-rev . 65)
   (use-defined-rev . T)
   (ion-permeabilities . ((NA 1.0)))
   (q10 . 1)
   (reference-temp . 27)
   (v-particles . ((NA-X-HPC 1)))))

(particle-type-def
 `(NA-X-HPC
   (class . :MARKOV)
   (STATES . (C1 C2 O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C2 O (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -51 :K 1 :TAU-MIN 1/3))
     (O C2 (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -57 :K -2 :TAU-MIN 1/3))
     (O I 3)
     (O C1 (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -51 :K -2 :TAU-MIN 1/3))
     (C1 O (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -42 :K 1 :TAU-MIN 1/3))
     (I C1 (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -53 :K -1 :TAU-MAX 100 :TAU-MIN 1))
     (C1 C2 (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -60 :K -1 :TAU-MAX 100 :TAU-MIN 1))))
   (reference-temp . 27)
   (qten . 1)))
   
(channel-type-def
 '(KD-HPC
   (gbar . 0.05)
   (e-rev . -95)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1)
   (reference-temp . 35)
   (v-particles . ((KDX-HPC 4)(KDY-HPC 4)))))

(particle-type-def
 `(KDX-HPC
   (class . :HH-EXT)
   (valence . 3)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -63)
   (tau-0 . 1)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 35)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 1)))

(particle-type-def
 `(KDY-HPC
   (class . :HH-EXT)
   (valence . -2.5)
   (gamma . 0)
   (base-rate . 2e-4)
   (v-half . -73)
   (tau-0 . 0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27)
   (qten . 1)))

(channel-type-def
 '(CA-N-HPC
   (permeability . 10e-9)
   (e-rev . 0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1)
   (reference-temp . 22)
   (conc-int-type-params . ((CA-IN-HPC (1 1))))
   (v-particles . ((CA-NM-HPC 2)(CA-NH-HPC 1)))))

(particle-type-def
 `(CA-NM-HPC
   (class . :HH-EXT)
   (valence . 3.4)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -21)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (qten . 1)))

(particle-type-def
 `(CA-NH-HPC
   (class . :HH-EXT)
   (valence . -2)
   (gamma . 0)
   (base-rate . 1)
   (v-half . -40)
   (tau-0 . 75)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27)
   (qten . 1)))

(channel-type-def
 '(KCT-HPC
   (gbar . 0.4)
   (e-rev . -80)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.9) (NA 0.1)))
   (q10 . 1)
   (reference-temp . 27)
   (v-particles . ((KCTX-HPC 1)))))

(defvar *enable-KCTX-HPC-ca-forward-v-term* t)
(defvar *KCTX-HPC-ca-activation-forward-exponential-term*)
(setq *KCTX-HPC-ca-activation-forward-exponential-term*
      (v-function-array '(squeezed-exponential-rate
			  voltage
			  :tau-min 0.001
			  :k 7
			  :v-half -20
			  :tau-max 1)))

(defun KCTX-HPC-ca-activation-forward (prt)
  (return-markov-rate
   (* (if *ENABLE-KCTX-HPC-CA-FORWARD-V-TERM*
	  (aref (the vec-df *KCTX-HPC-ca-activation-forward-exponential-term*) (particle-v-index prt))
	  1.0d0)
      (nthorder-conc-particle-forward-rate (particle-conc-particle prt)))))

(defun KCTX-HPC-ca-activation-backward (prt)
  (return-markov-rate (nthorder-conc-particle-backward-rate (particle-conc-particle prt))))

(particle-type-def
 `(KCTX-HPC
   (class . :MARKOV)
   (STATES . (C O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C O KCTX-HPC-CA-ACTIVATION-FORWARD T)
     (O C KCTX-HPC-CA-ACTIVATION-BACKWARD T)
     (I C (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -120 :K -10 :TAU-MIN 10))
     (O I (SQUEEZED-EXPONENTIAL-RATE VOLTAGE :V-HALF -64 :K -3.5 :TAU-MIN 0.1))))
   (reference-temp . 27)
   (qten . 1)
   (concentration-particle-type . KCTX-HPC-CA)))
   
;;; This concentration particle sets the C<->O transitions via KCTX-HPC-CA-ACTIVATION-FORWARD and KCTX-HPC-CA-ACTIVATION-BACKWARD.
(conc-particle-type-def
 '(KCTX-HPC-CA
        (class . :NTH-ORDER)
        (alpha . 1e6)		; Note that this is in units of mM(-n) since there is a leading voltage term.
        (beta . 0.05)
        (tau-0 . 0)
        (power . 3)
        (qten . 1)
        (reference-temp . 27)
        (shell . 1)
        (conc-int-type . CA-IN-HPC)))

(channel-type-def
 '(H-HPC
   (gbar . 0.003)
   (e-rev . -17)
   (use-defined-rev . T)
   (q10 . 1)
   (reference-temp . 32)
   (v-particles . ((HY-HPC 1)))))

(particle-type-def
 `(HY-HPC
   (class . :HH-EXT)
   (valence . -2)
   (gamma . 1)
   (base-rate . 1)
   (v-half . -98)
   (tau-0 . 180)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 32)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 32)
   (qten . 1)))

(conc-int-type-def
 '(CA-IN-HPC
   (class . :MULTI-SHELL)
   (species . CA)
   (intra-p . T)
   (shell-2-p . T)
   (shell-3-p . T)
   (juxtamembrane-shell-thickness . 1)
   (inner-shell-thickness . 0)
   (alpha-s . 10e-5)
   (interdigitation-coefficient . 1)
   (diffusion-coefficient . (((1 2) 8e-6)
			     ((1 3) 0)
			     ((2 3) 8e-6)))
   (transmembrane-concentration . 2)
   (PUMP-TYPE-PARAMS . ((CA-HPC-MM 2)))
   (resting-free-conc . 5e-5)
   (instantaneous-buffer-enabled . T)
   (instantaneous-buffer-ratio . 20)))


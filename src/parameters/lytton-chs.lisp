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

;; conductances of W. W. LYTTON and T. J. SEJNOWSKI
;;
;; na-lyt      wilson and Bower 1990
;; dr-lyt      
;; m-lyt
;; a-lyt      Segal and Barker 1984 hippocampus but current simular in the neocortex
;; caT-lyt   Coulter et al 1989 
;; caN-lyt
;; ahp-lyt
#|
Check this -
@ARTICLE{Lyt-Sej-91,
	AUTHOR = {Lytton, W. W. and Sejnowski, T. J.},
	TITLE = {Simulations of cortical pyramidal neurons synchronized by inhibitory interneurons},
	JOURNAL = {Journal of Neuroscience},
	YEAR = {1991},
	VOLUME = {66},
	NUMBER = {3},
	MONTH = {September}
}
|#
;; Note that the BG formalism referred to by this paper has :TAU-0 as a minimum time constant,
;; corresponding to the :HH-EXT-OLD particle type class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NA-LYT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(channel-type-def
 '(na-lyt
   (gbar-density . 20000.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 3)
   (reference-temp . 37.0)
   (v-particles . ((nax-lyt 3) (nay-lyt 1)))))
 
(particle-type-def
 '(nax-lyt
   (class . :hh-ext-old)
   (VALENCE . 3.30)
   (GAMMA . 0.7)
   (BASE-RATE . 4.2)
   (V-HALF . -34.5)
   (TAU-0 . 0.05)
   (QTEN . 3.0)
   (reference-temp . 37.0)))

(particle-type-def
 '(nay-lyt
   (class . :hh-ext-old)
   (VALENCE . -3.0)
   (GAMMA . 0.27)
   (BASE-RATE . 0.09)
   (V-HALF . -45.0)
   (TAU-0 . 0.250)
   (QTEN . 3.0)
   (reference-temp . 37.0)))


;;;;;;;;;;;;;;;;;;;;;;;;; POTASSIUM CHANNEL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; IKd-lyt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(channel-type-def
 '(Kd-lyt
   (gbar-density . 5000.0)
   (e-rev . -100.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 3)
   (reference-temp . 24.0)
   (v-particles . ((drx-lyt 4)))))

(particle-type-def
 '(drx-lyt
   (class . :hh-ext-old)
   (VALENCE . 3)
   (GAMMA . 0.8)
   (BASE-RATE . 0.3)
   (V-HALF . -35.0)
   (TAU-0 . 1)
   (QTEN . 3)
   (reference-temp . 24.0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; m-lyt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(channel-type-def
 '(m-lyt
   (gbar-density . 4)
   (e-rev . -100.0) 
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 3)
   (reference-temp . 25.0)
   (v-particles . ((mx-lyt 1)))))

(particle-type-def
 '(mx-lyt
   (class . :hh-ext-old)
   (VALENCE . 5)
   (GAMMA . 0.5)
   (BASE-RATE . 0.0008)
   (V-HALF . -44.0)
   (TAU-0 . 10)
   (QTEN . 3)
   (reference-temp . 25.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Ia-lyt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(channel-type-def
 '(a-lyt
   (gbar-density . 1000.0)
   (e-rev . -100.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 3)
   (reference-temp . 24.0)
   (v-particles . ((ax-lyt 3)(ay-lyt 1)))))

(particle-type-def
 '(ax-lyt
   (class . :hh-ext-old)
   (VALENCE . 4.5)
   (GAMMA . 0.8)
   (BASE-RATE . 0.2)
   (V-HALF . -35.0)
   (TAU-0 . 1)
   (QTEN . 3)
   (reference-temp . 24.0)))

(particle-type-def
 '(ay-lyt
   (class . :hh-ext-old)
   (VALENCE . -7)
   (GAMMA . 0.4)
   (BASE-RATE . 0.01)
   (V-HALF . -68.0)
   (TAU-0 . 24.0)
   (QTEN . 3)
   (reference-temp . 24.0)))


;;;;;;;;;;;;;;;;;;;;;;;;;; ahp-lyt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iahp

;; Needs to be completed vis-a-vis a concentration integrator type for the AHPW-LYT conc particle type.
#|
(channel-type-def
 '(ahp-lyt
   (gbar-density . 2)
   (e-rev . -100.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 3.0)
   (reference-temp . 30.0)
   (conc-particles . ((ahpw-lyt 1)))))

(conc-particle-type-def
 '(ahpw-lyt
   (class . :hh-ext)
   (alpha . 200.0)
   (beta . 0.014)
   (power . 1)
   (intracellular-p . t)
   (ion . ca)
   (QTEN . 3)
   (reference-temp . 30.0)
   (shell . 2)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;; CALCIUM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in the article the density is give with permeability.
;; p (cm/s)

;;;;;;;;;;;;;;;;;;;;;;;;;  caT-lyt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(channel-type-def
 '(caT-lyt
   (gbar-density . 700.0)
   (e-rev . 110.0)
   (ion-permeabilities . ((cA 1.0)))
   (QTEN . 1.5)
   (reference-temp . 23.0)
   (v-particles . ((casT-lyt 3) (cawT-lyt 1)))))

(particle-type-def
 '(casT-lyt 
   (class . :hh-ext-old)
   (VALENCE . 3.43)
   (GAMMA . 0.5)
   (BASE-RATE . 0.06)
   (V-HALF . -63.0)
   (TAU-0 . 2.5)
   (QTEN . 3.0)
   (reference-temp . 23.0)))

(particle-type-def
 '(cawT-lyt
   (class . :hh-ext-old)
   (VALENCE . -4.24)
   (GAMMA . 0.75)
   (BASE-RATE . 0.008)
   (V-HALF . -83.5)
   (TAU-0 . 18.0)
   (QTEN . 3.0)
   (reference-temp . 23.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; caN-lyt  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(channel-type-def
 '(caN-lyt
   (gbar-density . 6000.0)
   (e-rev . 110.0)
   (ion-permeabilities . ((cA 1.0)))
   (QTEN . 3.0)
   (reference-temp . 37.0)
   (v-particles . ((casN-lyt 3) (cawN-lyt 1)))))

(particle-type-def
 '(casN-lyt 
   (class . :hh-ext-old)
   (VALENCE . 3.0)
   (GAMMA . 0.5)
   (BASE-RATE . 0.1)
   (V-HALF . -30.0)
   (TAU-0 . 3)
   (QTEN . 3.0)
   (reference-temp . 37.0)))

(particle-type-def
 '(cawN-lyt
   (class . :hh-ext-old)
   (VALENCE . -4.0)
   (GAMMA . 0.7)
   (BASE-RATE . 0.025)
   (V-HALF . -50.0)
   (TAU-0 . 1.0)
   (QTEN . 3.0)
   (reference-temp . 37.0)))






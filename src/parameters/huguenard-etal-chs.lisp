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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NA-HUG  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
reproduced from
@ARTICLE{Hug-Ham-Pri-88,
	AUTHOR = {Huguenard, J. R. and Hamill, O. P. and Prince, D. A.},
	TITLE = {Developmental changes in $Na^+$ conductances in rat neocortical neurons: appearance of a slowly inactivating component},
	JOURNAL = {Journal of neurophysiology},
	YEAR = {1988},
	VOLUME = {59},
	number = {3},
	MONTH = {March}
}

Table 1.
                                                                                     tau Recovery from Inactivation(ms)
                                    Peak Activation       Steady-state Inactivation          Recovery Potential
cell             gbar(pS/um2)     k (mV/e-fold)  V1/2         k (mV/e-fold)  V1/2        -120      -100      -80
--------------------------------------------------------------------------------------------------------------------------
Immature (E16-P5)

  pyramidal         10.2, 2.1      5.6, 0.2   -30.4, 2.6        5.7, 0.2   -68.4, 3.7  2.3, 0.1   4.4, 0.3   12.0, 1.5


  non-pyramidal     12.7, 3.4      5.5, 0.2   -26.0, 1.5        5.5, 0.2   -64.4, 2.3  2.4, 0.3   4.4, 0.6   14.4, 1.6


Young (P6-P19)

  pyramidal         63.0, 4.4      5.4, 0.2   -39.4, 4.5        5.9, 0.4   -62.6, 1.8  2.2, 0.4   3.7, 0.7   14.1, 1.6


  non-pyramidal     59.6, 4.6      5.4, 0.2   -35.2, 1.1        5.6, 0.2   -66.4, 2.7  2.3, 0.3   4.8, 0.8   14.2, 3.4


Mature (P20-P50)

  pyramidal         109, 39        5.1, 0.2   -30.6, 2.8        6.2, 0.2   -65.0, 5.5  1.6        4.0        8.0


  non-pyramidal     58.7, 12       5.4, 0.2   -34.1, 1.7        5.9, 0.1   -66.2, 3.3



fitting m3h to young pyramidal cell data -

(ss-fit (boltzman-characteristic 5.4 -39.4)
	:volts (loop for voltage from -100.0 to 50 collect voltage)
	:exponent 3)
->
 (class . :hh-ext)
 (v-half . -49.051605)
 (valence . 3.7923443)

(ss-fit (boltzman-characteristic -5.9 -62.6)
	:volts (loop for voltage from -100.0 to 50 collect voltage)
	:exponent 1)
	(class . :hh-ext)

->

 (v-half . -62.6)
 (valence . -4.381693)
NIL
*

***************************************************************

fitting m3h to mature pyramidal cell data -
* (ss-fit (boltzman-characteristic 5.1 -30.6)
	:volts (loop for voltage from -100.0 to 50 collect voltage)
	:exponent 3)

 Converged but LAMBDA (FLA) large
	(class . :hh-ext)
 (v-half . -39.717506)
 (valence . 4.0144544)
NIL
(ss-fit (boltzman-characteristic -6.2 -65)
	:volts (loop for voltage from -100.0 to 50 collect voltage))
	(class . :hh-ext)
 (v-half . -65.0)
 (valence . -4.1696744)

|#

;; young pyramidal
(channel-type-def
 '(na-y-hug
   (gbar-density . 63.0)
   (e-rev . 55.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1)
   (reference-temp . 23.0)
   (v-particles . ((na-y-hug-m 3) (na-y-hug-h 1)))))
 
(particle-type-def
 '(na-y-hug-m
   (class . :hh-ext)
   (v-half . -49.051605)
   (valence . 3.8)
   (GAMMA . 0.5)
   (BASE-RATE . 4.0)
   (TAU-0 . 0.05)
   (QTEN . 1.0)
   (reference-temp . 23.0)))

(particle-type-def
 '(na-y-hug-h 
   (class . :hh-ext)
   (v-half . -62.6)
   (valence . -4.4)
   (GAMMA . 0.5)
   (BASE-RATE . 0.1)
   (TAU-0 . 0.0)
   (QTEN . 1.0)
   (reference-temp . 23.0)))

;; mature pyramidal
(channel-type-def
 '(na-m-hug
   (gbar-density . 109.0)
   (e-rev . 55.0)
   (ion-permeabilities . ((NA 1.0)))
   (QTEN . 1)
   (reference-temp . 23.0)
   (v-particles . ((na-m-hug-m 3) (na-m-hug-h 1)))))
 
(particle-type-def
 '(na-m-hug-m
   (class . :hh-ext)
   (v-half . -39.7)
   (valence . 4.0)
   (GAMMA . 0.5)
   (BASE-RATE . 4.0)
   (TAU-0 . 0.05)
   (QTEN . 1.0)
   (reference-temp . 23.0)))

(particle-type-def
 '(na-m-hug-h 
   (class . :hh-ext)
   (v-half . -65.0)
   (valence . -4.2)
   (GAMMA . 0.5)
   (BASE-RATE . 0.1)
   (TAU-0 . 0.0)
   (QTEN . 1.0)
   (reference-temp . 23.0)))




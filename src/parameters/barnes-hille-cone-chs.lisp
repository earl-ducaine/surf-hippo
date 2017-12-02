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

;; Parameters for
#|
@ARTICLE{Bar-Hil-89,
	AUTHOR = {Barnes, S. and Hille, B.},
	TITLE = {Ionic channels of the inner segment of tiger salamander cone photoreceptors},
	JOURNAL = {Journal of General Physiology},
	YEAR = {1989},
	VOLUME = {94},
	PAGES = {719-743},
	MONTH = {October}
}
|#


(in-package "SURF-HIPPO")

(channel-type-def
 '(H-BARNES-HILLE-89
   (gbar . 0.003)
   (e-rev . -17.0)
   (use-defined-rev . T)
   (q10 . 1.0)
   (reference-temp . 32.0)
   (v-particles . ((H-X-BARNES-HILLE-89 1)))))


(particle-type-def
 `(H-X-BARNES-HILLE-89
   (class . :HH-EXT)
   (linear-markov . (4 2))		; N = 4, M = 2
   (valence . -2.0)
   (gamma . 1.0)
   (base-rate . 1.0)
   (v-half . -98.0)
   (tau-0 . 180.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 32.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 32.0)
   (qten . 1.0)))


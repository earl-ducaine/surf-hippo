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
Taken from

@InCollection{Yam-Koc-Ada-89,
  Author    = {Yamada, W. M. and Koch, C. and Adams, P. R.},
  Title     = {Multiple Channels and Calcium Dynamics},
  BookTitle = {Methods in Neuronal  Modeling},
  Publisher = {Mit Press/Bradford Books},
  Year      = {1989},
  Editor    = {Koch, C. and Segev, I.},
  Chapter   = {4}
  }

|#


;; *****************************
;; Potassium C current
;; *****************************

(channel-type-def
 '(kc-yka89
   (gbar . 1.20)			; uS
   (e-rev . -105.0)			; yka use a pottasium integrator to compute Ek, this value
					; from Mcc-Hug-92.
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (conc-particles . ((kcm-yka89 1)))))


(conc-particle-type-def
 `(kcm-yka89
   (class . :conc-volt)
   (QTEN . 1)          	 
   (power . 1)
   (alpha .  (lambda (conc voltage)
	       (* 250 conc (exp (/ voltage 24)))))
   (beta . (lambda (conc voltage)
	     (* 0.1 (exp (/ voltage -24)))))
   (conc-int-type . ca-in-gen)
   (shell . 1)))




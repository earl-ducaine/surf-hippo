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

;; NOT TESTED

;; Yamada, W. M. and Koch, C. and Adams, P. R. 1989
;; Multiple Channels and Calcium Dynamics
;; in Methods in Neuronal Modeling

;; For juxta-membrane compartment 
(buffer-type-def
 `(ca-yamada-89-membrane
   (species . ca)
   (total-conc . 30.0e-3)		; mM
   (k-forward . 1.0e2)			; mM^-1 ms^-1
   (k-backward . 0.1)))

;; For inner shell compartments
(buffer-type-def
 `(ca-yamada-89-inner
   (species . ca)
   (total-conc . 3.0e-3)		; mM
   (k-forward . 1.0e2)			; mM^-1 ms^-1
   (k-backward . 0.1)))


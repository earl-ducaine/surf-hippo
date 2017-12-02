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


;; Parameters for the concentration integrators.


(in-package "SURF-HIPPO")


;; some preliminary parameters 9/15/95 lbg
(conc-int-type-quoted-def
 `(ca-in-TR1161
   (class . :multi-shell)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; This is the default
   (shell-2-p . t)			
   (core-p . t)
   (diffusion-coefficient . ,*D_CA*)			; Units for diffusion coefficients are cm^2 sec^-1
   (juxtamembrane-shell-thickness . 1.0) ; microns
   (interdigitation-coefficient . 1.0e-5) ; 1/microns
   (inner-shell-thickness . 2.0)	; microns
   (alpha-s . 0.01)
   (core-conc . 5.0e-5)))

;; taken from CA-IN-HPC (used in the WORKING-HPC model)
(conc-int-type-def
 '(CA-IN-GEN
   (class . :MULTI-SHELL)
   (species . CA)
   (intra-p . T)
   (shell-2-p . T)
   (shell-3-p . T)
   (juxtamembrane-shell-thickness . 1.0)
   (inner-shell-thickness . 0.0)
   (alpha-s . 10.0e-5)
   (interdigitation-coefficient . 1.0)
   (diffusion-coefficient . (((1 2) 8.0e-6)
			      ((1 3) 0.0)
			      ((2 3) 8.0e-6)))
   (transmembrane-concentration . 2.0)
   (core-conc . 0.00105)
   (PUMP-TYPE-PARAMS . ((CA-HPC-MM 2)))
   (resting-free-conc . 5.0e-5)
   (instantaneous-buffer-enabled . T)
   (instantaneous-buffer-ratio . 20.0)))

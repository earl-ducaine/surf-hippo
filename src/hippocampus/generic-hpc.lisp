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
This file includes the Working model, as described in

Borg-Graham, L. {\it Interpretations of Data and Mechanisms for
Hippocampal Pyramidal Cell Models}, Chapter in {\it Cerebral Cortex}
Vol.\ 13 - Cortical Models, ed.\ E.\ Jones, P.\ Ulinski and A.\ Peters, Plenum
Publishing Corporation 1998

|#

(defun working-hpc ()
  (setq *fix-e-k* nil *fix-e-na* nil)
  (create-element (ball-and-sticks :cell-type 'BG-97-HPC)
		  '(NA-HPC
		    KDR-HPC
		    KM-HPC
		    kd-HPC
			  
		    KCT-HPC
		    kahp-HPC
		    KA-HPC

		    h-HPC

		    ca-l-HPC
		    ca-n-HPC
		    ca-t-HPC))
  (loop for ch in (channels) when (element-of-ion-type-p ch 'ca) do (element-parameter ch 'conc-int-delta 0.34)))

(push 'working-hpc *CIRCUIT-CATALOG-FUNCTIONS*)

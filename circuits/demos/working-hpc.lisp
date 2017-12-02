;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Laboratoire Neurophysique et Physiologie du Système Moteur, CNRS
UMR 8119, UFR Biomédicale de l'Université René Descartes, Paris
                                                                                 
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

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 12, Cortical
Models", edited by E.G. Jones and P.S. Ulinski, Plenum Press, 1997.
|#

(cell-type-def
 '(BG-97-HPC				; This definition considers the effect of a somatic shunt by a microelectrode, with a reversal potential equivalent
					; to the leak battery.
   (rm . 40000)				; ohms-cm2
   (rm-soma . 2500)			; ohms-cm2
   (ri  . 200)				; ohm-cm
   (cm . 0.7)				; uF/cm2
   (v-leak . -65))			; mV
 )

(cell-type-def				; This definition ignores any somatic shunt.
 '(WORKING-HPC
   (rm . 40000)				; ohms-cm2
   (ri  . 200)				; ohm-cm
   (cm . 0.7)				; uF/cm2
   (v-leak . -65))			; mV
 )

(defun add-working-hpc-channels (&optional (element *soma*) absolute-model)
  (let ((chs (create-element (typecase (element-cell-element element)
			       (segment element)
			       (soma (or ; (virtual-soma-segment-attached-to-soma element)
				      (proximal-soma-segment element)
				      element)))
			     `(NA-HPC
			       KDR-HPC
			       KM-HPC
			       kd-HPC
			       ,(if absolute-model `KCT-HPC-ABSOLUTE `KCT-HPC)
			       ,(if absolute-model `KAHP-HPC-ABSOLUTE `KAHP-HPC)
			       KA-HPC
			       h-HPC
			       ,(if absolute-model `ca-l-HPC-absolute `ca-l-HPC)
			       ,(if absolute-model `ca-n-HPC-absolute `ca-n-HPC)
			       ,(if absolute-model `ca-t-HPC-absolute `ca-t-HPC)))))
    (loop for ch in chs when (element-of-ion-type-p ch 'ca) do (element-parameter ch 'conc-int-delta 0.34))))

(defun working-hpc (&key (name "Working-HPC") (origin '(0 0 0)) (active t))
  (ball-and-sticks :name name :cell-type 'BG-97-HPC :origin origin)
  (when active (add-working-hpc-channels))
  *cell*)

(defun working-hpc-test ()
  (working-hpc)
  (std-setup)
  (pulse-list *isource* '(10 500 0.7))
  (setq *user-stop-time* 1000))
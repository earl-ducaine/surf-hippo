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

;; Hippocampal pyramidal cartoon cell geometry.
(defun hippo (&key (name "Hippo")
		   (cell-origin '(0 0 0))
		   (cell-type 'CA1)
		   (apical-dendrite-diameter 12) (apical-dendrite-length 1200) (soma-diameter 35) ; microns
		   include-basal (include-apical t)
		   (basal-dendrite-diameter 12) (basal-dendrite-length 200) ; microns
		   ri			; ohms-cm
		   rm rm-soma		; ohms-cm2
		   cm cm-soma		; uF/cm2
		   (apical-total-segs 5) (basal-total-segs 5))
  (let ((soma (create-soma :diameter soma-diameter :cell (create-cell name :cell-origin cell-origin :cell-type cell-type))))
    (cond-every
     (rm (cell-type-parameter cell-type :rm rm))
     (ri (cell-type-parameter cell-type :ri ri))
     (rm-soma (cell-type-parameter cell-type :rm-soma rm-soma))
     (cm (cell-type-parameter cell-type :cm cm))
     (cm-soma (cell-type-parameter cell-type :cm-soma cm-soma)))
    (when include-apical
      (segment-chain soma
		     (when include-basal "a")
		     apical-total-segs (/ apical-dendrite-length apical-total-segs) apical-dendrite-diameter :proximal-theta (* 0.5 pi)))
    (when include-basal
      (segment-chain soma (when include-apical "b")
		     basal-total-segs (/ basal-dendrite-length basal-total-segs)  basal-dendrite-diameter :proximal-theta (* -0.5 pi)))
    (soma-cell soma)))


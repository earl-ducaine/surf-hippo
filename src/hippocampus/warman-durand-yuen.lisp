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



;; From Warman, E. N.; Durand, D. M.; and Yuen, G. L. F., Reconstruction of Hippocampal CA1
;; Pyramidal Cell Electrophysiology by Computer Simulation, J. Neurophy. v71, no 6, June 1994

;; see also parameters/warman94-chs.lisp

;; The top level cell function is WDY.

(cell-type-def
 '(WDY-94-HPC
   (rm . 16000)
   (rm-soma . 850)
   (ri . 100)
   (cm-soma . 1)
   (cm-dendrite . 1.85)
   (v-leak . -65)))

(defun wdy (&optional use-fitted-channels)
  (let* ((seg-diameter 5.2)		; microns
	 (segment-L 0.1)
	 (soma-area 942.5)		; um2
	 (cell (create-cell "wdy" :cell-type 'WDY-94-HPC))
	 (seg-length
	  ;; (LENGTH-FROM-LAMBDA 100.0 16000.0 2.6 0.1) => 144.22203 microns
	  (length-from-lambda (element-type-parameter (cell-type cell) :ri)
			      (element-type-parameter (cell-type cell) :rm)
			      (* 0.5 seg-diameter)
			      segment-L))
	 ;; (SPHERE-DIAMETER-FROM-AREA 942.5) => 17.320711 microns
	 (soma (create-soma :cell cell :diameter (sphere-diameter-from-area soma-area))))
    ;; The distal apical section.
    (segment-chain
     ;; The short central apical segment.
     (segment-chain
      ;; The proximal apical section.
      (segment-chain soma "apical-root" 4 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta (/ pi 2.0))
      "apical-center" 1 (* 0.5 seg-length) seg-diameter :proximal-phi 0.0 :proximal-theta 0.0)
     "apical-distal" 4 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta  0.0)
    ;; The basal section.
    (segment-chain soma "basal" 6 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta  (/ pi -2.0))
    
    (create-element soma
		    (if use-fitted-channels
			'(na-wdy-ext ca-wdy-ext kct-wdy-ext kahp-wdy km-wdy-ext ka-wdy-ext kdr-wdy-ext)
			'(na-wdy ca-wdy kct-wdy kahp-wdy km-wdy ka-wdy kdr-wdy)))
    cell))

(defun wdy-fitted ()
  (wdy t))

(defun wdy-test ()
  (wdy)
  (std-setup)
  (enable-element-plot (channels))
  nil)

(push 'wdy *CIRCUIT-CATALOG-FUNCTIONS*)
(push 'wdy-fitted *CIRCUIT-CATALOG-FUNCTIONS*)

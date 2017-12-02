;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;;                   The Surf-Hippo Neuron Simulator                                ;;; 
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;; This code was written as part of the Surf-Hippo Project at Center for Biological ;;; 
;;; Information Processing, Department of Brain and Cognitive Sciences,              ;;; 
;;; Massachusetts Institute of Technology, and currently at the Unite de             ;;; 
;;; Neurosciences Integratives et Computationnelles, Institut Federatif de           ;;; 
;;; Neurobiologie Alfred Fessard, CNRS.                                              ;;; 
;;;                                                                                  ;;; 
;;; Permission to use, copy, modify, and distribute this software and its            ;;; 
;;; documentation for any purpose and without fee is hereby granted, provided that   ;;; 
;;; this software is cited in derived published work, and the copyright notice       ;;; 
;;; appears in all copies and in supporting documentation. The Surf-Hippo Project    ;;; 
;;; makes no representations about the suitability of this software for any          ;;; 
;;; purpose. It is provided "as is" without express or implied warranty.             ;;; 
;;;                                                                                  ;;; 
;;; If you are using this code or any part of Surf-Hippo, please contact             ;;; 
;;; surf-hippo@ai.mit.edu to be put on the mailing list.                             ;;; 
;;;                                                                                  ;;; 
;;; Copyright (c) 1989 - 2002, Lyle J. Graham                                        ;;;                                                       
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L. Graham 01.01.2000

;; A sample passive neuron file, with a moving bar light input. This file should be CIRCUIT-LOADed to setup the circuit.

(setq *enable-light* t			; Let there be light
      *light-stimulus* :moving-bar
      *light-speed* 4			; microns/millisecond
      *bar-length* 500 *bar-width* 50	; microns
      *light-theta* (/ pi 2)
      *light-direction* nil		; T (nil) => movement is 90 degrees ahead (behind) of *light-theta
      *light-stimulus-start-time* 0 *light-stimulus-stop-time* 150 ; Time to start/stop bar moving, milliseconds
      *light-start-position-x* -300 *light-start-position-y* 400 ; Point of center of stimulus at *motion-start-time in microns
      *user-stop-time* 300)		; milliseconds

;; See definition of the HIPPO function in surf-hippo/src/hippocampus/hippo.lisp
(let (*use-simple-names*)
  (ball-and-sticks :name "hippo" :apical-total-segs 50 :apical-dendrite-diameter 2 :apical-dendrite-length 1000
		   :include-basal t :basal-total-segs 50 :basal-dendrite-diameter 2 :basal-dendrite-length 1000)
  (create-element "hippo-a-20" 'l-ex-fac))

(enable-element-plot '("hippo-a-20" "hippo-a-50" "hippo-b-20" "hippo-b-50" "hippo-soma" synapse))

(element-parameter 'l-ex-fac 'color 'green)
(just-draw :scale 5 :mark-elements 'synapse :draw-light-stimulus t :motion-snapshots 3 :mark-plotted-nodes t :label-plotted-nodes t)



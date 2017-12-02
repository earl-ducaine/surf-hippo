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

#|

Example code for running a Rallpack 3 defined axon voltage clamp simulation, as
fully described in:

	AUTHOR = {Borg-Graham, L.},
	TITLE = {Additional Efficient Computation of Branched Nerve Equations: Adaptive Time Step and Ideal Voltage Clamp},
	NOTE = {Submitted to Journal of Computational Neuroscience},
	YEAR = {1999}

Note that the channel and particle types defined below are included in the basic Surf-Hippo image.

This file can be loaded directly into Surf-Hippo for demonstration.

|#

(in-package "SURF-HIPPO")



(defun rallpack-3-11 ()
  (let ((distal-segment
         (segment-chain  ; Create a chain of segments originating from the soma.
                         ; SEGMENT-CHAIN returns the last segment, which we assign to
                         ; a local variable DISTAL-SEGMENT for use below.  

          (create-soma   ; This makes the soma.
           :cell (create-cell 'AXON :cell-type 'HH-AXON)    ; This makes the cell.
           :diameter (sphere-diameter-from-area (* 50 pi)))     ; Diameter => area equals 1/2 segment area. 
          NIL            ; Optional base name for segments - not used here. 
          10             ; Number of segments.
          100 1)))       ; Default length and diameter of each segment in microns.
   
    (element-length distal-segment 50)                ; Shorten the distal segment.
    (element-parameter distal-segment 'RI-COEFFICIENT 2)    ; Double its effective Ri.
   
    ;; Add the Hodgkin-Huxley Na and DR channels to the soma and all the segments, as returned
    ;; from the function CELL-ELEMENTS.
    
    (create-element (cell-elements) 'NA-HH 'DR-HH)))

;; Set up the circuit.
(setq *user-stop-time* 100
      *integration-time-reference* :variable
      *traces-per-plot* nil)

(circuit-load 'RALLPACK-3-11)

(enable-element-plot (add-vsource *soma*))

(enable-element-plot (cell-elements))

(ideal-vsource *vsource*)

(just-draw :mark-plotted-nodes t :label-plotted-nodes t)

;; CLAMP-STEPS runs a series of clamp protocols with a simple pulse holding command. The clamp is voltage or current depending on the SOURCE argument -
;; here it refers to the voltage source created above.
;;
;; (clamp-steps :start-clamp -20 :stop-clamp 90 :step 20 :source *vsource* :clamp-start-time 10 :clamp-stop-time 50))



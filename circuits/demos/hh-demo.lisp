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

;; A sample Surf-Hippo neuron file. This file should be CIRCUIT-LOADed to setup the circuit.
;; Lyle Graham 01/01/00

;; A cell type CORTICAL is defined and added to the cell type library. A cell of this type is then created, consisting of a 35
;; micron diameter soma, with a 1200 micron apical dendrite (12 micron diameter, 5 segments) and a a 500 micron basal dendrite (6
;; micron diameter, 3 segments). Hodgkin-Huxley squid axon Na+ and K+ channels are added to the soma, and there is a current
;; source at the soma, driven by a 0.5 nA pulse from 10 to 20 ms. Simulation time is set to 50 milliseconds. The voltages of the
;; soma and all segments, and the current and conductance of the two channels are plotted.
;;
;; From the Lisp top level, this circuit is loaded by
;;
;;          * (topload "circuits/demos/hh-demo.lisp")
;;
;; assuming that the file is in its original directory in the Surf-Hippo tree.
;;
;; Then, entering (run) will run the simulation.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a cell type definition CORTICAL to the library.
(cell-type-def
 '(cortical
   (rm . 40000)				; ohms-cm2
   (ri  . 200)				; ohms-cm
   (cm . 0.7)				; uF/cm2
   (v-leak . -65))			; mV
 )

;; The first step is a call to CREATE-CELL, which creates a soma with a diameter of 35 microns.
;; We assign the returned cell to a local variable CELL defined in the LET form, for the subsequent functions.
(let ((cell (create-cell "Demo-Cell"
			 :soma-diameter 35	; Soma diameter in ums.
			 :origin '(100 -300 50) ; XYZ coordinates of the soma in microns.
			 :cell-type 'cortical))) ; CORTICAL defined above with CELL-TYPE-DEF.  

  ;; Now create an "apical" and a "basal" dendrite.
  (segment-chain			; SEGMENT-CHAIN adds a simple straight chain of dendritic segments,
   (cell-soma cell)					; in this case to the soma.
   'apical				; Label to add to the name of each segment.
   5					; Number of segments in chain.
   (/ 1200 5)				; Length of each segment in microns.
   12					; Segment diameter in microns.
   :proximal-theta (* 0.5 pi))		; Relative orientation of chain in radians. Default for :PROXIMAL-PHI = 0.
  
  (segment-chain (cell-soma cell) 'basal 3 (/ 500 3) 6 :proximal-theta (* -0.5 pi)) ; Now add a "basal" cable to the soma. 

  ;; Add some channels to the soma.
  (create-element (cell-soma cell) 'na-hh-ext 'dr-hh-ext) ; NA-HH-EXT and DR-HH-EXT channel types are fits to the canonical HH channels.
  
  ;; Setup a current source stimulus, the plot flags and the simulation duration.
  (pulse-list				; Install and setup a current source at the soma.
   (add-isource (cell-soma cell))			; ADD-ISOURCE returns the current source.
   '(5 40 .25))				; A 0.5 nA pulse from 10 to 20 ms.

  (enable-element-plot (cell-elements))	; CELL-ELEMENTS returns all segments and somas, flagging voltage plotting by default.
  (enable-element-plot 'isource)	; Symbols referring to various types of elements can also be used for many ELEMENT-based functions.  
  (enable-element-plot 'channel '(current conductance)) ; The argument 'CHANNEL flags all the channels, here for their current and
					; conductance values.

  ;; The fact that the following global variables are set here (as well as the choices used) are just for demonstration.  

  (setq *plot-channels-by-major-ion* nil ; Make sure to plot all channels together.
	*save-data-step*             1   ; Save plot data at every time step. 
	*user-stop-time* 50)		; A 50ms simulation.

  (just-draw :mark-plotted-nodes t :label-plotted-nodes t)
  )



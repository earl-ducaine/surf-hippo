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

Example code for running a Rallpack 3 defined axon current clamp simulation of repetitive firing, as
fully described in:

	AUTHOR = {Borg-Graham, L.},
	TITLE = {Additional Efficient Computation of Branched Nerve Equations: Adaptive Time Step and Ideal Voltage Clamp},
	NOTE = {Submitted to Journal of Computational Neuroscience},
	YEAR = {1999}

Note that the channel and particle types defined below are included in the basic Surf-Hippo image.

This file can be loaded directly into Surf-Hippo for demonstration.

|#

(in-package "SURF-HIPPO")


(channel-type-def
 '(NA-HH
   (gbar-density . 1200)                 ; pS/um2
   (e-rev . 50)                          ; mV
   (v-particles . ((M-HH 3) (H-HH 1))))) ; There are 3 M-HH particles and 1 H-HH particle.

(channel-type-def
 '(DR-HH
   (gbar-density . 360)                ; pS/um2
   (e-rev . -77)                       ; mV
   (v-particles . ((N-HH 4)))))        ; There are 4 N-HH particles.

(particle-type-def
 `(M-HH
   (class . :HH)                        ; This particle is of the canonical HH form.
   (alpha-function . M-HH-ALPHA)        ; The forward rate constant is given by this function.
   (beta-function .  M-HH-BETA)))       ; The backward rate constant is given by this function.
   
(particle-type-def
 `(H-HH
   (class . :HH)
   (alpha-function . H-HH-ALPHA)
   (beta-function . H-HH-BETA)))

(particle-type-def
 `(N-HH
   (class . :HH)
   (alpha-function . N-HH-ALPHA)
   (beta-function . N-HH-BETA)))

(defun m-hh-alpha (voltage)
  (/ (* -0.1 (- voltage -40))
     (1- (exp (/ (- voltage -40) -10)))))

(defun m-hh-beta (voltage)
  (* 4 (exp (/ (- voltage -65) -18))))

(defun h-hh-alpha (voltage)
  (* 0.07 (exp (/ (- voltage -65) -20))))

(defun h-hh-beta (voltage)
  (/ 1.0 (1+ (exp (/ (- voltage -35) -10)))))

(defun n-hh-alpha (voltage)
  (/ (* -0.01 (- voltage -55))
     (1- (exp (/ (- voltage -55) -10)))))

(defun n-hh-beta (voltage)
  (* 0.125 (exp (/ (- voltage -65) -80))))

(cell-type-def
 '(HH-AXON
   (rm . 40000)      ; ohms cm2
   (ri . 100)        ; ohms cm
   (cm . 1)          ; uF/cm2
   (v-leak . -65)))  ; mV
   

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


;; These are the default values. 
(setq *use-fixed-step* nil		; Enable adaptive time step.
      *absolute-voltage-error* 0.05	; mV
      *absolute-particle-error*  0.001	; dimensionless
      *user-max-step*  5		; ms
      *user-min-step*  0		; ms
      *pick-time-step-fudge* 0.8	; dimensionless
      *save-data-step* 1		; Save every time point.
      )

(circuit-load 'RALLPACK-3-11)

(add-isource *SOMA*)	

(pulse-list *ISOURCE* '(10 50 0.1))

(enable-element-plot *ISOURCE*)     ; Default for current sources is the current.
(enable-element-plot *SOMA*)        ; Default for somas is the voltage.
(enable-element-plot (distal-tips)) ; Default for segments is the voltage.

(setq *USER-STOP-TIME* 50)	

(just-draw :mark-plotted-nodes t :label-plotted-nodes t)
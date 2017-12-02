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

;
; Parameters for axon types.
;

(in-package "SURF-HIPPO")



(defvar *axon-default-reference-temperature* 35.0 "Degrees C")
(defvar *axon-default-q10* 1.5) 


; check this: Length of intracortical axons of pyramidal cells varies, but can extend up to 6 mm in
; larger mammals (cats, primates).  Conduction velocity estimates range from 0.05 to 0.1 m/sec.

;; from rallpack-3-1000, putting qten of 1.5 for na-hh and dr-hh, ref-temp 27, gbar qten 

(defvar *axon-default-propagation-velocity* 0.10 "meters/second") 

(defvar *axon-default-input-threshold* -50.0 "mV") 
(defvar *axon-default-refractory-period* 2.0 "milliseconds") 
(defvar *axon-default-supra-threshold-duration-min* 0.1 "milliseconds") 
(defvar *axon-default-waveform-time-interval* 0.1 "milliseconds") 
(defvar *axon-default-waveform-reference* -70.0 "mV")

(axon-type-def
 '(simple
   (q10 . 1.5)
   (reference-temp . 35.0)
   (waveform-reference . -70.0)
   (extra-parameters . nil)
   (propagation-velocity . 1.0)
   (input-threshold . -50.0)
   (refractory-period . 04.0)
   (supra-threshold-duration-min . 0.10)
   (waveform . (0 10 40 70 100 110 105 95 85 75 65 56 48 41 35 30 26 23 20 17 14 12 10 8 6 4 3  2 1 0))
   (waveform-time-interval . 0.1)))

(axon-type-def
 '(simple-fast
   (q10 . 1.5)
   (reference-temp . 35.0)
   (waveform-reference . -70.0)
   (extra-parameters . nil)
   (propagation-velocity . 1.0)
   (input-threshold . -50.0)
   (refractory-period . 02.0)
   (supra-threshold-duration-min . 0.10)
   (waveform . (0 40 70 100 110 105 90 75 60 47 35 25 18 12 8 5 3 2 1 0))
   (waveform-time-interval . 0.1)))



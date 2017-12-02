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

;; Schematic parameters for synapse types. Very schematic...

(synapse-type-def
 '(fast-ex
   (control . :event)
   (e-rev . 0)				; mV
   (waveform-spec . (double-exponential 0.10 10.0 :step 0.2 :length 400 :normalize t))
   (waveform-time-interval . 0.2)	; ms
   (gbar-density . 50)))		; pS/um2

(synapse-type-def
 '(fast-ex-abs
   (parent-type . fast-ex)
   (gbar . 0.001)))			; uS

(synapse-type-def
 '(fast-ex-alpha
   (parent-type . fast-ex)
   (waveform-spec . (alpha-array 1.5 :step 0.2))
   (waveform-time-interval . 0.2)))

(synapse-type-def
 '(fast-ex-alpha-abs
   (parent-type . fast-ex-alpha)
   (gbar . 0.001)))

(synapse-type-def
 '(fast-ex-alpha-voltage
   (parent-type . fast-ex-alpha)
   (refractory-period . 2)		; ms
   (input-THRESHOLD . 10)		; mV
   (control . :voltage)))

(synapse-type-quoted-def
 `(nmda-alpha
   (parent-type . fast-ex-alpha)
   (static-voltage-dependence-function . (SIGMOID-ARRAY -50.0 0.5 
							,*PARTICLE-LOOK-UP-TABLE-MIN-VOLTAGE*
							,*PARTICLE-LOOK-UP-TABLE-MAX-VOLTAGE* 
							,*PARTICLE-LOOK-UP-TABLE-PRECISION*))))

(synapse-type-def
 `(nmda-alpha-auto
   (parent-type . nmda-alpha)
   (control . :event)))

;; Inhibitory

(synapse-type-def
 '(gaba-a
   (e-rev . -70)
   (waveform-spec . (double-exponential .5 20 :step 0.2))
   (waveform-time-interval . 0.2)
   (gbar-density . 1000)
   (control . :event)))

(synapse-type-def
 '(gaba-a-abs
   (parent-type . gaba-a)
   (gbar . 0.02)))

(synapse-type-def
 '(gaba-a-alpha
   (parent-type . gaba-a)
   (waveform-spec . (alpha-array 10 :step 0.2))
   (waveform-time-interval . 0.2)))

(synapse-type-def
 '(gaba-b-alpha
   (e-rev . -90)
   (waveform-spec . (alpha-array 40 :step 0.2))
   (waveform-time-interval . 0.2)
   (gbar-density . 1000)
   (control . :event)))

(synapse-type-def
 '(gaba-b-alpha-abs
   (parent-type . gaba-b-alpha)
   (gbar . 0.002)))

(synapse-type-def
 '(gaba-b-alpha-voltage
   (parent-type . gaba-b-alpha)
   (refractory-period . 2)
   (input-THRESHOLD . -30)
   (control . :voltage)))






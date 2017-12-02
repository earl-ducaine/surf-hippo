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
; Parameters for synapse types.
;

(in-package "SURF-HIPPO")


#|

Parameters described in:

@ARTICLE{Ber-Dog-Mar-Koc-91,
	AUTHOR = {Bernander, O. and Douglas, R. J. and Martin, K. A. C. and Koch, C.},
	TITLE = {Synaptic background activity influences spatiotemporal integration in single pyramidal cells},
        Journal = {Proc. Natl. Acad. Sci. USA},
	YEAR = {1991},
	VOLUME = {88},
	PAGES = {11569-11573}
}

|#

(synapse-type-def
 '(ampa-bernander-91
   (CONTROL . :event)
   (e-rev . 0)				; mV
   (waveform-spec . (alpha-array 1.5 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2)	; ms - consistent with :STEP arg for ALPHA-ARRAY.
   (gbar . 0.0005)			; uS
   ))

(synapse-type-def
 '(gaba-a-bernander-91
   (CONTROL . :event)
   (gbar . 0.001)			; uS
   (e-rev . -70)			; mV
   (waveform-spec . (alpha-array 10 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2)	; ms - consistent with :STEP arg for ALPHA-ARRAY.
   ))

(synapse-type-def
 '(gaba-b-bernander-91
   (CONTROL . :event)
   (gbar . 0.0001)			; uS
   (e-rev . -95)			; mV
   (waveform-spec . (alpha-array 40 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2)	; ms - consistent with :STEP arg for ALPHA-ARRAY.
   ))


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

#| A description of a slow TTX-insensitive Na current in neocortical cells:
@ARTICLE{Hoe-Wat-Mac-93,
	AUTHOR = {Hoehn, K. and Watson, T. W. J. and MacVicar, B. A.},
	TITLE = {A novel tetrodotoxin-insensitive, slow sodium current in striatal and hippocampal neurons},
	JOURNAL = {Neuron},
	YEAR = {1993},
	VOLUME = {10},
	PAGES = {543-552},
	MONTH = {March}
}

@ARTICLE{Dei-96,
	AUTHOR = {Deisz, R. A.},
	TITLE = {A tetrodozin-insensitive sodium current initiates burst firing of neocortical neurons},
	JOURNAL = {Neuroscience},
	YEAR = {1996},
	VOLUME = {70},
	NUMBER = {2},
	PAGES = {341-351}
}

evidence for na by blockage when na replaced with choline

conventional ca block by 2mM Co
conventional na block by 10uM TTX
|#

(channel-type-def
 `(na-deisz96

   ; cf fig 4 - max anodal break hump is about 7mV from a "resting" potential of -73. Assuming Ena of
   ; 50mV, and a cell impedance of 30mV/1nA = 30Mohms,
   ;
   ;    gbar = (7mV/30Mohms)/(50mV - -73mV) = .233nA/123mV =  1.9nS

   (gbar . 2.0e-3)			; uS
   (e-rev . 50.0)			
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-deisz96 1)(nah-deisz96 1)))))



;; Time constant of de-inactivation is less than 90 ms at between -85 and -75mV, cf. Figure 5. This
;; would be an upper bound, since there may be a slower Ca component here. 
(particle-type-def
 `(nah-deisz96  
   (class . :hh-ext)
   (VALENCE . -10)			;not bad by eye to fig 4c


   (V-HALF . -80.0)			;cf figure 4c - amplitude of anodal break response vs
					;hyperpolarizing level

   (base-rate-undefined . t)		; for now
   (TAU-0 . 90.0)
   
   (QTEN . 1))				
 )


;; under ttx and co, transient hump in response to depolarizing steps from -77mV has threshold about
;; -60 (cf fig 6d) with peak at about 40 ms. tau peak more or less same for all depolarizations.
(particle-type-def
 `(nam-deisz96  
   (class . :hh-ext)
   (VALENCE . 10)			; this give a threshold at about -60
   (V-HALF . -50.0)
   (base-rate-undefined . t)		; for now
   (TAU-0 . 30.0)			; maybe this is better than 40 
   (QTEN . 1))				
 )

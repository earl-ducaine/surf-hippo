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
; Schematic parameters for light synapse types.
;

(in-package "SURF-HIPPO")

(synapse-type-def
 '(l-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 10 60))))

(synapse-type-quoted-def
 `(l-ex-1-nmda
   (parent-type . l-ex-1)
   (static-voltage-dependence-function . (SIGMOID-ARRAY -50.0 0.5 
							,*PARTICLE-LOOK-UP-TABLE-MIN-VOLTAGE*
							,*PARTICLE-LOOK-UP-TABLE-MAX-VOLTAGE* 
							,*PARTICLE-LOOK-UP-TABLE-PRECISION*))))

(synapse-type-def
 '(l-in-1
   (gbar-density . 1000.0)
   (e-rev . -70.0)
   (control . :light)
   (impulse-function . (alpha-array 100 :ADJUSTMENT :UNIT-AREA ))))

(synapse-type-def
 '(l-ON-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 10 60))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 20 20))))

(synapse-type-def
 '(l-OFF-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 60 10))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 20 20))))

(synapse-type-def
 '(l-OFF-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 50 50 :grid-size 50))))

(synapse-type-def
 '(l-ONcs-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 2 12))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1))))

(synapse-type-def
 '(l-OFFcs-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1))))

(synapse-type-def
 '(l-ONcs-ex-3
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 2 12))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1 :grid-size 30))))

(synapse-type-def
 '(l-OFFcs-ex-3
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1 :grid-size 30))))

;; Why is this "fac"ilitating?
(synapse-type-def
 '(l-ex-fac
   (gbar-density .	 50.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 10 40 .9))))


(synapse-type-def
 '(transient-light-ex
   (gbar-density .	 50.0)
   (e-rev . 0.0)
   (control . :light)
   (impulse-function . (double-alpha 10 40 1.0))))

(synapse-type-def
 '(transient-light-inh
   (gbar-density .	 50.0)
   (e-rev . -70.0)
   (control . :light)
   (impulse-function . (double-alpha 10 40 1.0 :start 10))))


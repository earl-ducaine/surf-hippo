;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: pump-preliminaries.lisp

; These pump functions must be compiled before conc-int.lisp


(in-package "SURF-HIPPO")

(proclaim '(inline pump-conc-int-compartment-volume))
(defun pump-conc-int-compartment-volume (pump)
  "PUMP compartment volume in cm^3."
  (let ((cint (pump-conc-int pump)))
    (case (pump-conc-int-compartment pump)
      (1 (conc-int-shell-1-volume cint)) ; cm3
      (2 (conc-int-shell-2-volume cint))
      (3 (conc-int-shell-3-volume cint))
      (t (conc-int-total-volume cint)))))

(proclaim '(inline pump-concentration-current))
(defun pump-concentration-current (pump &optional compartment-volume)
  "Returns mM/ms. COMPARTMENT-VOLUME is in cm^3."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((volume (the df (or compartment-volume (pump-conc-int-compartment-volume pump)))))
    (if (= volume 0)
	0.0d0
	(/ (pump-current pump)		; millimoles/ms
	   (* (the df volume )
	      1.0d-3))			; liters/cm3
	)))


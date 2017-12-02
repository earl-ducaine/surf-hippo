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

;; adp models

(in-package "SURF-HIPPO")

(defun adp-c12861-cable ()
  (ball-and-sticks
   :apical-dendrite-diameter 5.0	; 6.0 ;8.0 ; 3.6055512 ; 9.196152
   :soma-diameter 18.2
   :ri 550 :rm 20000 :rm-soma 10000
   :cm 1 :cm-soma 0.75
   :apical-dendrite-length 1000 :apical-total-segs 50)
  (setq user-stop-time 12)
  (setq *relative-voltage-error 0.001)
  (enable-element-plot *soma*)
  (pulse-list (add-isource *soma*) '((1.0 2.9 1.0) (2.9 3.30 -1.2)))
  (enable-element-plot *isource*)
  *cell*)

(defun adp-c12861 ()
  (read-in-circuit "/home/lyle/surf-hippo/anatomy/misc/c12861.ca1")
  (enable-element-plot *soma*)
  (setq user-stop-time 12)
  (setq *relative-voltage-error 0.001)
  (let ((cell-type (cell-type *cell*)))
    (cell-type-parameter cell-type :rm-soma 10000)
    (cell-type-parameter cell-type :rm 20000)
    (cell-type-parameter cell-type :ri 550)
    (pulse-list (add-isource *soma*) '((1.0 2.9 1.0) (2.9 3.30 -1.2)))
    (enable-element-plot *isource*))
  *cell*)

(defun adp-HIPPO-f5 ()
  (ball-and-sticks :apical-dendrite-diameter 6 :apical-dendrite-length 10000 :apical-total-segs 50)
  (enable-element-plot *soma*)
  (setq user-stop-time 12)
  (setq *relative-voltage-error 0.001)
  (let ((cell-type (cell-type *cell*)))
    (cell-type-parameter cell-type :rm-soma 10000)
    (cell-type-parameter cell-type :rm 20000)
    (cell-type-parameter cell-type :ri 550))
  (pulse-list (add-isource *soma*) '((1 2.9 1) (2.9 3.3 -1.2)))
  (enable-element-plot *isource*)
  *cell*)





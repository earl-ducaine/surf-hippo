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



;; This model is designed to reproduce the results of Stuart and Sakmann, Nature, V367, 6 Jan 1994,
;; "Active propagation of somatic action potentials into neocortical pyramidal cell dendrites"

(channel-type-def
 '(NA-ss
   (gbar-density . 50.0)
   (e-rev . 60.0)
   (ion-permeabilities . ((NA 1.0)))
   (q10 . 1.5)
   (reference-temp . 24.0)
   (v-particles . ((M1-ss 2)(H1-ss 1)))))


(particle-type-def
 '(M1-ss
   (class . HH-EXT)
   (valence . 8.0)
   (gamma . 0.5)
   (qten . 3.0)
   (base-rate . 0.3)
   (v-half . -50.0)
   (tau-0 . 0.5)
   (reference-temp . 24.0)))


(particle-type-def
 '(H1-ss
   (class . HH-EXT)
   (valence . -15.0)
   (gamma . 0.7)
   (qten . 3.0)
   (base-rate . 0.001)
   (v-half . -62.0)
   (tau-0 . 1.0)
   (reference-temp . 24.0)))


(channel-type-def
 '(DR-ss
   (gbar-density . 20.0)
   (e-rev . -70.0)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.5)
   (reference-temp . 30.0)
   (v-particles . ((DRX-ss 4)(DRY-ss 1)))))


(particle-type-def
 '(DRX-ss
   (class . HH-EXT)
   (valence . 12.0)
   (gamma . 0.7)
   (qten . 3.0)
   (base-rate . 0.002)
   (v-half . -38.0)
   (tau-0 . 1.0)
   (reference-temp . 30.0)))

(particle-type-def
 '(DRY-ss
   (class . HH-EXT)
   (valence . -9.0)
   (gamma . 0.8)
   (qten . 3.0)
   (base-rate . 3.9999995e-4)
   (v-half . -45.0)
   (tau-0 . 6.0)
   (reference-temp . 30.0)))


(defun stuart-sakmann ()
  (create-cell "stuart-sakmann"
	       :soma-diameter 25
	       :tree-list
	       ;; Segment-list format: (mother-segment-name segment-name x y z diameter extras-list)
	       `(
		 (soma	1-1 0 100 0 6)
		 (1-1    1-2 0 175 0 4)
		 (1-2    1-3 0 250 0 4)
		 (1-3    1-4 0 325 0 4)
		 (1-4    1-5 0 400 0 4)
		 (1-5    1-6 0 475 0 4)
		 (1-6    1-7 0 550 0 4)
		 (1-7    1-8 -25 575 0 3)
		 (1-8    1-9 -75 600 0 3)
		 (1-9    1-10 -100 625 0 2)
		 (1-7    2-1 25 575 0 3)
		 (2-1    2-2 75 600 0 3)
		 (2-2    2-3 100 625 0 2)
		 (1-5    1-5-1 40.0 420.0 0.0 2.0)
		 (1-4    1-4-1 -50.0 320.0 0.0 2.0)
		 (1-3    1-3-1 50.0 250.0 0.0 2.0)
		 (1-10   1-10-1a -150.0 625.0 0.0 1.5)
		 (1-10   1-10-1b -100.0 650.0 0.0 1.50)
		 (2-3    2-3-1a 150.0 625.0 0.0 1.50)
		 (2-3    2-3-1b 100.0 650.0 0.0 1.50)))
  (create-element 'na-ss (element "1-5" "1-4" "1-7" "1-6" "1-1" "1-3" "1-2") 'segment)
  (create-channels *soma* '(na-ss dr-ss)))

(push 'stuart-sakmann *CIRCUIT-CATALOG-FUNCTIONS*)

(defun input-basic-cell (cell-function)
  (initialize-globals-for-circuit)
  (funcall cell-function)
  (setq *circuit-loaded* t)
  (process-circuit-structure)
  (set-circuit-elements-parameters)
  )

(defun axon ()
  (let ((soma (create-soma :cell "axon" :diameter 10.0)))
    (make-soma-segment-chain soma "chain" 30 50.0 1.0 nil nil)
    (create-element (segments) '(na-ss dr-ss))))

(defun small-basic ()
  (input-basic-cell)
  (loop for seg in (segments) do (setf (segment-diameter seg) (* 0.8 (segment-diameter seg)))
	(set-segment-membrane-parameters seg)))

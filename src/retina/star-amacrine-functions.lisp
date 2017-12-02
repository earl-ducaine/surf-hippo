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

;;; Functions using Cholinergic rabbit retina starburst AMACRINE cell described in rabbit/star-amacrine.lisp.


(channel-type-def
 '(dr-RET-hh-ext
   (gbar-density . 360.0)
   (e-rev . -77.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((n-RET-hh-ext 4)))))

(particle-type-def
 `(N-RET-HH-EXT
   (class . :HH-EXT)
   (valence . 1.5)
   (gamma . 0.9)			;  dimensionless - between 0 and 1
   (base-rate . 0.14)			;  1/ms
   (v-half . -41.0)			;  mV
   (tau-0 . 1.0)			;  ms
   ))

(channel-type-def
 '(na-RET-hh-ext
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((m-RET-hh-ext 3) (H-RET-hh-ext 1)))))

(particle-type-def
 `(M-RET-HH-EXT
   (class . :HH-EXT)
   (valence . 2.7)
   (gamma . 0.4)			;  dimensionless - between 0 and 1
   (base-rate . 1.2)			;  1/ms
   (v-half . -30.0)			;  mV
   (tau-0 . 0.07)			;  ms
   ))

(particle-type-def
 `(H-RET-HH-EXT
   (class . :HH-EXT)
   (valence . -3.7)
   (gamma . 0.4)			;  dimensionless - between 0 and 1
   (base-rate . 0.07)			;  1/ms
   (v-half . -52.0)			;  mV
   (tau-0 . 0.9)			;  ms
   ))



(defun rightward-bar () ;; This is a moving bar, from left to right.
  (setq
   *light-stimulus-plane* :xy
   *enable-light* t
   *light-stimulus* :ON-MOVING-BAR
   *light-speed* 2			; microns/millisecond
   *bar-length* 500 *bar-width* 50	; microns
   *light-theta* (deg-to-rad 90)	; radians
   *light-direction* nil		; T (nil) => movement is in the direction of / opposite to *light-theta-degrees
   *light-stimulus-start-time* 0	; Time to start bar moving, milliseconds
   *light-stimulus-stop-time* 100000	; Time to stop bar moving, milliseconds
   *light-start-position-x* -300	; X stimulus position at *light-stimulus-start-time*, in microns
   *light-start-position-y* 0))		; Y stimulus position at *light-stimulus-start-time*, in microns

(defun star-amacrine-ds ()
  (rightward-bar)
  (setq *enable-synapses* t
	*user-stop-time* 300)
  (star-amacrine)
  (create-element *soma*		; '(na1-TR1161 na2-TR1161 na3-TR1161 dr-TR1161 a-TR1161) original
		  'na-ret-hh-ext 'dr-ret-hh-ext) ; Squid axon, with all v-half's depolarized by 10mV.
  (create-element (segments) '(l-ex-fac L-IN-1))
  ;; This gets the soma, and a far left, far right, and top node.
  (enable-element-plot `(,*soma* 120 198 166))
  (enable-element-plot (synapses `(,*soma* 120 198 166)) '(current conductance)))

;; (circuit-load 'star-amacrine-ds)

(defun star-amacrine-demo ()
  (unless (equal *circuit-function* 'STAR-AMACRINE-DS)
    (circuit-load 'STAR-AMACRINE-DS))
  (setq *enable-colorize-time* t	; Enable time display in colorized histology windows.
	*enable-colorize-scale* t	; Enable color scale display in colorized histology windows.
	*colorize-simulation* t		; Enable colorization of simulation in some or all histology windows.
	*enable-sparse-data* t		; Enable data storage from all the circuit elements.
	*sparse-data-step* 0.1		; Time step (ms) target for sparse data storage. Simulation times for sparse data are collected in *SPARSE-DATA-TIMES*.
	)
  (run)
  (just-draw :scale 1.5 :draw-synapse-stimulus t)
  (set-color-map :map 'gray-rgb-map)
  (replay-colorized-simulation :time-step 0.2 :repetitions 10 :include-colorizing-scale t))
  


#|
;; Plot distal nodes, according to their angle
(enable-element-save-data (distal-tips))

;; Two versions, using PLOT-SCATTER and PLOT-POLAR-DATA.
(defun plot-tip-outputs ()
  (plot-scatter
   (loop for seg in (distal-tips)
	 collect
	 (let ((angle (atan (where-y seg) (where-x seg)))
	       (mag (integrate-x-y (element-data seg) (current-sim-plot-time-list) :y-base -70)))
	   (list (* mag (cos angle)) (* mag (sin angle)))))))

(defun plot-tip-outputs ()
  (plot-polar-data
   (loop for seg in (distal-tips)
	 collect (integrate-x-y (element-data seg) (current-sim-plot-time-list) :y-base -70) into rs
	 collect (atan (where-y seg) (where-x seg)) into ts
	 finally (return (list (list rs ts))))))
|#



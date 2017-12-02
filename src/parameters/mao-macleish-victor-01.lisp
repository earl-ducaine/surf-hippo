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

; From Mao, MacLeish and Victor (submitted JCNS 2001): Relation between Potassium-channel Kinetics and the Intrinsic Dynamics in
; Isolated Retinal Bipolar Cells.

(defun Mao-etal-bipolar ()
  (create-cell 'Mao-etal-bipolar :soma-diameter (sphere-diameter-from-capacitance 10) :cell-type (create-celltype 'Mao-etal-bipolar :specific-capacitance 1)))

#|
(progn
  (topload 'MAO-ETAL-BIPOLAR)
  (create-element 'dr-mao-bipolar 'a-mao-bipolar *soma*)
  (std-setup)
  (enable-element-plot '(particle channel))
  (disable-element-plot *isource*)
  )

(defun foo ()
  (let ((*user-stop-time* 500)
	(*save-data-step* 1)
	(*standard-graphics-width* 250)
	(*standard-graphics-height* 150)
	(*interpolate-particle-arrays* t))
    (loop for gbar-a in '(0 ; 0.001 0.01
			    0.03) do
	  (element-type-parameter 'a-mao-bipolar 'gbar gbar-a)
	  (clamp-steps :start-clamp 20e-3 :stop-clamp 60e-3 :step 10e-3 :lock-plots t
		       :include-comment t :comment (format nil "Gbar A-MAO-BIPOLAR ~,2euS" gbar-a)))))

;; band pass given by slow (~20ms) tau of Idr compared to 1ms tau of IA
;;
(defun foo ()
  (let ((*user-stop-time* 150)
	(*save-data-step* 1)
	(*standard-graphics-width* 325)
	(*standard-graphics-height* 200)
	(*interpolate-particle-arrays* t))
    (loop for tau-a-a-mao-bipolar in '(1 2 4 20) do
	  (turn-off 'dr-mao-bipolar)
	  (element-type-parameter 'a-mao-bipolar 'gbar 0.01)
	  (set-particle-type-tau 'a-a-mao-bipolar tau-a-a-mao-bipolar)
	  (clamp-steps :start-clamp 20e-3 :stop-clamp 60e-3 :step 10e-3 :lock-plots t
		       :include-comment t :comment (format nil "Tau A-MAO-BIPOLAR ~,2ems" tau-a-a-mao-bipolar)))))

|#

(channel-type-def
 '(dr-mao-bipolar
   (gbar . 5e-3)			; 5 nS
   (e-rev . -90)
   (ion-permeabilities . ((K 1)))
   (v-particles . ((n-dr-mao-bipolar 2)))))

(particle-type-def
 `(n-dr-mao-bipolar
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v+3 (- voltage -3)))
		(/ (* 0.003 v+3) (- 1 (exp (/ v+3 -8)))))))
   (beta . (lambda (voltage)
	     (let ((v+30 (- voltage -30)))
	       (/ (* -0.0002 v+30) (- 1 (exp (/ v+30 80)))))))))

(channel-type-def
 '(a-mao-bipolar
   (gbar . 1e-3)			; In the paper, they tried 0, 1, 2, 4, 8, 15, and 25 nS
   (e-rev . -90)
   (ion-permeabilities . ((K 1)))
   (v-particles . ((a-a-mao-bipolar 3) (b-a-mao-bipolar 1)))))

(particle-type-def
 `(a-a-mao-bipolar			; Activation
   (class . :hh)
   (ss . (lambda (voltage)
	   (expt (/ 1 (+ 1 (exp (/ (+ voltage -20) -8)))) (/ 3))))
   (tau . 1)				; ms
   ))

(particle-type-def
 `(b-a-mao-bipolar			; Inactivation
   (class . :hh)
   (ss . (lambda (voltage)
	   (/ 1 (+ 1 (exp (/ (+ voltage -30) 14))))))
   (tau . (lambda (voltage)
	    (+ 0.2 (/ 0.8 (+ 1 (exp (/ (+ voltage 40) 16)))))))))

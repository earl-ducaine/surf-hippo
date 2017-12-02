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

;;; 3-state Markovian models of Na channel

;; Includes 3 state markovian model of Na channel as described in Des-Man-Sej-94 -

#|
@ARTICLE{Des-Man-Sej-94,
	AUTHOR = {Destexhe, A. and Mainen, Z. F. and Sejnowski, T.},
	TITLE = {Synthesis of models for excitable membranes, synaptic transmission and neuromodulation using a common kinetic formalism},
	JOURNAL = {Journal of Computational Neuroscience},
	YEAR = {1994},
	VOLUME = {1},
	PAGES = {195-230}
}

Note that there is an error in this model - the "b" term for the rate term "r4" should be negated.


|#

(defvar Destexhe-3state-na-states)
(setq Destexhe-3state-na-states     '(C O I))
(defvar Destexhe-3state-na-open-states)
(setq Destexhe-3state-na-open-states  '(O))

;; Markov voltage functions to fill arrays must take single floats and return single float rates (1/ms).
;; Voltage in mV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destexhe-rate-function (a b c voltage)
  (declare (optimize (speed 3) (space 0))
	   (single-float voltage a b c))
  (/ a (+ 1 (exp (/ (- voltage c) (- b))))))

#|
;; Don't load the Destexhe model unless explicitely desired. 
(defvar *destexhe-3state-na-a1* 1.5)	; 1/ms
(defvar *destexhe-3state-na-a2* 0.2)
(defvar *destexhe-3state-na-a4* 0.15)

(defvar *destexhe-3state-na-b1* 5.0)	; mV
(defvar *destexhe-3state-na-b4* -5.0)	; note error in paper
(defvar *destexhe-3state-na-c1* -27.0)
(defvar *destexhe-3state-na-c2* -27.0)
(defvar *destexhe-3state-na-c4* -67.0)

(defvar *destexhe-3state-na-r3* 3.0)	; 1/ms

(defvar *destexhe-3state-na-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)
(setq *destexhe-3state-na-STATE-VOLTAGE-TRANSITION-FUNCTIONS*
      `((c o (lambda (voltage)		; r1
	       (destexhe-rate-function *destexhe-3state-na-a1* *destexhe-3state-na-b1*
				       *destexhe-3state-na-c1* voltage)))
	(o c (lambda (voltage)		; r2
	       (destexhe-rate-function *destexhe-3state-na-a2* *destexhe-3state-na-b1*
				       *destexhe-3state-na-c2* voltage)))
	(o i (lambda (voltage)		;r3
	       *destexhe-3state-na-r3*))
	(i c (lambda (voltage)		; r4
	       (destexhe-rate-function *destexhe-3state-na-a4* *destexhe-3state-na-b4*
				       *destexhe-3state-na-c4* voltage)))))
      
	

(channel-type-def
 '(destexhe-3state-na
   (gbar . 4.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((destexhe-3state-nax 1)))))

(particle-type-quoted-def
 `(destexhe-3state-nax
   (class . :markov)
   (states . ,Destexhe-3state-na-states)
   (open-states . ,Destexhe-3state-na-open-states)
   (STATE-TRANSITIONS . ,*destexhe-3state-na-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)))

(defun test-destexhe-3state-na ()
  (topload 'dead-hippo)(create-channel *soma* 'destexhe-3state-na)
  (enable-element-plot (car (particles)))
  (set-element-parameter (car (particles)) 'plot-markov-states t))


|#

;;;;;;;;;;;;;;; 3 state na gen
(defvar *na-3state-gen-a1* 1.5)	; 1/ms
(defvar *na-3state-gen-a2* 0.2)
(defvar *na-3state-gen-a3* 6.0)	; 1/ms
(defvar *na-3state-gen-a4* 0.15)

(defvar *na-3state-gen-b1* 2.0)	; mV - original 5
(defvar *na-3state-gen-b3* 40.0)
(defvar *na-3state-gen-b4* -5.0)	; mV

(defvar *na-3state-gen-c1* -45.0)	; original -27
(defvar *na-3state-gen-c2* -45.0)	; original -27
(defvar *na-3state-gen-c3* -10.0)	; original -27
(defvar *na-3state-gen-c4* -75.0)

(defvar *na-3state-gen-r3* 3.0)	; 1/ms

(defvar *na-3state-gen-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)
(setq *na-3state-gen-STATE-VOLTAGE-TRANSITION-FUNCTIONS*
      `((c o (lambda (voltage)		; r1
	       (destexhe-rate-function *na-3state-gen-a1* *na-3state-gen-b1* *na-3state-gen-c1* voltage)))
	(o c (lambda (voltage)		; r2
	       (destexhe-rate-function *na-3state-gen-a2* *na-3state-gen-b1* *na-3state-gen-c2* voltage)))
	(o i (lambda (voltage)		;r3
	       *na-3state-gen-r3*))
	(i c (lambda (voltage)		; r4
	       (destexhe-rate-function *na-3state-gen-a4* *na-3state-gen-b4* *na-3state-gen-c4* voltage)))))

	

(channel-type-def
 '(na-3state-gen
   (gbar . 4.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((na-3state-genx 1)))))

(particle-type-quoted-def
 `(na-3state-genx
   (class . :markov)
   (states . ,Destexhe-3state-na-states)
   (open-states . ,Destexhe-3state-na-open-states)
   (STATE-TRANSITIONS . ,*na-3state-gen-STATE-VOLTAGE-TRANSITION-FUNCTIONS*)))

(defun test-na-3state-gen ()
  (topload 'dead-hippo)
  (create-channel *soma* 'na-3state-gen)
  (enable-element-plot (car (particles)))
  (set-element-parameter (car (particles)) 'plot-markov-states t))


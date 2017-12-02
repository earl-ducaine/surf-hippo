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

;; Markovian model of Na channel as described in Des-Man-Sej-94 (which references Van-Bez-91b), using this paper's
;; equations for the NA-DESTEXHE-VANDENBERG-BEZANILLA channel type.

#|
@ARTICLE{Des-Man-Sej-94,
	AUTHOR = {Destexhe, A. and Mainen, Z. F. and Sejnowski, T.},
	TITLE = {Synthesis of models for excitable membranes, synaptic transmission and neuromodulation using a common kinetic formalism},
	JOURNAL = {Journal of Computational Neuroscience},
	YEAR = {1994},
	VOLUME = {1},
	PAGES = {195-230}}
|#




;; Original version in vandenberg-bezanilla.lisp works


(channel-type-def
 '(NA-dvb94
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((NAx-dvb94 1)))))

;; Equation 9 from the Destexhe et al paper - voltage is in mV, and returns 1/ms
(defmacro markov-exponential-rate (a b)
  `#'(lambda (voltage)
       (declare (optimize (speed 3) (space 0)) 
		(single-float voltage))
       (* 1.0e-3 ,a (exp (- (/ voltage ,b))))))

(defmacro markov-exponential-quotient-rate (a1 b1 a2 b2 a3 b3)
  `#'(lambda (voltage)
       (declare (optimize (speed 3) (space 0)) 
		(single-float voltage))
       (/ (* (* 1.0e-3 ,a1 (exp (- (/ voltage ,b1))))
	     (* 1.0e-3 ,a2 (exp (- (/ voltage ,b2)))))
	  (* 1.0e-3 ,a3 (exp (- (/ voltage ,b3)))))))

(defun markov-exponential-rate-fun (a b voltage)
  (* 1.0e-3 a (exp (- (/ voltage b)))))

(defvar destexhe-andenberg-bezanilla-na-states)
(setq dvb94-na-states  '(C1   C2   C3   C4   C5   I4  I5  I   O)) ; nomenclature of VB paper
;;                        C   C1   C2   C3   C4   I3  I4  I   O  - nomenclature in Destexhe et al (from eq.17) 

#|

a's in 1/second, b's in mV
      r8 * r9
r10 = -------
        r7


|#



;; Rates in comments refer to Destexhe paper, but state symbols in lists are from VB91
(defvar Destexhe-STATE-VOLTAGE-TRANSITION-FUNCTIONS
  `((c1 c2 ,(markov-exponential-rate  33350 ; a5
	   74.58			; b5
	   ))				; C -> C1  r5
	    
    (c2 c1 ,(markov-exponential-rate  1940 ; a6
	   -21.03			; b6
	   ))				; C1 -> C  r6
	    
    (c2 c3 ,(markov-exponential-rate  33350 ; a5
	   74.58			; b5
	   ))				; C1 -> C2 r5
	    
    (c3 c2 ,(markov-exponential-rate  1940 ; a6
	   -21.03			; b6
	   ))				; C2 -> C1 r6
	    
    (c3 c4 ,(markov-exponential-rate  33350 ; a5
	   74.58			; b5
	   ))				; C2 -> C3 r5
	    
    (c4 c3 ,(markov-exponential-rate  1940 ; a6
	   -21.03			; b6
	   ))				; C3 -> C2 r6
	    
    (c4 c5 ,(markov-exponential-rate 11490 ; a1
	   59.19			; b1
	   ))				; C3 -> C4 r1



    ;; (/ (* 1538 7.992) 863.1) = 14.241336
    ;; (+ 27050 -27.07 (- 27050)) = -27.070312
    (c4 i4 ,(markov-exponential-quotient-rate 1538 ; a8
	   27.050			; b8 -> 27050 in text
	   7.992			; a9
	   -27.07			; b9
	   863.1			; a7
	   27.050			; b7 -> 27050 in text
	   ))				; C3 -> I3 r10

    (c5 c4 ,(markov-exponential-rate  8641 ; a2
	   -58.60			; b2 -> -58.60 in text 
	   ))				; C4 -> C3 r2
	    
    (c5 o ,(markov-exponential-rate  31310 ; a3
	   17.18			; b3
	   ))				; C4 -> O  r3
	    
    (i4 c4 ,(markov-exponential-rate  1538 ; a8
	   27.050			; b8 -> 27050 in text
	   ))				; I3 -> C3 r8 

    (i4 i5 ,(markov-exponential-rate  2719 ; a4
	   -51.54			; b4
	   ))				; I3 -> I4 r4
	    
    (i5 i4 ,(markov-exponential-rate  31310 ; a3
	   17.18			; b3
	   ))				; I4 -> I3 r3
	    
    (i5 i ,(markov-exponential-rate  8641 ; a2
	   -58.60			; b2 -> -5860 in text
	   ))				; I4 -> I  r2

    (i i5 ,(markov-exponential-rate 11490 ; a1
	   59.19			; b1
	   ))				; I -> I4  r1

    (i o ,(markov-exponential-rate  863.1 ; a7
	   27.050			; b7  -> 27050 in text
	   ))				; I -> O   r7
	    
    (o c5 ,(markov-exponential-rate  2719 ; a4
	   -51.54			; b4
	   ))				; O -> C4  r4
	    
    (o i ,(markov-exponential-rate  7.992 ; a9
	   -27.07			; b9 
	   ))				; O -> I   r9
    ))

(defvar dvb94-na-open-states)
(setq dvb94-na-open-states  '(O))

(particle-type-quoted-def
 `(nax-dvb94
   (class . :markov)
   (states . ,dvb94-na-states)
   (open-states . ,dvb94-na-open-states)
   (STATE-TRANSITIONS . ,Destexhe-STATE-VOLTAGE-TRANSITION-FUNCTIONS)))

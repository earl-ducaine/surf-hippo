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

;;; Markovian model of Na channel as described in
;@ARTICLE{Pat-91,
;        AUTHOR = {Patlak, J.},
;        TITLE = {Molecular kinetics of voltage-dependent {$Na^+$} channels},
;        JOURNAL = {Physiological Reviews},
;        YEAR = {1991},
;        VOLUME = {71},
;        NUMBER = {4},
;        MONTH = {October}
;}


;; rate constant    W     delta    z      d  compliment-d-delta compliment-d-voltage
(defvar patlak-rate-parameters)
(setq patlak-rate-parameters
  '((alpha-m  .      (-19.0    nil     2.45   0.6))
    (beta-m    .   (-22.35   nil     2.45   0.6 nil t))
    (delta     .     (-19.00  -1.8     2.45   0.6))
    (gamma     .     (-23.90  -1.8     2.45   0.6 t t))
    (kappa     .     (-21.32  -1.8     2.45   0.6 t nil))
    (lambda    .     (-27.08  -1.8     2.45   0.6 nil t))
    (alpha-h   .     (-26.5    nil     0.3    0.5 nil t))
    (beta-h    .     (-22.4    nil     0.3    0.5))
    (K-ci      .     (-21.0    nil     2.45   0.55))
    (K-ic      .     (-28.05    nil     2.45   0.55 nil t))))



(defun patlak-w (param) (nth 0 (get-a-value param patlak-rate-parameters)))
(defun patlak-delta (param) (nth 1 (get-a-value param patlak-rate-parameters)))
(defun patlak-z (param) (nth 2 (get-a-value param patlak-rate-parameters)))
(defun patlak-d (param) (nth 3 (get-a-value param patlak-rate-parameters)))
(defun patlak-compliment-d-delta (param) (nth 4 (get-a-value param patlak-rate-parameters)))
(defun patlak-compliment-d-voltage (param) (nth 5 (get-a-value param patlak-rate-parameters)))



(defmacro patlak-rate-function (rate coefficient)
  `#'(lambda (voltage)
       (declare (optimize (speed 3) (space 0)) 
		(single-float voltage))
       (* ,coefficient
	  (/ (* BOLTZMANNS-CONSTANT *temperature*)
	     Plancks-constant)
	  (EXP (+ (patlak-w ,rate)
		  (if (patlak-delta ,rate)
		      (* (if (patlak-compliment-d-delta ,rate)
			     (- 1 (patlak-d ,rate))
			     (patlak-d ,rate))
			 (patlak-delta ,rate))
		      0)
		  (/ (* (patlak-z ,rate)
			(if (patlak-compliment-d-voltage ,rate)
			    (- (patlak-d ,rate) 1)
			    (patlak-d ,rate))
			voltage 1.0e-3
			ELECTRONIC-CHARGE)
		     (* BOLTZMANNS-CONSTANT *temperature*)))))))		


		  
(channel-type-def
 '(na-patlak91
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nax-patlak91 1)))))


(defvar patlak-na-states)
(setq patlak-na-states       '(C1   C2   C3   C4   I1   I2  O))
(defvar patlak-na-open-states)
(setq patlak-na-open-states  '(O))

(particle-type-quoted-def
 `(nax-patlak91
   (class . :markov)
   (states . ,patlak-na-states)
   (open-states . ,patlak-na-open-states)
   (STATE-TRANSITIONS .
    ,(loop for from-to-rate-coeff in '((c1 c2 kappa 4)
				       (c2 c1 lambda 1)
				       (c2 c3 alpha-m 3)
				       (c3 c2 beta-m 2)
				       (c3 c4 alpha-m 2)
				       (c4 c3 beta-m 3)
				       (c4 o delta 1)
				       (o c4 gamma 4)
				       (c3 i1 k-ci 1)
				       (i1 c3 k-ic 1)
				       (c4 i1 beta-h 1)
				       (i1 c4 alpha-h 1)
				       (i1 i2 delta 1)
				       (i2 i1 gamma 4)
				       (o i2 beta-h 1)
				       (i2 o alpha-h 1))
      collect
      (let ((from (nth 0 from-to-rate-coeff))
	    (to (nth 1 from-to-rate-coeff))
	    (rate (nth 2 from-to-rate-coeff))
	    (coeff (nth 3 from-to-rate-coeff)))
	(list from to (patlak-rate-function rate coeff)))))))


      
(defun test-na-patlak91 ()
  (topload 'dead-hippo)(create-channel *soma* 'na-patlak91)
  (enable-element-plot (car (particles)))
  (set-element-parameter (car (particles)) 'plot-markov-states t))

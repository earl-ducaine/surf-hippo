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

;; From the article :
;;
;; A Model of dendritic ca++ accumulation in hippocampal pyramidal neurons based on fluorescence
;; imaging measurements
;;
;; Jaffe, D. B. and Ross, W. N. and Lisman, J. E. and Lasser-Ross, N. and Miyakawa, H. and Johnston, D.
;; Journal of Neurophysiology, 1994, 71-3
;;

(pump-type-def
 '(ca-jaffe-94
   (species . ca)
   (class . :mm)			; michaelis menton
   (v-max . 6.0e-14)			; millimole ms^-1 cm^-2
   (kd . 1.0e-3)			; millimolar
   ))



;; Zador, A. M., PhD thesis - Biophysics of computation in single hippocampal neurons,
;; Yale University, 1993

(pump-type-def
 '(ca-high-zador-phd-93
   (species . ca)
   (class . :mm-zador)			; michaelis menton, dependent on A/V ratio (in 1/um?)
   (kmax . 0.2)				; 1/ms
   (density . 5.0e-16)			; micromoles/um2
   (kd . 0.5e-3)			; millimolar
   ))

(pump-type-def
 '(ca-low-zador-phd-93
   (species . ca)
   (class . :mm-zador)			; michaelis menton, dependent on A/V ratio (in 1/um?)
   (kmax . 0.2)				; 1/ms
   (density . 1.0e-15)			; micromoles/um2, for distal 1/3 of spine neck, and spine head
					; (density . 5.0e-15) micromoles/um2, for rest of spine.
   (kd . 20.0e-3)			; millimolar
   ))





;; Yamada, W. M. and Koch, C. and Adams, P. R. 1989
;; Multiple Channels and Calcium Dynamics
;; in Methods in Neuronal Modeling

(pump-type-def
 `(ca-yamada-89
   (species . ca)
   (class . :FIRST-ORDER-TAU-V)		; tau depends on voltage of associated cell element
   (equilibrium-conc . 50.0)		; mM
   (tau-function . (lambda (voltage) (* 17.7 (exp (/ voltage 35.0))))))

 )


;;   Computer simulations of a morphologically reconstructed CA3 hippocampal neuron
;;   M.Migliore, EP. Cook, DB. Jaffe, DA. Turner, D. Johnston
;;   J. Neurophysiol, March 1995, vol. 73, no. 3, 1157-1168
;;
;; adapted from Hines?
;;
(pump-type-def
 '(migliore-95
   (species . ca)
   (class . :state-eq)
   (k1 . 1.0e10)			; um3/ms
   (k2 . 5.0e6)				; ms^-1
   (k3 . 1.0e10)			; ms^-1
   (k4 . 5.0e6)				; um3/ms
   (total-density . 0.2e3)		; millimoles/cm2
   )
 )


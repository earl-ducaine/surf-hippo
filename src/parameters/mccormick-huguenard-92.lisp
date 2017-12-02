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

#|
From -

@ARTICLE{Hug-Mcc-92,
	AUTHOR = {Huguenard, J. R. and McCormick, D. A.},
	TITLE = {Simulation of the currents involved rhythmic oscillations in thalamic relay neurons},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1992},
	VOLUME = {68},
	NUMBER = {4},
	NOTE = {October}
}

and

@ARTICLE{Mcc-Hug-92,
	AUTHOR = {McCormick, D. A. and Huguenard, J. R.},
	TITLE = {A model of the electrophysiological properties of thalamic relay neurons},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1992},
	VOLUME = {68},
	NUMBER = {4},
	NOTE = {October}
}
|#

;; *****************************
;; Fast Na current
;; *****************************

;; Derived from Hug-Ham-Pri-88.

(channel-type-def
 '(na-mcc-hug
   (gbar . 12.0)			; uS, "in accordance with the model of Na+ action potentials
					; by Beluzzi and Sacchi 1991."
   (e-rev . 45.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((nam-mcc-hug 3) (nah-mcc-hug 1)))))

(particle-type-def
 `(nam-mcc-hug
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-38 (- voltage -38.0)))
		(/ (* -0.091 v-38)
		   (1- (exp (/ v-38 -5.0)))))))
   (beta . (lambda (voltage)
	     (let ((v-38 (- voltage -38.0)))
	       (/ (* 0.062 v-38)
		  (1- (exp (/ v-38 5.0)))))))))
			      


(particle-type-def
 `(nah-mcc-hug
   (class . :hh)
   (alpha . (lambda (voltage)
	      (* 0.016 (exp (/ (- -55 voltage) 15)))))
   (beta . (lambda (voltage)
	     (/ 2.07 (1+ (exp (/ (- 17 voltage) 21.0))))))))



;; *****************************
;; Persistent Na current
;; *****************************

;; Taken from the slice cells of Fre-Sah-Buc-Gag-90, defined by the channel type 'NA-FSBG-SLICE.
;; McCormick and Huguenard, however, assume activation time constant the same as that for the fast
;; Na current, whereas the time constant for activation of 'NA-FSBG-SLICE is constant (1ms).




;; *****************************
;; Potassium C current
;; *****************************

;; Taken from Yam-Koc-Ada-89. Reference yamada-koch-adams-chs.lisp


(channel-type-def
 '(kc-mcc-hug
   (parent-type . kc-yka89)
   (gbar . 1.0)				;uS
   (e-rev . -105.0)
   (use-defined-e-rev . t)
   (conc-particles . ((kcm-mcc-hug 1)))))

(conc-particle-type-def
 `(kcm-mcc-hug
   (parent-type . kcm-yka89)
   (conc-int-type . ca-in-L-mcc-hug)
   (shell . 1)))



(conc-int-type-def
 `(ca-in-L-mcc-hug
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; Default

   (tau . 1)				; ms
   (juxtamembrane-shell-thickness . 0.1) ; microns
   (core-conc . 50.0e-6)		; mM
   ))



(conc-int-type-def
 `(ca-in-T-mcc-hug
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; Default

   (tau . 1)				; ms
	
   (juxtamembrane-shell-thickness . 0.1) ; microns
   (core-conc . 50.0e-6)		; mM
   ))




;; T current


    
(channel-type-def
 '(cat-mcc-hug
   (permeability . 1.0)
   (ion-permeabilities . ((cA 1.0)))
   (use-variable-e-rev . nil)
   (conc-int-type-params . ((ca-in-T-mcc-hug (1 1.0))))
	
   (v-particles . ((catm-mcc-hug 2) (cath-mcc-hug 1)))))

(particle-type-def
 `(catm-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (+ 0.612
	       (/ 1.0
		  (+ (exp (/ (+ voltage 132)
			     -16.7))
		     (exp (/ (+ voltage 16.8)
			     18.2)))))))
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -57)
			   -6.2))))))))
			      
			      


(particle-type-def
 `(cath-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (if (< voltage -80)
	      (exp (/ (+ voltage 467)
		      66.6))
	      (+ (exp (/ (+ voltage 22)
			 -10.5))
		 28))))
				 
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -81)
			   4.0))))))))
			      
			      


;; H current, derived from McCormick and Pape, 1990.


(channel-type-def
 '(h-mcc-hug
   (gbar . 0.02)			; 15 - 30 nS from McCormick and Pape 1990
   (e-rev . -43)
   (use-variable-e-rev . nil)
   (q-ten . 1.0)
   (reference-temp . 35.5)
   (v-particles . ((hm-mcc-hug 1)))))

(channel-type-def
 '(h-mcc-hug-ext
   (parent-type . h-mcc-hug)
   (v-particles . ((hm-mcc-hug-ext 1)))))

(particle-type-def
 `(hm-mcc-hug-ext
   (class . :hh-ext)
   (v-half . -75.0)
   (valence . -4.7)
   (base-rate . 5.1e-4)
   (gamma . 0.38)
   (tau-0 . 17.8)))

   

(particle-type-def
 `(hm-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (/ 1.0
	       (+ (exp (- -14.59 (* 0.086 voltage)))
		  (exp (+ -1.87 (* 0.0701 voltage)))))))

   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -75)
			   5.5))))))
   (q-ten . 1.0)
   (reference-temp . 35.5)))




;; A current - two components are described as the sum of I-A1 and I-A2. In the first paper
;; Hug-Mcc-92, the max gbars were specified as ranges:
;;    gmaxA1  11.2 - 50nS
;;    gmaxA2   7.5 - 33nS
;;
;; However, in the second paper Mcc-Hug-92, a *single* I-A is referenced with an apparent (from
;; Table 1) gmax of 1uS! 


(channel-type-def
 '(ka1-mcc-hug
   (gbar . 1.0)
   (e-rev . -105)
   (ion-permeabilities . ((k 1.0)))
   (use-variable-e-rev . nil)
   (v-particles . ((kam1-mcc-hug 4) (kah1-mcc-hug 1)))))

(particle-type-def
 `(kam1-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (+ 0.37
	       (/ 1.0
		  (+ (exp (/ (+ voltage 35.8)
			     19.7))
		     (exp (/ (+ voltage 79.7)
			     -12.7)))))))
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -60)
			   -8.5))))))))
			      
			      


(particle-type-def
 `(kah1-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (if (< voltage 63)
	      (/ 1.0
		 (+ (exp (/ (+ voltage 46)
			    5))
		    (exp (/ (+ voltage 238)
			    -37.5))))
	      19)))
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -78)
			   6))))))))




(channel-type-def
 '(ka2-mcc-hug
   (gbar . 1.0)
   (e-rev . -105)
   (ion-permeabilities . ((k 1.0)))
   (use-variable-e-rev . nil)
   (v-particles . ((kam2-mcc-hug 4) (kah2-mcc-hug 1)))))

(particle-type-def
 `(kam2-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (+ 0.37
	       (/ 1.0
		  (+ (exp (/ (+ voltage 35.8)
			     19.7))
		     (exp (/ (+ voltage 79.7)
			     -12.7)))))))
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -36)
			   -20))))))))
			      


(particle-type-def
 `(kah2-mcc-hug
   (class . :hh)
   (tau . (lambda (voltage)
	    (if (< voltage 73)
	      (/ 1.0
		 (+ (exp (/ (+ voltage 46)
			    5))
		    (exp (/ (+ voltage 238)
			    -37.5))))
	      60)))
   (ss . (lambda (voltage)
	   (/ 1.0
	      (+ 1 (exp (/ (- voltage -78)
			   6))))))))

			      



;; temp 23.5C
;; Ena 45mV
;; Ek -105mV
(defun mcc-hug-gp-relay ()
  (let ((soma (create-soma
	       :cell (create-cell "mcc-hug-relay"
				  :cell-type (create-cell-type
					      "mcc-hug-relay"
					      ;; Leak conductance consists of 15nS of K-leak and 6nS of
					      ;; Na-leak, with a cell area of 29000um2
					      :membrane-resistivity (* (/ 1.0 (+ 15e-9 6e-9)) 29000e-8)
					      :specific-capacitance 1.0))
	       :diameter (SPHERE-DIAMETER-FROM-AREA 29000))))
    (create-channels soma '(cat-mcc-hug))
    (soma-cell soma)))
    
			   

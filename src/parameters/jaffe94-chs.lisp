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

;; From the article: A Model For Dendritic Ca++ Accumulation in Hippocampal
;; Pyramidal Neurons Based on Fluorescence Imaging Measurements D. B. Jaffe,
;; W. N. Ross, J. E. Lisman, N. Lasser-Ross, H.Miyakawa, D. Johnston J.
;; Neurophysiol. 1994, vol. 71, no. 3, 1065 - 1077
;;
;; They assume a total (?) Ca++ channel density of 50-200pS/um2, derived from
;; an estimate of 5-20 channel per um2 and 10pS per channel. In their model a
;; T:N:L channel ratio of 1:10:10 was used (Fisher, Gray, and Johnston
;; Properties and distribution of single voltage-gated calcium channels in
;; adult hippocampal neurons. J. Neurophysiol. 64:91-104, 1990), with the
;; density of the N and L channels set to 25 pS/um2 and that of the T
;; channels set to 2.5 pS/um2. All densities were independent of their
;; location in the cell.

;; This paper used a driving force for the calcium channels derived with the
;; Golman-Hodgkin-Katz rectification (Hagiwara and Byerly 1981 Calcium
;; channel, Annu. Rev. Neurosci. 4:69-125, 1981).

;; For the calcium channels, the authors used a least-squares fitting of
;; referenced Boltzman equation parameters, adjusted to various powers of the
;; gating particles, to derive parameters for HH type rate equations. The
;; referenced data included constant time constants, which were also used in
;; the fitting to the HH parameters. The result is some bizzare time constant
;; functions.


(channel-type-def
 '(na-Jaffe94
   (parent-type . na-hh-ext)		; Defined in hodgkin-huxley.lisp
   (gbar-density . 1200.0)		; In fact, same as HH, although the
					; paper references Sah et 
					; al 88.
   ))

(channel-type-def
 '(kdr-Jaffe94
   (parent-type . dr-hh-ext)		; Defined in hodgkin-huxley.lisp
   (gbar-density . 24.0))		; should this be 240.0??
 )

#|
questions for jaffe -

- why is k called boltzmann's constant in Table 1

- should k be R in eq. 11 - YES

- Fisher et al suggest 20mV shift, not 10mV

- the form of the ghk equation assumes that conc-out is constant, and that
multiplying the form in Hagiwara and Byerly by the inverse of the conc-out
transforms the perm term to the gbar*m*h form. 

|#

(defun jaffe-conductance-function (ch)
  (let* ((conc-int (channel-top-conc-int ch))
	 (conc-in (conc-int-shell-1-free-conc-n conc-int))
	 (conc-out (conc-int-transmembrane-conc conc-int))
	 (valence (conc-int-valence conc-int))
	 (voltage (channel-voltage ch)))
    (jaffe-ghk-term voltage conc-in conc-out valence)))
    

;(defvar jaffe-ghk-ca-term
;  (list-to-array (loop for voltage from -100.0 to 50 by 0.1
;                       collect (jaffe-ghk-term voltage 2))))
  

(defun alpha-m (voltage a b)
  (/ (* a (- b voltage))
     (- (exp (/ (- b voltage) 10)) 1)))

(defun beta-m (voltage c d)
  (* c (exp (/ (- voltage) d))))
  
;; ee since e is reserved
(defun alpha-h (voltage ee f)
  (* ee (exp (/ (- voltage) f))))

(defun beta-h (voltage g)
  (/ 1 (+ (exp (/ (- g voltage) 10)) 1)))


; The calcium current gbars have to be adjusted given that their form of the
; constant field equation folds the z^2*F^2/RT term and an assumption for
; [Ca++] out into the g-bar.

(defvar *jaffe94-ca-conc-extra* 2.0)	; mM

(defvar *jaffe94-ghk-gbar-correction*
  (/ 
   (* 1.0e3				; V -> mV
      1.0e-6				; uS -> S
      gasconstant *temperature*)
   (* 1.0e-3 *jaffe94-ca-conc-extra*	; mM to M
      2 2				; z^2
      Faraday faraday )))

;; T-Type Calcium channel 
(channel-type-quoted-def
 `(ca-t-Jaffe
;;   (iv-relation . :CONSTANT-FIELD)
   (gbar-density . ,(* 2.5 *jaffe94-ghk-gbar-correction*))
   
   (ion-permeabilities . ((CA 1.0)))
   (QTEN . 1.0)				; not given
   (reference-temp . 22.0)		; qten not given in article

   (conc-int-type-params . ((ca-in-jaffe-94 (1 1))))
   
   (v-particles . ((ca-t-jaffe-m 2)(ca-t-jaffe-h 1)))))

(channel-type-def
 `(ca-t-Jaffe-ext
   (parent-type . ca-t-Jaffe)
   (v-particles . ((ca-t-jaffe-m-ext 2)(ca-t-jaffe-h-ext 1)))))


;; T-Type Calcium channel activation particle
(particle-type-def
 `(ca-t-jaffe-m
   (class . :hh)
   (alpha . (lambda (voltage) (alpha-m voltage .2 19.26)))
   (beta . (lambda (voltage) (beta-m voltage 0.009 22.03)))))



;; Fitting with (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1
;; collect (BOLTZMANN-EQUATION voltage -28 -7.2)) :volts *PARTICLE-VOLTS*
;; :exponent 2)
(particle-type-def
 `(ca-t-jaffe-m-ext
   (class . :hh-ext)
   (v-half . -36.0)
   (valence . 3.0)
   (tau-0 . 1.50)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)))

;; fit with * (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1
;; collect (BOLTZMANN-EQUATION voltage -38 -7.2)) :volts *PARTICLE-VOLTS*
;;  :exponent 1)
(particle-type-def
 `(ca-t-jaffe-m1-ext-shift
   (class . :hh-ext)
   (v-half . -38.000008)
   (valence . 3.5907013)))
   

;; T-Type Calcium channel inactivation particle
(particle-type-def
 `(ca-t-jaffe-h
   (class . :hh)
   (alpha . (lambda (voltage) (alpha-h voltage 1.0e-6 16.26)))
   (beta . (lambda (voltage) (beta-h voltage 29.79)))))



;; fitting with  (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1 collect
;;		 (BOLTZMANN-EQUATION voltage -68 5))
;;	          :volts *PARTICLE-VOLTS*
;;		  :exponent 1)

(particle-type-def
 `(ca-t-jaffe-h-ext
   (class . :hh-ext)
   (v-half . -68.0)
   (valence . -5.2)
   (tau-0 . 10.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)))

 
;; N-Type Calcium channel 
(channel-type-quoted-def
 `(ca-n-jaffe
   (gbar-density . ,(* 25.0 *jaffe94-ghk-gbar-correction*))
  
;;   (iv-relation . :CONSTANT-FIELD)
   (conc-int-type-params . ((ca-in-jaffe-94 (1 1))))
   (ion-permeabilities . ((CA 1.0)))
   (QTEN . 1.0)				; not given
   (reference-temp . 22.0)		; qten not given in article
   (v-particles . ((ca-n-jaffe-m 2)(ca-n-jaffe-h 1)))))

(channel-type-def
 `(ca-n-jaffe-ext
   (parent-type . ca-n-jaffe)
   (v-particles . ((ca-n-jaffe-m-ext 2)(ca-n-jaffe-h-ext 1)))))


;; N-Type Calcium channel activation particle
(particle-type-def
 `(ca-n-jaffe-m
   (class . :hh)
   (alpha . (lambda (voltage) (alpha-m voltage 0.19 19.88)))			
   (beta . (lambda (voltage) (beta-m voltage 0.046 20.73)))))


;; fitting with (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1 collect
;;		 (BOLTZMANN-EQUATION voltage -14 -6.3))
;;	          :volts *PARTICLE-VOLTS*
;;		  :exponent 2)
(particle-type-def
 `(ca-n-jaffe-m-ext
   (class . :hh-ext)
   (v-half . -21.002111)
   (valence . 3.449111)
   (tau-0 . 1.50)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)))

;; 8/12/96
(particle-type-def
  '(CA-N-JAFFE-M-EXT
        (class . :HH-EXT)
        (valence . 3.449111)
        (gamma . 0.0)
        (qten . 1.0)
        (base-rate . 1.0)
        (v-half . -21.002111)
        (tau-0 . 0.125)
        (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
        (reference-temp . 27.0)
        (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
                   ))



   
;; N-Type Calcium channel inactivation particle
(particle-type-def
 `(ca-n-jaffe-h
   (class . :hh)
   (alpha . (lambda (voltage) (alpha-h voltage 1.6e-4 48.4)))
   (beta . (lambda (voltage) (beta-h voltage 39.0)))))



;; fitting with  (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1 collect
;		 (BOLTZMANN-EQUATION voltage -40 12.5))
;	          :volts *PARTICLE-VOLTS*
;		  :exponent 1) 

(particle-type-def
 `(ca-n-jaffe-h-ext
   (class . :hh-ext)
   (v-half . -40.0)
   (valence . -2.0)
   (tau-0 . 75.0)
					;   (tau-0 . 50.0) in the paper but a
					;   little longer is better for ca
					;   spikes
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)))



;; L-Type Calcium channel 
(channel-type-quoted-def
 `(ca-l-jaffe
   (gbar-density . ,(* 25.0 *jaffe94-ghk-gbar-correction*))

;;   (iv-relation . :CONSTANT-FIELD)
   (conc-int-type-params . ((ca-in-jaffe-94 (1 1))))
   (ion-permeabilities . ((CA 1.0)))
   (QTEN . 1.0)				; not given
   (reference-temp . 22.0)		; qten not given in article
   (v-particles . ((ca-l-jaffe-m 2)))))

(channel-type-def
 `(ca-l-jaffe-ext
   (parent-type . ca-l-jaffe)
   (v-particles . ((ca-l-jaffe-m-ext 2)))))


;; L-Type Calcium channel activation particle
(particle-type-def
 `(ca-l-jaffe-m
   (class . :hh)
   (alpha . (lambda (voltage) (alpha-m voltage 15.69 81.5)))
   (beta . (lambda (voltage) (beta-m voltage 0.29 10.86)))))


;; fitting with (ss-fit (loop for voltage from -100.0 to (- 50 0.1) by 0.1 collect
;;		 (BOLTZMANN-EQUATION voltage 4 -4.7))
;;	          :volts *PARTICLE-VOLTS*
;;		  :exponent 2)
(particle-type-def
 `(ca-l-jaffe-m-ext
   (class . :hh-ext)
   (v-half . -1.2)
   (valence . 4.6)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)))


(conc-int-type-def
 `(ca-in-jaffe-94
   (class . :FIRST-ORDER)
   (species . ca)
   (valence . 2)
   (intra-p . t)			; This is the default

   (pump-type-params . ((ca-jaffe-94 1))) ; Pump type CA-JAFFE-94 is
					  ; associated with (the single) 
					; compartment 1.

   (K-buffer . 1.0e3)			; Instantaneous buffer, ratio of bound[X]/[X] in
					; a compartment (dimensionless).
   
   (juxtamembrane-shell-thickness . 0.0) ; A value that is <= 0 means that
					 ; the compartment volume = cell
					 ; element volume. 


   (transmembrane-concentration . 2.0)	; The calcium out used in the paper 
   
   (core-conc . 5.0e-5)))



(channel-type-quoted-def
 `(kc-jaffe94
   ;; 	gkbar=.01	(mho/cm2)	: Maximum Permeability
   ;;   cai = 1e-3	(mM)
   (gbar-density . ,(* 0.0144 1.0e4))	; pS/um2 = 1.0e-12 S / 1.0e-8 cm2 = 1.0e-4 S/cm2
   ;; (e-rev . -91.0)
   (e-rev . -77.5) ; Put it to the same as kdr - otherwise no rep firing with the given kco params
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)				; qten not given in article
   (conc-particles . ((kco-jaffe95 1)))
   ))

;; K,C channel activation particle - see description of kco-ml83 in
;; NEURON-channels.lisp. 

;; In SURF-HIPPO, we maintain the conventions and notations of the ML paper -
;; thus alpha is the backward rate constant, 
;; and beta is the forward rate constant.
(conc-particle-type-def
 `(kco-jaffe95
   (parent-type . kco-ml83)

   ;; mM - in the Migliore paper they call this k1, after Hines. This is a
   ;; scaling term for the Ca++ dep component of the forward rate constant,
   ;; beta. In the M-L paper, this value ranges from .18 to .37 mM. Note that
   ;; Hines uses 0.18mM.
   (k1-0 .  0.18e-3)			;mM

   ;; mM - in the Migliore paper they call this k2, after Hines. This is a
   ;; scaling term for the Ca++ dep component of the backward rate constant,
   ;; alpha. In the M-L paper, this value ranges from .011 to .014 mM! Note
   ;; that Hines uses 0.011mM.
   (k4-0 .  0.011e-3)

   (conc-int-type . ca-in-jaffe-94)
   (shell . 1))				; For the CA-IN-JAFFE-94 conc
					; integrator type defined above, 
					; shell 1 corresponds to the entire
					; cell element volume. 
 )





(defun jaffe-ghk-exponential-term (voltage valence)
  (exp (/
	;; (coulombs/mole) millivolts 1.0e-3
	(* valence Faraday voltage 1.0e-3)
	;; DegKelvin (volts coulombs) / (degreesK mole)
	(* *temperature* GasConstant))))



;; multiplied by conductance in uS -> nA
(defun jaffe-ghk-term (voltage conc-in conc-out valence)
  (let ((exponential-term (jaffe-ghk-exponential-term voltage valence)))
    (* 1.0e3

    
       (if (= 0 voltage)

	   (/ (- (/ conc-in conc-out) 1)
	      (/ (* valence Faraday)
		 (* *temperature* GasConstant)))

	   (* 1.0e-3 voltage
	      (/ (- 1 (* (/ conc-in conc-out) exponential-term))
		 (- 1 exponential-term)))))))

(defun testghk (&optional ghk)
  (let* ((ch (element "jaffe94-c12861-ca1-soma-CA-T-JAFFE"))
	 (g-bar  			; (channel-gbar ch)
	   (if ch (channel-conductance ch)	   1.0				; uS
	       ))
	 (valence 2.0)
	 (conc-out (if ch (get-channel-conc-out ch) 2.0d0))
	 (conc-in (if ch (get-channel-conc-in ch)  50.0d-6))
	 (volts (loop for volt from -100.0d0 to 100 collect volt)))	 
    (plot-timed-data
     (loop for volt in volts collect
	   (if ghk
	       (*  g-bar *jaffe94-ghk-gbar-correction*
		   (constant-field-equation-double volt conc-in conc-out
						   1.0d0 valence 1.0d0)) 
	       (* g-bar (jaffe-ghk-term volt conc-in conc-out valence)))
	   )
     nil volts)))
  




;; COMMENTS


;; KC-JAFFE94 - This channel is problematic, since with the single
;; compartment ca integration, the profile of {CA} is slow - thus at any
;; given point in the rep firing, the ca-dep component of the kco is more or
;; less fixed, and the activation is mainly just v-dep.  kc fast ca activated
;; channel, adapted from Moczydlowski and Latorre 1983, via Hines. With the
;; rate constants given in the paper, the channel is tonically activated
;; during the first few spikes enough to kill subsequent rep firing.

;; KDR-JAFFE94 - probably the gbar figure in the paper is too small by a
;; factor of 10.

;; on the other hand - if we assume that their na/k only simulations included
;; kc as well as kdr, then the parameters given work fine for both channel
;; types, it is only that kdr doesnt do much - there is no qualitative change
;; after blocking kdrt when ca channels are present


;; compared to control (all channels) 0.5nA 500ms pulse
;;
;; block kdr
;; ------------------
;; spike freq ~unchanged
;; conc profile ~unchanged
;;
;;
;; block kdr and ca channels
;; ------------------


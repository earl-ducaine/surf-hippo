;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: biophysics-declare.lisp

(in-package "SURF-HIPPO")

;; **************************************** ****************************************
;;
;;         Various declarations for reality based variables.
;;
;; **************************************** ****************************************


;; ****************************************
;;
;; Numerical / Biophysical Constants
;;
;; ****************************************
;;; 7/5/92 - CMU CL is uptight about number types, especially with respect to structure slots. We are
;;; keeping everything in single precision, so we will define a single precision pi internal to the
;;; SURF package.

(defconstant Faraday 9.648e4 "Faraday's constant - Coulombs/mole")

;; Joules/(DegreesKelvin*mole) = (Volts*Coulombs)/(DegreesKelvin*mole)
;; Boltzmanns-constant	k = 1.380622e-23 Joule per Kelvin = 1.380622e-23 (Volts*Coulombs)/ Kelvin

;; Faraday / R = 9.648e4 Coulombs/mole / 8.314 (Volts*Coulombs)/(DegreesKelvin*mole)
;;             = 11604.522 degreesK/volts

;; F/k = 9.648e4 Coulombs/mole /  1.380622e-23 (Volts*Coulombs)/ DegreesKelvin
;;     = 6.9881546e+27 DegreesK / (mole * Volts)

;; FV/RT => dimensionless

(defconstant Boltzmanns-constant 1.380622e-23 "Joules/degree Kelvin")
(defconstant electronic-charge	1.6021917e-19) ; coulomb

;; Gas constant R in (Volts*Coulombs)/(DegreesKelvin*mole) = k * 6.022169e23/mole
;;                     = 1.380622e-23 (Volts*Coulombs)/ Kelvin * 6.022169e23/mole
;;                     = 8.31434 (volts coulombs) / (degreesK mole)

(defconstant GasConstant 8.31434 "Gas Constant") 
(defconstant FoverR (/ Faraday GasConstant) "Faraday / GasConstant.")
(defvar *f/rt* (/ foverr *temperature*))
(defvar nernst-eqn-const*1000*temp (/ (* 1000.0 *Temperature*) FoverR))
(defconstant Eca-Nernst-eqn-const (/ 0.5 foverR))

(defvar Eca-Nernst-eqn-const*1000*temp
  (* 1000.0				;convert to millivolt units
     Eca-Nernst-eqn-const
     *Temperature*))

(defconstant Plancks-constant 6.626196e-34 "Joules second")

;; Diffusion constant D of ions in aqueous solutions (Taken from Hille, B., Ionic Channels of Excitable Membranes, 1984, Sinaur).
;; Units are cm2/sec, Temperature = 25 C
(defvar *D_NA* 1.33e-5 "Diffusion constant of Na+ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_LI* 1.03e-5 "Diffusion constant of Li+ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_K* 1.96e-5 "Diffusion constant of K+ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_CS* 2.06e-5 "Diffusion constant of Cs+ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_CL* 2.03e-5 "Diffusion constant of Cl- in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_BR* 2.08e-5 "Diffusion constant of Br+ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_TEA* 0.87e-5 "Diffusion constant of TEA in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_MG* 0.71e-5 "Diffusion constant of Mg++ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")
(defvar *D_CA* 0.79e-5 "Diffusion constant of Ca++ in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable Membranes, 1984).")

(defconstant +k_CL_MW+ 74.53 "Molecular weight of KCl")
(defconstant +hepes_MW+ 238.3 "Molecular weight of HEPES")
(defconstant +mg_CL2_MW+ 203.30 "Molecular weight of MgCl2")
(defconstant +egta_MW+ 380.4 "Molecular weight of EGTA")
(defconstant +K_glu_MW+ 234.2 "Molecular weight of K gluconate")
(defconstant +atp_MW+ 583.4 "Molecular weight of ATP")
(defconstant +gtp_MW+ 523.2 "Molecular weight of GTP")

;; 1000 mM * g/M * v/1000

(defun print-solution-recipe (&key (volume 1) ; cm3
			      title
			      kcl hepes mgcl2 egta kglu atp gtp)
  (let ((osmolarity 0))			; mosm
    (flet ((grams/volume (volume mw conc) (* (/ volume 1000) ; liters
					     (/ conc 1000) ; M
					     mw)) ;g/M
	   (print-border () (format t "--------------------------------------------~%")))
      (cond-every
       (title (print-border) (format t "   ~A~%" title))
       (t (print-border))
       (kglu  (format t "Kglu  [~,fmM]:~20,T ~,2e g/~,2f cc~%" kglu (grams/volume volume +K_glu_mw+ kglu) volume)
	      (print-border)
	      (incf osmolarity (* 2 kglu)))
       (kcl   (format t "KCl   [~,fmM]:~20,T ~,2e g/~,2f cc~%" kcl (grams/volume volume +K_cl_mw+ kcl) volume)
	      (print-border)
	      (incf osmolarity (* 2 kcl)))
       (mgcl2 (format t "MgCl2 [~,fmM]:~20,T ~,2e g/~,2f cc~%" mgcl2 (grams/volume volume +mg_cl2_mw+ mgcl2) volume)
	      (print-border)
	      (incf osmolarity (* 3 mgcl2)))
       (hepes (format t "HEPES [~,fmM]:~20,T ~,2e g/~,2f cc~%" hepes (grams/volume volume +hepes_mw+ hepes) volume)
	      (print-border)
	      (incf osmolarity (* 2 hepes)))
       (egta  (format t "EGTA  [~,fmM]:~20,T ~,2e g/~,2f cc~%" egta (grams/volume volume +egta_mw+ egta) volume)
	      (print-border)
	      (incf osmolarity (* 2 egta)))
       (atp   (format t "ATP   [~,fmM]:~20,T ~,2e g/~,2f cc~%" atp (grams/volume volume +atp_mw+ atp) volume)
	      (print-border)
	      (incf osmolarity (* 2 atp)))
       (gtp   (format t "GTP   [~,fmM]:~20,T ~,2e g/~,2f cc~%" gtp (grams/volume volume +gtp_mw+ gtp) volume)
	      (print-border)
	      (incf osmolarity (* 2 gtp)))
       (t (format t "Osmolarity:~20,T ~,2f mOsm~%" osmolarity)
	  (print-border)

	  )))))

;; (PRINT-SOLUTION-RECIPE :kcl 4 :egta 0.5 :hepes 10 :kglu 140 :mgcl2 4 :volume 20 :atp 4 :title "Neuron 2002, Monier etal" :gtp 0.4)


;; ****************************************
;;
;; Reversal Potentials - Each cell type has a baseline value for the reversal potential and concentrations of K+, Na+, Ca++, Cl-,
;; and ??. Each cell type also has a baseline value for the resting potential. The default value for these potentials are obtained
;; from the global variables below.
;;
;; ****************************************

(defvar *v-holding* -70.0)		; mV
(defvar *v-leak* -70.0 "Default membrane leak reversal potential, mV.")
(defvar *v-leak-dendrite* -70.0 "Default dendritic membrane leak reversal potential, mV.")
					

;; ****************************************
;; Concentrations: All concentration related calculations are made assuming unity activity coefficients.

;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.
;; Note that Vandenberg and Bezanilla 1991 used a value of on the order of 500mM Na+ in some of their experiments with squid axon.
(defvar *na-conc-extra* 145.0 "mM")
(defvar *na-conc-intra* 12.0 "mM")
(defvar *e-na* 64.0 "mV")		; mvolts - at 300 deg (NERNST-POTENTIAL 12.0 145.0 1.0) = 64.4mV

(defvar *fix-e-na* t "Fix the global *E-NA* - otherwise, *E-NA* is updated as with the Nernst equation using *TEMPERATURE* and the
global *NA-CONC-INTRA* and *NA-CONC-EXTRA*.") 

;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.  
(defvar *k-conc-extra* 4.0 "mM")
(defvar *k-conc-intra* 155.0 "mM")
(defvar *e-k*  -95.0 "mV")			; mvolts - at 300 deg (NERNST-POTENTIAL 155.0 4.0 1.0) = 94.5mV

(defvar *fix-e-k* t "Fix the global *E-K* - otherwise, *E-K* is updated as with the Nernst equation using *TEMPERATURE* and the
global *K-CONC-INTRA* and *K-CONC-EXTRA*.") 

;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.
(defvar *e-cl*  -90.0 "mV")			; mvolts - at 300 deg (NERNST-POTENTIAL 4.0 120.0 -1.0) = 87.9mV
(defvar *cl-conc-extra* 120.0 "mM")
(defvar *cl-conc-intra* 4.0 "mM")

(defvar *fix-e-cl* t "Fix the global *E-Cl* - otherwise, *E-Cl* is updated as with the Nernst equation using *TEMPERATURE* and the
global *Cl-CONC-INTRA* and *Cl-CONC-EXTRA*.") 

(defvar *e-ca* 110.0 "mV")
(defvar *ca-conc-extra* 1.8 "mM")	; Extra-cellular Ca++ concentration [mM]

;; Hille says 1.5 mM Ca out, <10e-7 mM in.  Segal and Barker, 1986 use 4.0 mM Ca out Madison and Nicoll, 1982 use 2.5 mM Ca out
;; Blaxter et al, 1986 use ACSF with 3.25 mM Ca Wong and Prince, 1981 use 2.0 mM Ca. 
;; Note that Vandenberg and Bezanilla 1991 used a value of 10mM Ca++ in some of their experiments with squid axon.

(defvar *ca-conc-intra* 5.0e-5 "mM")	;Resting intra-cellular Ca++ concentration [mM]

(defvar *fix-e-ca* t "Fix the global *E-Ca* - otherwise, *E-Ca* is updated as with the Nernst equation using *TEMPERATURE* and the
global *Ca-CONC-INTRA* and *Ca-CONC-EXTRA*.") 

(defvar *mg-conc-extra* 1.5 "mM")	; Typical used for cortical slice Ringer's solution
;; Note that Vandenberg and Bezanilla 1991 used a value of 50mM Mg++ in some of their experiments with squid axon.
(defvar *mg-conc-intra* 1.5 "mM")		; Typical used for cortical slice patch solution

(defvar *e-mg* 0.0 "mV")

;; ****************************************
;;
;; Passive Components - used as defaults in CREATE-CELL-TYPE
;;
;; ****************************************

(defvar *rm* 40000.0 "Default value of dendrite membrane resistivity (ohms cm^2)")
(defvar *ri* 200.0 "Default value of segment axial resistivity (ohms cm)")
(defvar *rm-soma* 40000.0 "Default value of soma membrane resistivity (ohms cm^2)")
(defvar *cm* 0.7 "Default value of membrane capacitance (uF/cm^2)")
(defvar *cm-dendrite* 0.7 "Default value of membrane capacitance (uF/cm^2)")
(defvar *r-extracellular* 200.0 "Default value of extracellular resistivity (ohms cm)")

(defvar *integral-base* *v-holding*)	; The reference  for INTEGRATE-PLOT-DATA

(proclaim '(single-float Faraday GasConstant FoverR *F/RT*
	    *r-extracellular* *rm* *ra* *r-i* *rm-soma* *cm*
	    *e-k* *e-ca* *e-k* *v-holding* *e-l* *ca-conc-extra*
	    Eca-Nernst-eqn-const*1000*temp nernst-eqn-const*1000*temp))





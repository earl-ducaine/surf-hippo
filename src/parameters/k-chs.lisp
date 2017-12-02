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

;
; Various K channels.
;


(in-package "SURF-HIPPO")


#|
@ARTICLE{Sah-Gib-Gag-88b,
	Author  = {Sah, P. and Gibb, A. J. and French, C.},
	TITLE = {Potassium Current Activated by Depolarization of Dissociated Neurons from Adult Guinea Pig Hippocampus},
	JOURNAL = {Journal of General Physiology},
	YEAR = {1988},
	VOLUME = {92},
	PAGES = {263-278}
}
|#




(channel-type-def
 '(KDR-sah
   (gbar-density . 100.0)		; 7.7 +/-2.0 mS/cm2 = 77 pS/um2
   (e-rev . -70.0)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 24.0)
   (v-particles . ((KDRX-Sah 1)(KDRY-Sah 1)))))

(particle-type-quoted-def
 `(KDRX-sah
   (class . :HH-EXT)
   (valence .
    ,(valence-from-K 14.0 (+ 23 273.0))) ; k = 13.6 +/- 1.3mV
   (gamma . 0.8)
   (qten . 3.0)
   (base-rate . 0.005)
   (v-half . -5.0)			; -4.8 +/- 3.9, n=9
   (tau-0 . 1.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 23.0)
   (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 23.0)))))


(particle-type-def
 `(KDRY-sah
   (class . :HH-EXT)
   ;; k = 26.6 +/- 5.6mV, n=4
   ;; (valence-from-K -27.0 (+ 23 273.0))
   ;; -0.9447532
   (valence . -1.0) 
   (gamma . 0.0)
   (qten . 3.0)
   (base-rate . 0.0)
   (v-half . -68.0)			; -67.7 +/- 11.6
   (tau-0 . 450.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . t)
   (reference-temp . 23.0)
   (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 23.0)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
@ARTICLE{Lan-Nic-Per-91,
	AUTHOR = {Lancaster, B. and Nicoll, R. A. and Perkel, D. J.},
	TITLE = {Calcium activates two types of potassium channels in rat hippocampal neurons in culture},
	JOURNAL = {Journal of Neuroscience},
	YEAR = {1991},
	VOLUME = {11},
	NUMBER = {23}
}

patch, single channel recordings from cultured cells, E17

-------------------------------
19pS channel:

rise time at -70mV approx 70ms

0.5mM Mg++ blocks at +70mV but not at -70mV

no TEA block

they discuss the slow activation of iahp and discount

-------------------------------
220pS channel:

-70mV - no opening

TEA block

1uM Ca++ - no activity at -70mV - only at very depolarized levels

|#




#|
 @ARTICLE{Koh-Hir-Bon-Kin-Mar-May-Ade-96,
	AUTHOR = {K\"ohler, M. and Hirschberg, B. and Bond, C. T. and Kinzie, J. M. and Marrion, N. V. and Maylie, J. and Adelman, J. P.},
	TITLE = {Small-conductance, calcium-activated potassium channels from mammalian brain},
	JOURNAL = {Science},
	YEAR = {1996},
	VOLUME = {273},
	PAGES = {1709-1714}
}
|#

;; Instantaneous response to voltage changes, no inactivation.

;; The hSK1 channel
;; Single channel conductance 9-10pS
(channel-type-def
 '(KAHP-kohler-96
   (gbar . 0.10)			; arbitrary
   (ion-permeabilities . ((K 1.0)))	; 55.4mV change in e-rev with [K]-extra change from 2->20mM
   (q10 . 1.0)				; arbitrary
   (conc-particles . ((KAHPO-kohler-96 1)))))

    
;; Derived from Fig.3I 
;; assume tau of 100ms at k-1/2 = 0.71uM
(conc-particle-type-def
 '(KAHPO-kohler-96
   (class . :NTH-ORDER)
   (alpha . 1.9700001e+10)
   (beta . 0.00473)
   (tau-0 . 0.0d0)
   (power . 4)				; They fit 3.9
   (shell . 2)				; arbitrary
   (conc-int-type . CA-IN-gen)		; arbitrary
   ))


;; ******************* ******************* ******************* *******************
;; I-dr
;; ******************* ******************* ******************* *******************
#|
Derived from the commentary of -

	AUTHOR = {Storm, J.F.},
	TITLE = {Potassium currents in hippocampal pyramidal cells},
	BOOKTITLE = {Progress in Brain Research},
	PUBLISHER = {Elsevier Science Publishers B. V. (Biomedical Division)},
	YEAR = {1990},
	EDITOR = {Storm-Mathisen, J. and Zimmer, J. and Ottersen, O.P.},
	volume = {83},
	PAGES = {161--187}}
|#

;; preliminary
(channel-type-def
 '(KDR-sto
   (gbar-density . 500.0)
   (e-rev . -70.0)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 24.0)
   (v-particles . ((KDRX-Sto 2)(KDRY-Sto 1)))))

;; preliminary
(particle-type-def '(KDRX-sto
        (class . :HH-EXT)
        (valence . 5.0)
        (gamma . 0.8)
        (qten . 3.0)
        (base-rate . 0.005)
        (v-half . -20.0)
        (tau-0 . 10.0)
        (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
        (reference-temp . 24.0)
        (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 24.0)))))


;; preliminary
(particle-type-def '(KDRY-sto
        (class . :HH-EXT)
        (valence . -4.0)
        (gamma . 0.0)
        (qten . 3.0)
        (base-rate . 0.0)
        (v-half . -30.0)
        (tau-0 . 2000.0)
        (IGNORE-TAU-VOLTAGE-DEPENDENCE . t)
        (reference-temp . 24.0)
        (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 24.0)))))


;; ******************* ******************* ******************* *******************
;; I-a from Storm `88
#|
@ARTICLE{Sto-88,
	AUTHOR = {Storm, J. F.},
	TITLE = {Temporal integration by a slowly inactivating {$K^+$} current in hippocampal neurons},
	JOURNAL = {Nature},
	YEAR = {1988b},
	VOLUME = {336},
	MONTH = {November}
}
|#
;; ******************* ******************* ******************* *******************
(channel-type-def
 '(ka-sto88
   (gbar . 70.0e-3)			;uS
   (e-rev . -90.0)
   (ion-permeabilities . ((K 1.0)))
   (QTEN . 1.0)
   (reference-temp . 35.0)
   (v-particles . ((kax-sto88 1)(kay-sto88 3)))))


;; traced from scans of fig2 in paper
(setq kay-sto88-volts-ss  `((-85.27273 -80.36363 -75.63637 -70.72727 -66.0 -60.909092 -55.818184
			     -51.454544 -46.36364 -41.09091)
			    (0.978022 0.9194139 0.8681319 0.7948718 0.6959708 0.52747256 0.3516484
			     0.19780225 0.080586106 0.0036630332) ))

(setq kax-sto88-volts-ss `((-65.454544 -60.545456 -56.0 -50.727272 -45.636368 -40.909096 -36.181824)
			   (-0.007325977 -0.0036629736 0.040293068 0.15750921
			    0.3516484 0.5641026 0.8424909)))

#|
(ss-fit (cadr kay-sto88-volts-ss) :volts (car kay-sto88-volts-ss) :EXPONENT 1)
 Converged but LAMBDA (FLA) large
	(class . :hh-ext) (v-half . -60.72741) (valence . -3.7771132)

(ss-fit (cadr kay-sto88-volts-ss) :volts (car kay-sto88-volts-ss) :EXPONENT 2)
 Converged but LAMBDA (FLA) large
	(class . :hh-ext) (v-half . -53.16147) (valence . -3.1874692)

(ss-fit (cadr kay-sto88-volts-ss) :volts (car kay-sto88-volts-ss) :EXPONENT 3)
 No Function Improvement Possible
	(class . :hh-ext) (v-half . -48.53147) (valence . -2.998342)

(ss-fit (cadr kax-sto88-volts-ss) :volts (car kax-sto88-volts-ss) :EXPONENT 1)
 No Function Improvement Possible
	(class . :hh-ext) (v-half . -42.693783) (valence . 5.849196)

(particle-type-def '(KAX-STO88
        (class . :HH-EXT)
        (valence . 5.8)
        (gamma . 0.0)
        (qten . 1.0)
        (base-rate . 1.0)
        (v-half . -42.7)
        (tau-0 . 1.0)
	(IGNORE-Tau-VOLTAGE-DEPENDENCE . t)
        (reference-temp . 35.0)
        (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 35.0) (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))
                   ))

;; This is the fit for 3 particles
(particle-type-def '(KAY-STO88
        (class . :HH-EXT)
        (valence . -3.0)
        (gamma . 0.5)
        (qten . 1.0)
        (base-rate . 0.1)
        (v-half . -48.5)
        (tau-0 . 1.0)
        (reference-temp . 35.0)
        (parameters . ((FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 35.0) (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))
                   ))
	


;; 8/12

;; assume that the kax curve only hits 0.5 value in figure 2.c

(setq kax-sto88-volts-ss `((-65.454544 -60.545456 -56.0 -50.727272 -45.636368 -40.909096 -36.181824)
			   ,(loop for val in '(-0.007325977 -0.0036629736 0.040293068 0.15750921
			    0.3516484 0.5641026 0.8424909)
			     collect (/ val 2))))
* (ss-fit (cadr kax-sto88-volts-ss) :volts (car kax-sto88-volts-ss) :EXPONENT 2)
 Converged but LAMBDA (FLA) large
	(class . :hh-ext)
 (v-half . -42.027363)
 (valence . 2.8011818)
NIL

(ss-fit (cadr kax-sto88-volts-ss) :volts (car kax-sto88-volts-ss) :EXPONENT 1)
	(class . :hh-ext)
 (v-half . -34.33152)
 (valence . 3.8316479)
NIL
*


* (ss-fit (cadr kax-sto88-volts-ss) :volts (car kax-sto88-volts-ss) :EXPONENT 4)
 No Function Improvement Possible
	(class . :hh-ext)
 (v-half . -51.93499)
 (valence . 2.3420649)

|#

;; adjusted 8/8/96
(channel-type-def
 '(KA-STO88
   (gbar . 0.07)
   (iv-relation-modulation . 5.0)		; ?????
   (e-rev . -90.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 35.0)
   (v-particles . ((KAX-STO88 3)(KAY-STO88 3)))))

(particle-type-def
 '(KAX-STO88
   (class . :HH-EXT)
   (valence . 4.4)
   (gamma . 0.3)
   (qten . 1.0)
   (base-rate . 0.05)
   (v-half . -51.0)
   (tau-0 . 0.1)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)))

(particle-type-def
 '(KAY-STO88
   (class . :HH-EXT)
   (valence . -3.0)
   (gamma . 0.5)
   (qten . 1.0)
   (base-rate . 0.1)
   (v-half . -48.5)
   (tau-0 . 10.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)))




;; ******************* ******************* ******************* *******************
;; I-d from Sto-88
;; ******************* ******************* ******************* *******************
#|
(setq kdy-sto88-volts-ss
      `((-124.814804 -117.4074 -109.444435 -101.85184 -94.999985 -86.296295 -78.33333 -69.25925 -61.666656 -54.444443)
	(0.9828769 0.95890427 0.9006851 0.81849325 0.67808235 0.51027405 0.25342473 0.07876718 0.02397263 0.0068493485)))

(setq kdx-sto88-volts-ss 
      `((-95.09091 -89.27272 -82.0 -74.90909 -68.36363 -62.363632 -57.272736 -52.0 -47.09091 -41.636368)
	(-0.0031645298 -0.0031645298 -0.0031645298 2.9802322e-8 0.08227852 0.17405066 0.30063292
	 0.5443039 0.73417723 0.8892405)))

(ss-fit (cadr kdx-sto88-volts-ss) :volts (car kdx-sto88-volts-ss) :EXPONENT 1)
 No Function Improvement Possible
	(class . :hh-ext) (v-half . -52.9421) (valence . 4.552271)

(ss-fit (cadr kdy-sto88-volts-ss) :volts (car kdy-sto88-volts-ss) :EXPONENT 1)
 Converged but LAMBDA (FLA) large
	(class . :hh-ext) (v-half . -87.721634) (valence . -2.9193301)
|#

;; adjusted 8/8/96

(channel-type-def
 '(KD-STO88
   (gbar . 1.0)
   (e-rev . -90.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 35.0)
   (v-particles . ((KDX-STO88 4)(KDY-STO88 4)))))

(particle-type-def
 '(KDX-STO88
   (class . :HH-EXT)
   (valence . 3.0)
   (gamma . 0.0)
   (qten . 1.0)
   (base-rate . 1.0)
   (v-half . -63.0)
   (tau-0 . 25.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)))

(particle-type-def
 '(KDY-STO88
   (class . :HH-EXT)
   (valence . -2.5)
   (gamma . 0.0)
   (qten . 1.0)
   (base-rate . 5.999999e-4)
   (v-half . -76.0)
   (tau-0 . 0.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)))





;; ******************* ******************* ******************* *******************
;; i-t(slow) from 
;; ******************* ******************* ******************* *******************
#|
	AUTHOR = {Ficker, E. and Heinemann, U.},
	TITLE = {Slow and fast transient potassium currents in cultured rat hippocampal cells},
	JOURNAL = {Journal of physiology},
	YEAR = {1992},
	VOLUME = {445},
	PAGES = {431-445}
|#
;; reversal potential of slow current with control solution (5mM K) was -80mV



(channel-type-def
 '(KTSLOW-ficker-92
   (gbar-density . 500.0)
   (e-rev . -80.0)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 37.0)		; ?
   (v-particles . ((KTSLOWX-ficker-92 1)
		   (KTSLOWY-ficker-92 1)))))

(channel-type-def
 '(KTSLOW-ficker-92-act-power2
   (parent-type . KTSLOW-ficker-92) 
   (v-particles . ((KTSLOWX-ficker-92-power-2 2)
		   (KTSLOWY-ficker-92 1)))))

(channel-type-def
 '(KTSLOW-ficker-92-act-power3
   (parent-type . KTSLOW-ficker-92) 
   (v-particles . ((KTSLOWX-ficker-92-power-3 3)
		   (KTSLOWY-ficker-92 1)))))

(particle-type-def
 '(KTSLOWX-ficker-92
   (class . :hh-ext)
   ;; Fit from fig. 3, taking into account current to conductance conversion, with e-rev = -80
   (v-half . -4.5)			
   (valence . 2.3)

   (gamma . 0.50)			; preliminary
   (qten . 1.0)
   (base-rate . 0.001)			; preliminary
   (tau-0 . 10.0)			; preliminary
   (reference-temp . 37.0)		; ?
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 37.0)
   (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))


(particle-type-def
 '(KTSLOWX-ficker-92-power-2
   (class . :hh-ext)
   ;; Fit from fig. 3, taking into account current to conductance conversion, with e-rev = -80,
   ;; exponent 2
   (v-half . -17.0)
   (valence . 1.9)

   (gamma . 0.8)			; preliminary
   (qten . 1.0)
   (base-rate . 0.001)			; preliminary
   (tau-0 . 10.0)			; preliminary
   (reference-temp . 37.0)		; ?
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 37.0)
   (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))


(particle-type-def
 '(KTSLOWX-ficker-92-power-3
   (class . :hh-ext)
   ;; Fit from fig. 3, taking into account current to conductance conversion, with e-rev = -80,
   ;; exponent 3
   (v-half . -24.7)
   (valence . 1.8)

   (gamma . 0.80)			; preliminary
   (qten . 1.0)
   (base-rate . 0.01)			; preliminary
   (tau-0 . 0.0)			; preliminary
   (reference-temp . 37.0)		; ?
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 37.0)
   (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))



;; tau inactivation removal of 107ms at -110mV (fig. 4)
;; tau decay two components -  voltage  tau1    tau2
;;                              -80     .640    3.64   seconds
;;                              -70     .510    3.86
;;                              -60     .334    2.38


(particle-type-def
 `(KTSLOWy-ficker-92
   (class . :hh-ext)
   ;; Fit from fig. 3 directly
   (v-half . -59.3)
   (valence . -2.7)
   (IGNORE-Tau-VOLTAGE-DEPENDENCE . t)
   (gamma . 0.0)
   (qten . 1.0)
   (base-rate . 0.01)
   (tau-0 . 1000.0)
   (reference-temp . 37.0)		; ?
	
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 37.0)
   (USE-FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . T)))



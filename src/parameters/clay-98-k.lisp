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
Taken from a preprint version of

@ARTICLE{Cla-98,
	AUTHOR = {Clay, J. R.},
	TITLE = {Excitability of the squid giant axon revisited},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1998},
	VOLUME = {80},
	NUMBER = {2},
	PAGES = {903-913},
	MONTH = {August}
}
Note that the parameters in the published version are different.

This model is modified from the canonical Hodgkin-Huxley channel (see hodgkin-huxley.lisp).



The GHK term in the paper must be adjusted for the full GHK expression as used in SH.

Assumption of the paper:

Eq (1):  GHK =>   qV/kT *   ( exp[q(V-Ek)/kT]       - 1) / (exp[qV/kT]-1)


kT/q = 24.1mV (24) (T=8degC)

(note that but (/ (* gasconstant *temperature*)  faraday) =  0.024215685)
for *temperature* = 281.0


(loop for temp-c in '(0 5 10 15 20 25 30 35 37) do
      (format t "~,d deg C: F/RT = ~,2fmV~%" temp-c
 (* 1000 (/ (* gasconstant (+ 273.16 temp-c))  faraday))))
0 deg C: F/RT = 23.54mV
5 deg C: F/RT = 23.97mV
10 deg C: F/RT = 24.40mV
15 deg C: F/RT = 24.83mV
20 deg C: F/RT = 25.26mV
25 deg C: F/RT = 25.69mV
30 deg C: F/RT = 26.13mV
35 deg C: F/RT = 26.56mV
37 deg C: F/RT = 26.73mV

note that kT/q = RT/F


thus     GHK =>    V/24 * (exp([V-Ek]/24) - 1) / (exp[V/24]-1)
             or    V/24 * (exp(V/24) exp[q(-Ek)/kT] - 1) / (exp[V/24]-1)


Ek = RT/F ln Ks/Ki = kT/q ln Ks/Ki   (Ks = Kshell = Kout)

q(-Ek)/kT = - ln Ks/Ki = ln Ki/Ks

exp (-q Ek)/kT = exp (ln Ki/Ks) = Ki/Ks


so,     GHK =>     V/24 * (  [Ki/Ks * exp(V/24)]    - 1) / (exp[V/24]-1)


                   V/24 * ( Ki exp(V/24) - Ks)  / Ks (exp[V/24]-1)

                   V/24 * ( Ki - Ks exp(-V/24)) / Ks (1 - exp[-V/24])

note Ks term in denominator.



GHK current eq 10-5 Hille:

        I =  p z z V (F/24mV) * ( Ki - Ks exp(-V/24)) / (1 - exp[-V/24])

For:

        I =  a * V/24 * ( Ki - Ks exp(-V/24)) / (1 - exp[-V/24])


a = pzzF

p = a/zzF = 5e3 (uA/Mcm2) mole /  96480 coloumbs

= 

5e3  1e-6 coulombs    1e3 cm3                mole
-----------------------------------------------------
          sec         mole     cm2       96480.0 coloumbs



=    5 cm
    -------
    sec 96480


=    .05 100 cm
    -------
    sec 96480


;;  Faraday Faraday  millivolts temperature^-1 GasConstant^-1 milliMolar      Permeability
;; .....................................................................................
;;
;;  coulombs coulombs   mV       degreesK mole               10e-3  mole    cm cm cm
;;  --------  --------  --    ------------------            -----------     --------     =  nanoamps
;;    mole     mole          DegKelvin V coulomb           1000 cm cm cm     second



     a * n(V,t)^4 * V/24 * [Ki exp(V/24) - Ks] / [exp(V/24)-1]



Simulator units of permeability density:

(1.0e-6cm3/s/um2) =

1.0e-6 cm3 sec-1
---------------
1.0e-6m 1.0e-6m 

1.0e-6 cm3 sec-1
---------------
1.0e-6 100 cm 1.0e-6 100 cm

cm3 sec-1
---------------
1.0e-2 cm2

100 cm
------
sec


To change mA/M cm2 to 100 cm/sec, divide by (* 100 96480)
(/ 5.0 (* 100 96480)) = 5.182421e-7

|#




(channel-type-def
 '(k-clay98
   (permeability-density .  5.182421e-7)	; 100 cm/sec <= 5 mA / M cm2
   (conc-int-type-params . ((clay-98-k-ex (1 1))))
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((n-k-clay98 4)))))


;; alpha_n = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -10.0e3   1/(Volts*sec)	= -0.01	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.055    Volts		= -55	mV original HH ?
;; V0= -0.050    Volts		= -50	mV used in this paper

;; beta_n = A exp((v-V0)/B)
;; A = 120.0     1/sec		= 0.120	1/msec
;; B = -0.060    Volts		= -60	mV
;; V0= -0.025    Volts		= -25	mV

(defvar *n-k-clay98-alpha-V-shift* -50.0) ; For HH Vrest of -60. If Vrest is -65, the this should be -55.

(particle-type-def
 `(n-k-clay98
   (class . :hh)
   (alpha . (lambda (voltage)
	      (let ((v-shift (- voltage *n-k-clay98-alpha-V-shift*)))
		(/ (* -0.01 v-shift) (1- (exp (/ v-shift -10.0)))))))
   (beta . (lambda (voltage)
	     (* 0.12 (exp (/ (- voltage -60.0) -25.0)))))))


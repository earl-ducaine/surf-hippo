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

Need to check pump parameters LG July 31, 2002



Taken from a preprint version of:


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

This file requires channels defined in 

    surf-hippo/src/parameters/clay-98-na
    surf-hippo/src/parameters/clay-98-k



|#
;; Phi is the width of the periaxonal space
(defvar *clay-98-phi* 11.5) ; nm
(defvar *clay-98-FARADAY-phi* (/ 1.0  (* *clay-98-phi* faraday 1.0e-4)))

					; value for Fig. 4c
					; but phi = 13.8nm => (/ 1.0  (* 13.8 faraday 1.0e-4)) = 0.0075107557

					; (/ 1 0.0135) Fig.5c, 6c, 8,
					; but phi = 9.2nM => (/ 1.0  (* 9.2 faraday 1.0e-4)) = 0.011266134

					; (/ 1 0.0108) Fig 3
					; mA ms / M cm2 (?)

					; but phi = 11.5nm => (/ 1.0  (* 11.5 faraday 1.0e-4)) 0.009012907
#|

;; I / F phi =

;; coloumbs
;; ---------------------
;; sec cm2 96480 coloumbs mole-1 cm

;; mole
;; ----
;; sec cm3


* (/ 1 (* faraday 11.5 1.0e-4)) ; 11.5 1.0e-4 cm


0.009012907 mole coloumb-1 cm-1


11.5nM = 11.5 1.0e-9 m = 11.5 1.0e-9 1.0e2 cm = 11.5 1.0e-7 cm


coloumb sec-1 cm-2 mole coloumb-1 cm-1 = mole sec-1 cm-3 

= 0.001 M msec-1


coloumbs-1 mole cm-1 1.0e-3 couloumbs sec-1 cm-2

1.0e-3 mole cm-3 sec-1

1.0e-3 mole cm-3 1.0e3  sec-1 1.0e-3

|#

(defvar *clay-98-Kd* 0.002)		;Molar, dissociation constant for removal of excess K by glia

(defvar *clay-98-tau* 12.0)		;ms, time constant for clearance of excess K from the space

(defvar *clay-98-gamma* 5.0)		; 1/ms (?)

(setq *k-conc-extra* 10.0)		; mM, Figs 4c 5c 6c 7c 8
					; 0 Figs 3 7d

(setq *k-conc-intra* 300.0)		; mM

;; Generic conc-ints call a function for determining the dcdt with the arg CINT, returning mM/ms.
;; This function is optimized because it will be called at each time step for each cint.
(defun clay-98-dkdt-extra (cint)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((total-current (* 1.0e-6 (CONC-INT-MEMBRANE-CURRENT-COMPONENT cint))) ; mA
	 (total-current-per-unit-area (/ total-current (* 1.0e-8 (element-area cint)))) ; mA/cm2
	 (accumulation-term (/ total-current-per-unit-area (the sf *clay-98-FARADAY-phi*))) ; M/ms
	 (conc-shell-extra (conc-int-shell-1-free-conc-n cint)) ; mM
	 (difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 )
    (* 1000.0				; M to mM
       (+ accumulation-term		; M/ms
	  (- (/ difference-conc (the sf *clay-98-tau*))) ; M/ms
	  ))))

;; Implicit calculation of concentration at t_(n+1). Returns mM.
;; This function is optimized because it will be called at each time step for each cint.
(defun clay-98-k-n+1-extra (cint)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((delta-t (*delta-t[n]*))	;  The current time step [t_(n+1) - t_n], in msec.
	 (total-current (* 1.0e-6 (CONC-INT-MEMBRANE-CURRENT-COMPONENT cint))) ; mA
	 (total-current-per-unit-area (/ total-current (* 1.0e-8 (element-area cint)))) ; mA/cm2
	 (accumulation-term (/ total-current-per-unit-area (the sf *clay-98-FARADAY-phi*))) ; M/ms
	 (conc-shell-extra (conc-int-shell-1-free-conc-n cint)) ; mM
	 (difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 (f-of-cn (/ 1 (the sf *clay-98-tau*))))
    (/ (+ (* 1000 accumulation-term)	; mM/ms
	  (* conc-shell-extra
	     (- (/ 1 (the sf delta-t))
		(/ 1 (* 2 (the sf *clay-98-tau*)))))
	  (* (the sf *k-conc-extra*)
	     f-of-cn))
       (+ (/ 1 (the sf delta-t))
	  (/ 1 (* 2 (the sf *clay-98-tau*)))))))

;; Returns millimole/ms
;; This function is optimized because it will be called at each time step for each pump.
(defun clay-98-pump (pump conc-shell-extra)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (double-float conc-shell-extra))
  (let* ((difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 (result
	  (* 1000.0			; mM/M
	     (* (the sf *clay-98-gamma*) ; 1/ms
		(/ difference-conc	; M
		   (the df (expt (+ 1 (/ difference-conc (the sf *clay-98-Kd*))) 3)))))))
    (* 0.001				; L/mL
       (* 1.0e-3 *clay-98-phi* ; *clay-98-phi* is in nm
	  (element-area (pump-conc-int pump))
	  1.0e-12			; Convert um3 to cm3
	  )
       ; (pump-conc-int-compartment-volume pump)
       ; mL
       result)				; millimole / L ms
    ))
    
(pump-type-def
 '(k-clay-98
   (species . k)
   (class . :generic)
   (pump-function . clay-98-pump) ; For :GENERIC pumps, this function is called with args PUMP and concentration (mM), and returns pump current in millimole/ms.
   ))

(conc-int-type-quoted-def
 `(clay-98-k-ex
   (class . :generic)
   ;; For :GENERIC conc-ints, use either C-N+1-FUNCTION or explicit integration using DCDT-FUNCTION.
   (c-n+1-function . clay-98-k-n+1-extra)
   ;;   (dcdt-function . clay-98-dkdt-extra)
   (juxtamembrane-shell-thickness . ,(* 1.0e-3 *clay-98-phi*));  microns
   (core-conc . ,*k-conc-extra*)
   (PUMP-TYPE-PARAMS . ((k-clay-98 1)))
   (species . k)
   (valence . 1)
   (intra-p . nil)))

(defun clay-98-squid-membrane ()
  (setq *temp-celcius* 8.0)
  (create-soma :cell (create-cell "clay-98-squid-membrane"
				  :cell-type
				  (create-cell-type-w-params "clay-98-squid-membrane"
							     :rm
							     (/ 1
								(* 0.2 ; mS/cm2
								   0.001 ; S/mS
								   ))
							     :v-leak -59
							     :cm 1.0))
	       :diameter (sphere-diameter-from-area 1.0e4) ; total area is 0.0001cm2 (1e4um2)
	       )
  (create-element *soma* 'na-clay98 'k-clay98)
  (enable-element-plot 'soma)
  (enable-element-plot 'pump)
  (enable-element-plot 'k-clay98 '(current reversal-potential))
  (enable-element-plot 'conc-int)
  (setq *user-stop-time* 20)
  (add-pulse-list (add-isource *soma*) '(1 100 1)))


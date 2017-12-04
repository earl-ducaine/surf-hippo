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


;;; SYS Source file: biophysics.lisp
(in-package "SURF-HIPPO")

;; Equations specifically related to biophysical relationships. Such equations may appear in other files.

(defun nernst-potential (inside-conc outside-conc &optional (valence 1) (temperature-celcius *Temp-celcius*))
  "Returns potential (outside - inside) in mV. INSIDE-CONC and OUTSIDE-CONC are in mM. Default for optional VALENCE is 1, and for
TEMPERATURE-CELCIUS, in degrees centigrade, is the value of the global variable *TEMP-CELCIUS*."
  (declare (optimize (speed 3) (space 1)))
  (the sf (* (the sf (/ (/ (* 1000.0 (temperature-centigrade-to-kelvin temperature-celcius)) FoverR)
			valence))
	     (the sf (log (/ outside-conc inside-conc))))))

(defun ghk-potential (inside-activities outside-activities permeabilities &optional (temperature *temp-celcius*))
  "Returns potential (outside - inside) in mV.  For monovalent species, the list of activities in
INSIDE-ACTIVITIES and OUTSIDE-ACTIVITIES [mM] should be ordered according to specific species.  The
inside and outside activity of monovalent anions [e.g. Cl-] should be included in the
OUTSIDE-ACTIVITIES and INSIDE-ACTIVITIES, respectively. The relative PERMEABILITIES of the ions
should be in the same order as that for the other args. TEMPERATURE is in degrees celcius."
  (* (/ (* 1000.0 (temperature-centigrade-kelvin Temperature)) FoverR)
     (log (/
	   (apply '+ (mapcar '* outside-activities permeabilities))
	   (apply '+ (mapcar '* inside-activities permeabilities))))))

(defun ion-string (ion)
  (case ion
    ((or :na na) "Na+")
    ((or :ca ca) "Ca++")
    ((or :k k) "K+")
    ((or :cl cl) "Cl-")
    ((or :mg mg) "Mg+")))

(defun default-ion-reversal-potential (species &optional value)
  "Set the default (fixed) reversal potential for ion SPECIES ('NA, 'K, 'CL, 'CA, 'MG) if VALUE [mV]
supplied. Returns the current value."
  (if value
      (case species
	(na (setq *e-na* (s-flt value)))
	(k (setq *e-k* (s-flt value)))
	(cl (setq *e-cl* (s-flt value)))
	(ca (setq *e-ca* (s-flt value)))
	(mg (setq *e-mg* (s-flt value)))
	(t (sim-error (format nil "Species ~A not found..." species))))
      (case species
	(na *e-na*)
	(k *e-k*)
	(cl *e-cl*)
	(ca *e-ca*)
	(mg *e-mg*)
	(t 1.0))))

(proclaim '(inline default-extracellular-concentration))
(defun default-extracellular-concentration (species)
  (case species
    (na *na-conc-extra*)
    (k *k-conc-extra*)
    (cl *cl-conc-extra*)
    (ca *ca-conc-extra*)
    (mg *mg-conc-extra*)
    (t 1.0)))

(proclaim '(inline default-intracellular-concentration))
(defun default-intracellular-concentration (species)
  (case species
    (na *na-conc-intra*)
    (k *k-conc-intra*)
    (cl *cl-conc-intra*)
    (ca *ca-conc-intra*)
    (mg *mg-conc-intra*)
    (t 1.0)))

(proclaim '(inline ion-diffusion-coefficient))
(defun ion-diffusion-coefficient (species)
  (case species
    (na *D_NA*)
    (li *D_LI*)
    (k *D_K*)
    (cs *D_CS*)
    (cl *D_CL*)
    (br *D_BR*)
    (tea *D_TEA*)
    (mg *D_MG*)
    (ca *D_CA*)
    (t 0.0)))

(proclaim '(inline ion-valence))
(defun ion-valence (species)
  (case species
    (na 1.0)
    (li 1.0)
    (k 1.0)
    (cs 1.0)
    (cl -1.0)
    (br 1.0)
    (tea 1.0)
    (mg 2.0)
    (ca 2.0)
    (t 0.0)))

(proclaim '(inline effective-default-valence))
(defun effective-default-valence (elt)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for ion-perm in (ELEMENT-ION-PERMEABILITIES elt)
	sum (the sf (* (the sf (ion-valence (car ion-perm)))
		       (the sf (cadr ion-perm))))
	into result single-float
	finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline effective-default-intracellular-concentration-from-ion-perms))
(defun effective-default-intracellular-concentration-from-ion-perms (ion-perms)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for ion-perm in ion-perms
	sum (* (the sf (default-intracellular-concentration (car ion-perm))) (the sf (cadr ion-perm)))
	into result single-float
	finally (return result)))

(proclaim '(inline effective-default-intracellular-concentration))
(defun effective-default-intracellular-concentration (elt)
  (effective-default-intracellular-concentration-from-ion-perms (ELEMENT-ION-PERMEABILITIES elt)))

(proclaim '(inline effective-default-intracellular-concentration-double-from-ion-perms))
(defun effective-default-intracellular-concentration-double-from-ion-perms (ion-perms)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (coerce (effective-default-intracellular-concentration-from-ion-perms ion-perms) 'double-float))

(proclaim '(inline effective-default-intracellular-concentration-double))
(defun effective-default-intracellular-concentration-double (elt)
  (effective-default-intracellular-concentration-double-from-ion-perms (ELEMENT-ION-PERMEABILITIES elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline effective-default-extracellular-concentration-from-ion-perms))
(defun effective-default-extracellular-concentration-from-ion-perms (ion-perms )
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for ion-perm in ion-perms
	sum (* (the sf (default-extracellular-concentration (car ion-perm))) (the sf (cadr ion-perm))) into result single-float
	finally (return result)))

(proclaim '(inline effective-default-extracellular-concentration))
(defun effective-default-extracellular-concentration (elt)
  (effective-default-extracellular-concentration-from-ion-perms (ELEMENT-ION-PERMEABILITIES elt)))

(proclaim '(inline effective-default-extracellular-concentration-double-from-ion-perms))
(defun effective-default-extracellular-concentration-double-from-ion-perms (ion-perms )
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (coerce (effective-default-extracellular-concentration-from-ion-perms ion-perms) 'double-float))

(proclaim '(inline effective-default-extracellular-concentration-double))
(defun effective-default-extracellular-concentration-double (elt)
  (effective-default-extracellular-concentration-double-from-ion-perms (ELEMENT-ION-PERMEABILITIES elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;                             ;;;;;;;;;;;;;
;;;;;;;;;;;;; Boltzmann Related Functions ;;;;;;;;;;;;;
;;;;;;;;;;;;;                             ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valence-from-K (K &optional (temperature *temperature*))
  "Derive the valence of the gating particle given the slope of the Boltzmann fit (typically this is
referred to as \"K\" in the literature). TEMPERATURE is in degrees kelvin."
     (/ (/ temperature (*  1.0e-3 FoverR)) k))

(defun boltzmann-equation (voltage v-half k &optional (power 1))
  "Note that 1 divided by 4 times K is the slope of the Boltzmann expression at the midpoint."
  (let ((arg (/ 1.0 (+ 1.0 (exp-w-limits-generic (/ (- voltage v-half) k))))))
    (if (= power 1) arg (expt arg power))))

(defun boltzman-characteristic (k v-half)
  (loop for voltage from -100.0 to 50 collect (/ 1 (1+ (exp (/ (- v-half voltage) k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                             ;;;;;;;;;;;;
;;;;;;;;;;;;   Constant Field Equation   ;;;;;;;;;;;;
;;;;;;;;;;;;                             ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| from hagiwara and byerly, using constant field equation in equation 1.

with voltage in volts, a permeability term = 1.9466802e-2 gives figure 1 (in particular, -10nA at 29mV)

	     (let ((exponent-term (exp (/ (* 2 voltage faraday) (* *temperature* GasConstant)))))
	       (* 1.9466802e-2 (/ (* 4 faraday faraday voltage) (* *temperature* GasConstant))
		  (/ (- (* 1.0e-7 exponent-term) 10.0e-3)
		     (- exponent-term 1))))
|#

(defmacro constant-field-equation-exponential-term (voltage valence)
  "Voltage is in V. All arguments are single-floats. This form is that used by Mcc-Hug-92."
  `(exp (- (* (the sf ,valence) *F/rt* (the sf ,voltage)))))

(defmacro constant-field-equation-exponential-term-double (voltage valence)
  "Voltage (double-float) is in V. valence is single-float. This form is that used by Mcc-Hug-92."
  `(exp (- (* (the sf ,valence) *F/rt* (the df ,voltage)))))

(proclaim '(inline constant-field-equation))
(defun constant-field-equation (voltage conc-in conc-out permeability valence &optional (gating-term 1.0))
  "VOLTAGE is in mV, CONC-IN and CONC-OUT are in mM, PERMEABILITY in cm3/s, GATING-TERM and VALENCE are dimensionless. Returns
current in nA. All arguments and returned value are single-floats. This form is that used by Mcc-Hug-92."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type single-float valence voltage conc-in conc-out permeability gating-term))
  (let* ((voltage-in-volts (* 1.0e-3 voltage)) ; Convert mV to V
	 (exp-term (constant-field-equation-exponential-term voltage-in-volts valence))
	 (a-conc-numerator (* 1.0e3	; Convert microamps to nA
			      gating-term permeability valence valence Faraday *F/rt*
			      (- conc-in (* conc-out exp-term))))) ; mM
    (if (or (= 0.0 voltage)
	    (= 1.0 exp-term))		; See note in FAST-CONSTANT-FIELD-EQUATION
      (/ a-conc-numerator (* valence *F/RT*))
      (/ (* a-conc-numerator voltage-in-volts) (- 1 exp-term)))))

(proclaim '(inline constant-field-equation-double))
(defun constant-field-equation-double (voltage conc-in conc-out permeability valence gating-term)
  "Double-float version of CONSTANT-FIELD-EQUATION (except for VALENCE, which is a single-float).  Returns current in nA
(double-float)."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type double-float voltage conc-in conc-out permeability gating-term)
	   (type single-float valence))
  (let* ((voltage-in-volts (* 1.0e-3 voltage)) ; Convert mV to V
	 (exp-term (constant-field-equation-exponential-term-double voltage-in-volts valence))
	 (a-conc-numerator (* 1.0e3	; Convert microamps to nA
			      gating-term permeability valence valence Faraday *F/rt*
			      (- conc-in (* conc-out exp-term))))) ; mM
    (if (or (= 0.0 voltage)
	    (= 1.0 exp-term))		; See note in FAST-CONSTANT-FIELD-EQUATION
      (/ a-conc-numerator (* valence *F/RT*))
      (/ (* a-conc-numerator voltage-in-volts) (- 1 exp-term)))))

(defmacro fast-constant-field-equation (voltage conc-in conc-out zsqd-ff/rt zf/rt gating-term)
  ;; VOLTAGE in mV, CONC-IN and CONC-OUT in mM, GATING-TERM in cm3/sec. ZF/RT in 1/volts, ZSQD-FF/RT
  ;; in couloumbs mole-1 volt-1. Returns nA. Used in channel and synapse evaluations.
  `(let* ((voltage-in-volts (* ,voltage 1.0e-3))
	  (exp-term (exp (- (* ,zf/rt voltage-in-volts))))
	  (a-conc-numerator (* 1.0e3 ,gating-term ,zsqd-ff/rt (- ,conc-in (* ,conc-out exp-term)))))
     (if (or (= 0.0 voltage-in-volts)
	     (= 1.0 exp-term))		; This can show up even if volts /= 0 -> (KERNEL:%EXP (- (* 77.36033 (* 3.469446951953614d-17 0.001)))) 1.0d0
       (/ a-conc-numerator ,zf/rt)
       (/ (* voltage-in-volts a-conc-numerator) (- 1 exp-term)))))

;; Review of units
;;
;; Faraday = 9.648e4 Coulombs/mole, GasConstant = 8.314 (Volts*Coulombs)/(DegreesKelvin*mole)
;;
;;
;;  Faraday Faraday  millivolts temperature^-1 GasConstant^-1 milliMolar      Permeability
;; .....................................................................................
;;
;;  coulombs coulombs   mV       degreesK mole               10e-3  mole    cm cm cm
;;  --------  --------  --    ------------------            -----------     --------     =  nanoamps
;;    mole     mole          DegKelvin V coulomb           1000 cm cm cm     second

(proclaim '(inline constant-field-current-coefficient))
(defun constant-field-current-coefficient (elt &key valence voltage voltage-double conc-in conc-in-double conc-out conc-out-double)
  ;; For use in channel or synapse current evaluations, where the gating term and permeability are coefficients of this
  ;; term. Returns a double float.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((voltage (or voltage-double
		      (and voltage (d-flt voltage))
		      (element-voltage elt)))
	 (need-params (not (and valence
				(or conc-in-double conc-in)
				(or conc-out-double conc-out))))
	 (cint (when need-params (caar (element-conc-ints-params elt))))
	 (cint-type (when (and need-params cint) (conc-int-type cint)))
	 (elt-type (when (and need-params (not cint))
		     (typecase elt
		       (channel (channel-type elt))
		       (synapse (synapse-type elt)))))
	 (conc-in (the df (or conc-in-double
			      (and conc-in (d-flt conc-in))
			      (and cint (if (conc-int-type-intra-p cint-type)
					  (conc-int-shell-1-free-conc-n cint)
					  (conc-int-transmembrane-conc cint)))
			      (effective-default-intracellular-concentration-double elt-type)
			      1.0d0)))	; df
	 (conc-out (the df (or conc-out-double
			       (and conc-out (d-flt conc-out))
			       (and cint (if (conc-int-type-intra-p cint-type)
					   (conc-int-transmembrane-conc cint)
					   (conc-int-shell-1-free-conc-n cint)))
			       (effective-default-extracellular-concentration-double elt-type)
			       1.0d0)))	; df
	 (valence (or valence
		      (and cint (conc-int-type-valence (conc-int-type cint))) ;sf
		      (effective-default-valence elt-type)
		      1.0)))
    ;; (format t "~A conc-in ~A conc-out ~A valence ~A voltage ~A~%" elt conc-in conc-out valence voltage)
    (constant-field-equation-double voltage conc-in conc-out 1.0d0 valence 1.0d0)))

;; ************* ************* ************* *************
;;
;;    Temperature Related Functions
;;
;; ************* ************* ************* *************

(defun element-reference-temp (element)
  (or (element-parameter element 'reference-temperature)
      (let ((element-type (element-type element)))
	(when element-type
	  (typecase element-type
	    (pump-type (pump-type-reference-temp element-type))
	    (buffer-type (buffer-type-reference-temp element-type))
	    (axon-type (axon-type-reference-temp element-type))
	    (synapse-type (synapse-type-reference-temp element-type))
	    (channel-type (channel-type-reference-temp element-type))
	    (particle-type (particle-type-reference-temp element-type))
	    (conc-particle-type (conc-particle-type-reference-temp element-type))
	    (conc-int-type (conc-int-type-reference-temp element-type))
	    (t *Temperature*))))
      *Temperature*))

(defun element-q10 (element)
  (or (element-parameter element 'q10)
      (let ((element-type (element-type element)))
	(when element-type
	  (typecase element-type
	    (pump-type (pump-type-q10 element-type))
	    (buffer-type (buffer-type-q10 element-type))
	    (axon-type (axon-type-q10 element-type))
	    (synapse-type (synapse-type-q10 element-type))
	    (channel-type (channel-type-q10 element-type))
	    (particle-type (particle-type-q10 element-type))
	    (conc-particle-type (conc-particle-type-q10 element-type))
	    (conc-int-type (conc-int-type-q10 element-type))
	    (t 1.0))))
      1.0))

(proclaim '(inline q10-tau-factor))
(defun q10-tau-factor (reference-temp temp q10 &optional (ignore-q10 *ignore-q10*))
  "q10 factor for time constants (as temperature goes up, tau goes down). Temperatures in degrees K."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float reference-temp temp q10))
  (if ignore-q10 1.0 (the sf (expt q10 (/ (- reference-temp temp) 10.0)))))

(proclaim '(inline q10-rate-factor))
(defun q10-rate-factor (reference-temp temp q10 &optional (ignore-q10 *ignore-q10*))
  "q10 factor for rate constants (as temperature goes up, so does rate). Temperatures in degrees K."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type single-float reference-temp temp q10))
  (if ignore-q10 1.0 (the sf (expt q10 (/ (- temp reference-temp) 10.0)))))

(defun element-q10-rate-factor (element)
  (let ((ref-temp (element-reference-temp element))
	(q10 (element-q10 element)))
    (q10-rate-factor (temperature-centigrade-to-kelvin ref-temp) *temperature* q10)))

(defun element-q10-tau-factor (element)
  (let ((ref-temp (element-reference-temp element))
	(q10 (element-q10 element)))
    (q10-tau-factor (temperature-centigrade-to-kelvin ref-temp) *temperature* q10)))

(proclaim '(inline q10-factor))
(defun q10-factor (reference-temp temp q10 &optional (ignore-q10 *ignore-q10*))
  "q10 factor for rate constants (as temperature goes up, so does rate).  Temperatures in degrees K."
  (q10-rate-factor reference-temp temp q10 ignore-q10))

;; Backward compatibility
(proclaim '(inline qten-tau-factor))
(defun qten-tau-factor (reference-temp temp qten &optional (ignore-q10 *ignore-q10*))
  (q10-tau-factor reference-temp temp qten ignore-q10))

(proclaim '(inline qten-rate-factor))
(defun qten-rate-factor (reference-temp temp qten &optional (ignore-q10 *ignore-q10*))
  (q10-rate-factor reference-temp temp qten ignore-q10))

(proclaim '(inline qten-factor))
(defun qten-factor (reference-temp temp qten &optional (ignore-q10 *ignore-q10*))
  (q10-factor reference-temp temp qten ignore-q10))

(defun temperature-kelvin-to-centigrade (new-temp-k) (s-flt (- new-temp-k 273.16)))

(defun temperature-centigrade-to-kelvin (new-temp-c) (s-flt (+ new-temp-c 273.16)))

(defun assign-new-temp-k-to-globals (new-temp-k)
  (setq *Temp-celcius* (temperature-kelvin-to-centigrade new-temp-k)
	*temperature* (s-flt new-temp-k)))

(defun update-temperature-dependent-parameters (&optional new-temp-k force)
  (when new-temp-k (assign-new-temp-k-to-globals new-temp-k))
  (setq *update-temperature-dependent-parameters* nil)
  (when (or force
	    (not *LAST-SIMULATION-TEMPERATURE*) (not (= *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*))
	    (not *LAST-SIMULATION-TEMP-celcius*) (not (= *LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*)))

    (assign-new-temp-k-to-globals (temperature-centigrade-to-kelvin *temp-celcius*))
    (setq *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*
	  *LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*)

    (cond-every
     ((not *fix-e-na*) (setq *e-k* (NERNST-POTENTIAL *NA-CONC-INTRA* *NA-CONC-extra* 1)))
     ((not *fix-e-k*) (setq *e-k* (NERNST-POTENTIAL *k-CONC-INTRA* *k-CONC-extra* 1)))
     ((not *fix-e-cl*) (setq *e-cl* (NERNST-POTENTIAL *cl-CONC-INTRA* *cl-CONC-extra* -1)))
     ((not *fix-e-ca*) (setq *e-ca* (NERNST-POTENTIAL *cA-CONC-INTRA* *cA-CONC-extra* 2))))

    (setq NERNST-EQN-CONST*1000*TEMP (/ (* 1000.0 *Temperature*) FoverR))
    (setq Eca-Nernst-eqn-const*1000*temp
	  (* 1000.0			;convert to millivolt units
	     Eca-Nernst-eqn-const
	     *Temperature*))

    (setq *f/rt* (/ faraday (* *temperature* GasConstant)))
    (update-cell-type-ionic-parameters)

    (set-synapses-parameters)
    (set-channels-parameters)
    (set-axons-parameters)

    (make-needed-v-particle-arrays t)

    (make-needed-conc-particle-arrays t)
    (update-particle-type-q10s)
    (update-conc-particle-type-q10s)
    (update-conc-int-type-q10s)
    ))

;;; Miscellaneous

(defun scaled-sigmoid-rate (voltage &key (scale 1.0) (steepness 1.0) (v-half 0.0) (base-rate 0.0))
  ;; Adapted from Equation 33 of Des-Man-Sej-94.
  "VOLTAGE, STEEPNESS and V-HALF are in mV. BASE-RATE and SCALE are in 1/ms. The minimum/maximum values are given by
BASE-RATE and (BASE-RATE + SCALE), respectively. Note that the slope for VOLTAGE = V-HALF is [(0.25 * SCALE) / STEEPNESS]."
  (declare (optimize (speed 3) (space 0))
	   (single-float voltage scale steepness v-half base-rate))
  (+ base-rate
     (if (= 0 scale)
       0.0
       (/ scale (+ 1 (exp-w-limits (/ (- voltage v-half) (- steepness))))))))

(defun scaled-exponential-rate (voltage &key (scale 1.0) (steepness 1.0) (v-half 0.0) (base-rate 0.0) (max-rate -1.0))
  "VOLTAGE, STEEPNESS and V-HALF are in mV. BASE-RATE, SCALE, MAX-RATE are in 1/ms. The minimum/maximum values are given
by BASE-RATE and (BASE-RATE + SCALE), respectively. Note that the slope for VOLTAGE = V-HALF is SCALE / STEEPNESS."
  (declare (optimize (speed 3) (space 0) (safety 1))
	   (single-float voltage scale steepness v-half base-rate max-rate))
  (let ((value (the sf (+ base-rate
			  (the sf
			       (if (= 0 scale) 0.0
				   (the sf (* scale (the sf (exp-w-limits (/ (- voltage v-half) steepness)))))))))))
    (if (> max-rate 0)
	(min max-rate value)
	value)))

(defun scaled-exponential-soft-rate (voltage &key (scale 1.0) (steepness 1.0) (v-half 0.0) (base-rate 0.0) max-rate)
  "VOLTAGE, STEEPNESS and V-HALF are in mV.  BASE-RATE, SCALE, MAX-RATE are in 1/ms. The minimum/maximum values are given by
BASE-RATE and (BASE-RATE + SCALE), respectively. Note that the slope for VOLTAGE = V-HALF is SCALE / STEEPNESS. If MAX-RATE is
nil, then it is ignored."
  (declare (optimize (speed 3) (space 0) (safety 1))
	   (single-float voltage scale steepness v-half base-rate))
  (if (and (numberp max-rate) (= (the sf max-rate) 0))
      0.0
      (let ((value (the sf (+ base-rate
			      (the sf
				   (if (= 0 scale) 0.0
				       (the sf (* scale (the sf (exp-w-limits (/ (- voltage v-half) steepness)))))))))))
	(if (= value 0)
	    0.0
	    (if (and (numberp max-rate) (> (the sf  max-rate) 0))
		(/ (+ (/ 1 (the sf max-rate)) (/ 1 value)))
		value)))))

(defun constant-rate (voltage &key (rate 1.0))
  ;; "Ignore the VOLTAGE argument and return RATE as a single-float."
  (declare (ignore voltage))
  (s-flt rate))

(defun squashed-exponential (voltage &key (k 1.0) (v-half 0.0) (base-rate 0.0) (max-rate -1.0))
  (scaled-exponential-soft-rate (s-flt voltage )
				:steepness (s-flt k)
				:v-half (s-flt v-half) ; (v-half-prime v-half 1.0 steepness)
				:scale 1.0
				:base-rate (s-flt base-rate)
				:max-rate (s-flt max-rate)))


(defun squeezed-exponential (voltage &key (v-half 0.0) (k 1.0) (tau-max -1.0) (tau-min 0.0))
  "N.B Deprecated. Use SQUEEZED-EXPONENTIAL-RATE for correct behaviour."
;; Exponential rate function with minimum and maximum rates given by functions of TAU-MAX [default -1.0] and TAU-MIN [default
;; 0.0]. When TAU-MAX is non-positive, then the minimum rate is 0.0, otherwise 1/(TAU-MAX + 2*TAU-MIN); the maximum rate is 1/TAU-MIN. V-HALF [default 0.0] and the inverse
;; steepness K [default 1.0] are assumed to be in the same units as VOLTAGE, typically in mV. Returns single float value."
  (if (= tau-min tau-max)
      (s-flt (/ tau-min))
      (let ((value (+ (if (> tau-max 0)
			  (/ 1 (+ tau-max tau-min))
			  0)
		      (exp-w-limits (s-flt (/ (- voltage v-half) k))))))
	(if (and (= value 0) (= tau-min 0))
	    0.0
	    (/ 1 (+ tau-min (/ 1 value)))))))

(defun squeezed-exponential-rate (voltage &key (v-half 0.0) (k 1.0) (tau-max -1.0) (tau-min 0.0))
  "Exponential rate function with upper and lower bounds.
Maximum rate set by reciprocal of TAU-MIN [default 0.0]. When TAU-MAX [default -1.0] is non-positive, then the minimum rate is 0.0,
otherwise the reciprocal of TAU-MAX. V-HALF [default 0.0] and the inverse steepness K [default 1.0] are assumed to be in the same
units as VOLTAGE, typically in mV. Returns single float value."
  (when (> tau-min tau-max 0) (sim-error (format nil "SQUEEZED-EXPONENTIAL-RATE: TAU-MIN > TAU-MAX.")))
  (if (= tau-min tau-max)
      (if (zerop tau-min)
	  (sim-error (format nil "SQUEEZED-EXPONENTIAL-RATE: TAU-MIN and TAU-MAX both zero."))
	  (s-flt (/ tau-min)))
      (let* ((a (- tau-max tau-min))
	     (b tau-min)
      	     (arg (s-flt (/ (- voltage v-half) k)))
	     (rate (/ (+ (/ (+ (exp-w-limits arg)
			       (if (> tau-max 0)
				   (/ a) 0)))
			 b))))
	(when (< rate 0) (sim-error "Negative rate"))
	rate)))

(defun v-half-prime (vhalf scale steepness) (- vhalf (* steepness (log scale))))

(defun k-prime (scale v-half steepness) (* scale (exp (/ (- v-half) steepness))))

(defun exponential-term (voltage &key (scale 1.0) (steepness 1.0) (v-half 0.0))
  (* scale (exp-w-limits (s-flt (/ (- voltage v-half) steepness)))))

(defun new-exponential-term (voltage &key (steepness 1.0) (v-half 0.0))
  (let* ((scale 1.0)
	 (v-half (v-half-prime v-half scale steepness)))
    (* scale (exp-w-limits (/ (- voltage v-half) steepness)))))

(defun scaled-exponential-rate-double (voltage &key (scale 1.0d0) (steepness 1.0d0) (v-half 0.0d0) (base-rate 0.0d0) (max-rate -1.0d0))
  "VOLTAGE, STEEPNESS and V-HALF are in mV. BASE-RATE, SCALE, MAX-RATE are in 1/ms. The minimum/maximum values are given by
BASE-RATE and (BASE-RATE + SCALE), respectively. Note that the slope for VOLTAGE = V-HALF is SCALE / STEEPNESS."
  (declare (optimize (speed 3) (space 0))
	   (double-float voltage scale steepness v-half base-rate max-rate))
  (let ((value (the df (+ base-rate
		  (if (= 0.0d0 scale) 0.0d0
		      (* scale 1.0d0
			  (the df (exp-w-limits-double (/ (- voltage v-half) steepness)))))))))
    (declare (double-float value))
    (if (> max-rate 0)
	(min max-rate value)
	value)))

(proclaim '(notinline scaled-exponential-rate-double))
(defun scaled-exponential-rate-double (voltage &key (scale 1.0d0) (steepness 1.0d0) (v-half 0.0d0) (base-rate 0.0d0) (max-rate -1.0d0))
  "VOLTAGE, STEEPNESS and V-HALF are in mV. BASE-RATE, SCALE, MAX-RATE are in 1/ms. The minimum/maximum values are given by
BASE-RATE and (BASE-RATE + SCALE), respectively. Note that the slope for VOLTAGE = V-HALF is SCALE / STEEPNESS."
  (declare (optimize (speed 1) (space 0))
	   (double-float voltage scale steepness v-half base-rate max-rate))
  (let ((value
	 (+ base-rate
	    (if (= 0 scale) 0.0d0
		(* scale
		   (the df (exp-w-limits-double (the df (/ (- voltage v-half) steepness)))))))))
    (declare (double-float value))
    (if (or (< max-rate 0) (> max-rate value)) value max-rate)))

(defun scaled-slanted-step-rate (voltage &key (scale 1.0) (steepness 1.0) (v-half 0.0) (base-rate 0.0))
  "VOLTAGE, STEEPNESS and V-HALF are in mV. BASE-RATE and SCALE are in 1/ms. The minimum/maximum values are given by BASE-RATE and
(BASE-RATE + SCALE), respectively. Note that the slope at VOLTAGE = V-HALF is [SCALE / STEEPNESS]."
  (declare (optimize (speed 3) (space 0))
	   (single-float voltage scale steepness v-half base-rate))
  (+ base-rate
     (if (= 0 scale) 0.0
	 (max 0.0
	      (min (* scale (+ 0.5 (/ (- voltage v-half) steepness)))
		   scale)))))

;; not used
(defun update-constant-field-equation-exponential-term-array ()
  (setq *constant-field-equation-exponential-term-array*
	(make-array (list 3 *particle-look-up-table-length*) :element-type 'double-float))
  (loop for valence-index fixnum from 0 to 2 do
	(loop for voltage double-float from *particle-look-up-table-min-voltage-double*
	      by *particle-look-up-table-precision*
	      for voltage-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*)) do
	      (setf (aref *constant-field-equation-exponential-term-array* valence-index voltage-index)
		    (exp (* (1+ valence-index) *F/RT* voltage -1.0d-3) ; (coulombs/mole) millivolts 1.0e-3 volts/millivolt
					; DegKelvin (volts coulombs) / (degreesK mole)
			 )))))

(proclaim '(inline constant-field-equation-exponential-term-double-lookup))
(defun constant-field-equation-exponential-term-double-lookup (voltage-index valence)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type fixnum valence voltage-index))
  (aref (the (simple-array double-float (* *)) *constant-field-equation-exponential-term-array*) valence voltage-index))

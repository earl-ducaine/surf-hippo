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


;;; SYS Source file: particle.lisp

(in-package "SURF-HIPPO")

;; These two are for markov particles.

(defmacro nb-states-1 (prt) `(1- (the fn (particle-type-number-of-states (particle-type ,prt)))))
(defmacro prt-state (prt index) `(aref (the simple-vector (particle-state-arrays ,prt)) ,index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-PARTICLE-TYPE-PARTICLES (type)
  (let ((type (element type 'particle-type)))
    (when type
      (loop with prt = (particle-type-first-element type)
	    while prt 
	    sum 1 into total
	    do (setq prt (particle-next-element prt))
	    finally (return total)))))
  
(defun PARTICLE-TYPE-PARTICLES (type)
  (let* ((type (element type 'particle-type)))
    (when type
      (loop with prt = (particle-type-first-element type)
	    while prt collect prt into prts
	    do (setq prt (particle-next-element prt))
	    finally (return prts)))))

(defun last-particle (prt)
  (if (and prt (particle-next-element prt))
      (last-particle (particle-next-element prt))
      prt))

(defun last-particle-of-type (type) (last-particle (particle-type-first-element type)))

(defun clear-particles-of-type (type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (particle-type-first-element type) nil)
  (loop for prt being the hash-value of (PARTICLE-HASH-TABLE) when (eq type (particle-type prt)) do (setf (particle-next-element prt) nil)))

(defun get-particles-of-type (type) (loop for prt being the hash-value of (PARTICLE-HASH-TABLE) when (eq type (particle-type prt)) collect prt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edit-particle-type (type &optional channel-type number-particles-per-channel)
  (when (element type 'particle-type)
    (let* ((type (element type 'particle-type))
	   (channel-type (element channel-type 'channel-type))
	   menu-list
	   (dummy1 (particle-type-valence type))
	   (dummy2 (particle-type-gamma type))
	   (dummy3 (particle-type-base-rate type))
	   (dummy4 (particle-type-tau-0 type))
	   (dummy5 (particle-type-v-half type))
	   (dummy6 (particle-type-q10 type))
	   (dummy7 (particle-type-reference-temp type))
	   (dummy8 number-particles-per-channel)
	   (dummy9 (particle-type-name type))
	   (dummy10 (convert-p-type-class-sym (particle-type-class type)))
	   (dummy11 *Temp-celcius*)
	   (dummy12 (element-parameter type 'use-Fixed-boltzmann-reference-temp))
	   (dummy13 (or (element-parameter type 'Fixed-boltzmann-reference-temp) *temp-celcius*))
	   (dummy14 (cond ((particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) :Sets_tau)
			  (t (case (convert-p-type-class-sym (particle-type-class type))
			       (:hh-ext :Additive)
			       (t :Minimum_tau)))))
	   dummy15 dummy16 dummy17 dummy18
	   (dummy19 (or (element-parameter type 'alpha_0) 0.0))
	   (dummy20 (or (element-parameter type 'beta_0) 0.0))
	   (dummy21 t)
	   (dummy22 (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	   (dummy23 (element-parameter type 'linear-markov-n))
	   (dummy24 (element-parameter type 'linear-markov-m)))
      (loop while (or dummy15 dummy21) do
	    (setq dummy15 nil)
	    (case dummy10
	      (:markov (setq menu-list '((dummy17 "Edit Markov transition functions" :boolean))))
	      ((:hh-ext :hh-ext-old)
	       (setq menu-list `((:comment ,(format nil "Parameters of particle type class ~A" dummy10))
				 (dummy5 "V-1/2 [mV]" :float)
				 (dummy1 "Valence [+/- => act/inact]" :number)
				 (dummy2 "Gamma [0-1]" :float)
				 (dummy3 "Base-Rate for transitions [1/ms]" :float)
				 (dummy4 "Tau-0 [ms]" :float)
				 (dummy14 "Role of tau-0:" :choose (:Sets_tau :Additive :Minimum_tau))
				 (dummy22 "Apply tau-0 to:" :choose (:all_kinetics :v-dep_kinetics))
				 (:comment "Voltage independent rate constants")
				 (dummy19 "alpha_0 - Forward rate constant [1/ms]" :float)
				 (dummy20 "beta_0 - Backward rate constant [1/ms]" :float)
				 (:comment "")
				 (dummy12 "Use fixed boltzmann reference temperature" :boolean)
				 (dummy13 "Fixed boltzmann reference temperature [deg C]" :float))))
	      (t (setq menu-list `((:comment ,(format nil "Particle type class ~A." dummy10))))))
	    (setq menu-list (reverse menu-list))
	    (when (and dummy23 dummy24)
	      (push '(dummy24 "Linear Markov model M:" :integer) menu-list)
	      (push '(dummy23 "Linear Markov model N:" :integer) menu-list))
	    (unless channel-type
	      (push `(dummy15 "Plot voltage-dependent particles:" :x-choose
			      ,(case (particle-type-class type)
				 (:markov '(:alpha_&_beta :steady_state))
				 (t '(:steady_state :tau :alpha_&_beta)))) menu-list))
	    (push '(dummy6 "Q10" :float) menu-list)
	    (push '(dummy7 "Kinetics Reference Temperature [degs C]" :float) menu-list)
	    (push '(dummy11 "Current temperature [degs C]" :float) menu-list) 
	    (when (element-parameter type 'concentration-particle-type)
	      (push '(dummy18 "Edit associated concentration particle type" :boolean) menu-list))
	    (when (and number-particles-per-channel channel-type)
	      (push '(dummy8 "Number of particles per channel" :integer) menu-list))
	    (push `(dummy9 ,(format nil "Edit name of type (used if saved to file):") :string) menu-list)
	    (when (element-parameter type 'v-half-shift-particle-type)
	      (push `(dummy16 ,(format nil "Edit V-shift ~A particle type" (element-name (element-parameter type 'v-half-shift-particle-type))) :boolean)
		    menu-list))
	    (setq menu-list (reverse menu-list))
	    (choose-variable-values menu-list
				    :text (ADD-LINEFEEDS-TO-STRING-LIST (list (ELEMENT-SOURCEFILE-STRING type nil))) 
				    :label (format nil "Parameters Of Particle Type ~A" (particle-type-name type)))
	    (when (and dummy24 dummy23)
	      (element-parameter type 'linear-markov-n dummy23)
	      (element-parameter type 'linear-markov-m dummy24))
	    (when dummy17 (edit-markov-transition-functions type))
	    (when dummy16 (edit-element (element-parameter type 'v-half-shift-particle-type)))
	    (when dummy18 (edit-element (element-parameter type 'concentration-particle-type)))
	    (let* ((temp-changed-for-this-prt-type
		    (or (xor dummy12 (element-parameter type 'use-Fixed-boltzmann-reference-temp))
			(and (element-parameter type 'Fixed-boltzmann-reference-temp)
			     (not (and (element-parameter type 'Fixed-boltzmann-reference-temp)
				       (= dummy13 (element-parameter type 'Fixed-boltzmann-reference-temp)))))
			(not (eq dummy14 (cond ((particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) :Sets_tau)
					       (t (case (convert-p-type-class-sym (particle-type-class type))
						    (:hh-ext :Additive)
						    (t :Minimum_tau))))))
			(not (eq dummy22 (element-parameter type 'tau_0_applied_to)))
			(not (= dummy6 (particle-type-q10 type)))
			(not (= dummy7 (particle-type-reference-temp type)))))
		   (nothing-changed
		    (and (not temp-changed-for-this-prt-type)
			 (= dummy19 (or (element-parameter type 'alpha_0) 0.0))
			 (= dummy20 (or (element-parameter type 'beta_0) 0.0))
			 (= dummy1 (particle-type-valence type))
			 (= dummy2 (particle-type-gamma type))
			 (= dummy3 (particle-type-base-rate type))
			 (= dummy4 (particle-type-tau-0 type))
			 (= dummy5 (particle-type-v-half type))
			 (= dummy6 (particle-type-q10 type)))))
	      (setq *update-temperature-dependent-parameters*
		    (or *update-temperature-dependent-parameters* (not (= dummy11 *Temp-celcius*))))
	      (setq *temperature* (temperature-centigrade-to-kelvin dummy11) *Temp-celcius* dummy11)
	      (unless (string= (format nil "~A" (particle-type-name type)) dummy9)
		(SET-ELEMENT-NAME type dummy9))
	      (case (particle-type-class type)
		((:hh-ext :hh-ext-old)
		 (element-parameter type 'tau_0_applied_to dummy22)
		 (element-parameter type 'alpha_0 (unless (= 0 dummy19) dummy19))
		 (element-parameter type 'beta_0 (unless (= 0 dummy20) dummy20))
		 (setf (particle-type-valence type) dummy1
		       (particle-type-gamma type) dummy2
		       (particle-type-base-rate type) dummy3
		       (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) (eq :sets_tau dummy14)
		       (particle-type-tau-0 type) dummy4
		       (particle-type-v-half type) dummy5)
		 (element-parameter type 'use-Fixed-boltzmann-reference-temp dummy12)
		 (element-parameter type 'Fixed-boltzmann-reference-temp (s-flt dummy13))))


	      (case dummy14
		(:Sets_tau (setf (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) t))
		(:Additive (setf (particle-type-class type) :hh-ext
				 (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) nil))
		(:Minimum_tau (setf (particle-type-class type) :hh-ext-old
				    (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) nil)))
	      (setf (particle-type-q10 type) dummy6
		    (particle-type-reference-temp type) dummy7)
	      (if *update-temperature-dependent-parameters*
		(update-temperature-dependent-parameters))
	      (unless nothing-changed (update-particle-type type))
	      (when dummy15 (plot-particle-types type :what dummy15))
	      (setq dummy21 nil)))
      (values type dummy8))))

(defun particles-of-type (type)
  (let ((type (element type 'particle-type)))
    (loop for prt in (particles) when (equal (particle-type prt) type) collect prt)))

(defun print-particle-type (type &optional (always t))
  (let ((type (element type 'particle-type)))
    (when (and type (or always (particles-of-type type)))
      (format t "Particle-type ~a (class ~s):~%" (particle-type-name type) (particle-type-class type))
      (case (particle-type-class type)
	(:markov
	 (format t "    States: ") (simple-format-list (element-parameter type 'STATEs) t)
	 (format t "    Open States: ") (simple-format-list (element-parameter type 'open-STATEs) t)
	 (when (element-parameter type 'initial-state)
	   (format t "    Explicit Initial Conditions: ~A~%" (element-parameter type 'initial-state))) 
	 (format t "~%")
	 (print-markov-particle-type-STATE-VOLTAGE-TRANSITION-FUNCTIONS type))
	((or :hh-ext-old :hh-ext)
	 (format t "    z ~a gamma ~a base-rate ~a V-1/2 ~,f tau-0 ~a ~A~,f Q10 ~a T_ref ~a~%"
		 (particle-type-valence type)
		 (particle-type-gamma type)
		 (if (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) "UNDEFINED" (particle-type-base-rate type))
		 (particle-type-v-half type)
		 (case (particle-type-class type)
		   (:hh-ext-old "(hard limit)")
		   (:hh-ext "(additive)"))
		 (case (particle-type-class type)
		   (:hh-ext-old "")
		   (:hh-ext (case (or (element-parameter type 'tau_0_applied_to) :all_kinetics)
			      (:all_kinetics "(applied to entire kinetics) ") 
			      (t "(applied to v-dep kinetics) ")))) 
		 (particle-type-tau-0 type)
		 (particle-type-Q10 type)
		 (particle-type-reference-temp type)))
	(:hh			
	 (format t "    Canonical Hodgkin-Huxley Parameters, Q10 ~a T_ref ~a~%" (particle-type-Q10 type) (particle-type-reference-temp type))))
      (case (particle-type-class type)
      	((:hh :hh-ext :hh-ext-old)
	 (when (element-parameter type 'linear-markov) 
	   (format t "    Linear Markov Model, N= ~D, M = ~D~%" (element-parameter type 'linear-markov-n) (element-parameter type 'linear-markov-m)))))
      (case (particle-type-class type)
	((:hh-ext :hh-ext-old)
	 (when (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type)
	   (format t "    Tau voltage dependence ignored~%"))
	 (when (and (element-parameter type 'use-Fixed-boltzmann-reference-temp) (element-parameter type 'Fixed-boltzmann-reference-temp))
	   (format t "    Fixed boltzmann reference temperature ~A~%" (element-parameter type 'Fixed-boltzmann-reference-temp)))))
      (cond-every
       ((element-parameter type 'alpha_0)
	(format t "    Alpha_0 (v-indep forward rate constant) [1/ms]: ~A~%" (element-parameter type 'alpha_0)))
       ((element-parameter type 'beta_0)
	(format t "    Beta_0 (v-indep backward rate constant) [1/ms]: ~A~%" (element-parameter type 'beta_0)))
       ((particle-type-alpha-function type) (format t "    Explicit alpha rate function~%"))
       ((particle-type-beta-function type) (format t "    Explicit beta rate function~%"))
       ((and (particle-type-alpha-function type) (particle-type-beta-function type) (/= 1 (particle-type-tau-coefficient type)))
	(format t "    Tau coefficient: ~A~%" (particle-type-tau-coefficient type)))
       ((particle-type-tau-function type)
	(if (numberp (particle-type-tau-function type))
	  (format t "    Fixed Tau ~,2ems~%" (particle-type-tau-function type))
	  (format t "    Explicit tau function~%")))
       ((particle-type-ss-function type) (format t "    Explicit steady-state function~%")))
      (print-num-elements-sourcefile type t t)
      (format t "~%"))))
		 
(defun document-particle-type (type-name-or-type)
  (let* ((type-name (element-name type-name-or-type 'particle-type))
	 (type (element type-name-or-type 'particle-type))
	 (cprt-type (element-parameter type 'concentration-particle-type)))
    (when type
      (format t "(particle-type-def~%")
      (format t " (~a~%" type-name)
      (format t "  (class . ~s)~%" (particle-type-class type))
      (case (particle-type-class type)
	(:markov
	 (format t "  (STATES . ~A)~%" (element-parameter type 'STATES))
	 (format t "  (OPEN-STATES . ~A)~%" (element-parameter type 'open-STATEs))
	 (format t "  (STATE-TRANSITIONS . ~%")
	 (format t "   ~a)~%" (markov-particle-type-STATE-VOLTAGE-TRANSITION-FUNCTIONS-string type)))
	((:hh-ext :hh-ext-old)
	 (format t "  (valence . ~a)~%" (particle-type-valence type))
	 (format t "  (gamma . ~a) ; ~A~%" (particle-type-gamma type) (documentation 'particle-type-gamma 'function))
	 (format t "  (base-rate . ~a) ; ~A~%" (particle-type-base-rate type) (documentation 'particle-type-base-rate 'function))
	 (format t "  (v-half . ~a) ; ~A~%" (particle-type-v-half type) (documentation 'particle-type-v-half 'function))
	 (format t "  (tau-0 . ~a) ; ~A~%" (particle-type-tau-0 type) (documentation 'particle-type-tau-0 'function))
	 (format t "  (IGNORE-TAU-VOLTAGE-DEPENDENCE . ~a)~%" (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type))))
      (when (and (element-parameter type 'linear-markov-n) (element-parameter type 'linear-markov-m))
	(format t "  (linear-markov . ~A)~%" (list (element-parameter type 'linear-markov-n) (element-parameter type 'linear-markov-m))))
      (when (element-parameter type 'alpha_0)
	(format t "  (alpha_0 . ~a)~%"	(element-parameter type 'alpha_0)))
      (when (element-parameter type 'beta_0)
	(format t "  (beta_0 . ~a)~%"	(element-parameter type 'beta_0)))
      (format t "  (reference-temp . ~a) ; ~A~%" (particle-type-reference-temp type) (documentation 'particle-type-reference-temp 'function))
      (when (element-parameter type 'FIXED-BOLTZMANN-REFERENCE-TEMP)
	(format t "  (FIXED-BOLTZMANN-REFERENCE-TEMP . ~a)~%" (element-parameter type 'FIXED-BOLTZMANN-REFERENCE-TEMP)))
      (format t "  (q10 . ~a)~%" (particle-type-q10 type))
      (when cprt-type
	(format t "  (concentration-particle-type . ~a)~%" (element-name cprt-type)))
      (print-element-document-extras type)
      (format t "   ))~%~%")
      (when cprt-type (document-conc-particle-type cprt-type)))))

(defun print-particle-types () (PRINT-MODEL-PARAMETERS "particle-type"))
(defun print-particles () (PRINT-MODEL-PARAMETERS "particle"))

(defun print-particle (prt)
  (let ((prt (element prt 'particle)))
    (when prt
      (let ((prt-cell (node-cell (particle-vnode-point prt)))
	    (prt-channel (particle-channel prt)))
	(format t "~a: type ~A, cell ~a, channel ~A~A~%"
		(massage-element-plot-label prt nil t)
		(particle-type-name (particle-type prt))
		(if prt-cell (cell-name prt-cell) "No cell")
		(if prt-channel (channel-name prt-channel) "No channel")
		(if (and *simulation-initialized* (not (element-parameter prt 'is-sub-particle-of)))
		    (format nil ", State ~,2e @ ~,2e ms" (s-flt (particle-state-n+1-double prt)) *real-time*) "")))
      (when (element-parameter prt 'is-sub-particle-of)
	(format t "   This is a sub-particle of Markov particle ~A.~%" (element-name (element-parameter prt 'is-sub-particle-of))))
      (when (element-parameter prt 'dummy-particle)
	(format t "   This is a dummy particle.~%"))
      (when (element-parameter prt 'initial-state)
	(format t "   Explicit Initial Conditions: ~A~%" (element-parameter prt 'initial-state))))))

(defun convert-p-type-class-sym (sym)
  (case sym
    ((markov :markov) :markov)
    ((hh :hh) :hh)
    ((hh-ext :hh-ext) :hh-ext)
    ((hh-ext-old :hh-ext-old) :hh-ext-old)))

(defun create-particle-type (type-symbol &optional actual-type-symbol)
  (let* ((type (unless actual-type-symbol (if (particle-type-p type-symbol) type-symbol (PARTICLE-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (particle-type-p type-symbol) (particle-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'particle-type))
	 (library-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (type-parameters nil)
	 (parent-type-symbol (get-a-value 'parent-type library-parameters)))
      (when (eq parent-type-symbol type-symbol) (sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless library-parameters (sim-error (format nil "Don't know anything about particle type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      ;; If particle type does not already exist, create a new one, either based on the parent type reference, if supplied, or a totally new one. 
      (unless type (setq type (if parent-type-symbol
				  (create-particle-type parent-type-symbol type-symbol)
				  (make-particle-type :name type-symbol))))
      ;; Initialize the type's parameters with those of the parent type, if it exists.
      (update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type)
      ;; Now add in the parameters from the library.
      (setq type-parameters (update-element-parameters-with-new-parameters library-parameters type))
;;          (break)
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
 
     (setf (particle-type-class type) (convert-p-type-class-sym (get-a-value 'class type-parameters)))
      (let* ((ignore-tau-voltage-dependence (get-a-value 'ignore-tau-voltage-dependence type-parameters))
	     (base-rate-undefined (or (get-a-value 'base-rate-undefined type-parameters)
				      ignore-tau-voltage-dependence
				      (if parent-type-symbol (particle-type-ignore-tau-voltage-dependence type)	(not (assoc 'base-rate type-parameters))))))
	(case (particle-type-class type)
	  (:markov (set-markov-particle-type-parameters type type-parameters))
	  ((:hh-ext ::hh-ext-old)
	   (cond-every
	    ((assoc 'valence type-parameters) (setf (particle-type-valence type) (get-a-sf-value 'valence type-parameters)))
	    ((or base-rate-undefined ignore-tau-voltage-dependence) (setf (particle-type-gamma type) 0.0))
	    ((assoc 'gamma type-parameters) (setf (particle-type-gamma type) (get-a-sf-value 'gamma type-parameters)))
	    ((assoc 'v-half type-parameters) (setf (particle-type-v-half type) (get-a-sf-value 'v-half type-parameters)))
	    ((assoc 'tau-0 type-parameters) (setf (particle-type-tau-0 type) (get-a-sf-value 'tau-0 type-parameters)))
	    (base-rate-undefined (setf (particle-type-base-rate type) 1.0))
	    ((assoc 'base-rate type-parameters) (setf (particle-type-base-rate type) (get-a-sf-value 'base-rate type-parameters)))
	    (t (element-parameter type (or (get-a-value 'tau_0_applied_to type-parameters) :all_kinetics))))))
	(cond-every
	 ((assoc 'evaluation-function type-parameters)
	  (setf (particle-type-evaluation-function type) (compile-or-extract-function-or-number (get-a-value 'evaluation-function type-parameters))))
	 ((assoc 'linear-markov type-parameters)
	  (element-parameter type 'linear-markov-n (round (car (get-a-value 'linear-markov type-parameters))))
	  (element-parameter type 'linear-markov-m (round (cadr (get-a-value 'linear-markov type-parameters))))
	  (element-parameter type 'linear-markov t))
	 ((assoc 'alpha_0 type-parameters) (element-parameter type 'alpha_0 (s-flt (get-a-value 'alpha_0 type-parameters))))
	 ((assoc 'beta_0 type-parameters) (element-parameter type 'beta_0 (s-flt (get-a-value 'beta_0 type-parameters))))
	 ((assoc 'concentration-particle-type type-parameters)
	  (let ((cprt-type (create-conc-particle-type (get-a-value 'concentration-particle-type type-parameters))))
	    (element-parameter cprt-type 'reference-particle-type type)
	    (element-parameter type 'concentration-particle-type cprt-type)))
	 ((assoc 'v-half-shift-particle-type type-parameters)
	  (element-parameter type 'v-half-shift-particle-type (create-particle-type (get-a-value 'v-half-shift-particle-type type-parameters)))
	  (element-parameter type 'v-half-shift-magnitude (s-flt (or (get-a-value 'v-half-shift-magnitude type-parameters) 0.0))))
	 ((or (assoc 'Fixed-boltzmann-reference-temperature type-parameters)
	      (assoc 'Fixed-boltzmann-reference-temp type-parameters))
	  (element-parameter type 'Fixed-boltzmann-reference-temp (s-flt (or (get-a-value 'Fixed-boltzmann-reference-temp type-parameters)
									     (get-a-value 'Fixed-boltzmann-reference-temperature type-parameters))))
	  (element-parameter type 'use-Fixed-boltzmann-reference-temp t))
	 ((or base-rate-undefined IGNORE-Tau-VOLTAGE-DEPENDENCE) (setf (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) t))
	 ((or (assoc 'alpha type-parameters) (assoc 'alpha-function type-parameters))
	  (setf (particle-type-alpha-function type)
		(compile-or-extract-function-or-number (or (get-a-value 'alpha type-parameters) (get-a-value 'alpha-function type-parameters)))))
	 ((or (assoc 'beta type-parameters) (assoc 'beta-function type-parameters))
	  (setf (particle-type-beta-function type)
		(compile-or-extract-function-or-number (or (get-a-value 'beta type-parameters) (get-a-value 'beta-function type-parameters)))))
	 ((or (assoc 'ss type-parameters) (assoc 'ss-function type-parameters))
	  (setf (particle-type-ss-function type)
		(compile-or-extract-function-or-number (or (get-a-value 'ss type-parameters) (get-a-value 'ss-function type-parameters)))))
	 ((or (assoc 'tau type-parameters) (assoc 'tau-function type-parameters))
	  (setf (particle-type-tau-function type)
		(compile-or-extract-function-or-number (or (get-a-value 'tau type-parameters) (get-a-value 'tau-function type-parameters)))))
	 ((assoc 'tau-coefficient type-parameters) (setf (particle-type-tau-coefficient type) (get-a-value 'tau-coefficient type-parameters)))
	 ((or (assoc 'qten type-parameters) (assoc 'q10 type-parameters))
	  (setf (particle-type-q10 type) (s-flt (cdr (or (assoc 'qten type-parameters) (assoc 'q10 type-parameters))))))
	 ((assoc 'reference-temp type-parameters) (setf (particle-type-reference-temp type) (get-a-sf-value 'reference-temp type-parameters))))
	(setf (PARTICLE-TYPE-HASH-TABLE type-symbol) type)
	;; Some warnings...
	(cond ((and (eq (particle-type-class type) :hh)
		    (not
		     (or (and (particle-type-tau-function type) (particle-type-ss-function type))
			 (and (particle-type-alpha-function type) (particle-type-beta-function type)))))
	       (sim-error (format nil "Particle Type ~A is ~s class, and requires either both ALPHA and BETA, or both SS and TAU specifications."
				  (particle-type-name type) (particle-type-class type)))))
	;; Do this after the setf of the hash-table since we need ELEMENT to work.
	(make-v-particle-arrays type)
	(unless (or (member (particle-type-class type) '(:hh-ext ::hh-ext-old :markov))
		    (and (particle-type-alpha-function type) (particle-type-beta-function type))
		    (and (particle-type-tau-function type) (particle-type-ss-function type)))
	  (sim-error (format nil "Part type ~A needs a/b *or* tau/ss functions!" type))))
    (setq *particle-type* type)
    type))

(defun get-particle-simple-name ()
  (loop for candidate from (max 1 *particle-simple-name-counter*) until (not (particle-hash-table candidate))
	finally (return (setf *particle-simple-name-counter* candidate))))

(defun rename-particles-simple (&optional (particles (particles)))
  "Rename PARTICLES [default all particles in circuit] with simple integer names."
  (loop for prt in particles do (set-element-name prt (get-particle-simple-name) 'particle)))

(defun create-particle (channel type &optional particle dummy-particle)
  (let* (*print-pretty*
	 (type (if (particle-type-p type) type (create-particle-type type)))
	 ;; (type-parameters (particle-type-parameters type))
	 (particle-name (if particle (particle-name particle)
			    (if dummy-particle
			      (format nil "Dummy-Particle-~A" (gensym))
			      (if *use-simple-names* (get-particle-simple-name) (concatenate-strings (channel-name channel) "-" (particle-type-name type))))))
	 (nd (if dummy-particle (create-node nil :dummy-node t) (or (channel-pre-synaptic-node channel) (channel-node channel))))
	 (prt (or particle (make-particle :name particle-name :type type :channel channel :vnode-point nd))))
    (element-parameter prt 'dummy-particle dummy-particle)
    (cond-every
     ((element-parameter type 'concentration-particle-type)
      (let ((sub-particle (create-conc-particle channel (element (element-parameter type 'concentration-particle-type)) dummy-particle)))
	(element-parameter sub-particle 'is-sub-particle-of prt)
	(setf (particle-conc-particle prt) sub-particle)))
     ((element-parameter type 'v-half-shift-particle-type) (create-v-half-shift-particle prt type)))
    (case (particle-type-class type)
      (:markov
       (setf (particle-state-arrays prt) (sequence-to-array (loop for i from 1 to (particle-type-number-of-states type) collect (make-particle-double-floats))))))
    (setf (PARTICLE-HASH-TABLE particle-name) prt)
    (unless dummy-particle
      (push prt (node-elements nd))
      (element-slot-fast (element-slot-fast type :last-element) :next-element prt)
      (element-slot-fast type :last-element prt)
      (setf (node-has-v-dep-element nd) t))
    (setq *particle* prt)
    prt))

(defun markov-particle-p (particle)
  (let ((particle (element particle 'particle)))
    (when particle (eq (particle-type-class (particle-type particle)) :markov))))

(defun create-v-half-shift-particle (prt type)
  (sim-error "Fix this code create-v-half-shift-particle!")
  (element-parameter prt 'v-half-shift-particle (create-particle (particle-channel prt) (element-parameter type 'v-half-shift-particle-type))))

(proclaim '(inline particle-concentration-particle))
(defun particle-concentration-particle (prt) (particle-conc-particle prt))

(proclaim '(inline particle-v-index particle-v-rem))
(defun particle-v-index (prt) (node-prt-v-index (particle-vnode-point prt)))

(defun particle-v-rem (prt) (node-prt-v-index-rem (particle-vnode-point prt)))

(defun particle-look-up-voltages ()
  (loop for voltage single-float from (the sf *particle-look-up-table-min-voltage*) by (the sf *particle-look-up-table-precision*)
	for array-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*)) 
	collect voltage))

;; (defun particle-look-up-table-max-voltage () (+ *particle-look-up-table-min-voltage* (* *particle-look-up-table-precision* *particle-look-up-table-length*)))

#|
(defun particle-look-up-voltages-by-1 ()
  (list-of-nums (- *particle-look-up-table-max-voltage* *particle-look-up-table-min-voltage*) *particle-look-up-table-min-voltage*))
|#

(defun v-inf-particle-plot-list (particle-type &optional (power 1))
  (let ((particle-type (element particle-type 'particle-type)))
    (when particle-type
      (case (particle-type-class particle-type)
	(:markov (v-inf-markov-particle-plot-lists particle-type))
	(t (let ((linear-markov-n (element-parameter particle-type 'linear-markov-n))
		 (linear-markov-m (element-parameter particle-type 'linear-markov-m)))
	     (unless (arrayp (particle-type-inf-array particle-type)) (make-v-particle-arrays particle-type))
	     (loop for voltage  from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
		   for i from 0 to (1- (length  (particle-type-inf-array particle-type)))
		   collecting voltage into volts
		   collecting (let* ((p (aref (particle-type-inf-array particle-type) i))
				     (p-cumulative
				      (if linear-markov-n (linear-markov-binomial p linear-markov-n linear-markov-m) p)))
				(if (= power 1) p-cumulative (expt p-cumulative power)))
		   into inf
		   finally (return (list volts inf)))))))))

(defun v-tau-particle-plot-list (particle-type)
  (let ((particle-type (element particle-type 'particle-type)))
    (when particle-type
      (case (particle-type-class particle-type)
	(:markov)
	(t (unless (arrayp (particle-type-tau-array particle-type)) (make-v-particle-arrays particle-type))
	   (loop for voltage  from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
		 for i from 0 to (1- (length  (particle-type-inf-array particle-type)))
		 collecting voltage into volts
		 collecting (aref (particle-type-tau-array particle-type) i) into tau
		 finally (return (list volts tau))))))))

(proclaim '(inline gating-particle-beta-prime-function gating-particle-alpha-prime-function))
(defun gating-particle-alpha-prime-function (base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 1.0))
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature))
  (* base-rate 
     (exp-w-limits (if arg-constant
		     (* (the sf arg-constant) (- voltage v-half) gamma)
		     (* (/ (* z 1.0e-3 FoverR) Temperature)
			(- voltage v-half) gamma)))))

(defun gating-particle-beta-prime-function (base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 1.0))
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature))
  (* base-rate 
     (exp-w-limits (if arg-constant
		     (* (the sf arg-constant) (- voltage v-half) (- gamma 1.0))
		     (* (/ (* z 1.0e-3 FoverR) Temperature) (- voltage v-half) (- gamma 1.0))))))

(defun gating-particle-alpha-beta-values (type base-rate gamma v-half voltage
					       &key (arg-constant 0.0) (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0)
					       (tau_0_role :all_kinetics) ; :v-dep_kinetics
					       always-calculate-rate)
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage alpha_0 beta_0 tau_0 arg-constant))
  (cond ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
         (values alpha_0 beta_0))
        ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
         (if (= tau_0 0)
	   (sim-error (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!"  type))
	   (values (/ 1 (* 2 tau_0)) (/ 1 (* 2 tau_0)))))
	(t (let ((alpha-prime (if (particle-type-alpha-function type)
				(evaluate-particle-type-alpha-function type voltage)
				(gating-particle-alpha-prime-function base-rate gamma v-half voltage :arg-constant arg-constant)))
		 (beta-prime (if (particle-type-beta-function type)
			       (evaluate-particle-type-beta-function type voltage)
			       (gating-particle-beta-prime-function base-rate gamma v-half voltage :arg-constant arg-constant))))
	     (declare (single-float alpha-prime beta-prime))
	     (case tau_0_role
	       (:all_kinetics
		(let* ((alpha (+ alpha-prime alpha_0))
		       (beta (+ beta-prime beta_0))
		       (denominator (+ (* tau_0 (+ alpha beta)) 1)))
		  (values (/ alpha denominator) (/ beta denominator))))
	       (:v-dep_kinetics
		(let ((denominator (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))
		  (values (+ alpha_0 (/ alpha-prime denominator)) (+ beta_0 (/ beta-prime denominator))))))))))

(proclaim '(inline gating-particle-beta-function gating-particle-alpha-function))
(defun gating-particle-alpha-function (voltage &key type base-rate gamma v-half arg-constant (z 0.0) (temperature 0.0)
					       (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0) always-calculate-rate
					       (tau_0_role :all_kinetics) ; :v-dep_kinetics
					       )
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature alpha_0 beta_0 tau_0))
  (cond ((particle-type-alpha-function type) (evaluate-particle-type-alpha-function type voltage))
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
	 alpha_0)
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
	 (if (= tau_0 0)
	   (sim-error (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!" type))
	   (/ 1 tau_0)))
	(t (let ((alpha-prime
		  (gating-particle-alpha-prime-function base-rate gamma v-half voltage :arg-constant arg-constant :z z :temperature temperature)))
	     (if (or (and type (eq (particle-type-class type) :hh-ext-old)) (= tau_0 0))
	       (+ alpha-prime alpha_0)
	       (let ((beta-prime (gating-particle-beta-prime-function base-rate gamma v-half voltage :arg-constant arg-constant :z z :temperature temperature)))
		 (case tau_0_role
		   (:all_kinetics (/ (+ alpha-prime alpha_0) (+ (* tau_0 (+ alpha-prime alpha_0 beta-prime beta_0)) 1)))
		   (:v-dep_kinetics (+ alpha_0 (/ alpha-prime (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))))))))))

(defun gating-particle-beta-function (voltage &key type base-rate gamma v-half arg-constant (z 0.0) (temperature 0.0)
					      (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0) always-calculate-rate
					      (tau_0_role :all_kinetics)) ; :v-dep_kinetics
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature alpha_0 beta_0 tau_0))
  (cond ((particle-type-beta-function type) (evaluate-particle-type-beta-function type voltage))
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
	 beta_0)
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
	 (if (= tau_0 0)
	   (sim-error (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!" type))
	   (/ 1 tau_0)))
	(t
	 (let ((beta-prime (gating-particle-beta-prime-function base-rate gamma v-half voltage :arg-constant arg-constant :z z :temperature temperature)))
	   (if (or (and type (eq (particle-type-class type) :hh-ext-old)) (= tau_0 0))
	     (+ beta-prime beta_0)
	     (let ((alpha-prime (gating-particle-alpha-prime-function base-rate gamma v-half voltage :arg-constant arg-constant :z z :temperature temperature)))
	       (case tau_0_role
		 (:all_kinetics (/ (+ beta-prime beta_0) (+ (* tau_0 (+ alpha-prime alpha_0 beta-prime beta_0)) 1)))
		 (:v-dep_kinetics (+ beta_0 (/ beta-prime (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))))))))))

(defun get-particle-look-up-table-sf-array () (make-array *particle-look-up-table-length* :element-type 'single-float))
(defun get-particle-look-up-table-df-array () (make-array *particle-look-up-table-length* :element-type 'double-float))

(defun check-particle-type-inf-and-tau-array-characteristics (type)
    (declare (optimize (safety 1) (speed 3) (space 0)))
    ;; If the precision of the look up tables has been changed, we may need to adjust either the temporary alpha and beta arrays,
    ;; and the particle type alpha and beta arrays. This also creates the first instances of the particle-type arrays.
    (unless (and (arrayp (particle-type-inf-array type))
		 (arrayp (particle-type-tau-array type))
		 (= (length (the vec-flt (particle-type-inf-array type))) *particle-look-up-table-length*))
      (setf (particle-type-inf-array type) (get-particle-look-up-table-sf-array)
	    (particle-type-tau-array type) (get-particle-look-up-table-sf-array)))
    nil)

(defun make-v-particle-arrays (type)
  (let ((type (element type 'particle-type)))
    (when type
      (case (particle-type-class type)
	(:markov (fill-markov-transition-array type))
	(t (make-two-state-v-particle-arrays type))))))

(defun make-two-state-v-particle-arrays (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((type (element type 'particle-type)))
    (when type
      (check-particle-type-inf-and-tau-array-characteristics type)
      (let ((alpha_0 (or (element-parameter type 'alpha_0) 0.0))
	    (beta_0 (or (element-parameter type 'beta_0) 0.0))
	    (tau_0_role (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	    (tau_0 (particle-type-tau-0 type))
	    (base-rate (particle-type-base-rate type))
	    (arg-constant		; This just saves crunching in the loop below. The factor of 1.0e-3 is because the voltage units below are mV.
	     (/ (* 1.0e-3 FoverR (particle-type-valence type))
		(the sf (or (and (element-parameter type 'use-Fixed-boltzmann-reference-temp)
				 (+ 273.16 (the sf (element-parameter type 'Fixed-boltzmann-reference-temp))))
			    *Temperature*))))
	    (q10-tau-factor (element-q10-tau-factor type)))
	(declare (type single-float arg-constant q10-tau-factor alpha_0 beta_0))
	(case (particle-type-class type)
	  ((:hh-ext-old :hh-ext)
	   (when (= alpha_0 beta_0 tau_0 (particle-type-base-rate type) 0)
	     (sim-error (format nil "~A has alpha_0, beta_0, base rate and tau-0 equal to 0!!" type)))))
	;; Now loop over the voltage range.
	(loop for voltage single-float from *particle-look-up-table-min-voltage* by (the sf *particle-look-up-table-precision*)
	      for array-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*)) do
	      (case (particle-type-class type)
		((:hh-ext-old :hh-ext)
		 (put-values-for-hh-tau-and-inf-array type base-rate voltage arg-constant alpha_0 beta_0 tau_0 array-index q10-tau-factor tau_0_role))
		(t (put-values-for-non-hh-tau-and-inf-array type voltage array-index q10-tau-factor))))))))

(defun foo (param)
  (case param
    (:state-transitions (format t "found :state-transitions"))
    (state-transitions (format t "found 'state-transitions"))))
    
(defun particle-type-parameter (type param &optional (value nil value-supplied-p) (update t))
  ;; For examining or setting specific structure parameters of the particle type associated with TYPE. PARAM can be any of the entries documented for the
  ;; PARTICLE-TYPE-DEF macro.If no new VALUE follows the PARAM, then the current value of the slot corresponding to PARAM is returned. For example, if
  ;; *PARTICLE-TYPE* is a :HH-EXT class of gating particle, the :GAMMA parameter would be set to 0.2 by 
  ;;
  ;; (PARTICLE-TYPE-PARAMETER *PARTICLE-TYPE* :GAMMA 0.2)
  ;;
  ;; The properties of particle TYPE are updated to reflect the new parameter VALUE when UPDATE is T [default].
  (let ((type (or (element-type type `particle) (element-type type `particle-type))))
    (unless type (sim-error (format nil "~A does not have an associated particle type!" type)))
    (unless (case (particle-type-class type)
	      (:HH (member param '(:QTEN :q10 :REFERENCE-TEMP :FIXED-BOLTZMANN-REFERENCE-TEMP :IGNORE-TAU-VOLTAGE-DEPENDENCE :TAU-COEFFICIENT :ALPHA :BETA
				   :SS)))
	      (:HH-EXT (member param '(:QTEN :q10 :REFERENCE-TEMP :FIXED-BOLTZMANN-REFERENCE-TEMP :IGNORE-TAU-VOLTAGE-DEPENDENCE :TAU-COEFFICIENT
				       :ALPHA :BETA :SS :VALENCE :GAMMA :K :V-HALF :TAU-0 :ALPHA_0 :BETA_0)))
	      (:MARKOV (member param '(:QTEN :q10 :REFERENCE-TEMP :FIXED-BOLTZMANN-REFERENCE-TEMP :STATES :OPEN-STATES :STATE-TRANSITIONS)))
	      (t t))
      (sim-error (format nil "Particle type ~A is of class ~A, which does not have a ~s parameter!" (element-name type) (particle-type-class type) param)))
    (case param
      (:qten (setq param :q10))
      (:FIXED-BOLTZMANN-REFERENCE-TEMP (element-parameter type 'use-Fixed-boltzmann-reference-temperature t)
				       (element-parameter type 'use-Fixed-boltzmann-reference-temp t)))
    (when (and value-supplied-p (numberp value)) (setq value (s-flt value)))
    (let ((return-value
	   (if (element-slot-p type param)
	       (if value-supplied-p (element-slot-function type param value update) (element-slot-function type param))
	       (case param
		 (:state-transitions
		  (when value-supplied-p (element-parameter type 'state-transitions value) (MAKE-MARKOV-ARRAYS type))
		  (element-parameter type 'state-transitions))
		 (:states (when value-supplied-p (element-parameter type 'states value) (MAKE-MARKOV-ARRAYS type))
			  (element-parameter type 'states))
		 (:open-states (when value-supplied-p
				 (element-parameter type 'open-states value)
				 (let ((array (list-to-array (loop for state in (element-parameter type 'states) collect (true-p (member state value))))))
				   (setf (particle-type-open-state-array type) array)
				   (MAKE-MARKOV-ARRAYS type)))
			       (element-parameter type 'open-states))
		 (:evaluation-function (when value-supplied-p (setf (particle-type-evaluation-function type) (compile-or-extract-function-or-number value)))
				       (particle-type-evaluation-function type))
		 (:alpha (when value-supplied-p (setf (particle-type-alpha-function type) (compile-or-extract-function-or-number value)))
			 (particle-type-alpha-function type))
		 (:beta (when value-supplied-p (setf (particle-type-beta-function type) (compile-or-extract-function-or-number value)))
			(particle-type-beta-function type))
		 (:tau (when value-supplied-p (setf (particle-type-tau-function type) (compile-or-extract-function-or-number value)))
		       (particle-type-tau-function type))
		 (:ss (when value-supplied-p (setf (particle-type-ss-function type) (compile-or-extract-function-or-number value)))
		      (particle-type-ss-function type))))))
      (if (and value-supplied-p update)
	  (progn (update-particle-type type) (set-circuit-elements-parameters))
	  (setq *recheck-circuit-elements-parameters* t))
      return-value)))

(defun set-particle-type-evaluation (type evaluation-form)
  "Set the :EVALUATION-FUNCTION slot of particle TYPE to EVALUATION-FORM, and update the appropriate arrays. EVALULATION-FORM can be either a number,
representing a fixed time constant in milliseconds, or a function name or lambda form, either with a single voltage argument in
millivolts, that return a tau value in milliseconds."
  (let ((type (element type 'particle-type)))
    (when type
      (setf (particle-type-evaluation-function type) (compile-or-extract-function-or-number evaluation-form))
      (update-particle-type type))))

(defun set-particle-type-tau (type tau-form)
  "Set the :TAU-FUNCTION slot of particle TYPE to TAU-FORM, and update the appropriate arrays. TAU-FORM can be either a number,
representing a fixed time constant in milliseconds, or a function name or lambda form, either with a single voltage argument in
millivolts, that return a tau value in milliseconds."
  (let ((type (element type 'particle-type)))
    (when type
      (setf (particle-type-tau-function type) (compile-or-extract-function-or-number tau-form))
      (update-particle-type type))))

(defun set-particle-type-ss (type ss-form)
  "Set the :SS-FUNCTION slot of particle TYPE to SS-FORM, and update the appropriate arrays. SS-FORM can be either a number,
representing a fixed steady-state value between 0 and 1, or a function name or lambda form, either with a single voltage argument in
millivolts, that return a ss value between 0 and 1."
  (let ((type (element type 'particle-type)))
    (when type
      (setf (particle-type-ss-function type) (compile-or-extract-function-or-number ss-form))
      (update-particle-type type))))


(defun evaluate-particle-type-alpha-function (type voltage)
  (if (numberp (particle-type-alpha-function type))
    (particle-type-alpha-function type)
    (funcall (particle-type-alpha-function type) voltage)))

(defun evaluate-particle-type-beta-function (type voltage)
  (if (numberp (particle-type-beta-function type))
    (particle-type-beta-function type)
    (funcall (particle-type-beta-function type) voltage)))

(defun evaluate-particle-type-tau-function (type voltage)
  (if (numberp (particle-type-tau-function type))
    (particle-type-tau-function type)
    (funcall (particle-type-tau-function type) voltage)))

(defun evaluate-particle-type-ss-function (type voltage)
  (if (numberp (particle-type-ss-function type))
      (particle-type-ss-function type)
      (funcall (particle-type-ss-function type) voltage)))

(defun put-values-for-hh-tau-and-inf-array (type base-rate voltage arg-constant alpha_0 beta_0 tau_0 array-index q10-tau-factor tau_0_role)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type single-float arg-constant q10-tau-factor alpha_0 beta_0 voltage)
	   (type fixnum array-index))
  (when (and (not (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type))
	     (= base-rate 0))
    (sim-error (format nil "~A has IGNORE-Tau-VOLTAGE-DEPENDENCE nil and (= base-rate 0)" type)))
  (multiple-value-bind (alpha beta)
      (gating-particle-alpha-beta-values
       type (if (= base-rate 0) 1.0 base-rate) (particle-type-gamma type) (particle-type-v-half type) voltage
       :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0
       :tau_0_role tau_0_role
       :tau_0 (case (particle-type-class type)
		(:hh-ext tau_0)
		(:hh-ext-old 0.0))	; If limiting tau_0, consider it below.
       :always-calculate-rate t)
    ;; Do Q10 factor and tau limiting (if necessary) directly on the tau array.
    (let* ((base-tau (s-flt (if (particle-type-tau-function type)
				(evaluate-particle-type-tau-function type voltage)
				(/ 1.0 (+ (the sf alpha) (the sf beta))))))
	   (inf (s-flt (if (particle-type-ss-function type)
			   (evaluate-particle-type-ss-function type voltage)
			   (* alpha (if (particle-type-tau-function type)
					(/ 1.0 (+ (the sf alpha) (the sf beta)))
					base-tau))))))
      (declare (single-float alpha beta base-tau inf))
      (when (< inf 0) (sim-error (format nil "Particle type ~A has negative SS value @~AmV!" type voltage)))
      (setf (aref (the vec-flt (particle-type-tau-array type)) array-index)
	    (* q10-tau-factor
	       (if (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type)
		   tau_0
		   (case (particle-type-class type)
		     (:hh-ext-old (max tau_0 base-tau))
		     (:hh-ext base-tau)
		     (t 0.0)))))	; Dummy
      (setf (aref (the vec-flt (particle-type-inf-array type)) array-index) inf))))

(defun put-values-for-non-hh-tau-and-inf-array (type voltage array-index q10-tau-factor)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type single-float q10-tau-factor voltage)
	   (type fixnum array-index))
  ;; For non-HH-EXT class particle types, either the alpha and beta rate functions or the tau and steady-state functions must be
  ;; supplied as part of the type definitions.
  (let ((alpha 0.0) (beta 0.0) (tau 0.0) (ss 0.0)
	(alpha-beta-functions-p (and (particle-type-alpha-function type) (particle-type-beta-function type))))
;    (declare (type single-float alpha beta tau ss))
    (when alpha-beta-functions-p
      (setq alpha (evaluate-particle-type-alpha-function type voltage)
	    beta (evaluate-particle-type-beta-function type voltage)))
    (setq tau (if (particle-type-tau-function type)
		(evaluate-particle-type-tau-function type voltage)
		(/ (particle-type-tau-coefficient type) (+ alpha beta))))
    (setq ss (if (particle-type-ss-function type)
	       (evaluate-particle-type-ss-function type voltage)
	       (/ alpha (+ alpha beta))))
    (setf (aref (the vec-flt (particle-type-tau-array type)) array-index) (s-flt (* q10-tau-factor tau)))
    (setf (aref (the vec-flt (particle-type-inf-array type)) array-index) (rectify (s-flt ss)))))

(defun make-needed-v-particle-arrays (&optional all)
  (mapcar 'make-v-particle-arrays (if all (particle-types) (delete-duplicates *make-needed-v-particle-arrays*)))
  (setq *make-needed-v-particle-arrays* nil))

;; Make sure that there is no voltage in which we get a singular markov rate matrix (e.g. rates = 0)
(defvar *overall-minimum-particle-rate-constant* 0.0d0 ; 1.0d-5
  )

(defun v-function-array (function-or-form &key (min-voltage *particle-look-up-table-min-voltage*) (voltage-increment *particle-look-up-table-precision*)
			 (array-length *particle-look-up-table-length*))
  "Return a double-float array whose values are derived from evaluating FUNCTION-OR-FORM on a numeric sequence of ARRAY-LENGTH
values starting with MIN-VOLTAGE and incrementing by VOLTAGE-INCREMENT. FUNCTION-OR-FORM may be either a function with a single
argument, a list whose CAR is a function, whose CADR is a dummy variable that will be taken from the numeric sequence and the
remainder corresponding to other function arguments, or it can be a constant, which will be used to filled the returned
array. Default values of all arguments for V-FUNCTION-ARRAY reflect its usage in generating voltage-dependent particle type lookup
tables."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (fixnum array-length)
	   (single-float voltage-increment))
  (let* ((array (make-array array-length :element-type 'double-float))
	 (function-or-form-function-or-number (compile-or-extract-function-or-number function-or-form))
	 (funspec (typecase function-or-form-function-or-number
		    (function function-or-form-function-or-number)
		    (number nil)
		    (cons (extract-function-from-atom (car function-or-form-function-or-number))))))
    (loop for voltage single-float from min-voltage by voltage-increment
	  for array-index fixnum from 0 to (1- array-length) 
	  do (setf (aref array array-index)
		   (d-flt (typecase function-or-form-function-or-number
			    (function (funcall (the function funspec) voltage))
			    (number function-or-form-function-or-number) ; It's a constant 
			    (cons (apply funspec (cons voltage (cddr function-or-form-function-or-number))))))))

    array))

;; backward compatibility 5/12/99
(defun fill-v-function-array (function-or-form &key (min-voltage *particle-look-up-table-min-voltage*) (voltage-increment *particle-look-up-table-precision*)
					       (array-length *particle-look-up-table-length*))
  (v-function-array function-or-form :min-voltage min-voltage :voltage-increment voltage-increment :array-length array-length))

(defun particle-type-v-function-array (type function-or-form
					    &key (rate-p t) ; Otherwise, assume that this is for tau
					    (min-voltage *particle-look-up-table-min-voltage*)
					    (voltage-increment *particle-look-up-table-precision*)
					    (array-length *particle-look-up-table-length*))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((q10-factor (if type
		      (d-flt (if rate-p (element-q10-rate-factor type) (element-q10-tau-factor type)))
		      1.0d0))
	(array (v-function-array function-or-form :min-voltage min-voltage :voltage-increment voltage-increment	:array-length array-length)))
    (declare (type double-float q10-factor))
    ;; Now adjust for q-ten.
    (loop for array-index fixnum from 0 to (1- array-length) 
	  do (setf (aref array array-index)
		   (let ((val (the df (* q10-factor (aref array array-index)))))
		     (if rate-p
		       (max (the df *overall-minimum-particle-rate-constant*) val)
		       val))))
    array))

(defun plot-particle-type-rates (type &key concentration)
  (let ((type (element type 'particle-type)))
    (when type
      (case (particle-type-class type)
	(:markov (plot-markov-particle-type-rates type :separate-plots nil :use-menu t :concentration concentration))
	(t (plot-two-state-particle-type-rates type :separate-plots nil :user-menu t :concentration concentration))))))

(defun plot-particle-type-steady-states (prt-types-powers channel-type overlay new-plot)
  (loop for prt-type-power in prt-types-powers
	when (markov-p (car prt-type-power)) collect (car prt-type-power) into markov-prt-types
	else collect prt-type-power into non-markov-prt-types-powers
	finally 
	(loop for prt-type in markov-prt-types do (plot-markov-particle-type-steady-state prt-type))
	(loop for prt-type-power in non-markov-prt-types-powers
	      nconcing (loop for order from 1 to (cdr prt-type-power) by (max 1 (1- (cdr prt-type-power)))
			     collecting (v-inf-particle-plot-list (car prt-type-power) order))
	      into volts-inf
	      nconcing (loop for order from 1 to (cdr prt-type-power) by (max 1 (1- (cdr prt-type-power)))
			     collecting (if (> order 1)
					  (format nil "~A^~d" (element-name (car prt-type-power)) order)
					  (element-name (car prt-type-power))))
	      into labels
	      finally
	      (when volts-inf
		(plot-xy-data volts-inf labels
			      :title (if channel-type
				       (format nil "~a Channel Particles: Steady State(V)" (element-name channel-type))
				       (format nil "~a Particle Type: Steady State(V)" (element-name (caar prt-types-powers))))
			      :overlay (and overlay (not new-plot)) :prompt-for-overlay t :preserve-win-dims t
			      :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			      :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
			      :x-origin *particle-type-plot-minimum-voltage* 
			      :x-inc 50.0 :x-origin-tick t :x-label "mV"
			      :y-label-v-position :upper-right :x-are-fns t ; :include-y-tick-at-0 nil
			      :y-inc 0.25 :y-max 1.0 :y-min 0.0 :y-origin-tick t :y-label "SS"
			      :comment-position :upper-right
			      :comment (when *include-channel-type-comment-in-particle-plots* (channel-type-particle-plot-comment channel-type)))))))

(defun plot-particle-type-time-constants (prt-types-powers channel-type overlay new-plot)
  (loop for prt-type-power in prt-types-powers
	unless (markov-p (car prt-type-power))
	collect (v-tau-particle-plot-list (car prt-type-power)) into volts-tau and collect (element-name (car prt-type-power)) into labels
	finally
	(when volts-tau
	  (plot-xy-data volts-tau labels
			:title (if channel-type
				 (format nil "~a Channel Particles: Tau(V)" (element-name channel-type))
				 (format nil "~a Particle Type: Tau(V)" (element-name (caar prt-types-powers))))
			:preserve-win-dims t
			:width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*			
			:overlay (and overlay (not new-plot)) :prompt-for-overlay t
			:x-origin *particle-type-plot-minimum-voltage* 
			:x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
			:x-inc 50.0 :x-origin-tick t :x-label "mV"
			:x-are-fns t	; :include-y-tick-at-0 nil :reference-ticks-to-origin t					 
			:y-origin-tick t :y-label-v-position :upper-right :y-min 0.0 :y-label "ms"
			:comment-position :upper-right
			:comment (when *include-channel-type-comment-in-particle-plots* (channel-type-particle-plot-comment channel-type))))))

(defun plot-particle-types (prt-types-powers &key channel-type (what '(:steady_state :tau)) overlay new-plot
					     (log-conc-scale t) min-conc max-conc) ; These are for conc-dep Markov prts
  ;; Calls PLOT-V-MARKOV-PARTICLE
  (let ((what (coerce-to-list what))
	(channel-type (element channel-type 'channel-type))
	(prt-types-powers (if (consp prt-types-powers) prt-types-powers (list (cons prt-types-powers 1)))))
    (loop for prt-type-power in prt-types-powers
	  with prt-type with power 
	  do (setq prt-type (if (consp prt-type-power) (car prt-type-power) prt-type-power)
		   power (if (consp prt-type-power) (cdr prt-type-power) 1)) ; (format t "~A ~A ~a~%" prt-type-power prt-type power)
	  unless (markov-p prt-type) collect (cons prt-type power) into non-markov-prt-types-powers else collect prt-type into markov-prt-types
	  finally
	  (loop for prt-type in markov-prt-types
		do (plot-markov-particle-type prt-type :channel-type channel-type :what what :log-conc-scale log-conc-scale :min-conc min-conc :max-conc max-conc))
	  (cond-every
	   ((or (member :all what) (member :alpha_&_beta what))
	    (loop for prt-type-power in non-markov-prt-types-powers do (plot-particle-type-rates (car prt-type-power))))
	   ((or (member :all what) (member :steady_state what))
	    (plot-particle-type-steady-states non-markov-prt-types-powers channel-type overlay new-plot))
	   ((or (member :all what) (member :tau what))
	    (plot-particle-type-time-constants non-markov-prt-types-powers channel-type overlay new-plot))))))

#|
3/20/98 Strange problems all of a sudden (after misc file editing) - during compile:

In: DEFUN PLOT-TWO-STATE-PARTICLE-TYPE-RATES
  (GATING-PARTICLE-ALPHA-FUNCTION TYPE (PARTICLE-TYPE-BASE-RATE TYPE)
   (PARTICLE-TYPE-GAMMA TYPE) (PARTICLE-TYPE-V-HALF TYPE) ...)
--> BLOCK COND IF COND IF COND IF PROGN LET 
--> GATING-PARTICLE-ALPHA-PRIME-FUNCTION BLOCK * THE EXP-W-LIMITS IF * * * 
==>
  (/ (* Z 0.001 FOVERR) TEMPERATURE)
Warning: Lisp error during constant folding:


Error in function C::DO-CALL:
   Condition slot is not bound: CONDITIONS::OPERATION

Solved by local notinline of these functions. 
|#

(defun plot-two-state-particle-type-rates (type &key separate-plots user-menu concentration)
  (declare (ignore separate-plots user-menu concentration)
	   (notinline gating-particle-alpha-function gating-particle-beta-function))
  (let ((alpha_0 (or (element-parameter type 'alpha_0) 0.0))
	(beta_0 (or (element-parameter type 'beta_0) 0.0))
	(tau_0_role (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	(arg-constant (/ (* 1.0e-3 FoverR (particle-type-valence type))
			 (or (and (element-parameter type 'use-Fixed-boltzmann-reference-temp)
				  (+ 273.16 (element-parameter type 'Fixed-boltzmann-reference-temp)))
			     *Temperature*)))
	(tau_0 (particle-type-tau-0 type))
	(q10-rate-factor (element-q10-rate-factor type)))
    (loop for voltage from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
	  for i from 0 to (1- (length (particle-type-inf-array type)))
	  collecting voltage into volts
	  collecting
	  (* q10-rate-factor
	     (case (particle-type-class type)
	       ((:hh-ext-old :hh-ext)
		(gating-particle-alpha-function
		 voltage
		 :type type :base-rate (particle-type-base-rate type) :gamma (particle-type-gamma type) :v-half (particle-type-v-half type) 
		 :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0 :tau_0 tau_0 :tau_0_role tau_0_role))
	       (t (if (and (particle-type-alpha-function type) (particle-type-beta-function type))
		    (evaluate-particle-type-alpha-function type voltage)
		    (let ((tau (evaluate-particle-type-tau-function type voltage))
			  (ss (funcall (particle-type-ss-function type) voltage)))
		      (/ ss tau))))))
	  into alphas
	  collecting
	  (* q10-rate-factor
	     (case (particle-type-class type)
	       ((:hh-ext-old :hh-ext)
		(gating-particle-beta-function
		 voltage
		 :type type :base-rate (particle-type-base-rate type) :gamma (particle-type-gamma type) :v-half (particle-type-v-half type) 
		 :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0 :tau_0 tau_0 :tau_0_role tau_0_role))
	       (t (if (and (particle-type-alpha-function type) (particle-type-beta-function type))
		    (evaluate-particle-type-beta-function type voltage)
		    (let ((tau (evaluate-particle-type-tau-function type voltage))
			  (ss (funcall (particle-type-ss-function type) voltage)))
		      (/ (- 1 ss) tau))))))
	  into betas
	  finally
	  (plot-timed-data (list alphas betas) (list "Alpha" "Beta") volts
			   :title (format nil "~A Alpha and Beta" (element-name type))
			   :prompt-for-overlay t :preserve-win-dims t
			   :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			   :y-label-v-position :upper-right :x-are-fns t :include-y-tick-at-0 nil
			   :x-inc 50.0 :x-origin *particle-type-plot-minimum-voltage* :x-origin-tick t
			   :x-label "mV" :y-label "1/ms"
			   :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage* :y-min 0))))

(defun print-particle-state (prt)
  (format t "part: ~a, state-n ~f, state-n+1 ~f~%" (particle-name prt) (particle-state-n+1-double prt) (particle-state-n-double prt)))

(defun working-particles-p ()
  (and *enable-channels*
       (loop for prt being the hash-value of (particle-hash-table)
	     thereis (not (or (channel-type-blocked (channel-type (particle-channel prt)))
			      (channel-blocked (particle-channel prt)))))))
  
(defun update-particle-type (type)
  (update-particle-type-q10 type)
  (make-v-particle-arrays type))

(defun update-particle-type-q10 (type &optional value)
  "When the numeric VALUE is included, set the q10 of particle type TYPE to VALUE. Else, return current value. Do necessary updates."
  (let ((type (element type 'particle-type)))
    (when value (element-slot type :q10 (s-flt value)))
    (when type (setf (particle-type-q10-rate-factor type) (element-q10-rate-factor type)))))

(defun update-particle-type-q10s () (loop for type in (PARTICLE-TYPEs) do (update-particle-type-q10 type)))

(defun setup-particles ()
  (setq *particle-type-list*
	(delete-duplicates
	 (loop for ch-type in *channel-type-list*
	       when (CHANNEL-TYPE-ACTIVE-P ch-type)
	       nconc (loop for prt-type-power in (channel-type-particle-types-and-powers (element ch-type 'channel-type)) collect (car prt-type-power))
	       into prt-types
	       and
	       nconc (loop for prt-type-power in (channel-type-particle-types-and-powers (element ch-type 'channel-type))
			   collect (element-parameter (car prt-type-power) 'v-half-shift-particle-type))
	       into prt-types
	       finally (return (no-nils prt-types))))))
	
(defun remove-particle-type-arrays (type) (remove-element-parameters type '(active-particle-array active-particle-array-length)))

(defun advance-particles ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *particle-type-list* do
	(particle-type-iterator
	 (prt type)
	 unless (channel-blocked (particle-channel prt))
	 do
	 (setf (particle-state-n-double prt) (particle-state-n+1-double prt)
	       (particle-dsdt-n-1-double prt) (particle-dsdt-n-double prt))
	 (case (particle-type-class type)
	   (:markov (advance-markov-states prt)))))
  nil)
  
;; ******************* ******************* ******************* 
;;
;;                     Particle Evaluation
;;
;; ******************* ******************* *******************


(defmacro adapted-hines-1st-order-equation (tau inf previous-state)
  ;;
  ;; The key equation for solving the new particle state with variable time steps.
  ;;
  ;;  (*half-delta-t[n-1]*) = (the df (* 0.5d0 (the sf (* *mrt* *last-time-step*))))
  ;;  (*delta-forward*) = (the df (* 0.5d0 (the sf (* *mrt* *time-step*))))
  ;;
  ;;  (*sum-delta-for-back*) = (the df (+ (*delta-forward*) (*half-delta-t[n-1]*)))
  ;;  (*half-sum-delta-for-back*) = (the df (* 0.5 (+ (*delta-forward*) (*half-delta-t[n-1]*))))
  ;;
  `(/ (+ (* (*delta-t-prime[n]*) ,inf)
	 (* (- ,tau (*half-delta-t-prime[n]*)) ,previous-state))
      (+ ,tau (*half-delta-t-prime[n]*))))

;; Used in voltage particle evaluations
(proclaim '(inline adapted-hines-1st-order-equation-from-tau-inf))
(defun adapted-hines-1st-order-equation-from-tau-inf (tau inf previous-state initial-state prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type single-float tau inf)
	   (type double-float previous-state))
  (if initial-state
    (d-flt (or (get-a-value 'initial-state (particle-parameters prt))
	       (get-a-value 'initial-state (particle-type-parameters (particle-type prt)))
	       inf))
    (adapted-hines-1st-order-equation tau inf previous-state)))

(defun get-particle-initial-state-for-adapted-hines (prt inf)
  ;; Used in conc-part evaluations
  (d-flt (or (element-parameter prt 'initial-state)
	     (element-parameter (element-type prt) 'initial-state)
	     inf)))

(proclaim '(inline adapted-hines-1st-order-equation-from-double-tau-inf))
(defun adapted-hines-1st-order-equation-from-double-tau-inf (tau inf previous-state initial-state prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type double-float tau inf)
	   (type double-float previous-state))
  (if initial-state
    (the df (get-particle-initial-state-for-adapted-hines prt inf))
    (adapted-hines-1st-order-equation tau inf previous-state)))

(proclaim '(inline set-particle-aref-initial-states))
(defun set-particle-aref-initial-states (prt-df-array)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (setf (particle-aref-state-n prt-df-array) (particle-aref-state-n+1 prt-df-array)
	(particle-aref-dsdt-n prt-df-array) 0.0d0
	(particle-aref-dsdt-n-1 prt-df-array) 0.0d0)
  nil)

(proclaim '(inline calculate-particle-error))
(defun calculate-particle-error (prt-df-array name-or-prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setf (particle-aref-dsdt-n prt-df-array) (/ (- (particle-aref-state-n+1 prt-df-array) (particle-aref-state-n prt-df-array)) (*delta-t-prime[n]*)))
  (let ((d2sdt2-numerator (- (particle-aref-dsdt-n prt-df-array) (particle-aref-dsdt-n-1 prt-df-array))))
    (when (and (cond ((> d2sdt2-numerator (*maximum-particle-error-numerator*))
		      (setf (*maximum-particle-error-numerator*) d2sdt2-numerator))
		     ((> (the df (- d2sdt2-numerator)) (*maximum-particle-error-numerator*))
		      (setf (*maximum-particle-error-numerator*) (the df (- d2sdt2-numerator)))))
	       *debug-particle-error*)
      (setq *particle-w-max-error* (or (element-name name-or-prt) name-or-prt))))
  nil)

(proclaim '(inline finish-eval-particle-aref))
(defun finish-eval-particle-aref (prt-df-array initial-state &optional name-or-prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (cond (initial-state (set-particle-aref-initial-states prt-df-array))
	(*calculate-particle-error* (calculate-particle-error prt-df-array name-or-prt)))
  nil)

;; Never called
(proclaim '(inline finish-eval-markov-particle))
(defun finish-eval-markov-particle (prt initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for state-index from 0 to (nb-states-1 prt)
	do (finish-eval-particle-aref (prt-state prt state-index) initial-state
				      (when *debug-particle-error* (format nil "~A state ~A" (particle-name prt) state-index))))
  nil)

;; This is never called on Markov particles.
(proclaim '(inline finish-eval-particle))
(defun finish-eval-particle (prt initial-state &optional markov-p)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (finish-eval-particle-aref (particle-double-floats prt) initial-state (when *debug-particle-error* prt))
  (when markov-p (finish-eval-markov-particle prt initial-state))
  nil)
      
(proclaim '(inline v-half-shifted-particle-voltage-index))
(defun v-half-shifted-particle-voltage-index (prt v-half-shift-magnitude)
  (declare (optimize (safety 3) (speed 3) (space 0) (compilation-speed 0)))
  (let ((v-half-shift-particle
	 ;; stupid compiler ??
	 (the particle (or (get-a-value 'v-half-shift-particle (particle-parameters prt)) prt))))
    (max 0 (min (the fn (1- *particle-look-up-table-length*))
		(+ (the fn (particle-v-index prt))
		   (VOLTAGE-DOUBLE-TO-VOLTAGE-INDEX-RELATIVE (* (the sf v-half-shift-magnitude) (- (particle-state-n-double v-half-shift-particle) 0.5))))))))

(defun linear-markov-binomial (p n m)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type fixnum n m)
	   (type double-float p))
  (loop for i fixnum from m to n
	sum (* (the df (expt p i))
	       (the df (expt (- 1 p) (- N i)))
	       (the fn (choose n i)))
	into result double-float
	finally (return result)))

;; This is never called on Markov particles.
(proclaim '(notinline eval-two-state-particle-type))
(defun eval-two-state-particle-type (prt-type initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((tau-array (the vec-flt (particle-type-tau-array prt-type)))
	 (inf-array (the vec-flt (particle-type-inf-array prt-type)))
	 (params (particle-type-parameters prt-type))
	 (v-half-shift-particle-type nil) ; (get-a-value 'v-half-shift-particle-type params)
	 (v-half-shift-magnitude (when v-half-shift-particle-type (get-a-value 'v-half-shift-magnitude params)))
	 (linear-markov-n (get-a-value 'linear-markov-n params))
	 (linear-markov-m (get-a-value 'linear-markov-m params)))
    (particle-type-iterator
     (prt prt-type)
     unless (channel-blocked (particle-channel prt)) do
     (let* ((voltage-index (if v-half-shift-particle-type (v-half-shifted-particle-voltage-index prt v-half-shift-magnitude) (particle-v-index prt)))
	       (tau (aref tau-array voltage-index))
	       (inf (aref inf-array voltage-index)))
	  (declare (type particle prt)
		   (single-float tau inf)
		   (fixnum voltage-index))
	  (when *interpolate-particle-arrays*
	    (let ((remainder (particle-v-rem prt))
		  (1+voltage-index (1+ voltage-index)))
	      (unless (= 1+voltage-index *particle-look-up-table-length*)
		(setf tau (+ (* (- 1.0 remainder) tau) (* remainder (aref tau-array 1+voltage-index)))
		      inf (+ (* (- 1.0 remainder) inf) (* remainder (aref inf-array 1+voltage-index)))))))
	  ;; Now solve for the particle state.
	  (setf (particle-state-n+1-double prt) (adapted-hines-1st-order-equation-from-tau-inf tau inf (particle-state-n-double prt) initial-state prt))
	  (when linear-markov-n
	    (setf (particle-state-n+1-double prt) (linear-markov-binomial (particle-state-n+1-double prt) linear-markov-n linear-markov-m)))
	  (when *debug-eval-particle (format t " eval-prt ~A state-n+1 ~f vindex ~a~%" prt (particle-state-n+1-double prt) voltage-index))
	  (finish-eval-particle prt initial-state))))
  nil)

(defun eval-all-particles (&optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *particle-type-list* do
	(if (particle-type-evaluation-function type)
	  (funcall (particle-type-evaluation-function type) type initial-state)
	  (case (particle-type-class type)
	    (:markov (eval-markov-particle-type type initial-state))
	    ((:hh :hh-ext :HH-EXT-OLD) (eval-two-state-particle-type type initial-state))))))

(defun init-particles () (eval-all-particles t))


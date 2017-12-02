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


;;; SYS Source file: conc-part.lisp

; The concentration dependent gating particle model.

(in-package "SURF-HIPPO")

(defun number-CONC-PARTICLE-TYPE-CONC-PARTICLES (type)
  (let ((type (element type 'conc-particle-type)))
    (when type
      (loop with prt = (conc-particle-type-first-element type)
	    while prt sum 1 into total
	    do (setq prt (conc-particle-next-element prt))
	    finally (return total)))))
  
(defun CONC-PARTICLE-TYPE-CONC-PARTICLES (type)
  (let* ((type (element type 'conc-particle-type)))
    (when type
      (loop with prt = (conc-particle-type-first-element type)
	    while prt collect prt into prts
	    do (setq prt (conc-particle-next-element prt))
	    finally (return prts)))))

(defun CONC-PARTICLE-TYPE-PARTICLES (type) (CONC-PARTICLE-TYPE-CONC-PARTICLES type))

(defun last-conc-particle (prt)
  (if (and prt (conc-particle-next-element prt))
    (last-conc-particle (conc-particle-next-element prt))
    prt))

(defun last-conc-particle-of-type (type) (last-conc-particle (conc-particle-type-first-element type)))

(defun clear-conc-particles-of-type (type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (conc-particle-type-first-element type) nil)
  (loop for prt being the hash-value of (CONC-PARTICLE-HASH-TABLE)
	when (eq type (conc-particle-type prt)) do (setf (conc-particle-next-element prt) nil)))

(defun get-conc-particles-of-type (type)
  (loop for prt being the hash-value of (CONC-PARTICLE-HASH-TABLE)
	when (eq type (conc-particle-type prt)) collect prt))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-for-conc-particle-types (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (loop for prt-type-and-power in (channel-type-conc-particle-types-and-powers channel-type) do
	  (edit-conc-particle-type (car prt-type-and-power) channel-type (cadr prt-type-and-power)))))

(defun edit-conc-particle-type (type &optional channel-type number-particles-per-channel)
  (when (element type 'conc-particle-type)
    (let ((dummy13 t)(dummy11 number-particles-per-channel))
      (loop while dummy13 do
	    (let* ((prt-type (element type 'conc-particle-type))
		   (nthorder-p (eq :nth-order (conc-particle-type-class prt-type)))
		   (dummy1 (conc-particle-type-alpha prt-type))
		   (dummy2 (conc-particle-type-beta prt-type))
		   (dummy3 (conc-particle-type-conc-power prt-type))
		   (dummy4 (conc-particle-type-tau-0 prt-type))
		   dummy5 
		   (dummy6 (conc-particle-type-q10 prt-type))
		   (dummy7 (conc-particle-type-reference-temp prt-type))
		   (dummy9 (conc-particle-type-name prt-type))
		   dummy10 
		   dummy12
		   (dummy14 (conc-particle-type-base-concentration prt-type))
		   (dummy15 (conc-particle-type-concentration-coefficient prt-type)))
	      (choose-variable-values
	       `((dummy9 ,(format nil "Edit name of type (used if saved to file):") :string)
		 ,(when nthorder-p `(dummy1 "Alpha [1/ms*(mM^x)] - x is exponent below" :float))
		 ,(when nthorder-p `(dummy2 "Beta [1/ms]" :float))
		 ,(when nthorder-p `(dummy14 "Base concentration [mM]" :float))
		 (dummy15 "Concentration coefficient" :float)
		 (dummy3 "Concentration exponent x" :integer)
		 (dummy4 "Minimum time constant [ms]" :float)
		 ,(unless channel-type
		    '(dummy12 "Plot concentration-dependent particles:" :x-choose (:steady_state :tau)))
		 ,(when (and number-particles-per-channel channel-type)
		    '(dummy11 "Number of particles per channel" :integer))
		 (dummy6 "Q10" :float)
		 (dummy7 "Kinetics Reference Temperature [degs C]" :float)
		 ,(case (conc-particle-type-class prt-type)
		    (:ml `(dummy10 "Edit other parameters" :boolean))))	 
	       :text (ADD-LINEFEEDS-TO-STRING-LIST
		      (list (case (conc-particle-type-class prt-type)
			      (:ml "Derived from Moczydlowski and Latorre 1983")
			      (t (format nil "Concentration Particle Type Class: ~A" (conc-particle-type-class prt-type))))
			    (ELEMENT-SOURCEFILE-STRING type nil)))
	       :label (format nil "Parameters Of Conc-Particle Type ~A" (conc-particle-type-name prt-type)))
	      (unless (string= (format nil "~A" (conc-particle-type-name type)) dummy9)
		(SET-ELEMENT-NAME prt-type dummy9))
	      (let ((update-q10 (or (not (= dummy6 (conc-particle-type-q10 prt-type)))
				    (not (= dummy7 (conc-particle-type-reference-temp prt-type))))))
		(setf (conc-particle-type-concentration-coefficient prt-type) dummy15
		      (conc-particle-type-base-concentration prt-type) dummy14
		      (conc-particle-type-alpha prt-type) dummy1
		      (conc-particle-type-beta prt-type) dummy2
		      (conc-particle-type-tau-0 prt-type) (d-flt dummy4)
		      (conc-particle-type-conc-power prt-type) dummy3
		      (conc-particle-type-q10 prt-type) dummy6
		      (conc-particle-type-reference-temp prt-type) dummy7)
		(when dummy10 (case (conc-particle-type-class prt-type)
				(:ml (edit-ml-conc-particle-type-details prt-type))))
		(when update-q10 (update-conc-particle-type-q10 type)))
	      (make-conc-particle-arrays prt-type)
	      (when dummy12 (plot-conc-particle prt-type :what dummy12))
	      (setq dummy13 dummy12)))
      (values type dummy11))))

(defun edit-ml-conc-particle-type-details (type)
  (let ((dummy1 (element-parameter type 'k1-0))
	(dummy2 (element-parameter type 'k4-0))
	(dummy3 (element-parameter type 'delta-1))
	(dummy4 (element-parameter type 'delta-4))
	(dummy5 (or (element-parameter type 'valence) 2))
	(dummy6 (conc-particle-type-beta type))
	(dummy7 (conc-particle-type-alpha type)))
    (choose-variable-values
     '((dummy5 "Valence" :integer)
       ("Forward Reaction Parameters:" :comment)
       (dummy1 "K1(0) [mM]" :float)
       (dummy3 "Delta-1" :float)
       (dummy6 "Beta_0 [1/ms]" :float)
       ("Backward Reaction Parameters:" :comment)
       (dummy2 "K4(0) [mM[" :float)
       (dummy4 "Delta-4" :float)
       (dummy7 "Alpha_0 [1/ms]" :float))
     :text "Editing parameters derived from Moczydlowski and Latorre 1983"
     :label (format nil "ML Parameters Of Conc-Particle Type ~A"  (conc-particle-type-name type)))
    (element-parameter type 'k1-0 dummy1)
    (element-parameter type 'k4-0 dummy2)
    (element-parameter type 'delta-1 dummy3)
    (element-parameter type 'delta-4 dummy4)
    (element-parameter type 'valence dummy5)
    (setf (conc-particle-type-beta type) dummy6
	  (conc-particle-type-alpha type) dummy7)))

(defun document-conc-particle-type (type-name-or-type)
  (let ((type-name (element-name type-name-or-type 'conc-particle-type))
	(type (element type-name-or-type 'conc-particle-type)))
    (when type
      (format t "(conc-particle-type-def~%")
      (format t " (~a~%" type-name)
      (format t "  (class . ~s)~%" (conc-particle-type-class type))
      (format t "  (alpha . ~a)~%" (conc-particle-type-alpha type))
      (format t "  (beta . ~a)~%" (conc-particle-type-beta type))
      (format t "  (tau-0 . ~a)~%" (conc-particle-type-tau-0 type))
      (format t "  (power . ~a)~%" (conc-particle-type-conc-power type))
      (format t "  (q10 . ~a)~%" (conc-particle-type-q10 type))
      (format t "  (reference-temp . ~a)~%" (conc-particle-type-reference-temp type))
      (format t "  (shell . ~a)~%" (conc-particle-type-shell type))
      (format t "  (conc-int-type . ~a)~%" (conc-int-type-name (conc-particle-type-conc-int-type type)))
      (cond-every
       ((not (= (conc-particle-type-concentration-coefficient type) 1))
	(format t "  (concentration-coefficient . ~a)~%" (conc-particle-type-concentration-coefficient type)))
       ((not (= (conc-particle-type-base-concentration type) 0))
	(format t "  (base-concentration . ~a)~%" (conc-particle-type-base-concentration type))))
      ;; For :ml particles
      (case (conc-particle-type-class type)
	(:ML
	 (cond-every
	  ((element-parameter type 'valence)
	   (format t "  (valence . ~a)~%" (element-parameter type 'valence)))
	  ((element-parameter type 'delta-1)
	   (format t "  (delta-1 . ~a)~%" (element-parameter type 'delta-1)))
	  ((element-parameter type 'delta-4)
	   (format t "  (delta-4 . ~a)~%" (element-parameter type 'delta-4)))
	  ((element-parameter type 'K1-0)
	   (format t "  (k1-0 . ~a)~%" (element-parameter type 'K1-0)))
	  ((element-parameter type 'K4-0)
	   (format t "  (k4-0 . ~a)~%" (element-parameter type 'K4-0))))))

      (cond-every
       ((conc-particle-type-alpha-function type)
	(format t "  (alpha-function . ~a)~%" (conc-particle-type-alpha-function type)))
       ((conc-particle-type-beta-function type)
	(format t "  (beta-function . ~a)~%" (conc-particle-type-beta-function type)))
       ((conc-particle-type-tau-function type)
	(format t "  (tau-function . ~a)~%" (conc-particle-type-tau-function type)))
       ((conc-particle-type-ss-function type)
	(format t "  (ss-function . ~a)~%" (conc-particle-type-ss-function type)))
       ((conc-particle-type-conc-dependence type)
	(format t "  (conc-dependence . ~a)~%" (conc-particle-type-conc-dependence type))))
      (print-element-document-extras type)
      (format t "     ))~%~%"))))

(defun print-conc-particle-types () (PRINT-MODEL-PARAMETERS "conc-particle-type"))
(defun print-conc-particles () (PRINT-MODEL-PARAMETERS "conc-particle"))

(defun print-conc-particle (prt)
  (let ((prt (element prt 'conc-particle)))
    (when prt
      (format t (concatenate-strings
		 (format nil "~a: type ~A, cell ~a, channel ~A~A~%"
			 (massage-element-plot-label prt nil t)
			 (conc-particle-type-name (conc-particle-type prt))
			 (cell-name (node-cell (conc-particle-cnode-point prt)))
			 (channel-name (conc-particle-channel prt)) 
			 (if (and *simulation-initialized* (not (element-parameter prt 'is-sub-particle-of)))
			   (format nil ", State ~,2e @ ~,2e ms" (s-flt (conc-particle-state-n+1-double prt)) *real-time*) ""))))
      (when (element-parameter prt 'is-sub-particle-of)
	(format t "   This is a sub-particle of particle ~A.~%" (element-name (element-parameter prt 'is-sub-particle-of)))))))

(defun conc-particles-of-type (type)
  (let ((type (element type 'conc-particle-type)))
    (loop for prt being the hash-value of (CONC-PARTICLE-HASH-TABLE)
	  when (equal (conc-particle-type prt) type) collect prt)))

(defun print-conc-particle-type (type &optional (always t))
  (let ((type (element type 'conc-particle-type)))
    (when (and type (or always (conc-particles-of-type type)))
      (format t "Conc-Particle-type ~a (class ~s):~%"
	      (conc-particle-type-name type)  (conc-particle-type-class type))
      (format t "   Conc-Int-type ~a," (conc-int-type-name (conc-particle-type-conc-int-type type)))
      (if (< (conc-particle-type-shell type) 0)
	(format t " concentration taken over entire element~%")
	(format t " shell ~a~%" (conc-particle-type-shell type)))
      (case (conc-particle-type-class type)
	(:static
	 (format t "   Static concentration dependence~%"))
	(:static-linear
	 (format t "   Static concentration dependence, slope ~,2e, minimum ~,2e~%"
		 (element-parameter type `concentration-slope)
		 (element-parameter type `concentration-min)))
	(:nth-order
	 (format t "   alpha ~,2e (conc-power ~a)~A, beta ~,2e, Q10 ~a, T_ref ~a, tau_0 ~,2e ms~%"
		 (conc-particle-type-alpha type)
		 (conc-particle-type-conc-power type)
		 (if (= (conc-particle-type-base-concentration type) 0)
		   ""
		   (format nil ", base concentration ~,2emM" (conc-particle-type-base-concentration type)))
		 (conc-particle-type-beta type)
		 (tidy-number-format (conc-particle-type-Q10 type))
		 (tidy-number-format (conc-particle-type-reference-temp type))
		 (conc-particle-type-tau-0 type)))
	(:ml
	 (format t "   ML model params:~%")
	 (format t "      Forward rate constant: Beta-0 ~,2e [1/ms], K1(0) ~,2emM, Delta-1 ~,2e Valence ~A~%"
		 (conc-particle-type-beta type)
		 (element-parameter type 'K1-0)
		 (element-parameter type 'delta-1)
		 (or (element-parameter type 'valence) 2))
	 (format t "      Backward rate constant: Alpha-0 ~A [1/ms], K4(0) ~AmM, Delta-4 ~A~%"
		 (conc-particle-type-alpha type)
		 (element-parameter type 'K4-0)
		 (element-parameter type 'delta-4)))
	(t
	 (format t "   Explicit alpha and beta functions (conc-power ~a), Q10 ~a, T_ref ~a, tau_0 ~,2e ms~%"
		 (conc-particle-type-conc-power type)
		 (tidy-number-format (conc-particle-type-Q10 type))
		 (tidy-number-format (conc-particle-type-reference-temp type))
		 (conc-particle-type-tau-0 type))))
      (unless (= 1 (conc-particle-type-concentration-coefficient type))
	(format t "   Concentration coefficient ~a~%" (conc-particle-type-concentration-coefficient type)))
      (print-num-elements-sourcefile type t t))))

(defvar *conc-particle-tau-0* 0.1)	;ms

(defun create-conc-particle-type (type-symbol &optional actual-type-symbol)
  (let* ((type (unless actual-type-symbol
		 (if (conc-particle-type-p type-symbol)
		   type-symbol
		   (CONC-PARTICLE-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (conc-particle-type-p type-symbol) (conc-particle-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'conc-particle-type))
	 (library-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (type-parameters nil)
	 (parent-type-symbol (get-a-value 'parent-type library-parameters)))
    (when (eq parent-type-symbol type-symbol)
      (sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
    (unless library-parameters (sim-error (format nil "Don't know anything about conc particle type ~A!" type-symbol)))
    (when actual-type-symbol (setq type-symbol actual-type-symbol))
    (unless type (setq type (if parent-type-symbol
		     (create-CONC-PARTICLE-TYPE parent-type-symbol type-symbol)
		     (make-CONC-PARTICLE-TYPE :name type-symbol))))
    (update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type)
    (setq type-parameters (update-element-parameters-with-new-parameters library-parameters type))    
    (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
    (setf (conc-particle-type-class type) (or (get-a-value 'class type-parameters)
					      (conc-particle-type-class type)
					      :NTH-ORDER))
    (case (conc-particle-type-class type)
      (:markov (set-markov-particle-type-parameters type type-parameters)))
    (cond-every
     ((assoc 'evaluation-function type-parameters)
      (setf (conc-particle-type-evaluation-function type) (compile-or-extract-function-or-number (get-a-value 'evaluation-function type-parameters))))
     ((assoc 'shell type-parameters)
      (setf (conc-particle-type-shell type) (fix (get-a-value 'shell type-parameters))))
     ((assoc 'power type-parameters)
      (setf (conc-particle-type-conc-power type) (fix (get-a-value 'power type-parameters))))
     ((or (assoc 'alpha type-parameters) (assoc 'alpha-function type-parameters))
      (setf (conc-particle-type-alpha-function type)
	    (compile-or-extract-function-or-number (or (get-a-value 'alpha type-parameters) (get-a-value 'alpha-function type-parameters)))))
     ((or (assoc 'beta type-parameters) (assoc 'beta-function type-parameters))
      (setf (conc-particle-type-beta-function type)
	    (compile-or-extract-function-or-number (or (get-a-value 'beta type-parameters) (get-a-value 'beta-function type-parameters)))))
     ((or (assoc 'ss type-parameters) (assoc 'ss-function type-parameters))
      (setf (conc-particle-type-ss-function type)
	    (compile-or-extract-function-or-number (or (get-a-value 'ss type-parameters) (get-a-value 'ss-function type-parameters)))))
     ((or (assoc 'tau type-parameters) (assoc 'tau-function type-parameters))
      (setf (conc-particle-type-tau-function type)
	    (compile-or-extract-function-or-number (or (get-a-value 'tau type-parameters) (get-a-value 'tau-function type-parameters)))))
     ((assoc 'conc-dependence type-parameters)
      (setf (conc-particle-type-conc-dependence type) (get-a-value 'conc-dependence type-parameters)))
     ((or (assoc 'alpha-0 type-parameters) (assoc 'alpha type-parameters))		  
      (setf (conc-particle-type-alpha type) (s-flt (or (get-a-value 'alpha-0 type-parameters) (get-a-value 'alpha type-parameters)))))
     ((or (assoc 'beta-0 type-parameters) (assoc 'beta type-parameters))
      (setf (conc-particle-type-beta type) (s-flt (or (get-a-value 'beta-0 type-parameters) (get-a-value 'beta type-parameters)))))
     ((assoc 'concentration-coefficient type-parameters)
      (setf (conc-particle-type-concentration-coefficient type) (s-flt (get-a-value 'concentration-coefficient type-parameters))))
     ((assoc 'base-concentration type-parameters)
      (setf (conc-particle-type-base-concentration type) (s-flt (get-a-value 'base-concentration type-parameters))))
     ((assoc 'valence type-parameters)
      (element-parameter type 'valence (get-a-value 'valence type-parameters)))
     ((assoc 'delta-1 type-parameters)
      (element-parameter type 'delta-1 (get-a-value 'delta-1 type-parameters)))
     ((assoc 'delta-4 type-parameters)
      (element-parameter type 'delta-4 (get-a-value 'delta-4 type-parameters)))
     ((assoc 'k1-0 type-parameters)
      (element-parameter type 'k1-0 (get-a-value 'k1-0 type-parameters)))
     ((assoc 'k4-0 type-parameters)
      (element-parameter type 'k4-0 (get-a-value 'k4-0 type-parameters)))
     ((assoc 'concentration-min type-parameters)
      (element-parameter type 'concentration-min (d-flt (get-a-value 'concentration-min type-parameters))))
     ((assoc 'concentration-slope type-parameters)
      (element-parameter type 'concentration-slope (d-flt (get-a-value 'concentration-slope type-parameters))))
     ((or (assoc 'qten type-parameters) (assoc 'q10 type-parameters))
      (setf (conc-particle-type-q10 type) (s-flt (or (get-a-value 'qten type-parameters) (get-a-value 'q10 type-parameters)))))
     ((assoc 'reference-temp type-parameters)
      (setf (conc-particle-type-reference-temp type) (get-a-sf-value 'reference-temp type-parameters))))
    (case (conc-particle-type-class type)
      ((:nth-order :static :static-linear))
      (t (element-parameter type 'voltage-dependent t)))
    (unless (get-a-value 'conc-int-type type-parameters)
      (sim-error (format nil "Conc particle type ~A is missing a conc int!" type-symbol)))
    (setf (conc-particle-type-tau-0 type) (d-flt (or (get-a-value 'tau-0 type-parameters)
						     (and (> (conc-particle-type-tau-0 type) 0) (conc-particle-type-tau-0 type))
						     *conc-particle-tau-0*))
	  (conc-particle-type-conc-int-type type) (create-conc-int-type (get-a-value 'conc-int-type type-parameters)))
    (setf (CONC-PARTICLE-TYPE-HASH-TABLE type-symbol) type)
    (make-conc-particle-arrays type)
    (setq *conc-particle-type* type)
    type))

(defun make-needed-conc-particle-arrays (&optional all)
  (mapcar 'make-conc-particle-arrays (if all (CONC-PARTICLE-TYPES) (delete-duplicates *make-needed-conc-particle-arrays*)))
  (setq *make-needed-conc-particle-arrays* nil))

(defun make-conc-particle-arrays (type)	; ng
  (declare (optimize (safety 1) (speed 3) (space 0)) (type conc-particle-type type))
  (case (conc-particle-type-class type)
    (:static)
    (:nth-order)
    (:none)
    (:ml
     (let* ((delta-1 (s-flt (element-parameter type 'delta-1)))
	    (delta-4 (s-flt (element-parameter type 'delta-4)))
	    (k1-0 (s-flt (element-parameter type 'k1-0)))
	    (k4-0 (s-flt (element-parameter type 'k4-0)))
	    (z (coerce (or (element-parameter type 'valence) 2) 'fixnum))
	    ;; This just saves crunching in the loop below. The factor of 1.0e-3 is because the voltage units below are millivolts.
	    (arg-constant-1 (the sf (/ (* delta-1 -1.0e-3 FoverR z) *Temperature*)))
	    (arg-constant-4 (the sf (/ (* delta-4 -1.0e-3 FoverR z) *Temperature*))))
       ;; Now loop over the voltage range.
       (let ((a1 (make-array (list *particle-look-up-table-length*) :element-type 'single-float))
	     (a2 (make-array (list *particle-look-up-table-length*) :element-type 'single-float)))
	 (loop for voltage single-float from *particle-look-up-table-min-voltage*
	       by (the sf *particle-look-up-table-precision*)
	       for count fixnum from 0 to (1- *particle-look-up-table-length*)
	       do (setf (aref a1 count) (* k1-0 (the sf (exp-w-limits (* voltage arg-constant-1))))
			(aref a2 count) (* k4-0 (the sf (exp-w-limits (* voltage arg-constant-4)))))	       
	       finally (setf (conc-particle-type-k1 type) a1
			     (conc-particle-type-k4 type) a2)))))))
	     
(defun working-conc-particles-p ()
  (and *enable-channels*
       (loop for prt being the hash-value of (conc-particle-hash-table)
	     unless (or (channel-type-blocked (channel-type (conc-particle-channel prt)))
			(channel-blocked (conc-particle-channel prt)))
	     do (return t))))

(defun get-conc-particle-simple-name ()
  (loop for candidate from (max 1 *conc-particle-simple-name-counter*)
	until (not (conc-particle-hash-table candidate))
	finally (return (setf *conc-particle-simple-name-counter* candidate))))

(defun rename-conc-particles-simple (&optional (conc-particles (conc-particles)))
  "Rename CONC-PARTICLES [default all conc-particles in circuit] with simple integer names."
  (loop for seg in conc-particles do
	(let ((name (get-conc-particle-simple-name)))
	  (set-element-name seg name 'conc-particle))))

(defun create-conc-particle (channel type &optional dummy-particle)
  (let* ((type (create-conc-particle-type type))
	 (type-parameters (conc-particle-type-parameters type))
	 (conc-particle-name (if dummy-particle (format nil "~A" (gensym))
				 (if *use-simple-names*
				   (get-conc-particle-simple-name)
				   (format nil "~a-~a" (channel-name channel) (conc-particle-type-name type)))))
	 (nd (unless dummy-particle (or (channel-pre-synaptic-node channel) (channel-node channel))))
	 (cprt (make-conc-particle :name conc-particle-name :type type :channel channel
				   :conc-int (unless dummy-particle (create-conc-int (channel-cell-element channel) (conc-particle-type-conc-int-type type))))))
    (setf (CONC-PARTICLE-HASH-TABLE conc-particle-name) cprt)
    (unless dummy-particle
      (setf (conc-int-evaluate-total-concentration (conc-particle-conc-int cprt)) (< (conc-particle-type-shell type) 0))
      (setf (node-has-v-dep-element nd) (or (node-has-v-dep-element nd) (element-parameter type 'voltage-dependent)))
      (push cprt (node-elements nd))
      (element-slot-fast (element-slot-fast type :last-element) :next-element cprt)
      (element-slot-fast type :last-element cprt)
      (setf (conc-particle-cnode-point cprt)
	    (or (channel-pre-synaptic-node (conc-particle-channel cprt))
		(cell-element-physical-node-fast (conc-particle-cell-element cprt)))))
    (setq *conc-particle* cprt)
    cprt))

(proclaim '(inline conc-particle-voltage))
(defun conc-particle-voltage (prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let* ((conc-int (conc-particle-conc-int prt))
	 (cell-element (conc-int-cell-element conc-int)))
    (node-particle-voltage (typecase cell-element
			     (segment (segment-node-2 cell-element))
			     (t (soma-node cell-element))))))

(proclaim '(inline conc-particle-voltage-index))
(defun conc-particle-voltage-index (prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let* ((conc-int (conc-particle-conc-int prt))
	 (cell-element (conc-int-cell-element conc-int)))
    (node-prt-v-index (typecase cell-element
			(segment (segment-node-2 cell-element))
			(t (soma-node cell-element))))))

(proclaim '(inline conc-particle-concentration))
(defun conc-particle-concentration (prt &optional concentration)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (typecase concentration
    (double-float concentration)
    (single-float (d-flt concentration))
    (t
     (let ((conc-int (conc-particle-conc-int prt)))
       (if conc-int
	 (case (conc-particle-type-shell (conc-particle-type prt))
	   (1 (conc-int-shell-1-free-conc-n conc-int))
	   (2 (conc-int-shell-2-free-conc-n conc-int))
	   (-1 (total-free-concentration-n conc-int))
	   (t (d-flt (or (element-parameter prt 'concentration) 0.0d0))))
	 (d-flt (or (element-parameter prt 'concentration)
		    0.0d0)))))))

(proclaim '(inline conc-particle-concentration-arg))
;; Subtract CONCENTRATION by the :BASE-CONCENTRATION slot of TYPE, multiply shifted value by :CONCENTRATION-COEFFICIENT slot of
;; TYPE, and then raise result by exponent given by :CONC-POWER slot of TYPE. Returns double-float.
(defun conc-particle-concentration-arg (concentration type)
  "All concentration dependent particles [conc-particles] are evaluated with the actual CONCENTRATION [mM] passed through this
function. The returned value is given by

  [CONCENTRATION-COEFFICIENT * Th(CONCENTRATION - BASE-CONCENTRATION)]^CONC-POWER

where CONCENTRATION-COEFFICIENT, BASE-CONCENTRATION, and CONC-POWER are given by the corresponding slots of the concentration
particle TYPE. `Th' is the rectifying function."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (double-float concentration))
  (if (< concentration (conc-particle-type-base-concentration type))
    0.0d0
    (let ((shifted-conc (- concentration (conc-particle-type-base-concentration type)) ))
      (ch-power-double-macro
       (if (= 1.0 (conc-particle-type-concentration-coefficient type))
	 shifted-conc
	 (* (conc-particle-type-concentration-coefficient type) shifted-conc)) 
       (conc-particle-type-conc-power type)))))

(proclaim '(inline nthorder-conc-particle-forward-rate nthorder-conc-particle-backward-rate))
(defun nthorder-conc-particle-forward-rate (conc-part)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((conc-particle-type (conc-particle-type conc-part))
	 (conc-arg (conc-particle-concentration-arg (conc-particle-concentration conc-part) conc-particle-type))) ; df
    (* (conc-particle-type-alpha conc-particle-type) ; sf
       (conc-particle-type-q10-rate-factor conc-particle-type) ; sf
       conc-arg)))

(defun nthorder-conc-particle-backward-rate (conc-part)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((conc-particle-type (conc-particle-type conc-part)))
    (* 1.0d0				; so that a double float value is returned.
       (conc-particle-type-beta conc-particle-type) ; sf
       (conc-particle-type-q10-rate-factor conc-particle-type) ; sf
       )))

(proclaim '(inline ml-conc-particle-forward-rate))
(defun ml-conc-particle-forward-rate (type conc-arg voltage-index)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (fixnum voltage-index)
	   (double-float conc-arg))
  (/ (conc-particle-type-beta type)
     (+ 1.0 (/ (aref (the vec-flt (conc-particle-type-k1 type)) voltage-index) conc-arg))))

(proclaim '(inline ml-conc-particle-backward-rate))
(defun ml-conc-particle-backward-rate (type conc-arg voltage-index)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (fixnum voltage-index)
	   (double-float conc-arg))
  (/ (conc-particle-type-alpha type)
     (+ 1.0 (/ conc-arg (aref (the vec-flt (conc-particle-type-k4 type)) voltage-index)))))

(defvar *MAX-PLOTTING-CONC* 5)

(defun evaluate-conc-particle-type-alpha-function (type conc-arg voltage)
  (if (numberp (conc-particle-type-alpha-function type))
    (conc-particle-type-alpha-function type)
    (funcall (conc-particle-type-alpha-function type) conc-arg voltage)))

(defun evaluate-conc-particle-type-beta-function (type conc-arg voltage)
  (if (numberp (conc-particle-type-beta-function type))
    (conc-particle-type-beta-function type)
    (funcall (conc-particle-type-beta-function type) conc-arg voltage)))

(defun evaluate-conc-particle-type-tau-function (type conc-arg voltage)
  (if (numberp (conc-particle-type-tau-function type))
    (conc-particle-type-tau-function type)
    (funcall (conc-particle-type-tau-function type) conc-arg voltage)))

(defun evaluate-conc-particle-type-ss-function (type conc-arg voltage)
  (if (numberp (conc-particle-type-ss-function type))
    (conc-particle-type-ss-function type)
    (funcall (conc-particle-type-ss-function type) conc-arg voltage)))

(defun conc-particle-type-ss (type concentration &key (power 1) (voltage -70.0))
  (let* ((conc-arg (conc-particle-concentration-arg (d-flt concentration) type))
	 (voltage (d-flt voltage))
	 (voltage-index (voltage-to-voltage-index voltage))
	 (arg (case (conc-particle-type-class type)
		(:static-linear (static-linear-conc-particle-state conc-arg
								   (element-parameter type 'concentration-min)
								   (element-parameter type 'concentration-slope)))
		(:static (funcall (the compiled-function (conc-particle-type-conc-dependence type)) (d-flt conc-arg)))
		(:nth-order (let ((a (* (conc-particle-type-alpha type) conc-arg))
				  (b (conc-particle-type-beta type)))
			      (/ a (+ a b))))
		(:ml (let ((beta (ml-conc-particle-forward-rate type (d-flt conc-arg) voltage-index))
			   (alpha (ml-conc-particle-backward-rate type (d-flt conc-arg) voltage-index)))
		       (/ beta (+ alpha beta))))			    
		(t (if (conc-particle-type-ss-function type)
		     (evaluate-conc-particle-type-ss-function type conc-arg voltage)
		     (let ((alpha (evaluate-conc-particle-type-alpha-function type conc-arg voltage))
			   (beta (evaluate-conc-particle-type-beta-function type conc-arg voltage)))
		       (/ alpha (+ alpha beta))))))))
    (if (= power 1.0) arg (expt arg power))))

(defun conc-particle-type-tau (type concentration &key (voltage -70.0) q10-rate-factor)
  (let* ((q10-rate-factor (or q10-rate-factor (conc-particle-type-q10-rate-factor type)))
	 (conc-arg (conc-particle-concentration-arg (d-flt concentration) type))
	 (voltage (d-flt voltage))
	 (voltage-index (voltage-to-voltage-index voltage))
	 (base-tau (case (conc-particle-type-class type)
		     ((:static-linear :static) nil)
		     (:nth-order
		      (let ((a (* (conc-particle-type-alpha type) conc-arg))
			    (b (conc-particle-type-beta type)))
			(/ 1.0 (+ a b))))
		     (:ml (let ((beta (ml-conc-particle-forward-rate type (d-flt conc-arg) voltage-index))
				(alpha (ml-conc-particle-backward-rate type (d-flt conc-arg) voltage-index)))
			    (/ (+ alpha beta))))
		     (t (if (conc-particle-type-tau-function type)
			  (evaluate-conc-particle-type-tau-function type conc-arg voltage)
			  (let ((alpha (evaluate-conc-particle-type-alpha-function type conc-arg voltage))
				(beta (evaluate-conc-particle-type-beta-function type conc-arg voltage)))
			    (/ 1.0 (+ alpha beta))))))))
    (/ (+ (conc-particle-type-tau-0 type) base-tau) q10-rate-factor)))
	    
(defun conc-inf-conc-particle-plot-list (type &key (power 1) (voltage -70.0) (log-scale t) (min-conc 1.0e-6)(max-conc 1.0e2))
  (let ((step (/ (- (if log-scale (log max-conc 10) max-conc) (if log-scale (log min-conc 10) min-conc)) 150))
	(start-conc (if log-scale (log min-conc 10) min-conc))
	(stop-conc (if log-scale (log max-conc 10) max-conc)))
    (when (<= step 0) (sim-error "Check concentration limits!"))
    (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc) by step
	  collecting i into log-concentrations
	  collecting (conc-particle-type-ss type (if log-scale (expt 10.0 i) i) :power power :voltage voltage) into inf
	  finally (return (list log-concentrations inf)))))

(defun conc-tau-conc-particle-plot-list (type &key (voltage -70.0) (log-scale t) (min-conc 1.0e-6) (max-conc 1.0e2))
  (case (conc-particle-type-class type)
    ((:static-linear :static) nil)
    (t (let ((step (/ (- (if log-scale (log max-conc 10) max-conc) (if log-scale (log min-conc 10) min-conc)) 150))
	     (start-conc (if log-scale (log min-conc 10) min-conc))
	     (stop-conc (if log-scale (log max-conc 10) max-conc)))
	 (when (<= step 0) (sim-error "Check concentration limits!"))
	 (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc) by step
	       collecting i into log-concentrations
	       collecting (conc-particle-type-tau type (if log-scale (expt 10.0 i) i) :voltage voltage) into tau
	       finally (return (list log-concentrations tau)))))))

(defun plot-conc-particle (prt-type &key (power 1) (new-plot nil) overlay (what '(:steady_state :tau))
				    voltage-step voltage-min voltage-max (increment-voltage 20) min-conc max-conc)
  (let ((prt-type (element prt-type 'conc-particle-type))
	(overlay (and overlay (not new-plot)))
	(*create-new-plot-windows* new-plot)
	(v-dep (element-parameter prt-type 'voltage-dependent))
	(dummy1 (or voltage-min (element-parameter prt-type 'plot-voltage-min) *particle-type-plot-minimum-voltage*))
	(dummy2 (or voltage-max (element-parameter prt-type 'plot-voltage-max) *particle-type-plot-maximum-voltage*))
	(dummy3 (or voltage-step (element-parameter prt-type 'plot-voltage-step) 20))
	(dummy5 (or min-conc (element-parameter prt-type 'plot-min-conc) 1e-6))
	(dummy6 (or max-conc (element-parameter prt-type 'plot-max-conc) 1e2))
	(dummy7 (element-parameter prt-type 'plot-log-conc))
	(dummy8 :concentration))
    (choose-variable-values
     `(,(when v-dep `(dummy8 "Plot against:" :choose (:voltage :concentration)))
       (dummy5 "Minimum concentration [mM]" :float)
       (dummy6 "Maximum concentration [mM]" :float)
       (dummy7 "For concentration (of X) plots, plot against log[X]" :boolean)
       (dummy1 "Minimum voltage [mV]" :float)
       (dummy2 "Maximum voltage [mV]" :float)
       (dummy3 "Voltage increment [mV]" :float))
     :label (format nil "Plot Format for ~A" (conc-particle-type-name prt-type)))
    (element-parameter prt-type 'plot-voltage-min dummy1)
    (element-parameter prt-type 'plot-voltage-max dummy2)
    (element-parameter prt-type 'plot-voltage-step dummy3)
    (element-parameter prt-type 'plot-min-conc dummy5)
    (element-parameter prt-type 'plot-max-conc dummy6)
    (element-parameter prt-type 'plot-log-conc dummy7)
    (case dummy8
      (:voltage
       (when (member :steady_state what) (plot-conc-particle-ss-against-voltage prt-type power overlay))
       (when (member :tau what) (plot-conc-particle-tau-against-voltage prt-type overlay)))
      (:concentration
       (when (member :steady_state what)
	 (plot-conc-particle-type-ss prt-type :order-in-channel power :new-plot new-plot :overlay overlay))	    
       (when (member :tau what)
	 (plot-conc-particle-type-tau
	  prt-type
	  :new-plot new-plot :overlay overlay
	  :log-scale dummy7 :min-conc dummy5 :max-conc dummy6 :voltage-step dummy3 :voltage-min dummy1 :voltage-max dummy2))))))
  
(defun plot-conc-particle-type-ss (type &key (order-in-channel 1) (new-plot nil) overlay
					(log-scale (element-parameter type 'plot-log-conc))
					(min-conc (element-parameter type 'plot-min-conc))
					(max-conc (element-parameter type 'plot-max-conc))
					(voltage-step (element-parameter type 'plot-voltage-step))
					(voltage-min (or (element-parameter type 'plot-voltage-min) *particle-type-plot-minimum-voltage*))
					(voltage-max (or (element-parameter type 'plot-voltage-max) *particle-type-plot-maximum-voltage*))
					(increment-voltage (or (element-parameter type 'plot-increment-voltage) 20)))
  (let* ((type (element type 'conc-particle-type))
	 (name (element-name type))
	 (v-dep-conc-part (element-parameter type 'voltage-dependent))
	 (major-ion (ion-string (element-major-ion type)))
	 (xy-data (loop for voltage from voltage-min to (if (and increment-voltage v-dep-conc-part) voltage-max voltage-min)
			by voltage-step nconcing
			(loop for order from 1 to order-in-channel by (max 1 (1- order-in-channel))
			      collecting
			      (conc-inf-conc-particle-plot-list type :power order :log-scale log-scale :voltage voltage	:min-conc min-conc :max-conc max-conc))))
	 (labels (loop for voltage from voltage-min to (if (and increment-voltage v-dep-conc-part) voltage-max voltage-min) by voltage-step nconcing
		       (loop for order from 1 to order-in-channel
			     by (max 1 (1- order-in-channel))
			     collecting (format nil "~A~A"
						(if (> order 1) (format nil "~A-eff (^~d)" name order) name)
						(if v-dep-conc-part (format nil " @ ~AmV" voltage) ""))))))
    (plot-timed-data
     (list (loop for part-list in xy-data collect (cadr part-list)))
     labels
     (caar xy-data)
     :title (format nil "~a Channel Conc-Particles: Steady State([~A])" name major-ion)
     :y-origin-tick nil :y-label-v-position :upper-right
     :y-inc 0.25 :y-max 1.0 :y-min 0.0 :y-label "Steady State"
     :x-min (if log-scale (log-10 min-conc) (if (> (/ max-conc (+ max-conc min-conc)) (/ 10 11)) 0.0 min-conc))
     :x-max  (if log-scale (log-10 max-conc) max-conc) :x-inc (when log-scale 1.0) 
     :x-label (format nil (if log-scale "[~A](Log mM)" "[~A](mM)") major-ion)
     :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
     :overlay overlay :prompt-for-overlay t)))

(defun plot-conc-particle-type-tau (type &key (order-in-channel 1) (new-plot nil) overlay
					 (log-scale (element-parameter type 'plot-log-conc))
					 (min-conc (element-parameter type 'plot-min-conc))
					 (max-conc (element-parameter type 'plot-max-conc))
					 (voltage-step (element-parameter type 'plot-voltage-step))
					 (voltage-min (or (element-parameter type 'plot-voltage-min) *particle-type-plot-minimum-voltage*))
					 (voltage-max (or (element-parameter type 'plot-voltage-max) *particle-type-plot-maximum-voltage*))
					 (increment-voltage (or (element-parameter type 'plot-increment-voltage) 20)))
  (when (case (conc-particle-type-class type)
	  ((:static :static-linear) nil)
	  (t t))
    (let* ((type (element type 'conc-particle-type))
	   (name (element-name type))
	   (v-dep-conc-part (element-parameter type 'voltage-dependent))  (major-ion (ion-string (element-major-ion type)))
	   (xy-data (loop for voltage from voltage-min to (if (and increment-voltage v-dep-conc-part) voltage-max voltage-min) by voltage-step
			  collecting (conc-tau-conc-particle-plot-list type :log-scale log-scale :min-conc min-conc :max-conc max-conc :voltage voltage)))
	   (labels (loop for voltage from voltage-min to (if (and increment-voltage v-dep-conc-part) voltage-max voltage-min) by voltage-step
			 collecting (format nil "~A~A" name (if v-dep-conc-part (format nil " @ ~AmV" voltage) "")))))
      (plot-timed-data
       (list (loop for part-list in xy-data collect (cadr part-list)))
       labels
       (caar xy-data)
       :title (format nil "~a Channel Conc-Particles: Tau([~A])" name major-ion)
       :x-min (if log-scale (log-10 min-conc) (if (> (/ max-conc (+ max-conc min-conc)) (/ 10 11)) 0.0 min-conc))
       :x-max (if log-scale (log-10 max-conc) max-conc) :x-inc (when log-scale 1.0)
       :x-label (format nil (if log-scale "[~A](Log mM)" "[~A](mM)") major-ion)
       :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
       :y-origin-tick nil :y-label-v-position :upper-right :y-min 0.0 :y-label "ms"
       :overlay overlay :prompt-for-overlay t))))

(defun plot-conc-particle-ss-against-voltage (type power &optional overlay)
  (let ((concs (loop for exponent from -6 to 1 collect (expt 10.0 exponent))))
    (plot-timed-data
     (loop for conc in concs collect 
	   (loop for voltage from *particle-look-up-table-min-voltage* to *particle-look-up-table-max-voltage* by 1.0
		 collect (conc-particle-type-ss type conc :power power :voltage voltage)))
     (loop for conc in concs collect (format nil "~A mM" conc))
     1.0
     :delta-t-start *particle-look-up-table-min-voltage*
     :overlay overlay :prompt-for-overlay t :comment-position :upper-right
     :y-label "SS" :x-label "mV" :y-min 0.0 :y-max 1.0 :y-inc 0.25 :y-origin-tick t
     :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
     :x-origin *particle-type-plot-minimum-voltage* :x-inc 50.0 :x-origin-tick t :x-are-fns t
     :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage* :x-inc 50
     :title (format nil "SS Values (power ~A) For ~A" power (conc-particle-type-name type)))))

(defun plot-conc-particle-tau-against-voltage (type &optional overlay)
  (let ((concs (loop for exponent from -6 to 1 collect (expt 10.0 exponent))))
    (plot-timed-data
     (loop for conc in concs collect
	   (loop for voltage from (d-flt *particle-look-up-table-min-voltage*) to *particle-look-up-table-max-voltage* by 1.0
		 collect (conc-particle-type-tau type conc :voltage voltage)))
     (loop for conc in concs collect (format nil "~A mM" conc))
     1.0
     :delta-t-start *particle-look-up-table-min-voltage*
     :overlay overlay :prompt-for-overlay t
     :y-label "ms" :x-label "mV" :y-min 0.0
     :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
     :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage* :x-inc 50
     :x-origin *particle-type-plot-minimum-voltage* :x-origin-tick t :x-are-fns t
     :title (format nil "Tau Values For ~A" (conc-particle-type-name type)))))

(defun update-conc-particle-type-q10 (type &optional value)
  "When the numeric VALUE is included, set the q10 of conc-particle type TYPE to VALUE. Else, return current value. Do necessary updates."
  (let ((type (element type 'conc-particle-type)))
    (when value (element-slot type :q10 (s-flt value)))
    (when type (setf (conc-particle-type-q10-rate-factor type) (element-q10-rate-factor type)))))
  
(defun update-conc-particle-type-q10s () (loop for type in (CONC-PARTICLE-TYPEs) do (update-conc-particle-type-q10 type)))

(defun setup-conc-particles ()
  (mapcar 'remove-conc-particle-type-lists (conc-particle-types))
  (setq *conc-particle-type-list*
	(delete-duplicates
	 (loop for ch-type in *channel-type-list*
	       when (CHANNEL-TYPE-ACTIVE-P ch-type)
	       nconc (loop for prt-type-power
			   in (channel-type-conc-particle-types-and-powers (element ch-type 'channel-type))
			   collect (car prt-type-power))))))
	
(defun remove-conc-particle-type-lists (type) ; (remove-element-parameters type '(active-conc-particles))
  )

#|
(defun advance-conc-particles ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *conc-particle-type-list* do
	(loop for cprt in (element-parameter type 'active-conc-particles) do
	      (setf (conc-particle-state-n-double cprt) (conc-particle-state-n+1-double cprt)
		    (conc-particle-dsdt-n-1-double cprt) (conc-particle-dsdt-n-double cprt))))
  nil)
|#

(defun advance-conc-particles ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *conc-particle-type-list* do
	(conc-particle-type-iterator
	 (prt type)
	 unless (channel-blocked (conc-particle-channel prt))
	 do (setf (conc-particle-state-n-double prt) (conc-particle-state-n+1-double prt)
		  (conc-particle-dsdt-n-1-double prt) (conc-particle-dsdt-n-double prt))))
  nil)
  
(proclaim '(inline eval-ml-conc-particle))
(defun eval-ml-conc-particle (prt type concentration &optional initial-state)
  ;; This handles the :ML model. Within this function, the conventional nomenclature of ALPHA as forward rate and BETA as backward rate is used.
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let* ((conc-arg (conc-particle-concentration-arg concentration type))
	 (voltage-index (conc-particle-voltage-index prt))
	 (q10-rate-factor (conc-particle-type-q10-rate-factor type))
	 (base-forward-rate (the df (ml-conc-particle-forward-rate type conc-arg voltage-index)))
	 (base-backward-rate (the df (ml-conc-particle-backward-rate type conc-arg voltage-index)))
	 (base-tau (the df (/ 1.0d0 (the df (+ base-forward-rate base-backward-rate)))))
	 (tau (the df (/ (+ (conc-particle-type-tau-0 type) base-tau) q10-rate-factor)))
	 (inf (the df (* base-forward-rate base-tau))))
    (declare (double-float tau inf base-forward-rate base-backward-rate base-tau))
    (setf (conc-particle-state-n+1-double prt) (adapted-hines-1st-order-equation-from-double-tau-inf tau inf (conc-particle-state-n-double prt) initial-state prt))
    (finish-eval-particle-aref (conc-particle-double-floats prt) initial-state (when *debug-particle-error* prt)))
  nil)

(proclaim '(inline eval-nthorder-conc-particle))
(defun eval-nthorder-conc-particle (prt type concentration &optional initial-state) 
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let* ((conc-arg (conc-particle-concentration-arg concentration type))
	 (q10-rate-factor (conc-particle-type-q10-rate-factor type))
	 (base-forward-rate (the df (* (conc-particle-type-alpha type) conc-arg)))
	 (base-backward-rate (the df (* 1.0d0 (conc-particle-type-beta type))))
	 (base-tau (the df (/ 1.0d0 (the df (+ base-forward-rate base-backward-rate)))))
	 (tau (the df (/ (+ (conc-particle-type-tau-0 type) base-tau) q10-rate-factor)))
	 (inf (the df (* base-forward-rate base-tau))))
    (declare (double-float tau inf base-forward-rate base-backward-rate base-tau))
    (setf (conc-particle-state-n+1-double prt) (adapted-hines-1st-order-equation-from-double-tau-inf tau inf (conc-particle-state-n-double prt) initial-state prt))
    (finish-eval-particle-aref (conc-particle-double-floats prt) initial-state (when *debug-particle-error* prt)))
  nil)
  
(proclaim '(inline eval-static-conc-particle))
(defun eval-static-conc-particle (prt type concentration &optional initial-state) 
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let ((conc-arg (conc-particle-concentration-arg concentration type)))
    (setf (conc-particle-state-n+1-double prt) (the df (funcall (the (function (double-float) double-float) (conc-particle-type-conc-dependence type)) conc-arg)))
    (when initial-state (set-particle-aref-initial-states (conc-particle-double-floats prt))))
  nil)

(proclaim '(inline static-linear-conc-particle-state))
(defun static-linear-conc-particle-state (concentration concentration-min concentration-slope)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (the df (min (the df concentration-min)
	       (* (the df concentration-slope) (the df concentration)))))

(proclaim '(inline eval-static-linear-conc-particle))
(defun eval-static-linear-conc-particle (prt type concentration-min concentration-slope concentration &optional initial-state)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let* ((conc-arg (conc-particle-concentration-arg concentration type)))
    (setf (conc-particle-state-n+1-double prt) (static-linear-conc-particle-state conc-arg concentration-min concentration-slope)))
  (when initial-state (set-particle-aref-initial-states (conc-particle-double-floats prt)))
  nil)
  
(proclaim '(inline eval-conc-volt-alpha-beta-conc-particle))
(defun eval-conc-volt-alpha-beta-conc-particle (prt type q10-rate-factor concentration voltage initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt)
	   (double-float concentration voltage)
	   (single-float q10-rate-factor))
  (let* ((conc-arg (conc-particle-concentration-arg concentration type))
	 (alpha (d-flt (evaluate-conc-particle-type-alpha-function type conc-arg voltage)))
	 (beta (d-flt (evaluate-conc-particle-type-beta-function type conc-arg voltage)))
	 (base-tau (the df (/ 1.0d0 (the df (+ alpha beta)))))
	 (tau (the df (/ (+ (conc-particle-type-tau-0 type) base-tau) q10-rate-factor)))
	 (inf (the df (* alpha base-tau))))
    (declare (double-float inf tau alpha beta))
    (setf (conc-particle-state-n+1-double prt) (adapted-hines-1st-order-equation-from-double-tau-inf tau inf (conc-particle-state-n-double prt) initial-state prt))
    nil))

(proclaim '(inline eval-conc-volt-tau-ss-conc-particle))
(defun eval-conc-volt-tau-ss-conc-particle (prt type q10-rate-factor concentration voltage initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt)
	   (double-float concentration voltage)
	   (single-float q10-rate-factor))
  (let* ((conc-arg (conc-particle-concentration-arg concentration type))
	 (tau (d-flt (evaluate-conc-particle-type-tau-function type conc-arg voltage)))
	 (ss (d-flt (evaluate-conc-particle-type-ss-function type conc-arg voltage))))
    (setf (conc-particle-state-n+1-double prt)
	  (adapted-hines-1st-order-equation-from-double-tau-inf (/ (+ (conc-particle-type-tau-0 type) tau) q10-rate-factor)
								ss (conc-particle-state-n-double prt) initial-state prt))
    nil))

(proclaim '(inline eval-conc-volt-conc-particle))
(defun eval-conc-volt-conc-particle (prt type concentration voltage &optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let ((q10-rate-factor (the sf (conc-particle-type-q10-rate-factor type))))
    (if (conc-particle-type-alpha-function type)
      (eval-conc-volt-alpha-beta-conc-particle prt type q10-rate-factor concentration voltage initial-state)
      (eval-conc-volt-tau-ss-conc-particle prt type q10-rate-factor concentration voltage initial-state))
    (finish-eval-particle-aref (conc-particle-double-floats prt) initial-state (when *debug-particle-error* prt)))
  nil)

(proclaim '(inline eval-generic-alpha-beta-conc-particle))
(defun eval-generic-alpha-beta-conc-particle (prt type q10-rate-factor initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt)
	   (single-float q10-rate-factor))
  (let* ((alpha (the df (funcall (the function (conc-particle-type-alpha-function type)) prt)))
	 (beta (the df (funcall (the function (conc-particle-type-beta-function type)) prt)))
	 (base-tau (the df (/ 1.0d0 (the df (+ alpha beta)))))
	 (tau (the df (/ (+ (conc-particle-type-tau-0 type) base-tau) q10-rate-factor)))
	 (inf (the df (* alpha base-tau))))
    (declare (double-float inf tau alpha beta))
    (setf (conc-particle-state-n+1-double prt) (adapted-hines-1st-order-equation-from-double-tau-inf tau inf (conc-particle-state-n-double prt) initial-state prt))
    nil))

(proclaim '(inline eval-generic-tau-ss-conc-particle))
(defun eval-generic-tau-ss-conc-particle (prt type q10-rate-factor initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt)
	   (single-float q10-rate-factor))
  (let* ((tau (the df (funcall (the function (conc-particle-type-tau-function type)) prt)))
	 (ss (the df (funcall (the function (conc-particle-type-ss-function type)) prt))))
    (setf (conc-particle-state-n+1-double prt)
	  (adapted-hines-1st-order-equation-from-double-tau-inf (/ (+ (conc-particle-type-tau-0 type) tau) q10-rate-factor)
								ss (conc-particle-state-n-double prt) initial-state prt))
    nil))

(proclaim '(inline eval-generic-conc-particle))
(defun eval-generic-conc-particle (prt type &optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type conc-particle prt))
  (let ((q10-rate-factor (the sf (conc-particle-type-q10-rate-factor type))))
    (if (conc-particle-type-alpha-function type)
      (eval-generic-alpha-beta-conc-particle prt type q10-rate-factor initial-state)
      (eval-generic-tau-ss-conc-particle prt type q10-rate-factor initial-state))
    (finish-eval-particle-aref (conc-particle-double-floats prt) initial-state (when *debug-particle-error* prt)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-conc-particles () (eval-all-conc-particles t))

(defun eval-all-static-linear-conc-particles (type initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (conc-particle-type-parameters type))
	 (conc-min (the df (get-a-value 'concentration-min params)))
	 (conc-slope (the df (get-a-value 'concentration-slope params))))
    (conc-particle-type-iterator
     (prt type)
     unless (channel-blocked (conc-particle-channel prt))
     do (let ((concentration (conc-particle-concentration prt concentration)))
	  (eval-static-linear-conc-particle prt type conc-min conc-slope concentration initial-state))))
  nil)

(defun eval-all-ml-conc-particles (type initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (conc-particle-type-iterator
   (prt type)
   unless (channel-blocked (conc-particle-channel prt))
   do (let ((concentration (conc-particle-concentration prt concentration)))
	(eval-ml-conc-particle prt type concentration initial-state)))
  nil)

(defun eval-all-nthorder-conc-particles (type initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (conc-particle-type-iterator
   (prt type)
   unless (channel-blocked (conc-particle-channel prt))
   do (let ((concentration (conc-particle-concentration prt concentration)))
	(eval-nthorder-conc-particle prt type concentration initial-state)))
  nil)

(defun eval-all-static-conc-particles (type initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (conc-particle-type-iterator
   (prt type)
   unless (channel-blocked (conc-particle-channel prt))
   do (let ((concentration (conc-particle-concentration prt concentration)))
	(eval-static-conc-particle prt type concentration initial-state)))
  nil)

(defun eval-all-conc-volt-conc-particles (type initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (conc-particle-type-iterator
   (prt type)
   unless (channel-blocked (conc-particle-channel prt))
   do (let ((voltage (conc-particle-voltage prt))
	    (concentration (conc-particle-concentration prt concentration)))
	(eval-conc-volt-conc-particle prt type concentration voltage initial-state)))
  nil)

(defun eval-all-generic-conc-particles (type initial-state)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (conc-particle-type-iterator
   (prt type)
   unless (channel-blocked (conc-particle-channel prt))
   do (eval-generic-conc-particle prt type initial-state))
  nil)

(defun eval-all-conc-particles (&optional initial-state concentration)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *conc-particle-type-list* do
	(if (conc-particle-type-evaluation-function type)
	  (funcall (conc-particle-type-evaluation-function type) type initial-state concentration)
	  (case (conc-particle-type-class type)
	    (:static-linear (eval-all-static-linear-conc-particles type initial-state concentration))
	    (:ml            (eval-all-ml-conc-particles type initial-state concentration))
	    (:nth-order     (eval-all-nthorder-conc-particles type initial-state concentration))
	    (:static        (eval-all-static-conc-particles type initial-state concentration))
	    (:conc-volt     (eval-all-conc-volt-conc-particles type initial-state concentration))
	    (:generic       (eval-all-generic-conc-particles type initial-state)))))
  nil)
  



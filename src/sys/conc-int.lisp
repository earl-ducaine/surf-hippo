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


;;; SYS Source file: conc-int.lisp
;
; The model to integrate the changes in ion concentration.
;


(in-package "SURF-HIPPO")

(defun conc-int-types-shell-2-p ()
  ;; Check to make sure there are either shell 2 or shell 3 integrators to plot.
  (loop for type in (conc-int-types) thereis (and (element-in-circuit type) (conc-int-type-shell-2-p type))))

(defun conc-int-types-shell-3-p () (loop for type in (conc-int-types) thereis (and (element-in-circuit type) (conc-int-type-shell-3-p type))))

(proclaim '(inline nernst-potential-df))
(defun nernst-potential-df (inside-conc outside-conc valence)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (double-float inside-conc outside-conc)
	   (single-float valence))
  (* (/ NERNST-EQN-CONST*1000*TEMP valence)
     (the df (log (/ outside-conc inside-conc)))))

(defun conc-int-concentration (elt &optional (data-type 'total))
  (let ((elt (element-core elt 'conc-int)))
    (when elt
      (case data-type
	((or shell-1 1 concentration-1) (conc-int-shell-1-free-conc-n+1 elt))
	((or shell-2 2 concentration-2) (conc-int-shell-2-free-conc-n+1 elt))
	((or shell-3 3 concentration-3) (conc-int-shell-3-free-conc-n+1 elt))
	(t (conc-int-total-free-conc-n+1 elt))))))

(defun concentration-clamp (element &optional concentration)
  "Turn off all concentration integrators associated with ELEMENT. If CONCENTRATION is a number [mM, default NIL], then set
steady-state value of the associated integrator types to this value. If CONCENTRATION is :FIX, then set the steady-state value of
integrator type to the current value of the concentration integrator. Returns steady-state concentration(s). To turn concentration
integrators back on, use CONCENTRATION-CLAMP-OFF."
  (concentration-clamp-core element concentration t))

(defun concentration-clamp-off (element &optional concentration)
  "Turn on all concentration integrators associated with ELEMENT. Otherwise identical to CONCENTRATION-CLAMP-ON."
  (concentration-clamp-core element concentration nil))
	     
(defun concentration-clamp-core (element concentration clamp-on)
  (let ((conc-ints (cell-element-elements element 'conc-int)))
    (loop for type in (coerce-to-list (element-type conc-ints))
	  for conc-int in conc-ints
	  do (if clamp-on (turn-off type) (turn-on type))
	  when (or (numberp concentration) (equal :fix concentration))
	  do (element-parameter type 'resting-free-conc (if (numberp concentration) (s-flt concentration) (total-concentration conc-int)))
	  collect (element-parameter type 'resting-free-conc))))

(defun default-diffusion-coefficient (type)
  "The default diffusion coefficient for the ionic species specified for the concentration integrator type associated with TYPE,
given by global variables such as *D_CA*, [cm^2 sec^-1]. If ion not associated with a diffusion coefficient global variable,
returns 0.0."
  (let ((type (element-type type)))
    (typecase type
      (conc-int-type
       (case (conc-int-type-species type)
	 (na *D_NA*)
	 (LI *D_LI*)
	 (K *D_K*)
	 (cs *D_CS*)
	 (cl *D_CL*)
	 (br *D_BR*)
	 (tea *D_TEA*)
	 (mg *D_MG*)
	 (ca *D_CA*)
	 (t 0.0))))))

(defun conc-int-juxtamembrane-shell-thickness (cint) (conc-int-type-juxtamembrane-shell-thickness (conc-int-type cint)))
(defun conc-int-inner-shell-thickness (cint) (conc-int-type-inner-shell-thickness (conc-int-type cint)))

(defun conc-int-type-shell-p (type shell)
  (case shell
    (1 t)
    (2 (conc-int-type-shell-2-p type))
    (3 (conc-int-type-shell-3-p type))
    (core (conc-int-type-core-p type))))

(defun conc-int-type-d-shell-x-shell-y (type shell-x shell-y &optional value)
  ;; cm2/sec
  (let ((type (element-type type)))
    (typecase type
      (conc-int-type
       (let* ((diffusion-coefficient (conc-int-type-diffusion-coefficient type))
	      (assoc-value (when (consp diffusion-coefficient)
			     (or (assoc-with-test (list shell-x shell-y) diffusion-coefficient 'equal)
				 (assoc-with-test (list shell-y shell-x) diffusion-coefficient 'equal)))))
	 (if value
	   (setf (conc-int-type-diffusion-coefficient type)
		 (cons (list (list shell-x shell-y) (s-flt value))
		       (remove assoc-value diffusion-coefficient :test 'equal)))
	   (s-flt
	    (typecase diffusion-coefficient
	      (number diffusion-coefficient)
	      (cons (or (cadr assoc-value) (default-diffusion-coefficient type)))
	      (t (default-diffusion-coefficient type))))))))))

(defun conc-int-type-d-12 (type &optional value) (conc-int-type-d-shell-x-shell-y type 1 2 value))
(defun conc-int-type-d-13 (type &optional value) (conc-int-type-d-shell-x-shell-y type 1 3 value))
(defun conc-int-type-d-23 (type &optional value) (conc-int-type-d-shell-x-shell-y type 2 3 value))
(defun conc-int-type-d-3core (type &optional value) (conc-int-type-d-shell-x-shell-y type 3 'core value))
(defun conc-int-type-d-1core (type &optional value) (conc-int-type-d-shell-x-shell-y type 1 'core value))
(defun conc-int-type-d-2core (type &optional value) (conc-int-type-d-shell-x-shell-y type 2 'core value))
(defun conc-int-type-d-12core (type &optional value) (conc-int-type-d-shell-x-shell-y type 1 'core value))

(defun parse-conc-int-type-diffusion-coefficient (type)
  (setf (conc-int-type-diffusion-coefficient type)
	(no-nils
	 `(,(when (conc-int-type-shell-2-p type) `((1 2) ,(conc-int-type-d-12 type)))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type)) `((2 3) ,(conc-int-type-d-23 type)))
	   ,(when (conc-int-type-shell-3-p type) `((1 3) ,(conc-int-type-d-13 type)))
	   ,(when (conc-int-type-core-p type) `((1 core) ,(conc-int-type-d-1core type)))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-core-p type)) `((2 core) ,(conc-int-type-d-2core type)))
	   ,(when (and (conc-int-type-shell-3-p type) (conc-int-type-core-p type)) `((3 core) ,(conc-int-type-d-3core type)))))))

(defun conc-int-type-distance-shell-x-shell-y (type shell-x shell-y &optional value)
  ;; microns returns single-float
  (let ((type (element-type type)))
    (typecase type
      (conc-int-type
       (let* ((diffusion-distance (conc-int-type-diffusion-distances type))
	      (assoc-value (when (consp diffusion-distance)
			     (or (assoc-with-test (list shell-x shell-y) diffusion-distance 'equal)
				 (assoc-with-test (list shell-y shell-x) diffusion-distance 'equal)))))
	 (if value
	   (setf (conc-int-type-diffusion-distances type)
		 (cons (list (list shell-x shell-y) (s-flt value))
		       (remove assoc-value diffusion-distance :test 'equal)))
	   (s-flt
	    (typecase diffusion-distance
	      (number diffusion-distance)
	      (cons (or (cadr assoc-value) *default-conc-int-type-diffusion-distance*))
	      (t *default-conc-int-type-diffusion-distance*)))))))))

(defun conc-int-type-distance-12 (type &optional value) (conc-int-type-distance-shell-x-shell-y type 1 2 value))
(defun conc-int-type-distance-13 (type &optional value) (conc-int-type-distance-shell-x-shell-y type 1 3 value))
(defun conc-int-type-distance-23 (type &optional value) (conc-int-type-distance-shell-x-shell-y type 2 3 value))
(defun conc-int-type-distance-3core (type &optional value) (conc-int-type-distance-shell-x-shell-y type 3 'core value))
(defun conc-int-type-distance-1core (type &optional value) (conc-int-type-distance-shell-x-shell-y type 1 'core value))
(defun conc-int-type-distance-2core (type &optional value) (conc-int-type-distance-shell-x-shell-y type 2 'core value))
(defun conc-int-type-distance-12core (type &optional value) (conc-int-type-distance-shell-x-shell-y type 1 'core value))

(defun parse-conc-int-type-diffusion-distances (type)
  (setf (conc-int-type-diffusion-distances type)
	(no-nils
	 `(,(when (conc-int-type-shell-2-p type) `((1 2) ,(conc-int-type-distance-12 type)))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type)) `((2 3) ,(conc-int-type-distance-23 type)))
	   ,(when (conc-int-type-shell-3-p type) `((1 3) ,(conc-int-type-distance-13 type)))
	   ,(when (conc-int-type-core-p type) `((1 core) ,(conc-int-type-distance-1core type)))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-core-p type)) `((2 core) ,(conc-int-type-distance-2core type))
	      (when (and (conc-int-type-shell-3-p type) (conc-int-type-core-p type)) `((3 core) ,(conc-int-type-distance-3core type))))))))

(defun conc-int-valence (cint)
  (let ((type (element-type cint)))
    (when type (conc-int-type-valence type))))

(proclaim '(inline conc-int-shell-1-free-conc-n))
(defun conc-int-shell-1-free-conc-n (cint)
  "Concentration [mM] of free ion in shell 1 of CINT at time n."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-1-conc-n cint) (conc-int-type-shell-1-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-1-conc-n cint)))

(proclaim '(inline conc-int-shell-1-free-conc-n+1))
(defun conc-int-shell-1-free-conc-n+1 (cint)
  "Concentration [mM] of free ion in shell 1 of CINT at time n+1."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-1-conc-n+1 cint) (conc-int-type-shell-1-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-1-conc-n+1 cint)))

(proclaim '(inline conc-int-shell-2-free-conc-n))
(defun conc-int-shell-2-free-conc-n (cint)
  "Concentration [mM] of free ion in shell 2 of CINT at time n."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-2-conc-n cint) (conc-int-type-shell-2-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-2-conc-n cint)))

(proclaim '(inline conc-int-shell-2-free-conc-n+1))
(defun conc-int-shell-2-free-conc-n+1 (cint)
  "Concentration [mM] of free ion in shell 2 of CINT at time n+1."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-2-conc-n+1 cint) (conc-int-type-shell-2-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-2-conc-n+1 cint)))

(proclaim '(inline conc-int-shell-3-free-conc-n))
(defun conc-int-shell-3-free-conc-n (cint)
  "Concentration [mM] of free ion in shell 3 of CINT at time n."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-3-conc-n cint) (conc-int-type-shell-3-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-3-conc-n cint)))

(proclaim '(inline conc-int-shell-3-free-conc-n+1))
(defun conc-int-shell-3-free-conc-n+1 (cint)
  "Concentration [mM] of free ion in shell 3 of CINT at time n+1."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (/ (conc-int-shell-3-conc-n+1 cint) (conc-int-type-shell-3-instantaneous-buffer-ratio+1 (conc-int-type cint)))
    (conc-int-shell-3-conc-n+1 cint)))

(proclaim '(inline conc-int-core-free-conc))
(defun conc-int-core-free-conc (cint)
  "Concentration [mM] of free ion in core compartment of CINT."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((type (conc-int-type cint)))
    (if (conc-int-type-instantaneous-buffer-enabled type)
      (/ (conc-int-type-core-conc-double type) (conc-int-type-global-instantaneous-buffer-ratio+1 type))
      (conc-int-type-core-conc-double type))))

(proclaim '(inline total-concentration-n+x))
(defun total-concentration-n+x (cint &optional free (n+1-p t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((type (conc-int-type cint)))
    (case (conc-int-type-class type) 
      (:general (let ((total-compartment-volume (element-parameter cint 'total-compartment-volume)))
		  (if (and total-compartment-volume (> (the sf total-compartment-volume) 0.0))
		    (loop for state in (conc-int-state-arrays cint)
			  for volume in (element-parameter cint 'compartment-volumes)
			  sum (let ((shell-1-conc (if n+1-p (conc-int-aref-shell-1-conc-n+1 state) (conc-int-aref-shell-1-conc-n state))))
				(* (the sf volume)
				   (if (and free (conc-int-type-instantaneous-buffer-enabled type))
				     (/ shell-1-conc (conc-int-type-global-instantaneous-buffer-ratio+1 type))
				     shell-1-conc)))
			  into numerator double-float finally (return (/ numerator (the sf total-compartment-volume)))))
		  0.0d0))
      (t (if (= (conc-int-total-volume cint) 0.0)
	   0.0d0
	   (let ((shell-1-conc
		  (if free
		    (if n+1-p (conc-int-shell-1-free-conc-n+1 cint) (conc-int-shell-1-free-conc-n cint))
		    (if n+1-p (conc-int-shell-1-conc-n+1 cint) (conc-int-shell-1-conc-n cint))))
		 (shell-2-conc
		  (if free
		    (if n+1-p (conc-int-shell-2-free-conc-n+1 cint) (conc-int-shell-2-free-conc-n cint))
		    (if n+1-p (conc-int-shell-2-conc-n+1 cint) (conc-int-shell-2-conc-n cint))))
		 (shell-3-conc
		  (if free
		    (if n+1-p (conc-int-shell-3-free-conc-n+1 cint) (conc-int-shell-3-free-conc-n cint))
		    (if n+1-p (conc-int-shell-3-conc-n+1 cint) (conc-int-shell-3-conc-n cint))))
		 (core-conc
		  (if free (conc-int-core-free-conc cint) (conc-int-core-conc-double cint))))
	     (/ (+ (* (conc-int-shell-1-volume cint) shell-1-conc)
		   (* (conc-int-shell-2-volume cint) shell-2-conc)
		   (* (conc-int-shell-3-volume cint) shell-3-conc)
		   (* (conc-int-core-volume cint) core-conc))
		(conc-int-total-volume cint))))))))

(proclaim '(inline total-concentration-n+1))
(defun total-concentration-n+1 (cint &optional free) (total-concentration-n+x cint free t))

(proclaim '(inline total-free-concentration-n+1))
(defun total-free-concentration-n+1 (cint) (total-concentration-n+1 cint t))

(proclaim '(inline total-concentration-n))
(defun total-concentration-n (cint &optional free) (total-concentration-n+x cint free nil))

(proclaim '(inline total-free-concentration-n))
(defun total-free-concentration-n (cint) (total-concentration-n cint t))

(proclaim '(inline conc-int-total-free-conc-n))
(defun conc-int-total-free-conc-n (cint)
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (total-free-concentration-n cint)
    (if (conc-int-evaluate-total-concentration cint) (conc-int-total-conc-n cint) (total-concentration-n cint))))

(proclaim '(inline conc-int-total-free-conc-n+1))
(defun conc-int-total-free-conc-n+1 (cint)
  (if (conc-int-type-instantaneous-buffer-enabled (conc-int-type cint))
    (total-free-concentration-n+1 cint)
    (if (conc-int-evaluate-total-concentration cint) (conc-int-total-conc-n+1 cint) (total-concentration-n+1 cint))))	       
;; As defaults....
(proclaim '(inline total-free-concentration))
(defun total-free-concentration (cint) (total-free-concentration-n+1 cint))

(proclaim '(inline total-concentration))
(defun total-concentration (cint &optional free) (total-concentration-n+1 cint free))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conc-int-type-shell-instantaneous-buffer-ratio (type &optional shell)
  (1- 
   (case shell
     (1 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
     (2 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
     (3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
     (t (conc-int-type-global-instantaneous-buffer-ratio+1 type)))))

(defun conc-int-type-shell-instantaneous-buffer-p (type &optional shell)
  (if shell
    (> (conc-int-type-shell-instantaneous-buffer-ratio type shell) 0)
    (loop for shell from 1 to 3 thereis (> (conc-int-type-shell-instantaneous-buffer-ratio type shell) 0))))

(defun resting-concentration (type shell)
  ;; In mM. 
  (if (conc-int-type-instantaneous-buffer-enabled type)
    (* (element-parameter type 'resting-free-conc) ; single float
       (+ 1 (conc-int-type-shell-instantaneous-buffer-ratio type shell))) ; single float
    (element-parameter type 'resting-free-conc)))
  
(defun document-conc-int-type (type)
  (let* ((type (element type 'conc-int-type))
	 (type-name (element-name type 'conc-int-type)))
    (when type
      (format t "(conc-int-type-def~%")
      (format t " (~a~%" type-name)
      (format t "  (class . ~s)~%" (conc-int-type-class type))
      (format t "  (species . ~s)~%" (conc-int-type-species type))
      (format t "  (intra-p . ~a)~%" (conc-int-type-intra-p type))
      (when (conc-int-type-shell-2-p type) (format t "  (shell-2-p . T)~%"))
      (when (conc-int-type-shell-3-p type) (format t "  (shell-3-p . T)~%"))
      (when (conc-int-type-core-p type) (format t "  (core-p . T)~%"))
      (format t "  (juxtamembrane-shell-thickness . ~a)~%" (conc-int-type-juxtamembrane-shell-thickness type))
      (format t "  (inner-shell-thickness . ~a)~%" (conc-int-type-inner-shell-thickness type))
      (unless (= (conc-int-type-alpha-s type) 1) (format t "  (alpha-s . ~a)~%" (conc-int-type-alpha-s type)))
      (when (conc-int-type-volumes type) (format t "  (volumes . ~a)~%" (conc-int-type-volumes type)))
      (when (conc-int-type-membrane-areas type) (format t "  (membrane-areas . ~a)~%" (conc-int-type-membrane-areas type)))
      (when (conc-int-type-diffusion-areas type) (format t "  (diffusion-areas . ~a)~%" (conc-int-type-diffusion-areas type)))
      (format t "  (interdigitation-coefficient . ~a)~%" (conc-int-type-interdigitation-coefficient type))
      (format t "  (diffusion-coefficient . ~a)~%" (conc-int-type-diffusion-coefficient type))
      (format t "  (q10 . ~a)~%" (conc-int-type-q10 type))
      (format t "  (reference-temp . ~a)~%" (conc-int-type-reference-temp type))
      (format t "  (diffusion-distances . ~a)~%" (conc-int-type-diffusion-distances type))
      (format t "  (transmembrane-concentration . ~a)~%" (conc-int-type-transmembrane-conc type))
      (format t "  (PUMP-TYPE-PARAMS . ~a)~%" (conc-int-type-PUMP-TYPE-PARAMS type))
      (format t "  (resting-free-conc . ~a)~%" (element-parameter type 'resting-free-conc))
      (format t "  (instantaneous-buffer-enabled . ~A)~%" (conc-int-type-instantaneous-buffer-enabled type))
      (format t "  (instantaneous-buffer-ratio . ~s)~%"
	      (no-nils (list (list 1 (conc-int-type-shell-instantaneous-buffer-ratio type 1))
			     (when (conc-int-type-shell-2-p type) (list 2 (conc-int-type-shell-instantaneous-buffer-ratio type 2)))
			     (when (conc-int-type-shell-3-p type) (list 3 (conc-int-type-shell-instantaneous-buffer-ratio type 3)))
			     (list :global (conc-int-type-shell-instantaneous-buffer-ratio type :global)))))
      (cond-every
       ((> (or (element-parameter type 'tau) 0) 0)
	(format t "  (tau . ~A)~%" (element-parameter type 'tau))))
      (print-element-document-extras type)
      (format t "))"))))
       
(defun print-conc-int-type (type)
  (let* ((type (element type 'conc-int-type))
	 (core-connected-to-shells (and (conc-int-type-core-p type)
					(not (= (conc-int-type-D-3core type) (conc-int-type-D-12core type) 0)))))
    (when type
      (format t "Concentration Integrator Type ~a (class ~a): ~a, species ~a (Q10 ~a T_ref ~a)~%"
	      (conc-int-type-name type)
	      (conc-int-type-class type)
	      (if (conc-int-type-intra-p type) "intracellular" "extracellular")
	      (conc-int-type-species type)
	      (conc-int-type-q10 type)
	      (conc-int-type-reference-temp type))
      (when (eq (conc-int-type-class type) :multi-shell)
	(cond ((and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type))
	       (format t "    3 shells (interdigitated juxta-membrane pair and inner)~A~%" (if core-connected-to-shells " + core" "")))
	      ((conc-int-type-shell-3-p type)
	       (format t "    Juxtamembrane and inner shell~A~%" (if core-connected-to-shells " + core" "")))
	      ((conc-int-type-shell-2-p type)
	       (format t "    2 interdigitated juxtamembrane shells~A~%" (if core-connected-to-shells " + core" "")))
	      (t (format t "single shell~A~%" (if core-connected-to-shells " + core" "")))))
      (format t "    Inner shell(s) thickness: ~,2eum" (conc-int-type-juxtamembrane-shell-thickness type))
      (when (conc-int-type-shell-3-p type)
	(if (= (conc-int-type-inner-shell-thickness type) 0)
	  (format t ", Shell 3 occupies remainder of element volume")
	  (format t ", Shell 3 thickness: ~,2eum" (conc-int-type-inner-shell-thickness type))))
      (when (conc-int-type-shell-2-p type)
	(format t "~&    Proportion of juxta-membrane shell assigned to shell 1 (alpha_s): ~,2e~%" (conc-int-type-alpha-s type))
	(format t "~&    Shells 1&2 interdigitation (partitioning) coefficient: ~,2e [1/um]" (conc-int-type-interdigitation-coefficient type)))    
      (format t "~&")
      (format t "    Resting free concentration: ~,2emM~%" (element-parameter type 'resting-free-conc))
      (case (conc-int-type-class type)
	(:multi-shell
	 (when (or (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type) core-connected-to-shells)
	   (format t "    Diffusion coeff [cm^2/sec]/distance [um] between shell pairs:~%   "))
	 (when (conc-int-type-shell-2-p type)
	   (format t " D-12 ~,2e/~,2e" (conc-int-type-D-12 type) (conc-int-type-Distance-12 type)))
	 (when (conc-int-type-shell-3-p type)
	   (format t " D-13 ~,2e/~,2e" (conc-int-type-D-13 type) (conc-int-type-Distance-13 type) ))
	 (when (and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type))
	   (format t " D-23 ~,2e/~,2e" (conc-int-type-D-23 type) (conc-int-type-Distance-23 type)))
	 (when core-connected-to-shells
	   (if (conc-int-type-shell-3-p type)
	     (format t " D-3core ~,2e/~,2e" (conc-int-type-D-3core type) (conc-int-type-Distance-3core type))
	     (format t " D-12core ~,2e/~,2e" (conc-int-type-D-12core type) (conc-int-type-Distance-12core type))))
	 (format t "~&"))
	(:FIRST-ORDER
	 (when (> (or (element-parameter type 'tau) 0) 0)
	   (format t "    tau ~,2e [ms]~%" (element-parameter type 'tau)))))
      (when (conc-int-type-shell-instantaneous-buffer-p type)
	(format t "    Instantaneous buffer ratios (bound[X]/[X]) -")
	(format t " Shell 1: ~a" (conc-int-type-shell-instantaneous-buffer-ratio type 1))
	(when (conc-int-type-shell-2-p type)
	  (format t ", Shell 2: ~a" (conc-int-type-shell-instantaneous-buffer-ratio type 2)))
	(when (conc-int-type-shell-3-p type)
	  (format t ", Shell 3: ~a" (conc-int-type-shell-instantaneous-buffer-ratio type 3)))
	(format t "~&")
	(if (conc-int-type-instantaneous-buffer-enabled type)
	  (format t "    Instantaneous buffer enabled~%")
	  (format t "    Instantaneous buffer disabled~%")))
      (print-num-elements-sourcefile type)
      (format t "~%"))))

(defun edit-conc-int-type (&optional type)
  (let ((type (element (or type (car (conc-int-types))) 'conc-int-type)))
    (when type
      (case (conc-int-type-class type)
	((:general :generic) nil)
	(:multi-shell (edit-multi-shell-conc-int-type type))
	(:FIRST-ORDER
	 (edit-first-order-cont-int-type type))))))

(defun edit-first-order-cont-int-type (&optional type)
  (let ((type (element (or type (loop for type in (conc-int-types) when (eq (conc-int-type-class type) :FIRST-ORDER) do (return type))) 'conc-int-type)))
    (when type
      (let* ((dummy1 (conc-int-type-juxtamembrane-shell-thickness type))
	     (dummy2 (or (element-parameter type 'tau) 0.0))
	     (dummy3 (conc-int-type-valence type))
	     (dummy9 (s-flt (element-parameter type 'resting-free-conc)))
	     (dummy10 (conc-int-type-blocked type))
	     (dummy14 (conc-int-type-instantaneous-buffer-enabled type))
	     (dummy15 (conc-int-type-shell-instantaneous-buffer-ratio type 1)))
	(choose-variable-values
	 `((dummy10 "Block this concentration integrator type" :boolean)
	   (dummy3 ,(format nil "Valence used for species ~A" (conc-int-type-species type)) :float)
	   (dummy9 "Free Concentration at integrator core [mM]" :float)
	   (dummy2 "Time constant [ms] (<= 0 means that there is no intrinsic decay)" :float)
	   (dummy14 "Enable instantaneous buffer" :boolean)
	   (dummy15 "Instantaneous buffer ratio (bound[X]/[X], >= 0)" :float)
	   (dummy1 ,(format nil "Thickness of juxtamembrane shell [um]~%(<= 0 means use entire element volume)") :float))
	 :text (ADD-LINEFEEDS-TO-STRING-LIST
		(list (format nil "Single Compartment First Order ~A ~A Integrator"
			      (if (conc-int-type-intra-p type) "Intracellular" "Extracellular")
			      (conc-int-type-species type))
		      (ELEMENT-SOURCEFILE-STRING type nil)))
	 :label (format nil "Edit Concentration Integrator Type ~a" (element-name type)))
	(element-parameter type 'resting-free-conc dummy9)
	(element-parameter type 'tau (s-flt dummy2))
	(setf (conc-int-type-instantaneous-buffer-enabled type) dummy14
	      (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type) (+ 1 (max 0.0 dummy15))
	      (conc-int-type-blocked type) dummy10
	      (conc-int-type-core-conc type) (if dummy14 (* dummy9 (+ 1 (max 0.0 dummy15))) dummy9)
	      (conc-int-type-valence type) dummy3
	      (conc-int-type-juxtamembrane-shell-thickness type) dummy1)
	(maphash 'set-conc-integrator-parameters (CONC-INT-HASH-TABLE))))))  

(defun edit-multi-shell-conc-int-type-diffusion-params (&optional type)
  (let ((type (element (or type (loop for type in (conc-int-types) when (eq (conc-int-type-class type) :multi-shell) do (return type))) 'conc-int-type)))
    (when type
      (let* ((dummy4 (conc-int-type-D-12 type))
	     (dummy5 (conc-int-type-D-13 type))
	     (dummy6 (conc-int-type-D-23 type))
	     (dummy7 (conc-int-type-D-3core type))
	     (dummy8 (conc-int-type-D-1core type))
	     (dummy20 (conc-int-type-D-2core type))
	     (dummy23 (conc-int-type-Distance-12 type))
	     (dummy24 (conc-int-type-Distance-13 type))
	     (dummy25 (conc-int-type-Distance-23 type))
	     (dummy26 (conc-int-type-Distance-3core type))
	     (dummy27 (conc-int-type-Distance-1core type))
	     (dummy28 (conc-int-type-Distance-2core type)))
	(choose-variable-values
	 `((,(format  nil "Diffusion constants D are in cm^2/sec.") :comment)
	   ,(when (conc-int-type-shell-2-p type) '(dummy4 "D between shells 1 and 2 (D-12)" :float))
	   ,(when (conc-int-type-shell-2-p type) '(dummy23 "Distance between shells 1 and 2 [um]" :float))
	   ,(when (conc-int-type-shell-3-p type) '(dummy5 "D between shells 1 and 3 (D-13)" :float))
	   ,(when (conc-int-type-shell-3-p type) '(dummy24 "Distance between shells 1 and 3 [um]" :float))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type)) '(dummy6 "D between shells 2 and 3 (D-23)" :float))
	   ,(when (and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type)) '(dummy25 "Distance between shells 2 and 3 [um]" :float))
	   ,(when (and (conc-int-type-shell-3-p type) (conc-int-type-core-p type)) '(dummy7 "D between shell 3 and the core (D-3core)" :float))
	   ,(when (and (conc-int-type-shell-3-p type) (conc-int-type-core-p type)) '(dummy26 "Distance between shell 3 and core [um]" :float))
	   ,(when (and (not (conc-int-type-shell-3-p type)) (conc-int-type-shell-2-p type) (conc-int-type-core-p type))
		  '(dummy20 "D between shell 2 and the core (D-3core)" :float))
	   ,(when (and (not (conc-int-type-shell-3-p type)) (conc-int-type-shell-2-p type) (conc-int-type-core-p type))
		  '(dummy28 "Distance between shell 2 and core [um]" :float))
	   ,(when (and (not (conc-int-type-shell-3-p type)) (conc-int-type-core-p type)) '(dummy8 "D between shell 1 and the core (D-1core)" :float))
	   ,(when (and (not (conc-int-type-shell-3-p type)) (conc-int-type-core-p type)) '(dummy27 "Distance between shell 1 and core [um]" :float)))
	 :text (multi-shell-conc-int-type-menu-text type) :label (format nil "Edit Concentration Integrator Type ~a" (element-name type)))
	(conc-int-type-Distance-12 type dummy23)
	(conc-int-type-Distance-13 type dummy24)
	(conc-int-type-Distance-23 type dummy25)
	(conc-int-type-Distance-3core type dummy26)
	(conc-int-type-Distance-1core type dummy27)
	(conc-int-type-Distance-2core type dummy28)
	(conc-int-type-D-12 type dummy4)
	(conc-int-type-D-13 type dummy5)
	(conc-int-type-D-23 type dummy6)
	(conc-int-type-D-3core type dummy7)
	(conc-int-type-D-12core type dummy8)
	nil))))

(defun edit-multi-shell-conc-int-type (&optional type)
  (let ((type (element (or type (loop for type in (conc-int-types) when (eq (conc-int-type-class type) :multi-shell) do (return type))) 'conc-int-type)))
    (when type
      (let* ((dummy1 (conc-int-type-juxtamembrane-shell-thickness type))
	     (dummy2 (conc-int-type-inner-shell-thickness type))
	     (dummy3 (conc-int-type-alpha-s type))
	     dummy4
	     (dummy9 (s-flt (element-parameter type 'resting-free-conc)))
	     (dummy10 (conc-int-type-blocked type))
	     (dummy11 (conc-int-type-interdigitation-coefficient type))
	     (dummy12 (if (element-parameter type 'IMPLICIT-CONC-INT-INTEGRATION) :implicit :explicit))
	     (dummy13 (conc-int-type-valence type))
	     (dummy14 (conc-int-type-instantaneous-buffer-enabled type))
	     (dummy15 (conc-int-type-shell-instantaneous-buffer-ratio type 1))
	     (dummy16 (conc-int-type-shell-instantaneous-buffer-ratio type 2))
	     (dummy17 (conc-int-type-shell-instantaneous-buffer-ratio type 3))
	     (dummy18 (conc-int-type-shell-instantaneous-buffer-ratio type))
	     (dummy19 nil)
	     (dummy21 nil)
	     (dummy22 nil))
	(choose-variable-values
	 `((dummy10 "Block this concentration integrator type" :boolean)
	   (dummy13 ,(format nil "Valence used for species ~A" (conc-int-type-species type)) :float)
	   (dummy9 "Steady-state free concentrations [mM]" :float)
	   (dummy1 "Thickness of juxtamembrane shell [um]" :float)
	   ,(when (conc-int-type-shell-3-p type)
		  `(dummy2 ,(concatenate-strings (format nil "Thickness of inner shell 3 [um]~%") "(0 => inner shell occupies rest of element volume)") :float))
	   ,(when (conc-int-type-shell-2-p type)
		  `(dummy3 ,(format nil "Interdigitated juxtamembrane shells ratio~%[area of shell 1 / area of shell 2]") :float))
	   ,(when (conc-int-type-shell-2-p type)
		  `(dummy11 ,(concatenate-strings (format nil "Interdigitation coefficient [1/microns]~%") "(value < 0 => shell 2 behind shell 1, diffusionally)") :float))
	   (dummy12 "Integration method:" :choose (:explicit :implicit) :label-left)
	   (:comment "Instantaneous buffer ratios (bound[X]/[X]) >= 0")
	   (dummy15 "Shell 1 buffer ratio" :float)
	   ,(when (conc-int-type-shell-2-p type) `(dummy16 "Shell 2 buffer ratio" :float))
	   ,(when (conc-int-type-shell-3-p type) `(dummy17 "Shell 3 buffer ratio" :float))
	   (dummy18 "Default buffer ratio" :float)
	   (dummy19 "Use default buffer ratio for all shells" :boolean)
	   (dummy14 "Enable instantaneous buffer" :boolean)
	   (dummy4 "Edit diffusion parameters" :boolean)
	   (dummy21 ,(format nil "Revamp conc-int type~%definition from current library") :boolean)
	   (dummy22 "Cancel edit" :boolean))
	 :text (multi-shell-conc-int-type-menu-text type)
	 :label (format nil "Edit Concentration Integrator Type ~a" (element-name type)))
	(unless dummy22
	  (if dummy21 (create-conc-int-type type)
	      (progn
		(when dummy4 (edit-multi-shell-conc-int-type-diffusion-params type))
					;	    (setq *implicit-conc-int-integration* (case dummy12
					;						    (:implicit t)
					;						    (t nil)))
		(element-parameter type 'IMPLICIT-CONC-INT-INTEGRATION 
				   (case dummy12
				     (:implicit t)
				     (t nil)))
		(element-parameter type 'resting-free-conc dummy9)
		(setf (conc-int-type-instantaneous-buffer-enabled type) dummy14
		      (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type)
		      (s-flt (+ 1 (max 0.0 (if dummy19 dummy18 dummy15))))
		      (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type)
		      (s-flt (+ 1 (max 0.0 (if dummy19 dummy18 dummy16))))
		      (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type)
		      (s-flt (+ 1 (max 0.0 (if dummy19 dummy18 dummy17))))
		      (conc-int-type-global-instantaneous-buffer-ratio+1 type) (s-flt (+ 1 (max 0.0 dummy18)))
		      (conc-int-type-interdigitation-coefficient type) dummy11
		      (conc-int-type-blocked type) dummy10
		      (conc-int-type-valence type) dummy13
		      (conc-int-type-core-conc type) (if dummy14 (* dummy9 (s-flt (+ 1 (max 0.0 dummy18)))) dummy9)
		      (conc-int-type-juxtamembrane-shell-thickness type) dummy1
		      (conc-int-type-inner-shell-thickness type) dummy2
		      (conc-int-type-alpha-s type) dummy3)
		(set-conc-integrator-type-parameters nil type)
		(maphash 'set-conc-integrator-parameters (CONC-INT-HASH-TABLE)))))))))

(defun multi-shell-conc-int-type-menu-text (type)
  (ADD-LINEFEEDS-TO-STRING-LIST
   (list (format nil "~a ~a Integrator"
		 (if (conc-int-type-intra-p type) "Intracellular" "Extracellular")
		 (conc-int-type-species type))
	 (cond ((and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type))
		"3 shells: (interdigitated juxtamembrane pair [1&2] and inner [3]) + constant core")
	       ((conc-int-type-shell-3-p type)
		"2 shells: juxtamembrane [1] and inner [3] shell + constant core")
	       ((conc-int-type-shell-2-p type)
		"2 interdigitated juxtamembrane shells + constant core")
	       (t  "single juxtamembrane shell and constant core"))
	 (ELEMENT-SOURCEFILE-STRING type nil))))

(defun menu-for-conc-ints ()
  (mapcar 'edit-conc-int-type (choose-list-values (namelist-of-all-things 'conc-int-type t) nil :label "Select Concentration Integrator Types To Modify")))

(defun set-conc-int-type-transmembrane-concentrations (type)
  (setf (conc-int-type-core-conc-double type) (d-flt (conc-int-type-core-conc type)))
  (setf (conc-int-type-transmembrane-conc type)
	(s-flt (or (element-parameter type 'transmembrane-concentration)
		   (if (conc-int-type-intra-p type)
		       (default-extracellular-concentration (conc-int-type-species type))
		       (default-intracellular-concentration (conc-int-type-species type))))))
  (setf (conc-int-type-transmembrane-conc-double type) (d-flt (conc-int-type-transmembrane-conc type))))

(defun create-conc-int-type (type-symbol &optional actual-type-symbol)
  (let* ((type (unless actual-type-symbol (if (conc-int-type-p type-symbol) type-symbol (CONC-INT-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (conc-int-type-p type-symbol) (conc-int-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'conc-int-type))
	 (library-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (type-parameters nil)
	 (parent-type-symbol (get-a-value 'parent-type library-parameters)))
    (when (eq parent-type-symbol type-symbol)
      (sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
    (unless library-parameters (sim-error (format nil "Don't know anything about conc int type ~A!" type-symbol)))
    (when actual-type-symbol (setq type-symbol actual-type-symbol))
    (unless type (setq type (if parent-type-symbol
				(create-CONC-INT-TYPE parent-type-symbol type-symbol)
				(make-CONC-INT-TYPE :name type-symbol))))
    (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
    (update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type)
    (setq type-parameters (update-element-parameters-with-new-parameters library-parameters type))    
    (setf (conc-int-type-class type) (or (get-a-value 'class type-parameters) :multi-shell))
    (let* ((species (or (get-a-value 'species type-parameters) 'ca))
	   ;;	     (default-D-coeff (ion-diffusion-coefficient species))
	   (k-buffer (get-a-value 'K-buffer type-parameters))
	   (shells-w-instantaneous-buffer (get-a-value 'shells-w-instantaneous-buffer type-parameters))
	   (inst-buff-param-list (or (get-a-value 'instantaneous-buffer-ratio type-parameters)
				     (get-a-value 'instantaneous-buffer-ratios type-parameters)
				     (get-a-value 'shell-buffer-ratios type-parameters))))
      (setf (conc-int-type-volumes type) (get-a-value 'volumes type-parameters)
	    (conc-int-type-membrane-areas type) (get-a-value 'membrane-areas type-parameters)
	    (conc-int-type-diffusion-areas type) (get-a-value 'diffusion-areas type-parameters)
	    (conc-int-type-diffusion-coefficient type) (or (get-a-value 'diffusion-coefficient type-parameters)
							   (get-a-value 'diffusion-coefficients type-parameters))
	    (conc-int-type-diffusion-distances type) (or (get-a-value 'diffusion-distances type-parameters)  *default-conc-int-type-diffusion-distance*)
	    (conc-int-type-instantaneous-buffer-enabled type) (true-p (or (get-a-value 'K-buffer type-parameters)
									  inst-buff-param-list
									  (get-a-value 'instantaneous-buffer-enabled type-parameters)))
	    (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type) (s-flt (1+ (max 0.0 (or (and (numberp inst-buff-param-list) inst-buff-param-list)
											      (cadr (find 1 inst-buff-param-list :key 'car))
											      (and (find 1 shells-w-instantaneous-buffer) k-buffer)
											      (get-a-value 'SHELL-1-INSTANTANEOUS-BUFFER-RATIO type-parameters)
											      0.0))))
	    (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type) (s-flt (1+ (max 0.0 (or (and (numberp inst-buff-param-list) inst-buff-param-list)
											      (cadr (find 2 inst-buff-param-list :key 'car))
											      (and (find 2 shells-w-instantaneous-buffer) k-buffer)
											      (get-a-value 'SHELL-2-INSTANTANEOUS-BUFFER-RATIO type-parameters)
											      0.0))))
	    (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type) (s-flt (1+ (max 0.0 (or (and (numberp inst-buff-param-list) inst-buff-param-list)
											      (cadr (find 3 inst-buff-param-list :key 'car))
											      (and (find 3 shells-w-instantaneous-buffer) k-buffer)
											      (get-a-value 'SHELL-3-INSTANTANEOUS-BUFFER-RATIO type-parameters)
											      0.0))))
	    (conc-int-type-global-instantaneous-buffer-ratio+1 type) (s-flt (1+ (max 0.0 (or (and (numberp inst-buff-param-list) inst-buff-param-list)
											     (cadr (find :global inst-buff-param-list :key 'car))
											     (and (find 'all shells-w-instantaneous-buffer) k-buffer)
											     (get-a-value 'global-INSTANTANEOUS-BUFFER-RATIO type-parameters)
											     0.0))))
	    (conc-int-type-species type) species
	    (conc-int-type-valence type) (s-flt (or (get-a-value 'valence type-parameters) (ion-valence species))))
      (case (conc-int-type-class type)
	(:general
	 (let ((compartments (get-a-value 'compartments type-parameters))) ; A list of labels
	   (element-parameter type 'compartment-shells (get-a-value 'compartment-shells type-parameters))
	   (setf (conc-int-type-system-of-differential-equations type) (make-system-of-differential-equations))
	   (setf (conc-int-type-equation-coefficients-array type)
		 (make-differential-equation-a-array (get-a-value 'equation-coefficients type-parameters))
		 (conc-int-type-number-of-states type) (length compartments)))))
      (cond-every
       ((or (assoc 'qten type-parameters) (assoc 'q10 type-parameters))
	(setf (conc-int-type-q10 type) (s-flt (cdr (or (assoc 'qten type-parameters)
						       (assoc 'q10 type-parameters))))))
       ((assoc 'reference-temp type-parameters)
	(setf (conc-int-type-reference-temp type) (get-a-sf-value 'reference-temp type-parameters)))
       ((assoc 'transmembrane-concentration type-parameters)
	(element-parameter type 'transmembrane-concentration (get-a-value 'transmembrane-concentration type-parameters)))		 
       ((assoc 'pump-type-params type-parameters)
	(setf (conc-int-type-pump-type-params type) (get-a-value 'pump-type-params type-parameters)))
       ((assoc 'parameters type-parameters)
	(setf (conc-int-type-parameters type)	(nconc (get-a-value 'parameters type-parameters) (copy-list (conc-int-type-parameters type)))))
       ((assoc 'intra-p type-parameters)
	(setf (conc-int-type-intra-p type) (get-a-value 'intra-p type-parameters)))
       ((and (not (eq (conc-int-type-class type) :first-order))
	     (or (assoc 'shell-2-p type-parameters) (assoc 'shell2-p type-parameters)))
	(setf (conc-int-type-shell-2-p type) (or (get-a-value 'shell-2-p type-parameters)
						 (assoc 'shell2-p type-parameters))))
       ((and (not (eq (conc-int-type-class type) :first-order))
	     (or (assoc 'shell-3-p type-parameters) (assoc 'shell3-p type-parameters)))
	(setf (conc-int-type-shell-3-p type) (or (get-a-value 'shell-3-p type-parameters)
						 (get-a-value 'shell3-p type-parameters))))
       ((and (not (eq (conc-int-type-class type) :first-order))
	     (assoc 'core-p type-parameters))
	(setf (conc-int-type-core-p type) (get-a-value 'core-p type-parameters)))
       ((assoc 'juxtamembrane-shell-thickness type-parameters)
	(setf (conc-int-type-juxtamembrane-shell-thickness type)
	      (get-a-sf-value 'juxtamembrane-shell-thickness type-parameters)))
       ((assoc 'inner-shell-thickness type-parameters)
	(setf (conc-int-type-inner-shell-thickness type) (get-a-sf-value 'inner-shell-thickness type-parameters)))
       ((assoc 'alpha-s type-parameters)
	(setf (conc-int-type-alpha-s type) (get-a-sf-value 'alpha-s type-parameters)))
       ((assoc 'core-conc type-parameters)
	(setf (conc-int-type-core-conc type) (get-a-sf-value 'core-conc type-parameters)))
       ((assoc 'interdigitation-coefficient type-parameters)
	(setf (conc-int-type-interdigitation-coefficient type) (get-a-sf-value 'interdigitation-coefficient type-parameters)))
       ((assoc 'tau type-parameters) (element-parameter type 'tau (get-a-sf-value 'tau type-parameters))))
      (parse-conc-int-type-diffusion-distances type)
      (parse-conc-int-type-diffusion-coefficient type)
      (element-parameter type 'resting-free-conc (s-flt (or (get-a-value 'resting-free-conc type-parameters)
							    (get-a-value 'core-conc type-parameters)
							    (conc-int-type-core-conc type))))
      (setf (CONC-INT-TYPE-HASH-TABLE (conc-int-type-name type)) type))
    (element-parameter type 'IMPLICIT-CONC-INT-INTEGRATION t)
    (setq *conc-int-type* type)
    type))

#|
(defun revamp-conc-int-type-parameters (&optional type)
;;  "Update the parameters of conc-int TYPE, if specified, else all conc-int types, according to the current parameters in the
;; conc-int type parameter libraries."
  (loop for type in (if type (list (element type 'conc-int-type)) (conc-int-types)) do (create-conc-int-type type nil t)))
|#

(defun create-conc-int (cell-element type-sym)
  (let* ((node (element-node (element-cell-element cell-element)))
	 (type (typecase type-sym
		 (conc-int-type type-sym)
		 (t (create-conc-int-type type-sym))))
	 (name (if *use-simple-names*
		 (1+ (hash-table-count (CONC-INT-HASH-TABLE)))
		 (format nil "~a-~a" (element-name cell-element) (conc-int-type-name type)))))
    (or
     (atomize-list (element-conc-ints cell-element type-sym))
     (CONC-INT-HASH-TABLE name)
     (let ((cint (make-conc-int :name name :cell-element cell-element :type type)))
       (setf (node-elements node) (cons cint (node-elements node))
	     (CONC-INT-HASH-TABLE name) cint)
       (case (conc-int-type-class type)
	 (:generic
	  (setf (conc-int-Beta-current-1 cint) 1.0d0
		(conc-int-Beta-current-2 cint) 1.0d0))
	 (:general
	  (setf (conc-int-state-arrays cint) (loop for i from 1 to (conc-int-type-number-of-states type) collect (make-conc-int-double-floats)))
	  (set-state-volumes cint)))	     

       ;; '((CA-ATP 1)(CA-ATP 2)(CA-BASAL 1))
       (loop for pump-type-param in (conc-int-type-pump-type-params type)
	     do (create-pump cint (car pump-type-param) (cadr pump-type-param)))

       ;; To assign this concentration integrator to particles previously defined
       (loop for elt in (node-elements node)
	     when (and (conc-particle-p elt) (equal type (conc-particle-type-conc-int-type (conc-particle-type elt))))
	     do (setf (conc-particle-conc-int elt) cint))
       (push cint (conc-int-type-conc-ints (conc-int-type cint)))
       (setq *conc-int* cint)
       cint))))

(defun set-state-volumes (cint)
  (let ((type (conc-int-type cint)))
    (element-parameter cint 'compartment-volumes
		       (loop for state from 1 to (conc-int-type-number-of-states type)
			     collect (s-flt (let ((volume-function (cadr (assoc state (conc-int-type-volumes type)))))
					      (cond ((numberp volume-function) volume-function)
						    ((functionp volume-function) (funcall volume-function cint))
						    ((fboundp volume-function) (funcall (symbol-function volume-function) cint))
						    (t 0.0))))))
    (element-parameter cint 'total-compartment-volume (s-flt (apply '+ (element-parameter cint 'compartment-volumes))))
    nil))

(defun parse-conc-int-info-for-element (type cell-element)
  ;; For assigning conc-ints to channels and synapses - setting the :CONC-INTS-PARAMS slot. This information is used for evaluating
  ;; the reversal potential of the channel or synapse.  
  (let ((iv-parameters-structure (element-iv-parameters type)))
    (loop for conc-int-type-param in (or (element-parameter type 'CONC-INT-TYPE-E-REV-PARAMS)
					 (membrane-element-type-iv-conc-int-type-params iv-parameters-structure))
	  nconcing (let ((cint (create-conc-int cell-element (car conc-int-type-param))))
		     (loop for shell-perm in (cdr conc-int-type-param) collect
			   (list cint (car shell-perm)
				 (d-flt (* (cadr shell-perm)
					   (cadr (find (conc-int-type-species (conc-int-type cint))
						       (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)
						       :key 'car))))))))))

(defun conc-int-all-pumps (cint) (concatenate 'list (conc-int-shell-1-pumps cint) (conc-int-shell-2-pumps cint) (conc-int-shell-3-pumps cint)))

(defun remove-conc-int-from-pump (conc-int)
  (loop for pump in (conc-int-all-pumps conc-int) when (eq (pump-conc-int pump) conc-int) do (setf (pump-conc-int pump) nil)))

(defun remove-conc-int-from-particle (conc-int)
  (loop for prt in (conc-particles) when (eq (conc-particle-conc-int prt) conc-int) do (setf (conc-particle-conc-int prt) nil)))

(defun remove-conc-int-type-from-particle-type (conc-int-type)
  (loop for type in (conc-particle-types) when (eq (conc-particle-type-conc-int-type type) conc-int-type) do (setf (conc-particle-type-conc-int-type type) nil)))

;;; **************************************************************
;;; Setup Functions
;;; **************************************************************

(defun update-conc-int-type-q10s () (loop for type in (CONC-INT-TYPEs) do (setf (conc-int-type-q10-rate-factor type) (element-q10-rate-factor type))))

(defun remove-conc-int-type-lists (type) (REMOVE-element-PARAMeters type '(conc-int-variable-e-rev-flags active-conc-ints)))
				    
(defun revamp-conc-int-type-lists (type conc-ints)
  (when conc-ints
    (element-parameter
     type 'conc-int-variable-e-rev-flags
     (loop for conc-int in conc-ints collect
	   (loop for elt in (append (channels conc-int) (synapses conc-int))
		 thereis (typecase elt
			   (channel (or (channel-type-variable-e-rev (channel-type elt))
					(member elt *plot-channel-reversal-potentials-structures*)))
			   (synapse (or (synapse-type-variable-e-rev (synapse-type elt))
					(member elt *plot-synapse-reversal-potentials-structures*)))))))
    (element-parameter type 'active-conc-ints conc-ints)))
  
(defun conc-int-has-active-channel (cint) (loop for pore in (conc-int-pores cint) thereis (not (pore-blocked-p pore))))

(defun conc-int-pores (cint)
  (loop for shell-pores in (list (conc-int-shell-1-pores cint) (conc-int-shell-2-pores cint))
	nconc (loop for pore-info in shell-pores collect (conc-int-shell-pore pore-info))))

(defun active-conc-ints () (loop for conc-int in (conc-ints) when (conc-int-active-p conc-int) collect conc-int))

(defun conc-int-active-p (conc-int)
  "Only true when the CONC-INT will actually be evaluated - requires that not only the CONC-INT and it's type not be blocked, but also
that the associated channel is active."
  (let* ((conc-int (element conc-int 'conc-int))
	 (conc-int-type (element-type conc-int)))
    (and conc-int
	 (not (conc-int-blocked conc-int))
	 (conc-int-has-active-channel conc-int)
	 (not (conc-int-type-blocked conc-int-type)))))
    

(defun setup-conc-ints ()
  (declare (optimize (safety 1) (speed 1) (space 0) (compilation-speed 0)))
  (mapcar (lambda (type)
	    (remove-conc-int-type-lists type)
	    (setf (conc-int-type-enabled-for-this-simulation type) nil)
	    (mapcar (lambda (cint) (setf (conc-int-enabled-for-this-simulation cint) nil)) (conc-int-type-conc-ints type)))
	    (conc-int-types))
  (mapcar (lambda (ch-type)
	    (mapcar (lambda (conc-int-type-param) (setf (conc-int-type-enabled-for-this-simulation (element (car conc-int-type-param) 'conc-int-type)) t))
		    (channel-type-conc-int-type-params ch-type)))
	  *channel-type-list*)
  (loop for type in (conc-int-types) when (conc-int-type-enabled-for-this-simulation type) do
	(mapcar (lambda (cint) (setf (conc-int-enabled-for-this-simulation cint) (conc-int-has-active-channel cint)))
		(conc-int-type-conc-ints type)))
  (setq *conc-int-type-list*
	(no-nils
	 (loop for type in (conc-int-types)
	       when (and (not (conc-int-type-blocked type)) (conc-int-type-enabled-for-this-simulation type))
	       collect (let ((conc-ints (loop for conc-int in (conc-int-type-conc-ints type)
					      when (and (not (conc-int-blocked conc-int)) (conc-int-enabled-for-this-simulation conc-int))
					      collect conc-int)))
			 (when conc-ints
			   (update-membrane-element-type-params type)
			   (revamp-conc-int-type-lists type conc-ints)
			   type)))))
  (mapcar (lambda (type)
	    (case (conc-int-type-class type)
	      (:multi-shell
	       (when (and (element-parameter type 'IMPLICIT-CONC-INT-INTEGRATION) ;  *implicit-conc-int-integration*
			  (not (conc-int-type-shell-3-p type))
			  (not (= (the number (conc-int-type-D-12core type)) 0)))
		 (format t "Warning: Multi-shell concentration integrators (~a)~%" type)
		 (format t "with non-zero diffusion btwn 1,2 and core must be integrated explicitly~%")
		 (element-parameter type 'IMPLICIT-CONC-INT-INTEGRATION nil)))))
	  (conc-int-types))
  nil)

(defun set-*conc-int-initializations* ()
  (setq *conc-int-initializations*
	(loop for cint being the hash-value of (conc-int-hash-table)
	      collect
	      (list cint (list (conc-int-shell-1-conc-n+1 cint)
			       (conc-int-shell-2-conc-n+1 cint)
			       (conc-int-shell-3-conc-n+1 cint)
			       (if (conc-int-evaluate-total-concentration cint)
				 (conc-int-total-conc-n+1 cint)
				 (total-concentration-n cint))
			       (conc-int-core-conc cint)))))
  nil)

(defun init-conc-ints ()
  (loop for type in (conc-int-types) when (conc-int-type-conc-ints type) do
	(element-parameter type 'resting-free-conc (s-flt (element-parameter type 'resting-free-conc)))
	(setf (conc-int-type-core-conc type) (resting-concentration type :global))
	(set-conc-int-type-transmembrane-concentrations type)
	(case (conc-int-type-class type)
	  (:general (loop for cint in (conc-int-type-conc-ints type) do (init-general-conc-int cint)))
	  (t (let ((shell-1-init-conc (d-flt (resting-concentration type 1)))
		   (shell-2-init-conc (d-flt (resting-concentration type 2)))
		   (shell-3-init-conc (d-flt (resting-concentration type 3))))
	       (loop for cint in (conc-int-type-conc-ints type)
		     do (init-conc-int cint shell-1-init-conc shell-2-init-conc shell-3-init-conc))))))
  (when *use-conc-int-initializations* (transfer-conc-int-init-values))
  (loop for type in (conc-int-types) when (conc-int-type-conc-ints type) do
	(let ((type-params (conc-int-type-parameters type)))
	  (loop for cint in (conc-int-type-conc-ints type) 
		for e-rev-flag in (get-a-value 'conc-int-variable-e-rev-flags type-params)
		when e-rev-flag do (set-shell-e-revs cint))))
  nil)

(defun init-general-conc-int (cint)
  (loop for initial-value in (element-parameter (conc-int-type cint) 'state-initial-values)
	for state in (conc-int-state-arrays cint)
	do (setf (conc-int-aref-shell-1-conc-n+1 state) initial-value))
  (advance-general-conc-int cint))
  
(defun init-conc-int (cint shell-1-init-conc shell-2-init-conc shell-3-init-conc)
  (setf (conc-int-shell-1-conc-n+1 cint) shell-1-init-conc
	(conc-int-shell-2-conc-n+1 cint) shell-2-init-conc
	(conc-int-shell-3-conc-n+1 cint) shell-3-init-conc
	(conc-int-total-conc-n+1 cint) (conc-int-core-conc-double cint)
	(conc-int-shell-1-dcdt-n cint) 0.0d0
	(conc-int-shell-2-dcdt-n cint) 0.0d0)	  
  (advance-conc-int cint))

(defun transfer-conc-int-init-values ()
  (loop for cint-vals in *conc-int-initializations*
	do (setf (conc-int-shell-1-conc-n+1 (car cint-vals)) (nth 0 (cadr cint-vals))
		 (conc-int-shell-2-conc-n+1 (car cint-vals)) (nth 1 (cadr cint-vals))
		 (conc-int-shell-3-conc-n+1 (car cint-vals)) (nth 2 (cadr cint-vals))
		 (conc-int-total-conc-n+1 (car cint-vals)) (nth 3 (cadr cint-vals))
		 (conc-int-core-conc (car cint-vals)) (nth 4 (cadr cint-vals)))
	(advance-conc-int (car cint-vals))))

;;; **************************************************************

(proclaim '(inline conc-int-juxtamembrane-average-conc))
(defun conc-int-juxtamembrane-average-conc (cint)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (/ (+ (* (conc-int-shell-1-conc-n cint) (conc-int-shell-1-volume cint))
	(* (conc-int-shell-2-conc-n cint) (conc-int-shell-2-volume cint)))
     (conc-int-shell-3-volume cint)))

(proclaim '(inline conc-int-transmembrane-conc)) 
(defun conc-int-transmembrane-conc (cint &optional initial)
  (if (conc-int-transmembrane-integrator cint)
    (if initial
      (conc-int-core-conc-double (conc-int-transmembrane-integrator cint))
      (conc-int-juxtamembrane-average-conc (conc-int-transmembrane-integrator cint)))
    (conc-int-type-transmembrane-conc-double (conc-int-type cint))))

(proclaim '(notinline set-shell-e-revs))
(defun set-shell-e-revs (cint)
  ;; Sets the reversal potential of the ion integrated by the concentration integrator CINT according to the concentration in the
  ;; CINT's shells at *SIM-TIME-N* (i.e. the result of the previous time step).
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((type (conc-int-type cint))
	 (e-rev-function (get-element-parameter-fast 'e-rev-function (conc-int-type-parameters type))))
    (if e-rev-function
      (apply-e-rev-function e-rev-function cint type)
      (let* ((transmembrane-conc (conc-int-transmembrane-conc cint))
	     (intra-p (conc-int-type-intra-p type))
	     (shell-1-inner (if intra-p (the df (conc-int-shell-1-free-conc-n cint)) transmembrane-conc))
	     (shell-1-outer (if intra-p transmembrane-conc (the df (conc-int-shell-1-free-conc-n cint))))
	     (shell-2-inner (if intra-p (the df (conc-int-shell-2-free-conc-n cint)) transmembrane-conc))
	     (shell-2-outer (if intra-p transmembrane-conc (the df (conc-int-shell-2-free-conc-n cint))))
	     (valence (conc-int-type-valence type)))
	(declare (double-float transmembrane-conc shell-1-inner shell-1-outer shell-2-inner shell-2-outer))
	(setf (conc-int-e-rev-shell-1 cint) (nernst-potential-df shell-1-inner shell-1-outer valence))
	(when (conc-int-type-shell-2-p type)
	  (setf (conc-int-e-rev-shell-2 cint) (nernst-potential-df shell-2-inner shell-2-outer valence))))))
  nil)

(defun apply-e-rev-function (e-rev-function cint type)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((transmembrane-conc (conc-int-transmembrane-conc cint))
	 (intra-p (conc-int-type-intra-p type))
	 (shell-1-inner (if intra-p (the df (conc-int-shell-1-free-conc-n cint)) transmembrane-conc))
	 (shell-1-outer (if intra-p transmembrane-conc (the df (conc-int-shell-1-free-conc-n cint))))
	 (shell-2-inner (if intra-p (the df (conc-int-shell-2-free-conc-n cint)) transmembrane-conc))
	 (shell-2-outer (if intra-p transmembrane-conc (the df (conc-int-shell-2-free-conc-n cint))))
	 (e-rev-function (if (functionp e-rev-function) e-rev-function (symbol-function e-rev-function))))
    (declare (double-float transmembrane-conc shell-1-inner shell-1-outer shell-2-inner shell-2-outer))
    (setf (conc-int-e-rev-shell-1 cint)
	  (the df (funcall (the compiled-function e-rev-function) shell-1-inner shell-1-outer type)))
    (when (conc-int-type-shell-2-p type)
      (setf (conc-int-e-rev-shell-2 cint)
	    (the df (funcall (the compiled-function e-rev-function) shell-2-inner shell-2-outer type))))
    nil))

(proclaim '(inline collect-pump-current))
(defun collect-pump-current (cint shell)
  ;; Returns mM/ms
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint)
	   (fixnum shell))
  (loop for pump in (case shell
		      (1 (conc-int-shell-1-pumps cint))
		      (2 (conc-int-shell-2-pumps cint))
		      (3 (conc-int-shell-3-pumps cint)))
	when (pump-enabled-for-this-simulation pump)
	sum (the df (pump-concentration-current
		     pump
		     (* 1.0e-12		; convert um3 to cm3
			(case shell
			  (1 (conc-int-shell-1-volume cint)) 
			  (2 (conc-int-shell-2-volume cint))
			  (t (conc-int-total-volume cint))))))
	into result double-float
	finally (return result)))

(proclaim '(inline generic-conc-int-membrane-current-component))
(defun generic-conc-int-membrane-current-component (cint pore-perm shell)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((type (conc-int-type cint))
	 (valence (conc-int-type-valence type))
	 (shell-conc (case shell
		       (1 (the df (conc-int-shell-1-free-conc-n cint)))
		       (2 (the df (conc-int-shell-2-free-conc-n cint)))
		       (t 0.0d0)))
	 (conc-in (if (conc-int-type-intra-p type) shell-conc (conc-int-type-transmembrane-conc-double type)))
	 (conc-out (if (conc-int-type-intra-p type) (conc-int-type-transmembrane-conc-double type) shell-conc)))
    (declare (double-float shell-conc conc-in conc-out))
    (the df (element-current (car pore-perm) conc-in conc-out valence))))

(proclaim '(inline conc-int-membrane-current-component))
(defun conc-int-membrane-current-component (cint &optional (shell 1))
  "Returns either the concentration derivative [mM/ms], for non :GENERIC integrators, or the current [nA] for :GENERIC integrators, associated with the
concentration compartment SHELL [default 1] of CINT that is due to that compartment's associated :SHELL-PORES and any membrane pumps. The coefficient
:BETA-CURRENT-SH, specific for a given CINT, converts channel current (in nA) to d[x]/dt (in mM/ms). :BETA-CURRENT-SH = 1 for :GENERIC integrators."
;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (- (loop for pore-perm in (case shell
			      (1 (conc-int-shell-1-pores cint))
			      (2 (conc-int-shell-2-pores cint)))
	   summing (* (the df (typecase (car pore-perm)
				(channel (channel-current (car pore-perm)))
				(synapse (synapse-current (car pore-perm)))
				(t (generic-conc-int-membrane-current-component cint pore-perm shell))))
		      ;; Coefficient for current for this ch or syn.
		      (conc-int-shell-pore-perm pore-perm))
	   into result double-float
	   finally (return (* result (the df (case shell ; BETA-CURRENT-X to convert nA to mM/ms
					       (1 (conc-int-Beta-current-1 cint))	
					       (2 (conc-int-Beta-current-2 cint))
					       (t 0.0d0))))))
					; Pump current in mM/ms
     (collect-pump-current cint shell))) 

(proclaim '(inline implicit-eval-multi-shell-conc-int))
#|
(defun implicit-eval-multi-shell-conc-int (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((c1 (conc-int-shell-1-conc-n cint))
	 (c2 (conc-int-shell-2-conc-n cint))
	 (c3 (conc-int-shell-3-conc-n cint))
	 (core (conc-int-core-conc-double cint))
	 (delta-t (*delta-t[n]*))
	 (half-delta-t (* 0.5d0 delta-t))
	 ;; Channel or synaptic current into shells 1 and 2
	 (i1 (conc-int-membrane-current-component cint 1))
	 (i2 (conc-int-membrane-current-component cint 2))		

	 (A-prime-12 (* half-delta-t (conc-int-Beta-2-1 cint)))
	 (A-prime-21 (* half-delta-t (conc-int-Beta-1-2 cint)))
	 (A-prime-13 (* half-delta-t (conc-int-Beta-3-1 cint)))
	 (A-prime-31 (* half-delta-t (conc-int-Beta-1-3 cint)))
	 (A-prime-23 (* half-delta-t (conc-int-Beta-3-2 cint)))
	 (A-prime-32 (* half-delta-t (conc-int-Beta-2-3 cint)))
	 (A-34 (* delta-t (conc-int-Beta-core-3 cint)))
	 (A-prime-34 (* half-delta-t (conc-int-Beta-core-3 cint)))

	 (a (+ 1 A-prime-12 A-prime-13))
	 (b (- A-prime-12))
	 (c (- A-prime-13))
	 
	 (d (- A-prime-21))
	 (ee (+ 1 A-prime-21 A-prime-23))
	 (f (- A-prime-23))
	 
	 (g (- A-prime-31))
	 (h (- A-prime-32))
	 (i (+ 1 A-prime-31 A-prime-32 A-prime-34))
	 
	 (z1 (+ (* delta-t i1)
		(* c1 (- 1 (+ A-prime-12 A-prime-13)))
		(* c2 A-prime-12)
		(* c3 A-prime-13)))
	 
	 (z2 (+ (* delta-t i2)
		(* c1 A-prime-21)
		(* c2 (- 1 (+ A-prime-21 A-prime-23)))
		(* c3 A-prime-23)))

	 (z3 (+ (* c1 A-prime-31)
		(* c2 A-prime-32)
		(* c3 (- 1 (+ A-prime-31 (+ A-prime-32 A-prime-34))))
		(* core A-34)))
	 (det-A (+ (* a (- (* ee i)
			   (* h f)))
		   (* d (- (* h c)
			   (* b i)))
		   (* g (- (* b f)
			   (* ee c))))))

    (setf (conc-int-shell-1-conc-n+1 cint) (/ (+ (* z1 (- (* ee i)
							  (* h f)))
						 (* z2 (- (* h c)
							  (* b i)))
						 (* z3 (- (* b f)
							  (* ee c))))
					      det-A))
 
    (setf (conc-int-shell-2-conc-n+1 cint) (/ (+ (* a (- (* z2 i)
							 (* z3 f)))
						 (* d (- (* z3 c)
							 (* z1 i)))
						 (* g (- (* z1 f)
							 (* z2 c))))
					      det-A))
 
    (setf (conc-int-shell-3-conc-n+1 cint) (/ (+ (* a (- (* ee z3)
							 (* h z2)))
						 (* d (- (* h z1)
							 (* b z3)))
						 (* g (- (* b z2)
							 (* ee z1))))
					      det-A))
    nil))
|#

(proclaim '(inline 4-shell-implicit-eval-multi-shell-conc-int))
(defun 4-shell-implicit-eval-multi-shell-conc-int (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((type (conc-int-type cint))
	 (c1 (conc-int-shell-1-conc-n cint))
	 (c2 (conc-int-shell-2-conc-n cint))
	 (c3 (conc-int-shell-3-conc-n cint))
	 (core (conc-int-core-conc-double cint))
	 (delta-t (*delta-t[n]*))
	 (half-delta-t (* 0.5d0 delta-t))
	 ;; Channel or synaptic current into shells 1 and 2
	 (i1 (conc-int-membrane-current-component cint 1))
	 (i2 (conc-int-membrane-current-component cint 2))		
	     
	 (dt/2-beta-2-1 (* half-delta-t (conc-int-Beta-2-1 cint)))
	 (A-prime-121 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-1 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-1))
	 (A-prime-211 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-1 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-1))

	 (dt/2-beta-1-2 (* half-delta-t (conc-int-Beta-1-2 cint)))
	 (A-prime-122 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-2 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-2))
	 (A-prime-212 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-2 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-2))

	 (dt/2-beta-3-1 (* half-delta-t (conc-int-Beta-3-1 cint)))
	 (A-prime-131 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-1 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-1))
	 (A-prime-311 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-1 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-1))
	 
	 (dt/2-beta-1-3 (* half-delta-t (conc-int-Beta-1-3 cint)))
	 (A-prime-133 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-3 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-3))
	 (A-prime-313 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-3))
	 
	 (dt/2-beta-3-2 (* half-delta-t (conc-int-Beta-3-2 cint)))
	 (A-prime-322 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-2 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-2))
	 (A-prime-232 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-2 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-2))
	 
	 (dt/2-beta-2-3 (* half-delta-t (conc-int-Beta-2-3 cint)))
	 (A-prime-233 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-3 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-3))
	 (A-prime-323 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-3))

	 (dt/2-beta-core-3 (* half-delta-t (conc-int-Beta-core-3 cint)))
	 (A-prime-343 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-core-3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-core-3))
	 (A-433 (if (conc-int-type-instantaneous-buffer-enabled type)
		  (/ (conc-int-Beta-core-3 cint) (conc-int-type-global-instantaneous-buffer-ratio+1 type))
		  (conc-int-Beta-core-3 cint)))

	 (a (+ 1 A-prime-121 A-prime-131))
	 (b (- A-prime-211))
	 (c (- A-prime-311))
	 
	 (d (- A-prime-122))
	 (ee (+ 1 A-prime-212 A-prime-232))
	 (f (- A-prime-322))
	 
	 (g (- A-prime-133))
	 (h (- A-prime-233))
	 (i (+ 1 A-prime-313 A-prime-323 A-prime-343))
	 
	 (z1 (+ (* delta-t i1)
		(* c1 (- 1 (+ A-prime-121 A-prime-131)))
		(* c2 A-prime-211)
		(* c3 A-prime-311)))
	 
	 (z2 (+ (* delta-t i2)
		(* c1 A-prime-122)
		(* c2 (- 1 (+ A-prime-212 A-prime-232)))
		(* c3 A-prime-322)))

	 (z3 (+ (* c1 A-prime-133)
		(* c2 A-prime-233)
		(* c3 (- 1 (+ A-prime-313 (+ A-prime-323 A-prime-343))))
		(* core A-433)))
	 (det-A (+ (* a (- (* ee i)
			   (* h f)))
		   (* d (- (* h c)
			   (* b i)))
		   (* g (- (* b f)
			   (* ee c))))))

    (setf (conc-int-shell-1-conc-n+1 cint) (/ (+ (* z1 (- (* ee i)
							  (* h f)))
						 (* z2 (- (* h c)
							  (* b i)))
						 (* z3 (- (* b f)
							  (* ee c))))
					      det-A))
 
    (setf (conc-int-shell-2-conc-n+1 cint) (/ (+ (* a (- (* z2 i)
							 (* z3 f)))
						 (* d (- (* z3 c)
							 (* z1 i)))
						 (* g (- (* z1 f)
							 (* z2 c))))
					      det-A))
 
    (setf (conc-int-shell-3-conc-n+1 cint) (/ (+ (* a (- (* ee z3)
							 (* h z2)))
						 (* d (- (* h z1)
							 (* b z3)))
						 (* g (- (* b z2)
							 (* ee z1))))
					      det-A))
    nil))

(proclaim '(inline 3-shell-no-core-implicit-eval-multi-shell-conc-int))
(defun 3-shell-no-core-implicit-eval-multi-shell-conc-int (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((type (conc-int-type cint))
	 (c1 (conc-int-shell-1-conc-n cint))
	 (c2 (conc-int-shell-2-conc-n cint))
	 (c3 (conc-int-shell-3-conc-n cint))
	 (delta-t (*delta-t[n]*))
	 (half-delta-t (* 0.5d0 delta-t))
	 ;; Channel or synaptic current into shells 1 and 2
	 (i1 (conc-int-membrane-current-component cint 1))
	 (i2 (conc-int-membrane-current-component cint 2))		
	     
	 (dt/2-beta-2-1 (* half-delta-t (conc-int-Beta-2-1 cint)))
	 (A-prime-121 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-1 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-1))
	 (A-prime-211 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-1 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-1))

	 (dt/2-beta-1-2 (* half-delta-t (conc-int-Beta-1-2 cint)))
	 (A-prime-122 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-2 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-2))
	 (A-prime-212 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-2 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-2))

	 (dt/2-beta-3-1 (* half-delta-t (conc-int-Beta-3-1 cint)))
	 (A-prime-131 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-1 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-1))
	 (A-prime-311 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-1 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-1))
	 
	 (dt/2-beta-1-3 (* half-delta-t (conc-int-Beta-1-3 cint)))
	 (A-prime-133 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-3 (conc-int-type-shell-1-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-3))
	 (A-prime-313 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-1-3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-1-3))
	 
	 (dt/2-beta-3-2 (* half-delta-t (conc-int-Beta-3-2 cint)))
	 (A-prime-322 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-2 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-2))
	 (A-prime-232 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-3-2 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-3-2))
	 
	 (dt/2-beta-2-3 (* half-delta-t (conc-int-Beta-2-3 cint)))
	 (A-prime-233 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-3 (conc-int-type-shell-2-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-3))
	 (A-prime-323 (if (conc-int-type-instantaneous-buffer-enabled type)
			(/ dt/2-beta-2-3 (conc-int-type-shell-3-instantaneous-buffer-ratio+1 type))
			dt/2-beta-2-3))

	 (a (+ 1 A-prime-121 A-prime-131))
	 (b (- A-prime-211))
	 (c (- A-prime-311))
	 
	 (d (- A-prime-122))
	 (ee (+ 1 A-prime-212 A-prime-232))
	 (f (- A-prime-322))
	 
	 (g (- A-prime-133))
	 (h (- A-prime-233))
	 (i (+ 1 A-prime-313 A-prime-323))
	 
	 (z1 (+ (* delta-t i1)
		(* c1 (- 1 (+ A-prime-121 A-prime-131)))
		(* c2 A-prime-211)
		(* c3 A-prime-311)))
	 
	 (z2 (+ (* delta-t i2)
		(* c1 A-prime-122)
		(* c2 (- 1 (+ A-prime-212 A-prime-232)))
		(* c3 A-prime-322)))

	 (z3 (+ (* c1 A-prime-133)
		(* c2 A-prime-233)
		(* c3 (- 1 (+ A-prime-313 A-prime-323)))))
	 (det-A (+ (* a (- (* ee i)
			   (* h f)))
		   (* d (- (* h c)
			   (* b i)))
		   (* g (- (* b f)
			   (* ee c))))))

    (setf (conc-int-shell-1-conc-n+1 cint) (/ (+ (* z1 (- (* ee i)
							  (* h f)))
						 (* z2 (- (* h c)
							  (* b i)))
						 (* z3 (- (* b f)
							  (* ee c))))
					      det-A))
 
    (setf (conc-int-shell-2-conc-n+1 cint) (/ (+ (* a (- (* z2 i)
							 (* z3 f)))
						 (* d (- (* z3 c)
							 (* z1 i)))
						 (* g (- (* z1 f)
							 (* z2 c))))
					      det-A))
 
    (setf (conc-int-shell-3-conc-n+1 cint) (/ (+ (* a (- (* ee z3)
							 (* h z2)))
						 (* d (- (* h z1)
							 (* b z3)))
						 (* g (- (* b z2)
							 (* ee z1))))
					      det-A))
    nil))

(defun implicit-eval-multi-shell-conc-int (cint)
  (if (= (conc-int-Beta-core-1 cint)
	 (conc-int-Beta-core-2 cint)
	 (conc-int-Beta-core-3 cint) 0)
    (3-shell-no-core-implicit-eval-multi-shell-conc-int cint)
    (4-shell-implicit-eval-multi-shell-conc-int cint)))

(defun eval-general-conc-int (cint equation-coefficient-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((Xi (the vec-df (find-element-array cint 'xi (conc-int-type-number-of-states (conc-int-type cint))))))
    (loop for state in (conc-int-state-arrays cint)
	  for index fixnum from 0 do
	  (setf (aref xi index) (conc-int-aref-shell-1-conc-n state)))
    (let ((result (the vec-df (solve-matrix-system equation-coefficient-array Xi (*delta-t[n]*) :element cint))))
      (loop for state in (conc-int-state-arrays cint)
	    for index fixnum from 0 do
	    (setf (conc-int-aref-shell-1-conc-n+1 state) (aref result index)))))
  nil)
      
(defun transfer-general-conc-int-states-to-shells (cint)
  (setf (conc-int-shell-1-conc-n+1 cint) 0.0d0
	(conc-int-shell-2-conc-n+1 cint) 0.0d0
	(conc-int-shell-3-conc-n+1 cint) 0.0d0)
  (loop for state in (conc-int-state-arrays cint)
	for shell in (element-parameter (conc-int-type cint) 'compartment-shells)
	do (case shell
	     (1 (setf (conc-int-shell-1-conc-n+1 cint) (+ (conc-int-shell-1-conc-n+1 cint)
							  (conc-int-aref-shell-1-conc-n+1 state))))
	     (2 (setf (conc-int-shell-2-conc-n+1 cint) (+ (conc-int-shell-2-conc-n+1 cint)
							  (conc-int-aref-shell-1-conc-n+1 state))))
	     (3 (setf (conc-int-shell-3-conc-n+1 cint) (+ (conc-int-shell-3-conc-n+1 cint)
							  (conc-int-aref-shell-1-conc-n+1 state))))))
  nil)
   
(defun eval-generic-dcdt-conc-int (cint dcdt-function)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((type (conc-int-type cint))
	 (c1 (conc-int-shell-1-conc-n cint))
	 (delta-t (*delta-t[n]*))
	 (dcdt-function (if (functionp dcdt-function) dcdt-function (symbol-function dcdt-function))))
    (dcdt (funcall dcdt-function cint))) ; mM/ms
  (setf (conc-int-shell-1-conc-n+1 cint) (max-double-macro (conc-int-core-conc-double cint) (+ c1 (* delta-t dcdt (conc-int-type-q10-rate-factor type)))))
  nil)

(defun eval-generic-c-n+1-conc-int (cint c-n+1-function)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let ((c-n+1-function (if (functionp c-n+1-function) c-n+1-function (symbol-function c-n+1-function))))
    (setf (conc-int-shell-1-conc-n+1 cint) (max-double-macro (conc-int-core-conc-double cint) (funcall c-n+1-function cint)))
    nil))

(defun eval-generic-user-spec-conc-int (cint conc-function)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let ((conc-function (if (functionp conc-function) conc-function (symbol-function conc-function))))
    (funcall conc-function cint))
  nil)

(proclaim '(inline eval-first-order-conc-int))
(defun eval-first-order-conc-int (cint tau)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint)
	   (single-float tau))
  (let* ((c1 (conc-int-shell-1-conc-n cint))
	 (core (conc-int-core-conc cint))
	 (delta-t (*delta-t[n]*))
	 (half-delta-t (* 0.5d0 delta-t))
	 ;; Channel, synaptic and pump current into shells 1
	 (total-current (conc-int-membrane-current-component cint 1)))
    ;; (printvars  cint  total-current core c1)
    (setf (conc-int-shell-1-conc-n+1 cint)
	  (max-double-macro
	   (conc-int-core-conc-double cint)
	   (if (> tau 0)
	     (/ (- (* c1 (- half-delta-t tau))
		   (+ (* delta-t (- (* tau total-current) core))))
		(- (+ tau half-delta-t)))
	     (+ (conc-int-shell-1-conc-n cint) (* delta-t total-current))))))
  nil)

#|
(defun EXPLICIT-EVAL-multi-shell-CONC-INT (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((c1 (conc-int-shell-1-conc-n cint))
	 (c2 (conc-int-shell-2-conc-n cint))
	 (c3 (conc-int-shell-3-conc-n cint))
	 (delta-t (*delta-t[n]*))

	 ;; Channel, synaptic, and pump current into shells 1 and 2
	 (i1 (conc-int-membrane-current-component cint 1))
	 (i2 (conc-int-membrane-current-component cint 2))

	 (total-i1 (+ (* (- c2 c1) (conc-int-Beta-2-1 cint))
		      (* (- c3 c1) (conc-int-Beta-3-1 cint))
		      i1))
	 (total-i2 (+ (* (- c1 c2) (conc-int-Beta-1-2 cint))
		      (* (- c3 c2) (conc-int-Beta-3-2 cint))
		      i2))
	 (total-i3 (+ (* (- c1 c3) (conc-int-Beta-1-3 cint))
		      (* (- c2 c3) (conc-int-Beta-2-3 cint))
		      (* (- (conc-int-core-conc-double cint) c3) (conc-int-Beta-core-3 cint)))))
    (setf (conc-int-shell-1-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c1 (* delta-t total-i1)))
	  (conc-int-shell-2-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c2 (* delta-t total-i2)))
	  (conc-int-shell-3-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c3 (* delta-t total-i3)))))
  nil)
|#

(defun EXPLICIT-EVAL-multi-shell-CONC-INT (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (let* ((type (conc-int-type cint))
	 (c1 (conc-int-shell-1-conc-n cint))
	 (c2 (conc-int-shell-2-conc-n cint))
	 (c3 (conc-int-shell-3-conc-n cint))
	 (delta-t (*delta-t[n]*))

	 (free-c1 (the df (conc-int-shell-1-free-conc-n cint)))
	 (free-c2 (the df (conc-int-shell-2-free-conc-n cint)))
	 (free-c3 (the df (conc-int-shell-3-free-conc-n cint)))
	 (free-core (the df (conc-int-core-free-conc cint)))
	 
	 ;; Channel, synaptic, and pump current into shells 1 and 2
	 (i1 (conc-int-membrane-current-component cint 1)) ; [mM/ms]
	 (i2 (conc-int-membrane-current-component cint 2)) ; [mM/ms]

	 (total-i1 (+ (* (- free-c2 free-c1) (conc-int-Beta-2-1 cint)) ; (- mM mM) * 1/ms = mM/ms
		      (* (- free-c3 free-c1) (conc-int-Beta-3-1 cint))
		      (* (- free-core free-c1) (conc-int-Beta-core-1 cint))
		      i1))
	 (total-i2 (+ (* (- free-c1 free-c2) (conc-int-Beta-1-2 cint))
		      (* (- free-c3 free-c2) (conc-int-Beta-3-2 cint))
		      (* (- free-core free-c2) (conc-int-Beta-core-2 cint))
		      i2))
	 (total-i3 (+ (* (- free-c1 free-c3) (conc-int-Beta-1-3 cint))
		      (* (- free-c2 free-c3) (conc-int-Beta-2-3 cint))
		      ; (* (- free-core free-c1) (conc-int-Beta-core-1 cint))
		      ; (* (- free-core free-c2) (conc-int-Beta-core-2 cint))
		      (* (- free-core free-c3) (conc-int-Beta-core-3 cint)))))
    (setf (conc-int-shell-1-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c1 (* delta-t total-i1)))
	  (conc-int-shell-2-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c2 (* delta-t total-i2)))
	  (conc-int-shell-3-conc-n+1 cint)
	  (max-double-macro (conc-int-core-conc-double cint) (+ c3 (* delta-t total-i3)))))
  nil)

(proclaim '(inline eval-multi-shell-conc-int))
(defun eval-multi-shell-conc-int (cint &optional implicit-p)
  (if implicit-p (IMPLICIT-EVAL-MULTI-SHELL-CONC-INT cint) (EXPLICIT-EVAL-multi-shell-CONC-INT cint))
  nil)

(proclaim '(inline calculate-conc-int-error))
(defun calculate-conc-int-error (cint)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type conc-int cint))
  (setf (conc-int-shell-1-dcdt-n cint)
	(/ (- (conc-int-shell-1-conc-n+1 cint) (conc-int-shell-1-conc-n cint))
	   (*delta-t[n]*)))
  (when (conc-int-type-shell-2-p (conc-int-type cint))
    (setf (conc-int-shell-2-dcdt-n cint)
	  (/ (- (conc-int-shell-2-conc-n+1 cint) (conc-int-shell-2-conc-n cint))
	     (*delta-t[n]*))))
  (let ((d2cdt2-numerator (if (conc-int-type-shell-2-p (conc-int-type cint))
			    (max (- (conc-int-shell-1-dcdt-n cint) (conc-int-shell-1-dcdt-n-1 cint))
				 (- (conc-int-shell-2-dcdt-n cint) (conc-int-shell-1-dcdt-n-1 cint)))
			    (- (conc-int-shell-1-dcdt-n cint) (conc-int-shell-1-dcdt-n-1 cint)))))
    (when (and *debug-conc-int-error*
	       (cond ((> d2cdt2-numerator (*maximum-conc-int-error-numerator*))
		      (setf (*maximum-conc-int-error-numerator*) d2cdt2-numerator))
		     ((> (the df (- d2cdt2-numerator)) (*maximum-conc-int-error-numerator*))
		      (setf (*maximum-conc-int-error-numerator*) (the df (- d2cdt2-numerator))))))
      (setq *conc-int-w-max-error* (element-name cint))))
  nil)

(proclaim '(inline finish-conc-int-eval))
(defun finish-conc-int-eval (cint set-e-rev)
  (cond-every
   ((conc-int-evaluate-total-concentration cint)
    (setf (conc-int-total-conc-n+1 cint) (total-concentration-n cint)))
   (*calculate-conc-int-error* (calculate-conc-int-error cint))
   (set-e-rev (set-shell-e-revs cint))
   nil))

(defun eval-first-order-conc-ints (type cints e-rev-flags)
  (let* ((params (conc-int-type-parameters type))
	 (tau (/ (the sf (element-parameter type 'tau))
		 (conc-int-type-q10-rate-factor type))))
    (mapcar (lambda (cint e-rev-flag) 
	      (eval-first-order-conc-int cint tau)
	      (finish-conc-int-eval cint e-rev-flag))
	    cints e-rev-flags)
    nil))

(defun eval-generic-conc-ints (type cints e-rev-flags)
  (let ((conc-function (element-parameter type 'conc-function))
	(c-n+1-function (element-parameter type 'c-n+1-function))
	(dcdt-function (element-parameter type 'dcdt-function)))
    (mapcar (lambda (cint e-rev-flag) 
	      (cond (conc-function (eval-generic-user-spec-conc-int cint conc-function))
		    (c-n+1-function (eval-generic-c-n+1-conc-int cint c-n+1-function))
		    (dcdt-function (eval-generic-dcdt-conc-int cint dcdt-function)))
	      (finish-conc-int-eval cint e-rev-flag))
	    cints e-rev-flags)
    nil))

(defun eval-multi-shell-conc-ints (type cints e-rev-flags &optional implicit-p)
  (mapcar (lambda (cint e-rev-flag) 
	    (eval-multi-shell-conc-int cint implicit-p)
	    (finish-conc-int-eval cint e-rev-flag))
	  cints e-rev-flags)
  nil)

(defun eval-general-conc-ints (type cints e-rev-flags)
  (mapcar (lambda (cint e-rev-flag) 
	    (eval-general-conc-int cint)
	    (finish-conc-int-eval cint e-rev-flag))
	  cints e-rev-flags)
  nil)

(defun eval-all-conc-ints ()
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (mapcar (lambda (type) 
	    (let* ((type-params (conc-int-type-parameters type))
		   (cints (get-a-value 'active-conc-ints type-params))
		   (e-rev-flags (get-a-value 'conc-int-variable-e-rev-flags type-params))
		   (implicit-p (get-a-value 'IMPLICIT-CONC-INT-INTEGRATION type-params)))
	      (case (conc-int-type-class type)
		(:general (eval-general-conc-ints type cints e-rev-flags))
		(:FIRST-ORDER (eval-first-order-conc-ints type cints e-rev-flags))
		(:multi-shell (eval-multi-shell-conc-ints type cints e-rev-flags implicit-p))
		(:generic (eval-generic-conc-ints type cints e-rev-flags)))))
	  *conc-int-type-list*)
  nil)
  
(defun advance-conc-ints ()
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (mapcar (lambda (type) 
	    (let* ((type-params (conc-int-type-parameters type))
		   (cints (get-a-value 'active-conc-ints type-params)))
	      (case (conc-int-type-class type)
		(:general (loop for cint in cints do (advance-general-conc-int cint)))
		(t (loop for cint in cints do (advance-conc-int cint))))))
	  *conc-int-type-list*)
  nil)

(defun advance-general-conc-int (cint)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (mapcar (lambda (state) (setf (conc-int-aref-shell-1-conc-n state) (conc-int-aref-shell-1-conc-n+1 state))) (conc-int-state-arrays cint))
  (when (conc-int-evaluate-total-concentration cint) (setf (conc-int-total-conc-n cint) (conc-int-total-conc-n+1 cint)))
  (transfer-general-conc-int-states-to-shells cint)
  nil)

(defun advance-conc-int (cint)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setf (conc-int-shell-1-conc-n cint) (conc-int-shell-1-conc-n+1 cint)
	(conc-int-shell-1-dcdt-n-1 cint) (conc-int-shell-1-dcdt-n cint)
	(conc-int-shell-2-conc-n cint) (conc-int-shell-2-conc-n+1 cint)
	(conc-int-shell-2-dcdt-n-1 cint) (conc-int-shell-2-dcdt-n cint)
	(conc-int-shell-3-conc-n cint) (conc-int-shell-3-conc-n+1 cint))
  (when (conc-int-evaluate-total-concentration cint) (setf (conc-int-total-conc-n cint) (conc-int-total-conc-n+1 cint)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-conc-integrators-parameters ()
  ;; This must be called after the cell element dimensions are known.
  (maphash 'set-conc-integrator-type-parameters (CONC-INT-TYPE-HASH-TABLE))
  (maphash 'set-conc-integrator-parameters (CONC-INT-HASH-TABLE))
  (setq *recheck-circuit-elements-parameters* t))

(defun conc-int-volume (cint shell &optional element-conc-volume element-area)
  ;; Returns a double float in um^3.  
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let ((cint (element cint 'conc-int)))
    (when cint 
      (let* ((type (conc-int-type cint))
	     (volume-function (cadr (assoc shell (conc-int-type-volumes type)))))
	(if (not (conc-int-type-shell-p type shell))
	    0.0d0
	    (d-flt
	     (cond ((numberp volume-function) volume-function)
		   ((functionp volume-function) (funcall volume-function cint))
		   ((fboundp volume-function) (funcall (symbol-function volume-function) cint))
		   (t			; Default compartment volume formulae.
		    (let ((juxtamembrane-volume (juxtamembrane-shell-volume cint element-conc-volume element-area)))
		      (case shell		
			(1 (* juxtamembrane-volume (conc-int-type-alpha-s type)))
			(2 (* juxtamembrane-volume (- 1.0 (conc-int-type-alpha-s type))))
			(3 (let* ((total-conc-vol (or element-conc-volume (element-concentration-volume cint)))
				  (avail-conc-vol (max 0.0d0 (- total-conc-vol juxtamembrane-volume)))
				  (inner-shell-volume (* (conc-int-inner-shell-thickness cint) (or element-area (element-area cint)))))
			     (cond
			       ((and (> (conc-int-juxtamembrane-shell-thickness cint) 0)
				     (> (conc-int-inner-shell-thickness cint) 0))
				(if (conc-int-type-intra-p type) (min avail-conc-vol inner-shell-volume) inner-shell-volume))
			       ;; JUXTA SHELL THICKNESS = 0 => JUXTA SHELL INCLUDES TOTAL VOLUME
			       ((= (conc-int-juxtamembrane-shell-thickness cint) 0) 0.0d0)
			       ;; INNER SHELL 3 THICKNESS = 0 => SHELL 3 IS TOTAL VOLUME MINUS THE JUXTA VOLUME.
			       ((= (conc-int-inner-shell-thickness cint) 0)
				(if (conc-int-type-intra-p type) avail-conc-vol inner-shell-volume))
			       (t 0.0d0))))
			(core
			 (if (not (conc-int-type-intra-p type))
			     0.0d0	; No "core" for extracellular integrators 
			     (let* ((total-conc-vol (or element-conc-volume (element-concentration-volume cint)))
				    (shell-1-vol (shell-1-volume cint))
				    (shell-2-vol (shell-2-volume cint))
				    (shell-3-vol (shell-3-volume cint)))
			       (cond
				 ((or (= (conc-int-juxtamembrane-shell-thickness cint) 0)
				      (and (>= (conc-int-juxtamembrane-shell-thickness cint) 0)
					   (= (conc-int-inner-shell-thickness cint) 0)))
				  0.0d0)
				 ((and (>= (conc-int-juxtamembrane-shell-thickness cint) 0)
				       (>= (conc-int-inner-shell-thickness cint) 0))
				  (max 0.0d0 (- total-conc-vol (+ shell-1-vol shell-2-vol shell-3-vol))))
				 (t
				  (max 0.0d0 (- total-conc-vol (+ shell-1-vol shell-2-vol))))))))))))))))))


(defun conc-int-volume (cint shell &optional element-conc-volume element-area)
  ;; Returns a double float in um^3.
  ;; July 31, 2002 Conform to Table 2 in concentration integrator chapter of User Manual
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let ((cint (element cint 'conc-int)))
    (when cint 
      (let* ((type (conc-int-type cint))
	     (volume-function (cadr (assoc shell (conc-int-type-volumes type)))))
	(if (not (conc-int-type-shell-p type shell))
	    0.0d0
	    (d-flt
	     (cond ((numberp volume-function) volume-function)
		   ((functionp volume-function) (funcall volume-function cint))
		   ((fboundp volume-function) (funcall (symbol-function volume-function) cint))
		   (t			; Default compartment volume formulae.
		    (implicit-conc-int-volume cint shell (or element-conc-volume (element-concentration-volume cint)) (or element-area (element-area cint)))))))))))

(defun juxtamembrane-shell-volume (cint &optional (element-concentration-volume (element-concentration-volume cint)) (element-area (element-area cint)))
  ;; In um^3. Returns the smaller of the ELEMENT-CONCENTRATION-VOLUME, in um3, and the product of the juxtamembrane-shell-thickness and element-area (in um2).
  (if (> (conc-int-juxtamembrane-shell-thickness cint) 0)
      (min element-concentration-volume
	   (* (conc-int-juxtamembrane-shell-thickness cint) ; um
	      element-area)) ; um2
      element-concentration-volume))

(defun implicit-conc-int-volume (cint shell element-conc-volume element-area)
  (let* ((type (conc-int-type cint))
	 (D_juxta (conc-int-type-juxtamembrane-shell-thickness type))
	 (D_inner (conc-int-type-inner-shell-thickness type))
	 (juxtamembrane-volume (* D_juxta ; um juxtamembrane-shell-volume cint element-conc-volume element-area)
				  element-area)) ; um2
	 (inner-volume (* D_inner element-area))
	 (alpha-s (conc-int-type-alpha-s type)))
    (when (and (not (conc-int-type-intra-p type))
	       (or (zerop D_juxta)
		   (and (conc-int-type-shell-3-p type)
			(zerop D_inner))
		   (conc-int-type-core-p type)))
      (sim-error (format nil "Extracellular conc-int-type ~A has bogus parameters!" type)))
    (case shell		
      (1 (if (zerop D_juxta)
		(if (conc-int-type-shell-2-p type)
		    (* alpha-s element-conc-volume)
		    element-conc-volume))
	       (if (conc-int-type-shell-2-p type)
		   (* juxtamembrane-volume alpha-s)
		   juxtamembrane-volume))
      (2 (unless (conc-int-type-shell-2-p type) (sim-error (format nil "Conc-int-type ~A has no shell 2!" type)))
	 (if (zerop D_juxta)
	     (* element-conc-volume (- 1.0 alpha-s)))
	     (* juxtamembrane-volume (- 1.0 alpha-s)))
      (3 (unless (conc-int-type-shell-3-p type) (sim-error (format nil "Conc-int-type ~A has no shell 3!" type)))
	 (if (zerop D_inner)
	     (- element-conc-volume juxtamembrane-volume)
	     inner-volume))
      (core (unless (conc-int-type-core-p type) (sim-error (format nil "Conc-int-type ~A has no core!" type)))
	    (if (conc-int-type-shell-3-p type)
		(- element-conc-volume juxtamembrane-volume inner-volume)
		(- element-conc-volume juxtamembrane-volume))))))

;; These functions all return volumes in um^3.
(defun SHELL-1-VOLUME (cint &optional element-conc-volume element-area) (conc-int-volume cint 1 element-conc-volume element-area))
(defun SHELL-2-VOLUME (cint &optional element-conc-volume element-area) (conc-int-volume cint 2 element-conc-volume element-area))
(defun shell-3-volume (cint &optional element-conc-volume element-area) (conc-int-volume cint 3 element-conc-volume element-area))

(defun core-volume (cint &optional element-conc-volume element-area)
  "Returns the core volume of CINT in um^3. Optional ELEMENT-CONC-VOLUME is also in um^3."
  (conc-int-volume cint 'core element-conc-volume element-area))

(defun set-conc-integrator-type-parameters (name type) (declare (ignore name type)))
    
(defun conc-int-shell-membrane-area (cint shell &optional element-area)
  "Returns the surface area of one face of SHELL of CINT in um2. Optional ELEMENT-AREA is area of the associated cell element in um2."
  (let ((cint (element cint 'conc-int)))
    (when cint
      (let* ((type (conc-int-type cint))
	     (area-function (cadr (assoc shell (conc-int-type-membrane-areas type)))))
	(cond ((numberp area-function) area-function)
	      ((functionp area-function) (funcall area-function cint))
	      ((fboundp area-function) (funcall (symbol-function area-function) cint))
	      (t			; Default :MULTI-SHELL shell membrane areas.
	       (* (case shell
		    (1 (conc-int-type-alpha-s type))
		    (2 (- 1 (conc-int-type-alpha-s type)))
		    (t 0.0))
		  (or element-area (element-area cint)))))))))

(defun interdigitation-area (cint &optional element-area)
  "Returns the diffusion membrane area between shells 1 and 2 of CINT in um2 for the :MULTI-SHELL class. Optional ELEMENT-AREA is
area of the associated cell element in um2."
  (let ((type (conc-int-type cint))
	(area (or element-area (element-area cint))))
    (if (or (< (conc-int-type-interdigitation-coefficient type) 0)
	    (<= (conc-int-type-juxtamembrane-shell-thickness type) 0))
      area
      (* (conc-int-type-interdigitation-coefficient type) ; 1/microns
	 area			
	 (conc-int-type-juxtamembrane-shell-thickness type)))))

(defun conc-int-diff-area (cint shell-x shell-y &optional element-area)
  "Returns the double-float diffusional area between SHELL-X and SHELL-Y of CINT in cm2. Optional ELEMENT-AREA is area of the
associated cell element in um2."
  (let ((type (element-type cint))
	(shells (list shell-x shell-y)))
    (when (conc-int-type-p type)
      (let ((diff-area-function
	     (car (or (get-a-value shells (conc-int-type-diffusion-areas type) 'equal)
		      (get-a-value (reverse shells) (conc-int-type-diffusion-areas type) 'equal)))))
	(d-flt
	 (cond ((numberp diff-area-function) diff-area-function)
	       ((functionp diff-area-function) (funcall diff-area-function cint))
	       ((fboundp diff-area-function) (funcall (symbol-function diff-area-function) cint))
	       (t			; Default :MULTI-SHELL areas
		(let ((element-area (or element-area (element-area cint))))
		  (cond ((and (member 1 shells) (member 2 shells))
			 ;; INTERDIG-AREA-CM2
			 (the df (* 1.0d-8 (interdigitation-area cint element-area))))
			((and (member 1 shells) (member 3 shells))
			 ;; SHELL-1-AREA-CM2
			 (the df (* 1.0d-8 (conc-int-shell-membrane-area cint 1 element-area))))
			((and (member 2 shells) (member 3 shells))
			 ;; SHELL-2-AREA-CM2
			 (the df (* 1.0d-8 (conc-int-shell-membrane-area cint 2 element-area))))
			((and (member 1 shells) (member 'core shells))
			 ;; SHELL-1-AREA-CM2
			 (the df (* 1.0d-8 (conc-int-shell-membrane-area cint 1 element-area))))
			((and (member 2 shells) (member 'core shells))
			 ;; SHELL-2-AREA-CM2
			 (the df (* 1.0d-8 (conc-int-shell-membrane-area cint 2 element-area))))
			((and (member 3 shells) (member 'core shells))
			 ;; ELEMENT-AREA-CM2
			 (* 1.0d-8 element-area)))))))))))

#|
    ;; For beta-i-j coefficients (values for working-hpc):
    ;;

(/ (* D-12				; 8.0e-6 (cm2/sec) 0.001 sec/ms
    (conc-int-diff-area cint 1 2 elt-area)) ; = 3.8484511718750003d-5 (cm^2)
   (* distance-12			; 0.001 (cm)
  shell-1-vol-cm3))		; 3.8484510625604285d-13 cm3
					; = 800.0000175074103d0 1/sec
|#

(defun set-conc-integrator-parameters (name cint)
  ;; For setting diffusion constants that depend on geometric and conc-int type parameters.
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0))
	   (ignore name))
  (let* ((type (conc-int-type cint))
	 (class (conc-int-type-class type))
	 (cell-element (conc-int-cell-element cint))
	 (D-13 (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-13 type)))) ; cm2/sec
	 (D-3core (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-3core type))))
	 (D-23 (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-23 type))))
	 (D-2core (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-2core type))))
	 (D-1core (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-1core type))))
	 (D-12 (the sf (* (conc-int-type-q10-rate-factor type) (conc-int-type-D-12 type))))

	 (Distance-13 (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-13 type))))) ; cm
	 (Distance-3core (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-3core type)))))
	 (Distance-23 (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-23 type)))))
	 (Distance-2core (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-2core type)))))
	 (Distance-1core (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-1core type)))))
	 (Distance-12 (the sf (* 1.0e-4 (the sf (conc-int-type-Distance-12 type)))))

	 (elt-area (the sf (element-area cell-element))) ; um2
	 
	 ;; Negative anionic current flows from outside to inside.
	 (current-sign (if (conc-int-type-intra-p type) -1.0d0 1.0d0))

	 (shell-1-vol (SHELL-1-VOLUME cint nil elt-area))
	 (shell-2-vol (SHELL-2-VOLUME cint nil elt-area))
	 (shell-3-vol (shell-3-volume cint nil elt-area))
	 (core-vol (core-volume cint nil elt-area))

	 (shell-1-vol-cm3 (* 1.0d-12 SHELL-1-VOl))
	 (shell-2-vol-cm3 (* 1.0d-12 SHELL-2-VOL))
	 (shell-3-vol-cm3 (* 1.0d-12 shell-3-vol))
	 (core-vol-cm3 (* 1.0d-12 core-vol))

	 (total-conc-vol (if (conc-int-type-intra-p type)
			     (element-concentration-volume cell-element)
			     (+  shell-1-vol shell-2-vol shell-3-vol core-vol))) ; um^3
	 (total-conc-vol-cm3 (* 1.0d-12 total-conc-vol))

;;	 (shell-3-vol-cm3 (* 1.0d-12 (shell-3-volume cint total-conc-vol elt-area)))
;;	 (shell-1-vol-cm3 (* 1.0d-12 (SHELL-1-VOLUME cint total-conc-vol elt-area)))
;;	 (shell-2-vol-cm3 (* 1.0d-12 (SHELL-2-VOLUME cint total-conc-vol elt-area)))
;;	 (core-vol-cm3 (* 1.0d-12 (core-volume cint total-conc-vol elt-area)))
	 
	 (current-shell-base-coeff
	  ;; This coefficent is in units of

	  ;;     (1e-9 A/nA) (1e-3 ms/s) (1e3 cm^3/L) (1e3 milliMolar/Molar)
	  ;;     -----------------------------------------------------------
	  ;;                 Faraday (Coulombs/mole)

	  (* (the df (or (element-parameter cint 'current-shell-base-coefficient-factor) 1.0d0))  
	     (/ (the df (* current-sign 1d-6)) (* (conc-int-type-valence type) Faraday)))))

    ;; Set compartment volumes.
    (setf (conc-int-shell-1-volume cint) (* 1.0d12 shell-1-vol-cm3)) ; um3
    (when (conc-int-type-shell-2-p type)
      (setf (conc-int-shell-2-volume cint) (* 1.0d12 shell-2-vol-cm3))) ; um3
    (when (conc-int-type-shell-3-p type)
      (setf (conc-int-shell-3-volume cint) (* 1.0d12 shell-3-vol-cm3))) ; um3
    (when (conc-int-type-core-p type)
      (setf (conc-int-core-volume cint) (* 1.0d12 core-vol-cm3))) ; um3
    (setf (conc-int-total-volume cint) (* 1.0d12 total-conc-vol-cm3)) ; um3

    ;; Set all appropriate BETA coefficients, the constants of proportionality between either
    ;; differences of compartments' (i and j) free concentrations, or ionic current entering a
    ;; compartment j, and d[X]_j/dt.

    (cond-every
     ((> shell-1-vol-cm3 0)		; FUNCTIONAL SHELL 1
      (setf (conc-int-Beta-current-1 cint) (case class
					     (:generic 1.0d0)
					     (t (/ current-shell-base-coeff shell-1-vol-cm3))))
      (when (and (conc-int-type-shell-2-p type) (> shell-2-vol-cm3 0)) ; FUNCTIONAL SHELL 2
	(setf (conc-int-Beta-2-1 cint) (/ (* D-12 ; cm2/sec
					     (the df (conc-int-diff-area cint 1 2 elt-area)) ; cm2
					     0.001) ; sec/ms
					  (* distance-12 ; cm
					     shell-1-vol-cm3 ; cm3
					     ))
	      (conc-int-Beta-1-2 cint) (/ (* D-12 (the df (conc-int-diff-area cint 1 2 elt-area))
					     0.001) ; sec/ms
					  (* distance-12 shell-2-vol-cm3))))
      (if (and (conc-int-type-shell-3-p type) (> shell-3-vol-cm3 0)) ; FUNCTIONAL SHELL 3
	(setf (conc-int-Beta-1-3 cint) (/ (* D-13 (the df (conc-int-diff-area cint 1 3 elt-area))
					     0.001) ; sec/ms
					  (* distance-13 shell-3-vol-cm3))
	      (conc-int-Beta-3-1 cint) (/ (* D-13 (the df (conc-int-diff-area cint 1 3 elt-area))
					     0.001) ; sec/ms
					  (* distance-13  shell-1-vol-cm3)))
	(when (and (conc-int-type-core-p type) (> core-vol-cm3 0)) ; FUNCTIONAL CORE & NO SHELL 3
	  (setf (conc-int-Beta-core-1 cint)
		(/ (* D-1core (the df (conc-int-diff-area cint 1 'core elt-area))
		      0.001)		; sec/ms
		   (* distance-1core shell-1-vol-cm3))))))

     ((and (conc-int-type-shell-2-p type) (> shell-2-vol-cm3 0)) ; FUNCTIONAL SHELL 2
      (setf (conc-int-Beta-current-2 cint) (case class
					     (:generic 1.0d0)
					     (t (/ current-shell-base-coeff shell-2-vol-cm3))))
      (if (and (conc-int-type-shell-3-p type) (> shell-3-vol-cm3 0)) ; FUNCTIONAL SHELL 3.     
	(setf (conc-int-Beta-2-3 cint)  (/ (* D-23 (the df (conc-int-diff-area cint 2 3 elt-area))
					      0.001) ; sec/ms
					   (* distance-23 shell-3-vol-cm3))
	      (conc-int-Beta-3-2 cint) (/ (* D-23 (the df (conc-int-diff-area cint 2 3 elt-area))
					     0.001) ; sec/ms
					  (* distance-23 shell-2-vol-cm3)))
	(when (and (conc-int-type-core-p type) (> core-vol-cm3 0)) ; FUNCTIONAL CORE & NO SHELL 3.
	  (setf (conc-int-Beta-core-2 cint)
		(/ (* D-2core (the df (conc-int-diff-area cint 1 2 elt-area))
		      0.001)		; sec/ms
		   (* distance-2core shell-2-vol-cm3))))))

     ((and (conc-int-type-shell-3-p type) (> shell-3-vol-cm3 0)	; FUNCTIONAL SHELL 3 AND CORE.
	   (conc-int-type-core-p type) (> core-vol-cm3 0))
      (setf (conc-int-Beta-core-3 cint) (/ (* D-3core (the df (conc-int-diff-area cint 3 'core elt-area))
					      0.001) ; sec/ms
					   (* distance-3core shell-3-vol-cm3)))))

    (set-conc-int-current-elements cint)
    (set-conc-int-pumps cint))
  nil)

(defun set-conc-int-pumps (cint) (mapcar 'set-pump-area (conc-int-all-pumps cint)))

(defun set-conc-int-current-elements (conc-int)
  ;; Find all the channels and synapses that this integrator should be looking at and collect these elements along with their
  ;; current coefficients appropriate for shell 1 or 2.
  (let* ((type (conc-int-type conc-int))
	 (cell-element (conc-int-cell-element conc-int))
	 (conc-int-type-sym (conc-int-type-name type)))
    (setf (conc-int-shell-1-pores conc-int) nil
	  (conc-int-shell-2-pores conc-int) nil)
    (loop for elt in (node-elements (element-node cell-element)) do
	  (loop for conc-int-type-param in (element-conc-int-type-params elt)
		when (eq conc-int-type-sym (car conc-int-type-param)) do
		(loop for conc-int-type-shell-param in (cdr conc-int-type-param) do
		      (let* ((permeability (or (d-flt (cadar (member (the symbol (conc-int-type-species type))
									 (element-ion-permeabilities (element-type elt))
									 :key 'car)))
					       1.0d0))
			     (delta (or (element-parameter elt 'conc-int-delta) 1.0))
			     (pore-info (list elt (the df (* permeability delta
							     ;; Current proportion
							     (cadr conc-int-type-shell-param))))))
			(case (the fn (car conc-int-type-shell-param))
			  (1 (push pore-info (conc-int-shell-1-pores conc-int)))
			  (2 (if (conc-int-type-shell-2-p type)
			       (push pore-info (conc-int-shell-2-pores conc-int))
			       (sim-error (format nil "Setting up ~A with channels that expect shell 2!" conc-int)))))))))))

(defun print-diffusion-areas (cint)
  (let* ((type (element-type cint 'conc-int))
	 (core-connected-to-shells (and (conc-int-type-core-p type)
					(not (and (= (conc-int-type-D-3core type) 0) (= (conc-int-type-D-12core type) 0))))))
    (case (conc-int-type-class type)
      (:multi-shell (cond-every
		     ((or (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type) core-connected-to-shells)
		      (format t "    Diffusion area [cm^2] between shell pairs:~%   "))
		     ((and (conc-int-type-shell-2-p type) (> (conc-int-type-D-12 type) 0))
		      (format t "   A-12 ~,2e " (s-flt (conc-int-diff-area cint 1 2))))
		     ((and (conc-int-type-shell-3-p type) (> (conc-int-type-D-13 type) 0))
		      (format t "   A-13 ~,2e " (s-flt (conc-int-diff-area cint 1 3))))
		     ((and (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type)
			   (> (conc-int-type-D-23 type) 0))
		      (format t "   A-23 ~,2e " (s-flt (conc-int-diff-area cint 2 3))))
		     (core-connected-to-shells
		      (if (and (conc-int-type-shell-3-p type) (> (conc-int-type-D-3core type) 0))
			(format t "   A-3core ~,2e" (s-flt (conc-int-diff-area cint 3 'core)))
			(cond-every
			 ((> (conc-int-type-D-1core type) 0)
			  (format t "   A-1core ~,2e" (s-flt (conc-int-diff-area cint 1 'core))))
			 ((and (conc-int-type-shell-2-p type) (> (conc-int-type-D-2core type) 0))
			  (format t "   A-2core ~,2e" (s-flt (conc-int-diff-area cint 2 'core)))))))
		     ((or (conc-int-type-shell-2-p type) (conc-int-type-shell-3-p type) core-connected-to-shells)
		      (format t "~%")))))))

(defun print-conc-int (cint)
  (let* ((cint (element cint 'conc-int))
	 (type (element-type cint 'conc-int)))
    (when cint
      (format t "~a: type ~a~%" (massage-element-plot-label cint nil t) (conc-int-type-name (conc-int-type cint)))
      ;; (when (element-parameter cint 'ss-concentration) (format t "   Steady-state conc ~,2emM~%" (element-parameter cint 'ss-concentration)))
      (when (conc-int-type-blocked (conc-int-type cint))
	(format t "    *** Type Disabled ***~%"))
      (format t "    Shell ~A volume(s) ~a,~A total volume ~,2e um3 ~%"
	      (concatenate-strings "1" (when (conc-int-type-shell-2-p type) "/2") (when (conc-int-type-shell-3-p type) "/3"))
	      (concatenate-strings
	       (format nil "~,2e" (s-flt (conc-int-shell-1-volume cint)))
	       (when (conc-int-type-shell-2-p type) (format nil "/~,2e" (s-flt (conc-int-shell-2-volume cint))))
	       (when (conc-int-type-shell-3-p type) (format nil "/~,2e" (s-flt (conc-int-shell-3-volume cint)))))
	      (if (conc-int-type-core-p type)
		(format nil " core volume ~,2e," (s-flt (conc-int-core-volume cint))) "")
	      (conc-int-total-volume cint))
      (when *simulation-initialized*
	(format t "    Shell ~A free concentration(s) ~a~A mM, total conc ~,2e mM @ ~,2e ms~%"
		(concatenate-strings "1" (when (conc-int-type-shell-2-p type) "/2") (when (conc-int-type-shell-3-p type) "/3"))
		(concatenate-strings
		 (format nil "~,2e" (conc-int-shell-1-free-conc-n cint))
		 (when (conc-int-type-shell-2-p type) (format nil "/~,2e" (s-flt (conc-int-shell-3-free-conc-n cint))))
		 (when (conc-int-type-shell-3-p type) (format nil "/~,2e" (s-flt (conc-int-shell-3-free-conc-n cint)))))
		(if (conc-int-type-core-p type)	(format nil ", core free conc ~,2e" (s-flt (conc-int-core-free-conc *conc-int*))) "")
		(s-flt (conc-int-total-free-conc-n+1 cint)) *real-time*))
      (print-diffusion-areas cint)
      (loop for chperlist in (list (conc-int-shell-1-pores cint) (conc-int-shell-2-pores cint))
	    for label in '(" Shell 1" " Shell 2")
	    when chperlist do
	    (format t "   ~a pores/relative-permeabilities*deltas:~%" label)
	    (loop for chperm in chperlist ; (conc-int-shell-1-pores cint)
		  do (format t "      ~a ~,4f~%" (element-name (conc-int-shell-pore chperm)) (conc-int-shell-pore-perm chperm)))))))

#|
(defun conc-int-error-ok ()
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (or (not *calculate-conc-int-error*)
      (if (<= (*maximum-conc-int-error-numerator*) 1.0d-10) ; Getting problems with values around 1.0e-320.

	(setq *conc-int-error-max-time-step* *max-step*) ; Error Ok

	(let* ((d2m-ndt2 (/ (* 4.0d0 (*maximum-conc-int-error-numerator*))
			    (* mrt (- (- (+ *sim-time-n+1* *sim-time-n*) *sim-time-n-1*) *sim-time-n-2*))))

	       ;; This is the maximum time step allowed given the specified *RELATIVE-CONC-INT-ERROR* and the current
	       ;; maximum conc-int state second derivative.
	       (conc-int-conduction-state-error
		(- (kernel::%sqrt (/ *twice-relative-conc-int-error* d2m-ndt2)) (*half-delta-t[n-1]*)))
	       
	       (error-value conc-int-conduction-state-error)

	       ;; The maximum time step allowed as above, in units of mrt.
	       (conc-int-error-max-time-step
		(if (> error-value 0)
		  (let ((float-step (the df (* *pick-time-step-fudge* (/ error-value mrt)))))
		    (if (< float-step most-positive-fixnum-float-version) (round float-step) most-positive-fixnum))
		  *min-step*)))
	  (declare (type fixnum conc-int-error-max-time-step))
	  (unless (= conc-int-error-max-time-step *conc-int-error-max-time-step*)
	    (setq *conc-int-error-max-time-step* conc-int-error-max-time-step))

	  ;; Return NIL if we are stepping too far.
	  (or (<= *time-step* *min-step*)
	      (<= (*delta-t[n]*) error-value))))))
|#




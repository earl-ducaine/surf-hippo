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


;;; SYS Source file: general-membrane-elements.lisp
					
;; Functions that are applied to both channels and synapses.

(in-package "SURF-HIPPO")

(defun membrane-element-type-iv-source-menu-string (type)
  (case (membrane-element-type-iv-relation (element-iv-parameters type))
    (:ohmic "Source for reference conductance:")
    (:constant-field "Source for reference permeability:")))

(defun membrane-element-type-iv-relation-absolute-reference-menu-string (type)
  (case (membrane-element-type-iv-relation (element-iv-parameters type))
    (:ohmic "Absolute Conductance Reference [uS]")
    (:constant-field "Absolute Permeability Reference [cm3/s]")))

(defun membrane-element-type-iv-density-menu-string (type)
  (case (membrane-element-type-iv-relation (element-iv-parameters type))
    (:ohmic "Conductance Density Reference [pS/um2]")
    (:constant-field "Permeability Density Reference [1.0e-6cm3/s/um2]")))

(defun element-type-param (element param &optional (value nil value-supplied-p) (update t))
  (format t "Use of ELEMENT-TYPE-PARAM is deprecated (used here with ELEMENT arg ~A and PARAM arg ~A). Use IV-TYPE-PARAMETER instead.~&" element param)
  (if value-supplied-p
      (iv-type-parameter element param value update)
      (iv-type-parameter element param)))

(defun iv-type-parameter (element param &optional (value nil value-supplied-p) (update t))
  "For examining/setting specific structure parameters of the synapse or channel type associated with ELEMENT. PARAM can be:

     :IV-SOURCE (e.g. :ABSOLUTE or :DENSITY)
     :IV-REFERENCE [for :ABSOLUTE gbar (uS) or permeability (cm3/sec)]
     :IV-DENSITY [pS/um2 (0.1mS per square cm) for gbar, 1.0e-6 cm3/sec/um2 for permeability]
     :IV-MODULATION [applied to all type children, regardless of inheritance]
     :E-REV [mV]
     :BLOCKED [T or NIL]

If no new VALUE follows the PARAM, then the current value of the slot corresponding to PARAM is returned. Supplying a non-nil
value for UPDATE will cause the change to propagate to the appropriate elements of the type associated with ELEMENT."
  (let ((value (if (numberp value) (s-flt value) value))) 
    (atomize-list
     (loop for type in (flatten-no-nils-list (element-type element)) collect
	   (let ((return-value
		  (case param
		    (:iv-reference (if value-supplied-p (element-slot type :iv-reference (s-flt value) update) (element-slot type :iv-reference)))
		    (:iv-source (if value-supplied-p (element-slot type :iv-source value update) (element-slot type :iv-source)))
		    (:iv-density (if value-supplied-p (element-slot type :iv-density (s-flt value) update) (element-slot type :iv-density)))
		    (:blocked (if value-supplied-p (element-slot type :blocked (true-p value) update) (element-slot type :blocked)))
		    (:e-rev (if value-supplied-p (element-slot type :e-rev (s-flt value) update) (element-slot type :e-rev)))
		    (t (if value-supplied-p (element-slot-or-parameter-function type param value update) (element-slot-or-parameter-function type param))))))
	     (when (and value-supplied-p (not (eq value return-value)))
	       (unless update (setq *recheck-circuit-elements-parameters* t))
	       (typecase type
		 (channel-type (setq *enable-channel-membrane-parameter-update* t))
		 (synapse-type (setq *enable-synapse-membrane-parameter-update* t))))
	     return-value)))))

(defun element-type-parameter (element param &optional (value nil value-supplied-p) (update t))
  "Set or examine the PARAM of the element type associated with ELEMENT. If VALUE is supplied, PARAM will be set to the new VALUE and updated accordingly
when UPDATE is T [default]. In general, legal PARAM are the keywords listed in the appropriate TYPE-DEF macro. For example, for particle types, the
possible parameters are given in the documentation of PARTICLE-TYPE-DEF. For cell types see also the documentation for CELL-TYPE-PARAMETER, which applies
to ELEMENT-TYPE-PARAMETER as well."
  (let ((type (element-type element)))
    (unless type (sim-error (format nil "~A does not refer to any element type!" element)))
    (typecase type
      (particle-type (if value-supplied-p (particle-type-parameter type param value update) (particle-type-parameter type param)))
      ((or channel-type synapse-type) (if value-supplied-p (iv-type-parameter type param value update) (iv-type-parameter type param)))
      (cell-type (if value-supplied-p (cell-type-parameter type param value update) (cell-type-parameter type param))))))

(defun channel-type-parameter (element param &optional (value nil value-supplied-p) (update t))
  (let ((element (or (element element 'channel-type) (element-type (element element 'channel)))))
    (if value-supplied-p (iv-type-parameter element param value update) (iv-type-parameter element param))))

(defun synapse-type-parameter (element param &optional (value nil value-supplied-p) (update t))
  (let ((element (or (element element 'synapse-type) (element-type (element element 'synapse)))))
    (if value-supplied-p (iv-type-parameter element param value update) (iv-type-parameter element param))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pore-blocked-p (pore)
  "Predicate for the block of a channel or synapse PORE, either due to an individual block or block of the associated type. If
PORE is a channel or synapse type, then return T if that type is blocked."
  (let ((pore (element pore)))
    (typecase pore
      (channel-type (channel-type-blocked pore))
      (synapse-type (synapse-type-blocked pore))
      (channel (or (channel-blocked pore) (channel-type-blocked (element-type pore))))
      (synapse (or (synapse-blocked pore) (synapse-type-blocked (element-type pore)))))))

(defun element-has-intracellular-conc-ints (element)
  (loop for conc-int-type-param in (element-conc-int-type-params element)
	thereis (and (conc-int-type-intra-p (element (car conc-int-type-param)))
		     (not (= 0 (element-type-conc-int-type-param-permeability conc-int-type-param))))))

(defun element-has-extracellular-conc-ints (element)
  (loop for conc-int-type-param in (element-conc-int-type-params element)
	thereis (and (not (conc-int-type-intra-p (element (car conc-int-type-param))))
		     (not (= 0 (element-type-conc-int-type-param-permeability conc-int-type-param))))))

(proclaim '(inline get-element-conc-ints-conc-in))
(defun get-element-conc-ints-conc-in (elt conc-ints-params &optional fast-conc-in-calculation)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (if (and fast-conc-in-calculation conc-ints-params)
    (let ((conc-int-param (car conc-ints-params)))
      (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
	 (case (element-conc-int-param-shell conc-int-param)
	   (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
	   (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
	   (t 0.0d0))))
    (loop for conc-int-param in conc-ints-params
	  when (conc-int-type-intra-p (conc-int-type (element-conc-int-param-cint conc-int-param)))
	  summing (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
		     (case (element-conc-int-param-shell conc-int-param)
		       (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (t 0.0d0)))
	  into result double-float
	  finally (return (if (> result 0)
			    result
			    (element-parameter (element-type elt) 'effective-default-intracellular-concentration))))))

(proclaim '(inline get-element-conc-ints-conc-out))
(defun get-element-conc-ints-conc-out (elt conc-ints-params &optional fast-conc-out-calculation)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (if (and fast-conc-out-calculation conc-ints-params)
    (let ((conc-int-param (car conc-ints-params)))
      (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
	 (case (element-conc-int-param-shell conc-int-param)
	   (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
	   (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
	   (t 0.0d0))))
    (loop for conc-int-param in conc-ints-params
	  unless (conc-int-type-intra-p (conc-int-type (element-conc-int-param-cint conc-int-param)))
	  summing (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
		     (case (element-conc-int-param-shell conc-int-param)
		       (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (t 0.0d0)))
	  into result double-float
	  finally
	  (return (if (> result 0)
		    result
		    (element-parameter (element-type elt) 'effective-default-extracellular-concentration))))))

(defun massage-conc-int-type-params (CONC-INT-TYPE-PARAMS)
  ;; For both channel and synapse types. Result goes into the :CONC-INT-TYPE-PARAMS slot.
  (loop for conc-int-type-param in CONC-INT-TYPE-PARAMS
	collect (cons (car conc-int-type-param)	; Conc-int-type symbol
		      (loop for shell-param in (cdr conc-int-type-param)
			    collect (list (car shell-param) ; Shell 1 or 2
					  (d-flt (cadr shell-param)) ; Current proportion
					  )))))

(defun update-membrane-element-type-params (type)
  (update-membrane-element-type-effective-concentrations type))


(defun update-membrane-element-type-effective-concentrations (type)
  (element-parameter type 'effective-default-valence (s-flt (effective-default-valence type)))
  (element-parameter type 'effective-default-valence-fn (round (element-parameter type 'effective-default-valence)))
  (element-parameter type 'intracellular-conc-ints (element-has-intracellular-conc-ints type))
  (element-parameter type 'extracellular-conc-ints (element-has-extracellular-conc-ints type))
  (element-parameter type 'effective-default-intracellular-concentration (d-flt (effective-default-intracellular-concentration type)))
  (element-parameter type 'effective-default-extracellular-concentration (d-flt (effective-default-extracellular-concentration type))))

(defun clean-up-membrane-element-type-parameters-to-be-updated (type original-parameters)
  ;; Called by create-synapse-type and create-channel-type, when the update argument is set. For membrane element types such as synapse types and
  ;; channel types, revamping a type definition from the library definition will only redefine parameters given in the library definition. For some
  ;; properties it is important to clear them out if they are not explicitly part of the library definition.
  (loop for attribute in '(IV-MODULATION)
	unless (assoc attribute original-parameters)
	do (element-parameter type attribute nil)))

(defun clear-conductance-type-extra-parameters (type)
  (element-parameter type :IV-MODULATION nil)
  (element-parameter type 'conductance-function nil)
  (element-parameter type 'static-voltage-dependence-function nil))

(defun clear-up-iv-relation-references-from-parent-type (parent-type child-type-original-parameters)
  ;; Used for inheritance of channel and synapse types. We need to remove gbar or perm keys in order to allow update or change of
  ;; the absolute versus density type specs.
  (let ((original-spec-density-p (or (assoc 'GBAR-DENSITY child-type-original-parameters) (assoc 'PERMEABILITY-DENSITY child-type-original-parameters)))
	(original-spec-absolute-p (or (assoc 'GBAR child-type-original-parameters) (assoc 'PERMEABILITY child-type-original-parameters))))
    (loop for key-value in (element-parameters parent-type)
	  when (or (and original-spec-absolute-p (member (car key-value) '(GBAR-DENSITY PERMEABILITY-DENSITY)))
		   (and original-spec-density-p (member (car key-value) '(GBAR PERMEABILITY))))
	  do (element-parameter parent-type (car key-value) nil))))

(defun extract-conductance-type-parameters (type type-parameters)
  ;; Common parameter extraction from TYPE-PARAMETERS for both channel and synapse types.
  (clear-conductance-type-extra-parameters type)
  (let ((iv-parameters-structure (element-iv-parameters type)))
    (cond-every
     ((or (assoc 'GBAR type-parameters) (assoc 'PERMEABILITY type-parameters))
      (setf (membrane-element-type-iv-source iv-parameters-structure) :absolute)
      (setf (membrane-element-type-iv-reference iv-parameters-structure)
	    (s-flt (or (get-a-value 'GBAR type-parameters)
		       (get-a-value 'PERMEABILITY type-parameters)))))
     ((or (assoc 'GBAR-DENSITY type-parameters) (assoc 'PERMEABILITY-DENSITY type-parameters))
      (setf (membrane-element-type-iv-source iv-parameters-structure) :density)
      (setf (membrane-element-type-iv-density iv-parameters-structure)
	    (s-flt (or (get-a-value 'gbar-density type-parameters) (get-a-value 'permeability-density type-parameters)))))
     ((or (assoc :IV-MODULATION type-parameters) (assoc 'gbar-MODULATION type-parameters))
      (element-parameter type :IV-MODULATION
			 (or (get-a-value :IV-MODULATION type-parameters) (get-a-value 'gbar-MODULATION type-parameters))))
     ((or (assoc 'E-REV type-parameters) (assoc 'USE-DEFINED-E-REV type-parameters))
      (setf (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)
	    (or (not (assoc 'use-defined-e-rev type-parameters))
		(cdr-assoc 'use-defined-e-rev type-parameters))))
     ((assoc 'E-REV type-parameters)
      (setf (membrane-element-type-iv-e-rev iv-parameters-structure)
	    (s-flt (get-a-value 'e-rev type-parameters))))
     ((or (assoc 'ION-PERMEABILITIES type-parameters)
	  (assoc 'ION-PERMS type-parameters))
      (setf (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)
	    (let ((ion-perms (or (get-a-value 'ion-permeabilities type-parameters)
				 (get-a-value 'ion-perms type-parameters))))
	      (typecase ion-perms
		(cons (loop for ion-perm in ion-perms collect (list (car ion-perm) (float (cadr ion-perm)))))
		(t (list (list ion-perms 1.0)))))))

     ((assoc 'REFERENCE-TEMP type-parameters)
      (setf (membrane-element-type-iv-reference-temp iv-parameters-structure) (s-flt (get-a-value 'reference-temp type-parameters))))
     ((or (assoc 'QTEN type-parameters) (assoc 'Q10 type-parameters))
      (setf (membrane-element-type-iv-q10 iv-parameters-structure) (s-flt (or (get-a-value 'qten type-parameters) (get-a-value 'q10 type-parameters)))))
     ((assoc 'CONDUCTANCE-FUNCTION type-parameters)
      (element-parameter type 'conductance-function (get-a-value 'conductance-function type-parameters)))
     ((assoc 'STATIC-VOLTAGE-DEPENDENCE-function type-parameters)
      (element-parameter type 'static-voltage-dependence
			 (s-flt-array
			  (let ((funspec-or-sequence (get-a-value 'static-voltage-dependence-function type-parameters)))
			    (unless (numeric-sequence-p funspec-or-sequence)
			      (element-parameter type 'static-voltage-dependence-function funspec-or-sequence))
      			    (cond
			     ((numeric-sequence-p funspec-or-sequence) funspec-or-sequence)
			     ((consp funspec-or-sequence) (apply (car funspec-or-sequence) (cdr funspec-or-sequence)))
			     ((fboundp funspec-or-sequence) (funcall funspec-or-sequence))))))))
    (setf (membrane-element-type-iv-relation iv-parameters-structure)
	  (or (get-a-value 'iv-relation type-parameters)
	      (if (or (assoc 'PERMEABILITY-DENSITY type-parameters) (assoc 'PERMEABILITY type-parameters))
		:constant-field
		:ohmic)))
    (when (and (eq (membrane-element-type-iv-relation iv-parameters-structure) :CONSTANT-FIELD)
	       (not (element-conc-int-type-params type))
	       (not (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)))
      (sim-error (format nil "~A requires an 'ionic-perms term for :CONSTANT-FIELD model" (element-name type))))
    
    (setf (membrane-element-type-iv-variable-e-rev iv-parameters-structure)
	  (and (element-conc-int-type-params type)
	       (not (eq (membrane-element-type-iv-relation iv-parameters-structure) :CONSTANT-FIELD))
	       (or (get-a-value 'use-variable-e-rev type-parameters)
		   (not (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)))))
    (unless (or (and (> (membrane-element-type-iv-density iv-parameters-structure) 0)
		     (eq :density (membrane-element-type-iv-source iv-parameters-structure)))
		(and (membrane-element-type-iv-reference iv-parameters-structure)
		     (eq :absolute (membrane-element-type-iv-source iv-parameters-structure))))
      (setf (membrane-element-type-iv-source iv-parameters-structure)
	    (if (> (membrane-element-type-iv-density iv-parameters-structure) 0)
	      :density :absolute)))
    (unless (or (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)
		(membrane-element-type-iv-ion-permeabilities iv-parameters-structure))
      (sim-error (format nil "~a must have 'ion-perms specified!" type)))))  

(defun print-general-element-stuff (type)
  (when (element-parameter type 'parent-type)
    (format t "    Parameters inherited from type ~a~%" (element-parameter type 'parent-type))))

(defun print-inheritance-info (children)
  (let ((inherit 0) (non-inherit 0))
    (loop for child in children
	  when (element-slot child :inherit-parameters-from-type) do (incf inherit) else do (incf non-inherit))
    (cond ((and (= inherit 0) (> non-inherit 0)) (format t "    All child instances have independent gbars~%"))
	  ((and (> inherit 0) (> non-inherit 0))
	   (format t "    ~d% child instances have independent gbars~%" (* 100.0 (/ non-inherit (+ non-inherit inherit))))))))

(defun print-membrane-element-type-iv-parameters (type)
  (let* ((iv-parameters-structure (element-iv-parameters type))
	 (constant-field-p (eq :CONSTANT-FIELD (membrane-element-type-iv-relation iv-parameters-structure))))
    (if constant-field-p
      (case (membrane-element-type-iv-source iv-parameters-structure)
	(:density
	 (format t "Permeability density ~,2e 1.0e-6cm3/s/um2, " (s-flt (membrane-element-type-iv-density iv-parameters-structure))))
	(:absolute
	 (format t "Absolute Permeability ~,2e cm3/s, " (s-flt (membrane-element-type-iv-reference iv-parameters-structure)))))
      (case (membrane-element-type-iv-source iv-parameters-structure)
	(:density
	 (format t "Gbar density ~,2e pS/um2, " (s-flt (membrane-element-type-iv-density iv-parameters-structure))))
	(:absolute
	 (format t "Absolute gbar ~,2e uS, " (s-flt (membrane-element-type-iv-reference iv-parameters-structure))))))
    (format t "(~A Q10 ~,2f, T_ref ~,2e)~%"
	    (if constant-field-p "perm" "gbar")
	    (s-flt (membrane-element-type-iv-q10 iv-parameters-structure))
	    (s-flt (membrane-element-type-iv-reference-temp iv-parameters-structure)))
    (unless (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0)
      (format t "    GBAR modulation by ~,2e~%" (s-flt (element-parameter type :IV-MODULATION))))
    (if constant-field-p
      (format t "    Constant Field Permeability Model~%")
      (if (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)
	(format t "    Fixed E-rev ~,2f mV~%" (s-flt (current-element-type-e-rev type)))
	(format t "    E-rev: ~,2f mV [set w/concentrations according to ion-permeabilites]~%" (s-flt (current-element-type-e-rev type)))))
    (when (membrane-element-type-iv-variable-e-rev iv-parameters-structure)
      (format t "    Variable E-rev enabled~%"))
    (when (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)
      (format t  "    Ion Permeabilities:")
      (loop for ion-perm in (membrane-element-type-iv-ion-permeabilities iv-parameters-structure) do
	    (format t  " ~,1f% ~a" (s-flt (* 100 (cadr ion-perm))) (car ion-perm)))
      (format t  "~%"))))

(defvar *document-element-type-def-decimals* 2 "For automatic element TYPE-DEF form generation by the DOCUMENT-ELEMENT function,
this is the number of decimal values used for numeric parameters.")

(defun document-structure-slot-type-def (structure-name slot structure-instance &optional (indent 0))
  (let ((STRUCTURE-SLOT-ACCESSOR (read-from-string (format nil "~A-~A" structure-name slot))))
    (when (fboundp STRUCTURE-SLOT-ACCESSOR)
      (let* ((STRUCTURE-SLOT-ACCESSOR-doc-string (documentation STRUCTURE-SLOT-ACCESSOR 'function))
	     (value (funcall STRUCTURE-SLOT-ACCESSOR structure-instance))
	     (value-string (typecase value
			     (number (tidy-number-format value :default-decimals *document-element-type-def-decimals*))
			     (t (format nil "~S" value)))))
	(print-spaces t indent)
	(format t "(~A . ~a)~A~%"
		slot
		value-string
		(if STRUCTURE-SLOT-ACCESSOR-doc-string (format nil "  ; ~A" STRUCTURE-SLOT-ACCESSOR-doc-string) ""))))))

;; (document-structure-slot-type-def structure-name slot iv-parameters-structure 8)
				  
(defun document-iv-parameters (type)
  (let* ((iv-parameters-structure (element-iv-parameters type))
	 (iv-relation (membrane-element-type-iv-relation iv-parameters-structure))
	 (reference-documentation (documentation 'membrane-element-type-iv-reference 'function))
	 (density-documentation (documentation 'membrane-element-type-iv-density 'function)))
    (case iv-relation
      (:constant-field
       (case (membrane-element-type-iv-source iv-parameters-structure)
	 (:ABSOLUTE
	  ;; (document-structure-slot-type-def 'membrane-element-type-iv 'gbar-ref iv-parameters-structure 8)
	  (format t "  (permeability . ~a) ; ~A~%" (membrane-element-type-iv-reference iv-parameters-structure) reference-documentation))
	 (:density
	  (format t "  (permeability-density . ~a) ; ~A~%" (membrane-element-type-iv-density iv-parameters-structure) density-documentation))))
      (:ohmic
       (case (membrane-element-type-iv-source iv-parameters-structure)
	 (:ABSOLUTE (format t "  (gbar . ~a) ; ~A~%" (membrane-element-type-iv-reference iv-parameters-structure) reference-documentation))
	 (:density  (format t "  (gbar-density . ~a) ; ~A~%" (membrane-element-type-iv-density iv-parameters-structure) density-documentation)))))
    (when (and (element-parameter type :IV-MODULATION)
	       (/= 1.0 (element-parameter type :IV-MODULATION)))
      (format t "  (IV-MODULATION . ~a)~%" (element-parameter type :IV-MODULATION)))
    (when (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)
      (format t "  (e-rev . ~a) ; mV~%" (membrane-element-type-iv-e-rev iv-parameters-structure)))
    (format t "  (use-defined-e-rev . ~a)~%" (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure))
    (when (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)
      (format t "  (ion-permeabilities . ~a)~%" (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)))
    (format t "  (q10 . ~a)~%" (membrane-element-type-iv-q10 iv-parameters-structure))
    (format t "  (reference-temp . ~a) ; Centigrade~%" (membrane-element-type-iv-reference-temp iv-parameters-structure))
    (cond-every
     ((membrane-element-type-iv-conc-int-type-params iv-parameters-structure)
      (format t "  (conc-int-type-params . ~A)~%" (membrane-element-type-iv-conc-int-type-params iv-parameters-structure)))
     ((element-parameter type 'conductance-function)
      (format t "  (conductance-function . ~s)~%" (element-parameter type 'conductance-function)))
     ((element-parameter type 'static-voltage-dependence-function)
      (format t "  (static-voltage-dependence-function . ~s)~%" (element-parameter type 'static-voltage-dependence-function))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REVERSAL POTENTIAL FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-element-type-e-rev (elt &optional cell-type element-type)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((element-type (or element-type (element-type elt)))
	 (iv-parameters-structure (element-iv-parameters element-type)) ; Gets the params from the elt type.
	 ;; (cell-type (or (element cell-type 'cell-type) (and (element-cell elt) (cell-type (element-cell elt)))))
	 (ion-perms (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)))
    (if (or (membrane-element-type-iv-use-defined-e-rev iv-parameters-structure)
	    (not (membrane-element-type-iv-ion-permeabilities iv-parameters-structure)))
	(membrane-element-type-iv-e-rev iv-parameters-structure)
	(the sf (effective-reversal-potential ion-perms (or (element cell-type 'cell-type) (element-cell-type elt)))))))

(defun effective-reversal-potential (ion-perms &optional element)
  "Calculate reversal potential based on the list ION-PERMS, which has the format:

             '((ion permeability) (ion permeability) ...)

where ION is one of the symbols used by the function DEFAULT-ION-REVERSAL-POTENTIAL, and
PERMEABILITY is the [single float] relative permeability. The reversal potentials for each ION
references the cell-type associated with ELEMENT, or the DEFAULT-ION-REVERAL-POTENTIAL."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when (> (loop for ion-perm in ion-perms sum (cadr ion-perm)) 1)
    (sim-error (format nil "The perms in ~A are greater than 1." ion-perms)))
  (let* ((cell (element-cell element))
	 (cell-type (when cell (cell-type cell)))
	 (e-na (if cell-type (cell-type-e-na cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'na)))
	 (e-k (if cell-type (cell-type-e-k cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'k)))
	 (e-ca (if cell-type (cell-type-e-ca cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'ca)))
	 (e-cl (if cell-type (cell-type-e-cl cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'cl))))
    (declare (single-float e-na e-k e-ca e-cl))
    (loop for ion-perm in ion-perms
	  sum (* (the sf (case (car ion-perm)
			   (NA e-na)
			   (K e-k)
			   (CL e-cl)
			   (CA e-ca)
			   (t 0.0)))
		 (the sf (cadr ion-perm)))
	  into e-rev single-float finally (return e-rev))))

(defun update-element-fixed-e-rev (elt &optional fixed-e-rev elt-type)
  ;; For both synapses and channels - this does not apply to elements whose reversal potentials depend on ion integrators, since in
  ;; this case the e-revs are initialized by the integrators' states.  Called from INIT-CHANNELS-E-REV, INIT-SYNAPSES-E-REV,
  ;; SET-CHANNEL-PARAMETERS and SET-SYNAPSE-PARAMETERS.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (ignore elt-type))
  (let ((fixed-e-rev (the sf (or fixed-e-rev (current-element-type-e-rev elt nil elt-type)))))
    (typecase elt
      (channel (setf (channel-e-rev elt) (d-flt fixed-e-rev)))
      (synapse (setf (synapse-e-rev elt) (d-flt fixed-e-rev))))
    nil))

(proclaim '(inline element-e-rev-from-shells))
(defun element-e-rev-from-shells (conc-ints-params)
  ;; The CONC-INTS-PARAMS argument comes from the channel or synapse :CONC-INTS-PARAMS slot.  This function is called from within
  ;; EVAL-ALL-CHANNELS and EVAL-ALL-x-SYNAPSES. The reversal potentials as evaluated from the concentration integrators are
  ;; calculated on the concentrations of the *last* time step.
  ;;
  ;; The format of the CONC-INTS-PARAMS list is
  ;;
  ;;     '( (conc-int conc-int-shell e-reversal-coefficient)
  ;;        (conc-int conc-int-shell e-reversal-coefficient)
  ;;                          ...
  ;;        (conc-int conc-int-shell e-reversal-coefficient))
  ;;
  ;;  Typically, the value of E-REVERSAL-COEFFICIENT will be the same as the permeablility coefficient associated with this
  ;;  channel or synapse, and assigned to the concentration integrator CONC-INT :SHELL-x-PORES slot by the function
  ;;  SET-CONC-INTEGRATOR-PARAMETERS.
  ;;
  ;;  For each given channel or synapse, the :CONC-INTS-PARAMS list is set with the result of PARSE-CONC-INT-INFO-FOR-ELEMENT. For
  ;;  the information regarding what concentration integrators to reference the reversal potential, this function first looks at
  ;;  the associated channel or synapse type :PARAMETERS slot for an entry with the key 'CONC-INT-TYPE-E-REV-PARAMS. If this entry
  ;;  does not exist then, like the function SET-CONC-INTEGRATOR-PARAMETERS, this function references the :CONC-INT-TYPE-PARAMS
  ;;  slot of the apprpriate channel or synapse type. Both the 'CONC-INT-TYPE-E-REV-PARAMS list (if it exists) and the
  ;;  :CONC-INT-TYPE-PARAMS are lists including each ion that passes through the channel or synapse type, an entry for a
  ;;  concentration integrator type, for the shell (1 and/or 2) and the proportion of the current associated with the shell, e.g.:
  ;;
  ;;         '((CA-IN (1 0.7) (2 0.24)) (K-EXTRA (1 1)))
  ;;
  ;;  The distinction between the interpretation of the current proportion parameter when processed by
  ;;  SET-CONC-INTEGRATOR-PARAMETERS versus PARSE-CONC-INT-INFO-FOR-ELEMENT is that in the first case this parameter is used to
  ;;  fractionate the total current of the channel or synapse into various concentration integrator compartments, whereas in the
  ;;  latter case the parameter specifies the relative contribution of a concentration integrator compartment to the reversal
  ;;  potential of a given channel or synapse. Note that, as mentioned above, in the more typical case the relative permeability
  ;;  to flow and for reversal potential will be the same.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((result 0.0d0))
    (declare (double-float result))
    (do ((conc-ints-params conc-ints-params (cdr conc-ints-params)))
	((null conc-ints-params) result)
      (let* ((integrator-list (car conc-ints-params))
	     (coefficient (the df (caddr integrator-list))))		    
	(setq result (the df (+ result (the df (* coefficient
						  (the df (case (the fn (cadr integrator-list))
							    (1 (conc-int-e-rev-shell-1 (car integrator-list)))
							    (2 (conc-int-e-rev-shell-2 (car integrator-list)))
							    (t 0.0d0))))))))))))


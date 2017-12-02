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


;;; SYS Source file: structure-macros.lisp

;;;
;;; More structure slot macros.
;;;

(in-package "SURF-HIPPO")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro node-iterator (body) `(loop for nd being the hash-value of (NODE-HASH-TABLE) do ,body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Isources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ISOURCE-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (isource-type-first-element ,type)
	   while ,var ,@body do (setq ,var (isource-next-element ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vsources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro VSOURCE-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (vsource-type-first-element ,type)
	   while ,var ,@body do (setq ,var (vsource-next-element ,var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Membrane element-iv-parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These two are convenient accessors for the format of the :CONC-INT-TYPE-PARAMS in the
;; MEMBRANE-ELEMENT-TYPE-IV structure (inherited by channel and synapse types).

(defmacro element-type-conc-int-type-param-permeability (conc-int-type-param) `(cadadr ,conc-int-type-param))
(defmacro element-type-conc-int-type-param-shell (conc-int-type-param) `(caadr ,conc-int-type-param))
(defmacro element-conc-int-param-cint (conc-int-param) `(first ,conc-int-param))
(defmacro element-conc-int-param-permeability (conc-int-param) `(the df (third ,conc-int-param)))
(defmacro element-conc-int-param-shell (conc-int-param) `(the fn (second ,conc-int-param)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro segment-iterator (body) `(loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do ,body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conc-int types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro conc-int-type-number-of-states (type)
  `(system-of-differential-equations-number-of-states (conc-int-type-system-of-differential-equations ,type)))

(defmacro conc-int-type-equation-coefficients-array (type)
  `(system-of-differential-equations-coefficients-array (conc-int-type-system-of-differential-equations ,type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conc-ints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro conc-int-core-conc (cint) `(conc-int-type-core-conc (conc-int-type ,cint)))
(defmacro conc-int-core-conc-double (cint) `(conc-int-type-core-conc-double (conc-int-type ,cint)))
(defmacro conc-int-shell-pore (conc-int-shell-pore-list) `(car ,conc-int-shell-pore-list))
(defmacro conc-int-shell-pore-perm (conc-int-shell-pore-list) `(the df (cadr ,conc-int-shell-pore-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro buffer-type-number-of-states (type) `(system-of-differential-equations-number-of-states (buffer-type-system-of-differential-equations ,type)))

(defmacro buffer-type-equation-coefficients-array (type) `(system-of-differential-equations-coefficients-array (buffer-type-system-of-differential-equations ,type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pump types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pump-type-number-of-states (type) `(system-of-differential-equations-number-of-states (pump-type-system-of-differential-equations ,type)))
(defmacro pump-type-equation-coefficients-array (type) `(system-of-differential-equations-coefficients-array (pump-type-system-of-differential-equations ,type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Channels	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro channel-type-iv-relation (type) `(membrane-element-type-iv-relation (channel-type-iv-parameters ,type)))
(defmacro channel-type-iv-source (type) `(membrane-element-type-iv-source (channel-type-iv-parameters ,type)))
(defmacro channel-type-iv-reference (type) `(membrane-element-type-iv-reference (channel-type-iv-parameters ,type)))
(defmacro channel-type-iv-density (type) `(membrane-element-type-iv-density (channel-type-iv-parameters ,type)))

(defmacro channel-type-inherit-parameters-from-type (type) `(membrane-element-type-iv-inherit-parameters-from-type (channel-type-iv-parameters ,type)))
(defmacro channel-type-use-defined-e-rev (type) `(membrane-element-type-iv-use-defined-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-e-rev (type) `(membrane-element-type-iv-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-ion-permeabilities (type) `(membrane-element-type-iv-ion-permeabilities (channel-type-iv-parameters ,type)))
(defmacro channel-type-conc-int-type-params (type) `(membrane-element-type-iv-conc-int-type-params (channel-type-iv-parameters ,type)))
(defmacro channel-type-variable-e-rev (type) `(membrane-element-type-iv-variable-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-blocked (type) `(membrane-element-type-iv-blocked (channel-type-iv-parameters ,type)))
(defmacro channel-type-q10 (type) `(membrane-element-type-iv-q10 (channel-type-iv-parameters ,type)))
(defmacro channel-type-reference-temp (type) `(membrane-element-type-iv-reference-temp (channel-type-iv-parameters ,type)))

(defmacro channel-iv-relation (ch) `(channel-type-iv-relation (channel-type ,ch)))
(defmacro channel-use-defined-e-rev (ch) `(channel-type-use-defined-e-rev (channel-type ,ch)))
(defmacro channel-ion-permeabilities (ch) `(channel-type-ion-permeabilities (channel-type ,ch)))
(defmacro channel-conc-int-type-params (ch) `(channel-type-conc-int-type-params (channel-type ,ch)))
(defmacro channel-variable-e-rev (ch) `(channel-type-variable-e-rev (channel-type ,ch)))
(defmacro channel-q10 (ch) `(channel-type-q10 (channel-type ,ch)))
(defmacro channel-reference-temp (ch) "Degrees C" `(channel-type-reference-temp (channel-type ,ch)))

(defmacro channel-conductance (ch) "uS" `(memb-elt-iv-conductance (channel-iv-values ,ch)))
(defmacro channel-permeability (ch) "cm3/s" `(memb-elt-iv-permeability (channel-iv-values ,ch)))
(defmacro channel-gbar (ch) "uS" `(memb-elt-iv-gbar (channel-iv-values ,ch)))
(defmacro channel-pbar (ch) "cm3/s" `(memb-elt-iv-pbar (channel-iv-values ,ch)))
(defmacro channel-e-rev (ch) "mV" `(memb-elt-iv-e-rev (channel-iv-values ,ch)))
(defmacro channel-current (ch) "nA" `(memb-elt-iv-current (channel-iv-values ,ch)))
(defmacro channel-gbar/perm-reference-value (ch) "uS or cm3/s" `(memb-elt-iv-gbar/perm-reference-value (channel-iv-values ,ch)))
(defmacro channel-gbar-reference-value (ch) "uS" `(memb-elt-iv-gbar/perm-reference-value (channel-iv-values ,ch)))
(defmacro channel-pbar-reference-value (ch) "cm3/s" `(memb-elt-iv-gbar/perm-reference-value (channel-iv-values ,ch)))
(defmacro channel-iv-reference-value (syn) "uS or cm3/s" `(memb-elt-iv-reference-value (channel-iv-values ,syn)))

(defmacro CHANNEL-TYPE-DO ((elt elt-type) &rest body)
  ;; Iterates over all channels of a given type. ELT is the loop symbol, ELT-TYPE may be any form which evaluates to a channel type.
  (let ((channel-types (gensym)))
    `(let ((,channel-types (coerce-to-list ,elt-type)))
       (dolist (type ,channel-types)
	 (do ((,elt (channel-type-first-element type) (channel-next-element ,elt)))
	     ((null ,elt))
	   ,@body)))))

(defmacro CHANNEL-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (channel-type-first-element ,type)
	   while ,var ,@body do (setq ,var (channel-next-element ,var)))))

(defmacro CHANNEL-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(when ,type
     (loop with ,var = (channel-type-first-element ,type)
	   ,@body
	   (setq ,var (channel-next-element ,var))
	   unless ,var do (return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synapses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro synapse-type-iv-relation (type) `(membrane-element-type-iv-relation (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-iv-source (type) `(membrane-element-type-iv-source (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-iv-reference (type) `(membrane-element-type-iv-reference (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-iv-density (type) `(membrane-element-type-iv-density (synapse-type-iv-parameters ,type)))

(defmacro synapse-type-inherit-parameters-from-type (type) `(membrane-element-type-iv-inherit-parameters-from-type (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-use-defined-e-rev (type) `(membrane-element-type-iv-use-defined-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-e-rev (type) `(membrane-element-type-iv-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-ion-permeabilities (type) `(membrane-element-type-iv-ion-permeabilities (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-conc-int-type-params (type) `(membrane-element-type-iv-conc-int-type-params (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-variable-e-rev (type) `(membrane-element-type-iv-variable-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-blocked (type) `(membrane-element-type-iv-blocked (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-q10 (type) `(membrane-element-type-iv-q10 (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-reference-temp (type) `(membrane-element-type-iv-reference-temp (synapse-type-iv-parameters ,type)))

(defmacro synapse-iv-relation (syn) `(synapse-type-iv-relation (synapse-type ,syn)))
(defmacro synapse-use-defined-e-rev (syn) `(synapse-type-use-defined-e-rev (synapse-type ,syn)))
(defmacro synapse-ion-permeabilities (syn) `(synapse-type-ion-permeabilities (synapse-type ,syn)))
(defmacro synapse-conc-int-type-params (syn) `(synapse-type-conc-int-type-params (synapse-type ,syn)))
(defmacro synapse-variable-e-rev (syn) `(synapse-type-variable-e-rev (synapse-type ,syn)))
(defmacro synapse-q10 (syn) `(synapse-type-q10 (synapse-type ,syn)))
(defmacro synapse-reference-temp (syn) "Degrees C" `(synapse-type-reference-temp (synapse-type ,syn)))

(defmacro synapse-conductance (ch) "uS" `(memb-elt-iv-conductance (synapse-iv-values ,ch)))
(defmacro synapse-permeability (ch) "cm3/s" `(memb-elt-iv-permeability (synapse-iv-values ,ch)))
(defmacro synapse-gbar (ch) "uS" `(memb-elt-iv-gbar (synapse-iv-values ,ch)))
(defmacro synapse-pbar (ch) "cm3/s" `(memb-elt-iv-pbar (synapse-iv-values ,ch)))
(defmacro synapse-e-rev (ch) "mV" `(memb-elt-iv-e-rev (synapse-iv-values ,ch)))
(defmacro synapse-current (ch) "nA" `(memb-elt-iv-current (synapse-iv-values ,ch)))
(defmacro synapse-gbar/perm-reference-value (ch) "uS or cm3/s" `(memb-elt-iv-gbar/perm-reference-value (synapse-iv-values ,ch)))
(defmacro synapse-gbar-reference-value (ch) "uS" `(memb-elt-iv-gbar/perm-reference-value (synapse-iv-values ,ch)))
(defmacro synapse-pbar-reference-value (ch) "cm3/s" `(memb-elt-iv-gbar/perm-reference-value (synapse-iv-values ,ch)))
(defmacro synapse-iv-reference-value (syn) "uS or cm3/s" `(memb-elt-iv-reference-value (synapse-iv-values ,syn)))

(defmacro fast-syn-event-followers (syn) `(get-a-value 'event-followers (synapse-parameters ,syn)))

(defmacro fast-set-syn-event-followers (syn value) `(set-element-parameter-fast ,syn 'event-followers ,value (synapse-parameters ,syn)))

(defmacro SYNAPSE-TYPE-DO ((elt elt-type) &rest body)
  ;; Iterates over all eltapses of a given type. ELT is the loop symbol, ELT-TYPE may be any form which evaluates to a synapse type.
  (let ((synapse-types (gensym)))
    `(let ((,synapse-types (coerce-to-list ,elt-type)))
       (dolist (type ,synapse-types)
	 (do ((,elt (synapse-type-first-element type) (synapse-next-element ,elt)))
	     ((null ,elt))
	   ,@body)))))

(defmacro SYNAPSE-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (synapse-type-first-element ,type)
	   while ,var ,@body do (setq ,var (synapse-next-element ,var)))))

(defmacro SYNAPSE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(when ,type
     (loop with ,var = (synapse-type-first-element ,type)
	   ,@body
	   (setq ,var (synapse-next-element ,var))
	   unless ,var do (return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Particles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro PARTICLE-TYPE-DO ((elt elt-type) &rest body)
  ;; Iterates over all particles of a given type. ELT is the loop symbol, ELT-TYPE may be any form which evaluates to a particle type.
  (let ((particle-types (gensym)))
    `(let ((,particle-types (coerce-to-list ,elt-type)))
       (dolist (type ,particle-types)
	 (do ((,elt (particle-type-first-element type) (particle-next-element ,elt)))
	     ((null ,elt))
	   ,@body)))))

(defmacro PARTICLE-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (particle-type-first-element ,type)
	   while ,var ,@body do (setq ,var (particle-next-element ,var)))))

(defmacro PARTICLE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(when ,type
     (loop with ,var = (particle-type-first-element ,type)
	   ,@body
	   (setq ,var (particle-next-element ,var))
	   unless ,var do (return))))

;; (defun particle-cell-element (prt) (channel-cell-element (particle-channel prt)))

(defmacro particle-cell-element (prt)
  `(channel-cell-element (particle-channel ,prt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conc Particles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro CONC-PARTICLE-TYPE-DO ((elt elt-type) &rest body)
  ;; Iterates over all conc-particles of a given type. ELT is the loop symbol, ELT-TYPE may be any form which evaluates to a conc-particle type.
  (let ((conc-particle-types (gensym)))
    `(let ((,conc-particle-types (coerce-to-list ,elt-type)))
       (dolist (type ,conc-particle-types)
	 (do ((,elt (conc-particle-type-first-element type) (conc-particle-next-element ,elt)))
	     ((null ,elt))
	   ,@body)))))

(defmacro CONC-PARTICLE-TYPE-ITERATOR ((var type) &rest body)
  `(when ,type
     (loop with ,var = (conc-particle-type-first-element ,type)
	   while ,var ,@body do (setq ,var (conc-particle-next-element ,var)))))

(defmacro CONC-PARTICLE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(when ,type
     (loop with ,var = (conc-particle-type-first-element ,type)
	   ,@body
	   (setq ,var (conc-particle-next-element ,var))
	   unless ,var do (return))))

;; (defun conc-particle-cell-element (prt) (channel-cell-element (conc-particle-channel prt)))

(defmacro conc-particle-cell-element (prt)
  `(channel-cell-element (conc-particle-channel ,prt)))



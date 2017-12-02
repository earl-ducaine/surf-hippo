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


;;; SYS Source file: element-functions-1.lisp

(in-package "SURF-HIPPO")

;; ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* *******
;;
;; Various element functions, taking an optional ELEMENT arg as a single ELT or a list of ELTs. If omitted, a menu will be generated to select instances of
;; the elements. Each atom of the argument can be a string, a symbol that is the name of an element, or a symbol for the general type of element
;; (e.g. 'channel or 'channel-type). In the latter case the function operates on all loaded elements which are of the general type. 
;;
;; ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* *******

(defun structure-type-slot-p (model-name slot-sym)
  (let ((accessor-function (read-from-string (format nil "~A-~A" model-name slot-sym))))
    (setfable-p accessor-function)))
  
(defun element-slot-p (element slot-sym) (structure-type-slot-p (type-of (element element)) slot-sym))

(defun cell-element-elements (element &optional (types :all))
  "Return a list of all elements whose model type \(e.g. 'SYNAPSE or 'CONC-INT\) or element type \(e.g. specific synapse or
conc-int type\) is contained in TYPES, and that are associated with all the circuit nodes implied by ELEMENT. If TYPES is :ALL,
which is the default, then return all elements including the cell element directly associated with the circuit nodes, excluding
any proximally connected cell elements."
  (declare (optimize (speed 3) (space 1)))
  (let ((types (if (eq types :all)
		   :all
		   (mapcar #'(lambda (thing) (or (when (model-name-p thing) thing) (element-type thing) thing)) (coerce-to-list types)))))
    (flatten-no-nils-no-dups-list
     (mapcar #'(lambda (cell-elt)
		 (let* ((cell-elt-cell (element-cell cell-elt))
			(node-elements (cons cell-elt-cell (node-elements (element-physical-node cell-elt)))))
		   (if (eq types :all)
		       (loop for elt in node-elements unless (and (cell-element-p elt) (not (eq elt cell-elt))) collect elt)
		       (mapcar #'(lambda (elt)
				   (let (value)
				     (loop for type in types
					   when (setq value (cond ((or (eq (element-type elt) type) (eq (type-of elt) type)) elt)
								  ((eq (type-of (element-type elt)) type) (element-type elt))))
					   do  (return value))))
			       node-elements))))
	     (coerce-to-list (cell-element element))))))

(defun circuit-elements () (concatenate 'list (cell-elements) (extracellular-electrodes)))

#|
(defun cell-elements (&optional element model-type)
  "Returns a list of all somas and segments associated with the cell or cells associated with ELEMENT \(can be a
single atom or a list\) [default all cells in circuit]."
  (if (not element)
      (all-cell-elements)
      (loop for cell in (delete-duplicates (coerce-to-list (element-cell element model-type)))
	    collect (cell-soma cell) into out
	    collect (reverse (cell-segments cell)) into out
	    finally (return (flatten-no-nils-list out)))))

(defun cell-elements (&optional element)
  "Returns a list of all somas and segments associated with ELEMENT \(can be a single atom or a list\) [default all cells in circuit]."
  (if (not element)
      (all-cell-elements)
      (loop for element in (coerce-to-list element) nconc
	    (union (somas element)
		   (union (segments element)
			  (coerce-to-list (or (proximal-segment element) (somas (element-cell element)))))))))

(defun cell-elements (&optional element)
  "Returns a list of all somas and segments associated with ELEMENT \(can be a single atom or a list\) [default all cells in circuit]."
  (if (not element)
      (all-cell-elements)
      (delete-duplicates
       (flatten-list
	(loop for element in (coerce-to-list element) collect
	      (list (somas element)
		    (segments element)
		    (or (proximal-segment element) (somas (element-cell element)))))))))


(defun cell-elements (&optional element)
  "Returns a list of all somas and segments associated with ELEMENT \(can be a single atom or a list\) [default all cells in circuit]."
  (if (not element)
      (all-cell-elements)
      (delete-duplicates
       (loop for element in (coerce-to-list element) nconc
	     (somas element) into out
	     nconc (segments element) into out
	     nconc (or (list (proximal-segment element)) (somas (element-cell element))) into out
	     finally (return out)))))


(defun cell-elements (&optional element)
  "Returns a list of all somas and segments associated with ELEMENT \(can be a single atom or a list\) [default all somas and segments in circuit]."
  (if (not element)
      (all-cell-elements)
      (no-nils (delete-duplicates
		(loop for element in (coerce-to-list element)
		   unless (element-type-p element)
		   nconc (flatten-list (somas element) (segments element) (or (list (proximal-segment element)) (somas (element-cell element)))) into out
		   finally (return out))))))

|#

(defun cell-elements (&optional element)
  "Returns a list of all somas and segments associated with ELEMENT \(can be a single atom or a list\) [default all cells in circuit]."
  (if (not element)
      (all-cell-elements)
      (no-nils (delete-duplicates
		(loop for element in (coerce-to-list element) nconc
		      (somas element) into out
		      nconc (segments element) into out
		      nconc (or (list (proximal-segment element)) (somas (element-cell element))) into out
		      finally (return out))))))


(defun cell-element (element &optional model-type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all cell elements associated with ELEMENT of MODEL-TYPE. Note that for a segment ELEMENT,
the segment or soma attached to the proximal node of this segment will also be returned."
  (element-cell-element element model-type))

(defun element-cell-element (element &optional model-type fast (disable-element-slot-core-typecase-error t))
  (typecase element
    (cell nil)
    ((or soma segment) element)
    (t (element-wrapper (element model-type)
			(let ((*element-slot-t-typecase-error-p* (not disable-element-slot-core-typecase-error)))
			  (element-cell-element-core elt internal-type fast))))))

(defun element-cell-element-core (element &optional model-type fast)
  (typecase element
    ((or soma segment) element)
    (t (let ((element (element element model-type fast)))
	 (or (element-slot element :cell-element)
	     (typecase element
	       (node (loop for elt in (node-elements element)
			when (typecase elt
			       (segment (eq element (segment-node-2 elt)))
			       (soma  (eq element (soma-node elt))))
			do (return elt)))
	       (cell (cell-elements element))
	       (segment element)
	       (soma element)
	       ;; (pump (element-cell-element (pump-conc-int element nil t)))
	       (particle (when (particle-channel element) (channel-cell-element (particle-channel element))))
	       (conc-particle (when (conc-particle-channel element) (channel-cell-element (conc-particle-channel element))))))))))


;; *********** *********** *********** ***********
;;
;; Generic element model funcalls and slot access
;;
;; *********** *********** *********** ***********

(defun element-funcall (element-function element &optional type (arg nil arg-supplied-p))
  (loop for element in (elements element type) do
	(let* (*print-pretty*
	       (function-sym (read-from-string (format nil "~A-~A" element-function (or type (type-of element))))))
	  (cond
	    ((macro-function function-sym) (eval (macroexpand (if arg-supplied-p (list function-sym element arg) (list function-sym element)))))
	    ((fboundp function-sym) (if arg-supplied-p (funcall function-sym element arg) (funcall function-sym element)))))))
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pre-synaptic-element (element)
  "Return the pre-synaptic cell element of ELEMENT."
  (let (*element-slot-t-typecase-error-p*) (element-slot element :pre-synaptic-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for evaluating, transfering, and manipulating cell elements.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-next-element (elt) (let (*element-slot-t-typecase-error-p*) (element-slot elt :next-element)))

(defun element-first-element (elt) (let (*element-slot-t-typecase-error-p*) (element-slot (element-type elt) :first-element)))

(defun element-last-element (elt)
  (let ((next-element (element-next-element elt)))
    (if (and elt next-element) (element-last-element next-element) elt)))

(defun element-last-element (elt) (let (*element-slot-t-typecase-error-p*) (element-slot (element-type elt) :last-element)))

(defun last-element-of-type (type)
  ;; (element-last-element (element-first-element type))
  (element-last-element type))

(defun clear-elements-of-type (type)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (element-slot type :first-element nil)
  (element-slot type :last-element nil)
  (loop for elt being the hash-value of (get-model-hash-table (model-child-structure-type (element-model type)))
	when (eq (element-slot-fast elt :type) type) do (element-slot-fast elt :next-element nil)))

(defun number-of-reordered-type-children (type)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((count 0))
    (declare (fixnum count))
    (element-type-do (child (element-type type)) (when child (incf count)))
    count))

(defun model-elements-need-reordering-p (model)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (and (fboundp (read-from-string (format nil "~A-first-element" (model-name model))))
       (or 
	(loop for parent-type being the hash-value of (get-model-hash-table model)
	      thereis (element-parameter parent-type :reorder-elements))
	(/= (loop for parent-type being the hash-value of (get-model-hash-table model)
		  sum (the fn (number-of-reordered-type-children parent-type)) into total fixnum
		  finally (return total))
	    (the fn (hash-table-count (get-model-hash-table (model-child-structure-type model))))))))

(defun reorder-elements-of-type (type)
  "Reorders all the elements of TYPE for the iterator constructs (applies to synapses, channels, particles and conc-particles at
the moment). See also *ENABLE-REORDER-ELEMENTS*."
  (when *enable-reorder-elements*
    (let* ((type (element-type type))
	   ; (type-parameters (element-parameters type))
	   (model (element-model type))
	   (type-children-model-hash-table (get-model-hash-table (model-child-structure-type model)))
	   last-elt)
      (when (element-slot-p type :first-element)
	(clear-elements-of-type type)
	(loop for elt being the hash-value of type-children-model-hash-table
	      ;; do (format t "elt ~A~%" elt)
	      when (eq (element-slot-fast elt :type) type)
	      do
	      (let ((last-element-of-type (or last-elt (last-element-of-type type))))
		(if last-element-of-type (element-slot-fast last-element-of-type :next-element elt) (element-slot-fast type :first-element elt)))
	      (setq last-elt elt)
	      finally ;; (format t "elt ~A~%" last-elt)
	      (element-slot-fast type :last-element last-elt))
	(element-parameter type :reorder-elements nil)))))

(defun reorder-model-elements-as-needed ()
  ;; Called by SET-CIRCUIT-ELEMENTS-PARAMETERS, which is called by INITIALIZE-SIMULATION, so this is run every time before a
  ;; simulation. Also called by ERASE-ELEMENT.
  (let ((*enable-reorder-elements* t))
    (loop for model in (models)
	  when (model-elements-need-reordering-p model)
	  do (loop for element-parent-type being the hash-value of (get-model-hash-table model)
		   do (reorder-elements-of-type element-parent-type)))))
		 
;; (defvar *DEBUG-REORDER-ELEMENTS-OF-TYPE* nil)

(defun all-elements () (flatten-no-nils-list (loop for model in (models) collect (hash-table-list (model-HASH-TABLE model)))))

(defun element-type-p (element &optional type)
  "Predicate whether ELEMENT of TYPE is a circuit element type, for example BUFFER-TYPE, PUMP-TYPE, CHANNEL-TYPE,
CONC-INT-TYPE, PARTICLE-TYPE, CONC-PARTICLE-TYPE, SYNAPSE-TYPE, AXON-TYPE, or CELL-TYPE."
  (let ((type-sym (type-of (element element type))))
    (loop for sym in *circuit-element-type-model-names* thereis (eq type-sym sym))))

(defun element-model (elt &optional type)
  (flet ((element-model-core
	  (elt type)
	  (if (model-p elt)
	    elt
	    (or (gethash elt *model-hash-table*)
		(type-symbol-model (or type
				       (when (element elt type) (type-of (element elt type)))
				       elt))))))
    (or (element-model-core elt type) (element-model-core (element elt type) nil))))

(defun element-parent-model (elt &optional type)
  (let ((element-model (element-model elt type)))
    (or	(element-model (model-parent-structure-type element-model))
	element-model)))

(defun type-symbol-child-structure-type (type-symbol) (or (model-child-structure-type (get type-symbol 'model)) type-symbol))

(defun type-symbol-model (type-symbol) (get (if (stringp type-symbol) (read-from-string type-symbol) type-symbol) 'model))

(defun type-symbol-parent-model (type-symbol)
  (let ((orig-model (type-symbol-model (if (symbolp type-symbol) type-symbol (type-of (element-type type-symbol))))))
    (when orig-model (or (get (model-parent-structure-type orig-model) 'model) orig-model))))

(defun retrieve-model-parameters-from-library (type-symbol model) (get-a-value type-symbol (MODEL-PARAMETER-type-LIBRARY model) 'equal))

(defun library-catalog (reference &key only-in-circuit (synapse-control :all) verbose ionic-type)
  "Returns a list of all type symbols in the type library referenced by ELEMENT. Thus, if ELEMENT is a channel, then the symbols of all library channel
types are returned. ELEMENT can also be a symbol for the model class, e.g. 'CHANNEL or 'CHANNEL-TYPE. If VERBOSE then print out the contents of the
TYPE-DEF forms as well. For synapse types, the SYNAPSE-CONTROL argument can be :ALL [default], :LIGHT, :VOLTAGE, :EVENT, :LIGHT-EVENT, and :TONIC."
  (let* ((model ;; (type-symbol-parent-model (or (element-type reference) reference))
	  (element-parent-model reference))
	 (model-parameter-type-library (when model (delete-duplicates (model-parameter-type-library model) :key 'car :from-end t))))
    (mapcan #'(lambda (library-entry)
		(when (and (or (not only-in-circuit)
			       (instance-in-cell (car library-entry)))
			   (or (not ionic-type)
			       (equal ionic-type :ALL)
			       (find ionic-type (library-entry-parameter library-entry 'ion-permeabilities model-parameter-type-library) :key 'car))
			   (or (not (eq (model-name model) 'synapse-type))
			       (equal synapse-control :ALL)
			       (eql synapse-control (library-entry-parameter library-entry 'control model-parameter-type-library))))
		  (when verbose
		    (format t "~A Library Entry: ~A~%" (model-name model) (car library-entry))
		    (mapcar #'(lambda (key-value-pair) (format t "   ~A ~A~%" (car key-value-pair) (cdr key-value-pair))) (cdr library-entry)))
		  (list (car library-entry))))
	    model-parameter-type-library)))

(defun library-entry-parameter (library-entry parameter model-parameter-type-library)
  ;; Apply to LIBRARY-ENTRY, which has the form
  ;;
  ;; (KA-HPC (GBAR . 2.3) (E-REV . -70) (USE-DEFINED-REV . T)
  ;;         (ION-PERMEABILITIES (K 0.85) (NA 0.15)) (Q10 . 1) (REFERENCE-TEMP . 35)
  ;;         (V-PARTICLES (KAX-HPC 4) (KAY-HPC 3))
  ;;         (SOURCE-FILE . "/usr/local/surf-hippo/bin/parameters/working-hpc.fasl"))
  ;;
  ;; The entire MODEL-PARAMETER-TYPE-LIBRARY is referenced in order to recursively look up any 'PARENT-TYPE references.
  (when library-entry
    (or (cdr-assoc parameter (cdr library-entry))
	(library-entry-parameter
	 (assoc (cdr-assoc 'PARENT-TYPE (cdr library-entry)) model-parameter-type-library)
	 parameter
	 model-parameter-type-library))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-all-things (model-type &optional cell)
  (let ((things (all-model-instances model-type))
	(cell (element cell 'cell)))
    (when (eq model-type 'segment) (setq things (delete-if 'electrode-structure-p things)))
    (when cell (setq things (loop for thing in things when (eq (element-cell thing) cell) collect thing)))
    things))
  
(defun list-of-all-things-in-circuit (model-type &optional cell)
  (loop for thing in (list-of-all-things model-type cell) when (element-in-circuit thing) collect thing))

(defun things-in-circuit (model-type &optional cell) (list-of-all-things-in-circuit model-type cell))

(defun hash-table-has-more-than-one-value-p (table)
  (loop for thing being the hash-value of table
	for count from 1
	thereis (> count 2)))

(defun element-node (element &optional model-type)
  "Return the soma or segment circuit node(s) associated with ELEMENT of MODEL-TYPE."
  (atomize-list
   (loop for cell-element in (coerce-to-list (element-cell-element element model-type))
	 collect (cell-element-physical-node-fast cell-element))))

(defun cell-element-physical-node-fast (elt)
  (typecase elt
    (node elt)
    (segment (segment-node-2 elt))
    (soma (soma-node elt))))

(defun element-physical-node (element &optional model-type fast)
  (let ((elt (element element model-type fast)))
    (or (cell-element-physical-node-fast elt)
	(typecase elt
	  (cell (soma-node (cell-soma elt)))
	  (axon (axon-proximal-node elt))
	  (isource (isource-node-2 elt))
	  (vsource (vsource-node elt))
	  (channel (channel-node elt))
	  (synapse (synapse-node elt))
	  (pump (cell-element-physical-node-fast (conc-int-cell-element (pump-conc-int elt))))
	  (particle (channel-node (particle-channel elt)))
	  (conc-particle (channel-node (conc-particle-channel elt)))
	  (t (cell-element-physical-node-fast (let (*element-slot-t-typecase-error-p*) (element-slot elt :cell-element))))))))

(defun ELEMENT-PHYSICAL-NODE-FAST (elt &optional model-type) (ELEMENT-PHYSICAL-NODE elt model-type t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-isources (element &optional model-type) (cell-element-elements (element element model-type) 'isource))
(defun element-vsources (element &optional model-type) (cell-element-elements (element element model-type) 'vsource))
(defun element-synapses (element &optional model-type) (cell-element-elements (element element model-type) 'synapse))
(defun element-conc-ints (element &optional model-type) (cell-element-elements (element element model-type) 'conc-int))
(defun element-buffers (element &optional model-type) (cell-element-elements (element element model-type) 'buffer))
(defun element-pumps (element &optional model-type) (cell-element-elements (element element model-type) 'pump))
(defun element-channels (element &optional model-type) (cell-element-elements (element element model-type) 'channel))
(defun element-particles (element &optional model-type) (cell-element-elements (element element model-type) 'particle))
(defun element-conc-particles (element &optional model-type) (cell-element-elements (element element model-type) 'conc-particle))

;; Given a name or instance THING, returns T if THING or something of model-type THING is part of the current circuit.
(defun element-in-circuit (thing &optional model-type)
  (let ((thing (element thing model-type)))
    (typecase thing
      (extracellular-electrode t)
      (t (instance-in-cell thing model-type)))))

(defun instance-in-cell (thing &optional model-type cells)
  (let ((thing (element thing model-type))
	(cells (coerce-to-list cells)))
    (or (typecase thing
	  ((or channel particle conc-particle isource vsource axon pump buffer synapse conc-int segment soma) t)
	  (cell-type (if cells
		       (loop for cell in cells thereis (eq thing (cell-type cell)))
		       (consp (cell-type-cells thing))))
	  (cell (or (not cells) (member thing cells)))
;;	  (instance-in-cell-element-type-typecase-clauses)
	  (axon-type (if cells
		       (loop for axon in (axon-type-axons thing) thereis (generic-intersection (element-cell axon) cells))
		       (consp (axon-type-axons thing))))
	  (pump-type (if cells
		       (loop for pump in (pump-type-pumps thing) thereis (generic-intersection (element-cell pump) cells))
		       (consp (pump-type-pumps thing))))
	  (buffer-type (if cells
			 (loop for buffer in (buffer-type-buffers thing) thereis (generic-intersection (element-cell buffer) cells))
			 (consp (buffer-type-buffers thing))))
	  (channel-type (if cells
			  (loop for ch in (channel-type-channels thing) thereis (generic-intersection (element-cell ch) cells))
			  (consp (channel-type-channels thing))))
	  (synapse-type (if cells
			  (loop for syn in (synapse-type-synapses thing) thereis (generic-intersection (element-cell syn) cells))
			  (consp (synapse-type-synapses thing))))
	  (particle-type (if cells
			   (loop for prt in (particle-type-particles thing) thereis (generic-intersection (element-cell prt) cells))
			   (consp (particle-type-particles thing))))
	  (conc-particle-type (if cells
				(loop for prt in (conc-particle-type-particles thing) thereis (generic-intersection (element-cell prt) cells))
				(consp (conc-particle-type-conc-particles thing))))
	  (conc-int-type (if cells
			   (loop for cint in (conc-int-type-conc-ints thing) thereis (generic-intersection (element-cell cint) cells))
			   (consp (conc-int-type-conc-ints thing)))))
	(element-cell-element thing))))

(defun type-instances-in-cell (thing &optional model-type)
  "Given a name or instance of THING of MODEL-TYPE, returns a list of instances of the same model-type."
  (let ((thing (element thing model-type)))
    (typecase thing
      (isource (isources))
      (vsource (vsources))
      (soma (somas))
      (segment (segments))
      (cell (cells))
      (cell-type (cell-type-cells thing))
      (axon-type (axon-type-axons thing))
      (axon (axons))
      (channel-type (channel-type-channels thing))
      (channel (channels))
      (pump-type (pump-type-pumps thing))
      (pump (pumps))
      (buffer-type (buffer-type-buffers thing))
      (buffer (buffers))
      (synapse-type (synapse-type-synapses thing))
      (synapse (synapses))
      (particle-type (particle-type-particles thing))
      (particle (particles))
      (conc-particle-type (conc-particle-type-particles thing))
      (conc-particle (conc-particles))
      (conc-int-type (conc-int-type-conc-ints thing))
      (conc-int (conc-ints))
      (extracellular-electrode (extracellular-electrodes))
      ;; (electrode (electrodes))
      )))

(defun type-instances-in-circuit (thing &optional model-type) (type-instances-in-cell thing model-type))

(defun num-type-instances-in-circuit (thing &optional model-type)
  (let* ((instances (type-instances-in-circuit thing model-type))
	 (num-enabled (loop for instance in instances when (element-enabled-p instance) sum 1))
	 (total (length instances)))
    (values total num-enabled)))

(defun elements-of-type (element-reference &optional cell-elements-reference)
  "If ELEMENT-REFERENCE is a model type symbol [e.g. 'channel or 'channel-type], then returns all instances of the model [e.g. all
channels or all channel types]. Otherwise, if ELEMENT-REFERENCE refers to a specific instance of an element parent type (synapse
type, channel type, etc.), returns all the child instances (synapses of that synapse type, or channels of that channel type). If
CELL-ELEMENTS-REFERENCE is included, returned elements are restricted to cell elements associated with CELL-ELEMENTS-REFERENCE;
otherwise all elements are returned."
  (let* ((element-references
	  (flatten-no-nils-list (loop for ref in (coerce-to-list element-reference)
				      when (model-name-p ref) collect ref into out else
				      when (and (element-model ref) (model-child-structure-type (element-model ref))) collect (element ref) into out
				      finally (return out))))
	 (cell-elements (coerce-to-list (element-cell-element cell-elements-reference)))
	 (unfiltered-elements-of-type (flatten-no-nils-list (loop for element-reference in element-references collect
								  (if (model-name-p element-reference)
								      (hash-table-list (get-model-hash-table element-reference))
								      (element-parent-type-children element-reference))))))
    (if cell-elements
	(loop for elt in unfiltered-elements-of-type when (member (element-cell-element elt) cell-elements) collect elt)
	unfiltered-elements-of-type)))

(defun element-parent-type-children (parent-reference)
  (let ((element-hash-table (get-model-hash-table (model-child-structure-type (element-model parent-reference)))))
    (when element-hash-table (loop for elt being the hash-value of element-hash-table when (eq (element-slot elt :type) parent-reference) collect elt))))

#|
child model name (e.g. 'CHANNEL) -> all channels
channel instance or name ->
parent model type instance or name (e.g. *channel-type*) -> all child instances of the parent type instance (e.g. (channel-type-channels *channel-type*)) 
parent model name (e.g. 'CHANNEL-TYPE) -> all parent type instances (e.g. (channel-types))
|#

(defun all-things-of-same-type (thing &optional model-type)
  (let ((thing-type (element-type (element thing model-type))))
    (typecase thing-type
      (cell-type (cell-type-cells thing-type))
      (axon-type (axon-type-axons thing-type))
      (channel-type (channel-type-channels thing-type))
      (pump-type (pump-type-pumps thing-type))
      (buffer-type (buffer-type-buffers thing-type))
      (synapse-type (synapse-type-synapses thing-type))
      (particle-type (particle-type-particles thing-type))
      (conc-particle-type (conc-particle-type-conc-particles thing-type))
      (conc-int-type (conc-int-type-conc-ints thing-type)))))

(defun num-elements-of-type (model-type) (num-type-instances-in-circuit model-type))

(defun print-num-elements-of-type (model-type &optional (stream t) suppress-enabled-info)
  (multiple-value-bind (number enabled)
      (num-type-instances-in-circuit model-type)
    (format stream "  There ~a ~a ~a ~a~p~A.~%"
	    (if (= 1 number) "is" "are")
	    number
	    (element-name model-type)
	    (TYPE-SYMBOL-CHILD-STRUCTURE-TYPE (type-of model-type))
	    number
	    (if (and (not suppress-enabled-info) (> number enabled))
	      (format nil " (~D enabled)" enabled)
	      ""))))

(defun print-num-elements-sourcefile (type &optional (stream t) suppress-enabled-info)
  (format stream "  ") (print-num-elements-of-type type stream suppress-enabled-info)
  (when (ELEMENT-SOURCEFILE-STRING type nil)
    (format t "    ")
    (ELEMENT-SOURCEFILE-STRING type stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun namelist-of-all-things (model-type &optional in-circuit)
  (let (*print-pretty*)
    (loop for thing being the hash-value of (get-model-hash-table model-type)
	  when (or (not in-circuit) (element-in-circuit thing))
	  collect (element-name thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-w-string-or-symbol (elt-reference &optional model-type fast)
  (loop for pointer in (coerce-to-list elt-reference)
	collect pointer into out
	when (stringp pointer) collect (read-from-string pointer) into out
	when (symbolp pointer) collect (format nil "~A" pointer) into out
	finally (return (element (atomize-list out) model-type fast))))
  
(defun element-which-is-cell-element (element model-type) 
  (case model-type
    ((segment soma cell) (element element model-type))
    (t (or (element element 'segment)
	   (element element 'soma)
	   (element element 'cell)
	   (element-cell-element element model-type)))))

(defun check-cell-name (name &key (automatic-name-fixing t))
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let (*print-pretty*)
    (if (CELL-HASH-TABLE name)
      (do ((i 1 (the fn (+ 1 i))))
	  ((not (CELL-HASH-TABLE (format nil "~a-~D" name i)))
	   (if (or automatic-name-fixing
		   (go-ahead-menu 
		    (format nil "Cell ~a already defined - ~%use ~a for name of new cell instead  ~%(otherwise cancel)." name (format nil "~a-~D" name i))
		    "Verify Replacement Cell Name"
		    t))
	     (format nil "~a-~D" name i)))
	(declare (fixnum i)))
      name)))

(defun check-element-name (name model-type &key (automatic-name-fixing t))
  ;; If NAME is a hash key for the hash table associated with MODEL-TYPE, then if AUTOMATIC-NAME-FIXING is T or there is menu authorization a string made
  ;; with NAME followed by a hyphen and an integer is returned - otherwise return NIL. The integer is chosen, starting from 0 and advancing by one, when
  ;; the resulting name string is not an existing hash key for the hash table.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let (*print-pretty*
	(hash-table (get-model-hash-table model-type)))
    (if (gethash name hash-table)
	(do ((i 1 (the fn (+ 1 i))))
	    ((not (gethash (format nil "~a-~D" name i) hash-table))
	     (if (or automatic-name-fixing
		     (go-ahead-menu 
		      (format nil "~a ~a already defined - ~%use ~a for name of new ~a instead  ~%(otherwise cancel)."
			      model-type name (format nil "~a-~D" name i) model-type)
		      (format nil "Verify Replacement ~A Name" model-type)
		      t))
		 (format nil "~a-~D" name i)))
	  (declare (fixnum i)))
	name)))

(defun confirm-alternate-name-creation-and-update-*PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* (string)
  (or (not *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*)
      (when (go-ahead-menu string "Rename Authorization" t t)
	(setq *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* (not (go-ahead-menu (format nil "Automatically make names for duplicate membrane elements"))))
	t)))

(defun duplicate-element-check (node type)
  ;; True if *allow-duplicate-elements* is NIL and if an element of TYPE is a member of the :ELEMENTS of NODE.
  (and (not *allow-duplicate-elements*)
       (loop for elt in (node-elements node) thereis (eq (element-type elt) type))))

(defun call-create-element-create-routine (create-routine cell-element type type-model)
  (if cell-element
    (case (length (function-required-args create-routine))
      (1 (funcall create-routine cell-element))
      (2 (funcall create-routine cell-element type))
      (t (sim-error (format nil "CREATE-ELEMENT asked to create an element at ~A using ~a, which requires more information." cell-element create-routine))))
    (funcall (model-create-routine type-model) type)))

(defun create-element (thing &rest others)
  "Generic create function for elements. Takes any number of arguments, and considers all atoms in these arguments as members of a flat
list ARGS. Given any atom in ARGS which references an element type, CREATE-ELEMENT adds an element of that type to all the cell elements
referenced in ARGS. If any member of ARGS refers to a cell, then that cell's soma is processed as a cell element. Any element
types referenced in ARGS are created if they do not already exist. Returns either a single object \(as an atom\) or objects \(as a
list\) of all created circuit elements, if any; otherwise then all referenced element types are returned, whether or not created
during the current invocation of CREATE-ELEMENT. For example:

       * (create-element 'NA-HH)
       <Channel Type NA-HH>
                                   
       * (create-element 'NA-HH *soma*)
       <Channel Hippo-soma-NA-HH: type NA-HH>
                                              
       * (create-element 'NA-HH *soma* 'DR-HH)
       (<Channel Hippo-soma-NA-HH: type NA-HH>
        <Channel Hippo-soma-DR-HH: type DR-HH>)

If the keyword :NO-DUPLICATES is included in the arguments, then no duplicate elements \(for example the same synapse type on the
same cell element\) will be created. Otherwise duplicates may be generated with or without use interaction depending on the values
of *USE-SIMPLE-NAMES*, *ALLOW-DUPLICATE-ELEMENTS* and *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*. The arguments 'SEGMENT, 'SOMA and
'CELL-ELEMENT will reference all the segments, somas, and both segments and somas of the circuit, respectively. See also
the global variable *ENABLE-REORDER-ELEMENTS*." 
  (let* ((ARGS (flatten-no-nils-list thing others))
	 (new-things nil)
	 (*allow-duplicate-elements* (and (not (member :no-duplicates ARGS)) (not (member :no-dups ARGS)))) ; :NO-DUPS for backward compatibility
	 cell-element
	 (output
	  ;; First loop through all the args, creating a list of those that refer directly to cell elements, that is somas or
	  ;; segments, and of the rest a list of the associated element types.
	  (loop for arg in (remove-all '(:no-duplicates :no-dups) ARGS)
		do (setq cell-element (element-cell-element arg))
		when cell-element collect cell-element into cell-elements else collect arg into possible-types
		finally (return
			 ;; Now create instances of all the types, and for those that whose instances can be added to a cell
			 ;; element, go ahead and do so for all the collected cell elements.
			 (atomize-delete-duplicates-flatten-no-nils-list
			  (loop for type in possible-types collect
				(if cell-elements
				  (unless (or (cell-p type) (cell-element-p type))
				    (let* ((element-type (or (element-type type nil t) type))
					   (element-type-model (element-model element-type))
					   (child-model (when element-type-model (type-symbol-model (model-child-structure-type element-type-model))))
					   (create-routine (if child-model
							     (model-create-routine child-model)
							     (when element-type-model (model-create-routine element-type-model)))))
				      (if (and (not (cell-element-p element-type)) create-routine (or (not child-model) (model-slot-p (model-name child-model) :cell-element)))
					(mapcar #'(lambda (cell-element)
						    (let ((output (call-create-element-create-routine create-routine cell-element element-type element-type-model)))
						      (when output (setq new-things t))
						      output))
						(flatten-no-nils-list cell-elements))
					(unless (cell-element-p element-type)
					  (sim-error (format nil "~A doesn't refer to an element type that goes onto a cell
element, nor a cell element!" element-type))))))
				  (multiple-value-bind (element-type-of-type new-type-created)
				      (element-type type nil t)
				    (unless (cell-element-p element-type-of-type)
				      (when new-type-created (setq new-things t))
				      element-type-of-type)))))))))
    (when (and output *enable-reorder-elements* new-things)
      (reorder-model-elements-as-needed))
    output))

(defun create-element-type (type-name type-model) (funcall (model-create-routine (element-model type-model)) type-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-segment (&optional (element *cell*))
  "Returns a randomly selected (using RANDOM-NTH) segment from the cell or cells associated with ELEMENT (default *CELL*)."
  (RANDOM-NTH (segments element)))

(defun object-type-symbol-p (thing) (model-name-p thing))
  
(defun element-cell-type (element &optional model-type)
  (let ((cell (element-cell element model-type)))
    (when cell (cell-type (if (consp cell) (car cell) cell)))))

(defun extract-cell-name (string)
  ;; Finds the longest cell name that is embedded in STRING.
  (loop for cell-name in (sort (namelist-of-all-things 'cell) '> :key 'length)
	when (search cell-name string)
	do (return cell-name)
	finally (return string)))

(defun element-segments (element &optional (exclude-self t))
  (flatten-no-nils-list
   (loop for cell-element in (coerce-to-list (element-cell-element element)) collect
	 (loop for elt in (node-elements (cell-element-physical-node-fast cell-element))
	       when (and (segment-p elt) (or (not exclude-self)
					     (not (eq cell-element elt))))
	       collect elt))))
			      
(defun particle-type-channel-type (element)
  (let ((prt (element element `particle)))
    (if prt
      (channel-type (particle-channel prt))
      (let ((prt-type (element-type element)))
	(when (particle-type-p prt-type) 
	  (loop for channel-type in (channel-types)
		when (member prt-type (channel-type-particle-types-and-powers channel-type) :key 'car)
		do (return channel-type)))))))

(defun conc-particle-type-channel-type (element)
  (let ((prt (element element `conc-particle)))
    (if prt
      (channel-type (conc-particle-channel prt))
      (let ((prt-type (element-type element)))
	(when (conc-particle-type-p prt-type)
	  (loop for channel-type in (channel-types)
		when (member prt-type (channel-type-conc-particle-types-and-powers channel-type) :key 'car)
		do (return channel-type)))))))

(defun element-has-concentration-dependence (element)
  (let ((element (element-type element)))
    (typecase element
      (channel-type (channel-type-conc-particle-types-and-powers element))
      (conc-particle-type t))))

(defun element-major-ion (element &optional model-type)
  (let ((elt-type (element-type element model-type)))
    (typecase elt-type
      (conc-int-type (conc-int-type-species elt-type))
      (conc-particle-type (conc-int-type-species (conc-particle-type-conc-int-type elt-type)))
      (t (let* ((membrane-element (typecase elt-type
				    ((or synapse-type channel-type) elt-type)
				    (particle-type (particle-type-channel-type elt-type))))
		(ion-perms (copy-list (element-ion-permeabilities membrane-element))))
	   (caar (sort ion-perms '> :key 'cadr)))))))

(defun element-ion-permeabilities (elt)
  (let (*element-slot-t-typecase-error-p*) (element-slot (element-type elt) :ion-permeabilities)))

(defun element-of-ion-type-p (element ion-type)
  "Given ELEMENT, returns T if it is associated with ION-TYPE (e.g. 'NA, 'K, 'CA, 'CL, etc.)."
  (let ((elt-type (element-type element)))
    (typecase elt-type
      (conc-int-type (eq ion-type (conc-int-type-species elt-type)))
      (conc-particle-type (eq ion-type (conc-int-type-species (conc-particle-type-conc-int-type elt-type))))
      (t (consp (find ion-type (ELEMENT-ION-PERMEABILITIES elt-type) :key 'car))))))

(defun element-conc-int-type-params (elt) (let (*element-slot-t-typecase-error-p*) (element-slot elt :conc-int-type-params)))
(defun element-conc-ints-params (elt) (let (*element-slot-t-typecase-error-p*) (element-slot elt :conc-ints-params)))

(defun element-has-conc-ints (element) (loop for elt in (elements-of-type element) thereis (element-conc-ints-params elt)))
(defun element-iv-parameters (elt &optional model-type fast) (element-slot (if fast (element-type-core elt) (element-type elt model-type)) :iv-parameters))
(defun membrane-iv-element-p (element) (true-p (element-iv-parameters element)))

(defun element-capacitance (element &optional (value nil value-supplied-p))
  "Sets the capacitance of the soma or segment associated with ELEMENT to VALUE, in nF, if non-nil, Otherwise returns the current
value in nF."
  (if value-supplied-p
    (element-slot (element-cell-element element) :capacitance (d-flt value))
    (element-slot (element-cell-element element) :capacitance)))

(defun element-capacitance-fast (element) (element-slot-fast element :capacitance))

(defun element-current (element &optional conc-in conc-out valence)
  "ELEMENT must refer to a channel, synapse, isource, vsource or pump. Returns value in nA."
  (let ((elt (element element)))
    (typecase elt
      (pump  (pump-current-value element))
      (vsource (get-vsource-current element))
      (isource (get-isource-current element))
      (channel (get-channel-current elt :conc-in conc-in :conc-out conc-out :valence valence))
      (synapse (get-synapse-current elt :conc-in conc-in :conc-out conc-out :valence valence))
      (t (sim-error (format nil "~A ~A is not a channel, synapse, isource, vsource or pump" (type-of elt) (element-name elt)))))))

(defun element-reversal-potential (element)
  "Returns the reversal potential in mV for the channel or synapse associated with ELEMENT."
  (let ((elt (element element)))
    (typecase elt
      (channel (CHANNEL-REVERSAL-POTENTIAL-VALUE elt))
      (synapse (SYNAPSE-REVERSAL-POTENTIAL-VALUE elt)))))

(defun element-conductance (element)
  "Returns the conductance in uS for the channel or synapse associated with ELEMENT."
  (let ((elt (element element)))
    (typecase elt
      (channel (CHANNEL-CONDUCTANCE-VALUE elt))
      (synapse (SYNAPSE-CONDUCTANCE-VALUE elt)))))

(defun element-voltage (element)
  "Returns the voltage in mV for the node associated with ELEMENT."
  (s-flt (node-voltage-n+1 (element-physical-node element))))

(defun element-dvdt (element)
  "Either NODE-DVDT-N or GET-NODE-DVDT \(V_n+1 - V_n / delta-t[n]\). Value in mV/ms." 
  (let ((node (element-physical-node element)))
    (element-dvdt-fast node)))

(defun element-dvdt-fast (node)
  (s-flt (if (or (eq *INTEGRATION-TIME-REFERENCE* :fixed) ; *use-fixed-step*
		 (not (THING-IN-ARRAY-P node *NODE-W/ELEMENTS-ARRAY*)))
	     (get-node-dvdt node)
	     (node-dvdt-n node))))
    
(defun element-capacitance-current (element)
  "Return the membrane capacitance current in nA for the node associated with ELEMENT."
  (let* ((element (element-cell-element element))
	 (node (cell-element-physical-node-fast element)))
    (element-capacitance-current-fast element node)))

(defun element-capacitance-current-fast (element node)
  ;; 1.0e-9 F 1.0e-3 V / 1.0e-3 s = 1.0e-9 A
  (* (element-capacitance-fast element) ; nF
     (element-dvdt-fast node)		; mV/ms
     ))

;; Not used - use instead SET-SEGMENT-ABSOLUTE-PARAMETERS, SET-SOMA-ABSOLUTE-PARAMETERS, SET-SEGMENT-PARAMETER, SET-SOMA-PARAMETER
(defun element-g-leak (element &optional (value nil value-supplied-p)) ; uS
  ;; "Sets the g-leak of the soma or segment associated with ELEMENT to VALUE, in uS, if non-nil, Otherwise returns the current value in uS."
  (if value-supplied-p
      (let ((d-flt-value (d-flt value)))
	(element-slot (element-cell-element element) :g-leak d-flt-value)
	(process-circuit-structure t)
	d-flt-value)
      (element-slot (element-cell-element element) :g-leak)))

(defun element-leak-current (element)
  "Return the membrane leak current in nA for the node associated with ELEMENT."
    (element-leak-current-fast (element-cell-element element)))

(defun segment-leak-current (seg)
  (let ((seg (element seg 'segment)))
    (when seg (segment-leak-current-fast seg))))
	  
(defun segment-leak-current-fast (seg)
  (* (- (node-voltage-n+1 (segment-node-2 seg)) (cell-type-v-leak-dendrite (cell-type (segment-cell seg)))) ; mV
     (segment-g-leak seg))		; uS
  )

(defun soma-leak-current (soma)
  (let ((soma (element soma 'soma)))
    (when soma (soma-leak-current-fast soma))))

(defun soma-leak-current-fast (soma)
  (* (- (node-voltage-n+1 (soma-node soma)) (cell-type-v-leak-dendrite (cell-type (soma-cell soma)))) ; mV
     (soma-g-leak soma))		; uS
  )

(defun element-leak-current-fast (element)
  (typecase element
    (segment (segment-leak-current-fast element))
    (soma (soma-leak-current-fast element))))

(defun set-element-voltage (element voltage) (set-node-voltage (element-physical-node element) voltage))

(defun set-element-name (element name &optional model-type)
  (let* ((element (element element model-type))
	 (table (element-hash-table element))
	 (element-name (element-name element model-type)))
    (when (and element-name (not (equal element-name name)))
      (when (and (gethash name table) (eq (gethash name table) element)) (sim-error (format nil "SET-ELEMENT-NAME: ~A already is in ~A~%" name table)))
      (remhash element-name table)
      (setf (gethash name table) element)
      (element-slot element :name name)
      element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ELEMENT-PARAMETERS Family
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-parameters (element &optional model-type fast) (element-slot-fast (element-core element model-type fast) :parameters))
(defun element-parameters-fast (element) (element-slot-fast element :parameters))

(defun length-element-parameters (element keys &optional params) (or (loop for key in keys sum (or (length-element-parameter element key params) 0)) 0))
(defun length-element-parameter (element key &optional params) (length-element-param-acons element key params))

(defun length-element-param-acons (element key &optional params) 
  (let* ((params (or params (element-parameters element)))
	 (assoc-result (assoc key params)))
    (when assoc-result
      (if nil				; (eq assoc-result (car params))
	nil				; (length-element-parameters element (cdr params))
	(length (cdr assoc-result))))))

(defun get-cell-element-param-fast (element key)
  (get-a-value key (typecase element
		     (segment (segment-parameters element))
		     (soma (soma-parameters element)))))

(defun element-parameter-core (element element-parameters parameter &optional (value nil value-supplied-p) update)
  (if value-supplied-p
    (progn (set-element-parameter-fast element parameter value element-parameters)
	   (when update (update-element element nil t))
	   value)
    (get-a-value parameter element-parameters)))

(defun element-parameter (element parameter &optional (value nil value-supplied-p) update)
  "Returns the value or values associated with PARAMETER for elements in ELEMENT, where PARAMETER is stored as part of an
element's :PARAMETERS slot (an a-list). For accessing an element's explicitly defined slots, use the function ELEMENT-SLOT. If
VALUE is supplied, the parameter is set to this new value. For some types of elements and parameters, the UPDATE flag will cause
the parameter to be fully processed."
  (let* ((element (element element))
	 (elements-p (consp element)))
    (loop for elt in (coerce-to-list element)
	  collect (if value-supplied-p
		    (element-parameter-core elt (element-parameters elt nil t) parameter value update)
		    (element-parameter-core elt (element-parameters elt nil t) parameter))
	  into out
	  finally (return (if elements-p out (car out))))))

(defun element-parameter-fast (element parameter &optional (value nil value-supplied-p) update)
  "As ELEMENT-PARAMETER, but ELEMENT must be an atom and an element pointer."
  (if value-supplied-p
    (element-parameter-core element (element-parameters element nil t) parameter value update)
    (element-parameter-core element (element-parameters element nil t) parameter)))

(defun get-element-parameter-fast (key params) (get-a-value key params))

(defun set-element-parameter-fast (element key value params)
  (if value
    ;; Set ELEMENT-PARAMETER
    (let* ((assoc-result (assoc key params)))
      (if assoc-result 
	(rplacd assoc-result value)
	(let ((new-params (acons key value params)))
	  (element-slot element :parameters new-params)))
      value)
    ;; Remove ELEMENT-PARAMETER
    (remove-element-parameter-internal element key params)))

(defun remove-element-parameter-internal (element key params)
  (let ((assoc-result (assoc key params)))
    (when assoc-result
      (if (eq assoc-result (car params))
	(element-slot element :parameters (cdr params))
	(my-delete assoc-result params))))
  nil)

(defun remove-element-parameter (element key)
  (loop for elt in (elements element) do (remove-element-parameter-internal elt key (element-parameters elt nil t))))

(defun remove-element-parameters (element keys) (loop for key in keys do (remove-element-parameter element key)))

(defun push-element-parameter (element key value &optional (element-parameters (element-parameters element)))
  (if element-parameters
    (let ((param-a-list-entry (get-element-parameter-fast key element-parameters)))
      (typecase param-a-list-entry
	(cons (setf (setfable-get-a-value key element-parameters) (cons value param-a-list-entry)))
	(null (set-element-parameter-fast element key (list value) element-parameters))
	((or symbol number) (setf (setfable-get-a-value key element-parameters) (cons value (list param-a-list-entry))))))
    (element-parameter element key (list value))))

(defun update-element-parameters-with-new-parameters (new-parameters elt)
  ;; NEW-PARAMETERS is an A-list which may include entries with only a CAR. These entries will remove any associated reference in
  ;; the ELEMENT-PARAMETERS of ELT, but still will be returned by this function in order to propogate through the calling
  ;; CREATE-X-TYPE function.
  (loop for acons in new-parameters do (element-parameter elt (car acons) (cdr acons)))
  (let ((result (delete-duplicates (concatenate 'list new-parameters (element-parameters elt)) :key 'car)))
    ;; (print result)
    result))

(defun update-element (element &optional model-type fast)
  (let ((element (element element model-type fast)))
    (typecase element
      ((or CELL CELL-TYPE) (set-circuit-elements-parameters))
      (SOMA (set-soma-membrane-parameters element))
      (SEGMENT (set-segment-membrane-parameters element))
      (CHANNEL (set-CHANNEL-parameters element t))
      (CHANNEL-TYPE (set-CHANNEL-TYPE-parameters element) (set-channels-parameters nil element))
      (PARTICLE nil)
      (PARTICLE-TYPE (set-channels-parameters))
      (CONC-PARTICLE nil)
      (CONC-PARTICLE-TYPE (set-channels-parameters))
      ((or CONC-INT CONC-INT-type) (set-conc-integrators-parameters))
      (SYNAPSE (set-SYNAPSE-parameters element t))
      (SYNAPSE-TYPE (set-SYNAPSEs-parameters t element) (set-synapses-parameters nil element))
      (AXON (set-AXON-parameters nil element))
      (AXON-TYPE (set-AXONs-parameters))
      (VSOURCE nil)
      (ISOURCE nil)
      (PUMP nil)
      (PUMP-TYPE nil)
      (BUFFER nil)
      (BUFFER-TYPE nil)
      (NODE nil)
      (extracellular-electrode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-element-array (elt key dims &optional (type 'double-float) from-particle-type always-set-new)
  (or (and (not always-set-new)
	   (if from-particle-type
	     (get-a-value key (particle-type-parameters elt))
	     (element-parameter elt key)))
      (element-parameter elt key (make-array (coerce-to-list dims) :element-type type))))

(defun element-sourcefile-string (element &optional (stream t))
  (when (element-parameter element 'source-file) (format stream "Source file: ~A" (element-parameter element 'source-file))))

(defun update-type-from-definition (element)
  "Updates the ELEMENT type from the most recently loaded library definition. Note that change in an ELEMENT type does not
necessarily propagate to elements of that type."
  (let* ((type (element-type element))
	 (type-model (element-model type))
	 (create-routine (model-create-routine type-model)))
    ;; The following types have create routines with args (type-symbol &optional actual-type-symbol update-parameters):
    ;; 
    ;; AXON-TYPE SYNAPSE-TYPE CHANNEL-TYPE PARTICLE-TYPE CONC-PARTICLE-TYPE BUFFER-TYPE PUMP-TYPE CONC-INT-TYPE CELL-TYPE
    ;;
    ;; Here, we make sure that these args exist in the model CREATE-ROUTINE.
    (when (search "&optional actual-type-symbol update-parameters" (lisp::%function-arglist (extract-function create-routine)))
      (funcall create-routine type nil t))))

#|
;; This must be loaded after the revamp functions have been loaded. 
(defun revamp-type-parameters ()
;;  "Go through all the current instances of element types with REVAMP functions, updating them according to the appropriate type
;; parameter library list. Note that this will not remove any properties which are excluded from the current library definitions."
  (macrolet ((revamp-type-parameters-functions
	      ()
	      `(progn ,@(no-nils (mapcar #'(lambda (type) (let ((revamp-fun (read-from-string (format nil "revamp-~A-parameters" type))))
							    (when (fboundp revamp-fun) `(,revamp-fun))))
					 *circuit-element-type-model-names*)))))
    (revamp-type-parameters-functions)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gbar Pbar functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(defun independent-element-gbars-p (type)
  (typecase type
    (synapse-type (and (synapses-of-type type) (loop for elt in (synapses-of-type type) never (synapse-inherit-parameters-from-type elt))))
    (channel-type (and (channels-of-type type) (loop for elt in (channels-of-type type) never (channel-inherit-parameters-from-type elt))))))
    
(defun list-total-gbars (&optional type)
  (loop for type in (coerce-to-list (or type (channel-types)))
	collect (list (element-name type) (loop for elt in (elements-of-type type) sum (element-gbar elt)))))

(defun convert-iv-relations-to-densities (model-type &optional (reference-area (element-area *soma*)))
  "Convert all channel or synapse types, depending on whether MODEL-TYPE is 'CHANNEL-TYPE or 'SYNAPSE-TYPE,
from absolute gbars or permeabilities to densities, where REFERENCE-AREA is in um2."
  (loop for type in (case model-type
		      (channel-type (channel-types))
		      (synapse-type (synapse-types)))
	when (eq (element-type-parameter type 'iv-source) :absolute)
	do (let ((iv-density (/ (iv-type-parameter type :iv-reference) ; uS
				reference-area))) ; um^2
	     (element-type-parameter type 'iv-source :density)
	     (element-type-parameter type 'iv-density
				     (* 1e6 iv-density) ; convert uS/um2 to pS/um2
				     t)	; update the associated channels/synapses
	     ))
  nil)

(defun convert-gbars-to-densities (model-type &optional (reference-area (element-area *soma*))) (convert-iv-relations-to-densities model-type reference-area))

(defun set-element-absolute-iv-reference (element iv-reference)
  "Set the gbar or permeability for the synapse or channel ELEMENT to an absolute value IV-REFERENCE (uS or cm3/s). To get the
current value, use ELEMENT-GBAR." 
  (set-element-iv-reference element iv-reference))

(defun set-element-absolute-iv-relation-ref (element iv-reference) (set-element-absolute-iv-reference element iv-reference))

(defun set-element-iv-reference (elt iv-reference)
  (let ((elt (element elt))
	(iv-values (element-slot elt :iv-values))
	(iv-reference (d-flt iv-reference)))
    (typecase elt
      ((or synapse-type channel-type)
       (sim-error "Use ELEMENT-TYPE-PARAMETER on a type!"))
      (t
       (setf (memb-elt-iv-reference-value iv-values) iv-reference)
       (element-slot elt :inherit-parameters-from-type nil)))
    (SET-ELEMENT-MEMBRANE-PARAMETERS elt)
    iv-reference))

(defun set-element-iv-relation-ref (elt iv-reference) (set-element-iv-reference elt iv-reference))

#|
(defun set-element-density (element density)
  ;; The DENSITY argument is in pS per square micron (0.1mS per square cm)
  (set-element-iv-density element density))

(defun set-element-iv-density (elt iv-density)
  (let ((elt (element elt)))
    (typecase elt
      ((or synapse-type channel-type)
       (sim-error "Use ELEMENT-TYPE-PARAMETER on a type!"))
      (t
       (element-parameter elt 'iv-density (s-flt iv-density))
       (set-element-iv-source elt :density)
       (element-slot elt :inherit-parameters-from-type nil)
       (SET-ELEMENT-MEMBRANE-PARAMETERS elt)
       (element-parameter elt 'iv-density)))))
|#

(defun element-iv-source (elt &optional iv-parameters iv-relation-inherited)
  (let* ((elt (element elt)))
    (if (not (or iv-relation-inherited (inherit-parameters-from-type elt nil t)))
      :absolute
      (membrane-element-type-iv-source (or iv-parameters (element-iv-parameters elt nil t))))))

(defun element-pbar-source (elt &optional iv-parameters iv-relation-inherited) (element-iv-source elt iv-parameters iv-relation-inherited))
(defun channel-pbar-source (elt &optional iv-parameters iv-relation-inherited) (element-pbar-source elt iv-parameters iv-relation-inherited))
(defun synapse-pbar-source (elt &optional iv-parameters iv-relation-inherited) (element-pbar-source elt iv-parameters iv-relation-inherited))

(defun element-iv-reference (element)
  (let ((elt (element element)))
    (if (inherit-parameters-from-type elt nil t)
      (membrane-element-type-iv-reference (element-iv-parameters elt nil t)) 
      (membrane-element-iv-reference elt))))

(defun channel-iv-reference (element) (element-iv-reference element))
(defun synapse-iv-reference (element) (element-iv-reference element))

#|
(defun element-iv-density (element)
  (let* ((elt (element element))
	 (iv-relation-inherited (inherit-parameters-from-type elt))
	 (element-parameters (unless iv-relation-inherited (element-parameters elt))))
    (or (and (not iv-relation-inherited) (get-a-value 'iv-density element-parameters))
	(membrane-element-type-iv-density (element-iv-parameters elt)))))
|#

(defun element-iv-density (element)
  "Returns the effective gbar or permeability density in pS/um2 or 1.0e-6cm3/s/um2 for ELEMENT."
  (element-iv-density-fast (element-core element)))

(defun element-conductance (element)
  "Returns the conductance in uS for ELEMENT."
  (let* ((elt (element-core element))
	 (conductance (element-slot elt :conductance)))
    (unless conductance (sim-error (format nil "~A doesn't refer to a channel or synapse." element)))
    conductance))

(defun element-relative-conductance (element)
  "Returns the actual conductance divided by the GBAR for ELEMENT."
  (let* ((elt (element-core element))
	 (conductance (element-slot elt :conductance))
	 (gbar (element-slot elt :gbar)))
    (unless (and conductance gbar) (sim-error (format nil "~A doesn't refer to a channel or synapse." element)))
    (/ conductance gbar)))

(defun element-pbar-density (element) (element-iv-density element))
(defun channel-iv-density (element) (element-iv-density element))
(defun synapse-iv-density (element) (element-iv-density element))

(defun print-element-iv-relation-string (elt)
  (let* ((elt (element elt))
	 (gbar (element-iv-relation-slot elt))
	 (iv-parameters (element-iv-parameters elt))
	 (iv-relation-inherited (inherit-parameters-from-type elt))
	 (iv-relation (membrane-element-type-iv-relation iv-parameters))
	 (iv-source (element-iv-source elt iv-parameters iv-relation-inherited)))
    (format nil "~a ~,2e ~A ~a~A"
	    (case iv-relation
	      (:CONSTANT-FIELD "Pbar")
	      (t "Gbar"))
	    (s-flt gbar)
	    (case iv-relation
	      (:CONSTANT-FIELD "cm3/s")
	      (t  "uS"))
	    (if iv-relation-inherited "(Type " "(Non-inherited ")
	    (case iv-relation
	      (:CONSTANT-FIELD
	       (case iv-source
		 (:DENSITY (format nil "density reference ~,2e 1.0e-6cm3/s/um^2)" (s-flt (element-pbar-density elt))))
		 (:ABSOLUTE (format nil "absolute reference)"))))
	      (t
	       (case iv-source
		 (:DENSITY (format nil "density reference ~,2e pS/um^2)" (s-flt (element-iv-density elt))))
		 (:ABSOLUTE (format nil "absolute reference)"))))))))
	    
(defun membrane-element-iv-reference (element &optional fast)
  ;; Returns a double float.
  (let ((element (element element nil fast)))
    (typecase element
      (synapse (synapse-iv-reference-value element))
      (channel (channel-iv-reference-value element)))))

(proclaim '(notinline get-element-iv-reference))
(defun get-element-iv-reference (element)
  (let* ((elt (element element))
	 (iv-parameters (element-iv-parameters elt nil t))
	 (iv-relation-inherited (inherit-parameters-from-type elt nil t))
	 (element-parameters (unless iv-relation-inherited (element-parameters-fast elt))))
    (d-flt
     (if iv-relation-inherited
       (case (element-iv-source elt iv-parameters iv-relation-inherited)
	 (:absolute (membrane-element-type-iv-reference iv-parameters))
	 (:density (g-element (element-cell-element elt nil t) (membrane-element-type-iv-density iv-parameters)))
	 (t 0.0))
       (membrane-element-iv-reference elt t)))))

(proclaim '(inline get-element-pbar-reference))
(defun get-element-pbar-reference (element) (get-element-iv-reference element))

(defun modulate-type-iv-reference (element modulation)
  (let* ((type (element-type element))
	 (iv-parameters (element-iv-parameters type)))
    (case (membrane-element-type-iv-source iv-parameters)
      (:absolute (setf (membrane-element-type-iv-reference iv-parameters)
		       (* modulation (membrane-element-type-iv-reference iv-parameters))))
      (:density (setf (membrane-element-type-iv-density iv-parameters)
		      (* modulation (membrane-element-type-iv-density iv-parameters)))))))

(defun modulate-type-pbar-reference (element modulation) (modulate-type-iv-reference element modulation))

(defun update-g-type-cell-type-iv-relation-coefficient (type)
  (let ((type (or (element type 'synapse-type) (element type 'channel-type))))
    (when type
      (let ((element-type-parameters (element-parameters type)))
	(loop for cell-type in (cell-types) do (element-type-cell-type-iv-relation-coefficient type cell-type t element-type-parameters)))))
  nil)

(defun element-type-cell-type-iv-relation-coefficient (element-type cell-type &optional always-update element-type-parameters)
  (let* ((element-type-parameters (or element-type-parameters (element-parameters element-type)))
	 (iv-relation-coeffs (get-element-parameter-fast 'iv-relation-coefficients element-type-parameters))
	 (assoc-result (assoc cell-type iv-relation-coeffs)))
    (or (and (not always-update) (cdr assoc-result))
	(let ((result (if *ignore-q10*
			(cell-type-global-membrane-conductance-factor cell-type)
			(the sf (* (cell-type-global-membrane-conductance-factor cell-type)
				   (q10-factor (element-type-reference-temp-kelvin element-type) *temperature* (element-type-q10-gbar element-type)))))))
	  (if (cdr assoc-result)
	    (rplacd assoc-result result)
	    (set-element-parameter-fast element-type 'iv-relation-coefficients (acons cell-type result iv-relation-coeffs) element-type-parameters))
	  result))))

(defun element-type-cell-type-pbar-coefficient (element-type cell-type &optional always-update element-type-parameters)
  (element-type-cell-type-iv-relation-coefficient element-type cell-type always-update element-type-parameters))

(defun non-unity-channel-type-IV-MODULATION-p ()
  ;; Just checks types in circuit
  (and (channel-types)
       (loop for type in (channel-types) never (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0)))))

(defun non-unity-channel-type-pbar-modulation-p () (non-unity-channel-type-IV-MODULATION-p))

(defun channel-types-with-non-unity-IV-MODULATION ()
  (loop for type in (channel-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0))
	collect type))

(defun channel-types-with-non-unity-pbar-modulation () (channel-types-with-non-unity-IV-MODULATION))

(defun transfer-type-IV-MODULATION-to-references (model-type)
  (let ((reset-flag nil))
    (loop for type in (case model-type
			(channel-type (channel-types))
			(synapse-type (synapse-types)))
	  unless (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION)
							    (element-parameter type 'pbar-modulation)
							    1.0) 1.0))
	  do
	  (modulate-type-iv-reference type (or (element-parameter type :IV-MODULATION)
					       (element-parameter type 'pbar-modulation)))
	  (cond ((element-parameter type :IV-MODULATION)
		 (element-parameter type :IV-MODULATION 1.0))
		((element-parameter type 'pbar-modulation)
		 (element-parameter type 'pbar-modulation 1.0)))
	  (setq reset-flag t))
    (when reset-flag (setq *recheck-circuit-elements-parameters* t))))

(defun transfer-type-pbar-modulation-to-pbars (model-type) (transfer-type-IV-MODULATION-to-gbars model-type))

;; Just resets types in circuit
(defun reset-non-unity-channel-type-IV-MODULATION () (reset-non-unity-type-IV-MODULATION 'channel-type))

(defun reset-non-unity-type-IV-MODULATION (model-type)
  (let ((reset-flag nil))
    (loop for type in (case model-type
			(channel-type (channel-types))
			(synapse-type (synapse-types)))
	  unless (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION)
							    (element-parameter type 'pbar-modulation)
							    1.0) 1.0))
	  do (cond ((element-parameter type :IV-MODULATION)
		    (element-parameter type :IV-MODULATION 1.0))
		   ((element-parameter type 'pbar-modulation)
		    (element-parameter type 'pbar-modulation 1.0)))
	  (setq reset-flag t))
    (when reset-flag (setq *recheck-circuit-elements-parameters* t))))

;; Just checks types in circuit
(defun non-unity-synapse-type-IV-MODULATION-p ()
  (and (synapse-types)
       (loop for type in (synapse-types) never (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0)))))

(defun non-unity-synapse-type-pbar-modulation-p () (non-unity-synapse-type-IV-MODULATION-p))

(defun synapse-types-with-non-unity-IV-MODULATION ()
  (loop for type in (synapse-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0))
	collect type))

(defun synapse-types-with-non-unity-pbar-modulation () (synapse-types-with-non-unity-IV-MODULATION))

;; Just resets types in circuit
(defun reset-non-unity-synapse-type-IV-MODULATION () (reset-non-unity-type-IV-MODULATION 'synapse-type))

(defun element-iv-relation-slot (element) (element-slot element :gbar))
(defun element-pbar-slot (element) (element-iv-relation-slot element))

(defun fixup-type-modulation (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((element-parameters (element-parameters-fast type)))
    (cond-every
     ((get-a-value :IV-MODULATION element-parameters)
      (typecase (get-a-value :IV-MODULATION element-parameters)
	(single-float nil)
	(t (set-element-parameter-fast type :IV-MODULATION (s-flt (get-a-value :IV-MODULATION element-parameters)) element-parameters))))
     ((get-a-value 'pbar-modulation element-parameters)
      (typecase (get-a-value 'pbar-modulation element-parameters)
	(single-float nil)
	(t (set-element-parameter-fast type 'pbar-modulation (s-flt (get-a-value 'pbar-modulation element-parameters)) element-parameters))))))
  nil)

(proclaim '(inline element-gbar))
(defun element-gbar (element &optional cell-type always-update element-type)
  "Returns the total effective GBAR of all the elements associated with ELEMENT in uS."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for element in (elements element) sum
	(let* ((type (or element-type (element-type element)))
	       (element-type-parameters (element-parameters-fast type))
	       (IV-MODULATION (the sf (or (get-a-value :IV-MODULATION element-type-parameters)
					    (get-a-value 'pbar-modulation element-type-parameters)
					    1.0))))
	  (declare (single-float IV-MODULATION))
	  (unless (membrane-iv-element-p type) (sim-error (format nil "~A is not a membrane IV element!" (element-name element))))
	  (element-iv-relation-core element IV-MODULATION type cell-type always-update element-type-parameters))
	into total double-float finally (return total)))

(defun element-iv-relation-core (element IV-MODULATION element-type cell-type always-update element-type-parameters)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (* (the sf (or IV-MODULATION
		 (get-a-value :IV-MODULATION element-type-parameters)
		 (get-a-value 'pbar-modulation element-type-parameters)
		 1.0))
     (the sf (if cell-type (the sf (element-type-cell-type-iv-relation-coefficient element-type cell-type always-update element-type-parameters)) 1.0))
     (the df (get-element-iv-reference element))))

(defun element-pbar (element &optional cell-type always-update element-type) (element-gbar element cell-type always-update element-type))

;; Access the :GBAR slot of synapses and channels.
(defun element-iv-relation-fast (element) (element-slot-fast element :gbar))
(defun element-pbar-fast (element) (element-slot-fast element :gbar))

(defun element-iv-density-fast (element)
  (typecase element
    ((or channel synapse)
     (/ (element-iv-relation-fast element)	; uS
	(element-area-fast (element-slot-fast element :cell-element)))) ; um2
    (t (/
	(* (element-gbar element)	; uS
	   1.0e3)			; pS/uS
	(element-area element)		; um2
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element iv/temp parameters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inherit-parameters-from-type (elt &optional model-type fast)
  (let ((elt (element elt model-type fast)))
    (or (synapse-type-p elt) (channel-type-p elt)
	(if fast
	  (element-slot-fast elt :inherit-parameters-from-type)
	  (element-slot elt :inherit-parameters-from-type)))))

(defun element-type-reference-temp (element-type)
  (or (element-slot (element-type element-type) :reference-temp)
      (if (element-type-p element-type)
	*Temp-celcius*
	(let ((type (element-type element-type)))
	  (if type (element-type-reference-temp type) *Temp-celcius*)))))

(defun element-type-reference-temp-kelvin (element-type) (+ 273.16 (element-type-reference-temp element-type)))

(defun element-type-q10-gbar (element-type)
  (or (element-slot (element-type element-type) :q10)
      (if (element-type-p element-type)
	1.0
	(let ((type (element-type element-type)))
	  (if type (element-type-q10-gbar type) 1.0)))))

(defun element-type-q10-pbar (element-type) (element-type-q10-gbar element-type))

(defun set-synapse-parameters (synapse &optional (update-fixed-e-rev t) (update-gbar t) IV-MODULATION)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (setf (synapse-gbar synapse)
	(element-iv-relation-core synapse
				  IV-MODULATION
				  (synapse-type synapse)
				  (cell-type (synapse-cell synapse))
				  update-gbar
				  (synapse-type-parameters (synapse-type synapse))))
  (when update-fixed-e-rev (update-element-fixed-e-rev synapse))
  nil)

(defun set-channel-parameters (channel &optional (update-fixed-e-rev t) (update-gbar t) IV-MODULATION)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (setf (channel-gbar channel)
	(element-iv-relation-core channel
				  IV-MODULATION
				  (channel-type channel)
				  (cell-type (channel-cell channel))
				  update-gbar
				  (channel-type-parameters (channel-type channel))))
  (when update-fixed-e-rev (update-element-fixed-e-rev channel nil (channel-type channel)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun member-of-node-plot-lists (elt)
  (let ((node-name (element-name (element-node elt))))
    (loop for plot-list in (list *plot-path-nodes* *plot-membrane-voltages* *analysis-nodes*)
	  thereis (string-member node-name plot-list))))

(defun somas-and-segments (&optional (cells (cells)) select-plotted (only-segments t) (only-somas nil) only-connected)
  ;;  "Returns a list of all segments and somas associated with the cell or cells referenced by CELLS
  ;; \(can be a single cell or a list\) [default all cells in circuit]. Additional optional arguments are
  ;; self-explanatory."
  (let* ((cells (flatten-no-nils-list (element-cell cells)))
	 (result (unless only-somas
		   (loop for electrode in (electrodes)
			 when (and (member (element-cell electrode) cells)
				   (or (not select-plotted) (member-of-node-plot-lists electrode)))
			 collect electrode))))
    (loop for cell in cells
	  unless only-somas do
	  (loop for segment in (cell-segments cell)
		when (and (or (not only-connected) (proximal-segment segment))
			  (or (not select-plotted) (member-of-node-plot-lists segment)))
		do (push segment result))
	  unless only-segments do
	  (let ((soma (cell-soma cell)))
	    (when (or (not select-plotted) (member-of-node-plot-lists soma)) (push soma result))))
    result))

(defun segments-not-electrodes (&optional (cell *cell*))
  (let* ((electrodes (electrodes))
	 (cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (member seg electrodes) collect seg)))

(defun make-node-w/elements-array (&optional always)
  ;; Set the global array *NODE-W/ELEMENTS-ARRAY* to point to all nodes with membrane elements or ones for which the voltage dvdt
  ;; will be plotted. It is these node that are considered in estimating the maximum LTE in CALC-VOLTAGE-LTE-RATIO. Note that
  ;; CALC-VOLTAGE-LTE-RATIO also updates the voltage derivative, which is used for the particle evaluation.  Since all nodes with
  ;; particles \(channels\) are included in *NODE-W/ELEMENTS-ARRAY*, then these derivatives will be available for the proper nodes.
  (when (or always *make-node-w/elements-array*)
    (let* ((lte-node-criterium (coerce-to-list *lte-node-criterium*))
	   (all-driven-elements (member :all lte-node-criterium))
	   (axons (member :axons lte-node-criterium))
	   (vsource-driven-elements (member :vsources lte-node-criterium))
	   (isource-driven-elements (member :isources lte-node-criterium))
	   (synapse-driven-elements (member :synapses lte-node-criterium))
	   (channel-driven-elements (member :channels lte-node-criterium))
	   (somas (member :somas lte-node-criterium))
	   (explicit-elements (coerce-to-list (element-node lte-node-criterium))))
      (setq *node-w/elements-array*
	    (list-to-array
	     (delete-duplicates
	      (flatten-no-nils-list 
	       (when (or all-driven-elements vsource-driven-elements)
		 (loop for elt in 
		       (loop for src in (vsources)
			     collect (typecase (vsource-cell-element src)
				       (segment (list (proximal-cell-element src) (distal-segments src)))
				       (soma (list (soma-segments src) (trunk-segments src)))))
		       collect (cell-element-physical-node-fast elt)))
	       (loop for node in (nodes)
		     when (and
			   (not (equal node *ground-node*))
			   (or
			    (member node explicit-elements)
			    (loop for elt in (node-elements node)
				  when (and (soma-p elt) somas) do (return t)
				  unless (or (cell-element-p elt) (not (element-enabled-p elt)))
				  do
				  (typecase elt
				    (synapse (when (and (or all-driven-elements synapse-driven-elements)
							(not (= 0.0 (synapse-gbar elt))))
					       (return t)))
				    (channel (when (and (or all-driven-elements channel-driven-elements)
							(not (= 0.0 (channel-gbar elt))))
					       (return t)))
				    (axon (when (or all-driven-elements axons) (return t)))
				    (vsource (when (or *include-vsource-nodes-in-node-error-est*
						       all-driven-elements vsource-driven-elements
						       ;; If there is only a soma, still include this node.
						       (not (segments)))
					       (return t)))
				    (isource (when (or all-driven-elements isource-driven-elements)
					       (return t))))
				  finally (return nil))))
		     collect node)))))
      (setq *node-w/elements-array-length* (length *node-w/elements-array*)
	    *make-node-w/elements-array* nil))))

(defun SET-ELEMENT-MEMBRANE-PARAMETERS (element &optional ignore-membrane-elements)
  "Use when ELEMENT dimensions or gbar ref change."
  (loop for element in (elements element) do
	(typecase element
	  (segment (set-segment-membrane-parameters element ignore-membrane-elements))
	  (soma (set-soma-membrane-parameters element ignore-membrane-elements))
	  (axon (set-axon-parameters nil element))
	  (channel (SET-channel-PARAMETERS element))
	  (synapse (SET-synapse-pARAMETERS element))
	  (conc-int (set-conc-integrator-parameters nil element))))
  nil) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-data-point (element &optional data-type model-type)
  (let* ((element (element element model-type))
	 (data-type (element-data element data-type model-type)))
    (retrieve-single-data element data-type)))

(defun element-value (element &key (target-time *real-time*) (time-list (current-sim-plot-time-list)) data-type dt data-list)
  "Returns the value of ELEMENT of DATA-TYPE [default given by the function DEFAULT-DATA-TYPE] associated with TARGET-TIME [ms,
default *REAL-TIME*]. If TARGET-TIME is not equal to the value of *REAL-TIME*. then the DATA-TYPE for ELEMENT must have already
been specified for saving, e.g. by an ENABLE-ELEMENT-PLOT. Element data time base is given by TIME-LIST [default given by
CURRENT-SIM-PLOT-TIME-LIST]. Data values for times between simulation time points are linear interpolations. Original data can be
explicitly resampled by included the arg DT [ms]. "
  (let* ((elt (element element))
	 (get-current-value-p (= target-time *real-time*)))
    (if get-current-value-p
      (the sf (element-current-value elt data-type))
      (let ((data-list (or data-list (if dt (element-data-dted elt dt data-type nil time-list) (element-data elt data-type))))
	    data-1 time-1)
	(when data-list
	  (loop for data in data-list
		for time in (if dt (loop for time from 0.0 by dt
					 repeat (length data-list)
					 collect time)
				time-list)
		when (if data-1 (and (<= time-1 target-time) (< target-time time)))
		do (return (+ data-1
			      (* (- target-time time-1)
				 (/ (- data data-1) (- time time-1)))))
		do
		(setq time-1 time
		      data-1 data)
		finally (return (s-flt data-1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element collection as function of distance.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proximals-and-distals (&optional (model-type 'segment) (proximal-distal-distance-cutoff 0.5) cell)
  (let* ((elts (list-of-all-things model-type cell))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (<= distance cutoff) collect elt into proximals
	  else collect elt into distals
	  finally (return (list proximals distals)))))

(defun distals (&optional (model-type 'segment) (proximal-distal-distance-cutoff 0.5) cell)
  (let* ((elts (list-of-all-things model-type cell))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (> distance cutoff) collect elt)))

(defun proximals (&optional (model-type 'segment) (proximal-distal-distance-cutoff 0.5) cell)
  (let* ((elts (list-of-all-things model-type cell))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (<= distance cutoff) collect elt)))

(defun distals-without (distal-border &optional (model-type 'segment) cell)
  "Returns all elements of MODEL-TYPE associated with CELL (if specified, if not then all in circuit) that are further from the
soma than DISTAL-BORDER (microns)."
  (let ((elts (list-of-all-things model-type)))
    (loop for elt in elts
	  when (> (distance-to-soma elt) distal-border)
	  collect elt)))

(defun distals-farther-than (distal-border &optional (model-type 'segment) cell) (distals-without distal-border model-type cell))

(defun proximals-within (proximal-border &optional (model-type 'segment) cell)
  "Returns all elements of MODEL-TYPE  associated with CELL (if specified, if not then all in circuit) that are closer to the soma than PROXIMAL-BORDER (microns)."
  (let ((elts (list-of-all-things model-type cell)))
    (loop for elt in elts
	  when (< (distance-to-soma elt) proximal-border)
	  collect elt)))

(defun neighbors (target radius &optional restrict-to-cell-of-target)
  "Returns list of all elements of the same type as TARGE, when TARGET is a membrane element, or the same model type if it is a
cell element, which lie at most RADIUS microns away. If RESTRICT-TO-CELL-OF-TARGET is T, then only consider elements that are
part of the same cell as TARGET."
  (let* ((target (element target))
	(reference (element-absolute-location target))
	(cell (element-cell target)))
    (when reference
      (loop for elt in
	    (typecase target
	      (synapse (synapses-of-type (synapse-type target)))
	      (channel (channels-of-type (channel-type target)))
	      (particle (particles-of-type (particle-type target)))
	      (conc-particle (conc-particles-of-type (conc-particle-type target)))
	      (axon (axons-of-type (axon-type target)))
	      (segment (segments))
	      (soma (somas))
	      (cell (cells))
	      (t (segments)))
	    when
	    (and (or (not restrict-to-cell-of-target)
		     (eq cell (element-cell elt)))
		 (< (cartesian-vector-distance reference (element-absolute-location elt)) radius))
	    collect elt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Erase element 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transfer-node-elements (from-node to-node)	
  (let ((from-element (node-cell-element from-node))
	(to-element (node-cell-element to-node)))
    (loop for element in (node-elements from-node) do
	  (typecase element
	    (axon (cond ((eq (axon-proximal-node element) from-node) (setf (axon-proximal-node element) to-node))
			((eq (axon-node element) from-node) (setf (axon-node element) to-node))))
	    (isource (setf (isource-node-2 element) to-node))
	    (vsource (setf (vsource-node element) to-node))
	    (segment (when (eq (segment-node-2 element) from-node) (setf (segment-node-2 element) to-node))
		     (when (eq (segment-node-1 element) from-node) (setf (segment-node-1 element) to-node)))
	    (soma (setf (soma-node element) to-node))
	    (channel (cond ((eq (channel-cell-element element) from-element) (setf (channel-cell-element element) to-element))
			   ((eq (channel-pre-synaptic-element element) from-element) (setf (channel-pre-synaptic-element element) to-element))))
	    (synapse (cond ((eq (synapse-cell-element element) from-element) (setf (synapse-cell-element element) to-element))
			   ((eq (synapse-pre-synaptic-element element) from-element) (setf (synapse-pre-synaptic-element element) to-element))))
	    (particle (cond ((eq (particle-vnode-point element) from-node) (setf (particle-vnode-point element) to-node))
			    ((eq (particle-cell-element element) from-element) (setf (particle-cell-element element) to-element))))
	    (conc-particle (cond ((eq (conc-particle-cnode-point element) from-node) (setf (conc-particle-cnode-point element) to-node))
				 ((eq (conc-particle-cell-element element) from-element) (setf (conc-particle-cell-element element) to-element))))
	    ; (pump (cond ((eq (pump-cell-element element) from-element) (setf (pump-cell-element element) to-element))))
	    (buffer (cond ((eq (buffer-cell-element element) from-element) (setf (buffer-cell-element element) to-element))))
	    (conc-int  (cond ((eq (conc-int-cell-element element) from-element) (setf (conc-int-cell-element element) to-element))))))
    (setf (node-elements to-node) (delete-duplicates (concatenate 'list (node-elements from-node) (node-elements to-node)) :test #'equal))
    (setf (node-elements from-node) nil)))

(defun remove-entry-from-hash (entry table)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (when (node-p entry) (setq *num-nodes* (1- *num-nodes*)))
  (maphash #'(lambda (key val) (when (eq entry val) (remhash key table))) table)
  nil)

(defun remove-entry-from-hash (entry table)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (when (node-p entry) (setq *num-nodes* (1- *num-nodes*)))
  (let ((name (element-name entry)))
    (if (eq (gethash name table) entry)
      (remhash name table)
      (loop for key being the hash-key of table
	    for value being the hash-value of table
	    when (eq value entry) do (return (remhash key table)))))
  nil)

;;; REMOVE-MODEL-INSTANCE and REMOVE-MODEL-HASH-ELT Run both functions when a circuit object TARGET is to be removed. These do not remove the object from
;;; the hash table for the object type. 

(defun remove-model-instance (target)
  ;;  
  ;;  (loop for mod being the hash-value of *model-hash-table*
  ;;        when 
  ;;        (loop for inst in (model-instances mod) 
  ;;              when (eq target inst) do (return (setf (model-instances mod) (delete inst (model-instances mod)))))
  ;;        do (return t))
  )

(defun erase-all-table-elements (hash-table)
  (loop for element being the hash-value of hash-table
	do (erase-element element)))

(defun remove-node-element (element &optional nodes)
  ;; Removes ELEMENT from the :ELEMENTS slot of all the nodes.
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (if nodes
    (loop for node in nodes
	  when (eq (car (node-elements node)) element)
	  do (setf (node-elements node) (cdr (node-elements node)))
	  else do (my-delete element (node-elements node)))
    (loop for node being the hash-value of (NODE-HASH-TABLE)
	  when (eq (car (node-elements node)) element)
	  do (setf (node-elements node) (cdr (node-elements node)))
	  else do (my-delete element (node-elements node)))))

(defun remove-element-reference-from-plot-lists (element &optional model-type)
  (let ((element-name (or (element-name element model-type) element))
	(element (element element)))
    (loop for plot-list-info in *plot-lists-info* do
	  (setf (symbol-value (plot-list-info-names plot-list-info))
		(delete element-name (symbol-value (plot-list-info-names plot-list-info)) :test 'equal))
	  (setf (symbol-value (plot-list-info-structures plot-list-info))
		(delete element (symbol-value (plot-list-info-structures plot-list-info)) :test 'equal)))))

(defun remove-instance-from-type (elt)
  (let ((elt (element elt)))
    (typecase elt
      ;; (channel (setf (channel-type-channels (channel-type elt)) (remove elt (channel-type-channels (channel-type elt)))))
      (pump (setf (pump-type-pumps (pump-type elt)) (remove elt (pump-type-pumps (pump-type elt)))))
      (buffer (setf (buffer-type-buffers (buffer-type elt)) (remove elt (buffer-type-buffers (buffer-type elt)))))
      ;; (synapse (setf (synapse-type-synapses (synapse-type elt)) (remove elt (synapse-type-synapses (synapse-type elt)))))
      ;; (particle (setf (particle-type-particles (particle-type elt)) (remove elt (particle-type-particles (particle-type elt)))))
      ;; (conc-particle (setf (conc-particle-type-conc-particles (conc-particle-type elt))
      ;;        (remove elt (conc-particle-type-conc-particles (conc-particle-type elt)))))
      (conc-int (setf (conc-int-type-conc-ints (conc-int-type elt)) (remove elt (conc-int-type-conc-ints (conc-int-type elt)))))
      (axon (setf (axon-type-axons (axon-type elt)) (remove elt (axon-type-axons (axon-type elt)))))
      (cell (setf (cell-type-cells (cell-type elt)) (remove elt (cell-type-cells (cell-type elt))))))))

(defun erase-element-type (elt)
  "Specifically for removing all elements of a given type, where ELT is either a instance of a type or points to the type itself."
  (let ((type (element-type elt)))
    (loop for elt in (elements-of-type type) do (erase-element elt type))
    (erase-element-core type)))

(defun erase-elements (&rest elements)
  ;; Erases all the members of ELEMENTS. ERASE-ELEMENT may also be used on a list of elements.
  (loop for elt in elements do (erase-element-core elt)))

(defun update-top-pointer (element)
  ;; If the appropriate top-pointer-symbol (e.g. *channel* or *soma* or *segment*) is set to the ELEMENT, then update the value of
  ;; the top-pointer-symbol to the last entry of the appropriate hash table that is not equal to ELEMENT. 
  (let ((element-top-pointer-symbol (model-top-pointer-symbol (element-model element))))
    (when (eq element (symbol-value element-top-pointer-symbol))
      (setf (symbol-value element-top-pointer-symbol)
	    (loop for elt in (reverse (hash-table-list (get-model-hash-table element)))
		  unless (eq elt (symbol-value element-top-pointer-symbol)) do (return elt))))))

(defun element-symbol-to-elements (symbol &optional model-type)
  (case symbol
    (segment (segments))
    (synapse (synapses))
    (channel (channels))
    (particle (particles))
    (conc-particle (conc-particles))
    (pump (pumps))
    (buffer (buffers))
    (conc-int (conc-ints))
    (isource (isources))
    (vsource (vsources))
    (t (element symbol model-type))))
  
;; Cell-types, Particle and Conc-particle types are only erased if there are no instances of them
(defun erase-element-core (elements &optional model-type (remove-segment-from-cell t) just-erase-top-element
			   ;; (clear-working-arrays-and-lengths)	; Drastic, but safe
			   )
  (let ((*disable-process-circuit-structure* t)
	(*automatic-run* t))
    (loop for element in (elements elements model-type) do
	  (when (and element (not (typecase element ((or cell-type particle-type conc-particle-type) (elements-of-type element)))))
	    (let* ((element-type (element-type element))
		   (element-type-parameters (element-parameters element-type)))
	      (ELIMINATE-ELEMENT-FROM-MODEL-SAVE-DATA-INSTANCES element)
	      (remove-element-reference-from-plot-lists element model-type)
	      (setq *recheck-circuit-elements-parameters* t)
	      (typecase element
		((or channel synapse particle conc-particle) (element-parameter-core element-type element-type-parameters :reorder-elements t)))
	      (typecase element
		(cell (setq remove-segment-from-cell nil))
		(channel-type
		 (unless just-erase-top-element
		   (let ((look-at-cints (element-has-conc-ints element)))
		     (erase-elements (channels-of-type element) (channel-type-particle-types element) (channel-type-conc-particle-types element))
		     (when look-at-cints (loop for cint in (conc-ints) unless (conc-int-pores cint) do (erase-element cint))))))
		(synapse-type
		 (unless just-erase-top-element 
		   (let ((look-at-cints (element-has-conc-ints element)))
		     (erase-elements (synapses-of-type element))
		     (when look-at-cints (loop for cint in (conc-ints) unless (conc-int-pores cint) do (erase-element cint)))))
		 (setq *synapse-type-list* (remove element *synapse-type-list*)))
		(channel (unless just-erase-top-element
			   (erase-elements (channel-particles element) (channel-conc-particles element))))
		(synapse (unless just-erase-top-element
			   (erase-element (synapse-channel element)))
			 (find-and-remove-element-from-event-generators element)))

	      (remove-model-instance element)
	      ;; Do this since electrodes are listed in both the segment and electrode hash table, and we need to get the associated source as well.
	      (when (and (or (not model-type) (eq model-type 'electrode) (eq model-type 'segment)) (member element (electrodes)))
		(unless just-erase-top-element
		  (erase-element (car (or (cell-element-elements element 'isource) (cell-element-elements element 'vsource))) nil remove-segment-from-cell))
		;; (remove-entry-from-hash element (ELECTRODE-HASH-TABLE))
		)

	      (update-top-pointer element)
	      ;; First call some custom routines, and get the right hash table.
	      (typecase element
		(node (erase-node element remove-segment-from-cell))
		(isource (when (member element (node-elements (element-physical-node element)))
			   (let ((isrc-node (element-physical-node element)))
			     (element-parameter isrc-node :isources (remove (element-name element) (element-parameter isrc-node :isources))))))
		(vsource (setf (node-has-ideal-voltage-source (element-node element)) nil)
			 (setq *need-to-reorder-matrix* t))
		(segment (setq *make-segment-lists* t)
			 (remove-soma-segment (cell-soma (segment-cell element)) element)
			 (when remove-segment-from-cell (setf (cell-segments (segment-cell element)) (remove element (cell-segments (segment-cell element))))))
		(particle-type (element-parameter (element-parameter element 'concentration-particle-type) 'reference-particle-type nil))
		(conc-particle-type (element-parameter (element-parameter element 'reference-particle-type) 'concentration-particle-type))
		(conc-int (remove-conc-int-from-pump element)
			  (remove-conc-int-from-particle element))
		(conc-int-type (unless (conc-int-type-conc-ints element) (remove-conc-int-type-from-particle-type element)))
		(pump (remove-pump-from-conc-int element))
		(cell (loop for win in *output-windows* when (member element (g-value win :cells))
			    do (s-value win :cells (remove element (g-value win :cells))))
		      (loop for seg in (cell-segments element) do (erase-element seg 'segment nil))
		      (loop for node in (nodes) when (eq element (node-cell node)) do (erase-node node nil))))
	      (let ((table (element-hash-table element)))
		(when (cell-element-p element)
		  (loop for isrc in (isources)
			when (element-parameter isrc 'targets)
			do (element-parameter isrc 'targets (remove element (element-parameter isrc 'targets)))))
		(typecase element
		  (conc-int-type (unless (conc-int-type-conc-ints element) (setq table nil)))
		  (buffer-type (unless (buffer-type-buffers element) (setq table nil)))
		  (pump-type (unless (pump-type-pumps element) (setq table nil)))
		  ((or segment soma) (loop for win in *output-windows* when (eq element (g-value win :chosen-one)) do (s-value win :chosen-one nil))))
		(remove-instance-from-type element)
		(when table
		  (remove-entry-from-hash element table)
		  (remove-node-element element (typecase element
						 (synapse (SYNAPSE-IN-ELEMENTS-OF element))
						 (segment (list (segment-node-1 element) (segment-node-2 element)))
						 (soma (list (soma-node element)))))
		  (set-element-parameter-fast ELEMENT-type :reorder-elements t element-type-parameters)
		  (unless just-erase-top-element
		    (typecase element
		      (segment (loop for elt in  (node-elements (segment-node-2 element)) do (erase-element elt nil remove-segment-from-cell))
			       (erase-element (segment-node-2 element) 'node remove-segment-from-cell))
		      (soma (erase-element (soma-cell element) 'cell remove-segment-from-cell)
			    (erase-element (soma-node element)))))))
	      (remove-active-element-lists element-type))
	    (setq *make-node-w/elements-array* t)))))

(defun remove-active-element-lists (type)
  (loop for type in (coerce-to-list type) do
	(let* ((type (element-type type))
	       (active-symbol (typecase type
				(pump-type 'active-pumps) 
				(buffer-type 'active-buffers)
				(conc-int-type 'active-conc-ints))))
	  (element-parameter type active-symbol nil))))

(defun find-and-remove-element-from-event-generators (element)
  (let ((element element))
    (typecase element
      (synapse (synapse-type-iterator
		(syn (synapse-type element))
		when (eq element (synapse-event-generator syn))
		do (set-element-event-generator-slot-value syn nil)
		(let ((followers (and (not (eq syn element)) (fast-syn-event-followers syn))))
		  (when followers (fast-set-syn-event-followers syn (remove element followers))))))
      (t (loop for thing in (all-things-of-same-type element)
	       when (eq element (event-generator thing t)) do (set-element-event-generator-slot-value thing nil)
	       when (and (not (eq thing element)) (event-followers thing nil)) do (event-followers thing (remove element (event-followers thing))))))))

(defun erase-node (node &optional (remove-segment-from-cell t))
  (setq *node-voltage-initializations* (loop for pair in *node-voltage-initializations* unless (eq node (car pair)) collect pair))
  (loop for prt in (particles)
	when (eq node (particle-vnode-point prt))
	do (setf (particle-vnode-point prt) nil)
	(format t "Setting Particle ~A Vnode pointer to NIL (was pointing to ~A).~%" (particle-name prt) (node-name node))) 
  (loop for elt in (node-elements node)	do (erase-element elt nil remove-segment-from-cell)))

(defun no-input-p () (not (or (isources) (vsources) (synapses))))

(defun element-type (element &optional model-type create)
  "Returns as values a list \(if more than one\) or atom \(if just one\) of all element types associated with ELEMENT of
MODEL-TYPE, and also a flag if any new types are created (when CREATE is non-NIL). If CREATE is non-NIL [default NIL], then
element types associated with symbols in ELEMENT will be created (or updated if already exists) according to their definitions in the parameter library."
  (let* ((new-type-created nil)
	 (output-elements
	  (if (and (not create) (element-type-p element) (or (not model-type) (eq model-type (type-of element))))
	    (element element)
	    (or (element-wrapper (element model-type nil (not element)) (element-type-core elt internal-type create))
		(atomize-list (no-nils (mapcar #'(lambda (element)
						   (multiple-value-bind (output-type new-type-created-internal)
						       (element-type-core
							(unless (OBJECT-TYPE-SYMBOL-P element) (element element model-type)) element create)
						     (setq new-type-created (or new-type-created new-type-created-internal))
						     output-type))
					       (coerce-to-list element))))))))
    (values output-elements new-type-created)))

(defun element-type-core (elt &optional element create)
  (let* ((new-type-created nil)
	 (output-type
	  (if (and elt (not create))
	    (if (element-type-p elt) elt (let (*element-slot-t-typecase-error-p*) (element-slot elt :type)))
	    (when create
	      (when (stringp element) (setq element (intern element)))
	      (loop for model in (models)
		    when (get-a-value element (model-parameter-type-library model))
		    return (progn (setq new-type-created t)
				  (funcall (model-create-routine model) element)))))))
    (values output-type new-type-created)))

(defun element-types (element &optional model-type create) (coerce-to-list (element-type element model-type create)))

(defun element-soma (element &optional model-type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all soma associated with the elements associated with ELEMENT of
MODEL-TYPE."
  (typecase element
    (cell element)
    (t (element-wrapper (element model-type) (cell-soma (element-cell-core elt internal-type))))))

(defun element-cell (element &optional model-type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all cells associated with the elements associated with ELEMENT
of MODEL-TYPE."
  (typecase element
    (cell element)
    (t (element-wrapper (element model-type) (element-cell-core elt internal-type)))))

(defun element-cell-core (element &optional model-type)
  (if (cell-p element)
    element
    (let ((elt (element element model-type)))
      (if (cell-p elt)
	elt
	(or (let (*element-slot-t-typecase-error-p*) (element-slot elt :cell))
	    (if (element-type-p elt)
	      (loop for cell being the hash-value of (cell-hash-table) when (instance-in-cell elt nil cell) collect cell)
	      (let ((node (element-physical-node elt model-type)))
		(when node (node-cell node)))))))))

(defun erase-element (element &optional model-type (remove-segment-from-cell t) just-erase-top-element)
  "Erase ELEMENT, if it is singular, or the members of ELEMENT, if it is a list.
Erased elements are restricted to  MODEL-TYPE, if specified [default NIL]. If erasing a segment, 
be sure to remove it from its cell when REMOVE-SEGMENT-FROM-CELL is non-nil [default T]. For erasing all segments
of a cell, it is more efficient to remove the segments from the cell's :SEGMENTS slot separately.

If JUST-ERASE-TOP-ELEMENT is nil [default], then remove
all elements which are components of an erased element (i.e. particles from a channel). ELEMENT can also be a symbol such as 'SYNAPSE or 'CHANNEL, in which
case all instances of that type of element will be erased.

If element is a cell, then all elements of that cell are erased"
  (cond
;;    ((cell-p element) (erase-element (cell-elements element)))
    (element (erase-element-core element model-type remove-segment-from-cell just-erase-top-element))
    ((not *automatic-run*) (element-wrapper (element model-type) (erase-element-core elt internal-type remove-segment-from-cell just-erase-top-element))))
  (reorder-model-elements-as-needed))

(defun print-element (element &optional model-type (stream *standard-output*))
  "Print documentation apropos for elements associated with ELEMENT."
  (unless *circuit-processed* (process-circuit-structure))
  (let ((*standard-output* stream))
    (element-wrapper
     (element model-type)
     (progn
       (if (electrode-p elt)
	   (print-electrode elt)
	   (let ((print-routine (model-print-routine (element-model elt internal-type))))
	     (if print-routine (funcall print-routine elt) (element-funcall 'print elt internal-type))))
       (format t "~&")))))

(defun turn-off (element &optional model-type)
  "Generic disable (blocking) for elements associated with ELEMENT."
  (disable-element element model-type))

(defun disable-element (element &optional model-type)
;;  "Generic disable (blocking) for elements associated with ELEMENT."
  (element-wrapper 
   (element model-type)
   (when (element-slot-p elt :blocked) (element-slot elt :blocked t))))

(defun turn-on (element &optional model-type)
  "Generic enable (unblocking) for elements associated with ELEMENT."
  (enable-element element model-type))

(defun enable-element (element &optional model-type)
;;  "Generic enable (unblocking) for elements associated with ELEMENT."
  (element-wrapper 
   (element model-type)
   (when (element-slot-p elt :blocked) (element-slot elt :blocked nil))))

(defun element-enabled-p (element &optional model-type)
  (let ((elt (element element model-type)))
    (not (and (element-slot-p elt :blocked)
	      (element-slot elt :blocked)))))

(defun print-elements (elements &optional (stream *standard-output*)) (loop for elt in elements do (print-element elt nil stream)))

(defvar *document-elements-for-circuit-dump* nil) ; Provide default value of CIRCUIT-DUMP arg in document element routines.

(defun document-element (element &optional model-type (circuit-dump *document-elements-for-circuit-dump*))
  "Generates loadable description of ELEMENT."
  (let ((*document-elements-for-circuit-dump* circuit-dump)
	(*print-pretty* t))
    (element-wrapper (element model-type) (element-funcall 'document elt internal-type))))

#|
(defun element-document-extras (element)
  (when (element-parameter element 'nice-name)
    (format t " (nice-name . ~a)~%" (element-parameter element 'nice-name))))
|#

(defun element-document-extras (element)
  (no-nils
   (list
    (cons 'creation-date (print-date nil t))
    (when (element-parameter element 'nice-name) (cons 'nice-name (element-parameter element 'nice-name))))))

(defun print-element-document-extras (element)
  (mapcar #'(lambda (list) (format t "  ~S~%" list)) (element-document-extras element))
  nil)

#|
(defun edit-element (element &optional model-type)
  "Edit menu for properties of ELEMENT."
  (element-wrapper
   (element model-type nil nil t)
   (progn (element-funcall 'edit elt internal-type)
	  (when (and (plot-menu-class-enable elt internal-type)
		     (go-ahead-menu (format nil "Edit Plot for ~A ~A" (type-of (element elt internal-type)) (element-name elt internal-type)) "Authorization" nil))
	    (plot-element-menu elt internal-type)))))
|# 

(defun edit-element (element &optional model-type)
  "Edit menu for properties of ELEMENT."
  (loop for element in (elements element model-type) do
	(element-funcall 'edit element model-type)
	(when (and (plot-menu-class-enable element model-type)
		   (go-ahead-menu (format nil "Edit Plot for ~A ~A" (type-of (element element model-type)) (element-name element model-type)) "Authorization" nil))
	  (plot-element-menu element model-type))))

;; This allows for names that are either strings, numbers or symbols.
(defun same-element-names (name1 name2)
  (or (eq name1 name2)
      (and (stringp name1) (stringp name2) (string= name1 name2))
      (and (numberp name1) (numberp name2) (= name1 name2))))
      
(defun element-name (element &optional model-type)
  "The printed name for ELEMENT."
  (element-wrapper (element model-type) (element-slot elt :name)))

(defun element-names (element &optional model-type) (coerce-to-list (element-name element model-type)))

(defun element-control-waveform (element)
  "Return the controlling sequence (list or array) associated with ELEMENT, if it exists. Examples include conductance waveforms
for event synapses or synapse types, explicit current or voltage waveforms for current or voltage sources, respectively."
  (let ((element-type (element-type element)))
    (cond ((source-p element) (source-waveform-array element))
	  ((synapse-type-p element-type) (element-parameter element-type 'waveform)))))

(defun element-control-waveform-timestep (element)
  "Return the timestep associated with the controlling sequence associated with ELEMENT as given by the function
ELEMENT-CONTROL-WAVEFORM, if this sequencee exists. Otherwise return NIL."
  (let ((elt (element element)))
    (if (source-p elt)
      (/ 1.0 (source-waveform-time-interval-inverse elt))
      (let ((element-type (element-type elt)))
	(when (synapse-type-p element-type) (synapse-type-waveform-interval element-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Distributing elements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-distribution (type total-number targets distribution-function &rest distribution-function-args)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability given by DISTRIBUTION-FUNCTION and the optional
DISTRIBUTION-FUNCTION-ARGS. DISTRIBUTION-FUNCTION may be either a function or the keyword :FLAT. In the first case, the function is applied to the distance
to the soma of each cell element. The first argument of DISTRIBUTION-FUNCTION is the cell element, with possible additional arguments given by
DISTRIBUTION-FUNCTION-ARGS, and DISTRIBUTION-FUNCTION must return a single-float. If DISTRIBUTION-FUNCTION is set to :FLAT, then the targets that receive
an element of TYPE are chosen with an equal probability."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((total-number (round total-number))
	(targets (or targets (segments)))
	elts)
    (case DISTRIBUTION-FUNCTION
      (:flat (create-element type (sequence-head (shuffled-list targets) total-number)))
      (t (loop until
	       (loop for target in (shuffled-list targets)
		     when (< (random 1.0) (the sf (apply DISTRIBUTION-FUNCTION (cons target DISTRIBUTION-FUNCTION-ARGS))))
		     do (push (create-element type target) elts)
		     when (= (length elts) total-number) do (return t)))))))

(defun propagate-element-distribution (type total-number targets distribution-function &rest distribution-function-args)
  (let ((distribution-function-coefficient
	 (/ total-number
	    (loop for target in targets summing (apply DISTRIBUTION-FUNCTION (cons target DISTRIBUTION-FUNCTION-ARGS))))))
    (format t "Total leftover: ~A~%, distribution-function-coefficient: ~A"
	    (propagate-element-distribution-core
	     targets type 0.0
	     distribution-function distribution-function-args
	     distribution-function-coefficient)
	    distribution-function-coefficient)))

(defun propagate-element-distribution-core (targets type proximal-leftover distribution-function distribution-function-args distribution-function-coefficient)
  (multiple-value-bind (elts-here elts-there)
      (truncate (the sf (+ proximal-leftover
			   (* distribution-function-coefficient
			      (apply DISTRIBUTION-FUNCTION (cons (car targets) DISTRIBUTION-FUNCTION-ARGS))))))
    (dotimes (i elts-here) (create-element type (car targets)))
    (if (cdr targets)
      (propagate-element-distribution-core
       (cdr targets) type elts-there
       distribution-function distribution-function-args
       distribution-function-coefficient)
      elts-there)))

(defun cumulative-pdf-element-distribution (type total-number targets distribution-function &rest distribution-function-args)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability given by DISTRIBUTION-FUNCTION and its
DISTRIBUTION-FUNCTION-ARGS, applied to each target. It is assumed that the first argument of DISTRIBUTION-FUNCTION is the target, with possible additional
arguments given by DISTRIBUTION-FUNCTION-ARGS. DISTRIBUTION-FUNCTION must return a single-float. TOTAL-NUMBER must be a fixnum. The cumulative probability
distribution function for DISTRIBUTION-FUNCTION is integrated over all the TARGETS, with a final value of TOTAL-NUMBER, and each target is assigned the
value of the cpdf after taking into account their contribution to the integral (given by applying DISTRIBUTION-FUNCTION to the given target).  Elements of
TYPE are added one at a time, by generating a random number (range given by TOTAL-NUMBER) THIS-ROLL and finding the target which is associated with
THIS-ROLL in the cumulative PDf."
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum total-number))
  (let ((targets (or targets (segments)))
	(cumulative-probability-sym (gensym "cumulative-probability")) ; Make a new symbol just for use within this function.
	(cumulative-probability 0.0))
    (declare (single-float cumulative-probability))
    ;; Integrate the cumulative probability distribution function (CPDF) over all the potential targets and store the value of
    ;; this integral in each target after accounting for the target's contribution to the integral.
    (loop for target in targets
	  do (incf cumulative-probability (the sf (apply DISTRIBUTION-FUNCTION (cons target DISTRIBUTION-FUNCTION-ARGS))))
	  (element-parameter target cumulative-probability-sym cumulative-probability))

    (loop for i fixnum from 1 to total-number do
	  (let* ((search-cumulative-probability 0.0)
		 (new-search-cumulative-probability 0.0)
		 (this-roll (random cumulative-probability)))
	    (declare (single-float search-cumulative-probability new-search-cumulative-probability this-roll))
	    (loop for target in targets
		  do (setq new-search-cumulative-probability (the sf (element-parameter target cumulative-probability-sym)))
		  when (and (< search-cumulative-probability this-roll) (<= this-roll new-search-cumulative-probability))
		  do (create-element type target) (return t)
		  else do (setq search-cumulative-probability new-search-cumulative-probability))))
    ;; Clear out the cumulative-probability entry in the parameter lists.
    (loop for target in targets do (element-parameter target cumulative-probability-sym nil))))

(defun collect-cell-elements-by-distance (elements distance-resolution &optional reverse-order)
  (let ((targets (coerce-to-list (element-cell-element elements)))
	index)
    (loop for target in targets maximize (distance-to-soma target) into maximum-distance
	  finally (return
		   (let* ((bins (loop for distance from 0 to maximum-distance by distance-resolution collect nil))
			  (number-of-bins (length bins)))
		     (loop for target in targets
			   do (setq index (round (* (1- number-of-bins) (/ (distance-to-soma target) maximum-distance))))
			   (push target (nth index bins)))
		     (if reverse-order (reverse bins) bins))))))

(defun sprinkle-elements-exponentially (type targets total-number length-constant distance-resolution)
  (let* ((reverse-order (minusp length-constant))
	 (length-constant (abs (s-flt length-constant)))
	 (binned-targets (collect-cell-elements-by-distance targets distance-resolution reverse-order))
	 (new-elements 0)
	 target)
    (loop until (= new-elements total-number)
	  when (setq target (random-nth (nth (round (/ (exponential-pdf length-constant) distance-resolution)) binned-targets)))
	  do (create-element type target)
	  (incf new-elements))))

(defun gaussian-element-distribution (type targets total-number mean-distance sd)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability given by a gaussian of the
difference between MEAN-DISTANCE and the distance to the soma of each cell element, with a standard deviation of SD."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((targets (or targets (segments)))
	(mean-distance (s-flt mean-distance))
	(sd (s-flt sd))
	elts)
    (loop until
	  (loop for target in targets
		when (< (random 1.0) (the sf (gaussian (the sf (- (the sf mean-distance) (distance-to-soma target))) 0.0 (the sf (square (the sf sd))))))
		do (push (create-element type target) elts)
		when (= (length elts) total-number) do (return t)))))

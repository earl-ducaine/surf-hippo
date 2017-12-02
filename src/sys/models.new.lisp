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


;;; SYS Source file: models.lisp
					
;; The create model functions.

(in-package "SURF-HIPPO")

(defmacro DATA-TYPE-ACCESS-INFO (model-name symbols units-string &optional CURRENT-VALUE-FUNCTION SAVED-DATA-FUNCTION)
  (let* ((symbols (coerce-to-list symbols))
	 (ref-symbol (car symbols)))
    `(list
      ',(atomize-list symbols)
      '(units ,units-string)
      '(DATA-PARAM-KEY ,(read-from-string (FORMAT nil ":~A-data" ref-symbol)))
      '(SPARSE-DATA-PARAM-KEY ,(read-from-string (FORMAT nil ":SPARSE-~A-data" ref-symbol)))
      '(ORDERED-SPARSE-DATA-PARAM-KEY ,(read-from-string (FORMAT nil ":ordered-SPARSE-~A-data" ref-symbol)))
      '(CURRENT-VALUE-FUNCTION ,(or CURRENT-VALUE-FUNCTION (read-from-string (format nil "~A-~a-value" model-name ref-symbol))))
      '(SAVED-DATA-FUNCTION ,(or SAVED-DATA-FUNCTION (read-from-string (format nil "~A-~a-data" model-name ref-symbol)))))))
  
(defmacro model-output-wrapper (model &body body)
  `(let* ((model-arg ,model)
	  (model (when model-arg (or (gethash (model-child-structure-type model-arg) *model-hash-table*) model-arg))))
    ,@body))

(defun model-output-data-keys (model)
  (mapcar #'(lambda (DATA-TYPE-AND-ACCESS-info) (cadr (assoc 'DATA-PARAM-KEY (cdr DATA-TYPE-AND-ACCESS-info)))) (model-DATA-TYPES-AND-ACCESS-INFO model)))

(defun model-output-data-types (model)
  (model-output-wrapper model (mapcar #'(lambda (DATA-TYPE-AND-ACCESS-info) (car DATA-TYPE-AND-ACCESS-info)) (model-DATA-TYPES-AND-ACCESS-INFO model))))

;; The first entry in DATA-TYPES-AND-ACCESS-INFO gives the default data type. If there are synonyms, take the first one.
(defun model-output-default-data-type (model)
  (let ((first-type (car (model-output-data-types model))))
    (if (consp first-type) (car first-type) first-type)))

(defun model-output-data-type-info (model data-type)
  (loop for type-info in (model-DATA-TYPES-AND-ACCESS-INFO model) do
	(let ((result (if (consp (car type-info)) (member data-type (car type-info)) (equal data-type (car type-info)))))
	  (when result (return type-info)))))

(defun model-output-data-info (model data-type info-key)
  (cadr (assoc info-key (cdr (model-output-data-type-info model (or data-type (model-output-default-data-type model)))))))

(defun model-output-data-key (model &optional data-type) (model-output-data-info model data-type 'data-param-key))
(defun model-output-data-units (model &optional data-type) (model-output-data-info model data-type 'units))
(defun model-output-sparse-data-key (model &optional data-type) (model-output-data-info model data-type 'sparse-data-param-key))
(defun model-output-ordered-sparse-data-key (model &optional data-type) (model-output-data-info model data-type 'ordered-sparse-data-param-key))
(defun model-output-current-value-function (model &optional data-type) (model-output-data-info model data-type 'current-value-function))
(defun model-output-saved-data-function (model &optional data-type) (model-output-data-info model data-type 'saved-data-function))	       

(defun create-model (name &key
			  (parent-structure-type (unless (search "-TYPE" (string name)) (read-from-string (format nil "~A-type" name))))
			  (child-structure-type (when (search "-TYPE" (string name)) (string-head (string name) (search "-TYPE" (string name)))))
			  output-data-structure-variables DATA-TYPES-AND-ACCESS-INFO 
			  (save-output-data-routine (read-from-string (format nil "save-~A-data" name)))
			  (edit-routine (read-from-string (format nil "edit-~A" name)))
			  (eval-routine (read-from-string (format nil "eval-~A" name)))
			  (print-routine (read-from-string (format nil "print-~A" name)))
			  (short-print-routine (read-from-string (format nil "print-~A-brief" name)))
			  (document-routine (read-from-string (format nil "document-~A" name)))
			  (create-routine (read-from-string (format nil "create-~A" name))))
  (let* ((name (if (stringp name) (read-from-string name) name))
	 (top-pointer-symbol (read-from-string (format nil "*~a*" name)))
	 (model (make-model :name name :child-structure-type child-structure-type :parent-structure-type parent-structure-type
			    :parameter-type-library '() :hash-table (make-hash-table :test #'equal)
			    :output-data-structure-variables output-data-structure-variables
			    :DATA-TYPES-AND-ACCESS-INFO DATA-TYPES-AND-ACCESS-INFO
			    :save-output-data-routine (when (fboundp save-output-data-routine) save-output-data-routine)
			    :edit-routine (when (fboundp edit-routine) edit-routine)
			    :eval-routine (when (fboundp eval-routine) eval-routine)
			    :print-routine (when (fboundp print-routine) print-routine)
			    :short-print-routine (when (fboundp short-print-routine) short-print-routine) 
			    :document-routine (when (fboundp document-routine) document-routine)
			    :create-routine (when (fboundp create-routine) create-routine)
			    :top-pointer-symbol top-pointer-symbol))

	 (thing-string (string-downcase (string name)))
	 (elt-type-sym (read-from-string (format nil "~A-type" thing-string)))
	 (elt-type-sym-fboundp (fboundp elt-type-sym))

	 (whatevers-doc-format-string (concatenate 'string
						   "Returns a list of all ~(~A~)s associated with the cell elements referenced by ELEMENT, "
						   "if supplied; otherwise, all ~(~A~)s in circuit."))
	 (whatevers-symbol-collector-function (read-from-string (format nil "~As" name)))
	 (whatevers-function-doc-string (format nil whatevers-doc-format-string name name))
	 
	 (node-functions-args-symbol (read-from-string (format nil "(element~A)" (if elt-type-sym-fboundp (format nil " &optional ~A" elt-type-sym) ""))))
	 (node-functions-doc-string (format nil "Return all ~As associated with the cell element of ELEMENT.~a"
					    thing-string
					    (if elt-type-sym-fboundp
					      (format nil " If ~A is supplied, then only return ~As that match ~A." elt-type-sym thing-string elt-type-sym)
					      "")))	 
	 (hash-function-name (read-from-string (format nil "~A-hash-table" name)))
	 (hash-function-doc (concatenate 'string "With KEY, returns the associated hash value from the "
					 (format nil "~A model hash table; otherwise returns the table. " name)
					 "Entries can be added using SETF.")))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(progn
	(defun ,whatevers-symbol-collector-function (&optional (element nil element-supplied-p))
	  ,whatevers-function-doc-string
	  (if element-supplied-p (element-collector (quote ,name) element) (element-collector (quote ,name))))
	(defun ,(read-from-string (format nil "node-~as" name)) ,node-functions-args-symbol
	  ,node-functions-doc-string
	  ,(if (not elt-type-sym-fboundp)
	     `(cell-element-elements element ',name)
	     `(let* ((all-elts-of-type (cell-element-elements element ',name))
		     (element-type (element ,elt-type-sym)))
		(if ,elt-type-sym (delete-if-not #'(lambda (obj) (eq (,elt-type-sym obj) element-type)) all-elts-of-type) all-elts-of-type))))
	(defun ,hash-function-name (&optional (key nil key-supplied-p))
	  ,hash-function-doc
	  (let ((table (model-hash-table (get (quote ,name) (quote model)))))
	    (if key-supplied-p (gethash key table) table)))
	(defun (setf ,hash-function-name) (new-value key)
	  (setf (gethash key (,hash-function-name)) new-value))
	(defvar ,top-pointer-symbol nil ,(format nil "The last created ~(~A~)." name))))
    (setf (get name 'model) model
	  (gethash name *model-hash-table*) model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-models ()
  (create-model 'node :parent-structure-type nil)
  (create-model 'channel-type)
  (create-model 'channel
		:output-data-structure-variables
		'(*plot-channel-currents-structures* *plot-channel-reversal-potentials-structures* *plot-channel-conductances-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO channel CURRENT "nA")
					      ,(DATA-TYPE-ACCESS-INFO channel REVERSAL-POTENTIAL "mV")
					      ,(DATA-TYPE-ACCESS-INFO channel CONDUCTANCE "uS")))
  (create-model 'synapse-type)
  (create-model 'synapse
		:output-data-structure-variables
		'(*plot-synapse-currents-structures* *plot-synapse-reversal-potentials-structures* *plot-synapse-conductances-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO synapse CURRENT "nA")
					      ,(DATA-TYPE-ACCESS-INFO synapse REVERSAL-POTENTIAL "mV")
					      ,(DATA-TYPE-ACCESS-INFO synapse CONDUCTANCE "uS")))
  (create-model 'particle-type)
  (create-model 'particle
		:output-data-structure-variables '(*plot-particles-structures* *plot-markov-particles-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO particle state "State")))
  (create-model 'conc-particle-type)
  (create-model 'conc-particle
		:output-data-structure-variables '(*plot-conc-particles-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO conc-particle state "State")))
  (create-model 'conc-int-type)
  (create-model 'conc-int
		:output-data-structure-variables
		'(*plot-conc-1-ints-structures* *plot-conc-2-ints-structures* *plot-conc-3-ints-structures* *plot-conc-ints-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO conc-int (TOTAL TOTAL-CONCENTRATION) "mM")
					      ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-1 1 CONCENTRATION-1) "mM")
					      ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-2 2 CONCENTRATION-2) "mM")
					      ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-3 3 CONCENTRATION-3) "mM")))
  (create-model 'isource
		:parent-structure-type nil
		:output-data-structure-variables '(*plot-isource-currents-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO isource current "nA" iSOURCE-CURRENT-VALUE iSOURCE-CURRENT-DATA)))
  (create-model 'vsource
		:parent-structure-type nil
		:output-data-structure-variables '(*plot-vsource-currents-structures* *plot-vsource-voltages-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO vsource voltage "mV")
					      ,(DATA-TYPE-ACCESS-INFO vsource current "nA")))
  (create-model 'axon-type)
  (create-model 'axon
		:output-data-structure-variables '(*plot-axons-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO axon voltage "mV")))
  (create-model 'buffer-type)
  (create-model 'buffer
		:output-data-structure-variables '(*plot-buffers-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO buffer CONCENTRATION "mM")))
  (create-model 'cell-type)
  (create-model 'cell :short-print-routine 'print-cell)
  (create-model 'extracellular-electrode
		:parent-structure-type nil
		:output-data-structure-variables '(*plot-field-potentials*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO extracellular-electrode FIELD-POTENTIAL "mV")))
  (create-model 'electrode :parent-structure-type nil)
  (create-model 'pump-type)
  (create-model 'pump
		:output-data-structure-variables '(*plot-pumps-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO pump current "nA")))
  (create-model 'segment
		:parent-structure-type nil
		:output-data-structure-variables nil
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO segment voltage "mV" recorded-element-voltage)
					      ,(DATA-TYPE-ACCESS-INFO segment (VOLTAGE-DERIVATIVE DVDT NODE-VOLTAGE-DERIVATIVE) "mV/ms" ELEMENT-dvdt)
					      ,(DATA-TYPE-ACCESS-INFO segment leak-current "nA" element-LEAK-CURRENT-VALUE)
					      ,(DATA-TYPE-ACCESS-INFO segment CAPACITANCE-CURRENT "nA" element-CAPACITANCE-CURRENT-VALUE)))
  (create-model 'soma
		:parent-structure-type nil
		:output-data-structure-variables
		'(*all-save-voltage-nodes* *all-save-dvdt-nodes* *all-save-capacitance-current-nodes* *all-save-leak-current-nodes*
					   *plot-soma-dendrite-currents-structures*)
		:DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO soma voltage "mV" RECORDED-ELEMENT-VOLTAGE)
					      ,(DATA-TYPE-ACCESS-INFO soma (VOLTAGE-DERIVATIVE DVDT NODE-VOLTAGE-DERIVATIVE) "mV/ms" ELEMENT-dvdt)
					      ,(DATA-TYPE-ACCESS-INFO soma leak-current "nA" element-LEAK-CURRENT-VALUE)
					      ,(DATA-TYPE-ACCESS-INFO soma CAPACITANCE-CURRENT "nA" element-CAPACITANCE-CURRENT-VALUE)
					      ,(DATA-TYPE-ACCESS-INFO soma dendrite-CURRENT "nA"))))

#|
(defun create-models ()
  `(progn
     ,@(mapcar 
	#'(lambda (sym)
	    (let ((create-function (read-from-string (format nil "create-~A-model" sym))))
	      (when (fboundp create-function) `(,create-function))))
	*model-names*)))
|#

(eval (create-models))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The definition of the function CELL-TYPES generated by evaluating CREATE-MODELS is redefined below.
(defun cell-types (&optional element)
  "Returns a list of all cell types associated with the cell elements referenced by ELEMENT. If there are no such cell elements,
or ELEMENT is NIL, then a list of all cell-types in circuit is returned."
  (let ((element-cell (element-cell element)))
    (if element-cell
      (coerce-to-list (element-type element-cell))
      (let ((cell-types (hash-table-list (CELL-TYPE-HASH-TABLE))))
	(delete-if-not 'element-in-circuit cell-types)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; need to defstruct NAME as well, e.g. (defstruct axon-type ...)
;; also TYPE-DEF macro e.g. (defmacro axon-type-def (body) ...)
(defun add-object-type (name
			&key
			parameter-type-library
			save-output-data-routine
			output-data-structure-variables
			output-data-keys
			edit-routine
			eval-routine
			print-routine
			short-print-routine
			document-routine
			create-routine)
  "For adding type NAME to the element system."
  (let ((type-sym (typecase name
		    (string (read-from-string name))
		    (t name))))
    (push type-sym *model-names*)
    (create-model type-sym
		  :parameter-type-library parameter-type-library
		  :save-output-data-routine save-output-data-routine
		  :output-data-structure-variables output-data-structure-variables
		  :output-data-keys output-data-keys
		  :edit-routine edit-routine
		  :eval-routine eval-routine
		  :print-routine print-routine
		  :short-print-routine short-print-routine
		  :document-routine document-routine
		  :create-routine create-routine)))
|#

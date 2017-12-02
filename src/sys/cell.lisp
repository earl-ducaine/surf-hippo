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


;;; SYS Source file: cell.lisp


(in-package "SURF-HIPPO")

;;; This is the cell file.

(defun get-top-cell () (car (cells)))

(defun get-center-cell ()
  "Return the center cell, in 3D space, of the network."
  (multiple-value-bind (min-x max-x min-y max-y min-z max-z)
      (min-max-circuit-coordinates)
    (let ((center-x (/ (- max-x min-x) 2.0))
	  (center-y (/ (- max-y min-y) 2.0))
	  (center-z (/ (- max-z min-z) 2.0)))
      (caar
       (sort (loop for cell in (cells) collect (list cell (cartesian-vector-distance-float (where cell) (list center-x center-y center-z))))
	'< :key 'second)))))

(defun print-cell-type-concentrations (&optional cell-type)
  (loop for cell-type in (coerce-to-list (or cell-type (cell-types))) do
	(format t "    [X] in/out(mM): [Na+] ~a/~a (~A), [K+] ~a/~a (~A)~%                    [Cl-] ~a/~a (~A), [Ca++] ~a/~a (~A)~%"
		(simple-format-number (cell-type-na-conc-intra cell-type))
		(simple-format-number (cell-type-na-conc-extra cell-type))
		(cell-type-na-conc-extra-dependence cell-type)
		(simple-format-number (cell-type-k-conc-intra cell-type))
		(simple-format-number (cell-type-k-conc-extra cell-type))
		(cell-type-k-conc-extra-dependence cell-type)
		(simple-format-number (cell-type-cl-conc-intra cell-type))
		(simple-format-number (cell-type-cl-conc-extra cell-type))
		(cell-type-cl-conc-extra-dependence cell-type)
		(simple-format-number (cell-type-ca-conc-intra cell-type))
		(simple-format-number (cell-type-ca-conc-extra cell-type))
		(cell-type-ca-conc-extra-dependence cell-type))))

(defun update-cell-type-ionic-parameters (&optional type)
  (loop for type in (if type (list type) (cell-types))
	do
	(case (cell-type-na-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-na-conc-extra type) *na-conc-extra*)))
	(case (cell-type-e-na-dependence type)
	  (:follows-global (setf (cell-type-e-na type) *e-k*))
	  (:follows-concentration (setf (cell-type-e-na type) (nernst-potential (cell-type-na-conc-intra type) (cell-type-na-conc-extra type)))))
	(case (cell-type-k-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-k-conc-extra type) *k-conc-extra*)))
	(case (cell-type-e-k-dependence type)
	  (:follows-global (setf (cell-type-e-k type) *e-k*))
	  (:follows-concentration (setf (cell-type-e-k type) (nernst-potential (cell-type-k-conc-intra type) (cell-type-k-conc-extra type)))))
	(case (cell-type-ca-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-ca-conc-extra type) *ca-conc-extra*)))
	(case (cell-type-e-ca-dependence type)
	  (:follows-global (setf (cell-type-e-ca type) *e-ca*))
	  (:follows-concentration (setf (cell-type-e-ca type) (nernst-potential (cell-type-ca-conc-intra type) (cell-type-ca-conc-extra type) 2))))
	(case (cell-type-cl-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-cl-conc-extra type) *cl-conc-extra*)))
	(case (cell-type-e-cl-dependence type)
	  (:follows-global (setf (cell-type-e-cl type) *e-cl*))
	  (:follows-concentration (setf (cell-type-e-cl type) (nernst-potential (cell-type-cl-conc-intra type) (cell-type-cl-conc-extra type) -1))))))

(defun update-linear-z-in-cell-type (cell-type) (loop for cell in (cell-type-cells cell-type) do (update-linear-z-in cell)))

(defun update-linear-z-in-cells () (loop for cell in (cells) do (update-linear-z-in cell)))

(defun update-linear-z-in (cell)
  (setf (cell-max-g-in cell) (max-g-in cell))

  ;; Z-TREE-DISCRETE-IN-CELL and Z-TREE-CABLE-IN-CELL store the associated distal tree impedance in each trunk segment. These
  ;; values are then referenced by the subsequent calls to Z-TREE-DISCRETE-IN-CELL-FROM-STORED-VALUES and
  ;; Z-TREE-CABLE-IN-CELL-FROM-STORED-VALUES.
  
  (setf (cell-z-tree-discrete-in-cell cell) (z-tree-discrete-in-cell cell nil))
  (setf (cell-z-discrete-in-cell cell) (z-discrete-in-cell cell (z-tree-discrete-in-cell-from-stored-values cell t)))

  (setf (cell-z-tree-cable-in-cell cell) (z-tree-cable-in-cell cell nil))
  (setf (cell-z-cable-in-cell cell) (z-cable-in-cell cell (Z-TREE-CABLE-IN-CELL-FROM-STORED-VALUES cell t))))
						     
(defun cell-type-parameter (element param &optional (value nil value-supplied-p) (update t))
  "For examining/setting specific structure parameters of the cell type associated with ELEMENT. PARAM can be:

     :RI [ohms-cm]

These set both the soma and the dendritic values -

     :RM [ohms-cm2]
     :V-LEAK [mV]
     :CM [uF/cm2]

These set only the dendritic values-

     :RM-DENDRITE [ohms-cm2]
     :V-LEAK-DENDRITE [mV]
     :CM-DENDRITE [uF/cm2]

These set only the somatic values-

     :RM-SOMA [ohms-cm2]
     :V-LEAK-SOMA [mV]
     :CM-SOMA [uF/cm2]
     :SOMA-SHUNT [ohms]

If :CM is specified, this value is assigned to both the somatic and dendritic slots. Likewise, if :V-LEAK is specified, then this
value is assigned to both the :V-LEAK-SOMA and :V-LEAK-DENDRITE slots. Note that the cell type parameters will not be propagated
to the segments and soma until SET-CIRCUIT-ELEMENTS-PARAMETERS is called. When UPDATE is non-NIL then propagate parameter values
to type cells. If no new VALUE follows the PARAM, then the current value of the slot corresponding to PARAM is returned."
  (let ((type (if (cell-type-p (element element)) (element element) (element-type (element-cell element))))
	(return-value nil)
	(param-found t))
    (when value-supplied-p
      (setq value (when (numberp value) (s-flt value))
	    *enable-segment-membrane-parameter-update* t
	    *enable-soma-membrane-parameter-update* t))
    (when type
      (when value-supplied-p (clear-all-z-cable-in type))	
      (setq return-value
	    (case param
	      (:soma-shunt (if value-supplied-p (element-slot type :soma-shunt value update) (element-slot type :soma-shunt)))
	      (:v-leak-soma (if value-supplied-p (element-slot type :v-leak value update) (element-slot type :v-leak)))
	      (:v-leak-dendrite (if value-supplied-p (element-slot type :v-leak-dendrite value update) (element-slot type :v-leak-dendrite)))
	      (:v-leak (if value-supplied-p
			 (progn (element-slot type :v-leak-soma value update) (element-slot type :v-leak-dendrite value update))
			 (element-slot type :v-leak-soma)))
	      (:rm-dendrite (if value-supplied-p (element-slot type :rm-dendrite value update) (element-slot type :rm-dendrite)))
	      (:rm-soma (if value-supplied-p (element-slot type :rm-soma value update) (element-slot type :rm-soma)))
	      (:rm (if value-supplied-p
		     (progn (element-slot type :rm-dendrite value update) (element-slot type :rm-soma value update))
		     (element-slot type :rm-dendrite)))
	      (:cm-soma (if value-supplied-p (element-slot type :cm-soma value update) (element-slot type :cm-soma)))
	      (:cm-dendrite (if value-supplied-p (element-slot type :cm-dendrite value update) (element-slot type :cm-dendrite)))
	      (:cm (if value-supplied-p
		     (progn (element-slot type :cm-dendrite value update) (element-slot type :cm-soma value update))
		     (element-slot type :cm-soma)))
	      (:ri (if value-supplied-p (element-slot type :ri value update) (element-slot type :ri)))
	      (t (setq param-found nil))))
      (when (and value-supplied-p ; (not (eq value return-value))
		 param-found)
	(if update
	  (set-circuit-elements-parameters)
	  (setq *recheck-circuit-elements-parameters* t)))
      (unless param-found (format t "~%ERROR: Cell types do not have parameter ~a!~%" param)))
    (when param-found return-value)))

;; old version
(defun set-cell-type-param (element param &optional (value nil value-supplied-p) (update t))
  (if value-supplied-p (cell-type-parameter element param value update) (cell-type-parameter element param)))

(defun print-cell-types () (PRINT-MODEL-PARAMETERS "cell-type"))

;; Note that the function CREATE-CELLTYPE refers to the older version of this function which does not reference the cell-type parameter library. 

(defun create-cell-type (&optional type-symbol actual-type-symbol update-parameters)
  "TYPE-SYMBOL is a symbol or a cell type; in the former case it must match the CAR of one of the lists contained in cell type
model parameter library. Returns the cell type structure, whether is was already made or not. If the type was already made, and
UPDATE-PARAMETERS is T, the type will be updated according to the current description loaded in parameter library. If TYPE-SYMBOL
does not correspond to an entry in the parameter library, then the cell type parameters will be taken from various global
variables, including *RM*, *RI*, *CM*, *CM-DENDRITE*, *SOMA-SHUNT*, *V-LEAK*, *V-LEAK-DENDRITE*, *E-NA*, *E-K*, *E-CA*, *E-CL*, in
addition to default specifications for reversal potentials (:FIXED) and concentrations (:FOLLOWS-GLOBAL). The TYPE-SYMBOL that is
actually used for the type is an uppercase symbol."
;;  (typecase type-symbol
;;    (cell-type (setq type-symbol (intern (cell-type-name type-symbol))))
;;    (string (setq type-symbol (intern (string-upcase type-symbol)))))
  (let* ((type-symbol (or type-symbol *DEFAULT-CELL-TYPE-NAME* 'generic))
	 (type (unless actual-type-symbol (if (cell-type-p type-symbol) type-symbol (CELL-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (cell-type-p type-symbol) (cell-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'cell-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters
	;; (sim-error (format nil "Don't know anything about cell type ~A!" type-symbol))
	(format t "CREATE-CELL-TYPE: Don't know anything about cell type ~A - setting type parameters from global variables!~%" type-symbol))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
	(setq type (if parent-type-symbol (create-CELL-TYPE parent-type-symbol type-symbol update-parameters) (make-CELL-TYPE :name type-symbol))))
      (when (and update-parameters parent-type-symbol)
	(update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf
       (cell-type-notes type) (or (get-a-value 'notes original-parameters) "")
       (cell-type-rm-soma type) (s-flt (or (get-1st-non-nil-value '(rm-soma rm) original-parameters) *rm-soma*))
       (cell-type-rm-dendrite type) (s-flt (get-1st-non-nil-value '(rm-dendrite rm) original-parameters *rm*))
       (cell-type-soma-shunt type) (s-flt (get-1st-non-nil-value 'soma-shunt original-parameters *soma-shunt*))
       (cell-type-ri type) (s-flt (or (get-a-value 'ri original-parameters) *ri*))
       (cell-type-cm-soma type) (s-flt (get-1st-non-nil-value '(cm-soma cm) original-parameters *cm*))
       (cell-type-cm-dendrite type) (s-flt (get-1st-non-nil-value '(cm-dendrite cm) original-parameters *cm-dendrite*))
       (cell-type-v-leak-soma type) (s-flt (get-1st-non-nil-value '(v-leak-soma v-leak) original-parameters *v-leak*))
       (cell-type-v-leak-dendrite type) (s-flt (get-1st-non-nil-value '(v-leak-dendrite v-leak) original-parameters *v-leak-dendrite*))
       (cell-type-e-na type) (s-flt (or (get-a-value 'e-na original-parameters) *e-na*)) 
       (cell-type-e-k type) (s-flt (or (get-a-value 'e-k original-parameters) *e-k*))
       (cell-type-e-ca type) (s-flt (or (get-a-value 'e-ca original-parameters) *e-ca*))
       (cell-type-e-cl type) (s-flt (or (get-a-value 'e-cl original-parameters) *e-cl*))
       (cell-type-e-na-dependence type) (or (get-a-value 'e-na-dependence original-parameters) :fixed)
       (cell-type-na-conc-extra-dependence type) (or (get-a-value 'na-conc-extra-dependence original-parameters) :follows-global)
       (cell-type-e-k-dependence type) (or (get-a-value 'e-k-dependence original-parameters) :fixed)
       (cell-type-k-conc-extra-dependence type) (or (get-a-value 'k-conc-extra-dependence original-parameters) :follows-global)
       (cell-type-e-ca-dependence type) (or (get-a-value 'e-ca-dependence original-parameters) :fixed)
       (cell-type-ca-conc-extra-dependence type) (or (get-a-value 'ca-conc-extra-dependence original-parameters) :follows-global)
       (cell-type-e-cl-dependence type) (or (get-a-value 'e-cl-dependence original-parameters) :fixed)
       (cell-type-cl-conc-extra-dependence type) (or (get-a-value 'cl-conc-extra-dependence original-parameters) :follows-global))
      (setf (CELL-TYPE-HASH-TABLE (cell-type-name type)) type)
      (update-cell-type-ionic-parameters type))
    (setq *cell-type* type)))

#|
(defun document-cell-type (type)
  (let ((type-name (element-name type 'cell-type))
	(type (element type 'cell-type)))
    (when type
      (format t "(cell-type-def~%")
      (format t "  '(~a~%" type-name)
      (format t "     (rm-soma . ~S)~%" (cell-type-rm-soma type))
      (format t "     (rm-dendrite . ~S)~%" (cell-type-rm-dendrite type))
      (format t "     (soma-shunt . ~S)~%" (cell-type-soma-shunt type))
      (format t "     (ri . ~S)~%" (cell-type-ri type))

      (format t "     (cm-soma . ~S)~%" (cell-type-cm-soma type))
      (format t "     (cm-dendrite . ~S)~%" (cell-type-cm-dendrite type))

      (format t "     (v-leak . ~S)~%" (cell-type-v-leak-dendrite type))
      (format t "     (v-leak-soma . ~S)~%" (cell-type-v-leak-soma type))

      (format t "     (e-na . ~S)~%" (cell-type-e-na type))
      (format t "     (e-k . ~S)~%" (cell-type-e-k type))
      (format t "     (e-ca . ~S)~%" (cell-type-e-ca type))
      (format t "     (e-cl . ~S)~%" (cell-type-e-cl type))

      (format t "     (e-na-dependence . ~S)~%" (cell-type-e-na-dependence type))
      (format t "     (e-k-dependence . ~S)~%" (cell-type-e-k-dependence type))
      (format t "     (e-ca-dependence . ~S)~%" (cell-type-e-ca-dependence type))
      (format t "     (e-cl-dependence . ~S)~%" (cell-type-e-cl-dependence type))

      (format t "     (na-conc-extra-dependence . ~S)~%" (cell-type-na-conc-extra-dependence type))
      (format t "     (k-conc-extra-dependence . ~S)~%" (cell-type-k-conc-extra-dependence type))
      (format t "     (ca-conc-extra-dependence . ~S)~%" (cell-type-ca-conc-extra-dependence type))
      (format t "     (cl-conc-extra-dependence . ~S)~%" (cell-type-cl-conc-extra-dependence type))

      (format t "     (notes . ~s)~%" (or (cell-type-notes type) ""))
      (when (element-document-extras type) (format t "     ~s~%" (element-document-extras type)))
      
      (format t "                ))~%")
      (format t "~%~%~%"))))
|#

(defun document-cell-type (type)
  (let ((type (element type 'cell-type))
	(*print-pretty* t))
    (when type
      (print
       `(cell-type-def
	 ,(no-nils `(,(element-name type 'cell-type)
		     ,@(mapcar (lambda (sym) (cons sym (element-slot-function type sym)))
			       '(rm-dendrite rm-soma ri soma-shunt
				 v-leak-dendrite v-leak-soma
				 e-na e-k e-ca e-cl
				 e-na-dependence e-k-dependence e-ca-dependence e-cl-dependence
				 na-conc-extra-dependence k-conc-extra-dependence na-conc-extra-dependence cl-conc-extra-dependence
				 notes))
		     ,@(element-document-extras type)))))
      nil)))

(defun create-cell-type-w-params (&optional name &key
					    (rm *rm*) ; ohms-cm2
					    (ri *ri*) ; ohms-cm
					    rm-soma; ohms-cm2
					    (soma-shunt *soma-shunt*) ; ohms
					    cm-soma ; uF/cm2
					    cm-dendrite ; uF/cm2
					    (cm *cm-dendrite*) spcap ; uF/cm2
					    (v-leak *v-leak-dendrite*) ; mV
					    v-leak-dendrite ; mV
					    v-leak-soma ; mV
					    (e-na *e-na*) (e-k *e-k*) (e-ca *e-ca*) (e-cl *e-cl*) ; mV
					    (e-na-dependence :fixed) (na-conc-extra-dependence :follows-global)
					    (e-k-dependence :fixed) (k-conc-extra-dependence :follows-global)
					    (e-ca-dependence :fixed) (ca-conc-extra-dependence :follows-global)
					    (e-cl-dependence :fixed) (cl-conc-extra-dependence :follows-global)
					    (notes ""))
 "Creates and returns a new cell type with name NAME and parameters given by the key arguments, if not already
defined. Alternative to using the function CREATE-CELL-TYPE, which references the cell-type parameter library. For the
dendritic/somatic parameter assignments if the dendritic or somatic argument is not supplied, then the general parameter is
used. For example, :RM-SOMA will supersede :RM for the soma. Likewise, if V-LEAK is specified, then this value is assigned to both
the :V-LEAK-SOMA and :V-LEAK-DENDRITE slots. If CM is specified, this value is assigned to both the somatic and dendritic slots."
  (setq *cell-type*
	(or (element name 'cell-type)
	    (let ((name (typecase name
			  (symbol name)
			  (string name)
			  (schema nil)
			  (t (string (or name "Default Cell Type"))))))
	      (let ((cell-type (make-cell-type :name name :notes notes
					       :soma-shunt (s-flt soma-shunt)
					       :rm-soma (s-flt (or rm-soma rm))
					       :rm-dendrite (s-flt rm)
					       :ri (s-flt ri)
					       :cm-soma (s-flt (or cm-soma cm))
					       :cm-dendrite (s-flt (or cm-dendrite cm))
					       :v-leak-dendrite (s-flt (or v-leak-dendrite v-leak))
					       :v-leak-soma (s-flt (or v-leak-soma v-leak))
					       :e-na (s-flt e-na) :e-k (s-flt e-k) :e-ca (s-flt e-ca) :e-cl (s-flt e-cl)
					       :e-na-dependence e-na-dependence :na-conc-extra-dependence na-conc-extra-dependence
					       :e-k-dependence e-k-dependence   :k-conc-extra-dependence k-conc-extra-dependence
					       :e-ca-dependence e-ca-dependence :ca-conc-extra-dependence ca-conc-extra-dependence
					       :e-cl-dependence e-cl-dependence :cl-conc-extra-dependence cl-conc-extra-dependence)))
		(setf (CELL-TYPE-HASH-TABLE name) cell-type)
		(update-cell-type-ionic-parameters cell-type)
		cell-type)))))

;; Old version of create-cell-type, which does not use the cell-type parameter library.
(defun create-celltype (&optional name &key
				  (rm *rm*) rm-soma ; ohms-cm2
				  (ri *ri*) ; ohms-cm
				  (soma-shunt *soma-shunt*) ; ohms
				  cm-soma cm-dendrite (cm *cm-dendrite*) ; uF/cm2
				  (v-leak *v-leak-dendrite*) v-leak-dendrite v-leak-soma ; mV
				  (e-na *e-na*) (e-k *e-k*) (e-ca *e-ca*) (e-cl *e-cl*) ; mV
				  (e-na-dependence :fixed) (na-conc-extra-dependence :follows-global)
				  (e-k-dependence :fixed) (k-conc-extra-dependence :follows-global)
				  (e-ca-dependence :fixed) (ca-conc-extra-dependence :follows-global)
				  (e-cl-dependence :fixed) (cl-conc-extra-dependence :follows-global)
				  (notes ""))
  (create-cell-type-w-params name
			     :rm rm :rm-soma rm-soma :ri ri :soma-shunt soma-shunt 
			     :cm-soma cm-soma :cm-dendrite cm-dendrite :cm cm
			     :v-leak v-leak :v-leak-dendrite v-leak-dendrite :v-leak-soma v-leak-soma
			     :e-na e-na :e-k e-k :e-ca e-ca :e-cl e-cl
			     :e-na-dependence e-na-dependence :na-conc-extra-dependence na-conc-extra-dependence
			     :e-k-dependence e-k-dependence :k-conc-extra-dependence k-conc-extra-dependence
			     :e-ca-dependence e-ca-dependence :ca-conc-extra-dependence ca-conc-extra-dependence
			     :e-cl-dependence e-cl-dependence :cl-conc-extra-dependence cl-conc-extra-dependence
			     :notes notes))
			    
(defun print-cell-type-brief (&optional explicit-type) (print-cell-type explicit-type t))

(defun print-cell-type (&optional explicit-type brief)
  (loop for cell-type in (if explicit-type (list explicit-type) (CELL-TYPEs)) do
	(format t "Cell-type ~a:~%" (cell-type-name cell-type))
	(let ((rm-string
	       (if (= (cell-type-rm-dendrite cell-type) (cell-type-rm-soma cell-type))
		 (format nil "    Rm ~,2e ohm-cm2, " (cell-type-rm-dendrite cell-type))
		 (format nil "    Rm-soma ~,2e, Rm-dendrite ~,3e ohm-cm2, " (cell-type-rm-soma cell-type) (cell-type-rm-dendrite cell-type))))
	      (membrane-capacitance-string
	       (if (= (cell-type-cm-soma cell-type) (cell-type-cm-dendrite cell-type))
		 (format nil "Cm ~,2f uF/cm2" (cell-type-cm-soma cell-type))
		 (format nil "Cm soma ~,2f, Cm dendrite ~,2f uF/cm2" (cell-type-cm-soma cell-type) (cell-type-cm-dendrite cell-type))))
	      (leak-battery-string
	       (if (= (cell-type-v-leak-soma cell-type) (cell-type-v-leak-dendrite cell-type))
		 (format nil "V-leak ~,1f mV~%" (cell-type-v-leak-soma cell-type))
		 (format nil "V-soma-leak ~,1f mV, V-dendrite-leak ~,1f mV~%" (cell-type-v-leak-soma cell-type) (cell-type-v-leak-dendrite cell-type))))
	      (include-shunt (loop for cell in (cell-type-cells cell-type) thereis (soma-include-shunt (cell-soma cell)))))
	  (format t "~a" rm-string)
	  (if include-shunt
	    (format t "Soma shunt ~d ohms, Ri ~,1f ohm-cm, ~a, "
		    (cell-type-soma-shunt cell-type) (cell-type-ri cell-type) membrane-capacitance-string)
	    (format t "Ri ~,1f ohm-cm, ~a, " (cell-type-ri cell-type) membrane-capacitance-string))
	  (format t "~A" leak-battery-string)
	  (unless brief
	    (format t "    E-Na ~,1f mV (~A), E-K ~,1f mV (~A), E-Ca ~,1f mV (~A), E-Cl ~,1f mV (~A)~%"
		    (cell-type-e-na cell-type)
		    (cell-type-e-na-dependence cell-type)
		    (cell-type-e-k cell-type)
		    (cell-type-e-k-dependence cell-type)
		    (cell-type-e-ca cell-type)
		    (cell-type-e-ca-dependence cell-type)
		    (cell-type-e-cl cell-type)
		    (cell-type-e-cl-dependence cell-type)))
	  (when (> (length (cell-type-notes cell-type)) 0)
	    (format t "   ~A~%" (cell-type-notes cell-type)))
	  (unless brief
	    (PRINT-CELL-TYPE-CONCENTRATIONS cell-type)
	    (print-num-elements-sourcefile cell-type t t))
	  (format t "~&")
	  )))

(defun cell-r-in (&optional (cell *cell*))
  "Input resistance of CELL, in Mohms [references CELL-Z-DISCRETE-IN-CELL]."
  (unless *circuit-processed* (process-circuit-structure))
  (cell-z-discrete-in-cell cell))

(defun soma-shunt-string (cell)
  (if (soma-include-shunt (cell-soma cell))
      (format nil " (includes shunt of ~,2f Mohms)" (/ 1.0 (soma-g-shunt (cell-soma cell))))
      ""))

;; handle this in print-cell-type
(defun print-cell (&optional (cell *cell*))
  (unless *circuit-processed* (process-circuit-structure))
  (format t "~a (soma @ [~d ~d ~d])"
	  (MASSAGE-ELEMENT-PLOT-LABEL cell nil t)
	  (first (cell-origin cell))
	  (second (cell-origin cell))
	  (third (cell-origin cell)))
  (let ((cell-definition (element-parameter cell 'cell-definition)))
    (when (if (stringp cell-definition)
	      (> (length cell-definition) 0)
	      cell-definition)
      (format t "  -  Created from ~A" cell-definition)))
  (format t "~%")
  (if (cell-tree-p cell)
      (let ((terms (or (element-parameter cell 'number-cell-distal-segments)
		       (element-parameter cell 'number-cell-distal-segments (count-cell-distal-segments cell))))
	    (segs (length (cell-segments cell)))
	    (trunks (length (trunk-segments cell)))
	    (bps (or (element-parameter cell 'number-cell-branch-points)
		     (element-parameter cell 'number-cell-branch-points (count-branch-points cell)))))
	(format t   "    ~A~A~a~A Membrane Area ~,2eum^2~%"
		(if (> segs 0) (format nil "~A Segment~:p," segs) "")
		(if (> bps 0) (format nil " ~a Branch point~:p," bps) "")
		(if (> trunks 0) (format nil " ~a Trunk~:p," trunks) "") 
		(if (> terms 0) (format nil " ~a Terminal~:p," terms) "")
		(or (element-parameter cell 'total-area) (update-cell-area cell))))
      (format t   "    Membrane Area ~,2eum^2~%"
	      (or (element-parameter cell 'total-area) (update-cell-area cell))))
  (unless (cell-max-g-in cell) (update-linear-z-in cell))
  (let ((g-in (cell-max-g-in cell)))
    (format
     t
     "    Passive somatic R-in (actual|cable model) = ~,2e|~,2e Mohms ~%"
     (cell-z-discrete-in-cell cell) (cell-z-cable-in-cell cell))
					;    (when g-in
					;      (format
					;       t
					;       "       Max G-in / Min R-in = ~,2f uS / ~,2f Mohms ~%"
					;       g-in (/ 1.0 g-in)))
    (if (cell-tree-p cell)
	(unless (zero-soma-p cell)
	  (format t "    R-Soma (passive) = ~,2e Mohms~a~%"
		  (/ 1.0 (+ (soma-g-leak (cell-soma cell))
			    (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0)))
		  (soma-shunt-string cell)))
	(format t "    ~a~%" (soma-shunt-string cell)))
    (when (soma-segments cell)
      (format t "    ~a R-Soma segment~:p, with R-leak = ~,2e Mohms"
	      (length (soma-segments cell))
	      (/ 1.0 (soma-segments-g-leak cell))))
    (format t "~&")
    (when (cell-tree-p cell)
      (format
       t
       "    Tree R-in (passive) (actual|cable model) = ~,2e|~,2e Mohms~%"
       (cell-z-tree-discrete-in-cell cell)
       (cell-z-tree-cable-in-cell cell))
      (format
       t  
       (case (element-parameter (cell-type cell) 'g-axial-computation)
	 (:average-r "    Coupling R's from axial R average [average-r]~%")
	 (:average-g "    Coupling R's from axial G average [average-g]~%")
	 (t				; (:single-leg t)
	  "    Coupling R's from individual compartments [single-leg]~%")))))
					; (format t "~%")
  )

;;This is to make sure that everyone knows about everyone else - this step should eventually be unecessary when all references are filled at the
;;appropriate create object call.
(defun collect-circuit-objects ()
  (collect-cell-types)
;;  (collect-cells-nodes)
  (collect-cells-segments)
;;  (collect-cells-somas)
  )

(defun collect-cell-types ()
  (maphash 'clear-cell-type-cells (CELL-TYPE-HASH-TABLE))
  (maphash 'collect-cell-type (CELL-HASH-TABLE)))

(defun clear-cell-type-cells (name cell-type)
  (declare (ignore name))
  (setf (cell-type-cells cell-type) nil))

(defun collect-cell-type (name cell)
  (declare (ignore name))
  (let ((cell-type (cell-type cell)))
    (when cell-type
      (or (member cell (cell-type-cells cell-type))
	  (push cell (cell-type-cells cell-type))))))

(defun collect-cells-segments ()
  (maphash 'clear-cell-segments (CELL-HASH-TABLE))
  (maphash 'collect-segments-cell (SEGMENT-HASH-TABLE)))

(defun clear-cell-segments (name cell)
  (declare (ignore name))
  (setf (cell-segments cell) nil))

(defun collect-segments-cell (name seg)
  (declare (ignore name))
  (unless (get-a-value 'electrode (segment-parameters seg))
    (push seg (cell-segments (segment-cell seg)))))

(defun collect-cells-somas () (maphash 'collect-somas-cell (SOMA-HASH-TABLE)))

(defun collect-somas-cell (name soma)
  (declare (ignore name))
  (setf (cell-soma (soma-cell soma)) soma))

(defvar *clear-cell-name-maps* t)

(defun get-cell-simple-name ()
  (loop for candidate from (max 1 *cell-simple-name-counter*)
	until (not (cell-hash-table candidate))
	finally (return (setf *cell-simple-name-counter* candidate))))

(defun rename-cells-simple (&optional (cells (cells)))
  "Rename CELLS [default all cells in circuit] with simple integer names."
  (loop for cell in (coerce-to-list (element cells 'cell)) do (set-element-name cell (get-cell-simple-name))))

(defun rename-cell-elements-simple (&optional (cell-elements (cell-elements)))
  "Rename all the somas and segments associated with CELL-ELEMENTS [default all cell elements in circuit] with simple integer names."
  (loop for elt in (elements cell-elements)
	do (if (cell-element-p elt)
	     (rename-cell-element-simple elt)
	     (loop for elt in (cell-elements (element elt 'cell)) do (rename-cell-element-simple elt)))))

(defun rename-cell-element-simple (elt)
  (let ((name (get-cell-element-simple-name)))
    (set-element-name elt name (type-of elt))
    (set-element-name (element-physical-node elt) name 'node)))

(defun find-or-create-cell (cell &optional cell-type)
  (or (unless (consp (element cell 'cell)) (element cell 'cell))
      (atomize-list (ELEMENT-W-STRING-OR-SYMBOL cell 'cell))
      (create-cell cell :cell-type cell-type)))

(defun create-cell (cell-name &key cell-type tree-list
			      (soma-diameter nil) (segment-diameter 0.5) (origin '(0.0 0.0 0.0)) ; microns
			      (name-suffix *cell-name-suffix*)
			      (enable-automatic-cell-names *enable-automatic-cell-names*) (automatic-name-fixing *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*))
  "Creates a new cell of CELL-TYPE, with soma of SOMA-DIAMETER if non-nil [microns, default NIL], at XYZ coordinates ORIGIN [default '(0.0 0.0 0.0) in microns].
Cell segments are created if TREE-LIST is non-nil, used in a call to CREATE-TREE, with a :DEFAULT-DIAMETER argument given by SEGMENT-DIAMETER 
[microns, default 0.5]. If the global variable *NEXT-CELL-NAME* is non-NIL, then this will be used instead of CELL-NAME. Always sets *NEXT-CELL-NAME*
to NIL on completion. If NAME-SUFFIX is non-NIL [default given by global variable *CELL-NAME-SUFFIX*], it is automatically added as a suffix to the name of
a cell, even if this is supplied by *NEXT-CELL-NAME*. CELL-NAME can be a number."
  (when *clear-cell-name-maps* (element-parameter *cell* 'name-map nil)) ; A convenient place to put this. Also in PROCESS-CIRCUIT-STRUCTURE.
  (let* ((name
	  (if *use-simple-names*
	    (GET-Cell-SIMPLE-NAME)
	    (concatenate 'string (string (or *next-cell-name*
					     (typecase cell-name
					       (number (format nil "~A" cell-name))
					       (t cell-name))))
			 (when NAME-SUFFIX (string NAME-SUFFIX)))))
	 (valid-cell-name (if (and (not *use-simple-names*) enable-automatic-cell-names) (check-cell-name name :automatic-name-fixing automatic-name-fixing) name)))
    (when *next-cell-name* (format t "CREATE-CELL: Clearing the non-nil value of *NEXT-CELL-NAME*, which was ~A~%" *next-cell-name*))
    (setq *cell* 
	  (or (CELL-HASH-TABLE valid-cell-name)
	      (let* ((cell-def (case *circuit-source*
				 (:forms "Lisp forms")
				 (t (if *input-is-function* *circuit-function* *circuit-file*))))
		     (cell-type (if (cell-type-p cell-type)
				    cell-type
				    (create-cell-type (or cell-type
						      (unless (or *use-simple-names* (numberp cell-name) (numberp name))
							(typecase name
							  (string (read-from-string name))
							  (t name)))))))
		     (cell (make-cell :name valid-cell-name :origin (float-list (or origin '(0.0 0.0 0.0)))
				      :type cell-type :parameters (when cell-def (list (cons 'cell-definition cell-def))))))
		(setq *circuit-processed* nil)
		(push cell (cell-type-cells (cell-type cell)))
		(setf (CELL-HASH-TABLE valid-cell-name) cell)
		(unless (equal valid-cell-name name) (setq *add-cell-name-to-segs* t))
		(when soma-diameter
		  (let ((soma (create-soma :cell valid-cell-name :diameter soma-diameter
					   :enable-automatic-cell-names enable-automatic-cell-names
					   :automatic-name-fixing automatic-name-fixing)))
		    (when tree-list (create-tree soma tree-list :default-diameter segment-diameter))))
		cell)))))

(defun edit-cell-type (&optional (cell-type *cell-type*))
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12 cell-type-changed)
    (loop while (progn
		  (setq dummy1 (cell-type-ri cell-type)
			dummy2 (cell-type-rm-dendrite cell-type)
			dummy3 (cell-type-cm-soma cell-type)
			dummy4 (cell-type-rm-soma cell-type)
			dummy5 (cell-type-soma-shunt cell-type)
			dummy6 (cell-type-v-leak-soma cell-type)
			dummy7 (cell-type-v-leak-dendrite cell-type)
			dummy8 (cell-type-cm-dendrite cell-type)
			dummy9 (cell-type-global-membrane-conductance-factor cell-type)
			dummy11 (or (element-parameter cell-type 'g-axial-computation) :single-leg))
		  (choose-variable-values
		   `((dummy1 "Cytoplasmic resistivity [ohm-cm]" :float)
		     (dummy2 "Dendrite membrane resistivity [ohm-cm-sq]" :float) 
		     (dummy7 "Dendrite leak battery [mV]" :float) 
		     (dummy8 "Dendrite specific capacitance [uF/cm-sq]" :float)
		     (dummy4 "Soma membrane resistivity [ohm-cm-sq]" :float) 
		     (dummy5 "Soma membrane shunt [ohms]" :float) 
		     (dummy6 "Soma leak battery [mV]" :float) 
		     (dummy3 "Soma specific capacitance [uF/cm-sq]" :float)
		     (dummy9 "Coefficient for all channel and synapse conductances" :float)
		     ;; (dummy11 "Method for computing compartment coupling (g-axial):" :choose (:single-leg :average-r :average-g))
		     (dummy10 "Edit ionic concentration/e-rev parameters" :boolean))
		   :label (format niL "Linear Parameters For Cell Type ~a" (cell-type-name cell-type)))
		  (or (<= dummy1 0) (<= dummy2 0) (<= dummy3 0) (<= dummy4 0) (<= dummy5 0) (<= dummy8 0) (<= dummy9 0))))
    (when dummy10 (edit-cell-type-ionic-parameters cell-type))
    (setq cell-type-changed
	  (or (not (eq dummy11 (or (element-parameter cell-type 'g-axial-computation) :single-leg)))	      
	      (not (and (= dummy1 (cell-type-ri cell-type))
			(= dummy2 (cell-type-rm-dendrite cell-type))
			(= dummy3 (cell-type-cm-soma cell-type))
			(= dummy8 (cell-type-cm-dendrite cell-type))
			(= dummy4 (cell-type-rm-soma cell-type))
			(= dummy5 (cell-type-soma-shunt cell-type))
			(= dummy6 (cell-type-v-leak-soma cell-type))
			(= dummy7 (cell-type-v-leak-dendrite cell-type))
			(= dummy9 (cell-type-global-membrane-conductance-factor cell-type))))))
    (element-parameter cell-type 'g-axial-computation dummy11)
    (clear-all-z-cable-in cell-type)	
    (setf (cell-type-ri cell-type) dummy1
	  (cell-type-rm-dendrite cell-type) dummy2 
	  (cell-type-rm-soma cell-type) dummy4 
	  (cell-type-soma-shunt cell-type) dummy5 
	  (cell-type-cm-soma cell-type) dummy3
	  (cell-type-cm-dendrite cell-type) dummy8
	  (cell-type-v-leak-soma cell-type) dummy6
	  (cell-type-v-leak-dendrite cell-type) dummy7
	  (cell-type-global-membrane-conductance-factor cell-type) dummy9)
    (when dummy12
      (let (dummy1 dummy2)
	(choose-variable-values
	 `((dummy1 ,(format nil "Remove all ~a cell~:p of this type" (length (cell-type-cells cell-type))) :boolean)
	   (dummy2 "Remove specific cells of this type" :boolean))
	 :label (format nil "Removing Cells of Type ~A" (cell-type-name cell-type)))
	(if dummy1
	    (erase-elements (cell-type-cells cell-type))
	    (when dummy2
	      (loop for cell in
		    (choose-list-values
		     (loop for cell in (cell-type-cells cell-type) collect (element-name cell))
		     nil :label (format nil "Choose Cells of Type ~A to Remove" (cell-type-name cell-type)))
		    do (erase-element cell))))))
    (when cell-type-changed (set-and-update-cell-type-linear-membrane cell-type))))

(defun set-and-update-cell-type-linear-membrane (&optional cell-type)
  (loop for cell-type in (or (coerce-to-list cell-type) (cell-types)) do
	(set-segments-membrane-parameters t cell-type)
	(set-somas-membrane-parameters t cell-type)
	(update-linear-z-in-cell-type cell-type)))

(defun menu-for-cell-types (&optional type)
  (loop for cell-type in
	(if type
	    (list (element type 'cell-type))
	    (element
	     (select-hash-values-menu (CELL-TYPE-HASH-TABLE) "Select Cell Types To Modify" :punt-if-only-one-entry t :inclusion-key 'cell-type-cells)
	     'cell-type))
	do (edit-cell-type cell-type)))

(defun edit-cell-type-ionic-parameters (&optional type)
  (setq type (or type (car (cell-types))))
  (let ((dummy1 (cell-type-na-conc-intra type))	(dummy2 (cell-type-na-conc-extra type))
	(dummy3 (cell-type-na-conc-extra-dependence type)) (dummy21 (cell-type-e-na-dependence type))
	(dummy5 (cell-type-e-na type))

	(dummy6 (cell-type-k-conc-intra type)) (dummy7 (cell-type-k-conc-extra type))
	(dummy8 (cell-type-k-conc-extra-dependence type)) (dummy22 (cell-type-e-k-dependence type))
	(dummy10 (cell-type-e-k type))

	(dummy11 (cell-type-ca-conc-intra type)) (dummy12 (cell-type-ca-conc-extra type))
	(dummy13 (cell-type-ca-conc-extra-dependence type)) (dummy23 (cell-type-e-ca-dependence type))
	(dummy15 (cell-type-e-ca type))

	(dummy16 (cell-type-cl-conc-intra type)) (dummy17 (cell-type-cl-conc-extra type))
	(dummy18 (cell-type-cl-conc-extra-dependence type)) (dummy24 (cell-type-e-cl-dependence type))
	(dummy20 (cell-type-e-cl type))
	dummy25
	dummy26)
    (choose-variable-values
     '((dummy25 "Modify parameters for the following ions:" :x-choose (:na :k :ca :cl))
       (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
     :label (format nil "Modifying Ionic Parameters for Cell Type ~A" (cell-type-name type)))
    (cond-every
     ((member :na dummy25)
      (choose-variable-values
       `((dummy1 "[Na+]in [mM]" :float)
	 (dummy2 "[Na+]out [mM]" :float)
	 (dummy3 ,(format nil "[Na+]out dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global) :vertical)
	 (dummy21 ,(format nil "E-Na dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy5 "Fixed value for Na+ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :k dummy25)
      (choose-variable-values
       `((dummy6 "[K+]in [mM]" :float)
	 (dummy7 "[K+]out [mM]" :float)
	 (dummy8 ,(format nil "[K+]out dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global) :vertical)
	 (dummy22 ,(format nil "E-K dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy10 "Fixed value for K+ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :ca dummy25)
      (choose-variable-values
       `((dummy11 "[Ca++]in [mM]" :float)
	 (dummy12 "[Ca++]out [mM]" :float)
	 (dummy13 ,(format nil "[Ca++]out dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global) :vertical)
	 (dummy23 ,(format nil "E-Ca dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy15 "Fixed value for Ca++ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :cl dummy25)
      (choose-variable-values
       `((dummy16 "[Cl-]in [mM]" :float)
	 (dummy17 "[Cl-]out [mM]" :float)
	 (dummy18 ,(format nil "[Cl-]out dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global) :vertical)
	 (dummy24 ,(format nil "E-Cl dependence for cell type ~A:" (cell-type-name type)) :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy20 "Fixed value for Cl- Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type)))))
    (when dummy26 (MENU-FOR-CONCENTRATIONS))
    (setf
     (cell-type-e-na-dependence type) dummy21
     (cell-type-e-k-dependence type) dummy22
     (cell-type-e-ca-dependence type) dummy23
     (cell-type-e-cl-dependence type) dummy24		
     
     (cell-type-na-conc-intra type) dummy1
     (cell-type-na-conc-extra type) dummy2
     (cell-type-na-conc-extra-dependence type) dummy3
     (cell-type-e-na type) dummy5

     (cell-type-k-conc-intra type) dummy6
     (cell-type-k-conc-extra type) dummy7
     (cell-type-k-conc-extra-dependence type) dummy8
     (cell-type-e-k type) dummy10

     (cell-type-ca-conc-intra type) dummy11
     (cell-type-ca-conc-extra type) dummy12
     (cell-type-ca-conc-extra-dependence type) dummy13
     (cell-type-e-ca type) dummy15

     (cell-type-cl-conc-intra type) dummy16
     (cell-type-cl-conc-extra type) dummy17
     (cell-type-cl-conc-extra-dependence type) dummy18
     (cell-type-e-cl type) dummy20)    
    (update-cell-type-ionic-parameters type)))

(defun UPDATE-CELL-AREA (cell)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (element-parameter cell 'number-cell-distal-segments (count-cell-distal-segments cell))
  (element-parameter cell 'number-cell-branch-points (count-branch-points cell))
  (element-parameter cell 'total-area (+ (if (soma-segments cell) 0.0 (element-area (cell-soma cell)))
					 (element-area (cell-segments cell)))))

(defun menu-to-move-cells (&optional cell histology-win)
  (loop for cell in (if cell
		      (list (element cell 'cell))
		      (choose-list-values-from-keys
		       (loop for cell being the hash-value of (CELL-HASH-TABLE)
			     collect (list (format nil "~a @ ~a" (cell-name cell) (element-absolute-location cell)) cell))
		       nil :text "Cell names are followed by the XYZ soma coordinates." :label "Select cells to move"))
	do (let ((dummy1 (first (cell-origin cell)))
		 (dummy2 (second (cell-origin cell)))
		 (dummy3 (third (cell-origin cell)))
		 dummy4)
	     (choose-variable-values
	      `((dummy1 "Soma X coordinate" :Float)
		(dummy2 "Soma Y coordinate" :Float)
		(dummy3 "Soma Z coordinate" :Float)
		,(when histology-win (list 'dummy4 (format nil "Redraw histology in window ~a" (g-value histology-win :title)) :boolean)))
	      :Label (format nil "Move coordinates of cell ~A" (element-name cell)))
	     (move-cell cell (list dummy1 dummy2 dummy3))
	     (when dummy4
	       (let ((*automatic-run* t))
		 (drawing-menu histology-win t nil t))))))

(defun warp-cell (cell &key (x-factor 1.0) (y-factor 1.0) (z-factor 1.0))
  (loop for node being the hash-value of (NODE-HASH-TABLE)
	when (eq cell (node-cell node))
	do (setf (node-relative-location node) (mapcar '* (node-relative-location node) (list x-factor y-factor z-factor))))
  (process-circuit-structure t))

(defun shift-element-position (element shift)
  (let ((node (element-physical-node element)))
    (when node
      (setf (node-relative-location node)  (mapcar '+ (node-relative-location node) shift))
      (setf (node-absolute-location node)  (mapcar '+ (node-absolute-location node) shift))
      (when (and (segment-p (element element)) (segment-dummy-proximal-node-location (element element)))
	(setf (segment-dummy-proximal-node-location (element element)) (mapcar '+ (segment-dummy-proximal-node-location (element element)) shift))))))

(defun zero-soma-position (&optional cell)
  (let ((cell (or cell *cell*)))
    (when cell
      (let ((shift (mapcar '- (node-relative-location (soma-node (cell-soma cell))))))
	(loop for seg in (cell-segments cell) do (shift-element-position seg shift))
	(shift-element-position (cell-soma cell) shift)
	(setf (cell-origin cell) (mapcar '+ (cell-origin cell) shift)))
      (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE)))))

(defun move-cell (cell new-origin)
  "Moves the absolute location of CELL to the XYZ coordinates [microns] given in the numeric list NEW-ORIGIN."
  (let ((cell (element-cell cell)))
    (setf (cell-origin cell) (s-flt-list new-origin))
    (locate-cell-nodes cell)
    (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))))

(defun shift-cell (cell &key (x-shift 0.0) (y-shift 0.0) (z-shift 0.0))
  "Moves the relative location of CELL according to X-SHIFT, Y-SHIFT and Z-SHIFT [microns, default 0]." 
  (let ((cell (element-cell cell)))
    (setf (cell-origin cell) (mapcar '+ (cell-origin cell) (s-flt-list (list x-shift y-shift z-shift))))
    (locate-cell-nodes nil cell)
    (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))))

(defun cell-element-p (element)
  "True if ELEMENT is a soma or segment."
  (or (soma-p element)
      (segment-p element)
      (typecase (element element)
	((or soma segment) t))))



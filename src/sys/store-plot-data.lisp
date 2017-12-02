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


;;; SYS Source file: store-plot-data.lisp

(in-package "SURF-HIPPO")


;; For setting up output flags and saving out data during simulation.

(defun save-data (&optional always)
  ;; During the simulation, saves the data and time onto the appropriate lists, every *SAVE-DATA-STEP* time steps.
  (when *save-simulation-data*
    (save-time)
    (when (or always (= 0 (mod (the fn *total-num-time-points*) (the (unsigned-byte 29) *save-data-step*))))
      (save-plot-time)
      (save-models-output)
      (save-total-conductances)
      (user-save-data))
    (setq *save-data-called-p* t)))

(defun setup-models-output-data-enabled ()
  (loop for model in (models)
	when (model-save-output-data-routine model)
	do (setf (model-output-data-enabled model)
		 (loop for sym in (model-output-data-structure-variables model)
		       when (symbol-value sym)
		       do (return t)))))

#|(defun save-models-output ()
  (loop for model in (models)
	when (model-output-data-enabled model)
	do (funcall (model-save-output-data-routine model))))
|#


#| For example, 
 
(defvar *tuning* (make-array '(16 2)))

;; initialize number of cells per orientation
(loop for column from 0 to 15 do (setf (aref *tuning* column 1) 0))
(loop for soma in (somas)
      do (setf (aref *tuning* (soma-column soma) 1)
	       (1+ (aref *tuning* (soma-column soma) 1) )))

(defun record-column-output ()
  (loop for soma in (somas)
	do (setf (aref *tuning* (soma-column soma) 0)
		 (+ (aref *tuning* (soma-column soma) 0)
		    (* *last-time-step* (soma-voltage soma))))))

(setq *user-save-data-functions* (list 'record-column-output))

;; normalize output
(loop for column from 0 to 15 do
      (setf (aref *tuning* column 0)
	    (/ (aref *tuning* column 0) (* *user-stop-time* (aref *tuning* column 1)))))

(progn (gotimed) (save-array-to-file *tuning*))
|#

(defun user-save-data () (mapcar 'funcall *user-save-data-functions*))

;; UPDATE-ELEMENT-TYPE-DATA-TOTAL-RESPONSE and UPDATE-ELEMENT-DATA-MAX-MIN-VALUE are examples of functions added to *USER-SAVE-DATA-FUNCTIONS*.

(defun update-element-type-data-total-response (type &optional data-type (total-response-key 'total-response))
  ;; Build a list stored in the :PARAMETERS of TYPE (can be a list of types), under TOTAL-RESPONSE-KEY, of the sum of the data of DATA-TYPE for all the
  ;; children of TYPE. Referring to *SAVE-DATA-CALLED-P* allows this list to be cleared at the beginning of a simulation.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (mapcar #'(lambda (type) 
	      (let ((sum 0.0d0))
		(declare (double-float sum))
		(element-type-do (elt type) (incf sum (d-flt (element-current-value elt data-type))))
		(unless *SAVE-DATA-CALLED-P* (element-parameter type total-response-key nil)) ; Clear earlier simulation total-response
		(push-element-parameter type total-response-key sum)))
	  (coerce-to-list (element-type type)))
  nil)
  
(defun update-element-data-max-min-value (elt &optional data-type (max-response-key 'max-response) (min-response-key 'min-response))
  ;; For the current value of DATA-TYPE for ELT, update as neccessary the current maximum and minimum values stored in the ELT's
  ;; :PARAMETERS under the keys MAX-RESPONSE-KEY and MIN-RESPONSE-KEY, respectively.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((params (element-parameters-fast elt))
	 (current-min-assoc-result (when *SAVE-DATA-CALLED-P* (assoc-complete-guts min-response-key params)))
	 (current-max-assoc-result (when *SAVE-DATA-CALLED-P* (assoc-complete-guts max-response-key params))))
    (setf (*temporary-double-float-register*) (element-data-value-fast elt data-type))
    (cond-every ((or (not current-max-assoc-result) (> (*temporary-double-float-register*) (the df (cdr current-max-assoc-result))))
		 (set-element-parameter-fast elt max-response-key (*temporary-double-float-register*) params))
		((or (not current-min-assoc-result) (< (*temporary-double-float-register*) (the df (cdr current-min-assoc-result))))
		 (set-element-parameter-fast elt min-response-key (*temporary-double-float-register*) params)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-time ()
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (push *real-time* *sim-reverse-time-list*)
  (push *last-time-step* *sim-reverse-time-step-list*)
  nil)

(defun save-plot-time ()
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (push *real-time* *sim-reverse-plot-time-list*)
  nil)

(defun recorded-node-voltage (node voltage)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (the sf
    (if (and *consider-isource-drop* *isource*)	; Shunte le test s'il n'y a aucune source de cree. NG 31/10/97
	(let ((isrcs (get-element-parameter-fast :isources (node-parameters node))))
	  (if isrcs
	      (let* ((single-isource-on-this-node (= (length isrcs) 1))
		     (isrc (element (car isrcs) 'isource))
		     (isrc-params (isource-parameters isrc))
		     (isrc-current (isource-current isrc))
		     (isource-drop (if (and single-isource-on-this-node (get-element-parameter-fast :enable-isource-drop isrc-params))
				       (* (isource-resistance isrc) isrc-current)
				       0.0))
		     (bridge-balance (get-element-parameter-fast :bridge-balance isrc-params))
		     (bridge-drop (if (and single-isource-on-this-node
					   (get-element-parameter-fast :enable-bridge-balance isrc-params)
					   (numberp bridge-balance))						    
				      (* (the sf bridge-balance) isrc-current)
				      0.0)))
		(+ voltage (the sf isource-drop) (- (the sf bridge-drop))))
	      voltage))
	voltage)))

(defun recorded-element-voltage (element)
  (let ((element element))
    (typecase element
      (vsource (vsource-voltage element))
      (axon (axon-voltage element))
      ((or soma segment) (let ((node (typecase element
				       (soma (soma-node element))
				       (segment (segment-node-2 element)))))
			   (recorded-node-voltage node (s-flt (node-voltage-n+1 node))))))))

#|
(defun save-node-data ()
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (loop for element in *all-save-voltage-nodes* do
	(let* ((node (element-physical-node element))
	       (voltage (coerce (node-voltage-n+1 node) 'single-float))
	       (data (the sf (recorded-node-voltage node voltage))))
	  (typecase element
		    (soma (push-soma-voltage-data element data))
		    (segment (push-segment-voltage-data element data)))))
  (loop for element in *all-save-dvdt-nodes* do
	(let ((data (element-dvdt element)))
	  (typecase element
		    (soma (push-soma-voltage-derivative-data element data))
		    (segment (push-segment-voltage-derivative-data element data))))))
|#

(defun save-node-data ()
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for element in *all-save-voltage-nodes* do
	(let ((data (recorded-element-voltage element)))
          (typecase element
            (soma (push-soma-voltage-data element data))
            (segment (push-segment-voltage-data element data)))))
  ;; NG, 31/10/97 : je rajoute le test WHEN pour aller plus vite les 3/4 du temps.
  (when *all-save-dvdt-nodes*		; ?? does this really help ???
    (loop for element in *all-save-dvdt-nodes* do
	  (let ((data (element-dvdt element)))
	    (typecase element
	      (soma (push-soma-voltage-derivative-data element data))
	      (segment (push-segment-voltage-derivative-data element data))))))

  (loop for element in *all-save-leak-current-nodes* do
	(let ((data (element-leak-current element)))
	  (typecase element
	    (soma (push-soma-leak-current-data element data))
	    (segment (push-segment-leak-current-data element data)))))

  (loop for element in *all-save-capacitance-current-nodes* do
	(let ((data (element-capacitance-current element)))
	  (typecase element
	    (soma (push-soma-capacitance-current-data element data))
	    (segment (push-segment-capacitance-current-data element data))))))

(defun save-models-output ()
  (loop for model in (models) when t	; (model-output-data-enabled model)
	do (loop for data-type in (model-output-data-types model) do
		 (let ((data-type (if (consp data-type) (car data-type) data-type)))
		   (unless (eq data-type 'event)
		     (loop for element in (model-save-data-instances model data-type)
			   do
			   (if (and (particle-p element) (eq data-type 'markov-state))
			       (loop for state-label in (element-parameter (particle-type element) 'markov-state-labels)
				     for state-index fixnum from 0 to (nb-states-1 element)
				     do (push-element-parameter element state-label (s-flt (particle-aref-state-n+1 (prt-state element state-index)))))
			       (progn
				 ;; (format t "For ~A cv-function is ~A~%" model (model-output-current-value-function model data-type))
				 (push-element-parameter element (model-output-data-key model data-type)
							 (s-flt (if (model-output-current-value-function model data-type)
								    (if (macro-function (model-output-current-value-function model data-type))
									(eval (macroexpand (list (model-output-current-value-function model data-type) element)))
									(funcall (model-output-current-value-function model data-type) element))
								    (saved-element-data element data-type)))))))))))
  nil)

#|
(defun model-output-data-types (model) 
   (model-output-wrapper model (mapcar #'(lambda (DATA-TYPE-AND-ACCESS-info) (car DATA-TYPE-AND-ACCESS-info)) (model-DATA-TYPES-AND-ACCESS-INFO model)))
       )

(defun save-models-output ()
  (loop for model in (models) when t	; (model-output-data-enabled model)
	do 
	(loop for data-type in (model-output-data-types model) collect  data-type)))

(loop for model in (models) when t	; (model-output-data-enabled model)
      do (loop for data-type in (model-output-data-types model) do
	       (let ((data-type (if (consp data-type) (car data-type) data-type)))
		 (print (model-save-data-instances model data-type)
			data-type))))
|#

(defun push-model-output-data (element model data &optional data-type)
  (push-element-parameter element (model-output-data-key model data-type) (s-flt data)))

(defun element-data-value-fast (elt data-type)
  ;; Optimized for some default element types and data values. ELT must be an element structure.
  (d-flt (typecase elt
	   (particle
	    (case data-type
	      (t (particle-state-n+1-double elt))))
	   (conc-particle
	    (case data-type
	      (t (conc-particle-state-n+1-double elt))))
	   ((or soma segment)
	    (let ((node (typecase elt
			  (segment (segment-node-2 elt))
			  (soma (soma-node elt)))))
	      (case data-type
		(dvdt (element-dvdt-fast node))
		(leak-current (element-leak-current-fast elt))
		(capacitance-current (element-capacitance-current-fast elt node))
		(t (node-voltage-n+1 node)))))
	   (conc-int (conc-int-concentration elt data-type))
	   (channel
	    (case data-type
	      (conductance (channel-conductance elt))
	      (t (channel-ohmic-current-n+1 elt))))
	   (synapse
	    (case data-type
	      (conductance (synapse-conductance elt))
	      (t (synapse-ohmic-current-n+1 elt))))
	   (t (element-current-value elt data-type)))))


(defun saved-element-data (element data-type)
  (typecase element
    (conc-int (conc-int-concentration element data-type))
    (t
     (case data-type
       (current (element-current element))
       (reversal-potential (element-reversal-potential element))
       (conductance (element-conductance element))
       (concentration
	(typecase element
	  (buffer (buffer-concentration element))))
       (state
	(typecase element
	  (conc-particle (conc-particle-state element))
	  (particle (particle-state element))))
       (voltage (recorded-element-voltage element))
       ((voltage-derivative dvdt node-voltage-derivative) (element-dvdt element))
       (leak-current (element-leak-current element))
       (capacitance-current (element-capacitance-current element))
       (dendrite-current
	(typecase element
	  (soma (soma-dendrite-current element))))
       (field-potential (element-current element))))))
#|
|#

#|
(defun save-element-data (element data-type)
  (case data-type
    (current
     (typecase element
       (vsource (push-vsource-current-data element (get-vsource-current element)))
       (isource (push-isource-current-data element (get-isource-current element)))
       (channel (push-channel-current-data element (channel-current element)))
       (channel-type (push-channel-type-current-data element
						     (let ((sum 0.0))
						       (element-type-do (elt element) (incf sum (channel-current elt)))
						       sum)))
       (synapse (push-synapse-current-data element (synapse-current element)))
       (pump (push-pump-current-data element (pump-current element)))))
    (reversal-potential
     (typecase element
       (channel (push-channel-reversal-potential-data element (channel-reversal-potential element)))
       (synapse (push-synapse-reversal-potential-data element (synapse-reversal-potential element)))))
    (conductance
     (typecase element
       (channel (push-channel-conductance-data element (channel-conductance element)))
       (channel-type (push-channel-type-conductance-data element
							 (let ((sum 0.0))
							   (element-type-do (elt element) (incf sum (channel-conductance elt)))
							   sum)))
       (synapse (push-synapse-conductance-data element (synapse-conductance element)))))
    (total-concentration
     (typecase element
       (conc-int (push-conc-int-total-data element (conc-int-total element)))))
    (concentration-1
     (typecase element
       (conc-int (push-conc-int-shell-1-data element (conc-int-shell-1 element)))))
    (concentration-2
     (typecase element
       (conc-int (push-conc-int-shell-2-data element (conc-int-shell-2 element)))))
    (concentration-3
     (typecase element
       (conc-int (push-conc-int-shell-3-data element (conc-int-shell-3 element)))))
    (concentration
     (typecase element
       (buffer (push-buffer-concentration-data element (buffer-concentration element)))))
    (markov-state
     (typecase element
       (particle (loop for state-label in (element-parameter (particle-type element) 'markov-state-labels)
		       for state-index fixnum from 0 to (nb-states-1 element)
		       do (push-element-parameter element state-label (s-flt (particle-aref-state-n+1 (prt-state element state-index))))))))
    (state
     (typecase element
       (conc-particle (push-conc-particle-state-data element (conc-particle-state element)))
       (particle (push-particle-state-data element (particle-state element)))))
    (voltage 
     (let ((data (recorded-element-voltage element)))
       (typecase element
	 (vsource (push-vsource-voltage-data element (vsource-voltage element)))
	 (axon (push-axon-voltage-data element data))
	 (soma (push-soma-voltage-data element data))
	 (segment (push-segment-voltage-data element data)))))
    ;; NG, 31/10/97 : je rajoute le test WHEN pour aller plus vite les 3/4 du temps.
    (dvdt
     (let ((data (element-dvdt element)))
       (typecase element
	 (soma (push-soma-voltage-derivative-data element data))
	 (segment (push-segment-voltage-derivative-data element data)))))
    (leak-current
     (let ((data (element-leak-current element)))
       (typecase element
	 (soma (push-soma-leak-current-data element data))
	 (segment (push-segment-leak-current-data element data)))))
    (capacitance-current
     (let ((data (element-capacitance-current element)))
       (typecase element
	 (soma (push-soma-capacitance-current-data element data))
	 (segment (push-segment-capacitance-current-data element data)))))
    (dendrite-current
     (typecase element
       (soma 
	(push-soma-dendrite-current-data element (soma-dendrite-current element)))))
    (field-potential
     (typecase element
       (EXTRACELLULAR-ELECTRODE (push-element-parameter element :field-potential (eval-extracellular-electrode element)))))))
|#

(defun axon-voltage (axon)
  (let ((axon (element axon 'axon))) (when axon (node-voltage-n+1 (axon-node axon)))))

(defun particle-state (particle)
  (let ((prt (element particle 'particle))) (when prt (particle-state-n+1-double prt))))

(defun save-particle-data ()
  (loop for particle in *plot-particles-structures* do
	(push-particle-state-data particle (particle-state particle)))
  (loop for particle in *plot-markov-particles-structures* do
	(when (and (eq (particle-type-class (particle-type particle)) :markov)
		   t ; (element-parameter particle 'plot-markov-states)
		   )
	  (loop for state-label in (element-parameter (particle-type particle) 'markov-state-labels)
		for state-index fixnum from 0 to (nb-states-1 particle)
		do (push-element-parameter particle
					   state-label
					   (s-flt (particle-aref-state-n+1 (prt-state particle state-index))))))))

(defun pump-current (pump)
  (let ((pump (element pump 'pump))) (when pump (pump-concentration-current pump))))

(defun buffer-concentration (buffer)
  (let ((buffer (element buffer 'buffer))) (when buffer (buffer-conc-n+1 buffer))))
	
(defun conc-particle-state (conc-particle)
  (let ((prt (element conc-particle 'conc-particle))) (when prt (conc-particle-state-n+1-double prt))))

(defun conc-int-total (conc-int)
  (let ((cint (element conc-int 'conc-int))) (when cint (conc-int-total-free-conc-n+1 cint))))

(defun conc-int-shell-1 (conc-int)
  (let ((cint (element conc-int 'conc-int))) (when cint (conc-int-shell-1-free-conc-n+1 cint))))

(defun conc-int-shell-2 (conc-int)
  (let ((cint (element conc-int 'conc-int))) (when cint (conc-int-shell-2-free-conc-n+1 cint))))

(defun conc-int-shell-3 (conc-int)
  (let ((cint (element conc-int 'conc-int))) (when cint (conc-int-shell-3-free-conc-n+1 cint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro memb-element-type-value (type child-current-value-function cell)
  ;; Integrate the current element data value (as returned by the function CHILD-CURRENT-VALUE-FUNCTION) over all children of a given membrane element
  ;; TYPE, restricted to a specific CELL if included, otherwise over all in circuit.
  `(let ((sum 0.0)
	 (cell-internal ,cell))
    (element-type-do (elt ,type)
     (when (or (not cell-internal)
	       (equal cell-internal (element-cell elt)))
       (incf sum (,child-current-value-function elt))))
    sum))

(defun channel-type-current (type &optional cell) (memb-element-type-value type get-channel-current cell))
(defun channel-type-conductance (type &optional cell) (memb-element-type-value type channel-conductance cell))
(defun synapse-type-current (type &optional cell) (memb-element-type-value type get-synapse-current cell))
(defun synapse-type-conductance (type &optional cell) (memb-element-type-value type synapse-conductance cell))

(defun channel-reversal-potential (channel)
  (let ((ch (element channel 'channel))) (get-channel-e-rev ch)))

(defun get-channel-saved-conductance (channel)
  (let ((ch (element channel 'channel)))
    (if (channel-active-p ch)
	(if *save-conductances-normalized* (/ (channel-conductance ch) (channel-gbar ch)) (channel-conductance ch))
	0.0d0)))

(defun synapse-reversal-potential (synapse)
  (let ((syn (element synapse 'synapse))) (get-synapse-e-rev syn)))

(defun get-synapse-saved-conductance (synapse)
  (let ((syn (element synapse 'synapse)))
    (if (synapse-active-p syn)
	(if *save-conductances-normalized* (/ (synapse-conductance syn) (synapse-gbar syn)) (synapse-conductance syn))
	0.0d0)))

;; Isources

#|
(defun save-isource-data ()
  (loop for isource in *plot-isource-currents-structures* do (push-isource-current-data isource (get-isource-current isource))))
|#

;; Vsources

#|
(defun save-vsource-data ()
  (loop for vsource in *plot-vsource-currents-structures* do (push-vsource-current-data vsource (vsource-current vsource)))
  (loop for vsource in *plot-vsource-voltages-structures* do (push-vsource-voltage-data vsource (vsource-voltage vsource))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun length-element-output-data ()
  "Return the length of all the simulation data lists of all the circuit elements."
  (+ (length-total-conductances)
     (length-sparse-data)
     (length-markov-particles-output-data)
     (length *vsrvolt*)
     (length *vsrnodevolt*)
     (loop for model in (models)
	   when (model-output-data-keys model)
	   sum (loop for instance being the hash-value of (model-hash-table model)
		     sum (length-element-parameters instance (model-output-data-keys model))))))

   
(defun clear-element-output-data () ;; Initialize all the plot data lists to '().
  "Clears all simulation data stored by all the circuit elements."
  (when *enable-sparse-data* (clear-sparse-data))
  (clear-total-conductances)
  (clear-markov-particles-output-data)
  (setq *vsrvolt* nil
	*vsrnodevolt* nil)
  (loop for model in (models)
	when (model-output-data-keys model)
	do (loop for instance being the hash-value of (model-hash-table model)
		 do (remove-element-parameters instance (model-output-data-keys model)))))

(defun clear-markov-particles-output-data ()
  (loop for type in (particle-types)
	when (element-parameter type 'markov-state-labels) do
	(let ((labels (element-parameter type 'markov-state-labels)))
	  (loop for particle in (particle-type-particles type) do
		(loop for state-label in labels do (element-parameter particle state-label nil))))))
	
(defun length-markov-particles-output-data ()
  (loop for type in (particle-types)
	when (element-parameter type 'markov-state-labels) sum
	(let ((labels (element-parameter type 'markov-state-labels)))
	  (loop for particle in (particle-type-particles type) sum
		(loop for state-label in labels sum (length (element-parameter particle state-label)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;     Total Conductance Plotting    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-total-conductances ()
  (parse-*plot-total-conductances*)
  (setq *total-conductances-data* nil))

(defun length-total-conductances () (length *total-conductances-data*))

(defun plot-total-conductances-plot-labels ()
  (loop for spec in *plot-total-conductance-specifications*
	collect
	(typecase spec
	  (atom (element-name spec))
	  (cons (format nil "~A~{-~A~}" (element-name (car spec))
			(coerce-to-list (if (eq :all (cadr spec)) 'all-elts (element-name (cdr spec)))))))))

(defun parse-*plot-total-conductances* ()
  (setq *plot-total-conductance-specifications*
	(no-nils
	 (loop for preliminary-spec in (coerce-to-list *plot-total-conductances*)
	       nconc
	       (if (eq :ALL preliminary-spec)
		   (loop for cell in (cells) collect (list cell :ALL))
		   (list (typecase preliminary-spec
			   (atom (element preliminary-spec))
			   (cons
			    (let ((cell-or-cell-type (element (car preliminary-spec))))
			      (when (or (cell-p cell-or-cell-type)
					(cell-type-p cell-or-cell-type))
				(if (eq (nth 1 preliminary-spec) :ALL)
				    (list cell-or-cell-type :ALL)
				    (flatten-list (list cell-or-cell-type (element-type (cdr preliminary-spec)))))))))))))))

(defun channels-total-conductance (cell-or-chs)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (typecase cell-or-chs
    (cell
     (loop for ch being the hash-value of (channel-hash-table)
	   when (eq cell-or-chs (channel-cell ch))
	   sum (channel-conductance ch) into total double-float
	   finally (return total)))
    (t (do ((chs cell-or-chs (cdr chs))
	    (sum 0.0d0 (the df (+ sum (channel-conductance (car chs))))))
	   ((null chs) sum)))))

(defun synapses-total-conductance (cell-or-syns)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (typecase cell-or-syns
    (cell (loop for syn being the hash-value of (synapse-hash-table)
		when (eq cell-or-syns (synapse-cell syn))
		sum (synapse-conductance syn) into total double-float
		finally (return total)))
    (t (do ((syns cell-or-syns (cdr syns))
	    (sum 0.0d0 (the df (+ sum (synapse-conductance (car syns))))))
	   ((null syns) sum)))))

(defun total-conductance (specification)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((specification (coerce-to-list specification)))
    (s-flt
     (typecase (car specification)
       (synapse-type (the df (synapse-type-conductance (car specification))))
       (channel-type (the df (channel-type-conductance (car specification))))
       (t (loop for cell in (typecase (car specification)
			      (cell-type (cell-type-cells (car specification)))
			      (cell (list (car specification))))
		summing (the sf (cell-max-g-in cell)) into sf-total single-float
		summing (the df (if (eq :all (cadr specification))
				    (+ (the df (channels-total-conductance cell))
				       (the df (synapses-total-conductance cell)))
				    (loop for type in (cdr specification) summing
					  (the df (typecase type
						    (synapse-type (synapse-type-conductance type cell))
						    (channel-type (channel-type-conductance type cell))
						    (t 0.0d0)))
					  into df-subtotal double-float
					  finally (return df-subtotal))))
		into df-total double-float
		finally (return (+ sf-total df-total))))))))

(defun save-total-conductances ()
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when *plot-total-conductance-specifications* ; *PLOT-TOTAL-CONDUCTANCES-P*
    (setq *total-conductances-data*      
	  (if *total-conductances-data*
	      (loop for type in *plot-total-conductance-specifications*
		    for data-list in *total-conductances-data*
		    collect (push (the sf (total-conductance type)) data-list))

	      (loop for type in *plot-total-conductance-specifications*
		    collect (list (the sf (total-conductance type)))))))
  nil)
       


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


;;; SYS Source file: plot.lisp

(in-package "SURF-HIPPO")

;;; The function ELEMENT-DATA has now been defined to provide a more consistent interface, although it calls RETRIEVE-SINGLE-DATA
;;; (see element_functions.lisp).
;;;
;;; RETRIEVE-SINGLE-DATA Given the string or element ELEMENT and the symbol DATA-TYPE, returns the reversed plot data list
;;; directly from the circuit structure referenced in ELEMENT. If either ELEMENT does not refer to a circuit element, or if the
;;; DATA-TYPE is inconsistent with the referenced element, or if the data was not stored in the last simulation (i.e. was not
;;; earmarked for plotting), then the function returns NIL.

;;; Note that the data is in correct time order (ie the same as *SIM-PLOT-TIME-LIST*).

;;; The element names which refer to the following element types may be matched with data types as
;;; follows [for example, (RETRIEVE-SINGLE-DATA "Hippo-soma" 'soma-voltage)]:

;;; extracellular-electrode 'field-potential

;;; segment 'voltage, 'voltage-derivative , 'dvdt

;;; soma 'dendrite-current, 'voltage, 'voltage-derivative, 'dvdt

;;; channel 'current, 'reversal-potential, 'conductance

;;; synapse 'current, 'reversal-potential, 'conductance

;;; particle 'state 'markov-states

;; For markov state data, the optional STATE-INDEX integer argument must be supplied

;;; conc-particle 'state

;;; axon 'voltage

;;; vsource 'current

;;; isource 'current

;;; conc-int 'concentration-1, 'concentration-2, 'concentration-3, 'total-concentration
;;; conc-int 1, 2, 3, 'total

;;; buffer 'concentration

;;; pump 'current

(defun retrieve-single-data (element data-type &optional state-index)
  (let ((element (if (node-p element) (node-cell-element element) element)))
    (case data-type
      ((or markov-states		; This just here since it was here.
	   markov-state)
       (find-markov-particle-state-data element state-index))
      (use-*vsrvolt* (reverse *vsrvolt*))
      (use-*vsrnodevolt* (reverse *vsrnodevolt*))
      (t (car (retrieve-plot-data (list (list (list element) data-type))))))))

(defun clear-single-data (element data-type &optional state-index)
  (let ((element (if (node-p element) (node-cell-element element) element)))
    (case data-type
      ((or markov-states		; This just here since it was here.
	   markov-state)
       (clear-markov-particle-state-data element state-index))
      (use-*vsrvolt* (setq *vsrvolt* nil))
      (use-*vsrnodevolt* (setq *vsrnodevolt* nil))
      (t (car (clear-plot-data (list (list (list element) data-type))))))))

(defun retrieve-plot-data (structures-structure-data-types)
  ;; This function returns a list of data lists, each of which is of the type referenced in the cadr of each sublist in
  ;; STRUCTURES-STRUCTURE-DATA-TYPES and comes from the structure name which is the car of the sublists. The data is reversed from
  ;; that stored in the structures, thus it is returned in correct time order. This function allows the order of the traces (top
  ;; to bottom) of multiple data plots to be determined by the order in the original name-lists (e.g. *plot-membrane-voltages* or
  ;; *plot-channel-currents*).
  (no-nils
   (loop for structure-structure-data-type in structures-structure-data-types
	 nconcing
	 (loop for element in (first structure-structure-data-type)
	       collecting (reverse (element-saved-data (element element) (second structure-structure-data-type)))))))

(defun clear-plot-data (structures-structure-data-types)
  ;; Follows RETRIEVE-PLOT-data.
  (no-nils
   (loop for structure-structure-data-type in structures-structure-data-types
	 nconcing
	 (loop for element in (first structure-structure-data-type)
	       do (element-clear-saved-data (element element) (second structure-structure-data-type))))))

(defun pick-diagonal-somas-for-plotting ()
  (setq ;; *plot-membranesegment-voltages* nil
	*plot-membrane-voltages*
	(loop for soma being the hash-value of (SOMA-HASH-TABLE)
	      when
	      (or
	       (= (first (element-absolute-location soma))
		  (second (element-absolute-location soma)))
	       (= (first (element-absolute-location soma))
		  (third (element-absolute-location soma)))
	       (= (third (element-absolute-location soma))
		  (second (element-absolute-location soma))))
	      collect (soma-name soma))))

(defun dump-all-plot-lists ()
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "plot-settings.lisp"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix-mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (*standard-output* (open info-filename :direction :output :if-exists :supersede
						 :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-circuit t)
	(format t "|#~%~%")

	(loop for plot-list-info in *plot-lists-info*
	      when (symbol-value (plot-list-info-names plot-list-info)) do
	      (write-lisp-sym-list-to-stream (plot-list-info-names plot-list-info)
					     (symbol-value (plot-list-info-names plot-list-info))
					     t 10 0 nil))
	(loop for plot-list-info in *plot-lists-info* do
	      (format t "(setq ~A ~A)~%"
		      (plot-list-info-enable-var plot-list-info)
		      (true-p (symbol-value (plot-list-info-enable-var plot-list-info)))))

	(loop for plot-var in '( *create-new-simulation-plots* *plot-standard-windows* *plot-synapses-by-major-ion*
				*plot-channels-by-major-ion* ;; *plot-currents-by-major-ion*
				;; *position-plots-by-hand
				*enable-plot-labels*
				*SAVE-CONDUCTANCES-normalized*
				*use-same-line-style*
				*plot-data-grouping* *overlay-plots* *accomodate-overlays*
				;; *overlay-simulations *overlay-simulations-last
				*plot-custom-windows* *simulation-plot-window-comment*
				*traces-per-plot* *automatic-voltage-plot-scaling*
				*voltage-plot-min* *voltage-plot-max*
				*automatic-soma-voltage-plot-scaling* *soma-voltage-plot-min*
				*soma-voltage-plot-max* *save-data-step*
				*save-data-step-count* *plot-total-concs-separately*
				;; *plot-shell-concs-separately*
				*plot-event-generators* *plot-events*
				*plot-node-elements* *plot-separate-soma-window*
				;; *plot-concentrations
				;; *transformed-plot
				)
	      do (WRITE-LISP-SYM-AND-VALUE-TO-STREAM plot-var t)))
      (format t "File ~a written~%" info-filename))))

(defun clear-all-plot-lists ()
  (setq *plot-lists-cells* nil
	*plot-lists-cell-types* nil)
  (loop for plot-list-info in *plot-lists-info*
	do (set (plot-list-info-names plot-list-info) nil)
	(set (plot-list-info-structures plot-list-info) nil))
  (setup-models-output-data-enabled))

(defun remove-entry-from-*plot-node-elements* (name)
  (setq *plot-node-elements* (remove name *plot-node-elements* :test 'equal)))

(defun push-name-onto-pump-plot-list (elt)
  (when (and *plot-pump-currents-p* (not (eq *plot-pump-currents* 'all)))
    (push (pump-name elt) *plot-pump-currents*)))

(defun push-name-onto-isource-plot-list (elt)
  (when (and *plot-isource-currents-p* (not (eq *plot-isource-currents* 'all)))
    (push (isource-name elt) *plot-isource-currents*)))

(defun push-name-onto-vsource-plot-list (elt)
  (when (and *plot-vsource-currents-p* (not (eq *plot-vsource-currents* 'all)))
    (push (vsource-name elt) *plot-vsource-currents*)))

(defun push-name-onto-membrane-plot-list (elt node)
  (when (and *plot-membrane-voltages-p* (not (eq *plot-membrane-voltages* 'all)))
    (push (node-name node) *plot-membrane-voltages*))
  (when (and *plot-membrane-voltage-derivatives-p* (not (eq *plot-membrane-voltage-derivatives* 'all)))
    (push (node-name node) *plot-membrane-voltage-derivatives*))
  (when (and (soma-p elt) (not (eq *plot-soma-dendrite-currents* 'all)))
    (push (node-name node) *plot-soma-dendrite-currents*)))

(defun push-name-onto-axon-plot-list (elt node)
  (when (and *plot-axon-voltages-p* (not (eq *plot-axon-voltages* 'all)) (eq node (axon-node elt)))
    (push (node-name node) *plot-axon-voltages*)))

(defun push-name-onto-channel-plot-list (elt)
  (when (and *plot-channel-currents-p* (not (eq *plot-channel-currents* 'all)))
    (push (channel-name elt) *plot-channel-currents*))
  (when (and *plot-channel-reversal-potentials-p* (not (eq *plot-channel-reversal-potentials* 'all)))
    (push (channel-name elt) *plot-channel-reversal-potentials*))
  (when (and *plot-channel-conductances-p* (not (eq *plot-channel-conductances* 'all)))
    (push (channel-name elt) *plot-channel-conductances*)))

(defun push-name-onto-synapse-plot-list (elt)
  (when (not (eq *plot-synapse-currents* 'all))
    (push (synapse-name elt) *plot-synapse-currents*))
  (when (not (eq *plot-synapse-conductances* 'all))
    (push (synapse-name elt) *plot-synapse-conductances*)))

(defun push-name-onto-conc-int-plot-list (elt)
  (when (and *plot-shell-1-concentrations-p* (not (eq *plot-conc-1-ints* 'all)))
    (push (conc-int-name elt) *plot-conc-1-ints*))
  (when (and *plot-shell-2-concentrations-p* (not (eq *plot-conc-2-ints* 'all)))
    (push (conc-int-name elt) *plot-conc-2-ints*))
  (when (and *plot-shell-3-concentrations-p* (not (eq *plot-conc-3-ints* 'all)))
    (push (conc-int-name elt) *plot-conc-3-ints*))
  (when (and *plot-concentrations-p* (not (eq *plot-conc-ints* 'all)))
    (push (conc-int-name elt) *plot-conc-ints*)))

(defun push-name-onto-conc-particle-plot-list (elt)
  (when (not (eq *plot-conc-particles* 'all))
    (push (conc-particle-name elt) *plot-conc-particles*)))

(defun push-name-onto-particle-plot-list (elt)
  (when (and *plot-particles-p* (not (eq *plot-particles* 'all)))
    (push (particle-name elt) *plot-particles*)))

(defun parse-plot-node-elements ()
  (setq *plot-node-elements* (loop for elt in (delete-duplicates (coerce-to-list *plot-node-elements*))
				   collect (element-name (or (element-cell-element elt 'soma) (element-cell-element elt 'segment)))))
  (loop for node-name in *plot-node-elements* do
	(let ((node (or (element-physical-node node-name 'soma)	(element-physical-node node-name 'segment))))
	  (when node (loop for elt in (node-elements node) do
			   (typecase elt
			     (pump (push-name-onto-pump-plot-list elt))
			     (isource (push-name-onto-isource-plot-list elt))
			     (vsource (push-name-onto-vsource-plot-list elt))
			     ((segment soma) (push-name-onto-membrane-plot-list elt node))
			     (axon (push-name-onto-axon-plot-list elt node))
			     (channel (push-name-onto-channel-plot-list elt))
			     (synapse (push-name-onto-synapse-plot-list elt))
			     (conc-int (push-name-onto-conc-int-plot-list elt))
			     (conc-particle (push-name-onto-conc-particle-plot-list elt))
			     (particle (push-name-onto-particle-plot-list elt))))))))

(defun set-global-plot-vars-if-all ()
  (loop for plot-list-info in *plot-lists-info* do
	(if (eq (symbol-value (plot-list-info-names plot-list-info)) 'all) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
	  (set (car plot-list-info)
	       (loop for table in (plot-list-info-tables plot-list-info) ; LIST-OF-TABLES
		     nconcing (list-all-hash-names table))))))

(defun set-global-plot-vars ()
  (loop for plot-list-info in *plot-lists-info* do
	(set (plot-list-info-names plot-list-info) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
	     (delete-duplicates
	      (remove nil (loop for elt-name in (symbol-value (plot-list-info-names plot-list-info)) collecting
				(when
				    (and
				     ;; Test to filter out non event-driven elements from event plotting.
				     (or (not (eq 'event (plot-list-info-structure-slot plot-list-info)))
					 (event-driven-element-p elt-name))
				     ;; Named element is a member of the right hash-table.
				     (loop for table in (plot-list-info-tables plot-list-info) ; LIST-OF-TABLES
					   thereis (gethash (element-name elt-name) table)))
				  (element-name elt-name))))
	      :test #'equal))))

(defun transfer-plot-xx-names-to-plot-xx-structures ()
  (loop for plot-list-info in *plot-lists-info* do
	(set (plot-list-info-structures plot-list-info)	; GLOBAL-VAR-LIST-OF-STRUCTURES
	     (when (symbol-value (plot-list-info-enable-var plot-list-info)) ; ENABLE-VAR
	       (loop for name in (symbol-value (plot-list-info-names plot-list-info)) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
		     collect
		     (loop for table in (plot-list-info-tables plot-list-info)
			   when (gethash name table)
			   do (return (let* ((element (gethash name table))
					     (element-cell (element-cell element))
					     (name (modify-plot-data-trace-label name plot-list-info)))
					;; Associate trace labels to cell and cell type in *PLOT-LISTS-CELLS* and *PLOT-LISTS-CELL-TYPES*.
					(update-plot-lists-cells-and-types element-cell name)
					element))))))))

(defun update-all-save-node-lists ()
  (setq *all-save-voltage-nodes*
	(delete-duplicates (loop for struct-list in (list *plot-membrane-voltages-structures*
							  *plot-path-nodes-structures*
							  *analysis-nodes-structures*)
				 nconc (copy-list struct-list)))
	*all-save-dvdt-nodes*
	(loop for struct-list in
	      (list *plot-membrane-voltage-derivatives-structures*)
	      nconc (copy-list struct-list))

	*all-save-leak-current-nodes*
	(loop for struct-list in (list *plot-leak-currents-structures*) nconc (copy-list struct-list))

	*all-save-capacitance-current-nodes*
	(loop for struct-list in (list *plot-capacitance-currents-structures*) nconc (copy-list struct-list))))

(defun choose-plot-data ()
  ;; First checks to see if any of the global plot list variables is set to 'ALL - if so then that variable is set to a list of
  ;; names of all instances of the appropriate circuit element. Then, modifies various plot lists according to
  ;; *PLOT-NODE-ELEMENTS*. The global plot lists are then checked to remove any names that do not refer to actual circuit element
  ;; instances, and remove any duplicate entries.

  ;; *PLOT-LISTS-INFO* global variable structure:
  ;;
  ;; global-var-list-of-structure-names list-of-tables structure-slot enable-var global-var-list-of-structures plot-y-label
  ;;
  ;; Note that macros access these different posistions in *PLOT-LISTS-INFO* sublists.
  ;;
  ;; Lists of appropriate structures are then generated, from which the actual data saving is referenced.
  ;;
  (set-global-plot-vars-if-all)		; Update any plot lists which are specified as 'all.
  (parse-plot-node-elements)
  (set-global-plot-vars)		; Update plot lists with cleaned up plotting vars
  (init-plot-lists-cells-and-types)
  (transfer-plot-xx-names-to-plot-xx-structures)
  (TRANSFER-PLOT-XX-STRUCTURE-LISTS-TO-MODEL-SAVE-DATA-INSTANCES)
  (setq *analysis-nodes-structures* (loop for name in *analysis-nodes* collect (element-cell-element name)))
  (update-all-save-node-lists)
  (setup-models-output-data-enabled)
  (prepare-plot-file-output)		; Prepare pointers for possible data files.
  nil)

(defun modify-plot-data-trace-label (name plot-list-info)
  (case (plot-list-info-names plot-list-info)
    (*plot-conc-1-ints* (conc-int-shell-1-trace-label name))
    (*plot-conc-2-ints* (conc-int-shell-2-trace-label name))
    (*plot-conc-3-ints* (conc-int-shell-3-trace-label name))
    (*plot-conc-ints* (conc-int-total-trace-label name))
    (*plot-soma-dendrite-currents* (soma-dendrite-current-trace-label name))
    (t name)))

(defun prepare-plot-file-output ()
  ;; This updates *FILE-OUTPUT-VARIABLE-LIST* according to the output pointer lists, for
  ;; writing plot data to a file.
  (setq *file-output-variable-list* '())
  (loop for plot-list-info in *plot-lists-info* do
	(loop for element in (symbol-value (plot-list-info-structures plot-list-info))
	      do
	      ;; Concantenate data list information onto *FILE-OUTPUT-VARIABLE-LIST* that will be dumped to file.
	      (let ((name (element-name element))
		    (data-slot (plot-list-info-structure-slot plot-list-info)))
		(unless
		    (typecase name
		      (string (and (member name *plot-path-nodes* :test 'equal)
				   (not (member name *analysis-nodes* :test 'equal))))
		      (t (and (member name *plot-path-nodes*)
			      (not (member name *analysis-nodes*)))))
		  (push (list (create-output-symbol *simulation-name* name data-slot)
			      (element-name element)
			      data-slot)
			*FILE-OUTPUT-VARIABLE-LIST*)))))
  nil)

(defun init-plot-lists-cells-and-types ()
  (setq *plot-lists-cells* nil *plot-lists-cell-types* nil))

(defun update-plot-lists-cells-and-types (cell name)
  (when cell
    (setq *plot-lists-cells* (acons name cell *plot-lists-cells*)
	  *plot-lists-cell-types* (acons name (cell-type cell) *plot-lists-cell-types*))))

(defun conc-int-shell-1-trace-label (name) (format nil "~A Sh1" (MASSAGE-ELEMENT-PLOT-LABEL name 'conc-int)))
(defun conc-int-shell-2-trace-label (name) (format nil "~A Sh2" (MASSAGE-ELEMENT-PLOT-LABEL name 'conc-int)))
(defun conc-int-shell-3-trace-label (name) (format nil "~A Sh3" (MASSAGE-ELEMENT-PLOT-LABEL name 'conc-int)))
(defun conc-int-total-trace-label (name) (format nil "~A Total" (MASSAGE-ELEMENT-PLOT-LABEL name 'conc-int)))

(defun name-list-from-structures (structures)
  (loop for structure in structures collect (funcall (read-from-string (format nil "~S-NAME" (type-of structure))) structure)))

(defun slot-descriptor-string (plot-list-info &key tables slot extra electrode-p)
  (let ((tables (or tables (plot-list-info-tables plot-list-info)))
	(slot (or slot (plot-list-info-structure-slot plot-list-info)))
	(extra (or extra (plot-list-info-extra-info plot-list-info))))
    (string-capitalize
     (format nil "~a ~a~S"
	     (if electrode-p
	       "Electrode"
		 (concatenate-string-list
		  (loop for table in (coerce-to-list tables)
			when (type-of-first-hash-value table)
			collect (REPLACE-CHAR-W-SPACE-IN-STRING (string (type-of-first-hash-value table)) #\-))
		  :string-spacer "/"))
	     (if extra (format nil "~a " extra) "")
	     slot))))

(defun type-of-first-hash-value (table)
  (loop for struct being the hash-value of (typecase table
					     (symbol (symbol-value table))
					     (t table))
	do (return (type-of struct))))

(defun menu-to-clear-plot-lists ()
  (let ((menu-list) dummy27)
    (loop for plot-list-info in *plot-lists-info*
	  for dummy-sym in (list-of-dummys (length *plot-lists-info*))
	  when (symbol-value (plot-list-info-names plot-list-info))
	  do (setf (symbol-value dummy-sym) nil)
	  (push (list dummy-sym (format nil "Clear all plotted ~A~As"
					(if (eq (first plot-list-info) '*plot-path-nodes*) "Path " "")
					(slot-descriptor-string plot-list-info))
		      :boolean) menu-list))
    (when *plot-node-elements* (push '(dummy27 "Clear nodes for which all elements are plotted" :boolean) menu-list))
    (choose-variable-values menu-list :label "Clear plotting lists")
    (loop for plot-list-info in *plot-lists-info*
	  for dummy-sym in (list-of-dummys (length *plot-lists-info*))
	  when (and (symbol-value dummy-sym) (symbol-value (plot-list-info-names plot-list-info)))
	  do (setf (symbol-value (plot-list-info-names plot-list-info)) nil))
    (when dummy27 (setq *plot-node-elements* nil))))

(defun plot-all-somas () (enable-element-plot (somas)))

(defun choose-plot-elements-menu ()
  (when (and (>= (hash-table-count (SOMA-HASH-TABLE)) 4)
	     (go-ahead-menu "Clear all plotted nodes and just get somas on the diagonal"))
    (PICK-DIAGONAL-SOMAS-FOR-PLOTTING))
  (parse-plot-node-elements)
  ;; Set the flags that authorizes plotting of the various variable classes if each flag is already
  ;; set and there is an element in the circuit that can generate the variable class.
  (loop for plot-list-info in *plot-lists-info*
	when (symbol-value (plot-list-info-enable-var plot-list-info))
	do (set (plot-list-info-enable-var plot-list-info)
		(true-p (and (symbol-value (plot-list-info-enable-var plot-list-info))
			     (non-empty-hash-tables (plot-list-info-tables plot-list-info))))))
  (let ((plot-menu-class-enable
	 (choose-list-values-from-keys
	  (loop for plot-list-info in *plot-lists-info*
		when (and
		      (not (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)))
					; (= (length (plot-list-info-tables plot-list-info)) 1)
		      (symbol-value (plot-list-info-enable-var plot-list-info))
		      (non-empty-hash-tables (plot-list-info-tables plot-list-info)))
		collect
		(let ((slot-descriptor-string (slot-descriptor-string plot-list-info))
		      (num (loop for table in (plot-list-info-tables plot-list-info)
				 sum (cond ((eq 'event (plot-list-info-structure-slot plot-list-info))
					    (loop for thing being the hash-value of table
						  when (or (axon-p thing)
							   (and (synapse-p thing) (eq (synapse-type-control (synapse-type thing)) :event)))
						  sum 1))
					   ((eq 'MARKOV-STATE (plot-list-info-structure-slot plot-list-info))
					    (loop for thing being the hash-value of table
						  when (and (particle-p thing) (eq (particle-type-class (particle-type thing)) :markov))
						  sum 1))
					   (t (HASH-TABLE-count table))))))
		  ;; Not sure why i need SYMBOL-VALUE here
		  ;; (format t "slot-descriptor-string ~A, slot ~A~%" slot-descriptor-string (plot-list-info-structure-slot
		  ;; plot-list-info))
		  (unless (= num 0)
		    (list (format nil "~A (~a instance~:p)" slot-descriptor-string num)
			  slot-descriptor-string))))
	  nil
	  :do-all-at-once t :rank-margin 1 :direction :horizontal :label "Which classes need plot modification?")))
    (plot-selection-menus plot-menu-class-enable)))

(defun plot-menu-class-enable (element &optional model-type)
  (let ((hash (element-hash-table element model-type)))
    (loop for plot-list-info in *plot-lists-info*
	  when (loop for sym in (plot-list-info-tables plot-list-info) thereis (eq hash sym))
	  collect (slot-descriptor-string plot-list-info))))

(defun plot-element-menu (element &optional model-type)
  (element-wrapper
   (element model-type)
   (plot-selection-menus (plot-menu-class-enable elt internal-type) elt internal-type)))

(defun plot-selection-menus (plot-menu-class-enable &optional element model-type)
  ;; For each variable class for which plotting is enabled, generate a selection menu for all the instances of the variable class.
  (let ((element (element element model-type)) slot-descriptor-string slot)
    (loop for plot-list-info in *plot-lists-info*
	  do (setq slot (plot-list-info-structure-slot plot-list-info)
		   slot-descriptor-string (slot-descriptor-string plot-list-info))
	  when (and (member slot-descriptor-string plot-menu-class-enable :test 'equal)
		    (or element (symbol-value (plot-list-info-enable-var plot-list-info)))
		    (not (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)))
		    (non-empty-hash-tables (plot-list-info-tables plot-list-info)))
	  do (if element
	       (let* ((info-names (symbol-value (plot-list-info-names plot-list-info)))
		      (already-there (member (element-name element) info-names)))
		 (set (plot-list-info-names plot-list-info)
		      (if (go-ahead-menu (format nil "Select ~a for Plotting" slot-descriptor-string)
					 (format nil "Plotting for ~A" (element-name element)) already-there)
			(if already-there info-names (cons (element-name element) info-names))
			(remove (element-name element) info-names))))
	       (set (plot-list-info-names plot-list-info)
		    (reorder-names-by-type
		     (select-hash-values-menu
		      (non-empty-hash-tables (plot-list-info-tables plot-list-info))
		      (format nil "Select ~as for Plotting" slot-descriptor-string)
		      :max-per-menu 30 :direction :horizontal :rank-margin 2
		      :inclusion-key (when (eq 'MARKOV-STATE slot) 'MARKOV-PARTICLE-P)
		      :selected-list (if (eq (symbol-value (plot-list-info-names plot-list-info)) 'all)
				       (mapcar 'list-all-hash-names (non-empty-hash-tables (plot-list-info-tables plot-list-info)))
				       (symbol-value (plot-list-info-names plot-list-info))))))))))

;; possible bug in element-type 1/17/98
(defun reorder-names-by-type (list-of-names)
  (let ((types-in-names (delete-duplicates (loop for name in list-of-names collect (element-type name)))))
    (loop for type in types-in-names nconcing (loop for name in list-of-names when (eq type (element-type name)) collect name))))

(defun Choose-Archived-Simulations-to-Plot-menu ()
  (CHOOSE-LIST-VALUES-FROM-KEYS (mapcar #'(lambda (sim) (list (car sim) sim)) *ARCHIVE-VARIABLE-LIST*) nil :label "Choose Archived Simulations to Plot"))

(defun plot-archive-vars ()
  (setq *ARCHIVE-VARIABLE-LIST* (remove-duplicates *ARCHIVE-VARIABLE-LIST* :key 'car)) ; Do this here, anyway.
  (loop for simulation in (Choose-Archived-Simulations-to-Plot-menu) do
	(let* (*plot-data-grouping*
	       (*simulation-name* (ARCHIVE-VARIABLE-LIST-SIMULATION-NAME simulation))
	       (*sim-reverse-plot-time-list* (ARCHIVE-VARIABLE-LIST-SIMULATION-TIME simulation))
	       (menu-list (loop for symbol-and-type in(ARCHIVE-VARIABLE-LIST-SIMULATION-symbols-and-data-types simulation)
				collect	(list (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-descriptor symbol-and-type) symbol-and-type)))
	       (next-plots		; Plotted-symbols-and-types
		(CHOOSE-LIST-VALUES-FROM-KEYS menu-list nil :label (format nil "Choose Data from Simulation ~a" *simulation-name*)))
	       this-plot this-type)
	  (loop until (null next-plots)	do
		(setq this-type (cadar next-plots))
		(let ((this-and-next
		       (loop for symbol-and-type in next-plots
			     when (eq this-type (cadr symbol-and-type))
			     collect symbol-and-type into this-plot
			     else collect symbol-and-type into next-plots
			     finally (return (list this-plot next-plots)))))
		  (setq next-plots (cadr this-and-next))
		  (setq this-plot (car this-and-next))
		  (setq this-type (cadar this-plot))
		  (surf-plot (loop for val in this-plot collect (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-VALUE val))
			     (mapcar #'(lambda (symbol-and-type)
					 (string-remove-head (string (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL symbol-and-type)) (1+ (length *simulation-name*))))
				     this-plot)
			     (string this-type)
			     :x-max nil :x-min nil :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-label (plot-y-label-from-data-type this-type)))))))

(defun ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-descriptor (symbol-and-type)
  (let ((symbol (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL symbol-and-type))
	(data-type (ARCHIVE-VARIABLE-LIST-SIMULATION-DATA-TYPE symbol-and-type)))
    (format nil "~a (~a)"
	    (string-remove-head (string symbol)	(1+ (length *simulation-name*)))
	    (string-head		; remove "-data"
	     (string-downcase data-type)
	     (- (length (string data-type))
		(if (search "-data" (string-downcase data-type))
		    5 0))))))

(defun plot-y-label-from-data-type (data-type)
  (case data-type
    ((voltage soma-voltage-data segment-voltage-data)  "mV")
    ((dvdt voltage-derivative soma-voltage-derivative-data segment-voltage-derivative-data) "mV/ms")
    (axon-voltage-data "mV")
    ((current soma-leak-current-data segment-leak-current-data
	      isource-current-data vsource-current-data soma-dendrite-current-data synapse-current-data channel-current-data) "nA")
    ((conductance channel-conductance-data synapse-conductance-data) "uS")
    ((concentration 1 2 3 total
		    conc-int-concentration-1-data
		    conc-int-concentration-2-data
		    conc-int-concentration-3-data
		    conc-int-total-concentration-data) "mM")
    ((state conc-particle-state-data particle-state-data) "State")))

(defun choose-custom-plot-menu ()
  (setq *custom-plot-lists*
	(no-nils
	 (loop for plot-list-info in *plot-lists-info* collecting
	       (let* ((slot-descriptor-string (slot-descriptor-string plot-list-info))
		      (select-list (select-hash-values-menu (plot-list-info-tables plot-list-info)
							    (format nil "Select ~as for custom plotting" slot-descriptor-string))))
		 (when select-list (list (list select-list slot-descriptor-string))))))))

(defun event-plotter ()
  (cond-every
   ((and *plot-synapse-events-structures* *plot-synapse-events-p*)
    (when *plot-event-generators* (raster-plots :event-elements *plot-synapse-events-structures* :title-postfix "Synapse" :only-event-generators t))
    (when *plot-events* (raster-plots :event-elements *plot-synapse-events-structures* :title-postfix "Synapse" :only-event-generators nil)))
   ((and *plot-axon-events-structures* *plot-axon-events-p*)
    (when *plot-event-generators* (raster-plots :event-elements *plot-axon-events-structures* :title-postfix "Axon" :only-event-generators t))
    (when *plot-events* (raster-plots :event-elements *plot-axon-events-structures* :title-postfix "Axon" :only-event-generators nil)))))

(defun massage-element-plot-label (element &optional model-type include-model-type-for-string-names)
  ;; If INCLUDE-MODEL-TYPE-FOR-STRING-NAMES is a string, then it will be added for string names directly, as opposed to the (TYPE-OF ELEMENT).
  (if (not *massage-element-plot-labels*)
      element
      (let* ((elt (typecase model-type
		    (cons (loop for typ in model-type
				when (element element typ) ; (equal (type-of (element element typ)) typ)
				return (element element typ)))
		    (t (element element model-type))))
	     (print-name (if (numberp (element-name elt))
			     (if (element-type elt)
				 (format nil "~A ~A: type ~A"
					 (string (type-of elt)) (element-name elt) (element-name (element-type elt)))
				 (format nil "~A ~A" (string (type-of elt)) (element-name elt)))
			     (element-name elt))))
	(cond ((and include-model-type-for-string-names print-name)
	       (format nil "~A ~A" (string-capitalize
				    (if (stringp include-model-type-for-string-names)
					include-model-type-for-string-names
					(string (type-of elt)))) print-name))
	      ((not print-name)
	       (typecase element
		 (string  element)
		 (number (format nil "~A" element))
		 (t (format nil "~A" (type-of element)))))
	      (t
	       (let ((add-cell-name (HASH-TABLE-HAS-MORE-THAN-ONE-VALUE-P (cell-hash-table))))
		 (if (numberp print-name)
		     (if (electrode-p elt)
			 (format nil "Electrode ~D" print-name)
			 (typecase elt
			   (cell (format nil "Cell ~D (Type ~A)" print-name (element-name (element-type elt))))
			   (soma (format nil "Soma ~D~A" print-name (if add-cell-name (format nil " (Cell ~A)" (cell-name (soma-cell elt))) "")))
			   (segment (format nil "Seg ~D~A" print-name (if add-cell-name (format nil " (Cell ~A)" (cell-name (segment-cell elt))) "")))
			   ((or channel synapse particle conc-particle pump buffer conc-int isource vsource axon)
			    (let ((cell-elt (element-cell-element elt)))
			      (format nil "~A ~D (~A~A~A)"
				      (typecase elt
					(axon "Axon")
					(vsource "Vsrc")
					(isource "Isrc")
					(synapse "Syn")
					(channel "Chan")
					(particle "Part")
					(conc-particle "ConcPart")
					(pump "Pump")
					(buffer "Buffer")
					(conc-int "ConcInt")
					(t (string-capitalize (string (type-of elt)))))
				      print-name
				      (if (electrode-p cell-elt)
					  "Electrode "
					  (if (and add-cell-name (soma-p cell-elt)) ""
					      (typecase cell-elt
						(soma "Soma ")
						(segment "Seg "))))
				      (if (and (not (electrode-p cell-elt)) add-cell-name (soma-p cell-elt)) ""
					  (format nil "~A, " (element-name cell-elt))) ; If element is on soma, cell reference is sufficient.
				      (if add-cell-name (format nil "Cell ~A" (cell-name (element-cell elt))) ""))))
			   (t element)))
		     (if (not include-model-type-for-string-names)
			 print-name
			 (format nil "~:(~A~:) ~A"
				 (if (stringp include-model-type-for-string-names) include-model-type-for-string-names (string (type-of elt)))
				 print-name)))))))))

(defun massage-element-plot-labels (elements &optional model-type) (mapcar #'(lambda (elt) (massage-element-plot-label elt model-type)) (coerce-to-list elements)))

(defun plot-node-voltages () (plot-memb-elt-data-type '*plot-membrane-voltages*))
(defun plot-node-voltage-derivatives () (plot-memb-elt-data-type '*plot-membrane-voltage-derivatives*))
(defun plot-channel-conductances () (plot-memb-elt-data-type '*plot-channel-conductances*))
(defun plot-channel-reversal-potentials () (plot-memb-elt-data-type '*plot-channel-reversal-potentials*))
(defun plot-channel-currents () (plot-memb-elt-data-type '*plot-channel-currents*))
(defun plot-channel-type-currents () (plot-memb-elt-data-type '*plot-channel-type-currents*))
(defun plot-channel-type-conductances () (plot-memb-elt-data-type '*plot-channel-type-conductances*))
(defun plot-synapse-conductances () (plot-memb-elt-data-type '*plot-synapse-conductances*))
(defun plot-synapse-reversal-potentials () (plot-memb-elt-data-type '*plot-synapse-reversal-potentials*))
(defun plot-synapse-currents () (plot-memb-elt-data-type '*plot-synapse-currents*))
(defun plot-synapse-type-currents () (plot-memb-elt-data-type '*plot-synapse-type-currents*))
(defun plot-synapse-type-conductances () (plot-memb-elt-data-type '*plot-synapse-type-conductances*))
(defun plot-leak-currents () (plot-memb-elt-data-type '*plot-leak-currents*))
(defun plot-capacitance-currents () (plot-memb-elt-data-type '*plot-capacitance-currents*))

;; possible bug in element 1/17/98 (simple-names)
;; perhaps fixed 5/5/98
(defun plot-memb-elt-data-type (plot-list-names-symbol)
  (let* ((plot-list-info (find plot-list-names-symbol *plot-lists-info* :key 'car))
	 (plot-list-info-types (plot-list-info-types plot-list-info))
	 (plotted-elements (flatten-no-nils-list (loop for model-type in plot-list-info-types ; Parse with ELEMENT using the TYPE arg.
						       collect (element (symbol-value plot-list-names-symbol) model-type))))
	 (model-types (delete-duplicates (mapcar 'type-of plotted-elements)))
	 (data-type (plot-list-info-structure-slot plot-list-info))
	 k-elements na-elements ca-elements other-elements)
;;  (printvars plotted-elements plot-list-info-types plot-list-names-symbol)
    (loop for element in plotted-elements do
	  (case (and (case (type-of element) ; model-type
		       (channel *plot-channels-by-major-ion*)
		       (synapse *plot-synapses-by-major-ion*))
		     (element-major-ion element (type-of element)))
	    (k (push element k-elements))
	    (na (push element na-elements))
	    (ca (push element ca-elements))
	    (t  (push element other-elements))))
    (loop for elements in (list k-elements na-elements ca-elements other-elements)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when elements do
	  (surf-plot (retrieve-plot-data (list (list elements data-type)))
		     elements
		     (format nil "~A~A" ion (slot-descriptor-string nil :tables (mapcar 'get-model-hash-table model-types) :slot data-type))
		     :top-element-list elements
		     :model-type model-types
		     :y-min (case data-type
			      (conductance 0.0)
			      (t nil))
		     :x-label *DEFAULT-SURF-PLOT-X-LABEL*
		     :y-label (case data-type
				(conductance (if *SAVE-CONDUCTANCES-normalized* *normalized-gbar-label* (plot-list-info-units-label plot-list-info)))
				(t (plot-list-info-units-label plot-list-info)))))))

(defun plot-particles ()
  (plot-particles-core *plot-particles*)
  (plot-particles-core *plot-markov-particles* t))

(defun plot-particles-core (particles-to-plot &optional markov)
  (let (k-particles na-particles ca-particles other-particles)
    (loop for particle-name in particles-to-plot do
	  (case (and *plot-channels-by-major-ion* (element-major-ion particle-name 'particle))
	    (k (push particle-name k-particles))
	    (na (push particle-name na-particles))
	    (ca (push particle-name ca-particles))
	    (t  (push particle-name other-particles))))
    (loop for particles in (list k-particles na-particles ca-particles other-particles)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when particles do
	  (loop for prt in (element particles 'particle)
		when (and markov (eq :markov (particle-type-class (element-type prt))))
		collect prt into markov-particles
		else unless markov collect (element prt 'particle) into two-state-particles
		finally	(cond-every
			 (two-state-particles
			  (surf-plot (retrieve-plot-data (list (list two-state-particles `state)))
				     two-state-particles (format nil "~aChannel Particle States" ion)
				     :top-element-list two-state-particles
				     :model-type 'particle :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-label "State" :y-min 0 :y-max 1))
			 (markov-particles
			  (plot-markov-particles markov-particles)))))))

(defun plot-markov-particles (markov-particles)
  (loop for prt in markov-particles do
	(let ((prt (element prt 'particle)))
	  (when t			; (element-parameter prt 'plot-markov-states)
	    (loop for state-index fixnum from 0 to (nb-states-1 prt)
		  for label in (element-parameter (element-type prt) 'markov-state-labels)
		  collect (find-markov-particle-state-data prt state-index) into data-lists
		  collect (format nil "~A ~A" (massage-element-plot-label prt 'particle) label)
		  into label-list
		  finally (plot-timed-data data-lists label-list *sim-plot-time-list*
					   :title (format nil "~a Markov Particle" (channel-name (particle-channel prt)))
					   :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-label "State" :y-min 0 :y-max 1))))))

(defun plot-conc-particles ()
  (let (k-conc-particles na-conc-particles ca-conc-particles other-conc-particles)
    (loop for conc-particle-name in *plot-conc-particles* do
	  (case (and *plot-channels-by-major-ion* (element-major-ion conc-particle-name 'conc-particle))
	    (k (push conc-particle-name k-conc-particles))
	    (na (push conc-particle-name na-conc-particles))
	    (ca (push conc-particle-name ca-conc-particles))
	    (t  (push conc-particle-name other-conc-particles))))
    (loop for conc-particles in (list k-conc-particles na-conc-particles ca-conc-particles other-conc-particles)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when conc-particles do
	  (surf-plot (retrieve-plot-data (list (list (elements conc-particles 'conc-particle) `state)))
		     conc-particles (format nil "~aChannel Concentration Particle States" ion)
		     :top-element-list (elements conc-particles 'conc-particle)
		     :model-type 'conc-particle :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-label "State" :y-min 0 :y-max 1))))

(defun plot-vsource-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-vsource-currents-structures* `current)))
	      *plot-vsource-currents* "Voltage Clamp Currents"
	      :top-element-list *plot-vsource-currents-structures*
	      :model-type 'vsource :y-label "nA" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))

(defun plot-vsource-voltages ()
  (surf-plot (retrieve-plot-data (list (list *plot-vsource-voltages-structures* `voltage)))
	      *plot-vsource-voltages* "Voltage Clamp Voltages"
	      :top-element-list *plot-vsource-currents-structures*
	      :model-type 'vsource :y-label "mV" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))

(defun plot-custom-windows ()
  (loop for custom-plot-list in *custom-plot-lists* do
	(surf-plot (retrieve-plot-data custom-plot-list)
		   (caar custom-plot-list)
		   "Data Plot"
		   :y-label (let ((units (element-data-units (caar custom-plot-list))))
			      (typecase units
				(cons (car units))
				(t units)))
		   :x-label *DEFAULT-SURF-PLOT-X-LABEL*)))

(defun plot-field-potentials ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-extracellular-electrodes-structures* 'field-potential)))
   *plot-field-potentials*
   "Extracellular Field Potentials"
   :model-type 'extracellular-electrode
   :top-element-list *plot-extracellular-electrodes-structures* :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-label "mV"))

(defun plot-path-node-voltages ()
  (let ((cells-in-data (delete-duplicates (element-cell *plot-path-nodes-structures*))))
    (loop for cell in cells-in-data do
	  (let* ((this-path (reverse (loop for thing in *plot-path-nodes-structures* when (eq cell (element-cell thing)) collect thing)))
		 (*traces-per-plot* (length this-path))
		 (*auto-waterfall-y-trace-overlap* 0.5))
	    (surf-plot
	     (retrieve-plot-data (list (list this-path `voltage)))
	     (element-name this-path)
	     (format nil "Voltages From ~A to ~a Soma" (element-name (car this-path)) (cell-name cell))
	     :top-element-list this-path
	     :y-label "mV" :x-label *DEFAULT-SURF-PLOT-X-LABEL*
	     :waterfall-x-offset *voltage-plot-waterfall-x-offset* :waterfall-y-offset *voltage-plot-waterfall-y-offset* :waterfall t :label-waterfall t
	     :y-min (unless *automatic-voltage-plot-scaling* *voltage-plot-min*) :y-max (unless *automatic-voltage-plot-scaling* *voltage-plot-max*))))))

(defun plot-axon-voltages ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-axon-voltages-structures* `voltage)))
   *plot-axon-voltages*
   "Axon Output Voltages"
   :top-element-list *plot-axon-voltages-structures* :model-type 'axon :y-label "mV" :x-label *DEFAULT-SURF-PLOT-X-LABEL*
   :y-min (unless *automatic-voltage-plot-scaling* *voltage-plot-min*) :y-max (unless *automatic-voltage-plot-scaling* *voltage-plot-max*)))

(defun plot-isource-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-isource-currents-structures* `current)))
	     *plot-isource-currents* "Current Sources"
	     :top-element-list *plot-isource-currents-structures* :model-type 'isource :y-label "nA" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))

(defun plot-pump-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-pumps-structures* `current)))
	     *plot-pump-currents* "Membrane Ion Pumps" :top-element-list *plot-pumps-structures* :model-type 'pump :y-label "mM/ms" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))

(defun plot-channel-currents-with-dendrite-currents ()
  (let ((labels
	    ;; (concatenate 'list *plot-channel-currents* (soma-dendrite-current-labels  *plot-soma-dendrite-currents*))
	    (concatenate 'list (MASSAGE-ELEMENT-PLOT-LABELS *plot-channel-currents* 'channel)
			 (soma-dendrite-current-labels (MASSAGE-ELEMENT-PLOT-LABELS *plot-soma-dendrite-currents* 'soma)))))
    (surf-plot
     (retrieve-plot-data (list (list *plot-channel-currents-structures* `current) (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
     labels
     (concatenate-strings
      (when *plot-channel-currents* "Channel ")
      (when (and *plot-channel-currents* *plot-soma-dendrite-currents*) "and ")
      (when *plot-soma-dendrite-currents* "Soma-Dendrite ")
      "Currents")
     :top-element-list (concatenate 'list *plot-channel-currents-structures* *plot-soma-dendrite-currents-structures*)
     :y-label "nA" :x-label *DEFAULT-SURF-PLOT-X-LABEL*)))

(defun plot-channel-and-soma-dendrite-currents ()
  ;; Local bind of *plot-soma-dendrite-currents-p* to make sure that such a current exists (needs a segment).
  (let ((*plot-soma-dendrite-currents-p* (and *plot-soma-dendrite-currents-p* *segment*)))
    (cond (*plot-channels-by-major-ion*
	   (cond-every
	    (*plot-channel-currents-p* (plot-channel-currents))
	    (*plot-soma-dendrite-currents-p*
	     (surf-plot (retrieve-plot-data (list (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
			(soma-dendrite-current-labels *plot-soma-dendrite-currents*)
			"Soma-Dendrite Currents"
			:top-element-list *plot-soma-dendrite-currents-structures* :y-label "nA" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))))
	  ((and *plot-channel-currents-p* *plot-soma-dendrite-currents-p*) (plot-channel-currents-with-dendrite-currents))
	  (*plot-channel-currents-p* (plot-channel-currents))
	  (*plot-soma-dendrite-currents-p*
	   (surf-plot (retrieve-plot-data (list (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
		      (soma-dendrite-current-labels *plot-soma-dendrite-currents*)
		      "Soma-Dendrite Currents"
		      :top-element-list *plot-soma-dendrite-currents-structures* :y-label "nA" :x-label *DEFAULT-SURF-PLOT-X-LABEL*)))))

(defun soma-dendrite-current-trace-label (name) (format nil "~A Dendritic current" name))

(defun soma-dendrite-current-labels (soma-dendrite-currents)
  (loop for name in soma-dendrite-currents collect (soma-dendrite-current-trace-label (massage-element-plot-label name 'soma))))

(defun all-plotted-conc-ints ()
  (flatten-no-nils-list (when *plot-shell-1-concentrations-p* *plot-conc-1-ints-structures*)
			(when *plot-shell-2-concentrations-p* *plot-conc-2-ints-structures*)
			(when *plot-shell-3-concentrations-p* *plot-conc-3-ints-structures*)
			(when (and *plot-concentrations-p* (not *PLOT-TOTAL-CONCS-SEPARATELY*)) *plot-conc-ints-structures*)))

(defun return-ion-label-if-only-one-plotted ()
  (loop for conc-int in (all-plotted-conc-ints)
	collect (conc-int-type-species (conc-int-type conc-int)) into ions
	collect (conc-int-type-valence (conc-int-type conc-int)) into valences
	finally (return (let ((all-species (delete-duplicates ions)))
			  (if (= (length all-species) 1)
			    (format nil "~A~d " (car all-species)
				    (char-seq-to-string (loop for symbol-count from 1 to (abs (round (car valences)))
							      collect (if (< (round (car valences)) 0) "-" "+"))))
			    "")))))

(defun concentration-plotter-data-keys ()
  (list (no-nils (list (when *plot-shell-1-concentrations-p* (list *plot-conc-1-ints-structures* `concentration-1))
		       (when *plot-shell-2-concentrations-p* (list *plot-conc-2-ints-structures* `concentration-2))
		       (when *plot-shell-3-concentrations-p* (list *plot-conc-3-ints-structures* `concentration-3))
		       (when (and *plot-concentrations-p* (not *PLOT-TOTAL-CONCS-SEPARATELY*))
			 (list *plot-conc-ints-structures* `total-concentration))))
	(when (and *plot-concentrations-p* *PLOT-TOTAL-CONCS-SEPARATELY*)
	  (list (list *plot-conc-ints-structures* `total-concentration)))))

(defun concentration-plotter-labels ()
  (list (nconc (when *plot-shell-1-concentrations-p* (mapcar 'conc-int-shell-1-trace-label *plot-conc-1-ints*))
	       (when *plot-shell-2-concentrations-p* (mapcar 'conc-int-shell-2-trace-label *plot-conc-2-ints*))
	       (when *plot-shell-3-concentrations-p* (mapcar 'conc-int-shell-3-trace-label *plot-conc-3-ints*))
	       (when (and *plot-concentrations-p* (not *PLOT-TOTAL-CONCS-SEPARATELY*)) (mapcar 'conc-int-total-trace-label *plot-conc-ints*)))
	(when (and *plot-concentrations-p* *PLOT-TOTAL-CONCS-SEPARATELY*) (mapcar 'conc-int-total-trace-label *plot-conc-ints*))))

(defun concentration-plotter ()
  (loop for data-key-list in (concentration-plotter-data-keys)
	for label-list in (concentration-plotter-labels)
	for suffix in '("" " (Total)") do
	(let ((plot-data (retrieve-plot-data data-key-list))
	      (title (format nil "~AConcentrations~A" (return-ion-label-if-only-one-plotted) suffix)))
	  (when plot-data (surf-plot plot-data label-list title :y-label "mM" :x-label *DEFAULT-SURF-PLOT-X-LABEL* :y-min 0)))))

(defun total-conductance-plotter ()
  (when (and *PLOT-TOTAL-CONDUCTANCES-p* *total-conductances-data*)
    (let (*plot-data-grouping*)
      (surf-plot (mapcar 'reverse *total-conductances-data*) (plot-total-conductances-plot-labels)
		 "Total Conductances" :y-min 0.0 :y-label "uS" :x-label *DEFAULT-SURF-PLOT-X-LABEL*))))

(defun LABEL-CELL-TYPE-IN-*PLOT-LISTS-CELL-TYPES* (label)
  ;; Find cell type associated with plot LABEL.
  (cdr (typecase label
	 (string (assoc label *plot-lists-cell-types* :test 'equal))
	 (t (assoc label *plot-lists-cell-types*)))))

(defun label-cell-in-*plot-lists-cells* (label)
  ;; Find cell associated with plot LABEL.
  (cdr (typecase label
	 (string (assoc label *plot-lists-cells* :test 'equal))
	 (t (assoc label *plot-lists-cells*)))))

(defun surf-plot-time-incs (max-time)
  (cond ((< max-time 5) 0.5)
	((< max-time 10) 1.0)
	((< max-time 20) 2.0)
	((< max-time 40) 5.0)
	((< max-time 100) 10.0)))

(defun surf-plot-trace-analysis-strings (data-lists labels time-list)
  (mapcar
   #'(lambda (data label) (PLOT-TRACE-ANALYSIS-STRING data (when (> (length labels) 1) label) time-list
						      (if *remove-initial-value-from-plot-trace-analysis-integrals* (car data) 0.0)))
   data-lists labels))

(defun surf-plotter ()
  ;; Local bind of *plot-soma-dendrite-currents-p* to make sure that such a current exists (needs a segment).
  (let ((*plot-soma-dendrite-currents-p* (and *plot-soma-dendrite-currents-p* *segment*)))
    (when *save-simulation-data*
      (when *plot-custom-windows* (plot-custom-windows))
      (when *plot-standard-windows*
	(cond-every
	 (*plot-field-potentials-p* (plot-field-potentials))
	 (*plot-membrane-voltages-p* (plot-node-voltages))
	 (*plot-path-node-voltages-p* (plot-path-node-voltages))
	 (*plot-membrane-voltage-derivatives-p* (plot-node-voltage-derivatives))
	 (*plot-axon-voltages-p* (plot-axon-voltages))
	 (*plot-isource-currents-p* (plot-isource-currents))
	 (*plot-pump-currents-p* (plot-pump-currents))
	 ((or *plot-channel-currents-p* *plot-soma-dendrite-currents-p*) (plot-channel-and-soma-dendrite-currents))
	 (*plot-synapse-currents-p* (plot-synapse-currents))
	 (*plot-vsource-currents-p* (plot-vsource-currents))
	 (*plot-vsource-voltages-p* (plot-vsource-voltages))
	 ((or *plot-particles-p* *plot-markov-particles-p*) (plot-particles))
	 (*plot-conc-particles-p* (plot-conc-particles))
	 (*plot-channel-conductances-p* (plot-channel-conductances))
	 (*plot-channel-reversal-potentials-p* (plot-channel-reversal-potentials))
	 (*plot-channel-type-currents-p* (plot-channel-type-currents))
	 (*plot-channel-type-conductances-p* (plot-channel-type-conductances))
	 (*plot-synapse-reversal-potentials-p* (plot-synapse-reversal-potentials))
	 (*plot-synapse-conductances-p* (plot-synapse-conductances))
	 (*plot-synapse-type-currents-p* (plot-synapse-type-currents))
	 (*plot-synapse-type-conductances-p* (plot-synapse-type-conductances))
	 (*PLOT-TOTAL-CONDUCTANCES-p* (total-conductance-plotter))
	 (*plot-capacitance-currents-p* (plot-capacitance-currents))
	 (*plot-leak-currents-p* (plot-leak-currents))
	 ((or *plot-shell-1-concentrations-p* *plot-shell-2-concentrations-p* *plot-shell-3-concentrations-p* *plot-concentrations-p*)
	  (concentration-plotter))
	 ((or *plot-synapse-events-p* *plot-axon-events-p*) (event-plotter))))
      (setq *CREATE-NEW-SIMULATION-PLOTS* nil)
      (when *store-plot-results-to-folder* (traces-to-folder t))
      (when *hard-copy-screen* (HARD-COPY-SCREEN)))))

(defun get-circuit-or-simulation-name (&optional (use-simulation-name *use-simulation-name-for-simulation-plot-titles*)) (if use-simulation-name *simulation-name* *circuit*))

(defun surf-plot (top-data-lists top-label-list plot-pane-type &key
		  top-element-list model-type
		  (x-min *user-start-time*) (x-max *user-stop-time*) (x-label *DEFAULT-SURF-PLOT-X-LABEL*) y-min y-max y-label
		  (enable-labels *enable-plot-labels*) (use-same-line-style *use-same-line-style*)
		  (waterfall-x-offset 0.0) (waterfall-y-offset 0.0) waterfall label-waterfall
		  (overlay *overlay-plots*) (update *update-plots*) (resurrect *resurrect-plots*))
  ;; DATA-LISTS is a list of individual data lists. This generates multiple windows of type PLOT-PANE-TYPE (actually, this is just a name), with a
  ;; subscript, so that no more than *TRACES-PER-PLOT* traces out of DATA-LISTS is plotted in a given window. Very neat, not crowded.
  (setq overlay	(and (not (or *create-new-plot-windows* *CREATE-NEW-SIMULATION-PLOTS*)) *overlay-plots*))
  (let* ((model-has-no-cells-p (member model-type '(extracellular-electrode)))
	 (*plot-data-grouping* (if model-has-no-cells-p nil *plot-data-grouping*))
	 (*create-new-plot-windows* (or *create-new-plot-windows* *CREATE-NEW-SIMULATION-PLOTS*))
	 (*hide-output-windows* (or *hide-output-windows* *HIDE-plot-WINDOWS*))
	 (no-labels (or (not enable-labels) (not top-label-list)))
	 (top-label-list (or (loop for thing in (coerce-to-list top-label-list)
				   collect (typecase thing
					     (string thing)
					     (t (element-name thing))))
			     (loop for i from 1 to (length top-data-lists) collect nil)))
	 (cells-in-data (CLEAN-UP-LIST (mapcar 'label-cell-in-*plot-lists-cells* top-label-list)))
	 (cell-types-in-data (CLEAN-UP-LIST (mapcar 'LABEL-CELL-TYPE-IN-*PLOT-LISTS-CELL-TYPES* top-label-list)))
	 (*traces-per-plot* (or *traces-per-plot* 0))
	 (pane-counter (when (and (not (= 0 *traces-per-plot*))
				  (or (when (eq *plot-data-grouping* :cell) (> (length cells-in-data) 1))
				      (> (length top-label-list) *traces-per-plot*)))
			 1)))
    (loop for cell-or-type in (if model-has-no-cells-p
				  '(nil)
				  (or (if (eq *plot-data-grouping* :cell) cells-in-data cell-types-in-data) (cell-types)))
	  do
	  (loop for label in top-label-list for data-list in top-data-lists
		for count from 0
		when (let ((element (nth count top-element-list)))
		       (or (not element)
			   (not *plot-data-grouping*)
			   (and (eq *plot-data-grouping* :cell) (eq cell-or-type (element-cell element))
					; (label-cell-in-*plot-lists-cells* label)
				)
			   (and (eq *plot-data-grouping* :cell-type) (eq cell-or-type (cell-type (element-cell element)))
					; (label-cell-type-in-*plot-lists-cell-types* label)
				)))
		collect label into label-list and collect data-list into data-lists
		finally
		(when (>= (length data-lists) 1) ; Make sure that there is data to plot.
		  (let (pane-data-list pane-label-list)
		    (loop for data-list in data-lists for label in label-list for count from 1 for total-count from 1
			  do (push data-list pane-data-list) (push label pane-label-list)
			  when (or (= total-count (length data-lists)) (= count *traces-per-plot*))
			  do (let* ((sub-plot-pane-type (if pane-counter (concatenate-strings plot-pane-type (format nil " ~A" pane-counter)) plot-pane-type))
				    (name (concatenate-strings (get-circuit-or-simulation-name) ": " sub-plot-pane-type))
				    ;; Kill the overlay flag if there is no window to overlay.
				    (plot-pane (get-plot-window (if waterfall :waterfall :xy) sub-plot-pane-type overlay :session-name *session-name*)))
			       (s-value plot-pane :title (GET-win-TITLE-STRING name))
			       (s-value plot-pane :scatter-symbol-units-in-pixels t)
			       (s-value plot-pane :y-symbol-width 10)
			       (s-value plot-pane :x-symbol-width 10)
			       (let* (*create-new-plot-windows* ; Suppress this since the earlier call to get-plot-window will reflect this.
				      (labels (unless no-labels (massage-element-plot-labels (reverse pane-label-list) model-type)))
				      (data-lists (reverse pane-data-list))
				      (time-list (current-sim-plot-time-list))
				      (*simulation-plot-window-comment*
				       (if *add-trace-analysis-to-plot*
					   (concatenate-string-list
					    (cons *simulation-plot-window-comment* (surf-plot-trace-analysis-strings data-lists labels time-list))
					    :lf-count 1)
					   *simulation-plot-window-comment*)))
				 (plot-timed-data data-lists
						  labels
						  time-list
						  :win plot-pane :overlay overlay :timed-data t
						  :connect-data-points *connect-data-points
						  :comment *simulation-plot-window-comment* :comment-position *simulation-plot-window-comment-position*
						  :label-traces (and (not (or waterfall no-labels))
								     *label-surf-plots*
								     (not (and (numberp *max-num-traces-for-plot-trace-labels*)
									       (> (length pane-label-list)
										  *max-num-traces-for-plot-trace-labels*))))
						  :session-name *session-name*
						  :waterfall waterfall :label-waterfall label-waterfall :auto-wf-setup (and waterfall *AUTO-PLOT-WATERFALL*)
						  :x-trace-offset (if waterfall waterfall-x-offset 0.0) :y-trace-offset (if waterfall waterfall-y-offset 0.0)
						  :use-same-line-style use-same-line-style
						  :y-min y-min :y-max y-max :y-label (string y-label)
						  :x-max x-max :x-min x-min :x-label x-label
						  :x-inc (SURF-PLOT-TIME-INCS (or x-max *user-stop-time*))
						  :update update :resurrect resurrect))
			       (setq count 0
				     pane-data-list '()
				     pane-label-list '())
			       (when pane-counter (setq pane-counter (1+ pane-counter))))))))
	  (unless *plot-data-grouping* (return)))))

(defun collect-extracted-events (&key element data-type model-type data-dted (dt 0.1)
				      return-traces
				      (pre-event-time 0.0) (post-event-time 0.0)
				      event-times (event-type :spike) (threshold -20)
				      title title-prefix
				      plot draw-grid y-min y-max y-inc x-inc (y-label "mV") (x-label *DEFAULT-SURF-PLOT-X-LABEL*)
				      (time-base (current-sim-plot-time-list)) state-index)
  "Extract events from DATA-DTED, which may be a list or a list of lists, if supplied, otherwise from the data of DATA-TYPE
associated with ELEMENT of MODEL-TYPE. The time base for event extraction is given by TIME-BASE [default
\(CURRENT-SIM-PLOT-TIME-LIST\)], which is resampled at DT [default 0.1].  The reference times for the extracted events are either
explicitly given by EVENT-TIMES, if supplied, otherwise these times are derived by event detection from either DATA-DTED [which in
this case must be a flat list], or else from the ELEMENT data as mentioned above. In the case of where events are detected, the
detection algorithm is provided by the function ELEMENT-SPIKE-TIMES, searching for events of EVENT-TYPE [default :SPIKE, using the
value of THRESHOLD]. The duration of extracted events is defined by PRE-EVENT-TIME and POST-EVENT-TIME, prior to and after the
event time, respectively. If PLOT is T, then plot the overlaid events with window TITLE [default 'Superimposed' followed by the
EVENT-TYPE], concatenated with TITLE-PREFIX. If RETURN-TRACES returns event traces as

     ((event-1-timebase event-1)
      (event-2-timebase event-2)
       ...
      (event-n-timebase event-n))

where each event timebase is a list of times referenced to the associated event time."
  (let* ((data-dted (or data-dted (element-data-dted element dt data-type model-type time-base state-index)))
	 (data-max-time (* (length data-dted) dt))
	 (event-times (or event-times
			  (case event-type
			    (:spike (element-spike-times nil :data-list data-dted :time-base dt :spike-threshold threshold)))))
	 (labels (loop for count from 1 to (length event-times) collect (format nil "~A ~D" event-type count)))
	 event-traces event-time-lists)
    (loop for data-dted-list in (if (consp (car data-dted)) data-dted (list data-dted)) do
	  (loop for event-time in event-times do
		(let* ((actual-event-window-start (max 0 (- event-time pre-event-time)))
		       (actual-event-window-stop (min data-max-time (+ post-event-time event-time))))
		  (push (DATA-WINDOW data-dted-list actual-event-window-start actual-event-window-stop dt)
			event-traces)
		  (push (list-of-nums (/ (- actual-event-window-stop actual-event-window-start) dt)
				      (if (< event-time pre-event-time) (- event-time) (- pre-event-time))
				      dt)
			event-time-lists))))
    (cond-every
     (plot (plot-timed-data (reverse event-traces) labels (reverse event-time-lists) :y-label y-label :x-label x-label
			    :x-origin (- pre-event-time) :x-min (- pre-event-time) :x-max post-event-time :x-inc x-inc
			    :draw-grid draw-grid :y-min y-min :y-max y-max :y-inc y-inc
			    :title (format nil "~A~A"
					   (if title-prefix (format nil "~a: " title-prefix) "")
					   (or title (format nil "Superimposed ~As" event-type :CANONIC-LABEL event-type)))))
     (return-traces (loop for event-trace in (reverse event-traces)
			  for event-time-list in (reverse event-time-lists)
			  collect (list event-time-list event-trace))))))

(defun phase-plots (element-pairs &key title y-label x-label x-min y-min x-max y-max (prompt-for-overlay t))
  "The argument ELEMENT-PAIRS is either a list of two elements:

   (element-1 element-2) -> The data types for each element are defaults from DEFAULT-DATA-TYPE

or a list of element pairs (with optional data types), e.g.:

   (element-pair-1 element-pair-2 ...)

where each element-pair-X may specify a data-type for one, both or neither element of the pair:

   ((element-1 data-type) element-2)
   (element-1 (element-2 data-type))
   ((element-1 data-type) (element-2 data-type))

"
  (let (data-lists label-list actual-x-label actual-y-label)
    (loop for element-pair in (if (consp (car element-pairs)) element-pairs (list element-pairs))
	  for pair-count from 1
	  do (let* ((first-element-ref (if (consp (first element-pair)) (car (first element-pair)) (first element-pair)))
		    (first-element (element first-element-ref))
		    (second-element-ref (if (consp (second element-pair)) (car (second element-pair)) (second element-pair)))
		    (second-element (element second-element-ref)))
	       (if (not (and first-element second-element))
		 (sim-error (format nil "~s does not reference a circuit element" (if first-element second-element-ref first-element-ref)))
		 (let* ((data-type-1 (if (consp (first element-pair)) (cadr (first element-pair)) (DEFAULT-DATA-TYPE first-element)))
			(data-type-2 (if (consp (second element-pair)) (cadr (second element-pair)) (DEFAULT-DATA-TYPE second-element)))
			(data-1 (element-data first-element data-type-1))
			(data-2 (element-data second-element data-type-2))
			(trace-label (format nil "~A ~A vs ~A ~A" (element-name first-element) data-type-1 (element-name second-element) data-type-2)))
		   (when (and data-1 data-2)
		     (if (= pair-count 1)
		       (setq actual-x-label (or x-label (plot-y-label-from-data-type data-type-1))
			     actual-y-label (or y-label (plot-y-label-from-data-type data-type-2)))
		       (setq actual-x-label x-label
			     actual-y-label y-label))
		     (push (list data-1 data-2) data-lists)
		     (push trace-label label-list)))))
	  finally (plot-xy-data
		   data-lists label-list
		   :title (or title (car label-list)) :prompt-for-overlay prompt-for-overlay
		   :x-min x-min :x-max x-max :x-label actual-x-label
		   :y-min y-min :y-max y-max :y-label actual-y-label))))

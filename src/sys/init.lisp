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


;;; SYS Source file: init.lisp

;;; Initialization routines.


(in-package "SURF-HIPPO")

(defun initialize-simulation-vars ()
  ;; Called at the beginning of every simulation by INITIALIZE-SIMULATION, and also by INITIALIZE-GLOBALS-FOR-CIRCUIT and BLANKET-INITIALIZE.
  (when (eq *integration-time-reference* :list) ; *use-time-list*			; If *USE-TIME-LIST* is set, then check a few things.
    ;; There has to have been times stored from a prior simulation, with the same stop time.
    (unless (and *last-sim-reverse-time-list* (= (car *last-sim-reverse-time-list*) *user-stop-time*))
      ;; Other, see if the time steps from the last simulation can be used.
      (if (and *sim-reverse-time-list* (= (car *sim-reverse-time-list*) *user-stop-time*))
	  ;; If so, then set flag to transfer the last simulation's time steps.
	  (setq *auto-refresh-last-sim-reverse-time-list* t)
	  ;; Can't do it, so cancel the use of a time list for this simulation.
	  (setq *integration-time-reference* :variable ; *use-time-list* nil
		))))
  ;; Transfer the time steps from the last simulation for use in this simulation.
  (when *auto-refresh-last-sim-reverse-time-list*
    (setf *last-sim-reverse-time-list* (delete-duplicates *sim-reverse-time-list*))
    (setf *last-sim-reverse-time-step-list* *sim-reverse-time-step-list*)
    (setq *auto-refresh-last-sim-reverse-time-list* nil))
  
  (setf
   *MAX-DV-DIFF-NODES* nil
   *SAVE-DATA-CALLED-P* nil
   *relative-voltage-lte* 0.0  *relative-particle-lte* 0.0  *relative-conc-int-lte* 0.0

   *ARE-THERE-SYNAPSES* (ARE-THERE-SYNAPSES)
   *particles-are-working* (working-particles-p)
   *conc-particles-are-working* (WORKING-CONC-PARTICLES-P)

   *calculate-particle-error* (and *consider-particle-error* (eq *INTEGRATION-TIME-REFERENCE* :variable) ; (not (or *use-time-list* *use-fixed-step*))
				   (or *conc-particles-are-working* *particles-are-working*)
				   *enable-channels*)
   *calculate-conc-int-error* (and *consider-conc-int-error* (eq *INTEGRATION-TIME-REFERENCE* :variable) ; (not (or *use-time-list* *use-fixed-step*))
				   *conc-int-type-list*)


   *synapse-evaluation-times* nil
   *first-time-step* t
   *last-sparse-data-time* nil
   
   *reverse-sparse-data-times* nil  *sim-reverse-time-list* '()  *sim-reverse-time-step-list* '()  *sim-reverse-plot-time-list* '()  *FIXED-TIME-STEPS* '()

   *simulation-initialized* nil

   *total-num-iterations* 0   *total-num-time-points* 0
   *particle-w-max-error* ""  *conc-int-w-max-error* ""
   *particle-ERROR-STEP-less-than-min-step* '()  *particle-error-step-changes* '()  *conc-int-ERROR-STEP-CHANGES* '()  *voltage-error-step-changes* '()

   *lte-was-punted* nil

   *particle-error-max-time-step* 0)

  ;; Breakpoint variables
  
    (setf *breakpoint-list* (when *enable-user-breakpoint-list* *user-breakpoint-list* nil))
    (setf *dynamic-breakpoint-generation* (and *enable-dynamic-breakpoint-generation* (eq *INTEGRATION-TIME-REFERENCE* :variable) ; (not (or *use-time-list* *use-fixed-step*))
						))



  (setq *absolute-voltage-error-internal*  (s-flt *absolute-voltage-error*)
	*absolute-particle-error-internal*  (s-flt *absolute-particle-error*)
	*absolute-conc-int-error-internal*  (s-flt *absolute-conc-int-error*))
  
  (set-stepping-constants)		; Set constants that control time stepping.

  (initialize-time-step)
					;   (update-time-step-variables)
  (clear-element-output-data)

  (setf (*input-time*) 0.0		;maybe this is a good idea for the first step.
	*simulation-max-time* 0.0
	*real-time* 0.0
	*integer-time* 0
	(*fractional-time*) 0.0)
  
  (setq *simulation-actual-time* (get-universal-time))
  (setq *last-show-time-remaining-time* *simulation-actual-time*)

  (when *show-time-remaining* (init-timer-window)))

(defun initialize-time-step ()
  ;; Called by INITIALIZE-SIMULATION-VARS.
  (when *use-fixed-step*
    (format t "*USE-FIXED-STEP* is deprecated - instead specify integration time reference by the variable *INTEGRATION-TIME-REFERENCE*")
    (setq *integration-time-reference* :fixed)) 
  (cond
    ((eq *INTEGRATION-TIME-REFERENCE* :fixed) ; *use-fixed-step*
     (setq *time-step* (round (/ *user-step* *mrt*))
	   *last-time-step* (round (/ *user-step* *mrt*))))
    (t (setf *time-step* *min-step*
	     *last-time-step* *min-step*)))
  nil)

(defun set-stepping-constants ()
  ;; Called by INITIALIZE-SIMULATION-VARS.
  (setq *user-start-time* (coerce *user-start-time* 'single-float)) ; just in case...

  (setf (*t[n+1]*) *user-start-time*
	(*t[n]*) *user-start-time*
	(*t-prime[n+1]*) *user-start-time*
	(*t-prime[n]*) *user-start-time*
	(*t-prime[n-prime-1]*) *user-start-time*
	(*t-prime[n-prime]*) *user-start-time*)

  (setf (*delta-t[n]*) (coerce (- (*t[n+1]*) (*t[n]*)) 'double-float))
  (setf (*delta-t[n-1]*) (*delta-t[n]*))
  (setf (*delta-t-prime[n]*) (coerce (- (*t-prime[n+1]*) (*t-prime[n]*)) 'double-float))
  (setf (*delta-t-prime[n-1]*) (*delta-t-prime[n]*))
  (setq *user-stop-time* (coerce *user-stop-time* 'single-float))
  (when (<= *user-stop-time* *user-start-time*) (sim-error "The *user-stop-time* must be greater than the *user-start-time*."))
  (setf *mrt* (/ (float (+ *extra-time-after-stop-time* *user-stop-time*)) max-integer-time))
  (setf *stop-time* (truncate (/ *user-stop-time* *mrt*))
	*start-time* (truncate (/ *user-start-time* *mrt*)))
  (setf *INT-USER-STOP-TIME* (truncate *user-stop-time*))
  ;; Make sure that *MAX-STEP* and *MIN-STEP* are FIXNUMs.
  (setf	*max-step* (min most-positive-fixnum (truncate (/ (if (> *user-max-step* 0.0) *user-max-step* *user-stop-time*) *mrt*))))
  (setf *min-step* (min most-positive-fixnum (max (truncate (/ *user-min-step* *mrt*)) *MIN-STEP-MRTS*)))
  (setf *USER-MIN-STEP-DOUBLE* (d-flt *USER-MIN-STEP*))
  ;; This ensures that SAVE-DATA will save the data the first time it is called.
  (setq *save-data-step-count* (1- *save-data-step*))
  (setq *sim-time-n+1* *start-time*			
	*sim-time-n* *start-time*
	*sim-time-n-1* *start-time*
	*sim-time-n-2* *start-time*))
 
(defun scrub-and-gc (&key verbose (full t) (gen 0) show-dynamic-space)
  "This function will clean up dead pointers more thouroughly than the basic garbage collection. Explicit calls during large
simulations (temporally or spatially) may avoid excessive disk thrashing or memory faults. ** NOT VERIFIED **
"
  (let ((*GC-ANNOUNCE-text* verbose))
    (system::scrub-control-stack)
    (ext::gc :verbose verbose :full full :gen gen)
    (when show-dynamic-space (vm::instance-usage :dynamic))))

(defun initialize-model-top-pointer-symbols ()
  (loop for model in (models) do (setf (symbol-value (model-top-pointer-symbol model)) nil))
  (setq *electrode* nil))

(defun make-new-circuit-element-model-hash-tables ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (loop for model in (models) unless (model-child-structure-type model) do (setf (model-hash-table model) (make-hash-table :test #'equal))))

(defun make-new-circuit-element-type-model-hash-tables ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (loop for model in (models) when (model-child-structure-type model) do (setf (model-hash-table model) (make-hash-table :test #'equal))))

(defun initialize-physical-environment-variables ()
  (unless *LAST-SIMULATION-TEMPERATURE* (setq *update-temperature-dependent-parameters* t))
  (setq *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*
	*LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*)
  nil)
	
(defun initialize-globals-for-circuit ()
;;  "Initialize simulator to accept a new circuit definition."
  (when *scrub-and-gc-on-global-initialization* (scrub-and-gc))
  (reset-valid-histology-p-in-current-histology-windows)
  (clear-instance-model-instances)
  (when *always-clear-models* (clear-model-instances))
  (when *always-clear-types*
    (clear-model-instances)
    (make-new-circuit-element-type-model-hash-tables)) ; If we keep the element type hash tables intact between circuits, then circuits can
					; directly share the current edition of a type. 
  (make-new-circuit-element-model-hash-tables)
  (global-initialize-time-variables)
  (initialize-plotting-variables)
  (initialize-histology-variables)
  (initialize-simulation-vars)
  (initialize-physical-environment-variables)
  (initialise-nts-variables)
  (initialize-model-top-pointer-symbols) ; *SOMA*, *SEGMENT*, *CHANNEL* etc. etc.
  (clean-up-parameter-lists)		; just to clean things up
  (clear-node-and-matrix-arrays)
  (clear-working-arrays-and-lengths)
  (declare-ground "Ground")
  
  (setq *need-to-reorder-matrix* nil
	*MAX-DV-DIFF-NODES* nil
	*simulation-started* nil *simulation-finished* nil *simulation-in-progress* nil
	*DOCUMENTED-USER-VARIABLES* nil

	*last-lte-node-criterium* nil *particle-w-max-error* "" *conc-int-w-max-error* ""

	*node-voltage-initializations* '() *conc-int-initializations* '() *buffer-initializations* '() *pump-initializations* '()
	*use-node-voltage-initializations* nil
	*find-steady-state* nil
	
	*include-simulation-annotation* nil *simulation-annotation* ""
	
	*enable-channels* t *enable-synapses* t *ARE-THERE-SYNAPSES* nil *eval-all-synapses-every-step* nil
	*setup-voltage-synapses* t *setup-light-synapses* t *setup-event-synapses* t *enable-light* nil *LIGHT-INPUT-WAVEFORM* nil
  
	
	
	*recheck-circuit-elements-parameters* t	*LOADED-CIRCUIT-PARTS* nil *multiple-source-circuit* nil

	*use-simple-names* nil
	*CELL-NAME-SUFFIX* nil
	*cell-simple-name-counter* 0 *cell-element-simple-name-counter* 0 *synapse-simple-name-counter* 0 *channel-simple-name-counter* 0 *particle-simple-name-counter* 0
	*conc-particle-simple-name-counter* 0 *axon-simple-name-counter* 0 *pump-simple-name-counter* 0 *buffer-simple-name-counter* 0 *isource-simple-name-counter* 0
	*vsource-simple-name-counter* 0

	*circuit* "" *simulation-name* "" *circuit-filename* "" *circuit-file* "" *circuit-file-type* nil *input-is-function* nil *circuit-parts* nil
	*circuit-loaded* nil *circuit-processed* nil

	*CALCULATE-ABSOLUTE-LOCATIONS* t 
	*neuron-tree-consolidated* nil

	*make-node-w/elements-array* t *make-needed-v-particle-arrays* nil *make-needed-v-pump-arrays* nil *make-needed-v-buffer-arrays* nil

	*make-segment-lists* t

	*ENABLE-SEGMENT-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-SOMA-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CONC-INTEGRATOR-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-BUFFER-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-PUMP-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CHANNEL-TYPE-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CHANNEL-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-AXON-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-SYNAPSE-MEMBRANE-PARAMETER-UPDATE* t

	*ANALYSIS-NODES* nil *ANALYSIS-NODES-structures* nil

	*all-save-voltage-nodes* nil *all-save-dvdt-nodes* nil *all-save-capacitance-current-nodes* nil *all-save-leak-current-nodes* nil

	*file-output-variable-list* nil
	)
  nil)

(defun igfc () (initialize-globals-for-circuit))

(defun clean-slate ()
  "Initialize simulator to accept a new circuit definition."
  (initialize-globals-for-circuit))

(defun clear-all-ordered-elements ()
  (loop for model in (models)
	when (STRUCTURE-TYPE-SLOT-P (model-name model) :first-element)
	do (maphash #'(lambda (key value) (element-slot value :first-element nil)) (get-hash-table model))
	when (STRUCTURE-TYPE-SLOT-P (model-name model) :next-element)
	do (maphash #'(lambda (key value) (element-slot value :next-element nil)) (get-hash-table model))))

(defun clear-instance-model-instances ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (clear-all-ordered-elements)
  (loop for model in (models) when t	; (model-output-data-enabled model)
	do (loop for data-type in (model-output-data-types model)
		 do (CLEAR-MODEL-SAVE-DATA-INSTANCES model data-type)))
  (loop for name in *circuit-element-type-model-names* do
	(loop for instance being the hash-value of (model-hash-table (element-model name)) ; Note that (get-model-hash-table name) will give the same ref
	      do (typecase instance
		   (channel-type (clear-channels-of-type instance)
				 (remove-channel-type-lists instance))
		   (synapse-type (clear-synapses-of-type instance)
					; (remove-synapse-type-lists instance)
				 )
		   (particle-type (clear-particles-of-type instance))
		   (conc-particle-type (clear-conc-particles-of-type instance))
		   (conc-int-type (setf (conc-int-type-conc-ints instance) nil)
				  (element-parameter instance 'conc-int-array nil))
		   (buffer-type (setf (buffer-type-buffers instance) nil)
				(element-parameter instance 'buffer-array nil))
		   (pump-type (setf (pump-type-pumps instance) nil)
			      (element-parameter instance 'pump-array nil))
		   (axon-type (setf (axon-type-axons instance) nil)
			      (element-parameter instance 'axon-array nil))
		   (cell-type (setf (cell-type-cells instance) nil)
			      (element-parameter instance 'cell-array nil))))
	(setf (model-hash-table (element-model name)) (make-hash-table :test #'equal))))

;; Not called by anybody.
(defun clear-type-model-instances ()
  (loop for name in *circuit-element-type-model-names* do (setf (model-hash-table (element-model name)) (make-hash-table :test #'equal))))

(defun clear-model-instances ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (loop for model in (models) do (setf (model-hash-table model) (make-hash-table :test #'equal))))

(defun global-initialize-time-variables ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (setq *user-breakpoint-list* '()
	*user-start-time* 0.0
	*breakpoint-list* '()
	*last-breakpoint-list* '()
	*last-user-stop-time* 0
	*sparse-data-times* nil
	*reverse-sparse-data-times* nil
  	*SIM-REVERSE-PLOT-TIME-LIST* nil))

(defun initialize-plotting-variables ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (setq *create-new-plot-windows* nil
	*CREATE-NEW-SIMULATION-PLOTS* t
	; *overlay-simulations nil *OVERLAY-ALL-PLOTS* nil
	*PLOT-NODE-ELEMENTS* nil)
  (clear-all-plot-lists))

(defun initialize-histology-variables ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (dolist (win (clean-up-*output-windows*))
    (dolist (slot '(:element-TYPE-GRAPHICS-PARAMETERS
		    :marked-branches-and-colors
		    :marked-segments-and-colors
		    :restrict-axons-to-synapse-types
		    :cell-types
		    :cells
		    :chosen-one
		    :chosen-ones))
      (s-value win slot nil)))

  (setq *histology-comment* ""
	;; nts stuff
	*soma-outline* nil
	*soma-points* nil
	*colorized-windows* nil	*colorize-simulation* nil *enable-sparse-data* nil
	*create-new-histology-window* t *circuit-drawn* nil)
  nil)

(defun clear-node-and-matrix-arrays ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (setq *branch-list* '()  *branch-array* nil
	*last-seg-branch-array* nil *reverse-branch-array* nil *branch-array-limit* nil
	*num-nodes* 0 *num-unknowns* 0

	*diag-double* (make-array '(0) :element-type 'double-float)
	*lower-diag-double* (make-array '(0) :element-type 'double-float)
	*upper-diag-double* (make-array '(0) :element-type 'double-float)
	*v-at-half-step-double* (make-array '(0) :element-type 'double-float)
	*rhs-double* (make-array '(0) :element-type 'double-float)
	*core-node-array* nil *ALL-NODE-ARRAY* nil *node-w/elements-array* nil *node-w/elements-array-length* 0))

(defun clear-working-arrays-and-lengths ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (setq *soma-array* nil

	*segment-guts-list* nil
	*segment-node-2-floats-list*  nil    
	
	*channel-type-list* nil
	*synapse-type-list* nil
	*isource-type-list* nil
	*vsource-type-list* nil
	
	;; 	*USE-PARTICLE-ARRAY* t

	*particle-type-list* nil
	*CONC-INT-TYPE-LIST* nil
	*pump-type-list* nil
	*conc-particle-type-list* nil
	*axon-type-list* nil

	*vsource-array* nil
	*fixed-vsource-array* nil
	*isource-array* nil

	*non-ideal-vsource-list* nil
	*fixed-vsource-list* nil
	*isource-list* nil

	*vsource-array-length* 0
	;;	*fixed-vsource-array-length* 0
	*isource-array-length* 0))

(defun clean-up-parameter-lists ()
  ;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
  (loop for model in (models) do
	(setf (model-parameter-type-library model) (delete-duplicates (model-parameter-type-library model) :from-end t :key 'car :test 'eql))))

(defun blanket-initialize (&optional (kill-the-hippo t))
  ;; Not typically used by (SURF) based simulations. Used on initial startup by INIT-SURF (surf-hippo-loader.lisp).
  (let ((*always-clear-models* t))
    (initialize-window-system-variables)
    (unless kill-the-hippo (update-cool-hippo-window))
    (initialize-globals-for-circuit)
    (make-new-circuit-element-type-model-hash-tables)
    (initialize-simulation-vars)	; For variables that always have to be initialized.
    (get-original-surf-variable-symbols)
    (setq *plot-membrane-voltage-p* t)
    nil))



	    

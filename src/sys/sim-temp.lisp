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


;;; SYS Source file: sim.lisp

;;; Main routines, mostly.

(in-package "SURF-HIPPO")

(defun demo ()
  "Gives a menu of Surf-Hippo demos."
  (setq *circuit-directory* (concatenate-strings *SURF-HOME* "circuits/demos")
	*circuit-source* :FILE)
  (circuit-file-browser)
  (when *circuit-filename*
    (setq *circuit-parts* (list *circuit-filename*))
    (input-*circuit*s *circuit-parts*)))

(defun surf (&optional circuit (automatic *automatic-run*) (load-only *load-only*) (keep-track-of-time-for-auto-run nil))
  "The main simulation function of Surf-Hippo - launches the GUI loop. If there is an optional CIRCUIT, then
it is loaded first (even if there is already a loaded circuit). See also GOFERIT."
  (without-floating-underflow-traps
   (let ((*automatic-run* automatic)
	 (watch-time (or *KILL-ALL-OUTPUT* (and (not keep-track-of-time-for-auto-run) automatic)))
	 *print-pretty*)
     (unless *kill-all-output* (introduce)) ; Just an advertisement.
     (when circuit
       (unless *initialize-on-circuit-load* (setq *initialize-on-circuit-load* (go-ahead-menu "Initialize before next circuit")))
       (let ((*automatic-run* t)) (input-*circuit*s circuit)))
     (unless load-only
       (let ((quit-lisp
	      ;; The main SURF-HIPPO GUI loop.
	      (loop when (main-menu) ; Menus, load &initialize circuit. Return NIL if quitting. This function is run even when menus are disabled (i.e. automatic runs).  
		    do (simulation watch-time)
		    else do (return (YES-OR-NO-P-DEFAULT-NO "Do you want to quit LISP? (RETURN for NO, yes/YES for YES): "))
		    when automatic do (return nil))) ; So an automatic run will only run through this loop once.
	     )
	 (when quit-lisp
	   (sh-cleanup)
	   (system::quit t)))))))

(defun sh-cleanup ()
  (write-system-variables-file)
  )
  
(defun quit-sh () (when t ; (YES-OR-NO-P-DEFAULT-NO "Do you want to quit LISP? (RETURN for NO, yes/YES for YES): ")
		    (sh-cleanup)
		    (system::quit t)))

;; May 22 2002. Under RedHat 7.3 seem to need the RECKLESSLY-P flag - otherwise get
;;    Error in function UNIX::SIGSEGV-HANDLER:  Segmentation Violation at #x4000BB04.

(defun goferit (&optional (stop-time *USER-STOP-TIME*))
  "Simulate the loaded circuit for STOP-TIME [default *USER-STOP-TIME*] milliseconds."
  (let ((*user-stop-time* stop-time)) (surf nil t)))
       
(defun gotimed (&optional (stop-time *USER-STOP-TIME*))
  "As GOFERIT, but keeps track of the simulation run time."
  (let ((*user-stop-time* stop-time)) (surf nil t nil t)))	

(defun goquiet (&optional (stop-time *USER-STOP-TIME*))
  "As GOFERIT, but suppressing text output to Lisp window."
  (let ((*kill-extra-messages* t)) (goferit stop-time)))

(defun auto-surf () (surf nil T))

(export 'surf)

(defun simulation (&optional watch-time)
  (when *circuit-loaded*
    (unless *circuit-processed* (process-circuit-structure))
    (when *find-steady-state* (find-steady-state))
    (when *auto-update-sim-name* (update-simulation-time-stamp-and-name))
    (initialize-simulation))		; Save the initial time point also.
  (unless (or *KILL-ALL-OUTPUT* *kill-extra-messages*)
    (print-circuit (if (equal *simulation-print-detail* :specific_elements) :terse *simulation-print-detail*)))
  (when *circuit-loaded*
    (when *WRITE-LOG-FILE* (update-surf-log-file 'print-circuit))
    ;; SIMULATE!
    (if watch-time (do-time-control) (time (do-time-control)))
    (unless *KILL-ALL-OUTPUT* (sim-output))
    (when *WRITE-LOG-FILE* (update-surf-log-file 'analysis-output))
    (when *beep-after-surf* (inter:beep) (inter:beep))))

(defun make-circuit-element-arrays ()
  (make-needed-v-particle-arrays)
  (make-needed-v-pump-arrays)
  (make-soma-array)
  (make-segment-lists)
  (let ((*make-node-w/elements-array*
	 (and ; (not *use-fixed-step*)
	      (or *make-node-w/elements-array*
		  (not (eq *lte-node-criterium* *last-lte-node-criterium*))
		  *make-segment-lists*))))
    (setq *last-lte-node-criterium*  *lte-node-criterium*)
    (make-node-w/elements-array)))

(defun set-celcius-temperature (new-value)
  "Set the temperature of the simulation to NEW-VALUE degrees celcius, and propagate this value to all the temperature-dependent
elements in the circuit. Returns the new value of *TEMP-CELCIUS* as a single-float."
  (setq *TEMP-celcius* (s-flt new-value))
  (update-temperature t))

(defun update-temperature (&optional force)
  (unless (and *LAST-SIMULATION-TEMPERATURE*
	       *LAST-SIMULATION-TEMP-celcius*
	       (= *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*)
	       (= *LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*))
    (update-temperature-dependent-parameters
     (cond ((and *LAST-SIMULATION-TEMPERATURE*
		 (not (= *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*))) *TEMPERATURE*)
	   ((and *LAST-SIMULATION-TEMP-celcius*
		 (not (= *LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*)) (+ 273.16 *TEMP-celcius*)))
	   (t *TEMPERATURE*))
     force)))

(defun initialize-simulation ()
  ;; Called at the beginning of every simulation. Initialize circuit at the beginning of every simulation.
  (sys::set-floating-point-modes :traps '(:OVERFLOW :INVALID :DIVIDE-BY-ZERO)) ; For insurance!
  (CHECK-FIXED-VOLTAGE-NODES)	  ; and reprocess circuit if necessary
  (set-circuit-elements-parameters t)
  (choose-plot-data)
  (update-temperature)
  (make-circuit-element-arrays)
  (initialize-simulation-vars) ; For variables that always have to be initialized.
  (initialize-node-jacobians) ; Set the constant part of the node jacobians.
  (setup-node-elements)
  ;; Starts the circuit off by voltage clamping all the nodes to the defined resting potential, for example, the leak battery, and
  ;; setup voltage array indexes for the appropriate nodes.  Calls (init-all-nodes) to Clear tri-diag matrix, and initialize all
  ;; the accumulator fields, including (node-jacobian nd), (node-alpha-charge nd) and (node-current nd).
  (init-node-voltages-slots-and-matrix)
  (init-node-elements)			
  (fix-up-off-diags)
  (fix-breakpoint-list)	; Breakpoints include breakpoints of sources (pulses) and start times for event synapses.
  ;; One step vclamp initialization.
  (when (and *steady-state-linear-vclamp-enable*
	     (= (length (vsources)) 1)
	     (attached-to-soma-p *vsource*)
	     (not (vsource-blocked *vsource*)))
    (let ((*real-time* *user-start-time*))
      (init-with-steady-state-linear-voltage-clamp *vsource*)))
  (let ((*real-time* *user-start-time*))
    (save-data t)			; Save the initial time point
    (when (or *enable-sparse-data* *colorize-simulation*)
      (update-sparse-data-and-colorize-simulation *real-time*))) ; And the sparse data.
  (setq *simulation-initialized* t))

(defun setup-node-elements ()
  (when *setup-elements*
    (cond-every
     (*setup-channels* (setup-channels)) ; Must go before setup-particles  and setup-conc-particles 
     (*setup-synapses* (setup-synapses))
     (*setup-isources* (setup-isources))
     (*setup-conc-ints* (setup-conc-ints)) ; Must go after channel and synapse setup
     (*setup-pumps* (setup-pumps))	; Must go after conc-int setup
     (*setup-conc-particles* (setup-conc-particles))
     (*setup-sources* (setup-sources))
     (*setup-axons* (setup-axons))
     (*setup-particles* (setup-particles)))))

(defun init-node-elements ()
  (init-sources)
  (init-axons)				; Clear queued spike times.
  (init-conc-ints)
  (init-pumps)				; This has to go after conc-int initialization.
  (init-synapses)			; Clear queued event times, etc.
  (init-particles)
  (init-conc-particles)
  (init-channels)
  (init-segments)
  (init-somas))

(defun introduce ()
  (unless *automatic-run* (format t "~%~% ** The Surf-Hippo Neuron Simulation System, Version ~A **~%~%" user::Surf-Hippo-Version-Number)))

(defun set-circuit-element-parameters (element)
  (let ((element (element element)))
    (typecase element
      (segment (set-segment-membrane-parameters element))
      (soma (set-soma-membrane-parameters element))
      (conc-int (set-conc-integrator-parameters (element-name element) element))
      (axon (set-axon-parameters (element-name element) element))
      (synapse-type (set-synapse-type-parameters element))
      (synapse (set-synapse-parameters element))
      (channel-type (set-channel-type-parameters element))
      (channel (set-channel-parameters element))
      (cell (update-linear-z-in element)))))

(defun set-circuit-elements-parameters (&optional consider-*recheck-circuit-elements-parameters* circuit-element-to-update)
  (REORDER-MODEL-ELEMENTS-AS-NEEDED)	; Updates elements which have :first-element/:next-element based iteration constructs.
  (if circuit-element-to-update
    (set-circuit-element-parameters circuit-element-to-update)
    (let ((update-temperature-dependent-parameters *update-temperature-dependent-parameters*))
      ;; Because (UPDATE-TEMPERATURE-DEPENDENT-PARAMETERS) resets this, and we look at this later in the function.
      (when *update-temperature-dependent-parameters* (update-temperature-dependent-parameters))
      (when (and *circuit-processed*
		 (or (not consider-*recheck-circuit-elements-parameters*)
		     *recheck-circuit-elements-parameters*))
	(cond-every
	 (*enable-segment-membrane-parameter-update* (set-segments-membrane-parameters t))
	 (*enable-soma-membrane-parameter-update* (set-somas-membrane-parameters t))		
	 (*enable-conc-integrator-membrane-parameter-update* (set-conc-integrators-parameters))
	 ((and (not update-temperature-dependent-parameters) *enable-axon-membrane-parameter-update*) (set-axons-parameters))
	 ((and (not update-temperature-dependent-parameters) *enable-synapse-membrane-parameter-update*) (set-synapse-types-parameters)
	  (set-synapses-parameters t nil nil))
	 ((and (not update-temperature-dependent-parameters) *enable-channel-membrane-parameter-update*)
	  (set-channel-types-parameters)
	  (set-channels-parameters t nil nil)))
	(loop for cell being the hash-value of (CELL-HASH-TABLE) do
	      ;; (when *clear-cell-name-maps* (element-parameter cell 'name-map nil))
	      (update-linear-z-in cell)))
      (when consider-*recheck-circuit-elements-parameters* (setq *recheck-circuit-elements-parameters* nil)))))

(defun queue-internal-breakpoint-times (times)
  (when (eq *INTEGRATION-TIME-REFERENCE* :VARIABLE) (mapcar 'queue-internal-breakpoint-time times))
  nil)

(defun queue-internal-breakpoint-time (time) (push time *breakpoint-list*) nil)

(defun queue-breakpoint-time (time &rest times)
  "For variable step integration, puts TIME [milliseconds] on the queue of break points in *USER-BREAKPOINT-LIST* so that the simulation can be sure to step
there, given *ENABLE-USER-BREAKPOINT-LIST* is T."
  (when (eq *INTEGRATION-TIME-REFERENCE* :VARIABLE)
    (loop for breakpoint-time in (cons time times)
	  do (push breakpoint-time *user-breakpoint-list*)))
  nil)


(defun convert-time-to-mrt-units (time) (round (/ time *mrt*)))
(defun convert-time-to-mrt-units (time)
  (declare ; (optimize (safety 0) (speed 3) (space 1))
	   (single-float time))
  (kernel::sf-sb32-truncate (the sf (/ time *mrt*))))

(defun fix-breakpoint-list ()
  ;; Clean up values in *BREAK-POINT-LIST* and convert them for *MRT-BREAK-POINT-LIST*. 
;  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setq *user-stop-time* (float *user-stop-time*))
  (when (eq *INTEGRATION-TIME-REFERENCE* :VARIABLE) ; (unless (or *use-time-list* *use-fixed-step*)
    (let ((padded-stop-time (+ *extra-time-after-stop-time* *user-stop-time*)))
      (declare (single-float padded-stop-time))
      (push *user-stop-time* *breakpoint-list*)
      ;; just so something is on the list after stop-time ???
      (push padded-stop-time *breakpoint-list*)
      ;; Sorts and removes extra information from *BREAKPOINT-LIST*.
      (unless (equal *last-breakpoint-list* *breakpoint-list*)
	(setq *breakpoint-list* (fast-sort-delete-duplicates *breakpoint-list*))
	(setq *last-breakpoint-list* *breakpoint-list*
	      *last-user-stop-time* *user-stop-time*))
      (setq *MRT-BREAKPOINT-LIST*
	    (loop for val in *breakpoint-list*
		  unless (or (> (the sf val) padded-stop-time)
			     (< (the sf val) 0.0))
		  do (format t "VAL ~A, convert.. ~A~%" val (convert-time-to-mrt-units val))
		  collect (convert-time-to-mrt-units val))))
    nil))






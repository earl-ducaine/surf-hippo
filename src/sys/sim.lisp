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
  "The main user function of Surf-Hippo - launches the GUI loop.
If there is an optional CIRCUIT, then it is loaded first (even if there is already a loaded circuit). See also SIMULATE."
  (without-floating-underflow-traps
   (let ((*automatic-run* automatic)
	 (watch-time (and (not *KILL-ALL-OUTPUT*)
			  keep-track-of-time-for-auto-run
			  ;; (not automatic)
			  ))
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
		    else do (return (YES-OR-NO-P-DEFAULT-NO "Do you want to quit LISP? (Hit RETURN/ENTER for NO, type yes/YES for YES): "))
		    when automatic do (return nil))) ; So an automatic run will only run through this loop once.
	     )
	 (when quit-lisp (quit-sh)))))))

(defun sh-cleanup ()
  (write-system-variables-file)
  )
  
(defun quit-sh () (when t ; (YES-OR-NO-P-DEFAULT-NO "Do you want to quit LISP? (RETURN/ENTER for NO, yes/YES for YES): ")
		    (sh-cleanup)
		    (system::quit t)))

;; May 22 2002. Under RedHat 7.3 seem to need the RECKLESSLY-P flag - otherwise get
;;    Error in function UNIX::SIGSEGV-HANDLER:  Segmentation Violation at #x4000BB04.

(defun run (&optional (stop-time *USER-STOP-TIME*))
  "Run simulation of the loaded circuit for STOP-TIME [default *USER-STOP-TIME*] milliseconds."
  (let ((*user-stop-time* stop-time)) (surf nil t)))

(setf (symbol-function 'goferit) #'run) ; backward compatibility
(setf (documentation 'goferit 'function) "See RUN.")
       
(defun runtimed (&optional (stop-time *USER-STOP-TIME*))
  "As RUN, but keeps track of the simulation run time."
  (let ((*user-stop-time* stop-time)) (surf nil t nil t)))

(setf (symbol-function 'gotimed) #'runtimed) ; backward compatibility
(setf (documentation 'gotimed 'function) "")

(defun runquiet (&optional (stop-time *USER-STOP-TIME*))
  "As RUN, but suppressing text output to Lisp window."
  (let ((*kill-extra-messages* t)) (simulate stop-time)))

(setf (symbol-function 'goquiet) #'runquiet) ; backward compatibility
(setf (documentation 'goquiet 'function) "")

(defun auto-surf () (surf nil T))

(export '(surf run))

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
    (if watch-time (time (do-time-control)) (do-time-control)) 
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
  ;; INIT-NODE-VOLTAGES-SLOTS-AND-MATRIX: Starts the circuit off by
  ;; voltage clamping all the nodes to the defined resting potential,
  ;; for example, the leak battery, and setup voltage array indexes
  ;; for the appropriate nodes.  Calls (init-all-nodes) to clear
  ;; tri-diag matrix, and initialize all the accumulator fields,
  ;; including (node-jacobian nd), (node-alpha-charge nd) and
  ;; (node-current nd).
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

(defun convert-time-to-mrt-units (time) (truncate (/ time *mrt*)))

(defvar *enable-find-breakpoints-for-vsource-waveform* t)
(defvar *find-breakpoints-for-vsource-waveform-max-dv2dt2* 1)
(defun find-breakpoints-for-vsource-waveform (vsource)
  (let* ((array (vsource-waveform-array vsource))
	 (1/dt (vsource-WAVEFORM-TIME-INTERVAL-INVERSE vsource))
	 (max-dv2dt2 (* 1/dt *find-breakpoints-for-vsource-waveform-max-dv2dt2*))
	 (out '())
	 over-threshold-p)
    (when array
      (loop for time from 0 by (/ 1/dt)
	    for index from 0
	    do (let* ((voltage-value-n (aref array index))
		      (voltage-value-n+1 (aref array (1+ index)))
		      (voltage-value-n+2 (aref array (+ 2 index)))
		      (dv2dt2 (* (- (* (- voltage-value-n+2 voltage-value-n+1) 1/dt)
				    (* (- voltage-value-n+1 voltage-value-n) 1/dt))
				 1/dt)))
					; (printvars dv2dt2)
		 (if (> (abs dv2dt2) max-dv2dt2)
		     (unless over-threshold-p (push time out)
			     ;; (printvars dv2dt2 time)
			     (setq over-threshold-p t))
		     (setq over-threshold-p nil)))
		 
	    when (= (+ 2 index) (1- (length array))) do (return out)))))

(defun fix-breakpoint-list ()
  ;; Clean up values in *BREAK-POINT-LIST* and convert them for *MRT-BREAK-POINT-LIST*. 
  (setq *user-stop-time* (s-flt *user-stop-time*)) ; Do this somewhere else?
  (when (eq *INTEGRATION-TIME-REFERENCE* :VARIABLE)
    (let ((padded-stop-time (+ *extra-time-after-stop-time* *user-stop-time*)))
      (declare (single-float padded-stop-time))
      (push *user-stop-time* *breakpoint-list*)
      ;; Just so something is on the list after stop-time ???
      ;; (push padded-stop-time *breakpoint-list*)
      (when *enable-find-breakpoints-for-vsource-waveform*
	(loop for vsrc in (vsources) do
	      (mapcar #'(lambda (bp) (push bp *breakpoint-list*)) (find-breakpoints-for-vsource-waveform vsrc))))
      ;; Sorts and removes extra information from *BREAKPOINT-LIST*.
      (unless (equal *last-breakpoint-list* *breakpoint-list*)
	(setq *breakpoint-list* (s-flt-list (fast-sort-delete-duplicates *breakpoint-list*)))
	(setq *last-breakpoint-list* *breakpoint-list*
	      *last-user-stop-time* *user-stop-time*))
      (setq *MRT-BREAKPOINT-LIST*
	    (loop for val in *breakpoint-list*
		  unless (or (> val padded-stop-time)
			     (<  val 0.0))
		  collect (convert-time-to-mrt-units val))))
    nil))



(proclaim '(inline insert-event-mrt-breakpoint))
(defun insert-event-mrt-breakpoint (event-mrt)
  ;; Insert a breakpoint in mrt units in *MRT-BREAKPOINT-LIST* in the correct order.
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum event-mrt))
  (when *dynamic-breakpoint-generation*
    (if (> (the fn (car *MRT-BREAKPOINT-LIST*)) event-mrt)
	(push event-mrt *MRT-BREAKPOINT-LIST*)
	(if (> event-mrt (last-element *MRT-BREAKPOINT-LIST*))
	    (setq *MRT-BREAKPOINT-LIST* (add-to-end *MRT-BREAKPOINT-LIST* event-mrt))
	    (do ((MRT-BREAKPOINT-LIST *MRT-BREAKPOINT-LIST* (cdr MRT-BREAKPOINT-LIST)))
		((> (the fn (cadr MRT-BREAKPOINT-LIST)) event-mrt)
		 (unless (= (the fn (car MRT-BREAKPOINT-LIST)) event-mrt)
		   (rplacd MRT-BREAKPOINT-LIST (cons event-mrt (cdr MRT-BREAKPOINT-LIST))))))))
    nil))

(proclaim '(inline insert-event-breakpoint))
(defun insert-event-breakpoint (event)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float event))
  (when *dynamic-breakpoint-generation*
    (let ((event-mrt (the fn (truncate (/ event *mrt*)))))
      (insert-event-mrt-breakpoint event-mrt)))
  nil)


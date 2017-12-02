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


;;; SYS Source file: step.lisp
;
; The main iteration control
;

(in-package "SURF-HIPPO")

(defun print-out-time-step-vars (&optional first-iteration-of-step)
  (when (= (*t[n+1]*) (*t[n]*))		; (> *real-time* 11.43)
    (format t "~%#### ~A *real-time* ~A, *time-step* ~A, *last-time-step* ~A~%"
	    (if FIRST-ITERATION-OF-STEP 'FIRST_ITER "  ")
	    *real-time* *time-step* *last-time-step*)
    (format t " *t[n]* ~A, *t[n+1]* ~A, *fractional-time* ~A, *input-time* ~A, *t-prime[n+1]* ~A ~%"
	     (*t[n]*) (*t[n+1]*) (*fractional-time*) (*input-time*) (*t-prime[n+1]*))
    (format t " *t-prime[n]* ~A, *t-prime[n-prime]* ~A, *t-prime[n-prime-1]* ~A ~%"
	    (*t-prime[n]*) (*t-prime[n-prime]*) (*t-prime[n-prime-1]*))
    (format t " *t[n]-t-prime[n-prime]* ~A,  *delta-t[n]* ~A~%"
	    (*t[n]-t-prime[n-prime]*) (*delta-t[n]*))
    (format t "  *delta-t[n]-squared* ~A, *delta-t[n-1]* ~A~%"
	    (*delta-t[n]-squared*) (*delta-t[n-1]*))
    (format t "  *half-delta-t[n-1]* ~A, *delta-t-prime[n]* ~A~%"
	    (*half-delta-t[n-1]*) (*delta-t-prime[n]*))
    (format t " *delta-t-prime[n]-squared* ~A, *half-delta-t-prime[n]* ~A ~%"
	    (*delta-t-prime[n]-squared*) (*half-delta-t-prime[n]*))
    (format t "  *delta-t-prime[n-1]* ~A, *2/delta-t[n]* ~A~%"
	    (*delta-t-prime[n-1]*) (*2/delta-t[n]*))
					;  (format t " *markov-time-step* ~A, *markov-time-step/2* ~A~%"
					;	  (*markov-time-step*) (*markov-time-step/2*))
    ))

(defun print-out-time-step-vars (&optional first-iteration-of-step)
  (when (> *real-time* 11.4)
    (format t "~%#### ~A *real-time* ~A, *time-step* ~A, *last-time-step* ~A~%"
	    (if FIRST-ITERATION-OF-STEP 'FIRST_ITER "  ")
	    *real-time* *time-step* *last-time-step*)
    (format t " *t[n+1]* ~A, *t[n]* ~A, *t-prime[n+1]* ~A ~%"
	    (*t[n+1]*) (*t[n]*) (*t-prime[n+1]*))
    (format t " *t-prime[n]* ~A, *t-prime[n-prime]* ~A, *t-prime[n-prime-1]* ~A ~%"
	    (*t-prime[n]*) (*t-prime[n-prime]*) (*t-prime[n-prime-1]*))

    (format t " *t[n]-t-prime[n-prime]* ~A,  *delta-t[n]* ~A~%"
	    (*t[n]-t-prime[n-prime]*) (*delta-t[n]*))
    
    (format t "  *delta-t[n-1]* ~A, *delta-t-prime[n]* ~A, *half-delta-t[n-1]* ~A~%"
	    (*delta-t[n-1]*) (*delta-t-prime[n]*) (*half-delta-t[n-1]*))
    (format t " *half-delta-t-prime[n]* ~A, *delta-t-prime[n-1]* ~A%" 
	    (*half-delta-t-prime[n]*) (*delta-t-prime[n-1]*))))
	    
;;; Time step main loop in DO-TIME-CONTROL iterates the following variables:
;;; (*sim-time-n-2* *sim-time-n-1* *sim-time-n-1*)
;;; (*sim-time-n-1* *sim-time-n* *sim-time-n*)
;;; (*sim-time-n* *sim-time-n* *sim-time-n+1*)

;;; The iteration loop for each time step begins by calling this function:
(defun update-time-step-variables (&optional (first-iteration-of-step t))
  (declare (optimize (safety 0) (speed 3) (space 0)))

  (when first-iteration-of-step
    (setq *last-real-time* *real-time*)
    (setf (*delta-t[n-1]*) (*delta-t[n]*))
    (setf (*half-delta-t[n-1]*) (* 0.5d0 (*delta-t[n-1]*))) ; Is this necessary??
    (setf (*delta-t-prime[n-1]*) (*delta-t-prime[n]*))
    (setf (*t-prime[n]*) (*t-prime[n+1]*))
    (setf (*t-prime[n-prime-1]*) (*t-prime[n-prime]*))
    (setf (*t[n]*) (the sf (* *mrt* *sim-time-n*))))

  ;; CHANGES WITH ITERATIONS.
  (setf *sim-time-n+1* (the fn (+ *sim-time-n* *time-step*)));
  (setf (*t[n+1]*) (the sf (* *mrt* *sim-time-n+1*)))
  (setf (the sf *real-time*) (*t[n+1]*))
  (setf (the sf *simulation-max-time*) (the sf *real-time*))

  (multiple-value-bind (real-time-integer-part real-time-fractional-part)
      (truncate (the sf (*t[n+1]*)))
    (setf (*fractional-time*) real-time-fractional-part)
    (setq *integer-time* real-time-integer-part))

  (setf (*delta-t[n]*) (* 1.0d0 *mrt* *time-step*))
  (setf (*2/delta-t[n]*) (the df (/ 2.0d0 (*delta-t[n]*))))
  (setf (*delta-t[n]-squared*) (* (*delta-t[n]*) (*delta-t[n]*)))
  (setf (*markov-time-step*) (*DELTA-T[N]*))
  (setf (*markov-time-step/2*) (* 0.5d0 (*markov-time-step*)))

  (setf (*delta-t-prime[n]*) (* 0.5d0 (+ (*delta-t[n-1]*) (*delta-t[n]*))))
  (setf (*delta-t-prime[n]-squared*) (* (*delta-t-prime[n]*) (*delta-t-prime[n]*)))
  (setf (*half-delta-t-prime[n]*) (* 0.5d0 (*DELTA-T-Prime[N]*)))

  (setf (*t-prime[n+1]*) (coerce (+ (*t[n]*) (/ (*delta-t[n]*) 2)) 'single-float))

  (setf (*t-prime[n-prime]*) (coerce (+ (*t-prime[n]*) (*half-delta-t-prime[n]*)) 'single-float))

  (setf (*t[n]-t-prime[n-prime]*) (- (*t[n]*) (*t-prime[n-prime]*)))

  (setf (*input-time*)
	(if *evaluate-inputs-at-midpoint*
	    ;; Evaluate inputs evaluated at the midpoint between the time steps. This is equivalent
	    ;; to the last time + delta-forward.
	    (the sf (* *mrt* 0.5 (+ *time-step* *sim-time-n* *sim-time-n*)))
	    ;; (*t[n]*)
	    ;; Inputs evaluated at the prediction time.
	    (*t[n+1]*)
	    ))

  (incf (the fn *total-num-iterations*))

  (when *are-there-synapses*
    (setq *eval-all-synapses-this-iteration*
	  (or t *eval-all-synapses-every-step* 
	      *eval-all-synapses-this-iteration*
	      (when first-iteration-of-step
		(or (not *synapse-evaluation-times*)
		    (>= (- (*t[n+1]*) (the sf (car *synapse-evaluation-times*)))
			(the sf *synapse-evaluation-step*)))))))
	
  (when *eval-all-synapses-this-iteration* (setq *eval-all-synapses-this-step* t))

  (when *debug-update-time-step-vars* (print-out-time-step-vars first-iteration-of-step))
  
  nil)

(defun setup-*fixed-time-steps* ()
  (let (temp)
    (do* ((last-time (reverse *last-sim-reverse-time-step-list*) (cdr last-time))
	  (this-time (cdr last-time) (cdr this-time)))
	 ((null this-time))
      (push (round (/ (- (car this-time) (car last-time)) *mrt*)) temp))
    (setq *fixed-time-steps* (reverse temp))))

(defun debug-time-message ()
  (format t "Time: ~6f step: ~6f internal time-step: ~a last time-step: ~a input-time ~A~%"
	  (* *sim-time-n* *mrt*) (* *time-step* *mrt*) *time-step* *last-time-step*
	  (*input-time*)))

(defun get-check-break-times ()
  (let ((check-break-time-inc (s-flt (/ *num-nodes-check-break-times* *num-nodes*))))
    (loop for time single-float from 0.0 to (the sf *user-stop-time*)
	  by check-break-time-inc
	  collect time)))

(defun lte-status-message (lte-ok)
  (format t "LTE ~A: ~A~%"
	  (if lte-ok "OK" "not OK")
	  (concatenate-strings (format nil " *relative-voltage-lte* ~4f" *relative-voltage-lte*)
			       (when *calculate-particle-error* (format nil " *relative-particle-lte* ~4f" *relative-particle-lte*))
			       (when *calculate-conc-int-error* (format nil " *relative-conc-int-lte* ~4f" *relative-conc-int-lte*)))))


(defun do-time-control ()
  (declare (optimize (safety 1) (speed 3) (space 0)))

  ;; For all step methods.

  #+mp (when *enable-process-yield* (mp:process-yield))
  
  (unless (or *KILL-ALL-OUTPUT* *kill-extra-messages*) (display-message "Starting transient solution"))
  (when *colorize-simulation* (UNCHOOSE-CHOSEN-ONEs (or *colorized-windows* (opal-obj-exists *standard-graphics-output*))))
  (setq *simulation-in-progress* t
	*simulation-started* t
	*simulation-finished* nil)
  (let ((time-steps (when (eq *integration-time-reference* :list) ; *use-time-list*
		      (reverse *last-sim-reverse-time-step-list*)))
	;; (break-timer-value 0.0) (new-break-timer-value 0.0) (show-time-remaining-times (get-show-time-remaining-times))
	)
    ;; Some initialization when using a prior list of time steps.
    (when (eq *integration-time-reference* :list) ; *use-time-list*
      (setq *time-step* (car time-steps)
	    time-steps (cdr time-steps))
      (setq *last-time-step* (car time-steps)
	    time-steps (cdr time-steps)))

;;    (printvars *mrt-breakpoint-list*)

    ;; Loop over entire simulation time.
    (do* (;(*sim-time-n-2* *sim-time-n-1* *sim-time-n-1*)
	  ;(*sim-time-n-1* *sim-time-n* *sim-time-n*)
	  ;(*sim-time-n* *sim-time-n* *sim-time-n+1*) ; 9/12/95
	  (sim-time-n *sim-time-n* *sim-time-n+1*)
	  ;; (first-time t nil)
	  )
	((or (when (>= *sim-time-n* *stop-time*)
	       ;; (printvars "ready to break (>= *sim-time-n* *stop-time*)" *sim-time-n* *time-step*)
	       t)
		     
	     (when (> (+ *sim-time-n* *time-step*) *stop-time*)
	       ;; (printvars "ready to break (>= (+ *sim-time-n* *time-step*) *stop-time*)" *sim-time-n* *time-step*)
	       t)
	     
	     (when *use-max-iterations* (>= *total-num-iterations* *max-iterations*))
	     (when (eq *integration-time-reference* :list) ; *use-time-list*
	       (null time-steps))))

      ;; (printvars *sim-time-n* *time-step*)

      (setf *sim-time-n-2* *sim-time-n-1*)
      (setf *sim-time-n-1* *sim-time-n*)
      (setf *sim-time-n* *sim-time-n+1*)
      ;; *SIM-TIME-N+1* is updated in inner loop below, since *TIME-STEP* changes there.
      
      (when *break-on-every-step* (unless *debug-time-trace* (debug-time-message)) (break))

      (advance-nodes)			; Advance nodes one time step.

      (advance-elements-w-state)	; Advance elements with state variables (e.g. particles,
					; concentration integrators and voltage sources) or other
					; values which need to be stored from last time step.

      (setq *eval-all-synapses-this-step* nil
	    *eval-all-synapses-this-iteration* nil)

      ;; Loop over single time step.
      (do ((lte-ok nil)
	   (first-iteration-of-step t nil))		
	  (lte-ok)
	(update-time-step-variables first-iteration-of-step)

	;; (printvars "updated-time-step-variables" *sim-time-n* *time-step*)

	(when *debug-time-trace* (debug-time-message) (printvars *mrt-breakpoint-list*))

	;; Now evaluate all the elements and predict the node values for *sim-time-n+1*.
	(let ((hines-step-ok (hines-step first-iteration-of-step)))
	  ;; Estimate the error with CALC-VOLTAGE-LTE-RATIO. This is called even when using a fixed step,
	  ;; since we need the voltage derivatives that CALC-VOLTAGE-LTE-RATIO computes and stores.
	  (calc-VOLTAGE-lte-ratio)

	  (when *debug-all-iterations* (print-node-dv-states))

	  (case *integration-time-reference* ; Choose the next time step depending on the specified method.
	   (:list ; *use-time-list*
	    (setq *last-time-step* *time-step*)
	    (setq *time-step* (car time-steps))
	    (setq time-steps (cdr time-steps))
	    (setq lte-ok t)		; Just to get out of the loop.
	    (when *debug-use-time-list* (format t "Fixed time step - ~A~%" *time-step*)))
	   (:fixed ; *use-fixed-step*
	    (setq lte-ok t)) ; Just to get out of the loop.
	   (:variable ; t				; The LTE method.
	    (setq lte-ok (and hines-step-ok (<= *relative-voltage-lte* 1.0)))
	    (when (or *debug-lte *debug-time-trace*) (lte-status-message lte-ok))
	    (when (and (not lte-ok) (= *time-step* *min-step*))
	      (if *punt-lte-when-min-step-reached*
		(setf lte-ok t		; Error too big, but keep going anyway.
		      *lte-was-punted* (if *lte-was-punted* (1+ *lte-was-punted*) 1))
		(integration-stuck-message)))
	    (cond (lte-ok
		   (when (or *debug-advance* *debug-time-trace*) (format t "   Advance!~%"))
		   (setq *last-time-step* *time-step*)
		   (pick-time-step *sim-time-n+1*)) ; Advance
		  (t
		   (pick-time-step *sim-time-n*)
		   (when *debug-backup*
		     (format t " .... backing up! Backup time ~A, time-step ~A last-time-step ~A hines ~A~%"
			     *real-time* *time-step* *last-time-step*
			     (if hines-step-ok "OK" "NOT OK")))))))))
      ;; (printvars "end of lte loop" *sim-time-n* *time-step*)

      (unless t				; *eval-all-synapses-every-step*
	(when (and *enable-synapses* *use-constant-element-matrix-contribution* *eval-all-synapses-this-step*)
	  (push *real-time* *synapse-evaluation-times*)))

      (unless *eval-conc-ints-in-inner-loop* (eval-all-conc-ints)) ; Evaluate concentration integrators out of the loop.

      (when (or (eq *integration-time-reference* :list) ; *use-time-list*
		(<= *sim-time-n+1* *stop-time*)) ; this avoids points that overstep the stop time.
	(save-data (= *sim-time-n+1* *stop-time*))) ; When time step is successful, save data. Always save last simulation point.

      (when (or *enable-sparse-data* *colorize-simulation*)
	(update-sparse-data-and-colorize-simulation))

      (after-time-step-synapse-cleanup)
      
      (when *debug-at-time-steps* (format t "Time Step State:~%") (print-node-states))
      ;; (setq new-break-timer-value (the sf (get-new-break-timer-value)))

      (cond-every
       (nil				; (< new-break-timer-value break-timer-value)
	(inter::start-interactor (g-value *SIMULATION-TIMER-WINDOW* :break-interactor))	
	(when (g-value *SIMULATION-TIMER-WINDOW* :break) (break))
	(s-value *SIMULATION-TIMER-WINDOW* :break nil)
	(format t "clearing :break...~%")
	;; (inter::stop-interactor (g-value *SIMULATION-TIMER-WINDOW* :break-interactor))
	)

       (t
	(setq *simulation-actual-time* (get-universal-time))
	(when (and *show-time-remaining*
		   (>= (- *simulation-actual-time* *last-show-time-remaining-time*)
		       *time-window-update-time*))
	  (setq *last-show-time-remaining-time* *simulation-actual-time*)
	  (UPDATE-TIMER-WINDOW)))

       )

      ;;      (setq break-timer-value new-break-timer-value)
	
      (incf *total-num-time-points*)
      (setq *first-time-step* nil)
      ;; (printvars "end of loop" *sim-time-n* *time-step*)
      )

    ;; (printvars *sim-time-n* *time-step* *time-step*)

    (cond-every
     (*colorize-simulation* (reCHOOSE-CHOSEN-ONEs (or *colorized-windows* (opal-obj-exists *standard-graphics-output*)))))
    ;; Done with the time steps.
    (setq *sim-plot-time-list* (reverse *sim-reverse-plot-time-list*)
	  *sparse-data-times* (reverse *reverse-sparse-data-times*))
    (when *show-time-remaining* (update-timer-window 0.0 nil))
    (setq *simulation-finished* t
	  *simulation-in-progress* nil
					; *CURRENT-SIMULATION-FINISHED* t
	  )))

(defun current-sim-plot-time-list ()
  "If current simulation is finished, then return *SIM-PLOT-TIME-LIST*, otherwise, return the reverse of the current value of
*SIM-REVERSE-PLOT-TIME-LIST*."
  (if *simulation-finished* ; *CURRENT-SIMULATION-FINISHED*
      *sim-plot-time-list* (reverse *sim-reverse-plot-time-list*)))

(defun current-sparse-data-times ()
    "If current simulation is finished, then return *SPARSE-DATA-TIMES*, otherwise, return the reverse of the current value of
*REVERSE-SPARSE-DATA-TIMES*."
  (if (and *sparse-data-times*
	   *simulation-finished*
	   ; *CURRENT-SIMULATION-FINISHED*
	   ) *sparse-data-times* (reverse *reverse-sparse-data-times*)))

(defun get-new-break-timer-value ()
  (the sf (mod (/ (the fn *total-num-time-points*)
		  (* (the sf *num-nodes-check-break-times*)
		     (the fn *num-nodes*))) 1)))
(defun print-dvs ()
  (dotimes (index (the fn *NODE-W/ELEMENTS-ARRAY-LENGTH*))
    (declare (fixnum index))
    (let ((node (aref (the (simple-array node) *NODE-W/ELEMENTS-ARRAY*) index)))
      (format t "~A d2vdt2 ~a dvdtn ~A dvdtn-1 ~A~%v-n+1 ~A v-n ~A v-n-1 ~A~%" (node-name node)
	      (- (the double-float (node-dvdt-n node))
		 (the double-float (node-dvdt-n-1 node)))
	      (node-dvdt-n node)(node-dvdt-n-1 node)
	      (node-voltage-n+1 node) (node-voltage-n node) (node-voltage-n-1-double node)))))

(defun advance-elements-w-state ()
  ;; Advance elements with state variables (e.g. particles, concentration integrators and voltage sources).
  (advance-axons)
  (advance-particles)
  (ADVANCE-CONC-PARTICLES) 
  (advance-conc-ints)			;  (maphash 'advance-conc-int (CONC-INT-HASH-TABLE))
  (advance-vsources)
					; (advance-synapse-array)
  )

(defun hines-step (first-iteration-of-step)
  ;; Returns T when EVAL-ALL-ELEMENTS returns T, otherwise NIL.
  (declare (optimize (safety 0) (speed 3) (space 0)))

  (setf *element-time-step-maximum* nil)
  (SET-all-SOURCES)
  
  ;; Clear tri-diag matrix, and initialize all the accumulator fields, including (node-jacobian nd), (node-alpha-charge nd) and
  ;; (node-current nd), updates v-indexs. For all the nodes that include gating particles, estimate V(t-prime(n-prime)) from
  ;; V(t(n)) and V'(t(n)), and convert voltage estimate to particle steady state and tau array index.
  (INIT-ALL-NODES)			

  ;; Run all the eval routines for the circuit elements, each of which accumulates the jacobian, alpha-charge, and current entries
  ;; of the appropriate node, and accumulates the near off-diagonal entries of the tri-diag matrix, as appropriate. Evaluate
  ;; particles first.

  (setf (*maximum-particle-error-numerator*) 0.0d0
	(*maximum-conc-int-error-numerator*) 0.0d0
	(*markov-particle-state-delta-s-max-time-step*) -1.0d0)	
  
  (let ((eval-all-elements-ok (EVAL-ALL-ELEMENTS first-iteration-of-step)))

    ;; Solve both implicit and explicit Hines step: Evaluate all nodes: Set the RHS for the core-nodes, solve the matrix, update
    ;; the voltage estimate with the new delta-V's.
    (EVAL-ALL-NODES) 
    (when *debug-all-iterations* (format t "Iteration state:~%") (print-node-states))
    eval-all-elements-ok))

(defun eval-all-elements (&optional (first-iteration-of-step t))
  ;; The FIRST-ITERATION-OF-STEP argument is used by some of the eval routines which need to do some tasks only at
  ;; the first try of a given time step. For example, the EVAL-ALL-CHANNELS and EVAL-ALL-SYNAPSES routines only need to update
  ;; concentration-dependent reversal potentials once per time step since these potentials are functions of the concentrations
  ;; obtained in the previous time step. Returns result of call to PARTICLE-ERROR-OK. Evaluate all element types in a specific
  ;; order, since there are some interdependencies.
  (let ((particle-and-conc-int-ok (not *enable-channels*)))
    
    (eval-fixed-voltage-nodes)		; Eval ideal vsources

    (when *enable-channels*
      (when first-iteration-of-step (eval-all-pumps))
      ;; Else conc-ints evaluated out of inner loop, in DO-TIME-CONTROL.
      (when *eval-conc-ints-in-inner-loop* (eval-all-conc-ints)) 
      (eval-all-conc-particles)
      (eval-all-particles)
      (setq particle-and-conc-int-ok (let ((cint-ok (conc-int-error-ok))
					   (prt-ok (particle-error-ok)))
				       (and cint-ok prt-ok)))
      (eval-all-channels first-iteration-of-step)
      (eval-all-axons first-iteration-of-step))	
    (eval-all-synapses first-iteration-of-step)
    (eval-all-non-ideal-vsources)	; Eval non-ideal vsources
    (eval-all-isources)
    (eval-all-somas)
    (eval-all-segments)
    particle-and-conc-int-ok))

(defun integration-stuck-message ()
  (if (= 0 *user-min-step*)
      (sim-error
       (concatenate
	'string
	(format nil "Integration stuck at time ~ams [internal integer time step = 2]. ~%" (*t[n+1]*))
	"Try either increasing the error criteria "
	(format nil "(*ABSOLUTE-VOLTAGE-ERROR*, which is now ~a),~%" *ABSOLUTE-VOLTAGE-ERROR*)
	(when *calculate-particle-error* (format nil "(or *absolute-PARTICLE-ERROR*, which is now ~a),~%" *absolute-particle-error*))
	(unless *punt-lte-when-min-step-reached* (format nil "setting *PUNT-LTE-WHEN-MIN-STEP-REACHED* to T,~%"))
	"or reducing the simulation duration."))
      (sim-error (format nil "Integration stuck at ~ams, try reducing *USER-MIN-STEP*, which now is ~ams. ~%" (*t[n+1]*) *user-min-step*))))



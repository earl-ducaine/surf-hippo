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


;;; SYS Source file: synapse-evaluation.lisp

(in-package "SURF-HIPPO")


;; ************************************************
;; ************* EVALUATION FUNCTIONS *************
;; ************************************************


;; Only light-dependent, event, or voltage-controlled synapses get evaluated here. Channel driven/dependent synapses do not
;; have a syn. The activity of these synapses are evaluated by their associated channels. 

;; This is used by all the eval synapse routines.
(defmacro finish-syn-eval (syn iv-relation-coeff static-v-dependence conductance-function e-rev-update-enable)
  `(if (= 0.0 (the sf ,iv-relation-coeff))
     (unless (= 0.0d0 (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) 0.0d0))
     (let ((node (synapse-node ,syn)))
       (when ,e-rev-update-enable (setf (synapse-e-rev ,syn) (element-e-rev-from-shells (synapse-conc-ints-params ,syn))))
       (let ((conductance (the df (* (synapse-gbar ,syn) (the sf ,iv-relation-coeff)))))
	 (declare (double-float conductance))
	 (when ,static-v-dependence (setq conductance (* conductance (static-v-dependence-value node ,static-v-dependence))))
	 (when ,conductance-function (setq conductance (* conductance (the sf (funcall (the function ,conductance-function) syn)))))
	 (unless (node-has-ideal-voltage-source node)
	   (let ((node-double-floats (node-double-floats node)))
	     (deccumulate-setf (node-aref-current node-double-floats) (* conductance (synapse-e-rev ,syn)))
	     (accumulate-setf (node-aref-jacobian node-double-floats) conductance)))
	 (unless (= conductance (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) conductance))))))

(defmacro finish-syn-eval-w-decay-factor (syn iv-relation-coeff static-v-dependence conductance-function e-rev-update-enable decay-factor)
  `(let ((node (synapse-node ,syn)))
     (when ,e-rev-update-enable (setf (synapse-e-rev ,syn) (element-e-rev-from-shells (synapse-conc-ints-params ,syn))))
     (let ((conductance (the df (* (+ (* (synapse-conductance ,syn) (the sf ,decay-factor)) (* ,iv-relation-coeff (synapse-gbar ,syn)))))))
       (declare (double-float conductance))
       (when ,static-v-dependence (setq conductance (* conductance (static-v-dependence-value node ,static-v-dependence))))
       (when ,conductance-function (setq conductance (* conductance (the sf (funcall (the function ,conductance-function) syn)))))
       (unless (node-has-ideal-voltage-source node)
	 (let ((node-double-floats (node-double-floats node)))
	   (deccumulate-setf (node-aref-current node-double-floats) (* conductance (synapse-e-rev ,syn)))
	   (accumulate-setf (node-aref-jacobian node-double-floats) conductance)))
       (unless (= conductance (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) conductance)))))
				    
;; This returns a single float. LBG 7/6/99 Original modified from NG code.

;; This macro is evaluated within a loop in EVAL-EVENT-SYNAPSE over the entries in XFRMD-EVENTS, and returns the appropriate value
;; of the conductance wave associated with EVENT-TIME, which is derived from the CAR of XFRMD-EVENTS.

;; If at the current time step EVENT-TIME occurs too late to be relevant, then this form nulls the local XFRMD-EVENTS to stop the
;; loop iteration for this synapse and returns 0.0. If at the *last* time step EVENT-TIME occured too early to still be relevant,
;; then it is removed from the XFRMD-EVENTS slot of the synapse, and this form returns 0.0. If at the current time step EVENT-TIME
;; occured too early to still be relevant, then this form returns 0.0.

;; T-IN-INT and T-IN-FRACT are the integer and fractional parts, respectively, of the simulation time adjusted for the time base
;; of the synapse type conductance wave. Note that EVENT-TIMEs in this form are in units of the waveform interval. For
;; 1st-order-Depressing synapses, each entry in XFRMD-EVENTS is a list of the form (EVENT-TIME WEIGHT).

(defmacro EVENT-SYNAPSE-ACCUMULATER (syn wave delta-wave 
				     wave-input-time-int wave-input-time-fract ; Wave input time integer and fractional parts
				     last-valid-wave-input-time-int ; Last valid wave input time integer part
				     max-wave-index transformed-events rectify-conductance)
  `(let* ((event-time/weight (car ,transformed-events)) ; The next event in the queue.
	  (event-time (the fn (if (consp event-time/weight) (car event-time/weight) event-time/weight))))
    (declare (type (or fixnum cons) event-time/weight) (fixnum event-time))
    
    (if (> event-time ,wave-input-time-int)
	
	;; Time hasn't reached the next event yet. Null the local TRANSFORMED-EVENTS to exit loop, returning 0.0.
	(progn (setq ,transformed-events nil) 0.0)

	;; Now check if EVENT-TIME should be ignored since it referenced the end of the conductance wave the last go around. LAST-VALID-WAVE-INPUT-TIME-INT
	;; is the value of WAVE-INPUT-TIME-INT used in the last synapse type evaluation. This is used to calculate LAST-DELAYED-EFFECTIVE-TIME-INT, which
	;; is an index for the conductance wave.

	(if (and (numberp ,last-valid-wave-input-time-int)
		 (>= (the fn (- (the fn ,last-valid-wave-input-time-int) event-time)) ,max-wave-index))
						       
	    ;; If last valid integration step index overreaches wave, then we are permanently through with this event, and we
	    ;; throw it out and return a value of 0.0.
	    
	    (progn (setf (synapse-transformed-events ,syn) (cdr ,transformed-events)) 0.0)

	    ;; Further process wave. Calculate the wave index DELAYED-EFFECTIVE-TIME-INT, appropriate for the current time step.
	    
	    (let ((delayed-effective-time-int (- ,wave-input-time-int event-time))) ; Starts at 0
	      (declare (fixnum delayed-effective-time-int))
	      (if (>= delayed-effective-time-int ,max-wave-index)

		  ;; If the current index overreaches wave, then we may be permanently through with this event, but we throw it
		  ;; out on the next time step in case a subsequent iteration during the current time step is small enough such
		  ;; that the event is still relevant. For this iteration, ignoring the event means that the returned value of the
		  ;; accumulate is 0.0.
		  0.0

		  ;; Good index value -> Accumulate wave.
		  (let ((interpolated-array-value (interpolated-array-value-with-delta-array
						   ,wave ,delta-wave delayed-effective-time-int ,wave-input-time-fract
						   ,rectify-conductance)))
		    (if (consp event-time/weight) ; Consider weighted synapse g.
			(* (the sf (cdr event-time/weight)) (the sf interpolated-array-value))
			(the sf interpolated-array-value)))))))))

(defmacro EVENT-SYNAPSE-ACCUMULATER-w-decay-factor (syn
						    wave-input-time-int ; Wave input time integer part
						    transformed-events rectify-conductance)
  `(let* ((event-time/weight (car ,transformed-events)) ; The next event in the queue.
	  (event-time (the fn (if (consp event-time/weight) (car event-time/weight) event-time/weight))))
     (declare (type (or fixnum cons) event-time/weight) (fixnum event-time))
    
     (if (> event-time ,wave-input-time-int)
	
       ;; Time hasn't reached the next event yet. Null the local TRANSFORMED-EVENTS to exit loop, returning 0.0.
       (progn (setq ,transformed-events nil) 0.0)

       ;; Process event once, and remove if from the synapse's :TRANSFORMED-EVENTS.

       (progn (setf (synapse-transformed-events ,syn) (cdr ,transformed-events))
	      (if (consp event-time/weight) ; Consider weighted synapse g.
		(the sf (cdr event-time/weight))
		1.0)))))

(proclaim '(inline EVAL-EVENT-SYNAPSE))
(defun EVAL-EVENT-SYNAPSE (syn static-v-dependence conductance-function e-rev-update-enable
			       wave delta-wave max-wave-index wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int rectify-conductance)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (fixnum max-wave-index wave-input-time-int))
  (when (synapse-transformed-events syn)
    (let ((iv-relation-coeff 0.0))
      (declare (single-float iv-relation-coeff))
      ;; Use a DO so that EVENT-SYNAPSE-ACCUMULATER can set TRANSFORMED-EVENTS to NIL in order punt the loop prematurely.
      (do ((transformed-events (synapse-transformed-events syn) (cdr transformed-events)))
	  ((null transformed-events)
	   (finish-syn-eval syn iv-relation-coeff static-v-dependence conductance-function e-rev-update-enable))
	(declare (type (or cons null) transformed-events))  
	(incf iv-relation-coeff (the sf (EVENT-SYNAPSE-ACCUMULATER
					 syn wave delta-wave 
					 wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int
					 max-wave-index transformed-events rectify-conductance)))))
    NIL))

(proclaim '(inline EVAL-EVENT-SYNAPSE-w-decay-factor))
(defun EVAL-EVENT-SYNAPSE-w-decay-factor (syn static-v-dependence conductance-function e-rev-update-enable wave-input-time-int decay-factor rectify-conductance)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (fixnum wave-input-time-int))
  ;; NOT conditional on (synapse-transformed-events syn) since waveforms are always decaying....
  (let ((iv-relation-coeff 0.0))
    (declare (single-float iv-relation-coeff))
    ;; Use a DO so that EVENT-SYNAPSE-ACCUMULATER can set TRANSFORMED-EVENTS to NIL in order punt the loop prematurely.
    (do ((transformed-events (synapse-transformed-events syn) (cdr transformed-events)))
	((null transformed-events)
	 (finish-syn-eval-w-decay-factor syn iv-relation-coeff static-v-dependence conductance-function e-rev-update-enable decay-factor))
      (declare (type (or cons null) transformed-events))  
      (incf iv-relation-coeff (the sf (EVENT-SYNAPSE-ACCUMULATER-w-decay-factor syn wave-input-time-int transformed-events rectify-conductance)))))
    
  NIL)

(proclaim '(inline add-event-to-syn-transformed-events))
(defun add-event-to-syn-transformed-events (syn transformed-event-time event-breakpoints-mrt)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  ;; This is used by EVAL-VOLTAGE-SYNAPSE whenever a new event is detected.
  ;;
  ;; :TRANSFORMED-EVENTS = (1stevt 2ndevt 3rdevt ...)  NG, 3/11/97 rajoute la declaration fixnum apres list.  -
  ;; remplace le NCONC par un RPLACD plus efficace : conse deux fois moins, et est 20% plus rapide - rappel: la
  ;; forme 'list' alloue effectivement une nouvelle liste.
  (let ((listified-delayed-transformed-event-time
	 (list (the fn (+ (synapse-fixnum-delay syn) (the fn transformed-event-time))))))
    (if (synapse-transformed-events syn)
      (rplacd (last (synapse-transformed-events syn)) listified-delayed-transformed-event-time)
      (setf (synapse-transformed-events syn) listified-delayed-transformed-event-time)))
  (when event-breakpoints-mrt
    (let ((delay-mrt (kernel::sf-sb32-truncate (/ (synapse-delay syn) *mrt*))))
      (loop for event-breakpoint-mrt in event-breakpoints-mrt do
	    (insert-event-mrt-breakpoint (+ delay-mrt (the fn event-breakpoint-mrt)))))))

(proclaim '(inline eval-voltage-synapse))
(defun eval-voltage-synapse (syn type ignore-supra-threshold-duration conductance-waveform-event-breakpoints-mrt)
  ;; Determines whether a new event occurs at the current time step for SYN. If so, then the event time is saved in the
  ;; :EVENT-TIMES list, and the transformed event time is saved in the :TRANSFORMED-EVENTS list for SYN and, if SYN is an
  ;; EVENT-GENERATOR, onto all the EVENT-FOLLOWERS associated with SYN. Note that the transformation of events to synaptic
  ;; conductance changes is done by EVAL-EVENT-SYNAPSE.
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  (let ((below-threshold-p (< (node-voltage-n (synapse-pre-synaptic-node syn)) (synapse-type-input-threshold type)))
	(t[n] (*t[n]*)))
    (declare (single-float t[n]))
    ;; Test if either below threshold or within the refractory period of the last event.
    (if (or below-threshold-p
	    (when (synapse-event-times syn)
	      (> (synapse-type-refractory-period type) (- t[n] (the sf (car (synapse-event-times syn)))))))

      ;; Still sub-threshold or in refractory period so update :SUB-THRESHOLD-TIME.
      (setf (synapse-sub-threshold-time syn) t[n])
	
      ;; Fire event if above threshold and if either SYNAPSE-TYPE-SUPRA-THRESHOLD-DURATION-MIN is ignored
      ;; (= 0), or if the current period above threshold is is greater than this time.
      (when (and (not below-threshold-p)
		 (or ignore-supra-threshold-duration
		     (> (the sf (- (*t[n+1]*) (synapse-sub-threshold-time syn)))
			(synapse-type-supra-threshold-duration-min type))))

	;; Push event time, t[n], and reset sub threshold time when threshold conditions are met.
	(push t[n] (synapse-event-times syn))
	(setf (synapse-sub-threshold-time syn) t[n])
	(let ((transformed-event-time (the (SIGNED-BYTE 32) (round (the sf (* t[n] (synapse-type-waveform-time-interval-inverse type))))))
	      (event-breakpoints-mrt
	       (when *dynamic-breakpoint-generation* (cons (convert-time-to-mrt-units t[n]) conductance-waveform-event-breakpoints-mrt))))
	  (if *ENABLE-EVENT-GENERATORS*
	    ;; Add event to all the event-followers of this synapse.
	    (loop for event-follower in (fast-syn-event-followers syn) do
		  (add-event-to-syn-transformed-events event-follower transformed-event-time event-breakpoints-mrt))
	    (add-event-to-syn-transformed-events syn transformed-event-time event-breakpoints-mrt))))))
  nil)

(proclaim '(inline eval-light-synapse))
(defun eval-light-synapse (syn static-v-dependence conductance-function
			       e-rev-update-enable 2dwave 2dwave-length fractional-time=0-OR-do-not-interpolate base-gain-array )
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type (simple-array single-float (*)) base-gain-array))
  (let* ((wave-index (synapse-wave-index syn))
	 (wave-shift (- (the fn *integer-time*) (synapse-wave-shift syn)))
	 (iv-relation-coeff (if (< wave-shift 0)
			      0.0
			      (* (aref base-gain-array wave-index)
				 (interpolated-array-slice-value 2dwave 2dwave-length wave-index wave-shift
								 (*fractional-time*) fractional-time=0-OR-do-not-interpolate)))))
    (declare (single-float iv-relation-coeff))
    (finish-syn-eval syn iv-relation-coeff static-v-dependence conductance-function e-rev-update-enable))
     
  nil)

(proclaim '(inline eval-tonic-synapse))
(defun eval-tonic-synapse (syn static-v-dependence conductance-function e-rev-update-enable)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type boolean e-rev-update-enable))		     
  (finish-syn-eval syn 1.0 static-v-dependence conductance-function e-rev-update-enable)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Eval synapse types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-syn-current-during-eval-p (type)
  (true-p (or (synapse-type-conc-int-type-params type)
	      (let ((element-iv-parameters (synapse-type-iv-parameters type)))
		(when element-iv-parameters
		  (eq (membrane-element-type-iv-relation element-iv-parameters) :constant-field))))))

(defun first-iteration&var-e-rev-p (first-iteration type)
  (true-p (and first-iteration
	       (synapse-type-variable-e-rev type)
	       (synapse-type-conc-int-type-params type))))

(defun eval-all-synapses (&optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when *enable-synapses*
    (loop for type in *synapse-type-list* do
	  (if (synapse-type-evaluation-function type)
	    (funcall (synapse-type-evaluation-function type) type first-iteration)
	    (case (synapse-type-control type)
	      (:tonic (when *eval-all-synapses-this-iteration* (eval-tonic-synapse-type type first-iteration)))
	      (:light (when (and *eval-all-synapses-this-iteration* *enable-light*) (eval-light-synapse-type type first-iteration)))
	      (:light-event (when (and *eval-all-synapses-this-iteration* *enable-light*) (eval-event-synapse-type type first-iteration)))
	      (:event (when *eval-all-synapses-this-iteration* (eval-event-synapse-type type first-iteration)))
	      (:voltage (when (or *eval-all-synapses-this-iteration* first-iteration) (eval-voltage-synapse-type type first-iteration))))))))

(defun eval-tonic-synapse-type (type &optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (conductance-function (get-a-value 'conductance-function params))
	 (first-iteration&var-e-rev (first-iteration&var-e-rev-p first-iteration type)))
    (synapse-type-do
     (syn type)
     (unless (synapse-blocked syn) (eval-tonic-synapse syn static-v-dependence conductance-function first-iteration&var-e-rev)))))

(defvar *interpolate-light-synapse-conductance-waveforms* t)

(defun eval-light-synapse-type (type &optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (conductance-function (get-a-value 'conductance-function params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (fractional-time=0-OR-do-not-interpolate (or (= 0.0 (*fractional-time*)) (not *interpolate-light-synapse-conductance-waveforms*)))
	 (2dwave (get-a-value 'light-responses-ARRAY params))
	 (2dwave-length (array-dimension (the 2dfloat 2dwave) 1))
	 (base-gain-array (get-a-value 'light-inputs-base-gain params)))
    (synapse-type-do
     (syn type)
     (unless (or (synapse-blocked syn) (not (synapse-wave-ref syn)))
       (eval-light-synapse syn static-v-dependence conductance-function
			   e-rev-update-enable 2dwave 2dwave-length fractional-time=0-OR-do-not-interpolate base-gain-array)))))

;; The value for WAVE-INPUT-TIME-FRACTIONAL-PART is common for all event synapses of the same type at a given time step, so this is
;; calculated once and passed on to all the individual evals. This function is applied to both LIGHT-EVENT and EVENT synapse types.

;; NG, 31/10/97
;; - suppression d'appels multiples a synapse-node.
;; - ajout de declaration de types.
;; - suppression de multiple-value-bind pour truncate et optimisation.
;; - suppression de set-element-parameter-fast, qui est trop lent..
;; LBG 7/6/99 Original modified
(defun EVAL-EVENT-SYNAPSE-TYPE (type &optional (first-iteration t) time)
  (declare (optimize (safety 0) (speed 3)
		     (space 1)		; When space-1, avoids 3 notes about EQL.
		     (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (rectify-conductance (or (get-a-value 'rectify-conductance params) *rectify-synapse-conductances*))
	 ;; Future use.
	 ;; (evaluation-method-parameters (get-a-value 'evaluation-method-parameters params))
	 ;; New stuff from NG code
	 ;; For *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS*
	 (decay-factor (when *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* (get-a-value 'CONDUCTANCE-DECAY-FACTOR params)))
	 (wave (get-a-value 'waveform params))
	 (delta-wave (get-a-value 'delta-waveform params))
	 (wave-length (length (the vec-flt wave)))
	 (max-wave-index (1- wave-length))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (conductance-function (get-a-value 'conductance-function params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (last-valid-wave-input-time-int (get-a-value 'last-valid-wave-input-time-integer-part params)))
    (let* ((wave-input-time (* (synapse-type-waveform-time-interval-inverse type) (the sf (or time (*input-time*)))))
	   (wave-input-time-int (ext:truly-the fn (kernel:%unary-truncate wave-input-time)))
	   (wave-input-time-fract (- wave-input-time wave-input-time-int)))
      (declare (fixnum max-wave-index wave-input-time-int) (single-float wave-input-time-fract wave-input-time))
      (let ((*ADVANCE-EVENT-ELEMENTS* (or *ADVANCE-EVENT-ELEMENTS* *first-time-step*)))
	(synapse-type-do
	 (syn type)
	 (unless (synapse-blocked syn)
	   (if decay-factor
	     (EVAL-EVENT-SYNAPSE-w-decay-factor syn static-v-dependence conductance-function
						e-rev-update-enable wave-input-time-int decay-factor rectify-conductance)
	     (EVAL-EVENT-SYNAPSE syn static-v-dependence conductance-function e-rev-update-enable wave delta-wave max-wave-index 
				 wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int rectify-conductance))))
	(set-synapse-type-parameter-slot-fast type 'last-wave-input-time-integer-part wave-input-time-int params))
      nil)))

(defun eval-voltage-synapse-type (type &optional (first-iteration t) time)
  (declare (optimize (safety 0) (speed 3) (space 1) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (rectify-conductance (or (get-a-value 'rectify-conductance params) *rectify-synapse-conductances*))
	 (conductance-waveform (get-a-value 'waveform params))
	 (delta-wave (get-a-value 'delta-waveform params))
	 (conductance-waveform-length (length (the vec-flt conductance-waveform)))
	 (max-wave-index (1- conductance-waveform-length))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (conductance-function (get-a-value 'conductance-function params))
	 (conductance-waveform-event-breakpoints-mrt (get-a-value 'conductance-waveform-event-breakpoints-mrt params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (ignore-supra-threshold-duration (zerop (synapse-type-supra-threshold-duration-min type)))
	 (last-valid-wave-input-time-int (get-a-value 'last-valid-wave-input-time-integer-part params))

	 ;; future use?
	 ;; (evaluation-method-parameters (get-a-value 'evaluation-method-parameters params))
	 
	 ;; New stuff from NG code
	 ;; For *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS*
	 (decay-factor (when *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* (get-a-value 'CONDUCTANCE-DECAY-FACTOR params)))
					; (type-generators (get-a-value 'GENERATORS params))
	 )
    (let* ((wave-input-time (* (synapse-type-waveform-time-interval-inverse type) (the sf (or time (*input-time*)))))
	   (wave-input-time-int (ext:truly-the fn (kernel:%unary-truncate wave-input-time)))
	   (wave-input-time-fract (- wave-input-time wave-input-time-int)))
      (declare (fixnum max-wave-index wave-input-time-int) (single-float wave-input-time-fract wave-input-time))
      (let ((*ADVANCE-EVENT-ELEMENTS* (or *ADVANCE-EVENT-ELEMENTS* *FIRST-TIME-STEP*)))
	(synapse-type-do
	 (syn type)
	 (when (and first-iteration (eq syn (synapse-event-generator syn)))
	   (eval-voltage-synapse syn type ignore-supra-threshold-duration conductance-waveform-event-breakpoints-mrt))
	 (unless (synapse-blocked syn)
	   (when *eval-all-synapses-this-iteration*
	     (if decay-factor
	       (EVAL-EVENT-SYNAPSE-w-decay-factor syn static-v-dependence conductance-function
						  e-rev-update-enable wave-input-time-int decay-factor rectify-conductance)
	       (EVAL-EVENT-SYNAPSE syn static-v-dependence conductance-function
				   e-rev-update-enable conductance-waveform delta-wave max-wave-index
				   wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int rectify-conductance))))))
      ;; 'LAST-WAVE-INPUT-TIME-INTEGER-PART will be used for setting
      ;; 'LAST-VALID-WAVE-INPUT-TIME-INTEGER-PART if the integration step is successful.
      (set-synapse-type-parameter-slot-fast type 'last-wave-input-time-integer-part wave-input-time-int params))))

(defun after-time-step-synapse-cleanup ()
  ;; Called after a successful integration step. 'LAST-VALID-WAVE-INPUT-TIME-INTEGER-PART is used during the next time step for the
  ;; event synapse evaluations to filter out old event times that are no longer valid.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when *enable-synapses*
    (loop for type in *synapse-type-list* do
	  (case (synapse-type-control type)
	    ((:light-event :event)
	     (let* ((params (synapse-type-parameters type))
		    (last-wave-int (get-a-value 'last-wave-input-time-integer-part params))
		    (last-valid-int (get-a-value 'last-valid-wave-input-time-integer-part params)))
	       (when (and last-wave-int last-valid-int ; Make sure these numbers exist.
			  (> (the fn last-wave-int) (the fn last-valid-int))) ; Is this check necessary??
		 (set-element-parameter-fast type 'last-valid-wave-input-time-integer-part last-wave-int params))))))))


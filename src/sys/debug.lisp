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


;;; SYS Source file: debug.lisp
(in-package "SURF-HIPPO")


;; debugging stuff


(defun sh-test (&optional speed-through)
  (circuit-load 'three-hippos) (just-draw) (goferit)
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  (circuit-load 'star-amacrine-ds) (just-draw) (goferit)
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  (circuit-load 'working-hpc-test)(f/i :step 0.1 :start-stimulus 0.1 :stop-stimulus 1 :spike-threshold -50 :plot-gains t)
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  (load-surf-home-file "circuits/demos/colorized-n120.lisp")
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  (load-surf-home-file "circuits/demos/rallpack-3-cc-demo.lisp")
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  (load-surf-home-file "circuits/demos/rallpack-3-vc-demo.lisp")
  (unless speed-through (break "Return from BREAK to CAOWS"))
  (caows)
  )

;; ****************************************
;;
;; Debugging Variables
;;
;; ****************************************

(defvar *break-on-every-step* nil)
(defvar *debug-node-name* "" "Only print this node's state, otherwise all nodes.")
(defvar *test-for-loops* nil)		; When T, (loop-check) is called from process-circuit-structure.
(defvar *debug-backup* nil)
(defvar *debug-advance* nil)
(defvar *debug-use-time-list* nil)
(defvar   *DEBUG-SEGMENTS* nil)
(defvar   *DEBUG-NUMERICAL* nil)
(defvar *debug-diag* nil)
(defvar *debug-hines* nil)
(defvar *debug-time-trace* nil "Print one line of info at each time step.")
(defvar *debug-at-time-steps* nil "Prints out node voltages at every node.")
(defvar *debug-all-iterations*  nil "Print out node voltages at every iteration.")
(defvar *print-matrix* nil  "Print out the matrix at each iteration.")

(defvar *debug-lte nil)
(defvar *debug-consing nil)
(defvar *debug-conc-ints nil)
(defvar *debug-var-e-ca nil)
(defvar  *debug-eval-channel-1 nil)
(defvar *debug-eval-channel-2 nil)
(defvar *debug-eval-channel-3 nil)

(defvar *debug-hines-step nil)
(defvar  *debug-save-data nil)
(defvar *debug-set-sources nil)
(defvar *debug-init-all-nodes nil)
(defvar *debug-eval-all-elements nil)
(defvar *debug-eval-all-nodes nil)
(defvar  *debug-eval-elements nil)
(defvar *DEBUG-EVAL-SEGMENT nil)
(defvar *DEBUG-EVAL-channel nil)
(defvar *DEBUG-EVAL-particle nil)

(defvar *DEBUG-EVAL-soma nil)
(defvar *debug-particle-initial-state* nil)



(defun profile-eval-elements ()
  (profile:unprofile)
  (profile:profile save-total-conductances
		   save-models-output
		   save-plot-time
		   EVAL-ALL-NODES
		   conc-int-error-ok
		   particle-error-ok
		   INIT-ALL-NODES
		   SET-SOURCES
		   calc-VOLTAGE-lte-ratio
		   pick-time-step
		   after-time-step-synapse-cleanup
		   get-universal-time
		   update-time-step-variables
		   advance-elements-w-state
		   advance-nodes
		   ; eval-fixed-voltage-nodes
		   get-vsource-voltage
		   eval-all-conc-ints
		   eval-all-conc-particles
		   eval-all-particles
		   eval-all-pumps
		   eval-all-channels
		   eval-all-axons
		   eval-all-synapses
		   eval-all-non-ideal-vsources
		   eval-all-isources
		   eval-all-somas
		   eval-all-segments)
  (gotimed)
  (profile:report-time)
  (profile:unprofile))

(defun profile-all ()
  "Run a simulation while profiling most of the major functions active during integration."
  (profile::unprofile)
  (profile:profile ; eval-fixed-voltage-nodes
   ; store-element-sparse-data ; update-sparse-data
   UPDATE-SPARSE-DATA-AND-COLORIZE-SIMULATION ; update-sparse-data-fast
		   eval-all-conc-ints
		   eval-all-conc-particles
		   eval-markov-particle-type
		   eval-two-state-particle-type
		   eval-all-pumps
		   particle-error-ok
		   eval-all-channels
		   eval-all-axons
		   eval-light-synapse-type
		   eval-event-synapse-type
		   eval-tonic-synapse-type
		   eval-voltage-synapse-type

		   eval-all-non-ideal-vsources
		   eval-all-isources
		   set-all-isources

		   eval-all-somas
		   eval-all-segments
		   advance-nodes
		   advance-elements-w-state
		   update-time-step-variables
		   save-data
		   update-sparse-data
		   ; push-element-parameter model-save-data-instances ; save-plot-time save-models-output save-total-conductances user-save-data save-time
		   INIT-ALL-NODES
		   set-diag-rhs-floats
		   hines-solve
		   eval-node-floats
		   simulation-output)
  (gotimed)
  (profile::report-time)
  (profile::unprofile))

(defun profile-all ()
  (profile::unprofile)
  (profile:profile    update-sparse-data
		      update-histology-color

		      eval-fixed-node-voltages
		      eval-all-conc-ints
		      eval-all-conc-particles
		      ;; ---------
		      ;; DETAIL DE : eval-all-particles
		      ;; DETAIL DE : eval-markov-particle-type

		      find-markov-particle-steady-state
		      eval-markov-particle

		      v-half-shifted-particle-voltage-index
		      set-markov-rates-and-flows
		      solve-markov-matrix             
		      solve-markov-explicit                   
		      *markov-particle-state-delta-s-max-time-step*
		      set-markov-particle-open-state                  
		      ;; DETAIL DE :finish-eval-particle
		      finish-eval-particle-aref
		      finish-eval-markov-particle

		      eval-two-state-particle-type
		      ;; --------
		   
		      Eval-all-pumps
		      particle-error-ok

		      ;;--------------
		      ;; DETAIL: eval-all-channels
		      eval-all-channels		   

		      ;;                   save-current-during-eval-p  ;; <<<----
		      ;;                   element-e-rev-from-shells
		      ;;                   channel-ohmic-current = Attention. INLINE.
		      ;;                   deccumulate-setf = Macro
		      ;;                   accumulate-setf  = macro
		      ;; ???? conductance-function		   
		      ;; NE PAS PROFILER CA:
		      ;; node-has-ideal-voltage-source = accessor
		      ;; channel-e-rev = macro
		      ;; node-aref-current = macro
		      ;; node-aref-jacobian = macro
		      ;; -------------
		   
		      eval-all-axons
		      eval-light-synapse-type
		      eval-event-synapse-type
		      eval-tonic-synapse-type
		      eval-voltage-synapse-type

		      eval-all-non-ideal-vsources
		      eval-all-isources
		      eval-all-somas
		      eval-all-segments
		      advance-nodes
		      advance-elements-w-state
		      update-time-step-variables
		      ;; DETAIL DE :save-data

		      save-time		   
		      save-plot-time
		      save-models-output ; <- ceci prend du temps
					; a cause de save-node-data

		      ;; save-node-data 
;		      push-soma-voltage-data
;	   
;		      save-soma-data 
;		      save-segment-data 
;		      save-axon-data 
;		      save-particle-data 
;		      save-pump-data 
;		      save-buffer-data 
;		      save-conc-particle-data 
;		      save-conc-int-data 
;		      save-channel-data 
;		      save-synapse-data  
;		      save-isource-data 
;		      save-vsource-data 

		      save-total-conductances
		      user-save-data
		      ;; -----------------------
		      INIT-ALL-NODES
		      set-diag-rhs-floats
		      hines-solve

		      eval-node-floats)
  (gotimed)
  (profile::report-time)
  (profile::unprofile))


(defmacro time-innards (iterations &body body)
  "Prints total execution time for n ITERATIONS of BODY, with local bindings of:

        *KILL-ALL-OUTPUT* T            (for last n-1 iterations)
        *KILL-EXTRA-MESSAGES* T        (for last n-1 iterations)
        *SHOW-TIME-REMAINING* NIL
        *BEEP-AFTER-GC* NIL
        *BEEP-AFTER-SURF* NIL

Writes all results to a .timing file in surf-hippo/logs/.

"
  `(let ((*kill-all-output* nil)
	 (*kill-extra-messages* nil)
	 (*show-time-remaining* nil)
	 (*beep-after-gc* nil)
	 (*beep-after-surf* nil)
	 (*plot-standard-windows* nil)
	 (*plot-custom-windows* nil)
	 (*Trace-Output* t)
	 (dribble-file (wh::DATED-FILEname (concatenate-strings surf::*surf-user-dir* "logs") "timing")))
     (dribble dribble-file)
     (format t "************* TIME-INNARDS *************~%~%")
     (format t "~D iterations of ~A, with output suppressed.~%~%" ,iterations (car (quote ,body)))
     (lisp::print-herald)
     (time (loop for i from 1 to ,iterations
		 do
		 (format t "TIME-INNARDS iteration ~D~%" i)
		 (progn . ,body)
		 (setq *kill-extra-messages* t
		       *kill-all-output* t)))
		 
    (format t "~%")
    (format t "************************************************~%")
    (format t "*****    Done with TIME-INNARDS     ************~%")
    (format t "************************************************~%~%~%~%")
     (dribble)))
     
	 
(defun PRINT-OBJ-SIZE (obj)
  "Finds the space taken by an object."
  (let ((ptr (vm::get-lisp-obj-address obj)))
    (vm::map-allocated-objects
     #'(lambda (obj type size)
	 (declare (fixnum size) (optimize (speed 3) (safety 0))) 
	 (when (= (vm::get-lisp-obj-address obj) ptr)
	   (format T  "obj=~A~%size=~A~%type=~A (~A)~%"
		   obj size (type-of obj) type)
	   (return-from print-obj-size size)
	   ))
     :dynamic)
    (format t "Nothing found~%")
    ))

#| useful 
(vm::instance-usage :dynamic :top-n 100)
(vm::instance-usage :dynamic :top-n 10)

|#

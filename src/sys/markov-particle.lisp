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


;;; SYS Source file: markov-particle.lisp

; Functions specific to Markov particle types

(in-package "SURF-HIPPO")

(defun markov-p (elt)
  (let ((type (element-type elt)))
    (and (particle-type-p type)
	 (eq (particle-type-class type) :markov))))

;; Markov rate functions must setf this array value for their result.
(defvar *markov-rate-array* (make-array '(1) :element-type 'double-float))

(defmacro markov-rate-array-value-with-q10 (prt)
  `(if *ignore-q10*
    (aref (the vec-df *markov-rate-array*) 0)
    (the df (* (aref (the vec-df *markov-rate-array*) 0) (particle-type-q10-rate-factor (particle-type ,prt))))))

(defmacro markov-rate-value-with-q10 (prt value)
  `(if *ignore-q10* ,value (the df (* ,value (particle-type-q10-rate-factor (particle-type ,prt))))))

(defmacro set-markov-rate-array-q10-factor (value) `(setf (aref (the vec-df *markov-rate-array*) 1) ,value))

(defvar *markov-implicit-solve* t)
(defvar *markov-implicit-solve-immediate* t)
(defvar *find-markov-particle-initial-state* :bogus)
(defvar *debug-markov-particle* nil)
(defvar *debug-markov-implicit-solve* nil)

(defvar *markov-max-delta-s* 0.0005)
(defvar *markov-absolute-error* 0.001)
(defvar *max-markov-pred-corr-iterations* 2)
(defvar *markov-particle-state-delta-s-max-time-step* 1.0d0)
(defvar *markov-max-rate-time-step-factor* 0.75d0)
(defvar *markov-quantity-for-error* :rate)

(defvar *markov-integrator-method* nil); :predictor-corrector

(defvar *markov-bogus-state-index* 0)

(defvar *markov-particle-steady-state-max-change* 0.0001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Markov Macros ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For converting lists referencing state symbols and transition functions into a list that references state indices.

;; This is not needed anymore...  For backward compatibility....
(defmacro parse-markov-STATE-TRANSITION-FUNCTIONS (from-to-rate-coeff-list &optional states) from-to-rate-coeff-list)

;; For backward compatibility....
(defmacro MARKOV-STATE-VOLTAGE-TRANSITION-FUNCTIONS (from-to-rate-coeff-list states)
  `(parse-markov-STATE-TRANSITION-FUNCTIONS ,from-to-rate-coeff-list ,states))

(defmacro markov-state-trans-list-from-state (state-transition-list) `(nth 0 ,state-transition-list))
(defmacro markov-state-trans-list-to-state (state-transition-list) `(nth 1 ,state-transition-list))
(defmacro markov-state-trans-list-rate-def (state-transition-list) `(nth 2 ,state-transition-list))
(defmacro markov-state-trans-list-part-arg-p (state-transition-list) `(nth 3 ,state-transition-list))
(defmacro markov-state-trans-list-rate-def-part-arg-p (state-transition-list) `(cddr ,state-transition-list))

(defmacro return-markov-rate (val)
#|
  "This macro wraps around the code for any state transition function with the particle as its arg, where VAL is the double-float
result of the function. The purpose of this macro is to pass the double-float rate to the markov particle evaluation code via the
global *MARKOV-RATE-ARRAY*."
|#
  "This macro wraps around the code for any state transition function with the particle as its arg, where VAL is the real
result of the function. The purpose of this macro is to pass the double-float rate to the markov particle evaluation code via the
global *MARKOV-RATE-ARRAY*."
  `(progn				; (declare (optimize (safety 0) (speed 3) (space 0)))
     ; (setf (aref (the vec-df *markov-rate-array*) 0) ,val)
     (setf (aref (the vec-df *markov-rate-array*) 0) (d-flt ,val))
     nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup and edit functions ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-markov-arrays (type)
  (let ((num-states (particle-type-number-of-states (element-type type))))
    (find-element-array type 'rate-array (list num-states num-states) 'double-float nil t)
    (find-element-array type 'a-array (list num-states num-states) 'double-float nil t)
    (find-element-array type 'k-vector (list num-states) 'double-float nil t)
    (find-element-array type 'p-vector (list num-states) 'fixnum nil t)))

(defun set-markov-particle-type-parameters (type original-parameters)
  (let* ((states (get-a-value 'STATES original-parameters))
	 (number-of-states (length states)))
    (when (get-a-value 'V-HALF-SHIFT-PARTICLE-TYPE original-parameters)
      (unless (and (get-a-value 'VOLTAGE-SHIFTED-STATE-FROM-STATE original-parameters)
		   (get-a-value 'VOLTAGE-SHIFTED-STATE-TO-STATE original-parameters))
	(sim-error (format nil "~A must have from-to shifted state info" type)))
      (element-parameter type 'VOLTAGE-SHIFTED-STATE-FROM-STATE-INDEX
			     (position (get-a-value 'VOLTAGE-SHIFTED-STATE-FROM-STATE original-parameters) states))
      (element-parameter type 'VOLTAGE-SHIFTED-STATE-TO-STATE-INDEX
			     (position (get-a-value 'VOLTAGE-SHIFTED-STATE-TO-STATE original-parameters) states)))
    (setf (particle-type-number-of-states type) number-of-states)
    (setf (particle-type-state-transition-array type) (NIL-2D-ARRAY (make-array (list number-of-states number-of-states))))
    (element-parameter type 'STATE-TRANSITIONS
     (or (get-a-value 'STATE-TRANSITIONS original-parameters)
	 ;; This is for backward compatibility.
	 (get-a-value 'STATE-VOLTAGE-TRANSITION-FUNCTIONS original-parameters)))
    ;; The 'OPEN-STATES entry in the particle type parameter library entry is a list of the open state symbols.
    (setf (particle-type-open-state-array type)
	  (list-to-array (loop for state in states collect (true-p (member state (get-a-value 'OPEN-STATES original-parameters))))))

    (element-parameter type 'OPEN-STATE-INDICES
			   (loop for state in states
				 for index from 0
				 when (member state (get-a-value 'OPEN-STATES original-parameters)) collect index))
    (element-parameter type 'MARKOV-FLOW-ARRAYS
			   (loop for from-state-index from 1 to number-of-states
				 collect (make-array (list number-of-states) :element-type 'df)))
    (element-parameter type 'OPEN-STATES (get-a-value 'OPEN-STATES original-parameters))
    (element-parameter type 'STATES states)
    (element-parameter type 'MARKOV-STATE-LABELS (make-state-labels states))
    (make-markov-arrays type))
  type)

(defun make-state-labels (states) (loop for state in states collect (format nil "STATE ~A" state)))

(defun markov-particle-type-state-voltage-transition-functions-string (type)
  (loop for state-transition-list in (element-parameter type 'STATE-TRANSITIONS)
	for count from 0
	collect (format nil "~A(~s ~s ~s~a)~%"
			(if (= count 0) "" "      ")
			(markov-state-trans-list-from-state state-transition-list)
			(markov-state-trans-list-to-state state-transition-list) 
			(markov-state-trans-list-rate-def state-transition-list)
			(if (markov-state-trans-list-part-arg-p state-transition-list)
			    (format nil " ~s" (markov-state-trans-list-part-arg-p state-transition-list))
			  ""))))

(defun print-markov-particle-type-state-voltage-transition-functions (type)
  (loop for state-transition-list in (element-parameter type 'STATE-TRANSITIONS)
	do (format t "    State ~A -> State ~A "
		   (markov-state-trans-list-from-state state-transition-list) (markov-state-trans-list-to-state state-transition-list))
	(simple-format-list (markov-state-trans-list-rate-def state-transition-list))
	(format t "~%")))

(defun revamp-markov-array-parameters (type)
  (let* ((particle-type-state-transition-array (particle-type-state-transition-array type))
	 (index-limit (the fn (car (array-dimensions particle-type-state-transition-array)))))
    (df-zero-2d-array (ELEMENT-PARAMETER type 'rate-array))
    (element-parameter type 'non-zero-transitions
		       (loop for from-index from 0 to (1- index-limit)
			     nconc (loop for to-index from 0 to (1- index-limit)
					 when (aref particle-type-state-transition-array from-index to-index)
					 collect (list from-index to-index))))))

(defun fill-markov-transition-array (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((trans-array (the (simple-array * (* *)) (particle-type-state-transition-array type)))
	(states (element-parameter type 'STATES)))
    (loop for from-index fixnum from 0 to (1- (array-dimension trans-array 0)) do
	  (loop for to-index fixnum from 0 to (1- (array-dimension trans-array 1)) 
		when (arrayp (aref trans-array from-index to-index)) do (setf (aref trans-array from-index to-index) nil)))
    (loop for state-transition-list in (element-parameter type 'STATE-TRANSITIONS) do
	  (let ((from-state-position (position (markov-state-trans-list-from-state state-transition-list) states))
		(to-state-position (position (markov-state-trans-list-to-state state-transition-list) states)))
	    (unless from-state-position
	      (sim-error (format nil "~A: ~A FROM state in transitions spec not found in particle states!"
				 type (markov-state-trans-list-from-state state-transition-list))))
	    (unless to-state-position
	      (sim-error (format nil "~A: ~A TO state in transitions spec not found in particle states!"
				 type (markov-state-trans-list-to-state state-transition-list))))
	    
	    (setf (aref trans-array from-state-position to-state-position)
		  (let ((rate-def (markov-state-trans-list-rate-def state-transition-list)))
		    (if (markov-state-trans-list-part-arg-p state-transition-list)
			(compile-or-extract-function-or-number rate-def)
			(particle-type-v-function-array type rate-def) ; This function takes care of q10.
			))))) 
    (revamp-markov-array-parameters type)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Evaluation Functions ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bogus-initialize-markov-particle (prt)
  (loop for state-index fixnum from 0 to (nb-states-1 prt) do
	(setf (particle-aref-state-n (prt-state prt state-index))
	      (if (= *markov-bogus-state-index* state-index) 1.0d0 0.0d0)
	      (particle-aref-state-n+1 (prt-state prt state-index))
	      (if (= *markov-bogus-state-index* state-index) 1.0d0 0.0d0)))	      
  (set-markov-particle-open-state prt)
  (when *debug-markov-particle*
    (format t " bogus ini-markov-prt ~A state-n+1 ~f~%" (particle-name prt) (particle-state-n+1-double prt))
    (print-markov-particle-states prt)))

(defun advance-markov-states (prt)
  (loop for state-index fixnum from 0 to (nb-states-1 prt) do
	(let ((state (prt-state prt state-index)))
	  (setf (particle-aref-state-n state) (particle-aref-state-n+1 state)
		(particle-aref-dsdt-n-1 state) (particle-aref-dsdt-n state))))
  nil)

(defun markov-particle-max-dsdt (prt)
  (/ (loop for state-index fixnum from 0 to (nb-states-1 prt)
	   maximizing (abs (- (particle-aref-state-n (prt-state prt state-index))
			      (particle-aref-state-n+1 (prt-state prt state-index)))))
     (*markov-time-step*)))

;; For explicit integration, returns the maximum rate magnitude. The FLOW-ARRAYS and RATE-ARRAY args are used for only
;; the explicit or implicit integration, respectively.
(defmacro set-markov-rates-and-flows-guts-implicit (voltage-index to-state-index from-state-index
								  ;; from-state
								  prt transition-array ; flow-array
								  rate-array
								  v-shift-index v-shift-index-p
								  v-shift-state-from-state-index
								  v-shift-state-to-state-index)
  `(let ((transition (aref (the markov-particle-transition-array ,transition-array) ,from-state-index ,to-state-index)))
     (if transition 
	 (let ((rate (typecase transition
			       (array
				(aref (the vec-df transition)
				      (if (and ,v-shift-index-p
					       (eq (the fn ,v-shift-state-from-state-index) ,from-state-index)
					       (eq (the fn ,v-shift-state-to-state-index) ,to-state-index))
					  (the fn ,v-shift-index)
					,voltage-index)))
			       (number (markov-rate-value-with-q10 ,prt transition))
			       (t	; This function should use RETURN-MARKOV-RATE.
				(funcall (the function transition) ,prt) 
				(markov-rate-array-value-with-q10 ,prt)
				; (aref (the vec-df *markov-rate-array*) 0)
				))))
	   (setf (aref (the 2ddfloat ,rate-array) ,from-state-index ,to-state-index) rate))
       0.0d0)))

(defmacro set-markov-rates-and-flows-guts-explicit (voltage-index to-state-index from-state-index from-state
								  prt transition-array flow-array ; rate-array
								  v-shift-index v-shift-index-p
								  v-shift-state-from-state-index
								  v-shift-state-to-state-index)
  `(let ((transition (aref (the markov-particle-transition-array ,transition-array) ,from-state-index ,to-state-index)))
     (if transition 
	 (let ((rate (typecase transition
			       (array
				(aref (the vec-df transition)
				      (if (and ,v-shift-index-p
					       (eq (the fn ,v-shift-state-from-state-index) ,from-state-index)
					       (eq (the fn ,v-shift-state-to-state-index) ,to-state-index))
					  (the fn ,v-shift-index)
					,voltage-index)))
			       (number (markov-rate-value-with-q10 ,prt transition))
			       (t	; This function should use RETURN-MARKOV-RATE.
				(funcall (the function transition) ,prt)
				(markov-rate-array-value-with-q10 ,prt)
				; (aref (the vec-df *markov-rate-array*) 0)
				))))
	   (when ,from-state
	     (setf (aref (the vec-df ,flow-array) ,to-state-index) (* rate (particle-aref-state-n ,from-state))))
	   (abs rate))

       0.0d0)))

(proclaim '(inline set-markov-rates-and-flows))
(defun set-markov-rates-and-flows (prt transition-array flow-arrays rate-array v-shift-index v-shift-index-p
				       v-shift-state-from-state-index v-shift-state-to-state-index
				       non-zero-transitions-list)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if *markov-implicit-solve*
      (progn
	(if non-zero-transitions-list
	    (loop for non-zero-transitions in non-zero-transitions-list do
		  (set-markov-rates-and-flows-guts-implicit
		   (node-prt-v-index (particle-vnode-point prt))
		   (the ub32 (cadr non-zero-transitions)) ; TO-STATE-INDEX	
		   (the ub32 (car non-zero-transitions)) ; FROM-STATE-INDEX
					; nil
		   prt transition-array ; nil
		   rate-array
		   v-shift-index v-shift-index-p v-shift-state-from-state-index v-shift-state-to-state-index))
	  (loop for to-state-index fixnum from 0 to (nb-states-1 prt) do
		(loop for from-state-index fixnum from 0
		      for flow-array in flow-arrays do
		      (set-markov-rates-and-flows-guts-implicit
		       (node-prt-v-index (particle-vnode-point prt))
		       to-state-index from-state-index
					; nil
		       prt transition-array ; nil
		       rate-array
		       v-shift-index v-shift-index-p v-shift-state-from-state-index v-shift-state-to-state-index))))
	0.0d0)
    (the df
	 (loop for to-state-index fixnum from 0 to (nb-states-1 prt)
	       maximize
	       (loop for from-state-index fixnum from 0 to (nb-states-1 prt)
		     for flow-array in flow-arrays maximize
		     (set-markov-rates-and-flows-guts-explicit
		      (node-prt-v-index (particle-vnode-point prt))
		      to-state-index from-state-index (prt-state prt from-state-index)
		      prt transition-array flow-array ; rate-array
		      v-shift-index v-shift-index-p v-shift-state-from-state-index v-shift-state-to-state-index)
		     into max-rate double-float finally (return max-rate))
	       into max-rate double-float finally (return max-rate)))))

(proclaim '(inline set-markov-particle-open-state)) ;
#|
(defun set-markov-particle-open-state (prt)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (let ((open-states (particle-type-open-state-array (particle-type prt))))
    (setf (particle-state-n+1-double prt)
	  (loop for state-index fixnum from 0 for state in (particle-state-arrays prt)
		when (aref (the (vector boolean) open-states) state-index)
		sum (particle-aref-state-n+1 state) into result double-float
		finally (return result))))
  nil)
|#

(defun set-markov-particle-open-state (prt &optional type-params)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (let ((open-state-indices (get-a-value 'open-state-indices
					 (or type-params (particle-type-parameters (particle-type prt))))))
    (setf (particle-state-n+1-double prt)
	  (loop for state-index in open-state-indices
		sum (particle-aref-state-n+1 (prt-state prt state-index)) into result double-float
		finally (return result))))
  nil)

(defun solve-markov-explicit (flow-arrays prt)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop for to-state-index fixnum from 0 to (nb-states-1 prt) do
	(let* ((to-state (prt-state prt to-state-index))
	       (dsdt (loop for from-state-index fixnum from 0 for from-state in (particle-state-arrays prt)
			   for flow-array in flow-arrays sum
			   (the df (aref (the vec-df flow-array) to-state-index))
			   into in-result double-float
			   when (= from-state-index to-state-index)
			   sum (the df (loop for index fixnum from 0
					     to (nb-states-1 prt)
					     sum (aref (the vec-df flow-array) index) into result double-float
					     finally (return result)))
			   into out-result double-float
			   finally (return (- in-result out-result))))
	       (new-estimate (+ (particle-aref-state-n to-state)
				(* (*markov-time-step*) dsdt))))
	  (setf (particle-aref-state-n+1 to-state) new-estimate)
	  (when *debug-markov-particle*
	    (format t " Setting State-n+1 ~d (State ~A) to ~,6f, with dsdt ~,6f, time-step ~,6f~%"
		    to-state-index 
		    (nth to-state-index (particle-type-parameter (particle-type prt) :STATEs))
		    (particle-aref-state-n+1 to-state) dsdt (*markov-time-step*)))))
  nil)

(defun setup-a-and-k-for-ss (a-array k-vector nb-states-1)
  (declare (optimize (speed 3) (space 0)
		     (compilation-speed 0)
		     (safety 0))
	   (type ub32 nb-states-1)
	   (type 2ddfloat a-array)
	   (type (vector double-float) k-vector))
  (loop for state-index fixnum from 0 to nb-states-1 do
	;; Replace last row with conservation condition for sum of all states 
	(setf (aref A-array nb-states-1 state-index) 1.0d0) 
	;; SS => state derivative = 0
	(setf (aref k-vector state-index) 0.0d0))
  ;; Conservation condition for sum of all states
  (setf (aref k-vector nb-states-1) 1.0d0)
  nil)

(proclaim '(inline setup-a-and-k))
(defun setup-a-and-k (prt rate-array a-array k-vector nb-states-1 initial-state)
  (declare (optimize (speed 3) (space 0)
		     (compilation-speed 0)
		     (safety 0))
	   (type 2ddfloat rate-array a-array)
	   (type (vector double-float) k-vector))
  (if initial-state
      (progn
	(loop for from-state-index fixnum from 0 to nb-states-1 do
	      (loop for to-state-index fixnum from 0 to nb-states-1 do
		    (if (= from-state-index to-state-index)
			(loop for from-state-index-temp fixnum from 0 to nb-states-1
			      unless (= from-state-index-temp from-state-index)
			      sum (- (* (aref rate-array (the ub32 from-state-index-temp) (the ub32 from-state-index))
					(particle-aref-state-n (prt-state prt from-state-index-temp)))
				     (* (aref rate-array (the ub32 from-state-index) (the ub32 from-state-index-temp))
					(particle-aref-state-n (prt-state prt from-state-index))))
			      into k-result double-float
			      and sum (aref rate-array (the ub32 from-state-index) (the ub32 from-state-index-temp))
			      into a-result double-float
			      finally
			      (setf (aref k-vector (the ub32 from-state-index)) k-result
				    (aref a-array (the ub32 from-state-index) (the ub32 to-state-index)) (- a-result)))
		      (setf (aref a-array (the ub32 from-state-index) (the ub32 to-state-index))
			    (aref rate-array (the ub32 to-state-index) (the ub32 from-state-index))))))
	(setup-a-and-k-for-ss a-array k-vector nb-states-1))
    (loop for from-state-index fixnum from 0 to nb-states-1 do
	  (loop for to-state-index fixnum from 0 to nb-states-1 do
		(if (= from-state-index to-state-index)
		    (loop for from-state-index-temp fixnum from 0 to nb-states-1
			  unless (= from-state-index-temp from-state-index)
			  sum (- (* (aref rate-array (the ub32 from-state-index-temp) (the ub32 from-state-index))
				    (particle-aref-state-n (prt-state prt from-state-index-temp)))
				 (* (aref rate-array (the ub32 from-state-index) (the ub32 from-state-index-temp))
				    (particle-aref-state-n (prt-state prt from-state-index))))
			  into k-result double-float
			  and sum (aref rate-array (the ub32 from-state-index) (the ub32 from-state-index-temp))
			  into a-result double-float
			  finally
			  (setf (aref k-vector (the ub32 from-state-index))
				(+ (particle-aref-state-n (prt-state prt from-state-index))
				   (* (*markov-time-step/2*) k-result))
				(aref a-array (the ub32 from-state-index) (the ub32 to-state-index))
				(+ 1 (* (*markov-time-step/2*) a-result))))
		  (setf (aref a-array (the ub32 from-state-index) (the ub32 to-state-index))
			(- (* (*markov-time-step/2*)
			      (aref rate-array (the ub32 to-state-index) (the ub32 from-state-index)))))))))
  nil)


(defun normalize-markov-particle-states-for-initial-state (prt k-vector nb-states-1)
  (declare (optimize (speed 3) (space 0)
		     (compilation-speed 0)
		     (safety 0))
	   (type ub32 nb-states-1)
	   (type (vector double-float) k-vector))
  (let ((normalizer (the df (loop for state-index fixnum from 0 to nb-states-1
				  sum (aref k-vector state-index) into temp double-float finally (return temp)))))
    (loop for state-index fixnum from 0 to (nb-states-1 prt)
	  do (setf (aref k-vector state-index)
		   (the df
		    (or 
		     (nth state-index (or (element-parameter prt 'initial-state)
					  (element-parameter (particle-type prt) 'initial-state)))
		     (/ (aref k-vector state-index) normalizer)))))))

(defvar *enable-markov-matrix-solve* t)
(defvar *enable-markov-setup-a-and-k* t)
(defvar *enable-markov-normalize-initial-state* t)
(proclaim '(inline solve-markov-matrix))
(defun solve-markov-matrix (rate-array prt a-array k-vector p-vector &optional initial-state)
  ;; Implicit method. For initial state, normally we solve the system of nullspace equations with the last one substituted with
  ;; the conservation condition (sum of all states = 1).  The setup for this situation is accompished by SETUP-A-AND-K-FOR-SS,
  ;; called by SETUP-A-AND-K. On the other hand, if the particle or its type has (element-parameter prt 'initial-state), then this
  ;; list of numbers is used to set the initial-state explicitely.
  (declare (optimize (speed 3) (space 0)
		     (compilation-speed 0)
		     (safety 0))
	   (type 2ddfloat rate-array a-array)
	   (type vec-df k-vector))
  (let* ((num-states (particle-type-number-of-states (particle-type prt)))
	 (nb-states-1 (the ub32 (1- (the fixnum num-states)))))
    (setup-a-and-k prt rate-array a-array k-vector nb-states-1 initial-state)
    (when *debug-markov-implicit-solve* (debug-markov-implicit-solve prt rate-array a-array k-vector))
    ;; Result is put into vector k-vector
    (matrix-solve-double-optimized A-array p-vector k-vector num-states)
    (when initial-state
      (normalize-markov-particle-states-for-initial-state prt k-vector nb-states-1))
    (loop for state-index fixnum from 0 to nb-states-1
	  do (setf (particle-aref-state-n+1 (prt-state prt state-index)) (aref K-vector state-index)))
    nil))

(proclaim '(inline set-markov-particle-state-delta-s-max-time-step))
(defun set-markov-particle-state-delta-s-max-time-step (max-rate)
  (declare (optimize (safety 0)
		     (speed 3) (space 0)
		     (compilation-speed 0))
	   (type double-float max-rate))
  (let ((temp-markov-particle-state-delta-s-max-time-step (/ (the df *markov-max-rate-time-step-factor*) max-rate)))
    (when (or (= (*markov-particle-state-delta-s-max-time-step*) -1.0d0)
	      (< temp-markov-particle-state-delta-s-max-time-step (*markov-particle-state-delta-s-max-time-step*)))
      (setf (*markov-particle-state-delta-s-max-time-step*) temp-markov-particle-state-delta-s-max-time-step)))
  nil)

(proclaim '(inline eval-markov-particle))
(defun eval-markov-particle (prt transition-array flow-arrays a-array k-vector p-vector rate-array &optional initial-state
				 v-half-shift-particle-type v-half-shift-magnitude
				 v-shift-state-from-state-index v-shift-state-to-state-index
				 non-zero-transitions-list)
  ;; For N states, FLOW-ARRAYS is a list of N double-float arrays of length N. MAX-RATE is used only for explicit method.
  (declare (optimize ; (safety 0)
		     (speed 3) (space 0)
		     (compilation-speed 0))
	   (type (simple-array * (* *)) transition-array))
  (let* ((v-shift-index
	  (if v-half-shift-particle-type (v-half-shifted-particle-voltage-index prt v-half-shift-magnitude) 0))
	 (v-shift-index-p v-half-shift-particle-type)
	 ;; FILL THE LIST OF FLOW-ARRAYS OR THE RATE-ARRAY, AND FIND THE MAXIMUM RATE IF EXPLICIT METHOD.
	 (max-rate (the df (set-markov-rates-and-flows
			    prt transition-array flow-arrays rate-array
			    v-shift-index v-shift-index-p v-shift-state-from-state-index v-shift-state-to-state-index
			    non-zero-transitions-list))))
    (declare (double-float max-rate))
    (cond
     (*markov-implicit-solve* (solve-markov-matrix rate-array prt a-array k-vector p-vector initial-state))
     (t
      (solve-markov-explicit flow-arrays prt)
      (set-markov-particle-state-delta-s-max-time-step max-rate)))
    (set-markov-particle-open-state prt)
    (when *debug-markov-particle* (debug-markov-particle-comment prt))
    (unless initial-state (when (and *calculate-particle-error* *calculate-markov-particle-error*) (calculate-markov-particle-error prt)))
    ;; (finish-eval-particle prt nil t)
    nil))

(defun calculate-markov-particle-error (prt)
  (loop for state-index from 0 to (nb-states-1 prt) do
	(calculate-particle-error (prt-state prt state-index) (when *debug-particle-error* (format nil "~A state ~A" (particle-name prt) state-index)))))

(defun debug-markov-particle-comment (prt)
  (format t " eval-markov-prt ~A state-n+1 ~f~%" (particle-name prt) (particle-state-n+1-double prt))
  (print-markov-particle-states prt))

(defun adjust-markov-time-step-explicit ()
  (setf (*markov-time-step*) (d-flt (max 0.1d0 (* *particle-error-max-time-step* *mrt*)))))

; (proclaim '(inline find-markov-particle-steady-state))
(defun find-markov-particle-steady-state (prt transition-array flow-array a-array k-vector p-vector rate-array
					      &optional (markov-initial-time-step 0.001d0)
					      v-half-shift-particle-type
					      v-half-shift-magnitude
					      v-shift-state-from-state-index
					      v-shift-state-to-state-index
					      non-zero-transitions-list)
  (declare (optimize
	    (safety 0)
	    (speed 3) (space 0)
	    (compilation-speed 0))
	   (double-float markov-initial-time-step))
  (cond
   (*markov-implicit-solve*
    (setf (*markov-time-step*) markov-initial-time-step)
    (bogus-initialize-markov-particle prt)
    (loop for count fixnum from 1
	  when *debug-markov-implicit-solve* do
	  (format t "~A iteration of ss for ~A - ~A~%" count (particle-name prt) non-zero-transitions-list)
	  do (eval-markov-particle prt transition-array flow-array a-array k-vector p-vector rate-array
				   *MARKOV-IMPLICIT-SOLVE-IMMEDIATE*
				   v-half-shift-particle-type v-half-shift-magnitude
				   v-shift-state-from-state-index v-shift-state-to-state-index
				   non-zero-transitions-list)
	  when (or *MARKOV-IMPLICIT-SOLVE-IMMEDIATE*
		   (< (the df (markov-particle-max-dsdt prt)) (the sf *markov-particle-steady-state-max-change*)))
	  do (return)
	  do
	  (setf (*maximum-particle-error-numerator*) 0.0d0)
	  (finish-eval-particle prt nil t)
	  (particle-error-ok)
	  (adjust-markov-time-step-explicit)
	  (advance-markov-states prt))
    (finish-eval-particle prt t t))
   (t (case *find-markov-particle-initial-state*
	(:bogus (bogus-initialize-markov-particle prt))
	(t (setf (*markov-time-step*) markov-initial-time-step)
	   (loop until (< (the df (markov-particle-max-dsdt prt)) (the sf *markov-particle-steady-state-max-change*))
		 do (eval-markov-particle prt transition-array flow-array a-array k-vector p-vector rate-array nil
					  v-half-shift-particle-type v-half-shift-magnitude
					  v-shift-state-from-state-index v-shift-state-to-state-index
					  non-zero-transitions-list) 
		 (advance-markov-states prt)
		 finally (finish-eval-particle prt t t)))))))

(defun debug-markov-implicit-solve (prt rate-array a k)
  (format t "~A Voltage ~a~%" (particle-name prt)
	  (if (element-parameter prt 'dummy-particle) (element-parameter prt 'voltage) (element-voltage prt)))
  (let ((states (particle-type-parameter (particle-type prt) :STATEs)))
    (loop for from-state-index fixnum from 0 to (nb-states-1 prt) do
	 (loop for from-state-index-temp fixnum from 0 to (nb-states-1 prt) 
	    unless (= from-state-index-temp from-state-index) ; (= 0 (aref rate-array from-state-index-temp from-state-index)) 
	    do
	      (format t " rate ~A(~A)->~A(~A): ~,4e~%"
		      from-state-index-temp (nth from-state-index-temp states)
		      from-state-index  (nth from-state-index states)
		      (aref rate-array from-state-index-temp from-state-index)))))
  (format t "  A matrix:~%")
  (loop for from-state-index fixnum from 0 to (1- (the fn (car (array-dimensions a)))) do
	(format t "~%")
	(loop for to-state-index fixnum from 0 to (1- (the fn (cadr (array-dimensions a)))) do
	      (format t "~,3e " (aref a from-state-index to-state-index))))
  (format t "~%")
  (format t "  K matrix:~%")
  (loop for state-index fixnum from 0 to (1- (the fn (car (array-dimensions k))))
	do (format t "~,3e " (aref k state-index)))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *v-inf-markov-particle-plot-concentration* 0.0d0) ; mM

#|
(progn (profile::unprofile)
       (profile::profile eval-markov-particle-type)
       (let ((prt (car (particles-of-type 'na-x-hpc)))
	     (prt-type (element 'na-x-hpc)))
	 (dotimes (j 20)  
	   (format t "i = ~A~%" j) 
	   (dotimes (i 100)
	     (eval-markov-particle-type prt-type t 0.0d0 prt))))
       (profile::report-time)
       (profile::unprofile))
|#

(defun v-inf-markov-particle-plot-lists (prt-type &optional (concentration *v-inf-markov-particle-plot-concentration*))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((prt-type (element-type prt-type))
	 (dummy-particle (CREATE-PARTICLE nil prt-type nil t))
	 (dummy-node (particle-vnode-point dummy-particle))
	 (nb-voltages (round (/ (- *particle-type-plot-maximum-voltage* *particle-look-up-table-min-voltage*) *particle-look-up-table-precision*)))
	 (nb-states (particle-type-number-of-states prt-type))
	 (inf-array (make-array (list nb-states nb-voltages) :element-type 'double-float))
	 volts)
    (element-parameter dummy-particle 'dummy-particle t)
    (when concentration (element-parameter (particle-concentration-particle dummy-particle) 'concentration (d-flt concentration)))
    (setq volts
	  (loop for voltage from (d-flt *particle-look-up-table-min-voltage*) by *particle-look-up-table-precision*
		for count from 0 to (1- nb-voltages)
		collecting voltage into volts
		do
		(element-parameter dummy-particle 'voltage voltage)
		(setf (node-prt-v-index dummy-node) (VOLTAGE-DOUBLE-TO-VOLTAGE-INDEX voltage))
		(eval-markov-particle-type prt-type t 0.0d0 dummy-particle)
		(loop for state-index fixnum from 0 to (1- nb-states) do
		      (setf (aref inf-array state-index count) (particle-aref-state-n+1 (prt-state dummy-particle state-index))))
		finally (return volts)))
    (erase-element (particle-concentration-particle dummy-particle))
    (erase-element dummy-particle)
    (erase-element dummy-node)
    (loop for state-index fixnum from 0 to (1- nb-states)
	  collect (list volts (loop for count from 0 to (1- nb-voltages) collect (aref inf-array state-index count))))))

(proclaim '(notinline eval-markov-particle-type))
(defun eval-markov-particle-type (prt-type initial-state &optional (markov-initial-dt 0.001d0) eval-only-this-particle)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (particle-type-parameters prt-type))
	 (transition-array (particle-type-state-transition-array prt-type))
	 (non-zero-transitions-list (get-a-value 'non-zero-transitions params))
	 (flow-array (get-a-value 'markov-flow-arrays params))
	 (A-array (get-a-value 'a-array params))
	 (k-vector (get-a-value 'k-vector params))
	 (p-vector (get-a-value 'p-vector params))
	 (rate-array (when *markov-implicit-solve* (get-a-value 'rate-array params)))
	 (v-half-shift-particle-type (get-a-value 'v-half-shift-particle-type params))
	 (v-half-shift-magnitude (when v-half-shift-particle-type (get-a-value 'v-half-shift-magnitude params)))
	 (v-shift-state-from-state-index
	  (when v-half-shift-particle-type (get-a-value 'voltage-shifted-state-from-state-index params)))
	 (v-shift-state-to-state-index
	  (when v-half-shift-particle-type (get-a-value 'voltage-shifted-state-to-state-index params))))
    (when *debug-markov-particle* (format t "non-zero-transitions-list ~A~%" non-zero-transitions-list))
    (if eval-only-this-particle
	(if initial-state
	    (when *find-markov-particle-initial-state*
	      (find-markov-particle-steady-state eval-only-this-particle transition-array flow-array
						 a-array k-vector p-vector
						 rate-array markov-initial-dt
						 v-half-shift-particle-type
						 v-half-shift-magnitude
						 v-shift-state-from-state-index
						 v-shift-state-to-state-index
						 non-zero-transitions-list))
	  (eval-markov-particle eval-only-this-particle transition-array flow-array
				a-array k-vector p-vector rate-array nil
				v-half-shift-particle-type
				v-half-shift-magnitude
				v-shift-state-from-state-index
				v-shift-state-to-state-index
				non-zero-transitions-list))
      (particle-type-iterator
       (prt prt-type)
       unless nil ; (particle-block prt)
       do
       (if initial-state
	   (when *find-markov-particle-initial-state*
	     (find-markov-particle-steady-state prt transition-array flow-array
						a-array k-vector p-vector
						rate-array markov-initial-dt
						v-half-shift-particle-type
						v-half-shift-magnitude
						v-shift-state-from-state-index
						v-shift-state-to-state-index
						non-zero-transitions-list))
	 (eval-markov-particle prt transition-array flow-array
			       a-array k-vector p-vector rate-array nil
			       v-half-shift-particle-type
			       v-half-shift-magnitude
			       v-shift-state-from-state-index
			       v-shift-state-to-state-index
			       non-zero-transitions-list))))
    nil))

;; ***************************************************************
;; INFO STUFF
;; ***************************************************************

(defun print-markov-rates (type voltage)
  (let ((transition-array (particle-type-state-transition-array type))
	(number-of-states (particle-type-number-of-states type)))
    (loop for to-state-index from 0 to (1- number-of-states) do
	  (loop for from-state-index from 0 to (1- number-of-states) do
		(let ((transition (aref transition-array from-state-index to-state-index)))
		  (when transition
		    (format t "From State ~A to State ~A: ~,2e~%"
			    from-state-index to-state-index (if (arrayp transition) (aref transition (voltage-to-voltage-index voltage)) 0.0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plotting functions for Markov particles
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun markov-label-from-index-to-indexs-menu (type)
  (when (element type 'particle-type)
    (let* ((type (element type 'particle-type))
	   (transition-array (particle-type-state-transition-array type))
	   (number-of-states (particle-type-number-of-states type)))
      (choose-list-values-from-keys
       (loop for to-state-index from 0
	     for to-label in (ELEMENT-PARAMETER type 'markov-state-labels) nconc
	     (loop for from-state-index from 0 to (1- number-of-states)
		   for from-label in (ELEMENT-PARAMETER type 'markov-state-labels)
		   when (aref transition-array from-state-index to-state-index)
		   collect (list (format nil "~A to ~A" from-label to-label) (list from-state-index to-state-index))))
       nil :label (format nil "Choose State Transitions for ~A" (particle-type-name type))))))

(defun plot-markov-particle-type (type &key channel-type (what '(:steady_state :tau)) (conc-steps 6) (log-conc-scale t)
				       concentration min-conc max-conc) ; mM
  ;; Calls PLOT-MARKOV-PARTICLE-TYPE-RATES and PLOT-MARKOV-PARTICLE-TYPE-STEADY-STATE
  ;; If CONCENTRATION is set it overrides MIN-CONC and MAX-CONC.
  (let ((dummy1 (if (numberp concentration) concentration (or min-conc (element-parameter type 'plot-min-conc) 1.0e-5)))
	(dummy2 (if (numberp concentration) concentration (or max-conc (element-parameter type 'plot-max-conc) 1.0e0)))
	(dummy3 (or (element-parameter type 'plot-log-conc) log-conc-scale))
	(dummy4 (or (element-parameter type 'plot-conc-steps) conc-steps)))
    (when (element-parameter type 'concentration-particle-type)
      (choose-variable-values
       '((dummy1 "Minimum concentration [mM]" :float) (dummy2 "Maxmum concentration [mM]" :float)
	 (dummy4 "Number of concentration steps" :integer) (dummy3 "Logarithmic steps" :boolean))
       :title (format nil "Concentration parameters for ~A plot" (element-name type))))
    (let* ((step (/ (- (if dummy3 (log dummy2 10) dummy2)
		       (if dummy3 (log dummy1 10) dummy1))
		    (max 1 (1- dummy4))))
	   (start-conc (if dummy3 (log dummy1 10) dummy1))
	   (stop-conc (if dummy3 (log dummy2 10) dummy2))
	   (conc-comment (if (= start-conc stop-conc)
			   (format nil "Concentration: ~,1emM" dummy1)
			   (format nil "Concentration: ~,1e - ~,1emM" dummy1 dummy2)))
	   (from-idx-to-idxs (when (or (member :all what) (member :alpha_&_beta what)) (markov-label-from-index-to-indexs-menu type))))
      (cond-every
       ((or (member :all what) (member :alpha_&_beta what))
	(if (element-parameter type 'concentration-particle-type)
	  (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc) by (if (= start-conc stop-conc) 1 step)
		for count from 1
		do (plot-markov-particle-type-rates type :channel-type channel-type
						    :prompt-for-overlay (= count 1)
						    :from-idx-to-idxs from-idx-to-idxs :separate-plots nil :use-menu nil
						    :concentration (if dummy3 (expt 10.0 i) i))
		finally (add-comment *twin* conc-comment))
	  (plot-markov-particle-type-rates type :channel-type channel-type :separate-plots nil :from-idx-to-idxs from-idx-to-idxs)))
       ((or (member :all what) (member :steady_state what))
	(if (element-parameter type 'concentration-particle-type)
	  (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc) by (if (= start-conc stop-conc) 1 step)
		for count from 1
		do (plot-markov-particle-type-steady-state type :channel-type channel-type
							   :prompt-for-overlay (= count 1)
							   :concentration (if dummy3 (expt 10.0 i) i))
		finally (add-comment *twin* conc-comment))
	  (plot-markov-particle-type-steady-state type :channel-type channel-type)))))
    (when (element-parameter type 'concentration-particle-type)	; Save plot parameters in particle TYPE
      (element-parameter type 'plot-min-conc dummy1)
      (element-parameter type 'plot-max-conc dummy2)
      (element-parameter type 'plot-log-conc dummy3)
      (element-parameter type 'plot-conc-steps dummy4))
    nil))

(defun plot-markov-particle-type-rates (type &key concentration ; mM
					     channel-type from-idx-to-idxs (prompt-for-overlay t) separate-plots use-menu)
  (when (element type 'particle-type)
    (let* ((type (element type 'particle-type))
	   (*v-inf-markov-particle-plot-concentration* (or concentration *v-inf-markov-particle-plot-concentration*))
	   (transition-array (particle-type-state-transition-array type))
	   (number-of-states (particle-type-number-of-states type))
	   (dummy-prt (CREATE-PARTICLE nil type nil t))
	   (dummy-prt-node (particle-vnode-point dummy-prt))
	   (voltages (particle-look-up-voltages))
	   (from-idx-to-idxs (or from-idx-to-idxs (when use-menu (markov-label-from-index-to-indexs-menu type))))
	   rates labels)
      (element-parameter (particle-concentration-particle dummy-prt) 'concentration (d-flt *v-inf-markov-particle-plot-concentration*))
      (loop for to-state-index fixnum from 0
	    for to-label in (ELEMENT-PARAMETER type 'markov-state-labels) do
	    (loop for from-state-index fixnum from 0 to (1- number-of-states)
		  for from-label in (ELEMENT-PARAMETER type 'markov-state-labels)
		  when (member (list from-state-index to-state-index) from-idx-to-idxs :test 'equal)
		  do
		  (let ((transition (aref transition-array from-state-index to-state-index)))
		    (when transition
		      (push (format nil "~a to ~A" from-label to-label) labels)
		      (push
		       (typecase transition
			 (array transition)
			 (number (list-of-nums (length voltages) (markov-rate-value-with-q10 dummy-prt transition) 0.0))
			 (function (loop for voltage in voltages do
					 (element-parameter dummy-prt 'voltage voltage)
					 (setf (node-prt-v-index dummy-prt-node) (VOLTAGE-TO-VOLTAGE-INDEX voltage))
					 (funcall (the function transition) dummy-prt) 
					 collect (markov-rate-array-value-with-q10 dummy-prt))))
		       rates)))))
      (when rates
	(if separate-plots
	  (loop for label in labels for rate in rates do
		(plot-timed-data rate (list label) voltages
				 :title (if channel-type
					  (format nil "~a Channel Particle ~A: ~A Markov Rates"
						  (element-name channel-type) (element-name type) label)
					  (format nil "~a Particle Type: ~A Markov Rates" (element-name type) label))
				 :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
				 :x-are-fns t :y-label-v-position :upper-right :prompt-for-overlay t
				 :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
				 :x-inc 50 :x-origin *particle-type-plot-minimum-voltage*
				 :y-min 0.0 :y-label "1/ms" :x-label "mV"))
	  (plot-timed-data rates labels voltages
			   :title (if channel-type
				    (format nil "~a Channel Particle ~A: Markov Rates"
					    (element-name channel-type) (element-name type))
				    (format nil "~a Particle Type: Markov Rates" (element-name type)))
			   :comment-position :upper-right
			   :comment (when *include-channel-type-comment-in-particle-plots*
				      (channel-type-particle-plot-comment channel-type))
			   :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			   :x-are-fns t :y-label-v-position :upper-right :prompt-for-overlay prompt-for-overlay
			   :overlay (not prompt-for-overlay) :ACCOMODATE-overlays (not prompt-for-overlay)
			   :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
			   :x-inc 50 :x-origin *particle-type-plot-minimum-voltage*
			   :y-min 0.0 :y-label "1/ms" :x-label "mV")))
      (erase-element (particle-concentration-particle dummy-prt))
      (erase-element dummy-prt)
      (erase-element dummy-prt-node))))

(defun plot-markov-particle-type-steady-state (prt-type &key channel-type concentration (prompt-for-overlay t))
  (let ((v-inf-particle-plot-lists (v-inf-markov-particle-plot-lists prt-type concentration)))
    (plot-timed-data
     (loop for volts-infs in v-inf-particle-plot-lists collect (cadr volts-infs))
     (element-parameter prt-type 'markov-state-labels)
     (caar v-inf-particle-plot-lists)
     :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height* :prompt-for-overlay prompt-for-overlay
     :overlay (not prompt-for-overlay) :ACCOMODATE-overlays (not prompt-for-overlay)
     :title (if channel-type
	      (format nil "~a Channel Particle ~A: Markov Steady State" (element-name channel-type) (element-name prt-type))
	      (format nil "~a Particle Type: Markov Steady State" (element-name prt-type)))
     :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
     :x-origin *particle-type-plot-minimum-voltage*
     :x-inc 50.0 :x-origin-tick t :x-label "mV"
     :y-label-v-position :upper-right :x-are-fns t ; :include-y-tick-at-0 nil
     :y-inc 0.25 :y-max 1.0 :y-min 0.0 :y-origin-tick t :y-label "Steady State"
     :comment-position :upper-right
     :comment (when *include-channel-type-comment-in-particle-plots* (channel-type-particle-plot-comment channel-type)))))

(defun plot-markov-tau-ss (prt-type &optional SEPARATE-PLOTS use-menu concentration)
  (when (element prt-type 'particle-type)
    (let* ((type (element prt-type 'particle-type))
	   (concentration (or concentration *v-inf-markov-particle-plot-concentration*))
	   (transition-array (particle-type-state-transition-array type))
	   (number-of-states (particle-type-number-of-states type))
	   (voltages (particle-look-up-voltages))
	   rates labels all-taus all-ss)
      (if use-menu
	(let ((lbl-from-idx-to-idxs
	       (choose-list-values-from-keys
		(loop for to-index fixnum from 0
		      for to-label in (ELEMENT-PARAMETER type 'markov-state-labels) nconc
		      (loop for from-index from to-index to (1- number-of-states)
			    for from-label in (nthcdr to-index (ELEMENT-PARAMETER type 'markov-state-labels))
			    when (or (arrayp (aref transition-array from-index to-index))
				     (arrayp (aref transition-array to-index from-index)))
			    collect (list (format nil "~A <-> ~A" from-label to-label)
					  (list (format nil "~A <-> ~A" from-label to-label)
						from-index to-index))))
		nil :label (format nil "Choose State Transitions for ~A" (particle-type-name type)))))
	  (loop for lbl-from-idx-to-idx in lbl-from-idx-to-idxs 
		do (push (car lbl-from-idx-to-idx) labels)
		(push (list
		       (aref transition-array (nth 1 lbl-from-idx-to-idx) (nth 2 lbl-from-idx-to-idx))
		       (aref transition-array (nth 2 lbl-from-idx-to-idx) (nth 1 lbl-from-idx-to-idx)))
		      rates)))
	(loop for to-index fixnum from 0
	      for to-label in (ELEMENT-PARAMETER type 'markov-state-labels) nconc
	      (loop for from-index from to-index to (1- number-of-states)
		    for from-label in (nthcdr to-index (ELEMENT-PARAMETER type 'markov-state-labels))
		    when (or (arrayp (aref transition-array from-index to-index))
			     (arrayp (aref transition-array to-index from-index)))
		    do (push (format nil "~A <-> ~A" from-label to-label) labels)
		    (push (list (aref transition-array from-index to-index)
				(aref transition-array to-index from-index)) rates))))
      (loop for label in labels for rate in rates do
	    (let* ((forward-rates (array-to-list (car rate)))
		   (backward-rates (array-to-list (cadr rate)))
		   (taus (loop for forward-rate in forward-rates 
			       for backward-rate in backward-rates
			       collect (/ 1.0 (+ forward-rate backward-rate))))
		   (ss (loop for forward-rate in forward-rates 
			     for backward-rate in backward-rates
			     collect (/ forward-rate (+ forward-rate backward-rate)))))
	      (cond (separate-plots
		     (plot-timed-data ss (list label) voltages
				      :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
				      :x-are-fns t
				      :y-label "SS" :x-label "mV"
				      :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
				      :x-origin *particle-type-plot-minimum-voltage*
				      :y-min 0.0 :y-max 1.0
				      :title (format nil "~A - ~A SS" (particle-type-name type) label))
		     (plot-timed-data taus (list label) voltages
				      :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
				      :x-are-fns t
				      :y-label "ms" :x-label "mV"
				      :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
				      :x-origin *particle-type-plot-minimum-voltage*
				      :y-min 0.0 
				      :title (format nil "~A - ~A Tau" (particle-type-name type) label)))
		    (t (push taus all-taus)
		       (push ss all-ss))))
	    finally
	    (unless separate-plots
	      (plot-timed-data all-ss (reverse labels) voltages
			       :prompt-for-overlay t
			       :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			       :y-label-v-position :upper-right
			       :x-are-fns t
			       :y-label "SS" :x-label "mV"
			       :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
			       :x-origin *particle-type-plot-minimum-voltage*
			       :y-min 0.0 :y-max 1.0
			       :title (format nil "~A SS" (particle-type-name type)))

	      (plot-timed-data all-taus (reverse labels) voltages
			       :prompt-for-overlay t :y-label-v-position :upper-right
			       :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			       :x-are-fns t
			       :y-label "ms" :x-label "mV"
			       :x-min *particle-type-plot-minimum-voltage* :x-max *particle-type-plot-maximum-voltage*
			       :x-origin *particle-type-plot-minimum-voltage*
			       :y-min 0.0 
			       :title (format nil "~A Tau" (particle-type-name type))))))))
				
(defun print-markov-particle-states (prt)
  (let ((prt (element prt))
	(states (particle-type-parameter (particle-type prt) :STATEs)))
    (format t "Markov Particle ~A:~%" (particle-name prt))
    (loop for state-index fixnum from 0 to (nb-states-1 prt) do
	  (format t "  State ~A (~A): n ~a n+1 ~a~%"
		  state-index
		  (nth state-index states)
		  (particle-aref-state-n (prt-state prt state-index))
		  (particle-aref-state-n+1 (prt-state prt state-index))))))

(defun extract-markov-transition-function (markov-particle-type from-state to-state)
  (let* ((type (element markov-particle-type))
	 (trans-array (ELEMENT-PARAMETER type 'STATE-TRANSITIONS))
	 (states (ELEMENT-PARAMETER type 'states)))
    (loop for from-to-function in trans-array
	  when (and (eq (first from-to-function) (position from-state states))
		    (eq (second from-to-function) (position to-state states)))
	  do (return (third from-to-function)))))

(defun extract-markov-transition-function (markov-particle-type from-state to-state)
  (let* ((type (element markov-particle-type))
	 (states (ELEMENT-PARAMETER type 'states)))
    (loop for state-transition-list in (ELEMENT-PARAMETER type 'STATE-TRANSITIONS)
	  when (and (eq (markov-state-trans-list-from-state state-transition-list)
			(position from-state states))
		    (eq (markov-state-trans-list-to-state state-transition-list)
			(position to-state states)))
	  do (return (markov-state-trans-list-rate-def state-transition-list)))))

(defun edit-markov-transition-functions (type)
  (let* ((type (element type))
	 (key-functions
	  (loop for state-transition-list in (ELEMENT-PARAMETER type 'STATE-TRANSITIONS)
		when (consp (markov-state-trans-list-rate-def state-transition-list))
		collect (list (format nil "~A to ~A"
				      (markov-state-trans-list-from-state state-transition-list)
				      (markov-state-trans-list-to-state state-transition-list))
			      state-transition-list)))
	 (edited-from-to-functions
	  (loop for from-to-function in (choose-list-values-from-keys key-functions nil :label "Choose Markov Transition Functions to Edit")
		collect
		(list (first from-to-function)
		      (second from-to-function)
		      (setup-edited-function (third from-to-function)
					     :extra-text
					     (format nil "~A: Edit ~A to ~A"
						     (particle-type-name type)
						     (state-label-in-markov-states type (markov-state-trans-list-from-state from-to-function))
						     (state-label-in-markov-states type (markov-state-trans-list-to-state from-to-function))))))))
    (element-parameter
     type 'STATE-TRANSITIONS 
     (delete-duplicates (concatenate 'list (ELEMENT-PARAMETER type 'STATE-TRANSITIONS) edited-from-to-functions) :test 'TEST-TOP-TWO-OF-LISTS))
    (make-v-particle-arrays type)))

(defun state-index-in-markov-states (type state)
  (let* ((type (element type))
	 (states (ELEMENT-PARAMETER type 'states)))
    (when type (position state states))))

(defun state-label-in-markov-states (type state)
  (let* ((type (element type))
	 (labels (ELEMENT-PARAMETER type 'markov-state-labels)))
    (when type (nth (state-index-in-markov-states type state) labels))))

(defun find-markov-particle-state-data (particle &optional state-index)
  (let* ((particle (element particle 'particle))
	 (type (element-type particle))
	 (state-index (or state-index (state-index-in-markov-states type (nth 0 (element-parameter type 'open-states))))))
    (when particle
      (reverse
       (typecase state-index
	 (fixnum (element-parameter particle (nth state-index (element-parameter (particle-type particle) 'markov-state-labels))))
	 (symbol (element-parameter particle state-index)))))))

(defun clear-markov-particle-state-data (particle &optional state-index)
  (let* ((particle (element particle 'particle))
	 (type (element-type particle))
	 (state-index (or state-index (state-index-in-markov-states type (nth 0 (element-parameter type 'open-states))))))
    (when particle
      (reverse
       (typecase state-index
	 (fixnum (element-parameter particle (nth state-index (element-parameter (particle-type particle) 'markov-state-labels nil))))
	 (symbol (element-parameter particle state-index nil)))))))


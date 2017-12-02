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



;;; SYS Source file: axon.lisp
;
; the axon model
;

(in-package "SURF-HIPPO")

(defun edit-axon (axon &optional called-from-type-menu)
  (let ((dummy2 (axon-delay axon))
	(dummy3 (axon-inherit-parameters-from-type axon))
	(dummy4 (axon-blocked axon))
	(dummy5 nil))
    (choose-variable-values
     (list '(dummy2 "Absolute Delay (independent of prop delay) [ms]" :float)
	   '(dummy3 "Ignore values listed above and inherit from type" :boolean)
	   '(dummy4 "Block this axon" :boolean)
	   (unless called-from-type-menu '(dummy5 "Edit Axon type" :boolean)))
     :label (format nil "Edit Axon ~a" (axon-name axon)))
    (setf (axon-delay axon) dummy2
	  (axon-inherit-parameters-from-type axon) dummy3
	  (axon-blocked axon) dummy4)
    (when dummy5 (menu-for-axon-types (axon-type axon))))
  (set-axon-parameters nil axon))

;;; MENU-FOR-AXON-TYPES
(defun menu-for-axon-types (&optional type-arg)
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7)
    (setq dummy3 (not *enable-axons*))
    (choose-variable-values '((dummy3 "Block all the axon types" :boolean)) :label "Global Axon Block Cocktail")
    (setq *enable-axons* (not dummy3))
    (loop for type in (if type-arg (list (element type-arg 'axon-type)) (menu-for-type 'axon-type))
	  ;;	  (choose-list-values (GET-CURRENT-AXON-TYPE-NAMES) nil :label "Choose Axon Types to Modify")
	  do
	  (let ((name (axon-type-name type)))
	    (setf dummy1 (axon-type-propagation-velocity type)
		  dummy2 (axon-type-input-threshold type)
		  dummy4 (axon-type-refractory-period type)
		  dummy5 (axon-type-supra-threshold-duration-min type)
		  dummy3 (or (not *enable-axons*) (axon-type-blocked type))
		  dummy6 nil
		  dummy7 (axon-type-name type))
	    (choose-variable-values
	     `((dummy7 ,(format nil "Edit name of type: ~% (used if saved to file):") :string)
	       (dummy1 "Propagation Velocity [m/s]" :float)
	       (dummy2 "Input Threshold [mV]" :float)
	       (dummy4 "Refractory Period [ms]" :float)
	       (dummy5 "Supra-Threshold Minimun Duration [ms]" :float)
	       (dummy6 "Plot axon waveform (canned spike)" :boolean)
	       (dummy3 ,(format nil "Block all ~a axons" (axon-type-name type)) :boolean))
	     :text (ADD-LINEFEEDS-TO-STRING-LIST (list (ELEMENT-SOURCEFILE-STRING type nil)))
	     :label (format nil "Setting Up Parameters Of Axon Type ~A" (axon-type-name type)))
	    (setf (axon-type-name type) dummy7)
	    (setq *recheck-circuit-elements-parameters* 
		  (or *recheck-circuit-elements-parameters*
		      (not (and (= dummy1 (axon-type-propagation-velocity type))
				(= dummy2 (axon-type-input-threshold type))
				(= dummy4 (axon-type-refractory-period type))
				(= dummy5 (axon-type-supra-threshold-duration-min type))))))
	    (setf (axon-type-propagation-velocity type) dummy1
		  (axon-type-input-threshold type) (coerce dummy2 'double-float)
		  (axon-type-refractory-period type) dummy4 
		  (axon-type-supra-threshold-duration-min type) dummy5 
		  (axon-type-blocked type) dummy3)
	    (if dummy6 (plot-axon-waveform (axon-type-name type)))))))


(defun edit-axon-type (type)
  (setq type (element type 'axon-type))
  (when type (menu-for-axon-types type)))

(defun get-current-axon-type-names ()
  (loop for axon being the hash-value of (AXON-HASH-TABLE)
	when (not (member (axon-type-name (axon-type axon)) types))
	collect (axon-type-name  (axon-type axon)) into types
	finally (return types)))

(defun print-axon-spike-times ()
    (loop for axon being the hash-value of (AXON-HASH-TABLE)
	  when (axon-spike-times axon)
	  do
	  (format t "Spike times for ~a:~%" (axon-name axon))
	  (format-list (reverse (axon-spike-times axon)) 10 t t)
	  (format t "~%")))

(defun print-axon-types () (PRINT-MODEL-PARAMETERS "axon-type"))

(defun print-axons () (PRINT-MODEL-PARAMETERS "axon"))

(defun set-axon-type-param (type-name param &optional value)
  (let ((type (AXON-TYPE-HASH-TABLE type-name))
	(return-value nil)
	(param-found t))
    (if type
	(progn
	  (setq return-value
		(case param
		  (propagation-velocity (if value (setf (axon-type-propagation-velocity type) (coerce value 'single-float))
					    (axon-type-propagation-velocity type)))
		  (supra-threshold-duration-min
		   (if value (setf (axon-type-supra-threshold-duration-min type) (coerce value 'single-float))
		       (axon-type-supra-threshold-duration-min type)))
 		  (input-threshold (if value (setf (axon-type-input-threshold type) (coerce value 'double-float))
				       (axon-type-input-threshold type)))
		  (refractory-period (if value (setf (axon-type-refractory-period type) (coerce value 'single-float))
					 (axon-type-refractory-period type)))
		  (block (if value (setf (axon-type-blocked type) (= 1 value))
			     (axon-type-blocked type)))
		  (t (setq param-found nil))))
	  (if (and param-found value) (setq *recheck-circuit-elements-parameters* t))
	  (if (not param-found) (format t "~%ERROR: Axon types do not have parameter ~a!~%" param)))
	(format t "~%ERROR: Axon type ~a doesn't exist!~%" type-name))
    (if (not value) return-value)))
    
(defun print-axon (axon)
  (let ((axon (element axon 'axon)))
    (when axon
      (format t "~a: Origin cell ~a (~a)" (massage-element-plot-label axon nil t)
	      (cell-name (node-cell (axon-proximal-node axon))) (node-name (axon-proximal-node axon)))
      (if (axon-target-synapse axon)
	(format t", Target synapse ~a~%" (element-name (axon-target-synapse axon)))
	(format nil "~%"))
      (format t"  Length: ~,2f um," (axon-length axon))
      (when (> (axon-delay axon) 0)
	(format t" Delay: ~,2fms," (axon-delay axon)))
      (format t" Propagation delay: ~,2fms" (axon-propagation-delay axon))
      (when (or (axon-blocked axon) (axon-type-blocked (axon-type axon)))
	(format t"**Blocked**"))
      (format t"~%"))))

(defun print-axon-type (type)
  (format t "Axon Type ~a:  Prop vel. ~,2f (um/ms), Thresh ~,2f mV, T_refr ~,2f, Suprathresh T_min ~,2f ms, Q10 ~,2f, T_ref ~,2f~%"
	  (axon-type-name type)
	  (axon-type-propagation-velocity type)
	  (axon-type-input-threshold type)
	  (axon-type-refractory-period type)
	  (axon-type-supra-threshold-duration-min type)	    
	  (axon-type-q10 type)
	  (axon-type-reference-temp type))
  (when (axon-type-blocked type) (format t" **Blocked**~%"))
  (print-num-elements-sourcefile type)
  (format t "~%"))

(defun print-create-axons-for-cell (cell &optional (indent-output 0))
  (loop for axon being the hash-value of (AXON-HASH-TABLE)
	when (eq (element-cell axon) cell)
	do
	(when (> indent-output 0)
	  (dotimes (i indent-output)
	    (format t " ")))
	(print-create-axon axon)))

(defun print-create-axon (axon) 
  (format t
	  "(create-axon ~s ~s :length ~a :delay ~A :distance-coeff ~a"
	  (element-name (axon-proximal-node axon))
	  (axon-type-name (axon-type axon))
	  (axon-length axon)
	  (axon-delay axon)
	  (element-parameter axon 'distance-coeff))
  (when (element-parameter axon 'mid-points)
    (format t
	    "~%             :CONSIDER-MID-POINTS-FOR-LENGTH ~a"
	    (element-parameter axon 'CONSIDER-MID-POINTS-FOR-LENGTH))
    (format t
	    "~%             :mid-points '~A"
	    (element-parameter axon 'mid-points))
    (format t ")~%~%")))

(defun block-all-axons () (loop for syn in (axons) do (setf (axon-blocked syn) t)))
(defun block-all-axon-types () (loop for syn in (axon-types) do (setf (axon-type-blocked syn) t)))
(defun block-axon-types (types)
  (loop for type in (coerce-to-list types) when (element type 'axon-type) do (setf (axon-type-blocked (element type 'axon-type)) t)))
(defun unblock-all-axons () (unblock-all-axon-types) (loop for syn in (axons) do (setf (axon-blocked syn) nil)))
(defun unblock-all-axon-types () (loop for syn in (axon-types) do (setf (axon-type-blocked syn) nil)))
(defun unblock-axon-types (types)
  (loop for type in (coerce-to-list types) when (element type 'axon-type) do (setf (axon-type-blocked (element type 'axon-type)) nil)))
(defun blocked-axon-types ()
  (loop for type in (axon-types) when (or ; (not *active*)
				       (and (instance-in-cell type) (axon-type-blocked type))) collect type))

(defun set-axons-parameters () (maphash 'set-axon-parameters (AXON-HASH-TABLE)))

(defun set-axon-parameters (name axon)
  (declare (ignore name))
  (when (axon-target-synapse axon)	; (= 0.0 (axon-length axon))
    (setf (axon-length axon)
	  (if (and (element-parameter axon 'CONSIDER-MID-POINTS-FOR-LENGTH) (element-parameter axon 'MID-POINTS))
	    (let ((start (element-absolute-location axon)))
	      (loop for point in (add-to-end (element-parameter axon 'MID-POINTS)
					     (element-absolute-location (or (axon-target-synapse axon) (element-parameter axon 'target))))
		    sum (cartesian-vector-distance start point) into length
		    ;; do (format t "~A ~A~%" point length)
		    do (setq start point)
		    finally (return length)))
	    (* (or (element-parameter axon 'DISTANCE-COEFF) 1.0)
	       (cartesian-vector-distance
		(element-absolute-location (or (axon-target-synapse axon) (element-parameter axon 'target)))
		(element-absolute-location axon))))))
  (setf (axon-propagation-delay axon)	;; milliseconds
	(/ (axon-length axon)		;; microns
	   (*
	    (q10-rate-factor
	     (+ 273.16 (axon-type-reference-temp (axon-type axon)))
	     *temperature*
	     (axon-type-q10 (axon-type axon)))
	    1.0e6 ;; microns/meter
	    1.0e-3 ;; s/millisecond
	    (axon-type-propagation-velocity (axon-type axon)) ;; m/s
	    )))) 

(defun get-axon-simple-name ()
  (loop for candidate from (max 1 *axon-simple-name-counter*)
	until (not (axon-hash-table candidate))
	finally (return (setf *axon-simple-name-counter* candidate))))


(defun rename-axons-simple (&optional (axons (axons)))
  "Rename AXONS [default all axons in circuit] with simple integer names."
  (loop for seg in axons do (set-element-name seg (get-axon-simple-name) 'axon)))

(defun create-axon (proximal-element axon-type &key (length 0.0)	; um
				     (delay 0.0) ; ms
				     target (DISTANCE-COEFF 1.0) (CONSIDER-MID-POINTS-FOR-LENGTH t) MID-POINTS)
  "An axon of AXON-TYPE is created, controlled by the voltage of the cell element associated with PROXIMAL-ELEMENT. Axons
do have nodes, so that accessing their voltage is the same as for segments and somas, but these nodes are not considered part of
the core circuit [the :IS-PHYSICAL-CELL-NODE slot is NIL for these nodes] since they are not part of the circuit equations. If
included, MID-POINTS is a list of XYZ coordinates that define a series of points on the axon, that is considered in the graphical
representation of the axon, and as well for the actual length if CONSIDER-MID-POINTS-FOR-LENGTH is T unless LENGTH is
non-NIL. Otherwise, the axon length is derived from the straight line distance between PROXIMAL-ELEMENT and the TARGET, multiplied
by DISTANCE-COEFF. All distance arguments are in microns. TARGET is only used for calculating the axon length. Any synapse that is
controlled by the created axon must be created after the axon. In this case the length of the axon will be re-evaluated using the
location of the target synapse as a reference."
  (let* ((type (if (axon-type-p axon-type) axon-type (create-axon-type axon-type)))
	 (axon-type-symbol (axon-type-name type))
	 (proximal-element (element-cell-element proximal-element))
	 (proximal-node (element-node proximal-element))	   
	 (cell (node-cell proximal-node))
	 (name (if *use-simple-names*
		 (get-axon-simple-name) ; (1+ (hash-table-count (AXON-HASH-TABLE)))
		 (concatenate-strings "Axon-" (string (node-name proximal-node)) "-" (string axon-type-symbol)))))
    (cond ((and (AXON-HASH-TABLE name)
					; (equal target-synapse (axon-target-synapse (AXON-HASH-TABLE name)))
		)
	   (format t "create-axon: axon ~a already defined" name)
	   (AXON-HASH-TABLE name))
	  (t
	   (let* ((name (if (AXON-HASH-TABLE name)
			  (check-element-name name 'axon)
			  name))
		  (axon (make-axon :name name
				   :node (create-node name :cell (cell-name cell) :is-physical-cell-node nil)
				   :cell-element proximal-element
				   :proximal-node proximal-node
				   :delay (float delay)
				   :type type
				   :length (float length))))
	     (push axon (node-elements (axon-node axon)))
	     (push axon (axon-type-axons (axon-type axon)))
	     (setf (axon-event-generator axon) axon)
	     (push axon (node-elements proximal-node))
	     (push-element-parameter type 'axons axon)
	     (when MID-POINTS (push (cons 'mid-points (loop for mid-point in mid-points collect (s-flt-list MID-POINT)))
				    (axon-parameters axon)))
	     (push (cons 'CONSIDER-MID-POINTS-FOR-LENGTH CONSIDER-MID-POINTS-FOR-LENGTH) (axon-parameters axon))
	     (push (cons 'DISTANCE-COEFF DISTANCE-COEFF) (axon-parameters axon))
	     (setf (AXON-HASH-TABLE name) axon)
	     (when target (element-parameter axon 'target (element-cell-element target)))
	     (setq *axon* axon))))))

(defun ARE-THERE-AXONS () (> (hash-table-count (AXON-HASH-TABLE)) 0))

(defun all-axon-info () (maphash 'axon-info (AXON-HASH-TABLE)))

(defun axons-of-type (type)
  (let ((type (element type 'axon-type)))
    (loop for prt in (axons) when (equal (axon-type prt) type) collect prt)))

(defun axon-info  (name axon)
  (format t "Axon ~a at ~a; type ~a, vel ~a um/ms, ~%"
	  name
	  (axon-proximal-node axon)
	  (axon-type-name (axon-type axon))
	  (* (q10-rate-factor
	      (+ 273.16 (axon-type-reference-temp (axon-type axon)))
	      *temperature*
	      (axon-type-q10 (axon-type axon)))
	     1.0e6 ;; microns/meter
	     1.0e-3 ;; s/millisecond
	     (axon-type-propagation-velocity (axon-type axon)))))

(defun remove-axon-type-arrays (type) (REMOVE-element-PARAMeters type '(active-axon-array active-axon-array-length)))

(defun revamp-axon-type-arrays (type axons &optional inclusion)
  (loop for axon in axons
	when (if inclusion (funcall inclusion axon) t)
	collect axon into activated-axons
	finally
	(element-parameter type 'active-axon-array (list-to-array activated-axons))
	(element-parameter type 'active-axon-array-length (length activated-axons))))

(defun setup-axons ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setq *axon-type-list*
	(when (and (ARE-THERE-AXONS) *enable-axons*)
	  (no-nils
	   (loop for type in (axon-types)
		 do (remove-axon-type-arrays type)
		 unless (axon-type-blocked type)
		 collect
		 (let ((axons (loop for axon in (axon-type-axons type) unless (axon-blocked axon) collect axon)))
		   (when axons
		     (revamp-axon-type-arrays type axons)
		     (setup-event-generators-and-followers-of-type axons)
		     type)))))))

;; Used for axon and synapse intialization.
(defun waveform-interval-inverse-to-mrt-interval (time-interval-inverse) (round (/ 1.0  (* *mrt* time-interval-inverse))))

(defun init-axons ()
  ;; For adjusting simulation duration dependent parameters, and to clear input events.
  (loop for type in *axon-type-list* do
	(setf (axon-type-waveform-time-interval-mrt type)
	      (waveform-interval-inverse-to-mrt-interval (axon-type-waveform-time-interval-inverse type)))
	(let* ((params (axon-type-parameters type))
	       (axon-array (get-a-value 'active-axon-array params))
	       (axon-array-length (get-a-value 'active-axon-array-length params)))
	  (do ((i 0 (1+ (the fn i))))
	      ((= (the fn i) (the fn axon-array-length)))
	    (let ((axon (svref (the (simple-array axon (*)) axon-array) i)))
	      (setf (axon-spike-times axon) nil)
	      (set-node-voltage (axon-node axon) (element-parameter (axon-type axon) 'WAVEFORM-REFERENCE)))))))

(defun advance-axons ()
  (loop for type in *axon-type-list* do
	(let* ((params (axon-type-parameters type))
	       (axon-array (get-a-value 'active-axon-array params))
	       (axon-array-length (get-a-value 'active-axon-array-length params)))
	  (do ((i 0 (1+ (the fn i))))
	      ((= (the fn i) (the fn axon-array-length)))
	    (let ((nd (axon-node (svref (the (simple-array axon (*)) axon-array) i))))
	      (psetf
	       (node-voltage-n-1-double nd) (node-voltage-n nd)
	       (node-voltage-n nd) (node-voltage-n+1 nd)
	       (node-dvdt-n-1 nd) (node-dvdt-n nd)))))))

;;;; *** NEED TO CHANGE THIS TO MORE CLOSELY PARALLEL VOLTAGE SYNAPSES (EVENT GENERATORS). ***
(proclaim '(inline eval-axon))
(defun eval-axon (axon waveform axon-type-waveform-time-interval-inverse waveform-length WAVEFORM-REFERENCE
		       axon-type-input-threshold axon-type-refractory-period  axon-type-supra-threshold-duration-min 
		       axon-type-waveform-time-interval-mrt first-iteration-of-step)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type vec-flt waveform)
	   (type single-float axon-type-waveform-time-interval-inverse 
		 axon-type-refractory-period axon-type-supra-threshold-duration-min)
	   (type double-float axon-type-input-threshold WAVEFORM-REFERENCE)
	   (type fixnum waveform-length axon-type-waveform-time-interval-mrt))
  ;; Update sub-threshold-time, if we are below threshold, or within the refractory period of the last spike.
  (when (and first-iteration-of-step (eq axon (axon-event-generator axon)))
    (if (or
	 (< (node-voltage-n (axon-proximal-node axon)) axon-type-input-threshold)
	 (when (axon-spike-times axon)
	   (> axon-type-refractory-period
	      (- (the sf (*t[n]*)) ; (*input-time*)
		 (the sf (car (axon-spike-times axon)))))))
	(setf (axon-sub-threshold-time axon) (the sf (*t[n]*)) ; (*input-time*)
	      )
      ;; FIRE a spike if we are above threshold (implied by a value of AXON-SUB-THRESHOLD-TIME which is not
      ;; equal to *LAST-REAL-TIME*) for greater than AXON-TYPE-SUPRA-THRESHOLD-DURATION-MIN, if this latter
      ;; value is >0. If =0, then check whether we are above threshold explicitly.
      (when (or (and (= axon-type-supra-threshold-duration-min 0.0)
		     (>= (node-voltage-n (axon-proximal-node axon)) axon-type-input-threshold)) 
		(> (the sf (- (the sf (*t[n]*)) ; (*input-time*)
			      (axon-sub-threshold-time axon)))
		   axon-type-supra-threshold-duration-min))
	;; Push spike time.
	(push (the sf (*t[n]*)) (axon-spike-times axon))
	(INSERT-EVENT-BREAKPOINT (+ (axon-propagation-delay axon) (axon-delay axon) (the sf (car (axon-spike-times axon)))))
	;; Reset sub threshold time.
	(setf (axon-sub-threshold-time axon) (the sf (*t[n]*))))))
  ;; Interpolate axon waveform between millisecond points.
  (setf (node-voltage-n+1 (axon-node axon)) 
	(let ((voltage WAVEFORM-REFERENCE) past-last-spike)
	  (declare (double-float voltage))
	  ;; Now generate the output value.
	  (do ((spike-times (axon-spike-times axon) (cdr spike-times)))
	      ((or past-last-spike (null spike-times)))
	    (let* ((spike-time-floating-index
		    (* axon-type-waveform-time-interval-inverse
		       (- (the sf (- (*input-time*) (the sf (car spike-times))))
			  (+ (axon-propagation-delay axon) (axon-delay axon))))))
	      (multiple-value-bind (spike-time-index-integer-part spike-time-index-fractional-part)
		  (truncate (the sf spike-time-floating-index))
		(declare (single-float spike-time-index-fractional-part spike-time-floating-index)
			 (fixnum spike-time-index-integer-part))
		(if (>= spike-time-index-integer-part waveform-length)
		    (setq past-last-spike t)
		  (when (>= spike-time-floating-index 0.0)
		    (check-element-time-step-maximum axon-type-waveform-time-interval-mrt)
		    (setq voltage (+ voltage (interpolated-array-value
					      waveform
					      spike-time-index-integer-part
					      spike-time-index-fractional-part))))))))
	  voltage))
  nil)

(defun eval-all-axons (&optional (first-iteration-of-step t))
  (when *axon-type-list*
    (loop for type in *axon-type-list* do
	  (let* ((params (axon-type-parameters type))
		 (axon-array (get-a-value 'active-axon-array params))
		 (axon-array-length (get-a-value 'active-axon-array-length params))
		 (waveform (axon-type-output-waveform type))
		 (waveform-length (length (the vec-flt waveform)))
		 (WAVEFORM-REFERENCE (get-a-value 'WAVEFORM-REFERENCE params))
		 (axon-type-waveform-time-interval-inverse (axon-type-waveform-time-interval-inverse type))
		 (axon-type-input-threshold (axon-type-input-threshold type))
		 (axon-type-refractory-period (axon-type-refractory-period type))
		 (axon-type-supra-threshold-duration-min (axon-type-supra-threshold-duration-min type))
		 (axon-type-waveform-time-interval-mrt (axon-type-waveform-time-interval-mrt type)))
	    (do ((i 0 (1+ (the fn i))))
		((= (the fn i) (the fn axon-array-length)))
	      (eval-axon
	       (svref (the (simple-array axon (*)) axon-array) i)
	       waveform
	       axon-type-waveform-time-interval-inverse
	       waveform-length
	       WAVEFORM-REFERENCE
	       axon-type-input-threshold
	       axon-type-refractory-period
	       axon-type-supra-threshold-duration-min
	       axon-type-waveform-time-interval-mrt
	       first-iteration-of-step))))
    nil))

(defun create-axon-type (type-symbol &optional actual-type-symbol update-parameters)
  "TYPE-SYMBOL is a symbol or an axon type; in the former case it must match the CAR of one of the
lists contained in axon type parameter library. Returns the axon type structure, whether is was
already made or not."
  (let* ((type (unless actual-type-symbol (if (axon-type-p type-symbol) type-symbol (AXON-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (axon-type-p type-symbol) (axon-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'axon-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about axon type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
  	(setq type (if parent-type-symbol
		     (create-AXON-TYPE parent-type-symbol type-symbol update-parameters)
		     (make-AXON-TYPE :name type-symbol))))
      (when (and update-parameters parent-type-symbol)
	(update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf (axon-type-q10 type)
	    (s-flt (or (cdr-assoc 'q10 original-parameters) *axon-default-q10*))
	    (axon-type-reference-temp type)
	    (s-flt (or (cdr-assoc 'reference-temp original-parameters) *axon-default-reference-temperature*))
	    (axon-type-propagation-velocity type)
	    (s-flt (or (cdr-assoc 'propagation-velocity original-parameters) *axon-default-propagation-velocity*))
	    (axon-type-refractory-period type)
	    (s-flt (or (cdr-assoc 'refractory-period original-parameters) *axon-default-refractory-period*))
	    (axon-type-SUPRA-THRESHOLD-DURATION-MIN type)
	    (s-flt (or (cdr-assoc 'SUPRA-THRESHOLD-DURATION-MIN original-parameters) *axon-default-supra-threshold-duration-min*))
	    (axon-type-input-THRESHOLD type)
	    (d-flt (or (cdr-assoc 'input-THRESHOLD original-parameters) *axon-default-input-threshold*)))
      (cond-every
       ((assoc 'extra-parameters original-parameters)
	(setf (axon-type-parameters type) (acons 'extra-parameters (cdr-assoc 'extra-parameters original-parameters) (axon-type-parameters type))))
       ((assoc 'WAVEFORM-FUNCTION-ARGS original-parameters)
	(setf (axon-type-parameters type) (acons 'WAVEFORM-FUNCTION-ARGS (cdr-assoc 'WAVEFORM-FUNCTION-ARGS original-parameters) (axon-type-parameters type))))
       ((assoc 'WAVEFORM original-parameters)
	(setf (axon-type-parameters type) (acons 'WAVEFORM (cdr-assoc 'WAVEFORM original-parameters) (axon-type-parameters type)))))
      (element-parameter
       type
       'WAVEFORM-REFERENCE
       (d-flt (or (cdr-assoc 'WAVEFORM-REFERENCE original-parameters) *axon-default-waveform-reference*)))
      (let ((waveform (s-flt-array
		       (or (cdr-assoc 'WAVEFORM original-parameters)
			   (let ((funspec (get-a-value 'waveform-FUNCTION original-parameters)))
			     (apply (car funspec) (cdr funspec)))))))
	(setf (axon-type-output-waveform type) waveform))
      (setf (axon-type-waveform-time-interval-inverse type)
	    (/ 1.0 (or (cdr-assoc 'WAVEFORM-time-interval original-parameters) *axon-default-waveform-time-interval*)))
      (setf (AXON-TYPE-HASH-TABLE type-symbol) type))
    (setq *axon-type* type)
    type))

(defun create-axons (cell-element axon-type-symbols)
  ;; "Add axons of type specified in AXON-TYPE-SYMBOLS to the CELL-ELEMENT."
  (dolist (axon-type-symbol axon-type-symbols) (create-axon cell-element axon-type-symbol)))

(defun add-axons-ad-lib (location-names type-symbol) (add-axon-to-locations location-names type-symbol))  
 
(defun add-axon-to-locations (location-names type-symbol)
  (when *monitor-circuit-modifications*
    (let ((names (loop for name in location-names collect (element-name name))))
      (format t ";; (add-axon-to-locations ~%;; ")
      (FORMATTED-LIST-DUMP names t t)
      (format t "~%;; ~a)~%" type-symbol)))
  (dolist (name location-names)
    (let ((location (element-cell-element name)))
      (when location (create-axon location type-symbol)))))

(defun add-axon-type (type-symbol &optional percentage-of-axon-nodes)
  (erase-element type-symbol)
  (add-axon-to-cells (list-all-hash-names (CELL-HASH-TABLE)) type-symbol percentage-of-axon-nodes))
 
(defun add-axon-to-cells (cell-names type-symbol &optional percentage-of-axon-nodes)
  (when (and *always-intialize-random-gen* percentage-of-axon-nodes) (get-reference-random-state))
  (let ((*monitor-circuit-modifications* *monitor-circuit-modifications*))
    (when *monitor-circuit-modifications*
      (format t ";; (add-axon-to-cells ~%;; ")
      (FORMATTED-LIST-DUMP cell-names t t)
      (format t "~%;; ~a" type-symbol)
      (if percentage-of-axon-nodes
	  (format t " ~a" percentage-of-axon-nodes))
      (format t "~%;; )~%"))
    (setq *monitor-circuit-modifications* nil)
    (loop for cell-name in cell-names 
	  when (CELL-HASH-TABLE cell-name) do
	  (add-axon-to-locations
	   (loop for elt in (somas-and-segments (element cell-name) nil nil)
		 when (if percentage-of-axon-nodes (< (random 100) percentage-of-axon-nodes) t)
		 collecting elt)
	   type-symbol))))
  
(defun plot-axon-waveform (&optional axon-type-name)
  (let ((types (coerce-to-list (element-type (or axon-type-name (select-hash-values-menu (list (AXON-TYPE-HASH-TABLE)) "Choose axon type for plotting waveforms"))
					     'axon-type))))
    (loop for type in types do
	  (let* ((waveform (axon-type-output-waveform type))
		 (reference (element-parameter type 'WAVEFORM-REFERENCE))
		 (time-interval (/ 1.0 (axon-type-WAVEFORM-TIME-INTERVAL-INVERSE type))))
	    (plot-timed-data (list (loop for val in (array-to-list waveform) collect (+ val reference))) (list "Waveform Response") time-interval
			     :y-label "mV" :title (format nil "Axon Type ~a Waveform Response" (axon-type-name type)))))))

(defun get-current-axon-type-names ()
  (loop for axon being the hash-value of (AXON-HASH-TABLE)
	when (not (member (axon-type-name (axon-type axon)) types))
	collect (axon-type-name  (axon-type axon)) into types
	finally (return types)))

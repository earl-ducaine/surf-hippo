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

;;; SYS Source file: waveforms.lisp

(in-package "SURF-HIPPO")

;; Functions for creating and manipulating one and two dimensional waveforms.

(defun 1st-non-zero-elt-index (array)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (*)) array))
  (loop for i from 0 to (the fn (1- (length array)))
	when (not (= (aref array i) 0))
	do (return i)))

(defun last-non-zero-elt-index (array)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (*)) array))
  (loop for i from (the fn (1- (length array))) downto 0
	when (not (= (aref array i) 0))
	do (return i)))

(defun find-first-non-zero-element (array) (1st-non-zero-elt-index array))

(defun find-last-non-zero-element (array) (last-non-zero-elt-index array))

(defun float-array-support (array)
  (let ((first (1st-non-zero-elt-index array)))
    (if first (- (1+ (last-non-zero-elt-index array)) first) 0)))

(defun transfer-float-array (from-array to-array &optional (first 0) last)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type VEC-FLT from-array to-array)
	   (fixnum first))
  (unless last (setq last (1- (length from-array))))
  (do ((i first (1+ (the fn i))))
      ((= (the fn i) (the fn last)))
    (setf (aref to-array i) (aref from-array i))))

;; DELTA-WAVE-ARRAY, DOUBLE-DELTA-WAVE-ARRAY Given a n-valued sequence with values
;;
;;     [x1 x2 x3 ... xn]
;;
;; returns an (n-1)-valued array with values
;;
;;     [x2-x1, x3-x2, ... x(i+1)-xi, ... xn-x(n-1)]
;;
;; with the values of the returned array single or double float, respectively.
(defun delta-wave-array (wave) (s-flt-array (differentiate-wave wave 1)))

(defun delta-float-wave-array (wave) (s-flt-array (differentiate-float-wave wave 1.0 t)))

(defun double-delta-wave-array (wave) (d-flt-ARRAY (differentiate-wave wave 1)))

(defun nth-derivative (data time-base N)
  "Returns as values the Nth derivative of DATA, based on recursive calls to DIFFERENTIATE-WAVE, with the associated time list derived from TIME-BASE,
which can be a numeric list or a single number, in which case it is taken as the time step dt."
  (when (and data time-base)
    (let ((time-base (expand-time-reference time-base (length data))))
      (if (zerop N)
	  (values data time-base)
	  (nth-derivative (differentiate-wave data time-base) (midpoints time-base) (1- N))))))

(defun differentiate-wave (wave &optional (time-spec 1.0))
  "Given a n-valued sequence WAVE with values

 [x0 x1 x2 ... xn-1]

returns an (n-1)-valued list with values

 [(x1-x0)/TIME-SPEC, (x2-x1)/TIME-SPEC, ... (x(i)-x(i-1))/TIME-SPEC, ... (x(n-1)-x(n-2))/TIME-SPEC]

if TIME-SPEC is a number (corresponding to delta-T). Otherwise, if TIME-SPEC is a sequence, then returns

 [(x1-x0)/(T1-T0), (x2-x1)/(T1 - T2), ... (x(n-1)-x(n-2))/(T(n-1)-T(n-2)]

where Tn = (NTH n TIME-SPEC).
"
  (when (and wave time-spec)
    (let ((wave (sequence-to-list wave))
	  (time-spec (typecase time-spec
		       (number time-spec)
		       (t (sequence-to-list time-spec))))
	  last-x last-t)
      (if (consp time-spec)
	  (loop for value in wave
	     for time in time-spec
	     for count from 0
	     when (> count 0)
	     do (when (zerop (- time last-t))
		  (sim-error (format nil "The ~:R interval of the time specification for DIFFERENTIATE-WAVE has a zero duration!" count)))
	     and collect (/ (- value last-x) (- time last-t)) into out
	     do (setq last-x value
		      last-t time)
	     finally (return out))
	  (loop for value in wave
	     for count from 0
	     when (> count 0) collect (/ (- value last-x) time-spec) into out
	     do (setq last-x value)
	     finally (return out))))))

(defun differentiate-float-wave (wave &optional (time-spec 1.0) array-output)
  "As for DIFFERENTIATE-WAVE, but requires single float values (and is more efficient).
When ARRAY-OUTPUT is T [default NIL] returns the result in an array, otherwise a list."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (when (and wave time-spec)
    (let* ((time-spec (typecase time-spec
			(number time-spec)
			(t (sequence-to-list time-spec))))
	   ;;	 (wave (coerce-to-list wave))
	   (consp-time-spec (consp time-spec))
	   (time (if consp-time-spec (car time-spec) time-spec))
	   (last-x 0.0)
	   (last-t 0.0)
	   (diff-value 0.0)
	   output
	   (output-array (when array-output (make-array (list (length wave)) :element-type 'single-float))))
      (declare (single-float last-x last-t diff-value time))
      (typecase wave
	(cons
	 (loop for value single-float in wave
	    for count fixnum from 0
	    when consp-time-spec do (setq time (car time-spec) time-spec (cdr time-spec))
	    when (> count 0)
	    do (when (and consp-time-spec (zerop (- time last-t)))
		 (sim-error (format nil "The ~:R interval of the time specification for DIFFERENTIATE-FLOAT-WAVE has a zero duration!" count)))
	      (setq diff-value (/ (- value last-x) (if consp-time-spec (- time last-t) time)))
	    and when array-output do (setf (aref output-array count) diff-value) else collect diff-value into output
	    do				; (format t "last-x ~A, value ~A, count ~A~%" last-x value count)
	      (setq last-x value
		    last-t time)
	    finally (return (or output-array output))))
	(t
	 (loop for value single-float across (the (simple-array single-float (*)) wave)
	    for count fixnum from 0
	    when consp-time-spec do (setq time (car time-spec) time-spec (cdr time-spec))
	    when (> count 0)
	    do (setq diff-value (/ (- value last-x) (if consp-time-spec (- time last-t) time)))
	    and when array-output do (setf (aref output-array count) diff-value) else collect diff-value into output
	    do (setq last-x value
		     last-t time)
	    finally (return (or output-array output))))))))

#|

t0          t1            t2            t3             t4   ...
----------------------------------------------------------------
x0          x1            x2            x3             x4   ...

    x1-x0         x2-x1         x3-x2         x4-x3   ...

       (x2-2x1+x0)    (x3-2x2+x1)   (x4-2x3+x2)  ...
|#

(defun midpoints (wave)
  "Given a n-valued sequence WAVE with values

 [x0 x1 x2 ... xn-1]

returns an (n-1)-valued list with values

 [(x0 + x1)/2, (x1 + x2)/2, ... (xn-2 + xn-1)/2]

"
  (when wave (mapcar '+ wave (differentiate-wave wave 2.0))))


(defun element-data-window (element start stop &key (dt 0.1) data-list data-type model-type (time-base (current-sim-plot-time-list)) state-index)
  "Returns element data for the time window defined between START and STOP, in milliseconds. Remaining arguments are as for
ELEMENT-DATA-DTED. If DATA-LIST is supplied, the sampled data is taken directly from this list, which is assumed to be on a time
grid of DT, and ELEMENT is ignored."
  (atomize-list
   (loop for element in (if data-list '(T) (coerce-to-list element))
	 collect (let ((data-list (or data-list (element-data-dted element dt data-type model-type time-base state-index))))
		   (data-window data-list start stop dt)))))

(defun data-window (data-list start stop dt)
  "Return a sub-list of a timed sequence DATA-LIST, sampled at DT starting at time=0, from START to STOP."
  (loop for time from 0.0 by dt
	for data in data-list
	when (and (> stop time) (>= time start)) collect data))

(defun expand-time-reference (time-base length &optional (start-time 0.0))
  "If TIME-BASE is a list, return that list. Otherwise return the list made by (LIST-OF-NUMS LENGTH START-TIME TIME-BASE)."
  (when time-base
    (typecase time-base
      (cons time-base)
      (number (list-of-nums length (s-flt start-time) (s-flt time-base)))
      (t (sim-error (format nil "TIME-BASE must be a number or a list of numbers."))))))

(defun spike-times (element &key (spike-threshold -20.0) ; mV
			      (supra-threshold-duration-min 0.0)	; milliseconds
			      model-type data-list (start-time 0.0) (time-base (current-sim-plot-time-list)))
  "Detect spikes associated with ELEMENT and return their times.
Result is a list of interpolated positive threshold crossing times obtained from the voltage of the soma or segment associated with ELEMENT of MODEL-TYPE,
according to the SPIKE-THRESHOLD [mV] and SUPRA-THRESHOLD-DURATION-MIN. All times are in milliseconds, and referenced from the time that the voltage last went above
SPIKE-THRESHOLD. If DATA-LIST is supplied, the sampled data is taken directly from this list, and the ELEMENT argument is ignored. The time base is given by TIME-BASE
[default given by the function CURRENT-SIM-PLOT-TIME-LIST], after processing by the function EXPAND-TIME-REFERENCE."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (flet ((interpolate-for-spike-threshold-time (time last-time voltage last-voltage spike-threshold)
	   (declare (single-float time last-time voltage last-voltage spike-threshold))
	   (+ last-time
	      (if (zerop (- voltage last-voltage))
		  0.0
		  (* (the sf (- time last-time))
		     (/ (- spike-threshold last-voltage)
			(- voltage last-voltage)))))))
    (let* ((element (element-cell-element element))
	   (data (or data-list (element-data element 'voltage model-type)))
	   (time-base (expand-time-reference time-base (length data) start-time))
	   (spike-threshold (s-flt spike-threshold))
	   sub-threshold-time
					; (s-flt (max 0 sub-threshold-time))
	   (supra-threshold-duration-min (s-flt supra-threshold-duration-min))
	   spike-now
	   (spike-times '())
	   (last-time (car time-base))
	   (last-voltage (car data))
	   spike-time-recorded
	   (supra-threshold-duration-min-zerop (<= supra-threshold-duration-min 0))
	   (candidate-spike-time 0.0))
      (unless data (sim-error (cond ((cell-element-p element) (format nil "~A has no voltage data to process!" (element-name element)))
				    (element (format nil "~A is not a cell element" (ELEMENT-name element)))
				    (t "No DATA"))))
      (loop for voltage single-float in data
	 for time single-float in time-base
	 do
	   (if (< voltage spike-threshold)
	       (setq sub-threshold-time time spike-now nil spike-time-recorded nil)
	       (when sub-threshold-time
		 (unless spike-now (setq candidate-spike-time (the sf (interpolate-for-spike-threshold-time time last-time voltage last-voltage spike-threshold))))
		 (setq spike-now t)
		 (when (and (not spike-time-recorded)
			    (or supra-threshold-duration-min-zerop
				(> (- time sub-threshold-time) supra-threshold-duration-min)))
		   (push candidate-spike-time spike-times)
		   (setq spike-time-recorded t))))
	   (setq last-time time
		 last-voltage voltage)
	 finally (return (reverse spike-times))))))

(setf (symbol-function 'element-spike-times) (symbol-function 'spike-times)) ; Backward compatibility
(setf (documentation 'element-spike-times 'function) "")



(defun spike-heights (element &key (spike-threshold -20.0) ; mV
				sub-threshold-time (supra-threshold-duration-min 0.1) ; milliseconds
				model-type spike-times data-list (time-base (current-sim-plot-time-list)))
  "Find maximum value of spikes associated with ELEMENT.
Returns maximum voltages [when dV/dt = 0] from spikes referenced by an explicit list of SPIKE-TIMES, otherwise by calling the function SPIKE-TIMES on ELEMENT.
Remaining arguments are as for the function SPIKE-TIMES. Returns as values two lists of the max voltages and their times."
  (declare (ignore sub-threshold-time))
  (let* ((voltages (or data-list (element-data element 'voltage model-type)))
	 (time-base (expand-time-reference time-base (length voltages)))
	 (spike-times (or SPIKE-TIMES
			  (spike-times element :spike-threshold spike-threshold :supra-threshold-duration-min supra-threshold-duration-min
				       :model-type model-type
				       :data-list voltages
				       :time-base time-base))))
    (find-maxs voltages spike-times time-base :before/after :subsequent)))

(setf (symbol-function 'element-spike-heights) (symbol-function 'spike-heights)) ; Backward compatibility
(setf (documentation 'element-spike-heights 'function) "")

(defun element-data-d2dt2 (element &key (time-base (current-sim-plot-time-list)) data-type model-type state-index)
  (nth-derivative (element-data element data-type model-type state-index) time-base 2))

(defun element-data-d2dt2-dted (element dt &key (time-base (current-sim-plot-time-list)) data-type model-type state-index)
  (nth-derivative (element-data-dted element dt data-type model-type time-base state-index) dt 2))

(defun spike-thresholds (element &key (spike-threshold -20.0) ; mV
				   sub-threshold-time (supra-threshold-duration-min 0.1) ; milliseconds
				   model-type
				   spike-times data-list (time-base (current-sim-plot-time-list))
				   (minimum-d2vdt2 1.0e3)	; mV/ms2
				   plot-d2v-dt2 (plot-d2v-dt20-title "Threshold 2nd Derivative Output")
				   plot-thresholds (plot-thresholds-title "Threshold Output")
				   (x-label "ms") (y-label "mV"))
  "Find spike thresholds associated with ELEMENT.
Spike thresholds are defined as the time of the maximum second derivative [greater than MINIMUM-MAX-D2VDT2, default 1.0e3 mV/ms2] preceding each entry of reference times
from an explicit list of SPIKE-TIMES, if supplied, otherwise as detected by the function SPIKE-TIMES applied to ELEMENT. Returns as values a list of the threshold
voltages and a list of times for the thresholds. When PLOT-THRESHOLDS is T, then the data is plotted with markers at the calculated thresholds. PLOT-D2V-DT2 enables the
plotting of the second derivative of the data. Remaining arguments are as for the function SPIKE-TIMES."
  (declare (ignore sub-threshold-time))
  (let* ((minimum-d2vdt2 (s-flt minimum-d2vdt2))
	 (voltages (or data-list (element-data element 'voltage model-type)))
	 (time-base (expand-time-reference time-base (length voltages)))
	 (spike-times (or SPIKE-TIMES
			  (spike-times element :spike-threshold spike-threshold :supra-threshold-duration-min supra-threshold-duration-min :data-list voltages :time-base time-base))))
    (multiple-value-bind (d2vdt2 d2vdt2-time-base)
	(nth-derivative voltages time-base 2) ; (data-d2dt2 voltages time-base)
      (let ((voltages-win (when plot-thresholds (plot-timed-data voltages nil time-base :x-label x-label :y-label y-label :title plot-thresholds-title)))
	    (d2vdt2-plot-win (when plot-d2v-dt2 (plot-timed-data d2vdt2 nil d2vdt2-time-base :x-label x-label :y-label (format nil "~A/~A^2" y-label x-label) :title plot-d2v-dt20-title :x-min 0))))
	(multiple-value-bind (d2vdt2-maxs d2vdt2-max-times)
	    (find-maxs d2vdt2 spike-times d2vdt2-time-base :before/after :previous :min-max-value minimum-d2vdt2 :window 1.0)
	  (loop for voltage in voltages
	     for time in time-base
	     unless d2vdt2-max-times do
	       (when d2vdt2-plot-win (add-markers-with-data d2vdt2-plot-win threshold-times d2vdt2-maxs :add-point t :clear-previous-markers t))
	       (when voltages-win (add-markers-with-data voltages-win threshold-times thresholds :add-point t :clear-previous-markers t))
	       (return (values thresholds threshold-times))
	     when (> time (car d2vdt2-max-times)) collect voltage into thresholds and collect time into threshold-times and do (setq d2vdt2-max-times (cdr d2vdt2-max-times))))))))

(setf (symbol-function 'element-spike-thresholds) (symbol-function 'spike-thresholds)) ; Backward compatibility
(setf (documentation 'element-spike-thresholds 'function) "")

(defun find-maxs (values reference-times values-time-base &key (before/after :subsequent) (min-max-value 0.0) (window 2.0) (start-time 0.0))
  ;; In reference to each time in REFERENCE-TIMES return the preceeding or following (according to BEFORE/AFTER) maximum value
  ;; (greater than or equal to MIN-MAX-VALUE) during a time period of WINDOW (in ms) in VALUES. VALUES-TIME-BASE must be either a
  ;; list of single floats, or a single single float, in which case it is taken as the dt.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((num-values (length values))
	 (values-time-base (typecase values-time-base
			     (cons values-time-base)
			     (t (list-of-nums num-values start-time values-time-base))))
	 (reference-times (case before/after
			    (:subsequent reference-times)
			    (:previous (reverse reference-times))))
	 (MAX-VALUE 0.0) (reference-time 0.0) max-time LOOKING REVERSE-MAX-VALUES reverse-max-value-times)
    (declare (single-float max-value min-max-value reference-time))
    (loop for value single-float in (case before/after
				      (:subsequent values)
				      (:previous (reverse values)))
	  for time single-float in (case before/after
				     (:subsequent values-time-base)
				     (:previous (reverse values-time-base)))
	  for count fixnum from 1
	  unless (or looking reference-times) do (return (case before/after
							   (:subsequent (values (reverse reverse-max-values) (reverse reverse-max-value-times)))
							   (:previous (values reverse-max-values reverse-max-value-times))))
	  when looking do
	  ;; (format t "looking at value ~A, current max ~A time ~A~%" value max-value time)
	  (if (or (= count num-values)
		  (and (> max-value min-max-value)
		       (> (abs (- time reference-time))	window)))
	      (progn			; (format t "at time ~A, max-value ~A, max-time ~a~%" time max-value max-time)
		(push max-value reverse-max-values)
		(push max-time  reverse-max-value-times)
		(setq looking nil))
	      (progn			; (format t "value ~A, max-value ~A~%" value max-value)
		(when (and (> value max-value) (> value min-max-value))
		  (setq max-value value max-time time))))
	  else when (case before/after
		      (:subsequent (> time (the sf (car reference-times))))
		      (:previous (< time (the sf (car reference-times)))))
	  do				; (format t "starting to look - ~A ~A~%" (car reference-times) value)
	  (setq reference-time (car reference-times))
	  (setq reference-times (cdr reference-times)
		looking t
		max-time reference-time
		max-value value)
	  finally (return (case before/after
			    (:subsequent (values (reverse reverse-max-values) (reverse reverse-max-value-times)))
			    (:previous (values reverse-max-values reverse-max-value-times)))))))

(defun spike-frequency (element &key (spike-threshold -20.0) (supra-threshold-duration-min 0.1) sub-threshold-time spike-times
				  model-type data-list (time-base (current-sim-plot-time-list)) (start-time 0) (end-time *user-stop-time*))
  "Spike firing frequency in Hz between START-TIME and END-TIME [ms, default 0 and *USER-STOP-TIME*, respectively].
Spikes from SPIKE-TIMES or detected from the voltage of the soma or segment associated with ELEMENT of MODEL-TYPE. Keyword arguments for spike detection
as used by the SPIKE-TIMES function."
  (declare (ignore sub-threshold-time))
  (let ((spike-times (or SPIKE-TIMES
			 (spike-times element :model-type model-type :data-list data-list :time-base time-base :spike-threshold spike-threshold
				      :supra-threshold-duration-min supra-threshold-duration-min))))
    (* 1000 (/ (loop for spike-time in spike-times when (< start-time spike-time end-time) sum 1)
	       (- end-time start-time)))))

(setf (symbol-function 'element-firing-frequency) (symbol-function 'spike-frequency)) ; Backward compatibility
(setf (documentation 'element-firing-frequency 'function) "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element data analysis functions based on ELEMENT-EXTREME and thus DATA-EXTREME.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun data-extreme (data-list &key (what :value) ; Also :SLOPE, :1ST-DERIVATIVE (same as :SLOPE), :2ND-DERIVATIVE.
				 dt maxp (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))))
  "Analysis of DATA-LIST, corresponding to WHAT [default :VALUE].
Analysis based on time base of step DT, if supplied, otherwise from times in TIME-LIST. The maximum [respectively minimum], according to MAXP, of WHAT [:SLOPE,
:1ST-DERIVATIVE (same as :SLOPE), :2ND-DERIVATIVE, :VALUE \(default\)], within a time window between MIN-TIME and MAX-TIME [same units as DT or
TIME-LIST]. The default value of MAX-TIME is the last time given in the TIME-LIST. Returns as values the extreme value and the time for which that
value was detected. If no extreme was detected, then returns as values NIL and MIN-TIME."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((min-time (s-flt min-time)) (max-time (s-flt max-time))
	extreme (extreme-time 0.0) (time-n-1 0.0) (time-n-2 0.0) (data-n-1 0.0) (data-n-2 0.0)
	(dt (when dt (s-flt dt)))
	(time-n 0.0))
    (declare (single-float extreme-time time-n-1 time-n-2 data-n-1 data-n-2 time-n min-time max-time))
    (loop for data-n single-float in data-list
       for count fixnum from 0
       do (setq time-n (the sf (if dt
				   (if (= count 0) 0.0 (+ time-n (the sf dt)))
				   (nth count time-list))))
	 (when (> time-n min-time)
	   (let ((current-result
		  (case what
		    (:2ND-DERIVATIVE
		     (unless (< count 2)
		       (/ (- (/ (- data-n data-n-1)
				(the sf (or dt (- time-n time-n-1))))
			     (/ (- data-n-1 data-n-2)
				(the sf (or dt (- time-n-1 time-n-2)))))
			  (- time-n time-n-2))))
		    ((:1ST-DERIVATIVE :SLOPE)
		     (unless (< count 1)
		       (/ (- data-n data-n-1)
			  (the sf (or dt (- time-n time-n-1))))))
		    (:VALUE data-n))))
	     (when (and current-result (or (not extreme) (funcall (if maxp #'< #'>) (the sf extreme) (the sf current-result))))
	       (setq extreme (the sf current-result)
		     extreme-time (case what
				    (:2nd-derivative time-n-1)
				    ((:1st-derivative :slope) (/ (+ time-n time-n-1) 2))
				    (t time-n))))))
	 (setq data-n-2 data-n-1
	       data-n-1 data-n
	       time-n-2 time-n-1
	       time-n-1 time-n)
	 (when (> time-n max-time) (return (values extreme (or extreme-time min-time))))
       finally (return (values extreme (or extreme-time min-time))))))

(defun element-extreme (element &key data-type dt maxp (what :value) data-list model-type (time-list (current-sim-plot-time-list))
			(MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))))
  "Extreme values of data associated with ELEMENT.
Data taken from DATA-LIST, if supplied, otherwise from data of DATA-TYPE of ELEMENT of MODEL-TYPE, call the function DATA-EXTREME with remaining arguments."
  (data-extreme (or data-list (if dt (element-data-dted element dt data-type model-type time-list) (element-data element data-type)))
		:MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :maxp maxp :what what :time-list time-list))

(defun element-amplitude (element &key data-type dt (time-list (current-sim-plot-time-list))  (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST)))
			  data-list model-type negative-p base-level)
  "Amplitude of data associated with ELEMENT.
Amplitude units appropriate for the type of data in DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of ELEMENT. Additional arguments are as for DATA-AMPLITUDE."
  (data-amplitude (or data-list (if dt (element-data-dted element dt data-type model-type time-list) (element-data element data-type)))
		  :MIN-TIME (s-flt MIN-TIME) :MAX-TIME (s-flt MAX-TIME) :dt dt :time-list time-list
		  :negative-p negative-p :base-level base-level))

(defun data-amplitude (data-list &key dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) negative-p base-level)
  "Amplitude of DATA-LIST.
The reference level is given by BASE-LEVEL [assumed to be in the units corresponding to that of the data] if supplied, otherwise the reference is taken
as the minimum \(respectively maximum\) when NEGATIVE-P is NIL, \(respectively T\). The measured event amplitude is either the maximum or minimum value
thereafter, depending on NEGATIVE-P."
  (let ((base-level
	 (or base-level (apply (if negative-p 'data-max 'data-min) (list :data-list data-list :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
	(extreme
	 (apply (if negative-p 'data-min 'data-max) (list :data-list data-list :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
    (abs (- extreme base-level))))

(defun element-10-90-rise-time (element &key data-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST)))
				data-list model-type negative-p base-level)
  "Returns the time in milliseconds for the 10% to 90% rise time applied to DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT. Remaining arguments are as for ELEMENT-AMPLITUDE and ELEMENT-EXTREME."
  (let* ((data-list
	  (or data-list
	      (if dt (element-data-dted element dt data-type model-type time-list) (element-data element data-type))))
	 (time-list (if dt (loop for time from min-time to max-time by dt collect time) time-list))
	 (base-level (or base-level
			 (apply (if negative-p 'element-max 'element-min)
				(list element :data-type data-type :data-list data-list :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
	 (amp (element-amplitude element :data-type data-type :base-level base-level
				 :data-list data-list :negative-p negative-p :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))
	 (10%amp (+ base-level (* (if negative-p -1 1) 0.1 amp)))
	 (90%amp (+ base-level (* (if negative-p -1 1) 0.9 amp)))
	 10%time)
    (loop for data in data-list
	  for time in time-list
	  when (and (<= min-time time max-time)
		    (not 10%time) (if negative-p (< data 10%amp) (> data 10%amp)))
	  do (setq 10%time time)
	  when (and (<= min-time time max-time) (if negative-p (< data 90%amp) (> data 90%amp)))
	  do (return (- time 10%time)))))

(defun element-10-90-slope (element &key data-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST)))
			    data-list model-type negative-p base-level)
  "Returns the slope in units/ms for the 10% to 90% rise time applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT. Remaining arguments are as for ELEMENT-AMPLITUDE and ELEMENT-EXTREME."
  (let* ((data-list
	  (or data-list
	      (if dt (element-data-dted element dt data-type model-type time-list) (element-data element data-type))))
	 (time-list (if dt (loop for time from min-time to max-time by dt collect time) time-list))
	 (base-level (or base-level (apply (if negative-p 'element-max 'element-min)
					   (list element :data-type data-type :data-list data-list
						 :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
	 (amp (element-amplitude element :data-type data-type :base-level base-level
				 :data-list data-list :negative-p negative-p :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))
	 (10%amp (+ base-level (* (if negative-p -1 1) 0.1 amp)))
	 (90%amp (+ base-level (* (if negative-p -1 1) 0.9 amp)))
	 10%time)

    (loop for data in data-list
	  for time in time-list
	  when (and (<= min-time time max-time)
		    (not 10%time) (if negative-p (< data 10%amp) (> data 10%amp)))
	  do (setq 10%time time)
	  when (and (<= min-time time max-time) (if negative-p (< data 90%amp) (> data 90%amp)))
	  do (return (unless (= 0 (- time 10%time))
		       (/ (* 0.8 amp) (- time 10%time)))))))

(defun element-max-slope (element &key data-type model-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Returns the maximum slope in units/ms and the time for the max as values, applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme
   element :data-type data-type :model-type model-type :min-time min-time :max-time max-time :data-list data-list :dt dt :maxp t :what :slope :time-list time-list))

(defun data-max-slope (&key  dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Same as ELEMENT-MAX-SLOPE, except that DATA-LIST must be supplied."
  (data-extreme data-list :min-time min-time :max-time max-time :dt dt :maxp t :what :slope :time-list time-list))

(defun element-min-slope (element &key data-type model-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Returns the minimum slope in units/ms  and the time for the min as values, applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :model-type model-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp nil :what :slope :time-list time-list))

(defun data-min-slope (&key dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Same as ELEMENT-MIN-SLOPE, except that DATA-LIST must be supplied."
  (data-extreme data-list :min-time min-time :max-time max-time :dt dt :maxp nil :what :slope :time-list time-list))

(defun element-max (element &key data-type model-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (car (last time-list))) data-list)
  "Returns the maximum  and the time for the max as values, applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of ELEMENT. Remaining
arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :model-type model-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp t :what :value :time-list time-list))

(defun data-max (&key  dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Same as ELEMENT-MAX, except that DATA-LIST must be supplied."
  (data-extreme data-list :min-time min-time :max-time max-time :dt dt :maxp t :what :value :time-list time-list))

(defun element-min (element &key data-type model-type dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Returns the minimum and the time for the min as values, applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of ELEMENT of
MODEL-TYPE. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :model-type model-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp nil :what :value :time-list time-list))

(defun data-min (&key dt (time-list (current-sim-plot-time-list)) (MIN-TIME 0.0) (MAX-TIME (CAR (LAST TIME-LIST))) data-list)
  "Same as ELEMENT-MIN, except that DATA-LIST must be supplied."
  (data-extreme data-list :min-time min-time :max-time max-time :dt dt :maxp nil :what :value :time-list time-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-integrated-data (element &optional data-type model-type)
  "According to the plot data and time points of the last simulation, returns the sum of the integrals of the data of type DATA-TYPE of each element in
ELEMENT, of element type MODEL-TYPE, where the default DATA-TYPE is given in the documentation for the ELEMENT-DATA function. ELEMENT can either be a
single element or a list of elements."
  (loop for element in (coerce-to-list element) sum
	(let ((data (element-data element data-type model-type)))
	  (if data
	      (INTEGRATE-X-Y data (current-sim-plot-time-list))
	      0.0))))

(defun variance-of-wave (y-sequence x-sequence)
  (let (last-x delta-x)
    (loop for y in y-sequence
	  for x in x-sequence
	  when last-x do (setq delta-x (abs (- x last-x)))
	  do (setq last-x x)
	  when delta-x sum (* y delta-x) into integral and sum (* x x y delta-x) into squared-xs and sum (* x y delta-x) into mean-xs
	  finally (return
		   (values
		    (/ (- squared-xs (/ (square mean-xs) integral)) integral) ; variance
					; (/ squared-xs integral)	;
					; (/ (square mean-xs) integral)
		    (/ mean-xs integral) ; mean
		    integral		; integral
		    )))))

(defun gaussian-wave (x-sequence mean variance) (loop for x in x-sequence collect (gaussian x mean variance)))

(defun normalize-wave-area (wave integral)
  (loop for val single-float in (sequence-to-list wave) collect (the sf (/ val (the sf integral)))))

#|
(multiple-value-bind (xy-lists labels)
    (extract-plot-window-data)
  (let ((y-lists (loop for xy-list in xy-lists collect (nth 1 xy-list))))
    (CROSS-CORRELATION y-lists :labels labels :dc-bias '(2.1 2.1)
		       :delta-t 1.0 :return-result nil :normalize t :print-std-dev t :plot-gaussian-fit t :plot-result t)))
|#

(defun hwhh (data time-base &optional max-reference-time)
  "Simple estimate of the half width at half height of the numeric sequence DATA, indexed by TIME-BASE, which may either be a single value indicating a
time step dt, or a numeric sequence. The algorithm assumes unimodal data, and is therefore sensitive to noise. Maximum of DATA is determined internally, unless a
MAX-REFERENCE-TIME is supplied, and then the corresponding value from DATA is taken as the max. Returns as values the hwhh and the data value referenced as
the maximum"
  (let* ((data (s-flt-list data))
	 (time-sequence (if (sequencep time-base) (s-flt-list time-base) (list-of-nums (length data) 0.0 time-base)))
	 (half-height (if max-reference-time
			  (loop for y in data
				for time in time-sequence
				when (>= time max-reference-time) do (return (/ y 2.0)))
			  (/ (loop for y in data maximizing y) 2.0)))
	 first-HH-time second-HH-time)
    (loop for y in data
	  for time in time-sequence
	  when (and (not first-HH-time)
		    (>= y half-height))
	  do (setq first-HH-time time)
	  when (and first-HH-time
		    (not second-HH-time)
		    (<= y half-height))
	  do (setq second-HH-time time)
	  (return (values (- second-HH-time first-HH-time) (* 2 half-height))))))

(defun half-width-at-half-max (time-base data max-reference-time) (hwhh data time-base max-reference-time))

(defun auto-correlation-duration (time-base Rh max-reference-time)
  ;; Rh is the auto-correlation function. See Siebert, p.505
  (let (maximum last-time)
    (loop for y in Rh
	  for time in (if (consp time-base) time-base (list-of-nums (length Rh) 0.0 time-base))
	  when last-time
	  sum (* y (- time last-time)) into integral
	  do (setq last-time time)
	  when (and (not maximum) (>= time max-reference-time))	; Rh(0)
	  do (setq maximum y)		; (format t "max ~A time ~A~%" y time)
	  finally (return (/ integral maximum)))))

(defun cross-correlation (waves &key wave2s labels return-result (normalize t) plot-result title x-label y-label (print-std-dev t) plot-gaussian-fit (delta-t 1.0) (dc-bias 0.0))
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((delta-t (s-flt delta-t)))
    (declare (single-float delta-t))
    (loop for wave in (if (consp (car waves)) waves (list waves))
	  for wave2 in (if (consp (car (or wave2s waves))) (or wave2s waves) (list (or wave2s waves)))
	  for count from 0
	  for bias = (s-flt (case dc-bias
			      (:estimate (car wave))
			      (t (if (consp dc-bias) (nth count dc-bias) (or dc-bias 0.0)))))
	  collect
	  (let* ((wave (if (zerop (the sf bias))
			   (sequence-to-list wave)
			   (mapcar #'(lambda (val)
				       (declare (optimize (safety 1) (speed 3) (space 1)))
				       (- (the sf val) (the sf bias)))
				   (sequence-to-list wave))))
		 (wave-length (length (the cons wave)))
		 (reverse-wave (if wave2
				   (if (zerop (the sf bias))
				       (sequence-to-list wave2)
				       (mapcar #'(lambda (val)
						   (declare (optimize (safety 1) (speed 3) (space 1)))
						   (- (the sf val) (the sf bias)))
					       (sequence-to-list wave2)))
				   wave))
		 (reverse-wave-length (length (the cons reverse-wave)))
		 (duration (max wave-length reverse-wave-length))
		 (x-sequence (typecase delta-t
			       (cons delta-t)
			       (number
					;(list-of-sf-nums
					;(* delta-t (+ wave-length reverse-wave-length))
					;(* delta-t (- reverse-wave-length))
					;delta-t)

				(loop for x single-float from (* delta-t (- reverse-wave-length)) to (* delta-t wave-length) by delta-t collect x))))
		 (result (loop for shift fixnum from (- reverse-wave-length) to wave-length collect
			       (let ((left-wave (if (< shift 0) reverse-wave wave))
				     (right-wave (if (< shift 0) wave reverse-wave)))
				 (do ((left-wave left-wave (cdr left-wave))
				      (count 0 (1+ (the fn count))))
				     ((= (the fn count) (abs shift))
				      (/ (loop for left-value single-float in left-wave
					       for right-value single-float in right-wave
					       sum (* left-value right-value) into out single-float
					       finally (return out))
					 duration))))))
		 (result-at-0 (loop for val in result
				    for time single-float in x-sequence
				    when (>= time 0) do (return val))))
	    (list result x-sequence result-at-0))
	  into results-x-seqs
	  finally
	  (let ((return-value
		 (loop for result-x-seq in results-x-seqs
		       collect (let ((x-seq (nth 1 result-x-seq))
				     (result-seq (nth 0 result-x-seq)))
				 (multiple-value-bind (variance mean integral)
				     (variance-of-wave result-seq x-seq)
				   (list (if normalize (normalize-sequence result-seq integral) result-seq)
					 x-seq mean variance (half-width-at-half-max x-seq result-seq 0.0) (auto-correlation-duration x-seq result-seq 0.0))))
		       into normal-results-x-seq-mean-var-half-width-auto-corr-durations
		       finally
		       (when plot-result
			 (loop for normal-results-x-seq-mean-var-half-width-auto-corr-duration
			       in normal-results-x-seq-mean-var-half-width-auto-corr-durations
			       for count from 0
			       for mean = (nth 2 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for variance = (nth 3 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for half-width = (nth 4 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for acrl-duration = (nth 5 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for orig-label = (nth count labels)
			       collect (nth 0 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into out
			       collect (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into x-sequences
			       collect (format nil "~A CC" orig-label) into new-labels
			       when (and (> variance 0) plot-gaussian-fit)
			       collect (gaussian-wave (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration) mean variance)
			       into out and
			       collect (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into x-sequences
			       and collect (format nil "~A Gaussian" orig-label) into new-labels
			       when print-std-dev
			       collect (format nil "~A Std dev:~a HWHM:~,3f, ACdur: ~,3f" orig-label (when (> variance 0) (sqrt variance) " Neg var") half-width acrl-duration)
			       into std-dev-strings
			       finally
			       (let ((plot-win (plot-timed-data out new-labels x-sequences :x-label x-label :y-label y-label :title title)))
				 (when print-std-dev (add-comment plot-win (concatenate-string-list std-dev-strings :lf-count 1))))))
		       (return (values normal-results-x-seq-mean-var-half-width-auto-corr-durations)))))
	    (when return-result (return return-value))))))

(defun integrate-wave (wave &optional (delta-t 1.0) (x-0 0.0))
  "Given the sequence WAVE, assumed to be spaced evenly by DELTA-T with respect to the independent variable, returns a list which is the
cumulative integral of WAVE, with the initial conditions given by the optional argument X-0."
  (let (output)
    (do ((wave (sequence-to-list wave) (cdr wave))
	 (x (+ x-0 (* (car wave) delta-t)) (+ x (* (car wave) delta-t))))
	((null wave) (reverse output))
      (push x output))))

(defun clip-wave (wave min max)
  (loop for val in (sequence-to-list wave) collect (min max (max val min))))

(defun list-mins (wave &optional (delta-t 1.0) (min 0.0) (min-min-time 0.0))
  "Operating on the sequence WAVE, with time steps DELTA-T, returns two lists as values, comprising the MIN negative-going and positive-going crossing times,
respectively, whenever the duration framing a particular negative-positive pair of MIN crossings is greater than MIN-MIN-TIME."
  (let (min-reached min-end-times min-begin-times)
    (loop for val in (sequence-to-list wave)
	  for time from 0 by delta-t
	  do (if min-reached
		 (when (> val min)
		   (setq min-reached nil)
		   (if (> (- time (car min-begin-times)) min-min-time)
		       (push time min-end-times)
		       (setq min-begin-times (cdr min-begin-times))))
		 (when (< val min)
		   (setq min-reached t)
		   (push time min-begin-times))))
    (list (reverse min-begin-times)(reverse min-end-times))))

(defun list-maxs (wave &optional (delta-t 1.0) (max 0.0) (min-max-time 0.0))
  "Operating on the sequence WAVE, with time steps DELTA-T, returns two lists as values, comprising the MAX positive-going and negative-going crossing times,
respectively, whenever the duration framing a particular positive-negative pair of MAX crossings is greater than MIN-MAX-TIME."
  (let (max-reached max-end-times max-begin-times)
    (loop for val in (sequence-to-list wave)
	  for time from 0 by delta-t
	  do (if max-reached
		 (when (< val max)
		   (setq max-reached nil)
		   (if (> (- time (car max-begin-times)) min-max-time)
		       (push time max-end-times)
		       (setq max-begin-times (cdr max-begin-times))))
		 (when (> val max)
		   (setq max-reached t)
		   (push time max-begin-times))))
    (list (reverse max-begin-times)(reverse max-end-times))))

(defun frame-min-maxs (wave max-min-wave &optional (delta-t 1.0) (max 0.0) (min 0.0) (min-min-max-time 0.0) messages)
  "Strip epochs in the sequence WAVE (time step of DELTA-T) according to analysis applied to MAX-MIN-WAVE. Epochs are detected by applying LIST-MINS and
LIST-MAXS to WAVE, using MAX and MIN, respectively, and MIN-MIN-MAX-TIME as for the MIN-MIN-TIME and MIN-MAX-TIME arguments, respectively. Returns the
processed wave."
  (let* ((list-mins (LIST-MinS max-min-wave delta-t min min-min-max-time))
	 (list-maxs (LIST-MaxS max-min-wave delta-t max min-min-max-time))
	 (start-mins (car list-mins))
	 (start-maxs (car list-maxs))
	 (end-mins (cadr list-mins))
	 (end-maxs (cadr list-maxs))
	 within-neg-event within-pos-event output hold-value)
    (do* ((wave (sequence-to-list wave) (cdr wave))
	  (time 0.0 (+ time delta-t))
	  (this-x (car wave) (car wave)))
	 ((null wave))
      (when messages
	(when within-neg-event (format t "time:~A Within neg event...~%" time))
	(when within-pos-event (format t "time:~A Within pos event...~%" time)))
      (push (cond ((and			; (not within-pos-event)
		    (member time start-mins))
		   (setq within-neg-event t)
		   (if within-pos-event hold-value (setq hold-value this-x)))
		  ((and			; (not within-neg-event)
		    (member time start-maxs))
		   (setq within-pos-event t)
		   (if within-neg-event hold-value (setq hold-value this-x)))
		  ((member time end-mins)
		   (setq within-neg-event nil)
		   (if within-pos-event hold-value this-x))
		  ((member time end-maxs)
		   (setq within-pos-event nil)
		   (if within-neg-event hold-value this-x))
		  ((and hold-value (or within-neg-event within-pos-event))
		   hold-value)
		  (t this-x))
	    output))
    (reverse output)))

(defun upstroke-times (wave delta-t threshold)
  (let (over-threshold)
    (loop for dvdt in (differentiate-wave wave delta-t)
	  for time from 0.0 by delta-t
	  ;; do (format t "time ~A, dvdt ~A~%" time dvdt)
	  when (and (not over-threshold) (> dvdt threshold))
	  collect time into out and do (setq over-threshold t)
	  else do (unless (> dvdt threshold) (setq over-threshold nil))
	  finally (return out))))

(defun strip-spikes (wave delta-t threshold)
  (let ((upstroke-times (upstroke-times wave delta-t threshold))
	hold-value in-spike)
    (loop for value in (sequence-to-list wave)
	  for time from 0.0 by delta-t
	  when (and upstroke-times (= time (car upstroke-times)))
	  do (setq hold-value value upstroke-times (cdr upstroke-times) in-spike t)
	  and collect hold-value into out
	  else when (and in-spike (> value hold-value)) collect hold-value into out
	  else do (setq in-spike nil) and collect value into out
	  finally (return out))))

(defun find-zero-crossings (wave &optional (delta-t 1.0) (min-difference-from-0 0.0))
  "For data in the sequence WAVE, with time step DELTA-T, return a list of the zero-crossing times. True zero crossings are detected when they are framed by
alternating polarity amplitudes of at least MIN-DIFFERENCE-FROM-0 [default 0.0]."
  (let (output previous-ampltiude-above-minimum-p candidate-zero-crossing)
    (do* ((wave (sequence-to-list wave) (cdr wave))
	  (time 0.0 (+ time delta-t))
	  (time-1 0.0 time)
	  (x-2 nil x-1)
	  (x-1 nil this-x)
	  (this-x (car wave) (car wave)))
	 ((null wave) (reverse output))
      (when x-2
	(when (and candidate-zero-crossing
		   previous-ampltiude-above-minimum-p
		   (> (abs this-x) min-difference-from-0))
	  (push candidate-zero-crossing output)
	  (setq candidate-zero-crossing nil))
	(if (or (and (>= this-x x-1 x-2)
		     (> this-x 0.0 x-2))
		(and (<= this-x x-1 x-2)
		     (< this-x 0.0 x-2)))
	    (setq candidate-zero-crossing time)
	    (when (> (abs this-x) min-difference-from-0)
	      (setq previous-ampltiude-above-minimum-p t)))))
    (reverse output)))

;; ************* ************* ************* *************
;;
;;    2D Array Related Functions
;;
;; ************* ************* ************* *************

(defun nil-2d-array (array)
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j) nil)))
  array)

(defun add-offset-2d-array (array offset)
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j) (+ (aref array i j) offset))))
  array)

(defun add-offset-2d-array-float (array offset)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type single-float offset))
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j) (the sf (+ (the sf (aref array i j)) offset)))))
  array)

(defun array-vol (array grid-side &optional other-grid-side)
  "Given 2D ARRAY, with sides GRID-SIDE X OTHER-GRID-SIDE (when optional OTHER-GRID-SIDE supplied), GRID-SIDE X GRID-SIDE
otherwise, returns volume of array."
  (* grid-side (or other-grid-side grid-side)
     (loop for i from 0 to (1- (array-dimension array 0))
	   summing (loop for j from 0 to (1- (array-dimension array 1)) summing (aref array i j)))))

(defun array-min (array)
  (loop for i from 0 to (1- (array-dimension array 0))
	minimizing (loop for j from 0 to (1- (array-dimension array 1)) minimizing (aref array i j))))

(defun array-max (array)
  (loop for i from 0 to (1- (array-dimension array 0))
	maximizing (loop for j from 0 to (1- (array-dimension array 1)) maximizing (aref array i j))))

;; ************* ************* ************* *************
;;
;;   Array Interpolation Related Functions
;;
;; ************* ************* ************* *************

(proclaim '(inline interpolated-array-value))
(defun interpolated-array-value (waveform integer-time fractional-time &optional rectify)
  ;; Note that the units of the INTEGER-TIME and FRACTIONAL-TIME arguments are assumed to be consistent
  ;; with the time base of the WAVEFORM array, i.e. the index of the WAVEFORM array is in units of INTEGER-TIME.
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform)
	   (fixnum integer-time)
	   (single-float fractional-time))
  (let ((raw-value (if (= 0.0 fractional-time)
		       (aref waveform integer-time)
		       (+ (aref waveform integer-time)
			  (* fractional-time
			     (- (aref waveform (1+ integer-time))
				(aref waveform integer-time)))))))
    (if (and rectify (< raw-value 0)) 0.0 raw-value)))

(defun interpolated-array-value-double (waveform integer-time fractional-time &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform)
	   (fixnum integer-time)
	   (double-float fractional-time))
  (let ((raw-value (if (= 0.0d0 fractional-time)
		     (d-flt (aref waveform integer-time))
		     (+ (aref waveform integer-time)
			(* fractional-time
			   (- (aref waveform (1+ integer-time))
			      (aref waveform integer-time)))))))
    (if (and rectify (< raw-value 0)) 0.0d0 raw-value)))

(proclaim '(inline interpolated-array-value-with-delta-array))
(defun interpolated-array-value-with-delta-array (waveform delta-waveform integer-time fractional-time &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform delta-waveform)
	   (fixnum integer-time)
	   (single-float fractional-time))
  (let ((raw-value (if (= 0.0 fractional-time)
		     (aref waveform integer-time)
		     (+ (aref waveform integer-time) (* fractional-time (aref delta-waveform integer-time))))))
    (if (and rectify (< raw-value 0)) 0.0 raw-value)))

(proclaim '(inline double-interpolated-array-value-with-delta-array))
(defun double-interpolated-array-value-with-delta-array (waveform delta-waveform integer-time fractional-time &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type 1d-df waveform delta-waveform)
	   (fixnum integer-time)
	   (double-float fractional-time))
  (let ((raw-value (if (= 0.0d0 fractional-time)
		     (aref waveform integer-time)
		     (+ (aref waveform integer-time)
			(* fractional-time (aref delta-waveform integer-time))))))
    (if (and rectify (< raw-value 0)) 0.0d0 raw-value)))

(defun interpolated-value (value1 value2 fractional-time &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float fractional-time value1 value2))
  (let ((raw-value (+ value1 (* fractional-time (- value2 value1)))))
    (if (and rectify (< raw-value 0)) 0.0 raw-value)))

(defun interpolated-value-double (value1 value2 fractional-time &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float value1 value2)
	   (double-float fractional-time))
  (let ((raw-value (+ value1 (* fractional-time (- value2 value1)))))
    (if (and rectify (< raw-value 0)) 0.0d0 raw-value)))

(proclaim '(inline generic-interpolated-array-value))
(defun generic-interpolated-array-value (array index &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let ((raw-value (typecase index
		     (fixnum (aref (the vec-flt array) index))
		     (float (multiple-value-bind (integer-part fractional-part)
				(truncate (the sf index))
			      ;; (truncate (the sf index))
			      (interpolated-array-value (the vec-flt array) integer-part fractional-part)))
		     (t 0.0))))
    (if (and rectify (< raw-value 0))
      (typecase index
	(fixnum 0)
	(t 0.0))
      raw-value)))

(proclaim '(inline interpolated-array-slice-value))
(defun interpolated-array-slice-value (waveform-array waveform-length slice-index integer-time fractional-time fractional-time=0-OR-do-not-interpolate &optional rectify)
  (declare (optimize (safety 1) (speed 3) (space 0)(debug 0))
	   (type 2dfloat waveform-array)
	   (fixnum integer-time slice-index waveform-length)
	   (single-float fractional-time))
  (let* ((1+integer-time (1+ integer-time))
	 (raw-value
	  (if (or fractional-time=0-OR-do-not-interpolate (>= 1+integer-time waveform-length))
	    (aref waveform-array slice-index integer-time)
	    (+ (aref waveform-array slice-index integer-time)
	       (* fractional-time
		  (- (aref waveform-array slice-index 1+integer-time) (aref waveform-array slice-index integer-time)))))))
    (declare (fixnum 1+integer-time))
    (if (and rectify (< raw-value 0)) 0.0 raw-value)))

;; ************* ************* ************* *************
;;
;;   Array Information Functions
;;
;; ************* ************* ************* *************

(defun wave-name-from-type (type)
  (case type
    (spatial-rf-function "Spatial RF Function")
    (impulse-FUNCTION "Impulse Function")
    (linear-impulse-FUNCTION "Linear Stage Impulse Function")
    (waveform-function "Conductance Reference Waveform")
    (static-voltage-dependence-function "Static Voltage Dependence")
    (t (format nil "~A" type))))

(format t "~{ ~A ~}" '(1 2 3 4 5))
(defun 2d-spatial-rf-info (function-list)
  (format t "Spatial RF Function: ~a" (car function-list))
  (when (car function-list)
    (format t " - ARGS:")
    (format t "~{ ~A ~}" (cdr function-list))
    (format t "~%")))

(defun 1d-impulse-info (impulse-function-list)
  (format t "Impulse Function ")
  (document-function-args impulse-function-list))

(defun print-waveform-from-waveargs (waveargs &optional (indent 0))
  (when waveargs
    (let* ((dummy1 (car waveargs))
	   (waveargs (cdr waveargs))
	   (dummy2 (or (cdr-assoc 'step waveargs) 0.2))
	   (dummy3 (or (cdr-assoc 'tau waveargs) 10.0))
	   (dummy4 (or (cdr-assoc 'duration waveargs) 1.0))
	   (dummy10 (or (cdr-assoc 'delay waveargs) 0.0)))
      (print-spaces t indent)
      (format t "~A Waveform - ~%" dummy1)
;      (format t "Time step ~ams, Delay for start of waveform ~ams, Duration ~ams~%"  dummy2 dummy10 dummy4)
      (print-spaces t indent)
      (loop for arg in waveargs
	    for arg-count from 1
	    do (format t "~A: ~A" (car arg) (cdr arg))
	    when (< arg-count (length waveargs)) do (format t ", ")))))

;; Misc functions for various waveforms

(defun add-delay-to-waveform (waveform delay &optional (waveform-time-step 1.0) (delay-value 0.0))
  "Adds a series of numbers, given by DELAY-VALUE [default 0.0] to the head of the sequence WAVEFORM.  The length of this series
is given by DELAY divided by WAVEFORM-TIME-STEP [default 1.0].  The returned sequence is of the same type [cons or array] as the
original WAVEFORM."
  (let* ((waveform-arrayp (arrayp waveform))
	 (new-seq (nconc (list-of-nums (/ delay waveform-time-step) delay-value 0.0)
			 (sequence-to-list waveform))))
    (if waveform-arrayp (s-flt-array new-seq) new-seq)))

(defun extract-waveform-function-interval (funspec &optional function-args)
  (let ((parsed-args (extract-funspec-args funspec function-args t)))
    (or (cadr (or (find 'step parsed-args :key 'car)
		  (find 'interval parsed-args :key 'car)
		  (find 'grid-size parsed-args :key 'car)))
	; *default-waveform-step*
	)))

(defun extract-waveform-function-delay (funspec &optional function-args)
  (let ((parsed-args (extract-funspec-args funspec function-args t)))
    (and nil
	 (or (cadr (or (find 'delay parsed-args :key 'car)
		       (find 'start parsed-args :key 'car)))
					; 0.0
	     ))))

(defun waveform-menu (&optional funspec comment (use-menus t))
  (let* ((*automatic-run* (or *automatic-run* (not use-menus)))
	 (function-name (if (consp funspec) (car funspec) funspec))
	 new-function-name
	 (dummy1 (when funspec (read-from-string (format nil ":~A" (if (consp funspec) (car funspec) funspec)))))
	 (basics `(:exponential :double-exponential :impulse :alpha :double-alpha :sinewave))
	 (candidates (if funspec (delete-duplicates (cons dummy1 basics)) basics)))
    (choose-variable-values `((dummy1 "" :choose ,candidates :vertical)) :label "Choose Waveform Type" :text comment)
    (setq new-function-name (read-from-string (format nil "~A" dummy1)))
    (unless (eq new-function-name function-name)
      (setq funspec new-function-name
	    function-name new-function-name))
    (setq funspec (when function-name (cons function-name (edit-function-args funspec nil :extra-text comment))))
    (values (when (extract-function function-name) (apply (car funspec) (cdr funspec)))	; Function array
	    (extract-waveform-function-interval funspec)
	    (extract-waveform-function-delay funspec)
	    funspec)))

;; ************* ************* ************* *************
;;
;;   Some Specific Waveforms
;;
;; ************* ************* ************* *************

(defun zap-wave (&key (duration 0.0) (baseline-duration 0.0) (delta-t 1.0) ; milliseconds
		 (df/dt 1.0)		; Change in frequency argument (hz) per second. Note that actual instantaneous frequency at any given time is twice
					; that given by this slope.
		 (end-window-duration 0.0)
		 end-window-sigma
		 (amplitude 1.0)
		 (baseline 0.0) (offset 0.0)
		 (return-wave t)
		 plot-wave (plot-title "Zap Wave"))
  "Generate zap frequency-modulated sinwave, of length DURATION, time step DELTA-T [in milliseconds], and peak to peak AMPLITUDE, where the argument to the
SIN function is given by = [DF/DT * 2Pi * time-in-seconds^2], with the output adjusted by OFFSET. Modulated portion is preceeded and followed by a constant
value BASELINE for a period BASELINE-DURATION. Return wave when RETURN-WAVE is T [default]. A half gaussian window is applied to the last
END-WINDOW-DURATION [ms, default 0.0] portion of the modulated output, where the gaussian is parameterized by END-WINDOW-SIGMA [ms, default NIL]."
  (let* ((end-window-start-time (- duration baseline-duration baseline-duration end-window-duration))
	 (end-window-variance (when end-window-sigma (* 1.0 end-window-sigma end-window-sigma)))
	 (gaussian-normalizer (when end-window-sigma (/ (gaussian 0.0 0.0 end-window-variance))))
	 (out (loop for time from 0.0 to duration by delta-t collect
		    (if (or (< time baseline-duration)
			    (> time (- duration baseline-duration)))
			baseline
			(let* ((time-in-seconds (* (- time baseline-duration) 1e-3)) ; Convert time to seconds, starting after the baseline is finished
			       (temporal-frequency  (* time-in-seconds df/dt))
			       (angular-frequency (* 2 pi temporal-frequency)))
			  (+ OFFSET (* (if (and end-window-sigma (> time end-window-start-time)) (* gaussian-normalizer (gaussian time end-window-start-time end-window-variance)) 1.0)
				       amplitude
				       (sin (* angular-frequency time-in-seconds)))))))))
    (when plot-wave (plot-timed-data out nil delta-t :title plot-title :x-label "ms"))
    (when RETURN-WAVE out)))

(defun ramp (slope length &optional (dt 1) (start-time 0) (base 0))
  "Return a list of LENGTH single float values, corresponding to a time increment DT [default 1], with a reference value of BASE [default 0], increasing
after START-TIME [default 0] according to SLOPE."
  (loop for time from 0 by dt
	for count from 1 to length
	collect (s-flt (+ base
			  (if (< time start-time)  0
			      (* slope (- time start-time)))))))

(defun sinewave (&optional (amplitude 1.0) (duration *user-stop-time*) (frequency 1.0) &key (phase 0.0) (offset 0.0) (step 0.2) (start 0.0) zero-before-start)
  "FREQUENCY is in cycles per unit time, as given by STEP [default 0.2]. PHASE is in degrees. Returns a single-float array. Function times less than START
[default 0.0] return 0.0 when ZERO-BEFORE-START is T, otherwise OFFSET. Time argument given to the sin function is relative to START:

            sin [{2pi * FREQUENCY * (TIME - START)} + PHASE]

"
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((start (s-flt start))
	 (amplitude (s-flt amplitude))
	 (frequency (s-flt frequency))
	 (duration (s-flt duration))
	 (step (s-flt step))
	 (offset (s-flt offset))
	 (phase (deg-to-rad (s-flt phase)))
	 (array (make-array (list (round (/ (the sf duration) (the sf step)))) :element-type 'single-float)))
    (declare (single-float start phase))
    (loop for time single-float from 0.0 by (the sf step)
	  for index fixnum from 0
	  for val across array do
	  (setf (aref array index) (+ offset
				      (if (< time start)
					(if zero-before-start (- offset) 0.0)
					(* amplitude (sin (+ phase (* 2 pi-single (- time start) frequency))))))))
    array))

(defun sin-array (amplitude duration frequency &key (phase 0.0) (offset 0.0) (step 0.2) (start 0.0) zero-before-start) ;
  (sinewave amplitude duration frequency :phase phase :offset offset :step step :start start :zero-before-start zero-before-start))

(defun sigmoid-array (v-half slope vmin vmax vres)
  ;; Array length is given by (VMAX - VMIN) / VRES.
  (s-flt-array
   (loop for voltage from vmin to vmax by vres
	 collecting (/ 1.0 (+ 1.0 (exp-w-limits (max -50.0 (min 50.0 (* -1 slope (- voltage v-half))))))))))

(defun impulse-array (&optional (amplitude 1.0) (duration 1) (delay 0) (step 1.0))
  (let* ((length (round (/ duration step)))
	 (array (make-array length :element-type 'single-float))
	 (index (round (/ delay step))))
    (if (> index (1- length))
      (sim-error (format nil "Target impulse index ~D is too long for impulse array length ~D (delay ~a, duration ~A, step ~A)"
			 index length delay duration step))
      (setf (aref array index) (s-flt amplitude)))
    array))

(defun impulse (&optional (amplitude 1.0) (length 1) (delay 0) (step 1.0)) (impulse-array (s-flt amplitude) (s-flt length) (s-flt delay) (s-flt step)))

(defun pulse (delay pulse-duration amplitude total-duration step)
  "Return a single float pulse waveform [list] of length given by TOTAL-DURATION divided by STEP, which is 0.0 until DELAY, then AMPLITUDE for
PULSE-DURATION, then 0.0 for the rest of the list."
  (let ((amplitude (s-flt amplitude)))
    (loop for time from 0.0 by (s-flt step)
	  until (> time total-duration)
	  collect (cond
		    ((< time delay) 0.0)
		    ((< time (+ delay pulse-duration)) amplitude)
		    (t 0.0)))))

(defvar *wave-cutoff* (exp -6) "Relative max cut off value for various waveform creation functions, e.g. ALPHA-ARRAY.")
(defvar *alpha-cutoff-threshold-wrt-max* *wave-cutoff*)

(defun exponential-array-unit-area (&optional (tau 1.0) (step 1.0) length)
  "Returns an array filled with a decaying exponential, whose amplitude is adjusted so that its area is 1.0 and with time base increment given by STEP
 [default 1.0]. The length of array is given by LENGTH, if given, otherwise when the amplitude is less than
*WAVE-CUTOFF* times the AMPLITUDE."
  (let* ((original-exponential (exponential-array tau step length))
	 (area (* 1			; step
		  (loop for x single-float across original-exponential sum x))))
    (loop for index from 0 to (1- (length original-exponential)) do
	  (setf (aref original-exponential index)
		(/ (aref original-exponential index) area)))
    original-exponential))

(defun exponential-array (&optional (tau 1.0) (step 1.0) (length 0) (offset 0.0) (amplitude 1.0) (start 0.0))
  (exponential tau step length offset amplitude start))

(defun exponential (&optional (tau 1.0) (step 1.0) (length 0) (offset 0.0) (amplitude 1.0) (start 0.0))
  "Returns an array filled with a decaying exponential, of AMPLITUDE [default 1.0] and time base increment given by STEP [default 1.0], and OFFSET
 [0.0]. The length of array is given by LENGTH, if positive, otherwise when the amplitude without the OFFSET is less than *WAVE-CUTOFF* times the
AMPLITUDE. Values before the START [default 0.0] of waveform are given by OFFSET. Function time argument is referenced from START [default 0.0]."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((step (s-flt step))
	 (start (s-flt start))
	 (tau (s-flt tau))
	 (amplitude (s-flt amplitude))
	 (threshold (s-flt (* (abs amplitude) *wave-cutoff*)))
	 (value 0.0)
	 (length (and length
 		      (> length 0)
		      (round length))))
    (declare (single-float value threshold))
    (without-floating-underflow-traps
     (s-flt-array
      (loop for time single-float from 0.0 by step
	    for count fixnum from 1
	    do (setq value (if (< time start) 0.0 (exp-w-limits (* -1.0 (/ (- time start) tau)))))
	    collect (* amplitude value) into result
	    when (if length (= count (the fn length))
		     (and (< (abs value) threshold)
			  (> time tau)
			  (> time start)))
	    do (return (ADD-VAL-TO-FLOAT-LIST (s-flt offset) result)))))))

(defun double-exponential (&optional (tau-rise 1.0) (tau-fall 1.0) &key (amplitude 1.0) normalize (step 1.0) (length 0) (offset 0.0) (start 0.0))
  "Returns an array with the difference of two decaying exponentials:

          AMPLITUDE * [Exp(-t/TAU-FALL) - Exp(-t/TAU-RISE)] + OFFSET

AMPLITUDE has a default value of 1.0. Array time base increment is given by STEP [default 1.0].  OFFSET has a default value of 0.0. If NORMALIZE is
non-NIL, then the waveform is adjusted and the peak given by (AMPLITUDE + OFFSET). The length of array is given by LENGTH, if positive [default 0],
otherwise when the larger of the two exponential terms is less than *WAVE-CUTOFF*. Function time argument is referenced from START [default 0.0]."
;  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((step (s-flt step))
	 (start (s-flt start))
	 (tau-rise (s-flt tau-rise))
	 (tau-fall (s-flt tau-fall))
	 (offset (s-flt offset))
	 (amplitude (s-flt amplitude))
	 (threshold (s-flt *wave-cutoff*))
	 (falling-term 0.0)
	 (rising-term 0.0)
	 (difference-term 0.0)
	 (length (and length (> length 0) (round length))))
    (declare (single-float difference-term threshold rising-term falling-term))
    (multiple-value-bind (list max)
	(loop for time single-float from 0.0 by step
	      for count fixnum from 1
	      do (when (>= time start)
		   (setq falling-term (exp-w-limits (* -1.0 (/ (- time start) tau-fall)))
			 rising-term (exp-w-limits (* -1.0 (/ (- time start) tau-rise)))))
	      (setq difference-term
		    (if (< time start) 0.0
			(* amplitude (- falling-term rising-term))))
	      maximize (abs difference-term) into max single-float
	      collect difference-term into list
	      when (if length
		       (= count (the fn length))
		       (and (< (max rising-term falling-term) threshold) (> time tau-rise) (> time start)))
	      do (return (values list max)))
      (declare (single-float max))
      (s-flt-array (if (= max 0) list (ADD-VAL-TO-FLOAT-LIST (s-flt offset) (if normalize (SCALE-FLOAT-LIST (abs (/ amplitude max)) list) list)))))))

(defun double-exponential-array (&optional (tau-rise 1.0) (tau-fall 1.0) &key (amplitude 1.0) normalize (step 1.0) (length 0) (offset 0.0) (start 0.0))
  (double-exponential tau-rise tau-fall :amplitude amplitude :normalize normalize :step step :length length :offset offset :start start))

(defun alpha-integral (tau time-exponent)
  (* (factorial (round time-exponent))
     (expt tau (1+ time-exponent))))

(defun alpha-max-value (tau time-exponent)
  (/ (expt (* tau time-exponent) time-exponent)
     (exp-w-limits time-exponent)))

(defun alpha-array (&optional (tau 1.0) &key (TIME-EXPONENT 1) (adjustment :NORMALIZE) (step 1.0) (duration 0.0) (offset 0.0) (amplitude 1.0) (delay 0.0))
  (alpha tau :TIME-EXPONENT TIME-EXPONENT :adjustment adjustment :step step :duration duration :offset offset :amplitude amplitude :delay delay))

(defun alpha (&optional (tau 1.0) &key (TIME-EXPONENT 1) (adjustment :NORMALIZE) (step 1.0) (duration 0.0) (offset 0.0) (amplitude 1.0) (delay 0.0))
  "Returns an array of an alpha function (K * time^A * e^(-time/tau)) with time constant TAU [ms], starting at time = DELAY (value prior to DELAY is OFFSET
 [default 0.0]).  The exponent for the leading time coefficient, A, is given by TIME-EXPONENT [default 1] ADJUSTMENT [default :NORMALIZE] determines the
value of K as follows:

 :NORMALIZE  -  K set so that function amplitude is given by AMPLITUDE
 :UNIT-AREA  -  K set so that function area is given by AMPLITUDE
  ELSE       -  K = 1

STEP [ms, default 1.0] gives the time step of the array. The array length is given by DURATION [ms, default 0.0] if positive, otherwise the length is set
when the function value is less than *WAVE-CUTOFF* times the maximum. OFFSET adds an offset to the returned array, after the above constraints have been
met."
;;  (declare (optimize (safety 1) (speed 3) (space 1)))
  (without-floating-underflow-traps
   (let* ((tau (s-flt tau))
	  (offset (s-flt offset))
	  (duration (s-flt duration))
	  (delay (s-flt delay))
	  (amplitude (s-flt amplitude))
	  (TIME-EXPONENT (s-flt TIME-EXPONENT))
	  (step (s-flt step))
	  (adjustment-numeric-value (case adjustment
				      (:normalize (the sf (/ 1 (alpha-max-value tau time-exponent))))
				      (:unit-area (/ 1.0 (alpha-integral tau time-exponent)))
				      (t 1.0)))
	  (minimum (s-flt (abs (* (alpha-function-float tau tau adjustment-numeric-value) *wave-cutoff*)))))
     (declare (single-float adjustment-numeric-value minimum))
     (loop for time single-float from 0.0 by step
	   collect (+ offset (if (< time delay)
			       0.0
			       (* amplitude (the sf (alpha-function-float tau (- time delay) adjustment-numeric-value TIME-EXPONENT)))))
	   into values
	   when (if (and duration (> duration 0))
		  (>= time duration)
		  (and (> (- time delay) tau) (< (abs (- (the sf (car (last values))) offset)) minimum)))
	   do (return (s-flt-array values))))))

(defun alpha-list (&optional (tau 1.0) &key (adjustment :NORMALIZE) (step 1.0) (duration 0) (offset 0.0) (amplitude 1.0) (delay 0.0))
  "As in ALPHA-ARRAY, but returns a list."
  (array-to-list (alpha-array tau :adjustment adjustment :step step :duration duration :offset offset :delay delay :amplitude amplitude)))

(defun alpha-impulse (tau tau-power)
  (declare (ignore tau-power))
  (alpha-array tau))

(defun alpha-function (tau time &optional (adjustment :normalize))
  (* (if (numberp adjustment)
       adjustment
       (case adjustment
	 (:normalize (/ (exp-w-limits 1.0) tau))
	 (:unit-area (/ 1.0 (* tau tau)))
	 (t 1.0)))
     time
     (exp-w-limits (/ (- 0.0 time) tau))))

(defun alpha-function-float (tau time &optional (adjustment :normalize) (TIME-EXPONENT 1.0))
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float tau time TIME-EXPONENT))
  (* (the sf (if (numberp adjustment)
		 adjustment
		 (case adjustment
		   (:normalize (the sf (/ 1 (alpha-max-value tau time-exponent))))
		   (:unit-area (/ 1.0 (alpha-integral tau time-exponent)))
		   (t 1.0))))
     (if (= TIME-EXPONENT 1) time (expt time TIME-EXPONENT))
     (s-flt (exp-w-limits-double (/ (- 0.0d0 time) tau)))))

(defun general-alpha-function (tau time-power time)
  (* (/ (expt time time-power) tau)
     (exp-w-limits (/ (- 0.0 time) tau))))

(defun DOUBLE-ALPHA (&optional (tau1 1.0) (tau2 1.0) (alpha-proportion 1.0) &key (offset 0.0) (step 1.0) (tau1-alpha-area 1.0) (start 0.0))
  "Returns an array with the difference of two alpha functions, defined by TAU1 and TAU2 [ms] respectively. The area of the first alpha function is defined
with TAU1-ALPHA-AREA [default 1.0], with the relative area of the second given by ALPHA-PROPORTION, with a default of 1.0. Thus the total integral is equal
to (TAU1-ALPHA-AREA * (1 - ALPHA-PROPORTION)). The length of the array is determined when the value of the component with the longest time constant is less
than than *WAVE-CUTOFF* times its maximum. A correction term is added to the waveform in order to give the proper integral despite the truncated
length. The value of OFFSET [default 0.0] is added to the final waveform. Function time argument is referenced from START [default 0.0]."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((tau1 (s-flt tau1))
	 (tau2 (s-flt tau2))
	 (start (s-flt start))
	 (alpha-proportion (s-flt alpha-proportion))
	 (tau1-alpha-area (s-flt tau1-alpha-area))
	 (offset (s-flt offset))
	 (tau1-array (alpha-array tau1 :adjustment :unit-area :step step :amplitude tau1-alpha-area :delay start))
	 (tau2-array (alpha-array tau2 :adjustment :unit-area :step step :amplitude (* tau1-alpha-area ALPHA-PROPORTION) :delay start))
	 (max-tau1-array-index (the fn (1- (length tau1-array))))
	 (max-tau2-array-index (the fn (1- (length tau2-array))))
	 (max-index (the fn (max max-tau1-array-index max-tau2-array-index)))
	 (longest-array (if (> max-tau1-array-index max-tau2-array-index) tau1-array tau2-array)))
    (loop for index fixnum from 0 to max-index do
	  (setf (aref longest-array index)
		(- (the sf (if (> index max-tau1-array-index) 0.0 (aref tau1-array index)))
		   (the sf (if (> index max-tau2-array-index) 0.0 (aref tau2-array index))))))
    (let* ((actual-area (loop for index fixnum from 0 to max-index
			      sum (the sf (aref longest-array index))
			      into result single-float finally (return result)))
	   (area-adjustment (the sf (/ (- actual-area (* TAU1-ALPHA-AREA (- 1 ALPHA-PROPORTION)))
				       (1+ max-index)))))
      (loop for index fixnum from 0 to max-index
	    do (setf (aref longest-array index) (the sf (+ offset (aref longest-array index) (- area-adjustment)))))
      longest-array)))

(defun DOUBLE-ALPHA-array (&optional (tau1 1.0) (tau2 1.0) (alpha-proportion 1.0) &key (offset 0.0) (step 1.0) (tau1-alpha-area 1.0) (start 0.0))
  (DOUBLE-ALPHA tau1 tau2 alpha-proportion :offset offset :step step :tau1-alpha-area tau1-alpha-area :start start))

;;;;;;;;;;;;;; Misc Waveforms ;;;;;;;;;;;;;;;;;

(defvar *factorials*
  (loop for count from 1 to 30
	collect (* count (or (car (last out)) 1)) into out
	finally (return out)))

(defun GAMMA-DISTRIBUTION (length tau power)
  ;; Returns an array of length 'length of a normalized gamma distribution function.
  (list-to-array
   (loop for i from 0 to (1- length) collect
	 (* (/ 1.0 (* (nth power *factorials*) tau)) (expt (/ i tau) power) (exp-w-limits (- 0.0 (/ i tau)))))))

(defun DISTORTED-ALPHA (length tau1 tau2)
  ;; Convolution of two normalized alpha function with time constants TAU1 and TAU2.
  (list-to-array
   (loop for i from 0 to (1- length) collect
	 (let ((e1 (exp-w-limits (- 0.0 (/ i tau1))))
	       (e2 (exp-w-limits (- 0.0 (/ i tau2)))))
	   (/ (+ (* i (+ e1 e2)) (/ (* -2 tau1 tau2 (- e1 e2)) (- tau1 tau2)))
	      (* (- tau1 tau2) (- tau1 tau2)))))))


(defun DISTORTED-EXPONENTIAL (length tau1 tau2)
  ;; Convolution of a normalized alpha function with time constant TAU1 and a normalized exponential distribution with time constant TAU2.
  (list-to-array
   (loop for i from 0 to (1- length) collect
	 (let ((e1 (exp-w-limits (- 0.0 (/ i tau1))))
	       (e2 (exp-w-limits (- 0.0 (/ i tau2)))))
	   (/ (- (/ (* tau1 tau2 (- e2 e1)) (- tau2 tau1)) (* i e1)) (* tau1 (- tau2 tau1)))))))


(defun ZERO-TRIPLE-ALPHA (length tau1 tau2 tau3 alpha-proportion)
  ;; Returns an array of LENGTH of the difference between one alpha function and the sum of two others.  The first alpha function
  ;; is fast (TAU1) while the other two are slow. The second alpha function is faster (TAU2) than the third (TAU3).  The total
  ;; area is zero. The second alpha function contains 'alpha-proportion proportion of the total area of the negative functions.
  (list-to-array
   (loop for i from 0 to (1- length) collect
	 (- (*                        (/ i (* tau1 tau1)) (exp-w-limits (- 0.0 (/ i tau1))))
	    (* alpha-proportion       (/ i (* tau2 tau2)) (exp-w-limits (- 0.0 (/ i tau2))))
	    (* (- 1 alpha-proportion) (/ i (* tau3 tau3)) (exp-w-limits (- 0.0 (/ i tau3))))))))


(defun triple-alpha (length tau1 tau2 sus-tau &optional (transient-amplitude 1))
  ;; Returns an array of LENGTH of the difference of two alpha functions, with a total area of 0, plus another alpha function of area 1. The amplitude of
  ;; the alpha difference (the transient part) is scaled by the the optional argument TRANSIENT-AMPLITUDE.
  (list-to-array
   (loop for i from 0 to (1- length) collect
	 (+ (* transient-amplitude
	       (- (* (/ i (* tau1 tau1)) (exp-w-limits (- 0.0 (/ i tau1))))
		  (* (/ i (* tau2 tau2)) (exp-w-limits (- 0.0 (/ i tau2))))))
	    (* (/ i (* sus-tau sus-tau)) (exp-w-limits (- 0.0 (/ i sus-tau))))))))


(defun d-alpha-array (&optional length tau tau-power)
  ;; Returns an array of LENGTH of the derivitave of an alpha function whose maximum amplitude is 1.
  (declare (ignore length tau tau-power))
  (list-to-array '(1.0 0.0 -0.5 -0.3 -0.2)))


(defun trans-sus-array (&optional length tau tau-power)
  (declare (ignore length tau tau-power))
  (list-to-array '(1.25 0.0 -0.5 -0.3 -0.2)))

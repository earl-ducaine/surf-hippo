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



;;; SYS Source file: analysis.lisp

(in-package "SURF-HIPPO")

;;					
;; Main function for on-line analysis of simulation results
;;
;; related functions in misc.lisp and others


(defun apply-function-to-plot-DATA-plot-list-data (function)
  (no-nils
   (loop for plot-list-info in *PLOT-LISTS-INFO*
	 when (and (plot-list-info-enable-var plot-list-info)
		   (plot-list-info-structures plot-list-info)
		   (eq 'voltage (plot-list-info-structure-slot plot-list-info))
		   )
	 collect
	 (let ((plot-data 
		(retrieve-plot-data (list 
				     (list (symbol-value (plot-list-info-structures plot-list-info))
					   (plot-list-info-structure-slot plot-list-info))))))
	   (when plot-data
	     (funcall function
		      plot-data
		      (symbol-value (plot-list-info-names plot-list-info))))))))

(defun print-analysis ()
  (let (results)
    (cond-every
     (*print-linear-analysis*
      (loop for result-list in (APPLY-FUNCTION-TO-PLOT-DATA-PLOT-LIST-DATA 'INTEGRATE-PLOT-DATA)
	    do (loop for result in result-list do (push result RESULTS)))
      (loop for result in (INTEGRATE-plot-DATA (retrieve-plot-data (list (list *analysis-nodes* `voltage))) *analysis-nodes* *x-integrate-min* *x-integrate-max*)
	    do (push result RESULTS))
      (format t "~%"))
     (*print-nonlinear-analysis*
      (loop for result-list in (APPLY-FUNCTION-TO-PLOT-DATA-PLOT-LIST-DATA 'max-min-plot-data)
	    do (loop for result in result-list do (push result RESULTS)))
      (loop for result in (max-min-plot-DATA (retrieve-plot-data (list (list *analysis-nodes* `voltage))) *analysis-nodes* *x-integrate-min* *x-integrate-max*)
	    do (push result results))
      (format t "~%"))
     ((and *print-axon-spike-times* (> (hash-table-count (AXON-HASH-TABLE)) 0))
      (PRINT-AXON-SPIKE-TIMES))
     ((and *print-synapse-event-times* (> (hash-table-count (SYNAPSE-HASH-TABLE)) 0))
      (PRINT-SYNAPSE-EVENT-TIMES))
     ((and *print-synapse-total-events* (> (hash-table-count (SYNAPSE-HASH-TABLE)) 0))
      (print-synapse-total-events))
     (*DUMP-ANALYSIS-TO-FILE*
      (dump-analysis-file results)))
    results))

(defun plot-iv (&optional cells)
  (let* ((cells (or (element (coerce-to-list cells) 'cell) (cells)))
	 (ivs (mapcar 'i-v-characteristic cells))
	 *create-new-plot-windows*)
    (plot-timed-data (list (mapcar 'car ivs))
		     (loop for cell in cells collect (format nil "~A Somatic IV" (element-name cell)))
		     (cadar ivs)
		     :prompt-for-overlay t
		     :title (format nil "~ASomatic IV~:P" (if (= (length cells) 1) (format nil "~A " (cell-name (car cells))) ""))
		     :x-min -150.0 :x-max 50.0 :x-inc 25.0 :x-origin-tick t :y-origin-tick nil
		     :y-label "nA" :y-label-v-position :upper-right :x-label "mV"
		     :accomodate-overlays t :width 450 :height 350)))

(defun plot-ivs (&optional cells (use-menu t) use-one-window)
  (let* ((names (loop for cell in (or cells (cells)) collect (element-name cell 'cell)))
	 (*automatic-run* (or *automatic-run* (not use-menu)))
	 (cells (choose-list-values names (unless use-menu names) :label "Choose cells for IV curve(s)"))
	 (use-one-window (if (and (> (length cells) 1) use-menu)
			     (go-ahead-menu "Plot all IVs in one window?" "IV Details" use-one-window) 
			     use-one-window)))
    (if use-one-window (plot-iv cells) (mapcar 'plot-iv cells))))

(defun i-v-characteristic (&optional (cell *cell*))
  ;; Not very accurate - considers only somatic channels, corresponds to voltage clamp of soma, with the dendrites clamped at their resting potential.
  (set-circuit-elements-parameters)	; Just to be sure (for non-simulated cells).
  (initialize-simulation)	; To register the plotted conductances
  (let ((cell (element cell 'cell)))
    (let* ((soma (cell-soma cell))
	   (tree-load (if (cell-tree-p cell) (/ 1.0 (z-tree-cable-in-cell cell)) 0.0)))
      (loop for voltage from -150.0 to 50 by 1.0 do
	 ;; Set the soma voltage, update v-index.
	   (set-node-voltage (soma-node soma) voltage)
	   (init-all-nodes t)  
	 ;; Initialize particle states and calculate channel currents.
	   (when *enable-channels*
	     (eval-all-particles t)
	     (eval-all-conc-particles t)
	     (eval-all-channels))
	 ;; Add the passive soma current and soma channel currents.
	 collecting
	   (+ (* (soma-g-leak soma) (- voltage (soma-v-leak soma)))
	      (if (soma-include-shunt soma)
		  (* (soma-g-shunt soma) voltage)
		  0.0)
	      (* tree-load (- voltage (cell-type-v-leak-dendrite (cell-type cell))))
	      (if *enable-channels*
		  (loop for ch in (channels soma) when (channel-active-p ch) sum (get-channel-current ch))
		  0.0))
	 into nanoamps
	 collecting voltage into volts
	 finally (return (list (s-flt-list nanoamps) volts))))))



;;; MAX-MIN-PLOT-DATA prints out and returns the maxs and mins of each of the data sets of
;;; the plot-list, in the following format:
;;;
;;; (((NODE-NAME-1-SYMBOL (MAX maximum) (MIN minimum)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (MAX maximum) (MIN minimum)))
;;;
;;; The NODE-NAME-N-SYMBOLs are made of the labels in LABEL-LIST, with NODE prepended.
(defun max-min-plot-DATA (plot-data-list label-list &optional (x-min *x-integrate-min*) (x-max *x-integrate-max*) (units "mV"))
  (without-floating-underflow-traps
   (loop for plot-data in plot-data-list
	 for label in label-list
	 collect (let ((max-min (loop for val in plot-data
				      for time in *sim-reverse-plot-time-list*
				      when (and (if x-min (>= time x-min) t)
						(if x-max (<= time x-max) t))
				      maximize val into max
				      minimize val into min
				      finally (return (list max min)))))
		   (format t "Node ~a Max =  ~a, Min = ~a ~a ~%" label (tidy-number-format (car max-min)) (tidy-number-format (cadr max-min)) (string units))
		   (list (create-output-symbol "node" label) (cons 'Max (car max-min)) (cons 'min (cadr max-min)))))))


;;; INTEGRATE-PLOT-DATA prints out and returns the integrals of each of the data sets of
;;; the PLOT-DATA-LIST, in the following format when *AVERAGE-INTEGRALS* is NIL:
;;;
;;; (((NODE-NAME-1-SYMBOL (INTEGRAL integral) (BASE integral-base)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (INTEGRAL integral) (BASE integral-base))) 
;;;
;;; And in the following format when *AVERAGE-INTEGRALS* is T:
;;;
;;; (((NODE-NAME-1-SYMBOL (AVERAGE average) (BASE average-base)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (AVERAGE average) (BASE average-base))) 
;;;
;;; The NODE-NAME-N-SYMBOLs are made of the labels in LABEL-LIST, with NODE prepended.
;;; Note that the plot data lists must be in time-decreasing order.
(defun INTEGRATE-plot-DATA (plot-data-list label-list &optional (x-min *x-integrate-min*) (x-max *x-integrate-max*) (units "mV"))
  (without-floating-underflow-traps
   (loop for plot-data in plot-data-list
	 for label in label-list
	 collect (let ((result (integrate-x-y
				plot-data *sim-reverse-plot-time-list* :y-base *integral-base* :x-min x-min :x-max x-max :average *average-integrals*)))
		   (format t "Node ~a ~a =  ~a ~a (ref ~a) ~%"
			   label (if *average-integrals* "average" "integral")
			   (tidy-number-format result)
			   (if *average-integrals* (string units) (format nil "~A-ms" units))
			   *integral-base*)
		   (list
		    (create-output-symbol "node" label)
		    (cons (if *average-integrals* 'average 'integral) result)
		    (cons 'base *integral-base*))))))

(defun time-integral (data-list time-list &optional (data-offset 0) start stop)
  ;; If START and STOP not supplied, integrate over all times in TIME-LIST.
  (let ((integral 0.0))
    (do ((lt time-list (cdr lt))
	 (lv data-list (cdr lv)))
	((not (cdr lt)))
      (when (and (or (not stop) (> stop (car lt)))
		 (or (not start) (< start (cadr lt))))
      (setq integral (+ integral (* (- (car lv) data-offset) (- (cadr lt) (car lt)))))))
    integral))

(defun time-integral-float (data-list time-list &optional data-offset start stop threshold)
  ;; If START and STOP not supplied, integrate over all times in TIME-LIST.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((integral 0.0)
	(start (and start (s-flt start)))
	(stop (and stop (s-flt stop)))
	(threshold (and threshold (s-flt threshold)))	
	(data-offset (s-flt (or data-offset 0))))
    (declare (single-float integral data-offset))
    (do ((lt time-list (cdr lt))
	 (lv data-list (cdr lv)))
	((not (cdr lt)))
      (when (and (or (not stop) (> (the sf stop) (the sf (car lt))))
		 (or (not start) (< (the sf start) (the sf (cadr lt))))
		 (or (not threshold) (> (the sf (car lv)) (the sf threshold))))
	(setq integral (+ integral (* (- (the sf (car lv)) data-offset) (- (the sf (cadr lt)) (the sf (car lt))))))))
    integral))

(defun plot-voltage-sequence (sequence v-increment start-v label title &key prompt-for-overlay)
  (plot-timed-data sequence nil v-increment :delta-t-start start-v
		   :title title
		   :x-min start-v :x-origin-tick t :y-origin-tick nil
		   :y-label-v-position :upper-right
		   :y-label label :x-label "mV" :prompt-for-overlay prompt-for-overlay
		   :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*))

(defun fit-exp (data-list start stop &key (delta-t 1.0) plot-normalize-fit (max-amp-proportion 0.5) (min-amp-proportion 0.05) negative-p return-fit)
  "Fit single exponential to evenly sampled DATA-LIST points between START and STOP, with grid DELTA-T."
  (multiple-value-bind (min max)
      (loop for data in data-list
	    for time from 0.0 by delta-t
	    when (>= stop time start) maximize data into max and minimize data into min
	    finally (return (values min max)))
    (format t "min ~A max ~A~%" min max)
    (let* ((amp (abs (- max min)))
	   (pre-collect-data t)
	   post-collect-data
	   (max-amp (* max-amp-proportion amp))
	   (min-amp (* min-amp-proportion amp)))
      (multiple-value-bind (transformed-data-list transformed-time-list)

	  (loop for data in data-list	
		for time from 0.0 by delta-t
		when (and (>= time start) (<= time stop))
		do (format t "time ~A data ~A max ~A min-amp ~A max-amp ~A~%" time data max min-amp max-amp)
		(if (and (if negative-p (>= (- max data) min-amp) (>= (- data min) min-amp))
			 (if negative-p (<= (- max data) max-amp) (<= (- data min) max-amp))
			 (>= time start)
			 (not post-collect-data))
		    (if (> time stop)
			(setq post-collect-data t)
			(progn (print 'foo) (setq pre-collect-data nil)))
		    (if (not post-collect-data) (setq pre-collect-data t)))
		(format t "pre-collect ~A post-collect ~A~%" pre-collect-data post-collect-data)
		when (and (not post-collect-data) (not pre-collect-data))
		collect (/ (if negative-p (- max data) (- data min)) max-amp) into output-list
		and collect time into time-list
		finally (return (values output-list time-list)))
	
	(let ((transformed-time-start (car transformed-time-list)))
	  (loop for time in transformed-time-list
		for data in transformed-data-list
		sum (square (- time transformed-time-start)) into num
		sum (* (- time transformed-time-start) (log data)) into den
		finally
		(return
		  (let* ((tau (/ num den))
			 (fitted-normalized-data
			  (when (or plot-normalize-fit return-fit)
			    (loop for time in transformed-time-list
				  for data in data-list 
				  collect (exp (/ (- time transformed-time-start) tau)))))
			 (fitted-data
			  (when (or plot-normalize-fit return-fit)
			    (loop for data in fitted-normalized-data
				  collect (+ (* max-amp (if negative-p -1 1) data)
					     (if negative-p max min))))))
		    (when plot-normalize-fit
		      (plot-timed-data (list fitted-normalized-data transformed-data-list) (list "Normalised Fit" "Normalised Data") transformed-time-list
				       :title "Normalized Data" :y-max 1.0 :y-min 0.0))
		    (if return-fit
			(progn
			  (format t "tau ~A~%" (/ num den))
			  (list transformed-time-list fitted-data))
			(/ num den))))))))))

(defun convert-data-dt-lists (data-list old-time-base new-time-base &optional (output-order-data-time t) include-time-list new-time-base-is-length)
  "As CONVERT-DATA-TIME-LISTS, with that function's TIME-LIST arg set to a sequence whose length is given by the length of
DATA-LIST, starting with 0 and incremented by OLD-TIME-BASE." 
  (if (= old-time-base new-time-base)
      (sequence-to-list data-list)
      (convert-data-time-lists data-list old-time-base new-time-base output-order-data-time include-time-list new-time-base-is-length)))

#|
(defun interpolate-data (data-0 time-0 data-1 time-1 new-time)
  (+ data-1 (* (- new-time time-1) (/ (- data-0 data-1) (- time-0 time-1)))))
|#

(proclaim '(inline interpolate-data))
(defun interpolate-data (data-t0 t0 data-t-1 t-1 new-time)
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (if t-1
      (+ (the sf data-t-1) (* (- (the sf new-time) (the sf t-1))
			      (/ (- (the sf data-t0) (the sf data-t-1))
				 (- (the sf t0) (the sf t-1)))))
      (the sf data-t0)))

(proclaim '(inline interpolate-data-df))
(defun interpolate-data-df (data-t0 t0 data-t-1 t-1 new-time)
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (if t-1
      (+ (the df data-t-1) (* (- (the df new-time) (the df t-1))
			      (/ (- (the df data-t0) (the df data-t-1))
				 (- (the df t0) (the df t-1)))))
      (the df data-t0)))

(defun convert-data-time-lists (data-list time-base new-time-base &optional (output-order-is-data-then-time-p t) include-time-list new-time-base-is-length)
  "Given a list of time points in TIME-BASE, which may not be evenly spaced, and a sequence of data points in DATA-LIST that refer
to these time points, generate a data list that is sampled evenly [linear interpolation] at intervals derived from
NEW-TIME-BASE. If TIME-BASE is a single number, this is taken as the fixed time step of the input data. If NEW-TIME-BASE-IS-LENGTH is
NIL [default NIL] then NEW-TIME-BASE is the new time step, whose units are the same assumed in TIME-BASE. Otherwise, the new time
step is chosen so that the resampled data list has length given by NEW-TIME-BASE. For making evenly sampled versions of Surf-Hippo
simulations, the current simulation time list is found with CURRENT-SIM-PLOT-TIME-LIST. When the optional INCLUDE-TIME-LIST arg is
T, depending on the optional argument OUTPUT-ORDER-IS-DATA-THEN-TIME-P [default T] the function returns:

 (list new-data-list new-time-list)  <=  OUTPUT-ORDER-IS-DATA-THEN-TIME-P = T 
 (list new-time-list new-data-list)  <=  OUTPUT-ORDER-IS-DATA-THEN-TIME-P = NIL

Otherwise the function returns just the new-data-list."
					;  (declare (optimize (safety 1) (speed 3) (space 1))) 
  (let* ((time-list (if (consp time-base) time-base (list-of-nums (length data-list) 0.0 time-base)))
	 (data-list (sequence-to-list data-list))
	 (new-time-step (s-flt (if new-time-base-is-length
				   (* time-base (/ (length data-list) new-time-base))
				   new-time-base)))
	 (time-list-in-reverse-order-p (> (car time-list) (car (last time-list))))
	 (time-list (if time-list-in-reverse-order-p (reverse time-list) time-list))
	 (first-input-time (s-flt (first time-list)))
	 (last-input-time (s-flt (car (last time-list))))
	 (data-list (if time-list-in-reverse-order-p (reverse data-list) data-list))
	 (new-time-list (list (car time-list)))
	 (new-data-list (list (car data-list))))
    (loop for new-time single-float from (+ new-time-step first-input-time) by new-time-step while (<= new-time last-input-time)
	  do (push (s-flt new-time) new-time-list)
	  ;; (printvars (cadr time-list) (car time-list) new-time)
	  (if (= (car time-list) new-time)
	      (push (s-flt (car data-list)) new-data-list)
	      (loop until (or (not (caddr time-list))
			      (< new-time (cadr time-list)))
		    do (setq time-list (cdr time-list)
			     data-list (cdr data-list))
		    finally
		    ;; (printvars (car data-list)(car time-list) (cadr data-list) (cadr time-list))
		    (push (interpolate-data (s-flt (car data-list)) (s-flt (car time-list))
					    (s-flt (cadr data-list)) (s-flt (cadr time-list))
					    new-time)
			  new-data-list))))
    (if include-time-list
	(let ((output (list (reverse new-data-list) (reverse new-time-list))))
	  (if output-order-is-data-then-time-p output (reverse output)))
	(reverse new-data-list))))

#|

(defun value-at-time (data-list time-list target-time)
  (let* ((first-t (car time-list))
	 (last-t (car (last time-list)))
	 (time-list (if (> first-t last-t) (reverse time-list) time-list))
	 (data-list (if (> first-t last-t) (reverse data-list) data-list))
	 time	 time-1	 data	 data-1)
    (setq time (car time-list))
    (setq time-1 time)
    (setq time-list (cdr time-list))
    (setq time (car time-list))
    (setq data (car data-list))
    (setq data-1 data)
    (setq data-list (cdr data-list))
    (setq data (car data-list))
    (loop for new-t from 0.0 by new-time-step
	  until (null data-list)
	  collect
	  (+ data-1
	     (if (= 0 (- time time-1)) 0 (* (- new-t time-1) (/ (- data data-1) (- time time-1)))))
	  into new-data-list
	  collect new-t into new-time-list
	  do
	  (loop until (or (null data-list) (and (<= time-1 new-t)(< new-t time)) )
		do
		(setq time-1 time)
		(setq time-list (cdr time-list))
		(setq time (car time-list))
		(setq data-1 data)
		(setq data-list (cdr data-list))
		(setq data (car data-list)))
	  finally 
	  (return (list new-data-list new-time-list)))))


|#


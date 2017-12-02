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


;;; SYS Source file: synapse-events.lisp

(in-package "SURF-HIPPO")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating Synapse Event Times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun synapses-from-syns-or-types-ref (syns-or-types)
  (flatten-list
   (loop for foo in (flatten-list (coerce-to-list (or (element syns-or-types) syns-or-types (element 'synapse))))
	 collect (let ((thing (or (element foo 'synapse) (element foo 'synapse-type))))
		   (typecase thing
		     (synapse thing)
		     (synapse-type (synapses-of-type thing)))))))

(defun clear-events (&optional syns-or-types)
  "Clear all :EVENT-TIMES slots in the synapses associated with the atom or list SYNS-OR-TYPES. If
SYNS-OR-TYPES is not supplied, then this is done for all synapses."
    (loop for syn in (synapses-from-syns-or-types-ref syns-or-types) do (setf (synapse-event-times syn) nil)))

(defun add-event (syns-or-types event-time) (add-events syns-or-types event-time))
(defun add-events (syns-or-types event-times) (events syns-or-types event-times))

(defun events (&optional (syns-or-types 'synapse) (event-times nil event-times-supplied-p) &rest more-events)
  "Return the event times associated with the synapses associated with SYNS-OR-TYPES (atom or list). If EVENT-TIMES (arbitrary
number of lists of numbers or single numbers, in milliseconds) is included, these times are -added- to the existing events. If
EVENT-TIMES is set explictly to NIL, or if the keyword :REPLACE is included in the MORE-EVENTS args, then all prior events are cleared."
  (let* ((all-args-after-syns (flatten-no-nils-list event-times more-events))
	 (found-replace (find :replace all-args-after-syns))
	 (all-event-times (float-list (remove :replace all-args-after-syns))))
    (when (or (and event-times-supplied-p (not event-times)) found-replace) (clear-events syns-or-types))
    (loop for syn in (synapses-from-syns-or-types-ref syns-or-types)
	  do (setf (synapse-event-times syn) (flatten-no-nils-list all-event-times (synapse-event-times syn)))
	  nconc (copy-list (synapse-event-times syn)))))

(defun remove-events (syns-or-types event-times)
  "Remove EVENT-TIMES (list of numbers or single number) from existing events for all synapses associated with SYNS-OR-TYPES (atom
or list)."
  (let ((event-times-to-remove (float-list (coerce-to-list event-times))))
    (loop for syn in (synapses-from-syns-or-types-ref syns-or-types)
	  do (setf (synapse-event-times syn)
		   (let ((event-times (synapse-event-times syn)))
		     (loop for time in event-times-to-remove do (setq event-times (remove time event-times :count 1)))
		     event-times)))))
		
(defun clear-event-times (&optional syns-or-types) (clear-events syns-or-types))
(defun add-event-times (syns-or-types event-times) (add-events syns-or-types event-times))
(defun remove-event-times (syns-or-types event-times) (remove-events syns-or-types event-times))

(defun add-poisson-events (syns-or-types lambda-spec start stop &key (step 1.0) (time-offset 0.0) (lambda-coefficient 1.0) (min-interval-value 1.0) clear-events)
  "Adds events derived from a poisson process from START to STOP [ms] to synapses associated with SYNS-OR-TYPES \(atom or list\). Poisson processes
are generated with MODULATED-POISSON-EVENTS: if LAMBDA-SPEC is a number, then this is the mean lambda in 1/ms; if it is a function or a sequence, then
LAMBDA-SPEC determines the lambda as a function of time. In the latter case, the value of STEP [ms] is passed to
MODULATED-POISSON-EVENTS. LAMBDA-COEFFICIENT is applied to the lambda value at all times. If an element of SYNS-OR-TYPES refers specifically to a
synapse type, then events are added to all synapses of that type. All times in the returned list are adjusted by the addition of
TIME-OFFSET[ms]. Poisson intervals have a minimum value of MIN-INTERVAL-VALUE [ms]. When CLEAR-EVENTS is true, then the existing :EVENT-TIMES are
cleared before adding the new poisson events to each synapse."
  (WHEN lambda-spec
	(let ((start (s-flt start))
	      (stop (s-flt stop))
	      (step (s-flt step))
	      (time-offset (s-flt time-offset))
	      (min-interval-value (s-flt min-interval-value)))
	  (loop for syn in (synapses-from-syns-or-types-ref syns-or-types)
		do (set-synapse-event-times-to-poisson-events syn start stop step lambda-spec lambda-coefficient time-offset min-interval-value clear-events)))))

(defun set-synapse-event-times-to-poisson-events (syn start stop step lambda-spec
						      lambda-coefficient
						      time-offset
						      min-interval-value
						      clear-events)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float start stop step time-offset min-interval-value))
  (setf (synapse-event-times syn)
	(flatten-no-nils-list
	 (cons (unless clear-events (synapse-event-times syn))
	       (modulated-poisson-events lambda-spec start stop
					 :lambda-coefficient lambda-coefficient
					 :step step
					 :time-offset time-offset
					 :min-interval-value min-interval-value))))
  nil)

(defun edit-synapse-event-times (syn)
  (let ((syn (element syn 'synapse))
	dummy1 (dummy2 1.0) (dummy3 0.0) (dummy4 *user-stop-time*) dummy5 dummy6 (dummy7 0.0) (dummy8 *user-stop-time*))
    (when syn
      (loop while t do
	    (choose-variable-values
	     '((dummy6 "Add single event at start time indicated below." :boolean)
	       (dummy1 "Augment, clear or replace current list of events, or finished:"
		:choose (:Augment :replace :clear :finished))
	       (dummy2 "Time constant [ms]" :float)
	       (dummy3 "Start time for events [ms]" :float)
	       (dummy4 "Stop time for events [ms]" :float)
	       (dummy5 "Display event distribution" :boolean)
	       (dummy7 "Display event distribution start time" :float)	     
	       (dummy8 "Display event distribution stop time" :float))
	     :label (format nil "Edit Poisson Process Events For ~A" (synapse-name syn)))
	    (when dummy6 (setf (synapse-event-times syn) (cons dummy2 (synapse-event-times syn))))
	    (when (and dummy1 (not (eq dummy1 :finished)))
	      (let ((events (poisson-events (/ 1 dummy2) dummy3 dummy4)))
		(case dummy1
		  (:augment (setf (synapse-event-times syn) (flatten-list (concatenate 'list (synapse-event-times syn) events))))
		  (:replace (setf (synapse-event-times syn) events))
		  (:clear (setf (synapse-event-times syn) '())))))
	    (when dummy5
	      (let ((*enable-synapses* t))
		(HISTOGRAM-SYNAPSE-EVENTS :syn syn :min-time dummy7 :max-time dummy8)))

	    when (eq dummy1 :finished) do (return t)

	    do (setq dummy1 (unless (or dummy1 dummy5) :finished)) 
	    (setq dummy5 nil dummy6 nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setting up synapse events
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-1st-order-Depressing-weights-list (syn sorted-events)
  ;; Implement simple 1st-order-Depressing dynamic scheme.
  (let ((release-fraction (the sf (element-parameter (synapse-type syn) 'release-fraction)))
	(tau-recovery (the sf (element-parameter (synapse-type syn) 'tau-recovery))))
    (generate-1st-order-Depressing-weights-list-core sorted-events release-fraction tau-recovery)))

(defun test-1ST-ORDER-DEPRESSING-WEIGHTS (tau)
  (let ((release-fractions  '(0.01 0.1 0.2 0.5 1.0))
	(*overlay-plots* nil)
	(events  '(0.0 10.0 20.0 30.0 40.0)))
    (loop for release-fraction in release-fractions collect
	  (GENERATE-1ST-ORDER-DEPRESSING-WEIGHTS-LIST-CORE events release-fraction tau)
	  into actual-events
	  finally (plot-timed-data actual-events release-fractions events
				   :y-max 1.01 :x-min 0
				   :y-min 0
				   :y-inc 0.2
				   :x-inc 20
				   :x-are-fns t
				   :scatter t))))
			    
(defun generate-1st-order-Depressing-weights-list-core (sorted-events release-fraction tau-recovery)
  (let ((last-event nil)
	(last-weight nil))
    (loop for event in sorted-events collect
	  (let* ((dt (the sf (if last-event (- (the sf event) (the sf last-event)) 0.0)))
		 (exponential-term (exp (- (/ dt tau-recovery))))
		 (weight (the sf (if last-weight
				     (+ (* (- 1 release-fraction) exponential-term last-weight)
					(* release-fraction (- 1 exponential-term)))
				   release-fraction))))
	    (setq last-weight weight
		  last-event event)
	    weight))))

(defun sort-scale-and-shift-event-times (syn)
  ;; Takes a list of real time event times, sorts them, shifts them according to any delay defined by the synapse, converts them
  ;; to fixnums according to the timebase of the synapse type. This function also processes events according to the
  ;; 1st-order-Depressing model, when indicated by the synapse type. The resulting list is the final form used by the synapse
  ;; during the simulation.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((scaling (synapse-type-waveform-time-interval-inverse (synapse-type syn)))
	 (sorted-events (single-float-ascending-sort-list (get-events syn)))
	 (event-weights
	  (or (element-parameter (synapse-type syn) 'event-weights)
	      (element-parameter syn 'event-weights)
	      (when (element-parameter (synapse-type syn) '1st-order-Depressing-dynamics) ; This is a 1st-order-Depressing synapse
		(generate-1st-order-Depressing-weights-list syn sorted-events)))))
    ;; Scale events according to waveform time base and convert to fixnums. If 1st-order-Depressing or explicit dynamics, also collect associated weights.
    (if event-weights
      (mapcar #'(lambda (event weight) (cons (round (* scaling (the sf event))) (the sf weight))) sorted-events event-weights)
      (loop for event in sorted-events collect (round (* scaling (the sf event)))))))
    
(defun default-convert-light-response-to-events (syn)
  ;; When there is no a-list entry for 'CONVERT-LIGHT-RESPONSE-TO-EVENTS-FUNCTION in the parameters list for the synapse type
  ;; corresponding to SYN, then this function is used to convert the light response input to a spike train. This function should
  ;; return a list of event times, which is later assigned to the :EVENT-TIMES slot of SYN. SYNAPSE-EVENT-TIMES should thus be
  ;; either NIL or a list of single-floats.
  (let* ((light-response (get-light-response-input-waveform syn))
	 (max (sequence-max light-response))
	 (integral (integrate-wave light-response))
	 (remainder 0.0)
	 (threshold 2.0)
	 (events '()))
    (loop for value in integral
	  for time from 0.0 by 1.0
	  when (> (/ (- (/ value max) remainder) threshold) 1.0)
	  do
	  (let* ((splits (floor (/ (- (/ value max) remainder) threshold)))
		 (fraction (/ 1.0 splits)))
	    (loop for count from 1 to splits
		  do (push (+ time (* (1- count) fraction)) events))
	    (setq remainder (/ value max))))
    events))

(defun consolidate-lumped-synapse-events (cell-element)
  ;; Referencing the list of 'EVENT-SYNS of a given type prepared earlier for CELL-ELEMENT, choose one as the representative
  ;; lumped synapse, and consolidate all events onto that representative. Clear the list of 'EVENT-SYNS for CELL-ELEMENT, and
  ;; return the chosen lumped synpase.
  (let* ((node-syns-of-type (element-parameter cell-element 'event-syns))
	 (lumped-syn (car node-syns-of-type))) ; Lumped is first one, arbitrary.
    (setf (synapse-transformed-events lumped-syn)
	  (let ((nconced-events (loop for syn in node-syns-of-type nconc (copy-list (synapse-transformed-events syn)))))
	    (if (element-parameter (synapse-type lumped-syn) '1st-order-Depressing-dynamics)
					; Event list is in form (... (time . weight) , (time . weight) ...)
	      (sort nconced-events '< :key 'car)
					; Event list is in form (... time, time ...)
	      (sort nconced-events '<))))
    (remove-element-parameter cell-element 'event-syns)
    lumped-syn))		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Documenting synapse events
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun events-this-simulation (events &optional (stop-time *user-stop-time*)) (loop for event in events when (<= event stop-time) sum 1))

(defun PRINT-SYNAPSE-TOTAL-EVENTS ()
  (loop for type in (get-synapse-types :voltage) when (IS-THERE-ONE-ACTIVE-SYN-OF-TYPE type) do
	(synapse-type-iterator
	 (syn type)
	 when (synapse-event-generator syn)
	 summing (events-this-simulation (synapse-event-times (synapse-event-generator syn))) into post-synaptic-events
	 when (event-generator-p syn)
	 summing (events-this-simulation (synapse-event-times syn)) into pre-synaptic-events
	 finally (format t "~A total pre-synaptic event~:p, ~A total post-synaptic event~:p for synapse type ~A~%"
			 pre-synaptic-events post-synaptic-events (synapse-type-name type))))
  (loop for type in (get-synapse-types '(:event :light-event))
	when (IS-THERE-ONE-ACTIVE-SYN-OF-TYPE type) do
	(synapse-type-iterator
	 (syn type)
	 summing (events-this-simulation (synapse-event-times syn)) into synaptic-events
	 finally 
	 (format t "~A total synaptic event~:p for synapse type ~A~%" synaptic-events (synapse-type-name type)))))

(defun print-synapse-event-times () (print-synapse-events))

#|
(defun print-synapse-events ()
  (loop for synapse being the hash-value of (SYNAPSE-HASH-TABLE) when (synapse-event-times synapse) do
	(format t "Event times for ~a:~% " (synapse-name synapse))
	(format-list (sort (synapse-event-times synapse) '<) 20 t 4)
	(format t "~%")))
|#

(defun print-synapse-events (&optional type)
  (when *enable-synapses*
    (loop for type in (or (and type (list type)) (get-synapse-types :event)) do
	  (loop for syn in (synapses-of-type type)
		when (synapse-event-times syn)
		do (format t "Synapse ~A event time(s) : ~A~%"
			   (synapse-name syn)
			   (or (synapse-event-times syn) "None"))))))

(defun histogram-synapse-events (&key (bins 10) (min-time 0) type syn (max-time *user-stop-time*))
  "Plot a histogram of the event times assigned to either synapses associated with SYN or TYPE - if
neither is supplied then plot events of all EVENT synapses. Individual plots are generated for all
types of referenced synapses."
					;  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when min-time (setq min-time (float min-time)))
  (when max-time (setq max-time (float max-time)))
  (let* ((synapse (element syn 'synapse))
	 (specified-syns (coerce-to-list synapse))
	 (all-types (delete-duplicates (coerce-to-list (or (element-type syn 'synapse)
							   (element type 'synapse-type)
							   (get-synapse-types :event)))))
	 (title-ref (when (or (not (consp synapse))
			      (= 1 (length synapse)))
		      (format nil "Synapse ~A" (element-name synapse)))))
    (loop for type in all-types do
	  (let* ((synapses (loop for syn in (synapses-of-type type)
				 when (or (not specified-syns)
					; (= (length all-types) 1)
					  (member syn specified-syns))
				 collect syn))
		 (events (let (out)
			   (loop for syn in synapses
				 do (loop for event single-float in (synapse-event-times syn) ; (list (synapse-delay syn))
					  when (cond ((and min-time max-time)
						      (<= (the sf min-time) event (the sf max-time)))
						     (min-time (<= (the sf min-time) event))
						     (max-time (<= event (the sf max-time)))
						     (t t))
					  do (push event out)))
			   out)))
	    (when events (plot-histogram
			  events
			  :bins bins :bin-min min-time :bin-max max-time :x-label "ms" :x-origin :use-min :y-label (format nil "Synapse~%Events")
			  :title (format nil "~a: ~A Events Histogram"
					 *simulation-name*
					 (or title-ref (element-name type)))))))))

#|
(let ((array (list 3dplot-time-bins 3dplot-distance-bins))
		      (maximum-distance (or maximum-distance (max-of-list max-distances))))
		  (loop for time-distance in times-distances
			(let ((time (nth 0 time-distance)) ; time
			      (distance (nth 1 time-distance)) ; distance
			      )
			  (incf (aref array
				      (floor (* 3dplot-time-bins (/ time  maximum-time)))
				      (floor (* 3dplot-distance-bins (/ distance maximum-distance)))))))
		  (3dplot array :title title))
|#

(defun plot-scatter-synapse-distances-event-times (&key (synapse-types (synapse-types))
							(white-is-maximum-p t)  
							(x-are-fns t)(y-are-fns t) (width 400) (height 400)
							plot-type ; :density :3dplot
							min-plot-time-given-by-events 
							(plot-events-prior-to-0 t)
							title
							(3dplot-scale 30.0)
							(3dplot-aspect 0.20)
							(3dplot-time-bins (/ *user-stop-time* 100))
							(3dplot-distance-bins 20)
							(minimum-distance 0) maximum-distance
							(maximum-time *user-stop-time*)
							minimum-time
							(cell *cell*) distance-plot-increment)
  "Plot event times for all enabled :EVENT controlled synapses in CELL [default *CELL*] of types referenced by SYNAPSE-TYPES .[default all in circuit]
versus distances to soma of the synapses."
  (let* ((maximum-time (s-flt maximum-time))
	 (minimum-time (and minimum-time (s-flt minimum-time)))
	 (min-event-time *user-stop-time*)
	 (x-label "Event time [ms]")
	 (y-label "Distance to Soma [um]")
	 (distance 0)
	 (maximum-distance (or maximum-distance (tree-radius cell)))
	 (types (coerce-to-list synapse-types))
	 (title (or title (format nil "~A: ~A Events and Locations" *simulation-name* (if (> (length types) 1) "Synapse" (element-name types))))))
    (loop for type in types
	  when (and (element-in-circuit type) (element-enabled-p type) (IS-THERE-ONE-ACTIVE-SYN-OF-TYPE type))
	  collect
	  (loop for syn in (synapses-of-type type)
		do (setq distance (distance-to-soma syn))
		when (and (eq cell (element-cell syn)) (<= minimum-distance distance maximum-distance))
		nconcing (loop for time in (synapse-event-times syn)
			       when (or (or (not minimum-time) (>= time minimum-time)) plot-events-prior-to-0)
			       minimize time into temp-min-event-time
			       when (< (the sf time) (the sf maximum-time))
			       collect (list time distance) into time-distances
			       finally (setq min-event-time (min min-event-time temp-min-event-time))
			       (return time-distances)))
	  into times-distances finally
	  (return
	    (let ((maximum-distance (or maximum-distance (loop for time-distances in times-distances
							       maximize (loop for time-distance in time-distances
									      maximize (nth 1 time-distance))))))
	      (case plot-type
		((:3dplot :density)
		 (let ((times '()) (distances '()))
		   (loop for time-distances in times-distances
			 do (loop for time-distance in time-distances
				  do (push (nth 0 time-distance) times) (push (nth 1 time-distance) distances))
			 finally
			 (case plot-type
			   (:density (density-histo-plot times (round 3dplot-time-bins)
							 distances (round 3dplot-distance-bins)
					; z-max (dynamic-range 100) (increment 1) (white-is-maximum-p t)
					; x-are-fns y-are-fns
							 :white-is-maximum-p white-is-maximum-p 
							 :width width :height height 
							 :title title :element-aspect-ratio (s-flt 3dplot-aspect)
					; (border 75) print-out-max-mins
							 :x-are-fns x-are-fns 
							 :y-are-fns y-are-fns
				  
							 :x-min minimum-time :x-max maximum-time
							 :y-min minimum-distance :y-max maximum-distance
							 :x-axis-tick-skip (round (1- (/ 3dplot-time-bins 5)))
							 :y-axis-tick-skip (round (1- (/ 3dplot-distance-bins 5)))
							 :x-label x-label :y-label "um")) 
			   (:3dplot (3d-histo-plot times (round 3dplot-time-bins) distances (round 3dplot-distance-bins)
						   :aspect (s-flt 3dplot-aspect) :scale (s-flt 3dplot-scale)
						   :width width :height height 
						   :x-min minimum-time :x-max maximum-time
						   :y-min minimum-distance :y-max maximum-distance
						   :x-label x-label :y-label y-label :title title))))))
		(t
		 (plot-scatter times-distances (element-name types)
			       :title title :width width :height height 
			       :x-label x-label :y-label y-label :y-label-v-position :upper-center
			       :x-min (if min-plot-time-given-by-events min-event-time minimum-time)
			       :x-origin (if min-plot-time-given-by-events min-event-time 0)
			       :x-max maximum-time
			       :y-are-fns t :y-min 0 :y-max maximum-distance :y-inc distance-plot-increment
			       :x-symbol-width 7 :y-symbol-width 7))))))))
#|
;; To be finished...
(defun plot-scatter-element-distances-attribute (&key (element-types (synapse-types))
						      (attribute :event-times)
						      (white-is-maximum-p t)  
						      (x-are-fns t)(y-are-fns t) (width 400) (height 400)
						      plot-type ; :density :3dplot
						      min-plot-attribute-given-by-events 
						      (plot-events-prior-to-0 t)
						      title
						      (3dplot-scale 30.0)
						      (3dplot-aspect 0.20)
						      (3dplot-attribute-bins (/ *user-stop-time* 100))
						      (3dplot-distance-bins 20)
						      (minimum-distance 0) maximum-distance
						      maximum-attribute ; *user-stop-time*
						      minimum-attribute ; 0.0
						      (cell *cell*) distance-plot-increment)
  "Plot event attributes for all enabled event elements in CELL of types given by the atom or list
ELEMENT-TYPES versus distances to soma of the elements. ATTRIBUTE can include :EVENT-TIMES, :VOLTAGES."
  (let* ((maximum-attribute (or maximum-attribute
				(case attribute
				  (:event-time *user-stop-time*)
				  (:voltage nil))))
	 (minimum-attribute (or minimum-attribute
				(case attribute
				  (:event-time 0.0)
				  (:voltage nil))))
	 (min-event-attribute maximim-attribute)
	 (x-label (case attribute
		    (:event-times "Event attribute [ms]")
		    (:voltage "Membrane voltage [mV]")))
	 (y-label (format nil "Distance to Soma~%of ~A [um]" (element-name cell)))
	 (distance 0)
	 (maximum-distance (or maximum-distance (tree-radius cell)))
	 (types (coerce-to-list element-types))
	 (title (or title (format nil "~A: ~A Events and Locations" *simulation-name*
				  (if (> (length types) 1) "Element"
				      (element-name types)))))
	 (attributes-distances
	  (loop for type in types
		when (and (element-in-circuit type) (element-enabled-p type) (IS-THERE-ONE-ACTIVE-SYN-OF-TYPE type))
		collect
		(loop for syn in (synapses-of-type type)
		      do (setq distance (distance-to-soma syn))
		      when (and (eq cell (element-cell syn)) (<= minimum-distance distance maximum-distance))
		      nconcing (loop for attribute in (synapse-event-times syn)
				     when (or (>= attribute minimum-attribute) plot-events-prior-to-0)
				     minimize attribute into temp-min-event-attribute
				     when (< (the sf attribute) (the sf maximum-attribute))
				     collect (list attribute distance) into attribute-distances
				     finally (setq min-event-attribute (min min-event-attribute temp-min-event-attribute))
				     (return attribute-distances)))))
	 
	 (maximum-distance (or maximum-distance (loop for attribute-distances in attributes-distances
						      maximize (loop for attribute-distance in attribute-distances
								     maximize (nth 1 attribute-distance))))))
    (case plot-type
      ((:3dplot :density)
       (let ((attributes '()) (distances '()))
	 (loop for attribute-distances in attributes-distances
	       do (loop for attribute-distance in attribute-distances
			do (push (nth 0 attribute-distance) attributes) (push (nth 1 attribute-distance) distances))
	       finally
	       (case plot-type
		 (:density (density-histo-plot attributes (round 3dplot-attribute-bins)
					       distances (round 3dplot-distance-bins)
					; z-max (dynamic-range 100) (increment 1) (white-is-maximum-p t)
					; x-are-fns y-are-fns
					       :white-is-maximum-p white-is-maximum-p 
					       :width width :height height 
					       :title title :element-aspect-ratio (s-flt 3dplot-aspect)
					; (border 75) print-out-max-mins
					       :x-are-fns x-are-fns 
					       :y-are-fns y-are-fns
				  
					       :x-min minimum-attribute :x-max maximum-attribute
					       :y-min minimum-distance :y-max maximum-distance
					       :x-axis-tick-skip (round (1- (/ 3dplot-attribute-bins 5)))
					       :y-axis-tick-skip (round (1- (/ 3dplot-distance-bins 5)))
					       :x-label x-label :y-label "um")) 
		 (:3dplot (3d-histo-plot attributes (round 3dplot-attribute-bins) distances (round 3dplot-distance-bins)
					 :aspect (s-flt 3dplot-aspect) :scale (s-flt 3dplot-scale)
					 :width width :height height 
					 :x-min minimum-attribute :x-max maximum-attribute
					 :y-min minimum-distance :y-max maximum-distance
					 :x-label x-label :y-label y-label :title title))))))
      (t
       (plot-scatter attributes-distances (element-name types)
		     :title title :width width :height height 
		     :x-label x-label :y-label y-label :y-label-v-position :upper-center
		     :x-min (if min-plot-attribute-given-by-events min-event-attribute 0)
		     :x-origin (if min-plot-attribute-given-by-events min-event-attribute 0)
		     :x-max maximum-attribute
		     :y-are-fns t :y-min 0 :y-max maximum-distance :y-inc distance-plot-increment
		     :x-symbol-width 7 :y-symbol-width 7)))))
|#


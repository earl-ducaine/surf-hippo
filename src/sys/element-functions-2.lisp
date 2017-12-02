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


;;; SYS Source file: element-functions-2.lisp

(in-package "SURF-HIPPO")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Element data and plot
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-data-units (element &optional data-type model-type)
  (atomize-list
   (loop for element in (coerce-to-list (element element model-type)) collect
	 (case (or data-type (default-data-type element))
	   ((field-potential reversal-potential voltage) "mV")
	   ((dvdt voltage-derivative) "mV/ms")
	   ;; (axon-voltage "mV")
	   ((current dendrite-current) "nA")
	   (conductance (if *SAVE-CONDUCTANCES-normalized* *NORMALIZED-GBAR-LABEL* "uS"))
	   ((1 2 3 total) "mM")
	   ((markov-state state) "State")))))

(defun element-data (element &optional data-type model-type state-index)
  "Given ELEMENT or elements of type MODEL-TYPE, returns the plot data list [in correct time order, according to the list of times given by the function
CURRENT-SIM-PLOT-TIME-LIST]. or a list of lists [for more than one element] of type given by DATA-TYPE. The possible DATA-TYPE for the different element
model-types are:

 Element Model-Type          Data Type [first is default, given by the function DEFAULT-DATA-TYPE]
 ----------------------      -----------------------------------------------------------------------

 SOMA                        'VOLTAGE, 'DVDT, 'LEAK-CURRENT, 'CAPACITANCE-CURRENT, 'DENDRITE-CURRENT
 SEGMENT                     'VOLTAGE, 'DVDT, 'LEAK-CURRENT, 'CAPACITANCE-CURRENT 
 EXTRACELLULAR-ELECTRODE     'FIELD-POTENTIAL
 AXON                        'VOLTAGE
 CHANNEL, SYNAPSE            'CURRENT, 'REVERSAL-POTENTIAL, 'CONDUCTANCE
 CHANNEL-TYPE, SYNAPSE-TYPE  'CURRENT, 'CONDUCTANCE
 ISOURCE                     'CURRENT
 VSOURCE                     'CURRENT, 'VOLTAGE
 PARTICLE                    'STATE, 'MARKOV-STATE
 CONC-PARTICLE               'STATE
 CONC-INT                    'TOTAL, 1, 2, 3 (numbers refer to shells or compartments)
 BUFFER                      'CONCENTRATION
 PUMP                        'CURRENT

The STATE-INDEX argument is used when retrieving state data of Markov gating particles. Event times for synapses
and axons may be accessed by the EVENTS function. If no data was saved from the last simulation, then return NIL.
"
  (atomize-list
   (loop for element in (coerce-to-list (element element model-type)) collect
	 (retrieve-single-data element (or data-type (default-data-type element)) state-index))))

(defun element-data-clear (element &optional data-type model-type state-index)
  "Given ELEMENT or elements of type MODEL-TYPE, clears any saved data, referenced with arguments as in ELEMENT-DATA."
  (atomize-list
   (loop for element in (coerce-to-list (element element model-type)) collect
	 (clear-single-data element (or data-type (default-data-type element)) state-index))))

(defun element-saved-data (element &optional data-type model-type)
  (get-a-value (model-output-data-key (element-model element model-type) data-type) (element-parameters element model-type)))

(defun element-clear-saved-data (element &optional data-type model-type)
  (let ((assoc-result (assoc (model-output-data-key (element-model element model-type) data-type) (element-parameters element model-type))))
    (when assoc-result
      (setf (cdr assoc-result) nil))))

(proclaim '(inline element-current-value-internal))
(defun element-current-value-internal (element data-type &optional double-float-p)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((model (type-symbol-model (type-of element)))
	 (model-output-current-value-function (model-output-current-value-function model data-type)))
    (when (fboundp model-output-current-value-function)
      (if double-float-p
	  (d-flt (funcall model-output-current-value-function element))
	  (s-flt (funcall model-output-current-value-function element))))))

(defun element-current-value (element &optional data-type model-type)
  (let ((element (element element model-type))
	(date-type (or data-type (default-data-type element))))
    (typecase element
      (cons (loop for elt in element collect (element-current-value-internal elt data-type)))
      (t (element-current-value-internal element data-type)))))

(defun element-data-dted (element &optional (delta-t 1.0) data-type model-type (time-base (current-sim-plot-time-list)) state-index)
  "Given an element or elements in ELEMENT or element type MODEL-TYPE, returns a simulation data list (or lists for more than one
element) of type DATA-TYPE [as is ELEMENT-DATA] sampled on an even time base as given by the optional DELTA-T [milliseconds, default
1.0]. The time base for the original data is taken from TIME-BASE, which is either a list of numbers or a single number, in which
case this is the even time grid of the original data. [Bug] Note that the original time base must include steps on the order of DT
in order for proper sampling during simulation periods of long time steps."
  (let ((elements (coerce-to-list (element element model-type)))
	data)
    (loop for element in elements
	  do (setq data (element-data element data-type model-type state-index))
	  when data collect (convert-data-time-lists data time-base delta-t) into out
	  else do (format t "In ELEMENT-DATA-DTED, the ELEMENT ~A does not have any data!~%" element)
	  finally (return (if (> (length elements) 1) out (car out))))))

;; ***** ***** ***** ***** ***** ***** *****
;;
;; Anatomy geometric hacks.
;;
;; ***** ***** ***** ***** ***** ***** *****

(defun element-absolute-location (element &optional model-type)
  (let ((element-internal (element element model-type)))
    (typecase element-internal
      (EXTRACELLULAR-ELECTRODE (EXTRACELLULAR-ELECTRODE-absolute-location element-internal))
      (t (let ((node (element-physical-node element-internal model-type)))
	   (if node
	     (or
	      (node-absolute-location node)
	      (progn (process-circuit-structure)
		     (or (node-absolute-location node)
			 (sim-error (format nil "ELEMENT-ABSOLUTE-LOCATION: The cell containing ~A has not been processed. Run PROCESS-CIRCUIT-STRUCTURE." node)))))
	     (format t "~A has no node." element)))))))

(defun cell-element-absolute-location-fast (cell-elt) (node-absolute-location (cell-element-physical-node-fast cell-elt)))

(defun element-relative-location (element &optional model-type)
  (let ((node (element-physical-node element model-type)))
    (when node (node-relative-location node))))

(defun element-location (element &optional model-type)
  "Returns the XYZ coordinates [microns] of the cell element node associated with ELEMENT of MODEL-TYPE."
  (element-absolute-location element model-type))

(defun where (element &optional model-type)
  "Returns the XYZ coordinates [microns] of the cell element node associated with ELEMENT of MODEL-TYPE."
  (element-location element model-type))

(defun where-val (element model-type index)
  (let ((location (element-location element model-type)))
    (typecase (car location)
      (cons (loop for loc in location collect (nth index loc)))
      (t (nth index location)))))

(defun where-x (element &optional model-type)
  "Returns the single float X coordinate [microns] of the cell element node associated with ELEMENT of MODEL-TYPE."
  (s-flt (where-val element model-type 0)))

(defun where-y (element &optional model-type)
  "Returns the single float Y coordinate [microns] of the cell element node associated with ELEMENT of MODEL-TYPE."
  (s-flt (where-val element model-type 1)))

(defun where-z (element &optional model-type)
  "Returns the single float Z coordinate [microns] of the cell element node associated with ELEMENT of MODEL-TYPE."
  (s-flt (where-val element model-type 2)))

(defun min-max-circuit-coordinates ()
  "Return as values the minimum and maximum coordinates of the curent circuit cell elements:

    (min-x max-x min-y max-y min-z max-z)

"
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for cell-element in (cell-elements)
	minimizing (where-x cell-element) into min-x single-float
	minimizing (where-y cell-element) into min-y single-float
	minimizing (where-z cell-element) into min-z single-float

	maximizing (where-x cell-element) into max-x single-float
	maximizing (where-y cell-element) into max-y single-float
	maximizing (where-z cell-element) into max-z single-float
	finally (return (values min-x max-x min-y max-y min-z max-z))))

(defun as-the-crow-flies (location-1 location-2)
  "Returns the straight line distance between LOCATION-1 and LOCATION-2, where the arguments can
either be references to circuit elements or explicit location lists \(X Y Z\)."
  (if (and location-1 location-2)
    (cartesian-vector-distance
     (typecase location-1
       (cons location-1) 
       (t (element-absolute-location (element location-1))))
     (typecase location-2
       (cons location-2) 
       (t (element-absolute-location (element location-2)))))
    0.0))

(defun element-cloud (reference-element cloud-radius &optional restrict-to-reference-element-cell returned-model-type)
  "Return a list of elements of RETURNED-MODEL-TYPE [somas and segments if this is NIL, the default] that
are within CLOUD-RADIUS [microns] of REFERENCE-ELEMENT. Candidate returned elements are restricted
to the cell associated with REFERENCE-ELEMENT when RESTRICT-TO-REFERENCE-ELEMENT-CELL is non-nil."
  (let ((reference-element (element-cell-element reference-element))
	(reference-element-cell (element-cell reference-element)))
    (loop for elt in
	  (or (loop for elt in 
		    (or (hash-table-list (get-model-hash-table returned-model-type))
			(elements-of-type returned-model-type))
		    when (or (not restrict-to-reference-element-cell)
			     (eq (element-cell elt) reference-element-cell))
		    collect elt)
	      (cell-elements (if restrict-to-reference-element-cell reference-element-cell (cells))))
	  when (< (as-the-crow-flies reference-element elt) cloud-radius)
	  collect elt)))
	
(defun segs-distance-elts (candidates element exclude-these-elements element-point)
  (let ((cell (element-cell element)))
    (sort 
     (loop for reference in (or candidates (if cell (cell-segments cell) (segments)))
	   unless (or (eq reference element)
		      (loop for elt in exclude-these-elements thereis (eq elt reference)))
	   collecting (list (as-the-crow-flies element-point reference) reference))
     '< :key 'car)))

(defun closest-ELEMENT (element &key exclude-these-elements proximal-measure candidates just-somas)
  "Returns the closest soma or segment to ELEMENT, calculated with AS-THE-CROW-FLIES, taken from cell elements in CANDIDATES and excluding those in
EXCLUDE-THESE-ELEMENTS. ELEMENT may also be an explicit location list \(x y z\), with each value in microns. The second returned value is the distance
between ELEMENT and the closest cell element in microns. If CANDIDATES is not supplied then all segments and the soma of the cell associated with
ELEMENT are used, unless ELEMENT is a location, then all cell elements in the circuit are tested. If PROXIMAL-MEASURE is non-NIL, then the proximal
location of ELEMENT is used, otherwise the distal location is used to calculate the distance metric. If JUST-SOMAS is T, then candidate elements are
restricted to somas."
  (DECLARE (optimize (safety 1) (speed 3) (space 1)))
  (let* ((element (if (consp element) (float-list element) (element-cell-element element)))
	 (exclude-these-elements (coerce-to-list (element exclude-these-elements)))
	 (dummy-candidates (or candidates (cell-elements (if (consp element) (cells) element))))
	 (candidates (if just-somas
		       (loop for elt in dummy-candidates when (soma-p elt) collect elt)
		       dummy-candidates))
	 (element-point (if (consp element)
			  element
			  (if proximal-measure
			    (typecase element
			      (segment (node-absolute-location (segment-node-1 element)))
			      (soma (element-absolute-location element)))
			    (element-absolute-location element))))
	 (ref-distance 0.0)
	 (min-distance 0.0)
	 closest-elt)
    (declare (single-float min-distance ref-distance))
    (loop for reference in candidates
	  unless (or (eq reference element)
		     (member reference exclude-these-elements :test 'eq))
	  do (setq ref-distance (as-the-crow-flies element-point reference))
	  and when (or (< ref-distance min-distance)
		       (not closest-elt))
	  do (setq closest-elt reference
		   min-distance ref-distance)
	  finally (return (values closest-elt min-distance)))))
	 
(defun distance-to-soma (element)
  "Given an ELEMENT (name or object), returns the distance along the tree to the soma in microns. Faster way is to reference the
:SEGMENT-DISTANCE-TO-SOMA slot of the segment which is set when the cell anatomy is first processed."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (process-circuit-structure)		; Just in case this is not done yet
  (distance-to-soma-fast element))

(defun distance-to-soma-fast (element)
  (let ((seg (element-cell-element element nil t)))
    (typecase seg
      (segment (if (= (segment-distance-to-soma seg) 0.0) ; This slot hasn't been set yet.
		 (setf (segment-distance-to-soma seg) (+ (segment-length seg) (the sf (distance-to-soma-fast (proximal-segment seg)))))
		 (segment-distance-to-soma seg)))
      (t 0.0))))

(defun electrotonic-distance-to-soma (element)
  "Given an ELEMENT (name or object), returns the electrotonic distance along the tree to the soma in microns."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (process-circuit-structure)		; Just in case this is not done yet
  (let ((seg (element-cell-element element)))
    (typecase seg
      (segment (+ (segment-electrotonic-length seg) (the sf (electrotonic-distance-to-soma (proximal-segment seg)))))
      (t 0.0))))

(defun distal-tips (&optional (cell (cells)))
  "Return a list of all distal tip segments associated with CELL, if supplied, otherwise, all distal tips in circuit."
  (flatten-no-nils-list 
   (loop for cell in (elements cell 'cell) collect
	 (loop for seg in (if cell (cell-segments cell) (segments))
	       when (= (length (distal-segments seg)) 0) collect seg))))

(defun most-distal (&optional (cell *cell*))
  "Return the segment that has the longest dendritic path to the soma of CELL [default *CELL*]."
  (let (max most-distal)
    (loop for seg in (distal-tips cell)
	  when (or (not max) (> (distance-to-soma seg) max))
	  do (setq max (distance-to-soma seg) most-distal seg))
    most-distal))

(defun segments-out (element &optional (segment-skip 0) previous-segs)
  "Starting with the cell element associated with ELEMENT, returns a list of all the segments moving distally, skipping by
SEGMENT-SKIP [default 0]."
  (let* ((segment (element-cell-element element))
	 (result
	  (if (distal-segments segment)
	      (flatten-no-nils-list (cons segment
					  (loop for seg in (distal-segments segment) collect (segments-out seg segment-skip (cons segment previous-segs)))))
	      (list segment))))
    (if (= segment-skip 0)
	result
	(loop for segment in result
	      for count from 0
	      when (= (mod count (1+ segment-skip)) 0) collect segment))))

(defun segments-in (element &optional (segment-skip 0))
  "Returns an ordered inclusive list of all the segments starting from the segment associated with ELEMENT on the path to the soma,
skipping by SEGMENT-SKIP [default 0]."
  (let ((segment (element-cell-element element)))
    (when (segment-p segment)
      (let ((result (if (proximal-segment segment)
		      (flatten-list (list (segments-in (proximal-segment segment) nil) (list segment)))
		      (list segment))))
	(if segment-skip
	  (loop for segment in (reverse result)
		for count from 0
		when (= (mod count (1+ segment-skip)) 0) collect segment)
	  result)))))

(defun check-loop-at-seg (segment &optional previous-segs)
  (when segment
    (format t "Proximal-segment for ~A: ~A~%" segment (proximal-segment segment))
    (if (member segment previous-segs)
      (progn (format t "Found loop ~A ~A~%" segment previous-segs)
	     (break))
      (check-loop-at-seg (proximal-segment segment)
			 (if previous-segs
			   (cons segment previous-segs)
			   (list segment))))))

(defun segments-to-soma (segment &optional (segment-skip 0)) (segments-in segment segment-skip))

(defun trunk-segment (element)
  "Returns the trunk segment associated with the dendritic branch that includes ELEMENT."
  (let ((element (element-cell-element element)))
    (if (proximal-segment element)
      (trunk-segment (proximal-segment element))
      element)))

(defun trunk-segments (&optional element)
  "Return a list of trunk segments for the cell associated with ELEMENT, if supplied, otherwise all in circuit."
  (flatten-no-nils-list
   (loop for cell in (coerce-to-list (or (element-cell element) (cells)))
	 collecting
	 (let* ((soma (cell-soma cell))
		(soma-segments (soma-segments soma)))
	   (list
	    ;; Pick up segments abutting those assigned to the "soma".
	    (loop for seg in soma-segments collect
		  (loop for seg in (distal-segments seg)
			unless (member seg soma-segments)
			collect seg))
	    ;; Pick up segments abutting soma which are not assigned to the "soma".
	    (true-soma-trunks soma))))))

(defun true-soma-trunks (soma)
  ;; Pick up segments abutting soma which are not assigned to the "soma".
  (let ((soma (element soma 'soma)))
    (when soma
      (let ((soma-segments (soma-segments soma)))
	(loop for elt in (node-elements (soma-node soma))
	      when (and (segment-p elt) (not (member elt soma-segments)))
	      collect elt)))))

(defun primary-segs-internal (cell) (loop for seg in (distal-segments (cell-soma cell)) append (segs-until-bifurcation seg)))

(defun PRIMARY-SEGS (&optional element)   
  "Returns a list of all segments of the cell of ELEMENT, if supplied, otherwise all in circuit, that are proximal to the first branch point."
  (let ((cell-ref (or (element-cell element) (cells))))
    (if (listp cell-ref)
	(flatten-no-nils-list (loop for cell in cell-ref collect (primary-segs-internal cell)))
	(primary-segs-internal cell-ref))))

(defun cell-distal-segments (&optional (cell *cell*))
  "Returns a list of all the distal segments of CELL [default *CELL*]."
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (distal-segments seg)
	  collect seg)))

(defun count-cell-distal-segments (&optional (cell *cell*))
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (distal-segments seg)
	  sum 1)))

(defun random-segments (percent-of-total &optional (cell *cell*))
  (let* ((cell (element cell 'cell))
	 (segments (if cell (cell-segments cell) (segments))))
    (RANDOM-SUBSEQ segments (round (* .01 percent-of-total (length segments))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter display
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun label-for-element-parameter-value (param-key)
  (case param-key
    (gbar "Gbar")
    (distance "Distance to soma")
    (length "Length")
    (diameter "Diameter [um]")
    (area "Memb Area")
    (capacitance "Memb Capacitance")
    (t (format nil "~A" (or param-key "")))))

(defun units-for-element-parameter-value (param-key)
  (case param-key
    (gbar "uS")
    ((length diameter distance) "um")
    (area "um2")
    (capacitance "nF")
    (t (format nil "~A" (or param-key "")))))

(defun get-lists-of-parameter-values (parameters parameter-functions elements)
  (loop for param in parameters
	for param-count from 1
	collect
	(let ((param-fun (or (nth (1- param-count) parameter-functions) param)))
	  (no-nils (loop for elt in elements
			 collect (case param-fun
				   (area (element-area-fast elt))
				   (distance (DISTANCE-TO-SOMA-fast elt))
				   (length (element-length elt))
				   (capacitance (element-capacitance elt))
				   (diameter (element-diameter elt))
				   ((or pbar gbar) (element-iv-relation-fast elt))
				   ((or pbar-density gbar-density iv-relation-density) (element-iv-density-fast elt)) 
				   (t (cond ((macro-function param-fun) (eval (macroexpand (list param-fun elt))))
					    ((fboundp param-fun) (funcall param-fun elt))
					    (t (or (ELEMENT-PARAMETER-OR-SLOT-FUNCTION elt param-fun)
						   (sim-error (format nil "No answer for ~A on ~A" param-fun elt))))))))))))

(defun element-param-distribution (model-type parameter &key parameter-function cell note param-max param-min type-for-title
				   x-label y-label x-min x-max x-inc (x-axis-tick-skip 0) x-are-fns y-min y-max y-inc (y-axis-tick-skip 0) y-are-fns
				   bin-width include-simulation-name (width 350) (height 300) font title-position create-new-window)
  "Plots properties of all elements associated with MODEL-TYPE (associated with CELL, if supplied, otherwise all in the circuit). MODEL-TYPE either refers
to a class of elements (e.g. 'SEGMENT, 'CHANNEL, 'SYNAPSE) or a specific element type (e.g. a particular channel or synapse type). PARAMETER may be a
single symbol or a list of two symbols, consistent with MODEL-TYPE, and for a given ELT of MODEL-TYPE the symbols can include:

 'AREA         =>  (element-area elt)
 'DISTANCE     =>  (distance-to-soma elt)
 'DIAMETER     =>  (element-diameter elt)
 'CAPACITANCE  =>  (element-capacitance elt)
 'GBAR         =>  (element-gbar elt)
 'GBAR-DENSITY =>  (/ (element-gbar elt) (element-area elt))

If ELT is on a segment, then:

 'LENGTH       =>  (segment-length (element-cell-element elt))

In the case of a single PARAMETER symbol, a histogram is plotted. Otherwise, a scatter plot is generated for the two properties in the PARAMETER list.

"
  (process-circuit-structure)		; Just in case.
  (let* ((*create-new-plot-windows* (or create-new-window *create-new-plot-windows*))
	 (cell (element cell 'cell))
	 (parameters (coerce-to-list parameter))
	 (parameter-functions (coerce-to-list parameter-function))
	 (types (unless (member model-type *circuit-element-model-names*) (elements model-type)))
	 (types-elements (if (member model-type *circuit-element-model-names*)
			     (list (elements model-type))
			     (loop for type in types collect (elements-of-type type cell))))
	 (title (apply 'concatenate-strings
		       (flatten-no-nils-list 
			(when cell (massage-element-plot-label cell))
			(when cell " - ")
			(or (when type-for-title (format nil "~A " type-for-title))
			    (if (and types (< (length types) 4))
				(loop for thing in types collect (list (massage-element-plot-label thing) " "))
				(format nil "~:(~A~:) " (type-of (caar types-elements)))))
			(concatenate-string-list (loop for param in parameters collect (label-for-element-parameter-value param)) :string-spacer " vs. ")
			(when (and include-simulation-name (> (length *simulation-name*) 0)) (format nil ": ~A" *simulation-name*)))))
	 (labels (element-name types))
	 (x-label (or x-label (units-for-element-parameter-value (nth 0 parameters))))
	 (y-label (or y-label (units-for-element-parameter-value (nth 1 parameters))))
	 (all-lists-of-parameter-values (loop for elements in types-elements collect (get-lists-of-parameter-values parameters parameter-functions elements))))
    (when (car (nth 0 all-lists-of-parameter-values))
      (cond ((= 2 (length (nth 0 all-lists-of-parameter-values)))
	     (plot-scatter (loop for lists-of-parameter-values in all-lists-of-parameter-values collect
				 (mapcar 'list (nth 0 lists-of-parameter-values) (nth 1 lists-of-parameter-values)))
			   labels
			   :width width :height height ; :preserve-plot-layout t
			   :x-min x-min :x-max x-max :x-inc x-inc :x-are-fns x-are-fns :y-min y-min :y-max y-max :y-inc y-inc :y-are-fns y-are-fns
			   :x-symbol-width 4 :scatter-symbol-borderp nil :width width :height height :x-label x-label :y-label y-label :title title :comment note))
	    ((= 1 (length (nth 0 all-lists-of-parameter-values)))
	     (let ((parameter-values (nth 0 (nth 0 all-lists-of-parameter-values))))
	       (plot-histogram parameter-values
			       :title title
			       :bin-width bin-width :bin-min param-min :bin-max param-max :bins 10
			       :width width :height height :bin-width bin-width :stipple-percent 50
			       :x-axis-tick-skip x-axis-tick-skip :x-are-fns x-are-fns
			       :y-min 0 :y-max y-max :y-inc y-inc :y-axis-tick-skip y-axis-tick-skip
			       :x-label x-label :y-label "# Elements" :font font :title-position title-position :comment note)))))))

(defun membrane-area-distribution (&optional (cell *cell*) &key x-axis-tick-skip (plot-pdf-histogram t) (distance-increment 10)
				   histogram-x-max y-inc return-distribution (pdf-title "Distribution of Membrane Area vs. Distance to Soma"))
  "Construct the distribution of membrane area as a function of distance from the soma for all cell elements referenced by CELL [default *CELL*]. Resulting
distribution is binned with increments given by DISTANCE-INCREMENT, in microns [default 10], and is plotted as a histogram with PDF-TITLE when
PLOT-PDF-HISTOGRAM is T. Y-INC and X-AXIS-TICK-SKIP apply to the plotted histogram, as specified in the function PLOT-PDF-HISTOGRAM [default T]. The
distribution is returned as a list of (Distances Areas) when RETURN-DISTRIBUTION is T."
  (process-circuit-structure)		; Just in case.
  (CONSTRUCT-PDF (loop for elt in (cell-elements cell) collect (list (distance-to-soma elt) (element-area elt)))
		 :plot-pdf-histogram plot-pdf-histogram :x-axis-tick-skip x-axis-tick-skip :x-are-fns t
		 :x-inc distance-increment :histogram-x-max histogram-x-max :y-inc y-inc :y-label "um2" :x-label "um" :pdf-title pdf-title
		 :return-pdf-xy-lists return-distribution))

(defun segment-max-info (&optional cell)
  (loop for cell in (coerce-to-list (or (element cell 'cell) (cells))) do
	(loop for segment in (cell-segments cell)
	      collecting (list Segment (segment-length segment)) into segslens
	      collecting (list Segment (element-area segment)) into segsareas
	      collecting (list Segment (segment-diameter segment)) into segsdiams
	      finally
	      (format t "**** Segment with Max length ****~%") (print-element (caar (sort segslens `> :key 'cadr)))
	      (format t "~%**** Segment with Max area ****~%") (print-element (caar (sort segsareas `> :key 'cadr)))
	      (format t "~%**** Segment with Max diameter ****~%") (print-element (caar (sort segsdiams `> :key 'cadr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element diameter, area and volume
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-diameter (element &optional new-diameter)
  "For the the cell element of ELEMENT, return the diameter of the segment or soma in microns. If NEW-DIAMETER is a number, then
the cell element diameter will be changed to this value, and *CIRCUIT-PROCESSED* will be set to NIL."
  (setf (get 'element-diameter 'units) microns-string)
  (let ((cell-element (element-cell-element element)))
    (if new-diameter
	(progn (setq *circuit-processed* nil) (element-slot cell-element :diameter (s-flt new-diameter)))
	(element-slot cell-element :diameter))))

(defun element-length (element &optional new-length)
  "When the cell element of ELEMENT is a segment, return the length of the segment in microns. If NEW-LENGTH is a number, then the
segment length will be changed to this value, and *CIRCUIT-PROCESSED* will be set to NIL."
  (setf (get 'element-length 'units) microns-string)
  (let ((cell-element (element-cell-element element)))
    (if new-length
	(progn (setq *circuit-processed* nil) (element-slot cell-element :length (s-flt new-length)))
	(element-slot cell-element :length))))

(defun soma-area (cell-element &optional consider-virtual-elements)
  "Return the membrane area of the soma associated with CELL-ELEMENT, in um2. The area of any virtual soma segments is included
when CONSIDER-VIRTUAL-ELEMENTS is T."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (get 'soma-area 'units) square-microns-string)
  (let ((soma (typecase cell-element
		(soma cell-element)
		(t (element-cell-element cell-element)))))
    (unless (soma-p soma) (sim-error (format nil "~A does not reference a soma." cell-element)))
    (let ((params (soma-parameters soma))
	  (soma-segments (soma-segments soma)))
      (+ (if (and consider-virtual-elements soma-segments)
	     (loop for seg in soma-segments sum (the sf (segment-area seg)) into area single-float finally (return area))
	     0.0)
	 (let ((length (get-a-value 'length params))
	       (diameter (the sf (if (get-a-value 'soma-cylinder params) (get-a-value 'soma-cylinder-diameter params) (soma-diameter soma)))))
	   (* (the sf (or (get-a-value 'membrane-area-coefficient params) 1.0))
	      (if (and length (get-a-value 'soma-cylinder params))
		  (* pi-single (* diameter (the sf length))) ; Cylinder area, without ends.
		  (* pi-single (* diameter diameter))))) ; Sphere area
	 (if (get-a-value 'adjust-area-for-trunks params)
	     (* -1 pi-single (square (half (s-flt (loop for seg in (true-soma-trunks soma) sum (segment-diameter seg))))))
	     0)))))

(defun segment-area (cell-element)
  "Return the membrane area of the segment associated with CELL-ELEMENT, in um2."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (get 'segment-area 'units) square-microns-string)
  (let ((segment (typecase cell-element
		   (segment cell-element)
		   (t (element-cell-element cell-element)))))
    (unless (segment-p segment) (sim-error (format nil "~A does not reference a segment." cell-element)))
    (* (the sf (or (get-a-value 'membrane-area-coefficient (segment-parameters cell-element)) 1.0))
       pi-single (segment-diameter cell-element) (segment-length cell-element))))

(defun element-area-fast (element &optional consider-virtual-elements)
  (typecase element
    (soma (soma-area element consider-virtual-elements))
    (segment (segment-area element))
    (t (element-area-fast (or (element-slot element :cell-element)
			      (element-cell-element element nil t))))))
    
(defun element-area (element &optional consider-virtual-elements model-type)
  "The total area of somas and segments associated with ELEMENT, in square microns \(single float\).  Segment areas do not include
the cylinder ends \(only the lateral areas are considered\). If somas have 'ADJUST-AREA-FOR-TRUNKS parameter, then their area is
adjusted for the areas of the faces of any abutting segments. If CONSIDER-VIRTUAL-ELEMENTS, any virtual soma segments will be
included in somatic area calculations."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (get 'element-area 'units) square-microns-string)
  (loop for cell-element in (coerce-to-list (element-cell-element element model-type)) sum 
	(typecase cell-element			
	  (soma (soma-area cell-element consider-virtual-elements))
	  (segment (segment-area cell-element))
	  (t 0.0))
	into area single-float finally (return area)))

(defun element-area-cm2 (element &optional virtual-element model-type) (* 1.0e-8 (element-area element virtual-element model-type)))

(defun element-volume (element &optional consider-virtual-elements model-type)
  "Total volume of cell elements associated with ELEMENT in cubic microns \(single-float\). If a cell is given by ELEMENT, then
the total cell volume is considered."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for cell-element in (coerce-to-list (element-cell-element element model-type)) sum 
	(typecase cell-element			
	  (soma (+ (if (and consider-virtual-elements (soma-segments (soma-cell cell-element)))
		       (element-volume (soma-segments (soma-cell cell-element)))
		       0.0)
		   (let* ((params (soma-parameters cell-element))
			  (length (get-a-value 'length params))
			  (diameter (the sf (if (get-a-value 'soma-cylinder params) (get-a-value 'soma-cylinder-diameter params) (soma-diameter cell-element)))))
		     (if (and length (get-a-value 'soma-cylinder params))
			 (* pi-single (square (half diameter)) (the sf length))	; Cylinder volume
			 (sphere-volume-from-diameter (soma-diameter cell-element))))))
	  (segment (cylinder-volume (segment-length cell-element) (segment-diameter cell-element)))
	  (t 0.0))
	into volume single-float
	finally (return volume)))

(defun element-volume-cm3 (element &optional virtual-element model-type)
  ;; single-float
  (* 1.0e-4 1.0e-4 1.0e-4		; cm3/um3
     (element-volume element virtual-element model-type)))

(defun element-sv-ratio (element &optional consider-virtual-elements model-type)
  "Ratio of area divided by volume of the cell elements associated with ELEMENT, in 1/microns \(single-float\)."
  (/ (element-area element consider-virtual-elements model-type)
     (element-volume element consider-virtual-elements model-type)))

;; rectified single-float, um3
(defun element-concentration-volume (element &optional consider-virtual-elements model-type)
  "Returns the volume in um^3 of the cell element associated with ELEMENT, minus the volume of any
nucleus associated with the cell element, as indicated by the element parameter 'nucleus-diameter in
microns."
  (let* ((element (element-cell-element element))
	 (nucleus-diameter (or (element-parameter element 'nucleus-diameter) 0.0)) ; um
	 (nucleus-volume (sphere-volume-from-diameter nucleus-diameter))) ; in um3, if any
    (max 0.0 (- (element-volume element consider-virtual-elements model-type)
		nucleus-volume))))

(defun element-concentration-volume-cm3 (element &optional consider-virtual-elements model-type)
  (* (element-concentration-volume element consider-virtual-elements model-type)
     1.0e-4 1.0e-4 1.0e-4		; cm3/um3
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-data-types (element &optional model-type)
  "Returns a list of symbols corresponding to all possible types of plot data appropriate for ELEMENT or for the associated child type, with the first being the default data type."
  (model-output-data-types (element-model element model-type)))

(defun default-data-type (element &optional model-type model)
  "Returns a symbol corresponding the default type of plot data appropriate for ELEMENT or for the associated child type. Apropos for element types with more than one type of data."
  (declare (ignore model))
  (model-output-default-data-type (element-model element model-type)))

(defun disable-all-element-plot ()
  "Disables plotting of all circuit elements."
  (clear-all-plot-lists))

(defun element-plot-enabled-p (element &optional data-type model-type)
  (let ((element (element element model-type)))
    (loop for plot-list-info in *PLOT-LISTS-INFO*
	  thereis (and (eq (or data-type (default-data-type element model-type))
			   (plot-list-info-structure-slot plot-list-info))
		       (member (type-of element) (plot-list-info-types plot-list-info))
		       (member element (symbol-value (plot-list-info-structures plot-list-info)))))))

(defun plotted-elements ()
  "Return a list of all element names and the data types for which data is currently saved, in the form:

    ((<element name> <data type>) (<element name> <data type>) ...)

"
  (flatten-no-nils-list
   (loop for plot-list-info in *PLOT-LISTS-INFO*
	 when (and (plot-list-info-enable-var plot-list-info)
		   (plot-list-info-structures plot-list-info))
	 collect (loop for element in (symbol-value (plot-list-info-structures plot-list-info))
		       collect (list (element-name element) (plot-list-info-structure-slot plot-list-info))))))

(defun clear-all-plot-lists ()
  (loop for plot-list-info in *plot-lists-info* do (setf (symbol-value (plot-list-info-names plot-list-info)) nil)))

(defun non-nil-plot-lists ()
  (loop for plot-list-info in *plot-lists-info* when (symbol-value (plot-list-info-names plot-list-info)) collect (plot-list-info-names plot-list-info)))
	
(defun toggle-element-analysis (element ANALYSIS-P) 
  (when (cell-element-p element)
    (let ((name (element-name element)))
      (if analysis-p
	  (unless (member name *analysis-nodes* :test 'equal) (push name *analysis-nodes*))
	  (when (member name *analysis-nodes* :test 'equal) (setq *analysis-nodes* (remove name *analysis-nodes*)))))))

(defun toggle-element-plot (elements data-type model-type plot-p &optional toggle-analysis analysis-p (choose-plot-data t))
  (loop for elt-ref in (flatten-list elements) do
       (let ((element-list (if (and (element-type-p elt-ref) (not model-type))
			       (elements-of-type elt-ref)
			       (list elt-ref)))
	     (model-type (if (and (element-type-p elt-ref) (not model-type))
			     (type-symbol-child-structure-type (type-of elt-ref))
			     model-type)))
	 (loop for element in element-list
	    do
	      (let* ((element (if (cell-p (element element model-type)) (cell-soma (element element model-type)) (element element model-type)))
		     (data-type (or data-type (DEFAULT-DATA-TYPE element)))
		     (hash-table (element-hash-table element)))
		(loop for data-type in (coerce-to-list data-type) do
		     (if (or (eq data-type 'all) (eq data-type :all))
			 (toggle-element-plot element (all-data-types element) model-type plot-p toggle-analysis analysis-p nil)
			 (progn
			   (when (eq data-type 'voltage-derivative) (setq data-type 'dvdt) )
			   (when (particle-element-and-markov-states-data-type-p element data-type) (element-parameter element 'plot-markov-states plot-p))
			   (if toggle-analysis
			       (toggle-element-analysis element ANALYSIS-P)
			       (toggle-element-plot-core element data-type plot-p hash-table model-type)))))))))
  (when choose-plot-data (choose-plot-data))
  nil)

(defun toggle-element-plot-core (element data-type plot-p hash-table model-type)
  ;; DATA-TYPE may be a list, for example when :ALL is the enable/disable-element-plot option, and the result of ALL-DATA-TYPES is
  ;; a list of lists.
  (mapcar #'(lambda (plot-list-info)
	      ;; (print (plot-list-info-structure-slot plot-list-info))
	      (when (and (find hash-table (plot-list-info-tables plot-list-info))
			 (if (consp data-type)
			     (member (plot-list-info-structure-slot plot-list-info) data-type)
			     (eq (plot-list-info-structure-slot plot-list-info) data-type))
			 (not (eq (first plot-list-info) '*plot-path-nodes*))
			 (xor plot-p (member (element-name element model-type) (symbol-value (plot-list-info-names plot-list-info)) :test 'equal))
			 (or (not (particle-p (element element)))
			     (case data-type
			       (state t)
			       (markov-state (eq :markov (particle-type-class (element-type element)))))))
		;; (printvars plot-list-info data-type)
		(if plot-p
		    (setf (symbol-value (plot-list-info-enable-var plot-list-info)) t
			  (symbol-value (plot-list-info-names plot-list-info))
			  (cons (element-name element model-type) (symbol-value (plot-list-info-names plot-list-info))))
		    (setf (symbol-value (plot-list-info-names plot-list-info))
			  (remove (element-name element model-type) (symbol-value (plot-list-info-names plot-list-info)) :test 'equal)))))
	  *plot-lists-info*)
  nil)

(defun particle-element-and-markov-states-data-type-p (element data-type)
  (and (particle-p (element element))
       (eq data-type 'markov-states)
       (eq :markov (particle-type-class (element-type element)))))
  
(defun enable-element-plot (element &optional data-type model-type)
  "Enable plot of DATA-TYPE (as in ELEMENT-DATA, but also including 'EVENT for axons and synapses) of elements in ELEMENT of MODEL-TYPE. If ELEMENT is an
element type, and MODEL-TYPE is set to the apropriate element type (e.g. 'channel-type or 'synapse-type), then plotting of the element type itself is
enabled; otherwise, if MODEL-TYPE is not supplied, then all elements of that type are individually affected. For elements that can generate more than one
type of simulation data, setting DATA-TYPE to :ALL will enable all plotting of all data types, as given by ALL-DATA-TYPES. DATA-TYPE may also be a list of
data types."
  (toggle-element-plot (element (flatten-list element) model-type) data-type model-type t))

(defun disable-element-plot (element &optional data-type model-type)
  "As enable-element-plot, but disables plot."
  (case element
    ((:all all) (clear-all-plot-lists))
    (t (toggle-element-plot (element (flatten-list element) model-type) data-type model-type nil))))

(defun shadow-plotted-nodes (element-type-to-plot)
  "For all the segments and somas that are currently plotted, enable plotting for the associated membrane elements. ELEMENT-TYPE-TO-PLOT can be 'CHANNELS,
'SYNAPSES, 'PARTICLES, 'CONC-PARTICLES, etc."
  (enable-element-plot (funcall element-type-to-plot (delete-duplicates (cell-element (mapcar 'car (plotted-elements)))))))

(defun unshadow-plotted-nodes (element-type-to-plot)
  "For all the segments and somas that are currently plotted, disable plotting for the associated membrane elements. ELEMENT-TYPE-TO-PLOT can be 'CHANNELS,
'SYNAPSES, 'PARTICLES, 'CONC-PARTICLES, etc."
  (disable-element-plot (funcall element-type-to-plot (delete-duplicates (cell-element (mapcar 'car (plotted-elements)))))))

(defun setup-plot-total-conductances (&optional spec-list)
  "Set up and enable plotting of total conductances with a SPEC-LIST whose format is described in the documentation for
*PLOT-TOTAL-CONDUCTANCES*. To plot the total conductance of all cells in the circuit:

   (SETUP-PLOT-TOTAL-CONDUCTANCES t)

If called with no argument, then total conductance processing is disabled.

"
  (setq *plot-total-conductances* (if (equal spec-list t) :ALL spec-list)
	*PLOT-TOTAL-CONDUCTANCES-P* (true-p spec-list)))

(defun clear-plot-total-conductances ()
  "Clear and disable plotting of total conductances."
  (setq *plot-total-conductances* nil
	*PLOT-TOTAL-CONDUCTANCES-P* nil))

;; For backward compatibility
(defun enable-element-analysis (element &optional data-type model-type) (enable-element-save-data element data-type model-type))
(defun disable-element-analysis (element &optional data-type model-type) (disable-element-save-data element data-type t model-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plotting characteristics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plot-type (element &optional model-type)
  (element-wrapper (element model-type)
		   (typecase internal-type
		     (channel-type (plot-channel-types internal-type)))))

(defun plot-types (elements) (plot-element elements))
(defun plot-elements (elements) (plot-element elements))

(defun plot-element (element)  
  "Takes a single element or a list of elements for ELEMENT, and plots the intrinsic characteristics [not simulation data] of the
associated element type."
  (loop for elt in (elements element)
	do (if (or (vsource-p elt) (isource-p elt))
	       (plot-source-waveform elt)
	       (loop for type in (element-types elt)
		     do (typecase type
			  (synapse-type (plot-synapse-types type))
			  (particle-type (plot-particle-types type))
			  (channel-type (plot-channel-types type)))))))

(defun plot-segments-to-soma (element &optional (segment-skip 0) clear-first)
  "Enables plotting on a separate window all the segments on the path from the ELEMENT to the soma, skipping path segments by
SEGMENT-SKIP [default 0]. If CLEAR-FIRST [default NIL] is T, then any segments previously including in such a plot are cleared
first."
  (let ((start-segment (element-cell-element element)))
    (when (segment-p start-segment)
      (setq *plot-path-node-voltages-p* t)
      (when clear-first (setq *plot-path-nodes* nil))
      (let ((seg-list (flatten-no-nils-list (segments-to-soma start-segment segment-skip) (list (cell-soma (segment-cell start-segment))))))
	(setq *plot-path-nodes*
	      (concatenate 'list 
			   (loop for seg in (reverse seg-list)
				 do (setq *plot-path-nodes* (remove (element-name seg) *plot-path-nodes* :test 'equal))
				 collect (element-name seg))
			   *plot-path-nodes*))))))

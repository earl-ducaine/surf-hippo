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


;;; SYS Source file: source.lisp

(in-package "SURF-HIPPO")


;; Functions relevant to both current and voltage sources

;; ******* Includes inline functions for vsource.lisp and isource.lisp *************

(defun source-p (element)
  (let ((elt (element element)))
    (or (isource-p elt) (vsource-p elt))))

(defun source-units-string (source)
  (typecase (element source)
    (vsource "mV")
    (isource "nA")))

(defun source-stimulus-p (source)
  (true-p (or (extract-waveform-function source)
	      (source-waveform-array source)
	      (pulse-list source)
	      (pulse-train source))))

(defun sources (&optional cell)
  "Return a list of all current and voltage sources."
  (if cell (nconc (isources cell) (vsources cell)) (nconc (isources) (vsources))))

(defun set-pwl-list (source pwl-list) (element-slot source :pwl-list (list (double-float-list (nth 0 pwl-list)) (float-list (nth 1 pwl-list)))))

(defun source-waveform-time-interval-inverse (source) (element-slot source :waveform-time-interval-inverse))
(defun source-use-pulse-list (source) (element-slot source :use-pulse-list))
(defun source-waveform-array (source) (element-slot source :waveform-array))
(defun set-source-use-pulse-list (source) (element-slot source :use-pulse-list t))
(defun enable-source (source) (element-slot source :blocked nil))
(defun disable-source (source) (element-slot source :blocked t))
(defun turn-on-source (source) (enable-source source))
(defun turn-off-source (source) (disable-source source))

(defun add-isource (&optional (element *soma*) &key name pulse-list (type 'autonomous) (reference-magnitude *isource-default-reference-magnitude*))
  "Adds current source to cell elements associated with optional ELEMENT [default the value of *SOMA*]. Source is called NAME, if
supplied, or is given by EltName-isrc, where EltName is the name of cell-element. Creates new source only if one does not exist of
the derived name. Optional PULSE-LIST may also be supplied. Returns the source\(s\). If ELEMENT refers to a cell, then an isource
is added to that cell's soma. REFERENCE-MAGNITUDE [nA, default *ISOURCE-DEFAULT-REFERENCE-MAGNITUDE*] is used as the
default magnitude for the source. The default 'AUTONOMOUS type is the generic :AUTONOMOUS current source driven by a waveform or pulse
specification \(here given  by PULSE-LIST - see the function PULSE-LIST for the format of this list\)."
  (let* ((element element)
	 (cell-element (if (cell-p element) (cell-soma element) (element-cell-element element))))
    (when cell-element
      (let ((isource (create-isource cell-element :type type :name name :pulse-list pulse-list)))
	(isource-reference-magnitude isource reference-magnitude)
	isource))))

(defun add-vsource (&optional (element *soma*) &key name (ideal t) pulse-list (reference-magnitude *vsource-default-reference-magnitude*))
  "Adds voltage source to cell elements associated with the optional ELEMENT [default the value of *SOMA*], if no voltage source is already there. Source
is called NAME, if supplied, or is given by EltName-vsrc, where EltName is the name of cell-element. If IDEAL then ideal voltage sources are created. An
optional PULSE-LIST may also be supplied. Returns the source\(s\). REFERENCE-MAGNITUDE [mV, default *VSOURCE-DEFAULT-REFERENCE-MAGNITUDE*] is used as the
default magnitude for the source. If ELEMENT refers to a cell, then a vsource is added to that cell's soma."
  (let* ((element element)
	 (cell-element (if (cell-p element) (cell-soma element) (element-cell-element element))))
    (when cell-element
      (let ((vsource (create-vsource cell-element :name name :pulse-list pulse-list)))
	(element-parameter vsource 'ideal-vsource ideal)
	(setq *need-to-reorder-matrix* (or *need-to-reorder-matrix* ideal))
	(vsource-reference-magnitude vsource reference-magnitude)
	vsource))))

(defun cell-isource (element)
  "Returns a current source, from the soma preferably, associated with the cell associated with
ELEMENT, if there are any such sources."
  (let ((cell (element-cell element)))
    (when cell
      (or (car (element-isources (cell-soma cell)))
	  (loop for src in (isources) when (eq (element-cell src) cell) do (return src))))))

(defun cell-vsource (element)
  "Returns a voltage source, from the soma preferably, associated with the cell associated with
ELEMENT, if there are any such sources."
  (let ((cell (element-cell element)))
    (when cell
      (or (car (element-vsources (cell-soma cell)))
	  (loop for src in (vsources) when (eq (element-cell src) cell) do (return src))))))
  
(defun sources-menu ()
  (let (dummy1 dummy2 dummy3 dummy4)
    (choose-variable-values
     `((dummy1 "Add current sources" :boolean)
       (dummy3 "Edit current source parameters" :boolean)
       (dummy2 "Add voltage sources" :boolean)
       (dummy4 "Edit voltage source parameters" :boolean))
     :label "Editing current and voltage sources")
    (cond-every
     (dummy1 (add-sources-menu 'isource))
     (dummy3 (menu-for-isources))
     (dummy2 (add-sources-menu 'vsource))
     (dummy4 (menu-for-vsources)))))

(defun edit-source (source)
  "For editing all SOURCE parameters."
  (typecase (element source)
    (vsource (edit-vsource source))
    (isource (edit-isource source))))

(defun add-sources-menu (type-sym)
  (let* ((type-string (case type-sym (isource "current source") (vsource "voltage source")))
	 (original-sources (list-of-all-things type-sym))
	 (original-source-node-names (coerce-to-list (element-name (element-cell-element original-sources))))
	 (new-source-node-names
	  (nconc (if (ask-for-element-modifications (format nil "Add/remove segment ~ss" type-string) (length (segments)) type-string)
		   (select-hash-values-menu (SEGMENT-HASH-TABLE) (format nil "Select Segments for ~a" type-string)
					    :selected-list (rem-not-in-keys original-source-node-names (namelist-of-all-things 'segment)))
		   (rem-not-in-keys original-source-node-names (namelist-of-all-things 'segment)))
		 (select-hash-values-menu (SOMA-HASH-TABLE) (format nil "Select Soma for ~a" type-string)
					  :selected-list (rem-not-in-keys original-source-node-names (namelist-of-all-things 'soma))))))
    (loop for original-source-node-name in original-source-node-names
	  unless (member original-source-node-name new-source-node-names :test 'equal)
	  do (erase-element (car (cell-element-elements original-source-node-name type-sym))))
    (loop for node-name in new-source-node-names
	  unless (member node-name original-source-node-names :test 'equal)
	  do (case type-sym
	       (isource (create-isource node-name :type 'autonomous))
	       (vsource (create-vsource node-name))))))

(defun pulse-list (source &optional (pulse-list nil pulse-list-supplied))
  "For adding a PULSE-LIST to SOURCE, where the format of PULSE-LIST is either:

  (pulse-1 pulse-2 ...)

or for just a single pulse:

  pulse

The format of each pulse is as follows:

  (start-time stop-time amplitude)

The time parameters are in milliseconds, and amplitude is either nA or mV for current and voltage sources, respectively. For
example a pulse defined with '(4 6 .1) applied to a current source defines a 0.1nA pulse from 4 to 6 milliseconds. This function
will also set the :USE-PULSE-LIST slot for the source. If called with only the SOURCE arg, the pulse-list currently assigned to
the source will be returned. If there is an explicit NIL PULSE-LIST arg any pulse list assigned to SOURCE will be cleared.

Pulse must be separated by a minimum time which is a function of the transition speed, and thus they cannot overlap in
time. Typically the minimum separation is on the order of 0.005 milliseconds. The SOURCE argument is initially processed by
ELEMENT."
  (unless (every 'source-p (elements source)) (sim-error (format nil "~A is not a source!" source)))
  (let* ((pulse-list (cleanup-pulse-list pulse-list))
	 (sources (elements source))
	 (pulse-lists (mapcar (lambda (src)
				(when pulse-list-supplied
				  (element-parameter src 'pulse-list pulse-list)
				  (element-parameter src 'enable-individual-pulses (true-p pulse-list))
				  (if pulse-list
				      (progn (set-source-use-pulse-list src) (setup-source-values src))
				      (progn (element-slot src :pwl-list nil) (element-slot src :use-pulse-list nil))))
				(element-parameter src 'pulse-list))
			      sources)))
    (if (= 1 (length sources)) (car pulse-lists) pulse-lists)))

;; Backward compatibility
(defun add-pulse-list (source &optional pulse-list clear-if-nil) (if (and clear-if-nil (not pulse-list)) (pulse-list source nil) (pulse-list source pulse-list)))

(defun cleanup-pulse-list (pulse-list)
  ;; Clean up the pulse list a bit by deleting pulses with duplicate start times and ordering the pulses wrt time. Separate the
  ;; pulse components into start times, stop times, and mags.  this nonsense to avoid "-0.0". also converts all numbers to s-flts.
  (let ((pulse-list 
	 (loop for pulse in (if (consp (car pulse-list)) pulse-list (list pulse-list))
	       when (and (car pulse) (not (= (length pulse) 3)))
	       do (sim-error (format nil "Choking on ~A. Each pulse must include a start, stop and magnitude entry."  pulse))
	       when (car pulse)
	       collect (s-flt-list  pulse)
	       ; (list (s-flt (nth 0 pulse)) (s-flt (nth 1 pulse)) (s-flt (if (= 0 (nth 2 pulse)) 0.0 (nth 2 pulse))))
	       )))
    ; (sort (delete-duplicates pulse-list :test '= :key 'car) #'> :key 'car)
    (sort pulse-list '< :key 'car)
    ))

(defun document-pulse-list (source &optional (source-name (element-name source)))
  (print `(pulse-list ,(quote-symbol-cons source-name) ,(extract-pulse-list source)))
  nil)

(defun document-waveform-function (source &optional (source-name (element-name source)))
  (print `(element-parameter ,(quote-symbol-cons source-name) 'waveform-function ,(element-parameter source 'waveform-function)))
  nil)

#|
(defun print-create-sources-for-cell (cell &optional (indent-output 0))
  (loop for source in (sources cell) do
	(typecase source
	  (vsource (print-create-vsource source indent-output))
	  (isource (print-create-isource source indent-output)))))
|#

(defun print-pulse-train-info (units-string pulse-train-args)
  (format t " ~a ~a pulse train from ~a ms to ~a ms, pulse duration ~a ms, delay ~a ms, period ~a ms~%"
	  (or (cdr-assoc :amplitude pulse-train-args) 1.0)
	  units-string
	  (or (cdr-assoc :start pulse-train-args) 0.0)
	  (or (cdr-assoc :stop pulse-train-args) *user-stop-time*)
	  (or (cdr-assoc :duration pulse-train-args) 1.0)
	  (or (cdr-assoc :delay pulse-train-args) 0.0)
	  (or (cdr-assoc :period pulse-train-args) 2.0)))

(defun extract-source-lists (source)
  (values (extract-pulse-list source) (extract-pulse-train-args source) (extract-waveform-function source) (element-parameter (element source) 'user-pwl-list)))

(defun extract-pulse-list (source)
  (let ((source-list (element-parameter source 'pulse-list)))
    (when source-list (sort (copy-list source-list) '< :key 'car))))

(defun extract-waveform-function (source) (element-parameter source 'waveform-function))
(defun extract-pulse-train-args (source) (element-parameter source 'pulse-train-args)) 

(defun plot-source-waveform (source &optional model-type)
  (let* ((source (if model-type (element source model-type) (or (element source 'isource) (element source 'vsource))))
	 (delta-t (if (source-use-pulse-list source) 0.1 (/ 1.0 (source-waveform-time-interval-inverse source)))))
    (plot-timed-data (if (source-use-pulse-list source)
		       (list (loop for time from 0.0 to *user-stop-time* by delta-t
				   collect (or (extract-pwl-value time source) 0.0)))
		       (element-control-waveform source))
		     (list "") delta-t
		     :prompt-for-overlay t
		     :title (format nil "~A for ~A"
				    (if (source-use-pulse-list source) "Pulse List Waveform" (or (car (extract-waveform-function source)) "Arbitrary Waveform"))
				    (massage-element-plot-label (element-name source) (type-of source)))
		     :x-label "ms" :y-label (typecase source (isource "nA") (vsource "mV")))))

(defun plot-isource (isource) (plot-source-waveform isource 'isource))
(defun plot-vsource (vsource) (plot-source-waveform vsource 'vsource))

(defun edit-source-stimuli ()
  (let ((constant-current-elements (constant-current-elements)))
    (loop for src-or-sym in
	  (choose-list-values-from-keys
	   (cons (list (format nil "Edit constant currents~A"
			       (if constant-current-elements (format nil "~%(~A element~:P now w/current)" (length constant-current-elements)) ""))
		       :constant-current-element-edit)
		 (loop for src in (sources)
		       when (typecase src
			      (vsource t)
			      (isource (eq :AUTONOMOUS (isource-type-class (isource-type src)))))
		       collect (list (MASSAGE-ELEMENT-PLOT-LABEL src) src)))
	   nil :label "Choose Sources to Modify")
	  do (case src-or-sym
	       (:constant-current-element-edit
		(let ((constant-current-menu-directives
		       (choose-list-values-from-keys
			(cons (when constant-current-elements
				(list (format nil "First clear all~%constant currents") :clear-constant-currents))
			      (cons (list (format nil "Add constant current~%to cell elements") :constant-current-element-global-edit)
				    (loop for constant-current-element in constant-current-elements
					  collect (list (element-name constant-current-element) constant-current-element))))
			nil :label "Editing Constant Current")))
		  (when (member :clear-constant-currents constant-current-menu-directives)
		    (clear-constant-currents))
		  (constant-current-menu
		   (loop for elt-or-sym in constant-current-menu-directives
			 nconc (case elt-or-sym
				 (:constant-current-element-global-edit (cell-elements))
				 (t (list (element-cell-element elt-or-sym))))))))
	       (t (edit-source-stimulus src-or-sym))))))

(defun edit-source-stimulus (source)
  (when (and *surf-interactive* (not *automatic-run*))
    (let (quit-flag)
      (loop until quit-flag do
	    (setup-source-values (element source))
	    (let* ((source (element source))
		   (dummy1 (if (source-use-pulse-list source) :pulses :waveform))
		   (dummy2 (element-enabled-p source))
		   (dummy3 (element-enabled-p source))
		   dummy4)
	      (choose-variable-values '((dummy1 "Select form:" :choose (:pulses :waveform))
					(dummy2 "Turn on this source" :boolean)
					(dummy4 "Plot current stimulus" :boolean)
					(dummy3 "Edit stimulus" :boolean))
				      :label (format nil "Form of Stimulus for ~A" (massage-element-plot-label source)))
	      (if dummy2 (turn-on source) (turn-off source))
	      (setq quit-flag t)
	      (when dummy4 (plot-source-waveform source))
	      (when dummy3
		(let ((*automatic-run* nil)
		      (use-pulse-list (case dummy1 (:waveform nil) (:pulses t))))
		  (element-slot source :use-pulse-list use-pulse-list)
		  (if use-pulse-list
		    (edit-pulse-spec source)
		    (add-waveform source :waveform-spec (extract-waveform-function source) :use-menu t))
		  (when (and (source-stimulus-p source) (go-ahead-menu (format nil "Plot ~A waveform" (element-name source)) "Authorization" nil))
		    (plot-source-waveform source)
		    (setq quit-flag (go-ahead-menu (format nil "Quit ~A edit" (element-name source))))))))))))

(defun bundle-pulse-train-args (start stop delay duration period amplitude)
  (when (and (numberp start) (numberp stop) (numberp delay) (numberp duration) (numberp amplitude))
    `(,(cons :start (s-flt start)) ,(cons :stop (s-flt stop)) ,(cons :delay (s-flt delay))
      ,(cons :duration (s-flt duration)) ,(cons :period (s-flt period)) ,(cons :amplitude (s-flt amplitude)))))
  
(defun edit-pulse-train-args (pulse-train-args source) 
  (let ((dummy1 (or (cdr-assoc :start pulse-train-args) 0.0))
	(dummy2 (or (cdr-assoc :stop pulse-train-args) *user-stop-time*))
	(dummy3 (or (cdr-assoc :delay pulse-train-args) 0.0))
	(dummy4 (or (cdr-assoc :duration pulse-train-args) 1.0))
	(dummy5 (or (cdr-assoc :period pulse-train-args) 2.0))
	(dummy6 (or (cdr-assoc :amplitude pulse-train-args) 1.0))
	(dummy7 (when pulse-train-args t)))
    (choose-variable-values
     `((dummy1 "Start time [ms]" :float)
       (dummy2 "Stop time [ms]" :float)
       (dummy3 "Delay for each pulse [ms]" :float)
       (dummy4 "Pulse duration [ms]" :float)
       (dummy5 "Pulse period [ms]" :float)
       (dummy6 ,(format nil "Pulse level ~a" (typecase source (isource "nA") (vsource "mV"))) :float))
     :label (format nil "Parameters for Source ~a Pulse Train" (element-name source)))
    (element-parameter source 'pulse-train-args (bundle-pulse-train-args dummy1 dummy2 dummy3 dummy4 dummy5 dummy6))))

(defun square-wave-pulse-train (source pulse-period amplitude &optional (duration *user-stop-time*) (start 0.0) (delay 0.0))
  (PULSE-TRAIN source start duration delay (/ pulse-period 2) pulse-period amplitude))

(defun pulse-train (source &optional (start nil start-supplied) stop delay duration period amplitude)
  "When START, STOP, DELAY, DURATION, PERIOD, AMPLITUDE are numbers, assign the corresonding pulse train specification to SOURCE
and enable the pulse train. All time args in milliseconds. AMPLITUDE is in nA or mV depending on whether SOURCE refers to a
current or voltage source, respectively. If called with only the SOURCE arg, the list of pulse train specs [same order as
PULSE-TRAIN args] currently assigned to the source will be returned. If there is an explicit NIL START arg any pulse train
assigned to SOURCE will be cleared, and pulse train will be disabled for this SOURCE. "
  (let ((source (element source)))
    (when (source-p source)
      (if start-supplied
	(progn
	  (element-parameter source 'enable-pulse-train (true-p start))
	  (if start
	    (set-source-use-pulse-list source)
	    (progn (element-slot source :pwl-list nil) (element-slot source :use-pulse-list nil)))
	  (element-parameter source 'pulse-train-args (bundle-pulse-train-args start stop delay duration period amplitude)))
	(element-parameter source 'pulse-train-args)))))

(defun set-pulse-train-args (source start stop delay duration period amplitude)
  (element-parameter source 'enable-pulse-train t)
  (element-parameter source 'pulse-train-args (bundle-pulse-train-args start stop delay duration period amplitude)))

;; (defmacro pulse-number-minus-1 (pulse-list pulse-number) `(nth (1- ,pulse-number) ,pulse-list))

(defun source-pulse-list-menu (pulse-list source)
  (let ((current-start 0.0) source-pulse end-pulses
	(total-pulses (pulse-number-from-menu pulse-list (element-name source)))
	pulse-number-minus-1)
    (loop for pulse-number from 1 to total-pulses
	  do (setq pulse-number-minus-1 (nth (1- pulse-number) pulse-list))
	  when (setq source-pulse
		     (multiple-value-bind (source-pulse-temp end-pulses-temp)
			 (single-PULSE-menu pulse-number source
					    (or (nth 0 pulse-number-minus-1) current-start)
					    (or (nth 1 pulse-number-minus-1) 0.0)
					    (or (nth 2 pulse-number-minus-1) 0.0)
					    total-pulses)
		       (setq end-pulses end-pulses-temp)
		       (when source-pulse-temp (setq current-start (nth 1 source-pulse-temp)))
		       source-pulse-temp))
	  collect source-pulse into output
	  when end-pulses do (return output) finally (return output))))

(defun enable-pulse-train (source)
  "Enables pulse train generation by SOURCE."
  (element-parameter source 'enable-pulse-train t))

(defun disable-pulse-train (source)
  "Disables pulse train generation by SOURCE."
  (element-parameter source 'enable-pulse-train nil))

(defun enable-individual-pulses (source)
  "Enables individual pulse generation by SOURCE."
  (element-parameter source 'enable-individual-pulses t))

(defun disable-individual-pulses (source)
  "Disables individual pulse generation by SOURCE."
  (element-parameter source 'enable-individual-pulses nil))

(defun edit-pulse-spec (source)
  ;; Extracts source-list for the SOURCE from the :PARAMETERS slot. Prompts user to change number of pulses, and calls single-PULSE-menu to edit the
  ;; parameters of each pulse in turn. 
  (multiple-value-bind (pulse-list pulse-train-args waveform-spec user-pwl-list)
      (EXTRACT-SOURCE-LISTs source)
    (declare (ignore waveform-spec user-pwl-list))
    (let ((dummy5 (or (element-parameter source 'reference-magnitude)
		      (typecase source (isource *isource-default-reference-magnitude*) (vsource *vsource-default-reference-magnitude*))))
	  (dummy1 (element-parameter source 'enable-pulse-train))
	  (dummy2 (element-parameter source 'enable-individual-pulses))
	  (dummy3 (element-parameter source 'enable-pulse-train))
	  (dummy4 (element-parameter source 'enable-individual-pulses)))
      (choose-variable-values
       `((dummy1 "Edit pulse train" :boolean)
	 (dummy3 "Enable pulse train" :boolean)
	 (dummy2 "Edit set of individual pulses" :boolean)
	 (dummy4 "Enable individual pulses" :boolean)
	 (dummy5 ,(format nil "Default reference magnitude [~A]" (typecase source (isource "nA") (vsource "mV"))) :number))
       :label (format nil "Pulse menu for Source ~A" (element-name source))
       :text "Pulse train and individual pulses are added together.")
      (element-parameter source 'reference-magnitude dummy5)
      (element-parameter source 'enable-pulse-train dummy3)
      (element-parameter source 'enable-individual-pulses dummy4)
      (cond-every
       (dummy1 (setq pulse-train-args (edit-pulse-train-args pulse-train-args source)))
       (dummy2 (pulse-list source (source-pulse-list-menu pulse-list source)))))))

(defun pulse-train-from-args (pulse-train-args)
  (when pulse-train-args
    (let ((start (cdr-assoc :start pulse-train-args))
	  (stop (cdr-assoc :stop pulse-train-args))
	  (delay (cdr-assoc :delay pulse-train-args))
	  (duration (cdr-assoc :duration pulse-train-args))
	  (period (cdr-assoc :period pulse-train-args))
	  (amplitude (cdr-assoc :amplitude pulse-train-args)))
      (loop for time from start to stop by (max period 0.1) 
	    collect (list (+ time delay) (+ time delay duration) amplitude)))))

(defun single-PULSE-menu (pulse-number source start stop amplitude total-pulses)
  ;; Generates a menu that prompts for the current/voltage pulse parameters. Returns a list '(start-time stop-time pulse-amplitude).
  (let* ((source (element source))
	 (units (typecase source (isource "[nA]") (vsource "[mV]")))
	 (dummy1 (float start))
	 (dummy2 (float stop))
	 (dummy3 (float amplitude))
	 dummy4)
    (choose-variable-values
     `((dummy1 "Pulse start time [ms]" :number :precision)
       (dummy2 "Pulse stop time [ms]" :number :precision)
       (,(format nil "Pulse reference level ~,2f~A" (element-parameter source 'REFERENCE-MAGNITUDE) units) :comment)
       (dummy3 ,(format nil "Pulse level ~A" units) :number :precision)
       ,(unless (= total-pulses pulse-number) `(dummy4 "Cancel rest of pulses" :boolean)))
     :text (format nil "Clamp pulse ~d (out of ~A, stop time ~a ms)" pulse-number total-pulses *user-stop-time*)
     :label (format nil "Pulse for ~A" (element-name source)))
    (values (list dummy1 dummy2 dummy3) dummy4)))

(defun setup-source-values (source)
  ;; Loads the SOURCE with either a PWL-LIST for pulses, or a waveform array. The initial call to EXTRACT-SOURCE-LIST also may set/reset the
  ;; :USE-PULSE-LIST slot if the source has no waveform or pulse information in its :PARAMETERS slot. Depending on the resulting value of :USE-PULSE-LIST,
  ;; ADD-WAVEFORM is called on the extracted SOURCE-LIST, otherwise a list of pulses is converted into PWL-LIST. Each pulse is approximated in a piece-wise
  ;; linear fashion such that the slope of each transition is determined by *PWL-ISOURCE-DI-DT* or *PWL-VSOURCE-DV-DT*, as appropriate. The
  ;; piece-wise-linear list PWL-LIST contains the breakpoints of the pwl approximation, as follows:
  ;;
  ;;    `((bp-time1  bp-time2  bp-time3  ... bp-timeN)
  ;;      (bp-value1 bp-value2 bp-value3 ... bp-valueN)
  ;;
  ;;  This is a 2D list ([2,N], where N is the number of breakpoints), with time points in the first sublist and source values in the second sublist.
  (typecase source
    (isource (setf (isource-current source) 0.0)))
  (multiple-value-bind (pulse-list pulse-train-args waveform-spec user-pwl-list)         
      (EXTRACT-SOURCE-LISTs source)
    (when (or pulse-list pulse-train-args waveform-spec user-pwl-list)
      (let ((use-pulse-spec (and (or pulse-list pulse-train-args user-pwl-list)
				 (or (source-use-pulse-list source) ; (not waveform-spec)
				     )))
	    (enable-pulse-train (element-parameter source 'enable-pulse-train))
	    (enable-individual-pulses (element-parameter source 'enable-individual-pulses)))
	(element-slot source :use-pulse-list use-pulse-spec)
	(if use-pulse-spec
	    (if user-pwl-list
		(set-pwl-list source user-pwl-list)
		(convert-and-add-pulse-list-to-PWL-LIST
		 source
		 (sort (concatenate 'list (when enable-individual-pulses pulse-list) (when enable-pulse-train (pulse-train-from-args pulse-train-args)))
		       '< :key 'car)))
	    (let ((*automatic-run* t))	; Kill menus.
	      (when nil			; waveform-spec
		(add-waveform source :waveform-spec waveform-spec :use-menu nil))))))))

(defun convert-and-add-pulse-list-to-PWL-LIST (source &optional (pulse-list (pulse-list source)))
  ;; Convert a list PULSE-LIST of pulses (start stop mag) into a list of breakpoints and amplitudes, taking into account limits on transition slopes.
  ;; PULSE-LIST format is ((start stop amp) ... ), e.g. '((10.0 20.0 2.0) (100.0 110.0 -5.0) ... ) - pulses which overlap in time are summed.
  (let* ((reference-magnitude (or (element-parameter source 'REFERENCE-MAGNITUDE)
					  (typecase source (isource *isource-default-reference-magnitude*) (vsource *vsource-default-reference-magnitude*))))
	 (max-source-slope (or (element-parameter source 'PULSE-TRANSITION-SLOPE) (typecase source (isource *pwl-isource-di-dt*) (vsource *pwl-vsource-dv-dt*))))
	 (transition-time (or (element-parameter source 'PULSE-TRANSITION-TIME) 0.1)) ; ms
	 (minimum-source-transition-time (or (element-parameter source 'minimum-source-transition-time) *minimum-source-transition-time* 0.0))) ; ms
    (flet ((pulse-start (pulse) (nth 0 pulse))
	   (pulse-stop (pulse)  (nth 1 pulse))
	   (pulse-amp (pulse)   (nth 2 pulse))
	   (transition-time (delta-pulse) (max minimum-source-transition-time
					       (case (or (element-parameter source 'pulse-transition-constraint) :fixed-slope)
						 (:fixed-slope (/ (abs delta-pulse) max-source-slope))
						 (:fixed-transition-time transition-time))))
	   (transition-start (transition) (nth 0 transition))
	   (transition-amp (transition) (nth 1 transition)))
      (let* ((transitions (sort (mapcan (lambda (pulse) `((,(pulse-start pulse) ,(pulse-amp pulse)) (,(pulse-stop pulse) ,(- (pulse-amp pulse))))) pulse-list) '< :key 'car))
	     (bps `(0))
	     (1st-pulse-at-zero (zerop (transition-start (car transitions))))
	     (amplitudes `(,(+ reference-magnitude (if 1st-pulse-at-zero (transition-amp (car transitions)) 0)))))
	(mapcar (lambda (transition) 
		  (let ((last-amp (car amplitudes)))
		    (push (transition-start transition) bps)
		    (push last-amp amplitudes)
		    (push (+ (transition-start transition) (transition-time (transition-amp transition))) bps)
		    (push (+ last-amp (transition-amp transition)) amplitudes)))
		(if 1st-pulse-at-zero (cdr transitions) transitions))
	(set-pwl-list source (list (reverse bps) (reverse amplitudes)))))))

(defun are-there-sources () (true-p (or *isource* *vsource*)))
(defun set-all-sources () (when *advance-sources* (set-all-isources)))

(defun source-stimulus-p (source)
  (true-p
   (if (element-slot source :use-pulse-list)
     (or (and (element-parameter source 'enable-individual-pulses) (extract-pulse-list source))
	 (and (element-parameter source 'enable-pulse-train) (extract-pulse-train-args source)))
     (or (extract-waveform-function source)
	 (source-waveform-array source)))))

(defun setup-sources ()
  (let* ((active-non-ideal-vsources
	  (loop for src in (vsources)
		when (and (not (vsource-blocked src)) (not (vsource-use-pulse-list src)))
		do (setf (vsource-waveform-time-interval-mrt src) (round (/ 1.0 (* *mrt* (vsource-waveform-time-interval-inverse src)))))
		when (and (source-stimulus-p src) (not (element-parameter src 'ideal-vsource)) (not (vsource-blocked src)))
		collect src))
	 (fixed-active-vsources
	  (loop for src in (vsources)
		when (and		; (source-stimulus-p src)
		      (element-parameter src 'ideal-vsource) (not (vsource-blocked src)))
		collect src))
	 (active-isources
	  (loop for src in (isources)
		when (and (not (isource-blocked src)) (not (isource-use-pulse-list src)))
		do (setf (isource-waveform-time-interval-mrt src) (round (/ 1.0 (* *mrt* (isource-waveform-time-interval-inverse src))))
			 (isource-current src) 0.0)
		(when (numberp (element-parameter src :bridge-balance))
		  (element-parameter src :bridge-balance (s-flt (element-parameter src :bridge-balance))))
		when (and (not (isource-blocked src)) (source-stimulus-p src)) 
		collect src)))
    (setq *non-ideal-vsource-list* active-non-ideal-vsources
	  *fixed-vsource-list* fixed-active-vsources
	  *isource-list* active-isources))
  nil)

(defun init-sources ()
  (loop for src-list in (list *non-ideal-vsource-list* *fixed-vsource-list* (isources)) do
	(loop for src in src-list do (setup-source-values src))
	(fix-source-bps src-list))
  (INIT-VSOURCEs))

(defun queue-pwl-source-break-points (value-array period delay)
  ;; Looks at the pwl breakpoints, and puts each time on the queue of break points so that the simulation can be sure to step there.
  (dotimes (i (ceiling (/ *user-stop-time* period)))
    (typecase value-array
      (array (loop for index from 0 to (1- (nth 1 (array-dimensions value-array))) do
		   (queue-internal-breakpoint-time (s-flt (+ (aref value-array 0 index) (* i period) delay)))))
      (cons (loop for val in (nth 0 value-array) do (queue-internal-breakpoint-time (s-flt (+ val (* i period) delay))))))))

(defun fix-source-bps (srcs)
  (loop for src in srcs do
	(when (element-slot src :pwl-list)
	  (queue-pwl-source-break-points (element-slot src :pwl-list)
					 (if (= 0 (element-slot src :period))
					     (* 2.0 *user-stop-time*)
					     (element-slot src :period))
					 (element-slot src :delay)))
	(queue-internal-breakpoint-times (element-parameter src 'breakpoints))))

;; for sources, Resulting waveform array is put into :WAVEFORM-ARRAY slot of DESTINATION.
(defun add-waveform-to-element-control-waveform (element-reference added-waveform &key element-reference-timestep added-waveform-timestep)
  "Add successive values of the numerical sequence ADDED-WAVEFORM to the successive values of ELEMENT-REFERENCE, if ELEMENT-REFERENCE is a sequence, else
the sequence returned by applying ELEMENT-CONTROL-WAVEFORM to ELEMENT-REFERENCE. If the length of ADDED-WAVEFORM is less than the sequence associated with
ELEMENT-REFERENCE, then the subsequent added values are taken to be 0. The returned sequence is the same type \(array or list\) and length as that of the
sequence associated with ELEMENT-REFERENCE. If ADDED-WAVEFORM-TIMESTEP is supplied, and is inconsistent with the timestep of the sequence associated with
ELEMENT-REFERENCE, then the ADDED-WAVEFORM is resampled as appropriate. The latter timestep is either given explicitly by ELEMENT-REFERENCE-TIMESTEP or
determined by the function ELEMENT-CONTROL-WAVEFORM-TIMESTEP"
  (flet ((add-cars (list1 list2) (+ (car list1) (car list2))))
    (let* ((added-waveform-list (sequence-to-list added-waveform))
	   (element-waveform (typecase element-REFERENCE
			       (NONSTRING-SEQUENCE element-REFERENCE)
			       (t (ELEMENT-CONTROL-WAVEFORM element-REFERENCE))))
	   (element-waveform-listp (listp element-waveform))
	   (element-waveform-list (sequence-to-list element-waveform))
	   (ELEMENT-REFERENCE-TIMESTEP (or ELEMENT-REFERENCE-TIMESTEP (ELEMENT-CONTROL-WAVEFORM-TIMESTEP element-REFERENCE))))
      (when (and added-waveform-timestep ELEMENT-REFERENCE-TIMESTEP
		 (not (= added-waveform-timestep ELEMENT-REFERENCE-TIMESTEP)))
	(setq added-waveform-list (CONVERT-DATA-TIME-LISTS added-waveform-list added-waveform-timestep ELEMENT-REFERENCE-TIMESTEP)))
      (do* ((element-waveform-list element-waveform-list (cdr element-waveform-list))
	    (added-waveform-list added-waveform-list (cdr added-waveform-list))
	    (reverse-new-waveform-list (list (add-cars element-waveform-list added-waveform-list))
				       (push (add-cars element-waveform-list added-waveform-list) reverse-new-waveform-list)))
	   ((or (null (cdr element-waveform-list)) (null (cdr added-waveform-list)))
	    (let* ((new-waveform-list (reverse reverse-new-waveform-list))
		   (returned-waveform-list (if (null (cdr element-waveform-list))
					       new-waveform-list
					       (concatenate 'list new-waveform-list (cdr element-waveform-list)))))
	      (if element-waveform-listp returned-waveform-list (list-to-array returned-waveform-list))))))))

(defun add-waveform (destination &key waveform-spec (WAVEFORM-time-interval *default-waveform-step*) delay use-menu float-input breakpoints)
  "Add a waveform to DESTINATION, which can refer to either current or voltage sources, synapse or synapse types.  WAVEFORM-SPEC is either a sequence of
numbers or a function specification (lambda list) which returns a number sequence. If not included, or if WAVEFORM-SPEC is a function spec and USE-MENU is
T, the function WAVEFORM-MENU is called. WAVEFORM-TIME-INTERVAL [ms, default *DEFAULT-WAVEFORM-STEP*] is the time base for WAVEFORM-SPEC. DELAY, when not
NIL [default] is in milliseconds, and sets the destination :DELAY slot of current or voltage sources, or adds a delay to the actual waveform used in
synapse types.  DESTINATION may also be an electrode, in which case the actual destination is extracted with the function ELECTRODE-SOURCE. If FLOAT-INPUT
is T, then the waveform associated with WAVEFORM-SPEC is a single float numeric sequence. A list of explicit BREAKPOINTS may be included, which will
constrain the simulation to use these time points."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((waveform-spec (coerce-to-list waveform-spec))
	 (*automatic-run* (not use-menu))
	 (waveform (when waveform-spec-is-numeric-seq waveform-spec))
	 (WAVEFORM-time-interval (s-flt WAVEFORM-time-interval)))
    (loop for destination in (elements destination)
	  do (setq destination (cond 
				 ((electrode-p destination) (electrode-source destination))
				 ((source-p destination) destination)
				 (t (element-type destination))))
	  when (or (source-p destination) (synapse-type-p destination)) do
	  (add-waveform-core destination waveform waveform-spec WAVEFORM-time-interval delay use-menu float-input breakpoints))))

(defun add-waveform-core (destination waveform waveform-spec WAVEFORM-time-interval delay use-menu float-input breakpoints)
  (let ((waveform-spec-is-numeric-seq (numeric-sequence-p waveform-spec)))
    (unless waveform-spec-is-numeric-seq
      (multiple-value-bind (waveform-from-menu interval-from-menu delay-from-menu waveform-spec-from-menu)
	  (waveform-menu (or waveform-spec (element-parameter destination 'waveform-function))
			 (format nil "Waveform for ~A" (element-name destination)) use-menu)
	(cond-every
	 (interval-from-menu (setq WAVEFORM-time-interval interval-from-menu))
	 (delay-from-menu (setq delay delay-from-menu)))
	(setq waveform waveform-from-menu
	      waveform-spec waveform-spec-from-menu)))
    (element-parameter destination 'waveform-function (unless waveform-spec-is-numeric-seq waveform-spec))
    (when (and (synapse-type-p destination) (numberp delay))
      (setq waveform (add-delay-to-waveform waveform delay waveform-time-interval)))
    (let ((waveform-time-interval-inverse (/ 1.0 WAVEFORM-time-interval))
	  (waveform-array (s-flt-array waveform)))
      (typecase destination
	(synapse-type
	 (element-parameter destination 'waveform-time-interval waveform-time-interval)    
	 (element-parameter destination 'waveform waveform-array)
	 (element-parameter destination 'delta-waveform (if float-input (delta-float-wave-array waveform-array) (delta-wave-array waveform-array))))
	(t (element-slot destination :waveform-array waveform-array)
	   (element-slot destination :use-pulse-list nil)
	   (element-slot destination :waveform-time-interval-inverse waveform-time-interval-inverse)))
      (add-breakpoints destination)
      (when (numberp delay)
	(let ((delay (s-flt delay)))
	  (element-slot destination :delay delay)
	  ;; Need to adjust this for synapse types.
	  (add-breakpoints destination delay)))
      (when breakpoints (add-breakpoints destination breakpoints)))))

(defun add-breakpoints (element &optional breakpoints (clear-first t))
  ;; When no BREAKPOINTS, or if CLEAR-FIRST is T, then clear all breakpoints of ELEMENT. Then, add any BREAKPOINTS to the ELEMENT.
  (when (or (not breakpoints) clear-first) (element-parameter element 'breakpoints nil))
  (loop for bp in (S-FLT-LIST (coerce-to-list breakpoints)) do (push-element-parameter element 'breakpoints bp)))

(defun source-pwl-list (src) (element-slot src :pwl-list))

(proclaim '(inline extract-pwl-value))
(defun extract-pwl-value (time src &optional pwl-list return-single-float)
  ;; Derive source value from PWL list at TIME. Returns double-float value unless RETURN-SINGLE-FLOAT is supplied. The SRC pwl-list
  ;; times are dfs and values are single-floats.
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((time (d-flt time)) 
	 (pwl-list (or pwl-list (source-pwl-list src)))
	 (value
	  (if pwl-list
	      (do ((times (nth 0 pwl-list) (cdr times))
		   (values (nth 1 pwl-list) (cdr values)))
		  ((or (null (cadr times)) (<= (the df (car times)) time (the df (cadr times))))
		   (let ((prior-time (car times))
			 (prior-mag (car values)))
		     (declare (double-float prior-time) (single-float prior-mag))
		     (if (null (cadr times))
			 (coerce prior-mag 'double-float)
			    
			 (let ((later-mag (cadr values))
			       (later-time (cadr times)))
			   (declare (double-float later-time) (single-float later-mag))
			   (if (/= later-time 0.0)
			       (if (<= prior-time 0.0)
				   (coerce prior-mag 'double-float)
				   (+ prior-mag	; Interpolate between breakpoints.
				      (* (- time prior-time)
					 (/ (- later-mag prior-mag) (- later-time prior-time)))))
			       (coerce later-mag 'double-float)))))))
	      (d-flt (or (when (element-parameter src 'reference-magnitude) (d-flt (element-parameter src 'reference-magnitude)))
			 (typecase src (isource *isource-default-reference-magnitude*) (vsource *vsource-default-reference-magnitude*)))))))
    (declare (double-float value))
    (if return-single-float (s-flt value) (d-flt value))))
    
(proclaim '(inline extract-pwl-value-single))
(defun extract-pwl-value-single (time src &optional pwl-list) (extract-pwl-value time src pwl-list t))

(defun user-pwl-list (source &optional user-pwl-list)
 ;;
 ;; The optional USER-PWL-LIST is '((time-points)(data-points))), e.g.
 ;;
 ;;    (list *sim-reverse-plot-time-list* (element-data *soma*))
 ;;
 ;; If this arg is missing, tries to find a pwl-list in the SOURCE :PARAMETERS slot under the
 ;; reference 'PWL-LIST. 
 ;;
 ;; This function also sets the :USE-PULSE-LIST slot for SOURCE if a pwl-list is found.
  (let ((pwl-list (or user-pwl-list (element-parameter source 'user-pwl-list))))
    (when pwl-list
      (set-source-use-pulse-list source)
      (element-parameter source 'pwl-list pwl-list))))

(defun reset-user-pwl-list (source)
  ;; Erases the 'PWL-LIST :PARAMETERS reference for SOURCE. 
  (element-parameter source 'pwl-list nil))

(defun source-type-on-node (node)
  (loop for elt in (node-elements node)
	when (isource-p elt) do (return 'isource)
	when (vsource-p elt) do (return 'vsource)))

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


;;; SYS Source file: menus.lisp

(in-package "SURF-HIPPO")

(defun main-menu-text (&optional text)
  (concatenate-strings
   (when (and (stringp text) (> (length text) 0)) text)
   (when (and (stringp text) (> (length text) 0)) (format nil "~%"))
   (when *circuit-loaded* (format nil "Current circuit is ~a" *circuit*))
   (when (and *circuit-loaded* *simulation-finished*) (format nil "~%"))
   (when *simulation-finished* (format nil "Last run: ~a" *simulation-name*))
   (when *circuit-loaded* (format nil "~%"))
   (format nil "~%Hit \"OK\" only to return to top-level or quit")))

(defun global-variable-menu (&rest vars)
  "Menu for editing global variables taken from a list VARS, if supplied, otherwise all new user-defined global variables. In all cases, only those
variables which are bound to numbers, symbols, strings or lists that contain only these types are included in the menu."
  (choose-variable-values
   (loop for var in (flatten-no-nils-list (or vars (get-new-surf-variable-symbols t)))
	 when (and (boundp var)
		   (let ((symbol-value (symbol-value var)))
		     (or
		      (NUM-OR-SYMBOL-OR-STRING-LIST-P symbol-value)
		      (numberp symbol-value)
		      (stringp symbol-value)
		      (symbolp symbol-value)
		      (t-or-nil-p symbol-value))))
	 collect (list var (GET-MENU-ENTRY-TYPE-KEYWORD var)))
   :label (if vars "Global Variable Menu" "User-Defined Global Variables Menu")))

(defun main-menu ()
  ;; Sets up all the parameters for the current run. Returns with either a circuit loaded and ready to go, or NIL to *quit*. Note
  ;; that this function is called even with an automatic run since it does some of the circuit setup.
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy8 dummy9 dummy10 dummy7 dummy11 dummy12 dummy13 dummy14 dummy15 help-string)
    (loop while t do
	 (setq dummy1 nil dummy2 nil dummy3 nil dummy4 nil dummy5 nil dummy6 nil dummy7 nil
	       dummy8 (and *circuit-loaded* (no-input-p))
	       dummy12 nil dummy14 nil dummy15 nil)
	  (CHOOSE-VARIABLE-VALUES
	   `(,(when *circuit-loaded* `(dummy10 "Run simulation" :boolean))
	      (dummy1 ,(format nil "Overall parameters") :boolean)
	      (dummy6 ,(format nil "Load circuit or files") :boolean)
	     ,(when *circuit-loaded* `(dummy2 "Histology" :boolean))
	     (dummy3 "Edit circuit elements" :boolean)
	     ,(when (are-there-sources) '(*modify-stimulus* "Edit clamp stimulus" :boolean))
	     (dummy7 "Information management" :boolean)
	     (dummy5 ,(concatenate-strings "Edit plot parameters"
					   (when *archive-variable-list* (format nil ",~%plot loaded archive data"))
					   (when (or *HIDE-plot-WINDOWS* *overlay-plots*) (format nil "~%"))
					   (cond ((and *HIDE-plot-WINDOWS* *overlay-plots*) "(plots hidden, overlay set)")
						 (*HIDE-plot-WINDOWS* "(plots hidden)")
						 (*overlay-plots* "(plot overlay set)"))) :boolean)
	     ,(when (are-there-light-synapses) '(dummy14 "Edit light inputs" :boolean))
	     ,(when (get-new-surf-variable-symbols t) `(dummy15 "Edit user-defined globals" :boolean))
	     (dummy12 "Surf-Hippo help" :boolean))
	   :image (create-instance nil *main-menu-image*) :text (main-menu-text help-string) :label "Surf-Hippo Main Menu")
	  (setq help-string nil)
	  unless (or *automatic-run* *modify-stimulus* dummy1 dummy2 dummy3 dummy5 dummy6 dummy7 dummy10 dummy12 dummy14 dummy15)
	  do (return nil)
	  when dummy12 do
	  (setq help-string (format nil "~%** Read the User Manual in ~A. **~%** Type \"h\" over output windows for help. **~%" (concatenate-strings *surf-home* "doc/")))
	  (format t "~A~%~%" help-string)
	  when dummy15 do (global-variable-menu)
	  when dummy7 do (quick-info-menu)
	  when (and (not dummy12) (not *circuit-loaded*)) do (setq dummy10 nil)
	  ;; If we want to run the simulation (assuming the circuit is loaded) then don't run these menus.
	  unless dummy10 do
	  (setq dummy11 nil)
	 (cond-every (dummy1 (overall-parameter-menu))
		     (dummy6 (load-circuit-or-files-menu))
		     (dummy3 (Edit-circuit-elements-menu))
		     (dummy5 (plot-parameters-menu))
		     (dummy14 (menu-for-light-stimulus))
		     (dummy2 (drawing-menu)))
       ;; Another chance to edit sources, since sources can be added from histology windows.
	 #|
	  (when (and nil
		     (not *modify-stimulus*) ; dummy8
		     (no-nils (mapcar 'source-stimulus-p (sources))))
	    (choose-variable-values '((*modify-stimulus* "Edit clamp stimulus" :boolean)) :label "More ??...."))
	 |#
	  do
	  (when dummy10 (setq *modify-stimulus* nil))
	  ;; Set up the circuit sources.
	  (when *modify-stimulus*
	    (EDIT-SOURCE-STIMULI)
	    (setq *modify-stimulus* nil))
	  ;; Propagate parameters to the circuit elements. The optional T arg is to consider *RECHECK-CIRCUIT-ELEMENTS-PARAMETERS* .
	  (when (or dummy1 dummy3) (set-circuit-elements-parameters t))
	  when (or *automatic-run* (and *circuit-loaded* dummy10)) do (return t))))

(defun overall-parameter-menu ()
  ;; Set up the overall simulation parameters. Loads circuit.
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11
	       )
    (setq dummy2 nil			; (not *circuit-loaded*)
	  dummy6 nil
	  )
    (choose-variable-values
     `(;(dummy2 "Load circuit function or file" :boolean)
       ;(dummy8 "Load generic Lisp file" :boolean)
       (*user-stop-time* :float)
       (dummy10 "Colorize and sparse data menu" :boolean)
       (dummy1 "Numerical integration parameters" :boolean)
       (dummy3 ,(format nil "Node voltage and~%concentration integrator initializations") :boolean)
       (dummy6 "Global variables and misc parameters" :boolean)
       ;;       (dummy7 "Load archived data" :boolean)
       ,(when (vsources) '(dummy11 "Run linear corrected voltage clamp sequence" :boolean))
       )
     :label "Overall Parameters")
    (setq *SESSION-NAME* (REPLACE-SPACE-WITH-UNDERSCORE *SESSION-NAME*))
    (cond-every
     (dummy3 (initialization-menu))
     ;;     (dummy7 (data-file-browser))
     ;; (dummy8 (file-browser-loader '("lisp" "sparcf" "fasl" "elts" "dat") "Lisp File Browser"))
     (dummy1 (NUMERICAL-PARAMETERS-MENU))
     (dummy6 (globals-menu))
     ;; (dummy2 (load-circuit-menu))
     (dummy10 (colorize-and-sparse-data-menu))
     (dummy11 (linear-corrected-vclp :use-menu t)))))

(defun load-circuit-or-files-menu ()
  ;; Loads circuit.
  (let (dummy2 dummy8)
    (choose-variable-values
     `((dummy8 "Load generic Lisp file" :boolean)
       (dummy2 "Load circuit function or file" :boolean)
       (*user-stop-time* :float))
     :label "Load Circuit or Files")
    (setq *SESSION-NAME* (REPLACE-SPACE-WITH-UNDERSCORE *SESSION-NAME*))
    (cond-every
     (dummy8 (file-browser-loader '("lisp" "sparcf" "fasl" "elts" "dat") "Lisp File Browser"))
     (dummy2 (load-circuit-menu)))))



(defun simulation-note-string-menu ()
  (setq *simulation-annotation*
	(SIMPLE-TEXT-MENU ""
			  "Simulation Annotation"
			  (if (> (length *simulation-annotation*) 0) *simulation-annotation* (format nil "Note:~%~%"))
			  200 500
			  (if (> (length *simulation-name*) 0) (format nil "Edit note above for ~a" *simulation-name*) "Edit *SIMULATION-ANNOTATION*"))))

(defun histology-wins-with-current-cells () (loop for win in (WINDOWS-OF-MODE :histology) when (graphics-window-cells win) collect win))

(defun initialization-menu ()
  (let (dummy1 dummy2 dummy3 dummy4)
    (choose-variable-values
     `((dummy1 "Grab the current node voltages and store in *NODE-VOLTAGE-INITIALIZATIONS*" :boolean)
       (*use-node-voltage-initializations* :boolean)
       ,(when (conc-ints) `(dummy2 "Grab the current concentration integrator values and store in *CONC-INT-INITIALIZATIONS*" :boolean))
       ,(when (conc-ints) `(*use-conc-int-initializations* :boolean))
       (dummy3 "Set virtual holding potentials for nodes with v-dep particles" :boolean)
       (dummy4 ,(format nil "Clear any holding potentials (~d currently) for nodes" (ELEMENTS-WITH-HOLDING-POTENTIALS)) :boolean))
     :label "Setting Up Initializations")
    (when dummy4 (CLEAR-HOLDING-POTENTIALS))
    (when dummy3 (menu-for-holding-potentials))
    (when dummy1 (SET-*NODE-VOLTAGE-INITIALIZATIONS*))
    (when dummy2 (set-*conc-int-initializations*))))

(defun menu-for-holding-potentials ()
  (loop for node in (choose-list-values-from-keys
		     (loop for node in (nodes) when (node-has-v-dep-element node) collect (list (node-name node) node))
		     nil :label "Select nodes to set holding potential")
	do (let ((dummy1 (s-flt (or (element-holding-potential node) (element-resting-potential node)))))
	     (choose-variable-values
	      `((dummy1 ,(format nil "Holding potential [mV] for ~A" (node-name node)) :float))
	      :label "Setting Holding Potentials")
	     (element-holding-potential node dummy1))))

(defvar *store-plot-results-to-folder* nil)

(defun info-menu ()
  (choose-variable-values
   '((*store-plot-results-to-folder* "Stored in data folder" :boolean)
     (*print-numerical-details* "Include parameters of numerical method in print outs" :boolean)
     (*print-out-to-lisp* "Print simulation information to Lisp Window" :boolean)
     (*simulation-print-detail* "Print details for every simulation:"
      :choose (:none :terse :medium :full :FULL_With_SEGMENTS :specific_elements) :vertical :rank-margin 3)
     (*print-out-to-info-window* "Print simulation information to Information Window" :boolean)
     (*create-new-info-window* "Create new Information Window" :boolean)
     (*save-simulation-data-to-file* "Save simulation data to file" :boolean)
     (*save-simulation-info* "Save simulation information to file" :boolean)
     (*save-full-simulation-info* "Save all the simulation information to file" :boolean)
     (*make-circuit-subdir* "Make subdirectory for circuit's output files" :boolean)
     (*data-directory* "" :string))
   :label (if *last-simulation-file-path* (format nil "Simulation Info - last file was for ~S" *last-simulation-file-path*) "Simulation Info")))

(defun quick-info-menu ()
  (unless *simulation-description-destination* (setq *simulation-description-destination* :Info_Window))
  (let* (dummy1
	 dummy2 dummy3 dummy4 dummy5
	 dummy7 dummy8 dummy9 dummy11
	 dummy12 dummy13 (dummy14 *include-events-in-element-documentation-code*)
	 (menu-list `((*simulation-description-destination* "Simulation description destination:"
							    :choose (:File :Lisp_Window :Info_Window :Log_file) :rank-margin 5)
		      (dummy5 "Write description according to above destination now" :boolean)
		      (:comment "Writing Lisp Files of Current Data and Circuit")
		      (dummy3 "Write current plot data" :boolean)
		      (dummy1 "Dump loadable circuit element definitions:" :toggle-p t :choose (:prompt_for_elements :all_in_circuit) :LABEL-LEFT)
		      (dummy14 "Include events in element documentation code" :boolean)
		      ,(when *circuit-processed* `(dummy8 "Dump cell geometry and/or all circuit elements" :boolean))
		      (dummy11 "Dump current plot spec settings" :boolean)
		      (:comment "General Annotation Comment String")
		      (dummy12 "Edit annotation string for dumped files" :boolean)
		      (*include-simulation-annotation* ,(format nil "Include current simulation annotation string [\"~A\"]"
								(if (= 0 (length *simulation-annotation*)) "(Comment is empty)" *simulation-annotation*))
						       :boolean)
		      (:comment "Miscellaneous...")
		      (*dump-analysis-to-file* :boolean)
		      (*DOCUMENT-all-new-VARIABLES* :boolean)
		      ,(when (get-documentable-user-variables t) `(dummy13 "Dump documented variables to loadable .vars file now" :boolean))
		      (*create-new-info-window* "Create new Information Window" :boolean)
		      (dummy7 "Store data from last simulation into Data Folder" :boolean)
		      (dummy9 "Specify automatic information output" :boolean))))
    (choose-variable-values menu-list :label
			    (if *last-simulation-file-path*
			      (format nil "Simulation Information Output - Last File Was For ~S" *last-simulation-file-path*)
			      "Simulation Information Output"))
    (when dummy12 (simulation-note-string-menu))
    (when dummy11 (dump-all-plot-lists))
    (when dummy9 (info-menu))
    (when dummy8 (dump-tree-menu))
    (when dummy7 (traces-to-folder))
    (when dummy5 ; *simulation-description-destination*
      (let ((dummy1 *simulation-print-detail*)
	    (dummy6 *print-numerical-details*))
	(choose-variable-values
	 `((dummy1 ,(format nil "Text output detail for simulation description to ~A:" *simulation-description-destination*)
		   :choose (:none :terse :medium :full :FULL_With_SEGMENTS :specific_elements) :horizontal :rank-margin 3)
	   (dummy6 "Include simulation numerical parameters in this output" :boolean))
	 :label "Simulation Text Output Detail")
	(let ((*print-numerical-details* dummy6))
	  (when (and dummy3 *simulation-finished* (not (and *save-simulation-data-to-file* (eq :file *simulation-description-destination*))))
	    (write-element-data (when (unless *automatic-run* (go-ahead-menu "Prompt for each data list to save")) :menu)))
	  (case dummy1
	    (:specific_elements (information-for-elements-menu nil *simulation-description-destination*))
	    (t (case *simulation-description-destination*
		 (:file (dump-simulation-files nil t dummy1))
		 (:lisp_window (print-circuit dummy1))
		 (:info_window (print-circuit-to-info-window dummy1))
		 (:log_file (update-surf-log-file 'print-circuit (list dummy1)))))))))
    (when dummy13 (dump-DOCUMENTED-USER-VARIABLES-file))
    (let ((*include-events-in-element-documentation-code* dummy14))
      (case dummy1
	(:all_in_circuit (dump-all-circuit-elements-file))
	(:prompt_for_elements (dump-elements-file t))))))

(defun default-comment-font-menu ()
  (setq *plot-axis-font* (font-menu *plot-axis-font* "Choose default plot axis font (*PLOT-AXIS-FONT*)"))
  (when (go-ahead-menu "Change axis font of current plot windows") (loop for win in *output-windows* do (s-value win :plot-axis-font *plot-axis-font*))))

(defun enable-plot-variable-types-menu ()
  ;; Let the user change the flags if desired.
  (choose-variable-values
   (loop for plot-list-info in *plot-lists-info*
	 when (and (>= (length (plot-list-info-tables plot-list-info)) 1)
		   (plot-list-info-enable-var plot-list-info)
		   (loop for table in (plot-list-info-tables plot-list-info) never (HASH-TABLE-EMPTY table))
		   (case (plot-list-info-structure-slot plot-list-info)
		     (total-concentration (or (conc-int-types-shell-2-p) (conc-int-types-shell-3-p)))
		     (concentration-2 (conc-int-types-shell-2-p))
		     (concentration-3 (conc-int-types-shell-3-p))
		     (t t)))
	 collecting
	 (list (plot-list-info-enable-var plot-list-info)
	       (concatenate-strings "Plot "
				    (when (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)) "Path ")
				    (slot-descriptor-string plot-list-info)
				    "s")
	       :boolean))
   :label "Enable variables to plot"))

(defun plot-parameters-menu ()
  (let (dummy1 dummy2 dummy4 dummy6 dummy7 dummy8 dummy9 dummy10)
    (loop while t do
	  (setq dummy1 nil dummy2 nil dummy4 nil dummy6 nil dummy7 nil dummy8 nil dummy9 nil dummy10 nil)
	  (let ((menu-list `(,(when *circuit-loaded* `(dummy7 "Enable/disable plotted variable types" :boolean))
			     ;; (dummy9 "Clear some plot lists" :boolean)
			     ,(when *circuit-loaded* `(dummy1 "Edit plotted elements" :boolean))
			     ,(when *simulation-started* `(*CREATE-NEW-SIMULATION-PLOTS* "Create a new set of plot windows" :boolean))
			     (*OVERLAY-PLOTS* "Overlay data (unless creating new windows)" :boolean)
			     (*ACCOMODATE-OVERLAYS* "Adjust plots to accomodate all overlays" :boolean)
			     (*preserve-plot-layout* "Preserve layout of existing plots" :boolean)
			     ,(when *simulation-started* `(dummy2 "Replot all data now" :boolean))
			     ,(when (clean-up-*output-windows*) `(dummy10 ,(format nil "Lock the ~d current output window~:p" (length (clean-up-*output-windows*))) :boolean))
			     (dummy4 "More plot details" :boolean))))
	    (when *archive-variable-list* (push '(dummy6 "Plot loaded archive data" :boolean) menu-list))
	    (when *data-folder* (push '(dummy8 "Plot data from Data Folder" :boolean) menu-list))
	    (choose-variable-values menu-list :label "Setting Up Plot Parameters"))
	  (cond-every
	   ((and *overlay-simulations *create-new-plot-windows*) (setq *overlay-simulations nil))
	   (dummy7 (enable-plot-variable-types-menu))
	   (dummy6 (PLOT-ARCHIVE-VARS))
	   (dummy8 (plot-folder-traces))
	   (dummy9 (menu-to-clear-plot-lists))
	   (dummy4 (plot-details-menu))
	   (dummy1 (choose-plot-elements-menu))
	   ((and *simulation-finished* dummy2) (surf-plotter))
	   (dummy10 (lock-all-windows)))
	  when (not (or dummy6 (and *simulation-finished* dummy2) dummy4)) do (return t)))
  (choose-plot-data)
  nil)

(defun plot-details-menu ()
  (let (dummy1 dummy3)
    (choose-variable-values
     `((*save-simulation-data* "Enable saving of simulation data" :boolean)
       (*plot-standard-windows* "Enable plotting" :boolean)
       (*HIDE-plot-WINDOWS* "Hide plots, even when created" :boolean)
       (*hard-copy-screen* "Hardcopy screen after simulation" :boolean)
       ,(when (or *plot-synapse-events-p* *plot-axon-events-p*) '(*plot-events* "Plot rasters of all events" :boolean))
       ,(when (or *plot-synapse-events-p* *plot-axon-events-p*) '(*plot-event-generators* "Plot rasters of event generators" :boolean))
       (*plot-channels-by-major-ion* "Plot currents by major ion" :boolean)
       (*SAVE-CONDUCTANCES-normalized* "Save and plot conductances as percents of total" :boolean)
       (*PLOT-TOTAL-CONCS-SEPARATELY* "Plot total integrator concentrations separately" :boolean)
;;     (*PLOT-shell-CONCS-SEPARATELY* "Plot integrator shell concentrations separately" :boolean)
       (*save-data-step* :integer)
       (*MASSAGE-ELEMENT-PLOT-LABELS* "Massage simple name element plot labels" :boolean)
       (*plot-data-grouping* :choose (:cell :cell-type) :toggle-p)
       (*traces-per-plot* :integer)
       (*label-surf-plots* "Show trace labels on plots" :boolean)
;;     (dummy1 "Wipe out the list of plot windows" :boolean)
       (*plot-line-style-family* "Plot line style family (new windows only):" :choose ,*plot-line-style-families*)
       (*voltage-plot-waterfall* "Plot voltages in waterfall format" :boolean)
       (*voltage-plot-waterfall-x-offset* "X offset [ms] for voltage waterfalls" :float)
       (*voltage-plot-waterfall-y-offset* "Y offset [mV] for voltage waterfalls" :float)
       (*AUTO-PLOT-WATERFALL* ,(format nil "When waterfall for plots (e.g. voltage) enabled, X offset is 0 and Y offset is~%automatically adjusted (overrides above offsets).") :boolean)
       (dummy3 "Set plot axis and comment default fonts" :boolean))
     :label "Setting Up Some More Plot Parameters")
;    (when dummy1 (clear-plot-windows))
    (when dummy3 (plot-axis-and-comment-default-fonts-menu))
    (if *automatic-voltage-plot-scaling*
	(setq *voltage-plot-min* nil *voltage-plot-max* nil)
	(progn
	  (cond-every
	   ((not *voltage-plot-min*) (setq *voltage-plot-min* -90.0))
	   ((not *voltage-plot-max*) (setq *voltage-plot-max* -40.0)))
	  (dendrite-plot-parameters-menu)))))

(defun dendrite-plot-parameters-menu ()
  (choose-variable-values
    '((*voltage-plot-min* :number)
      (*voltage-plot-max* :number))
    :label "Voltage Plot Scaling"))

(defun soma-plot-parameters-menu ()
  (choose-variable-values
    '((*soma-voltage-plot-min* :number)
      (*soma-voltage-plot-max* :number))
    :label "Soma Voltage Plot Scaling"))

(defvar *last-edited-synapse-type* nil)
(defvar *last-edited-channel-type* nil)

(defun edit-circuit-elements-menu ()
  (unless (element *last-edited-channel-type*) (setq *last-edited-channel-type* nil))
  (unless (element *last-edited-synapse-type*) (setq *last-edited-synapse-type* nil))
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy7 (dummy8 t) dummy9 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17
	       (dummy18 :nothing) (dummy19 :nothing) dummy20 dummy21
	       last-edited-channel-sym last-edited-synapse-sym
	       (previous-*ignore-q10* *ignore-q10*)
	       (menu-list '((dummy15 "Edit names of circuit objects" :boolean))))
    (loop while dummy8 do
	 (setf last-edited-channel-sym (when *last-edited-channel-type* (if (stringp (element-name *last-edited-channel-type*))
									    (intern (element-name *last-edited-channel-type*))
									    (element-name *last-edited-channel-type*)))
	       last-edited-synapse-sym (when *last-edited-synapse-type* (if (stringp (element-name *last-edited-synapse-type*))
									    (intern (element-name *last-edited-synapse-type*))
									    (element-name *last-edited-synapse-type*)))
	       dummy1 nil dummy2 nil dummy3 nil dummy5 nil dummy7 nil dummy8 nil dummy9 nil
	       dummy11 nil dummy12 nil dummy13 nil dummy14 nil dummy15 nil dummy16 nil dummy17 nil
	       dummy18 :nothing dummy19 :nothing dummy20 nil dummy21 nil
	       menu-list '((:comment "Temperature Parameters")
			   (*Temp-celcius* "Temperature [deg C]" :number)
			   (*ignore-q10* :boolean)))
	 (when *circuit-processed*
	   (push '(dummy15 "Edit names of circuit objects" :boolean) menu-list)
	   (push '(dummy14 "Move cell(s) around" :boolean) menu-list))
	 (push '(dummy9 "Edit global ionic concentrations" :boolean) menu-list)
       ;; (push '(dummy22 "Revamp channel types w/ current library parameters" :boolean) menu-list)
	 (push `(dummy16 "Edit the following elements or types:"
			 :x-choose ,(no-nils (list (when *segment* :dendritic_trees)
						   (when *cell-type* :cell_types)
						   (when *soma* :somas)
						   (when (or *vsource* *isource*) :sources)
						   :synapses
						   (when *synapse* :synapses_in_circuit)
						   (when *last-edited-synapse-type* last-edited-synapse-sym)
						   :channels
						   (when *channel* :channels_in_circuit)
						   (when *last-edited-channel-type* last-edited-channel-sym)
						   (when *conc-int* :conc_ints)
						   (when *pump* :pumps)
						   (when *electrode* :electrodes)
						   (when *axon* :axon_types)))
			 :rank-margin 3)
	       menu-list)
	 (when *circuit-processed* (push '(dummy8 "Plot somatic steady-state IV characteristics" :boolean) menu-list))
	 (when *channel*
	   (push '(dummy20 "Edit blocking of channels by major ion type" :boolean) menu-list)
	   (push '(*enable-channels* :boolean) menu-list)
	   (push '(dummy7 "Edit blocking of channels by type" :boolean) menu-list)
	   (when (blocked-channel-types)
	     (push `(:comment ,(format nil "Blocked types: ~A" (concatenate-string-list (coerce-to-list (element-name (blocked-channel-types)))	:string-spacer " " :string-count-to-add-linefeed 4)))
		   menu-list))
	   (when (non-unity-channel-type-IV-MODULATION-p)
	     (push `(dummy18 ,(format nil "Manipulate channel type GBAR modulation~A:"
				      (when (channel-TYPES-WITH-NON-UNITY-IV-MODULATION)
					(format nil "~%[Currently modulated types ~A]" (element-names (channel-TYPES-WITH-NON-UNITY-IV-MODULATION)))))
			     :choose (:apply-modulation-to-gbar :reset-modulation :nothing) :vertical) menu-list))
	   (when (non-unity-synapse-type-IV-MODULATION-p)
	     (push `(dummy19 ,(format nil "Manipulate synapse type GBAR modulation~%~A:" (element-names (synapse-TYPES-WITH-NON-UNITY-IV-MODULATION)))
			     :choose (:apply-modulation-to-gbar :reset-modulation :nothing)) menu-list)))
	 (when *synapse*
	   (push '(dummy21 "Edit blocking of synapses by major ion type" :boolean) menu-list)
	   (push '(dummy17 "Edit blocking of synapses by type" :boolean) menu-list)
	   (push '(*enable-synapses* :boolean) menu-list)
	   (when (blocked-synapse-types)
	     (push `(:comment ,(format nil "Blocked types: ~A"
				       (concatenate-string-list (element-names (blocked-synapse-types)) :string-spacer " " :string-count-to-add-linefeed 4)))
		   menu-list)))
	 (choose-variable-values menu-list :label "Edit circuit elements")
	 (update-temperature (not (equal previous-*ignore-q10* *ignore-q10*)))
	 (when dummy20 (menu-for-channel-blocking-by-ion-type))
	 (when dummy7 (menu-for-channel-blocking))
	 (when dummy17 (menu-for-synapse-blocking))
	 (when dummy21 (menu-for-synapse-blocking-by-ion-type))
	 (case dummy18
	   (:apply-modulation-to-gbar (transfer-type-IV-MODULATION-to-references 'channel-type))
	   (:reset-modulation (RESET-NON-UNITY-CHANNEL-TYPE-IV-MODULATION)))
	 (case dummy19
	   (:apply-modulation-to-gbar (transfer-type-IV-MODULATION-to-references 'synapse-type))
	   (:reset-modulation (RESET-NON-UNITY-synapse-TYPE-IV-MODULATION)))
	 (cond-every
	  (dummy15 (EDIT-all-THINGs-NAME-MENU))
	  (dummy14 (menu-to-move-cells nil (opal-obj-exists *standard-graphics-output*)))
	  ((member :dendritic_trees dummy16) (menu-for-dendrites))
	  ((member :somas dummy16) (menu-for-somaS))
	  ((member :cell_types dummy16) (menu-for-cell-TYPES))
	  ((member last-edited-channel-sym dummy16) (menu-for-channel-types *last-edited-channel-type*))
	  ((member :channels_in_circuit dummy16)
	   (let ((types (get-current-channel-types-menu "Edit Channel Types In Circuit")))
	     (when types (menu-for-channel-types types))))
	  ((member :pumps dummy16) (menu-for-pumps))
	  ((member :channels dummy16) (menu-for-channel-parameters))
	  ((member :conc_ints dummy16) (menu-for-conc-ints))
	  ((member :axon_types dummy16) (MENU-FOR-AXON-TYPES) (edit-element (axons)))
	  ((member last-edited-synapse-sym dummy16) (menu-for-synapse-types *last-edited-synapse-type*))
	  ((member :synapses_in_circuit dummy16)
	   (let ((types (get-current-synapse-types-menu "Edit Synapse Types In Circuit")))
	     (when types (menu-for-synapse-types types))))
	  ((member :synapses dummy16) (menu-for-synapse-parameters))
	  ((member :electrodes dummy16) (edit-electrodes))
	  ((member :sources dummy16)  (sources-menu))
	  (dummy8 (MENU-FOR-UPDATE-AND-PLOT-IV))
	  (dummy9 (MENU-FOR-CONCENTRATIONS))))))

(defun MENU-FOR-CHANNEL-BLOCKING-BY-ION-TYPE ()
  (let (dummy1)
    (choose-variable-values
     '((dummy1 "Channel type ionic types:" :x-choose ("NA" "CA" "K" "CL")))
     :text "Blocking Channel Types" :label "Select ions to be blocked")
    (loop for ion in dummy1 do
	  (loop for type in (channel-types)
		when (and (instance-in-cell type)
			  (member (read-from-string ion) (channel-type-ion-permeabilities type) :key 'car))
		do (setf (channel-type-blocked type) t)))))

(defun MENU-FOR-SYNAPSE-BLOCKING-BY-ION-TYPE ()
  (let (dummy1)
    (choose-variable-values
     '((dummy1 "Synapse type ionic types:" :x-choose ("NA" "CA" "K" "CL")))
     :text "Blocking Synapse Types" :label "Select ions to be blocked")
    (loop for ion in dummy1 do
	  (loop for type in (synapse-types)
		when (and (instance-in-cell type)
			  (member (read-from-string ion) (synapse-type-ion-permeabilities type) :key 'car))
		do (setf (synapse-type-blocked type) t)))))

(defun MENU-FOR-CHANNEL-BLOCKING ()
  (let ((blocked-types (select-hash-values-menu	(CHANNEL-TYPE-HASH-TABLE) "Select Channel Types To Block"
						:selection-key 'channel-type-blocked :rank-margin 2 :do-all-at-once t :inclusion-key 'instance-in-cell)))
    (loop for type being the hash-value of (CHANNEL-TYPE-HASH-TABLE)
	  do (setf (channel-type-blocked type) (true-p (or (member (channel-type-name type) blocked-types)))))))

(defun MENU-FOR-SYNAPSE-BLOCKING ()
  (let ((blocked-types (select-hash-values-menu	(SYNAPSE-TYPE-HASH-TABLE) "Select Synapse Types To Block"
						:selection-key 'synapse-type-blocked :rank-margin 2 :do-all-at-once t :inclusion-key 'instance-in-cell)))
    (loop for type being the hash-value of (SYNAPSE-TYPE-HASH-TABLE)
	  do (setf (synapse-type-blocked type) (true-p (or (member (synapse-type-name type) blocked-types)))))))

(defun menu-for-concentrations ()
  (let ((dummy1 (if *fix-e-na* :fixed_value :nernst))
	(dummy2 (if *fix-e-k* :fixed_value :nernst))
	(dummy3 (if *fix-e-ca* :fixed_value :nernst))
	(dummy4 (if *fix-e-cl* :fixed_value :nernst)))
    (choose-variable-values
     `((dummy1 "Global E-Na dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-na* "Fixed Na+ Reversal Potential [mV]" :float)
       (*na-conc-extra* "[Na+]out [mM]" :float)
       (*na-conc-intra* "[Na+]in [mM]" :float)
       (dummy2 "Global E-K dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-k* "Fixed K+ Reversal Potential [mV]" :float)
       (*k-conc-extra* "[K+]out [mM]" :float)
       (*k-conc-intra* "[K+]in [mM]" :float)
       (dummy3 "Global E-Ca dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-ca* "Fixed Ca++ Reversal Potential [mV]" :float)
       (*ca-conc-extra* "[Ca++]out [mM]" :float)
       (*ca-conc-intra* "[Ca++]in [mM]" :float)
       (dummy4 "Global E-Cl dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-cl* "Fixed Cl- Reversal Potential [mV]" :float)
       (*cl-conc-extra* "[Cl-]out [mM]" :float)
       (*cl-conc-intra* "[Cl-]in [mM]" :float))
     :text (concatenate-strings
	    (format nil "Global values for concentrations and reversal potentials.~%")
	    (format nil "Note that individual elements may use their own reference."))
     :label (format nil "Global Ionic Concentrations"))
    (setq *fix-e-na* (eq dummy1 :fixed_value)
	  *fix-e-k* (eq dummy2 :fixed_value)
	  *fix-e-ca* (eq dummy3 :fixed_value)
	  *fix-e-cl* (eq dummy4 :fixed_value))
    (setq *recheck-circuit-elements-parameters* t)
    (update-temperature-dependent-parameters nil t)))

(defun names-from-*parameters* (*parameters*)
  (delete-duplicates (loop for type-list in *parameters* collect (string (car type-list))) :from-end t))

(defun menu-for-type-choose-names (filtered-library-types already-chosen-type-names exclude-type-names type-model-name label text)
  (choose-list-values
   (loop for element-name in filtered-library-types unless (member element-name exclude-type-names :test 'equal) collect element-name)
   already-chosen-type-names
   :do-all-at-once t :rank-margin 1 :direction :horizontal :PUNT-IF-ONLY-ONE-ENTRY nil
   :text text :label (or label (format nil "Choose ~A Types" type-model-name))))

(defun menu-for-type (type-model-name &key already-chosen-types label text exclude-types)
  ;; This will create the selected type, if not already created
  (when (or (type-symbol-parent-model  type-model-name)
	    (type-symbol-parent-model (element-type type-model-name)))
    (let* ((already-chosen-type-names (element-names already-chosen-types))
	   (exclude-type-names (element-names exclude-types))
	   (dummy1 :ALL) (dummy2 :ALL) (dummy3 :ALL))
      (choose-variable-values
       `((dummy1 "Ionic types:" :choose ("NA" "CA" "K" "CL" :ALL))
	 ,(when (or (eq type-model-name 'synapse-type) (eq type-model-name 'synapse))
		`(dummy3 "Synapse control types:" :choose (:LIGHT-EVENT :LIGHT :EVENT :VOLTAGE :CHANNEL :TONIC :ALL)))
	 (dummy2 "All types or only those currently in circuit:" :choose (:ALL :IN_CELLS)))
       :label (format nil "Choosing ~A Types" type-model-name))
      (let* ((filtered-library-types (library-catalog TYPE-MODEL-NAME
						      :ionic-type (if (stringp dummy1) (read-from-string dummy1) dummy1)
						      :only-in-circuit (eq dummy2 :IN_CELLS) :synapse-control dummy3))
	     (chosen-names (menu-for-type-choose-names filtered-library-types already-chosen-type-names exclude-type-names type-model-name label text)))
	(loop for name in chosen-names collect (CREATE-ELEMENT-TYPE name type-model-name))))))

(defun ion-permeabilities-ion-value (ion permeabilities)
  (loop for ion-perm in permeabilities when (eq (car ion-perm) ion) do (return (cadr ion-perm)) finally (return 0.0)))

(defun edit-ion-permeabilities (elt)
  (let* (ion-permeabilities-ok
	 (element (element-type elt))
	 (ion-permeabilities (element-slot element :ion-permeabilities))
	 (dummy1 (ion-permeabilities-ion-value 'na ion-permeabilities))
	 (dummy2 (ion-permeabilities-ion-value 'k ion-permeabilities))
	 (dummy3 (ion-permeabilities-ion-value 'ca ion-permeabilities))
	 (dummy4 (ion-permeabilities-ion-value 'cl ion-permeabilities))
	 dummy5)
    (flet ((perm-ranges-ok () (loop for perm in (list dummy1 dummy2 dummy3 dummy4) always (<= 0.0 perm 1.0))))
      (loop until ion-permeabilities-ok do
	    (choose-variable-values
	     '((dummy1 "Na+ Permeability" :float) (dummy2 "K+ Permeability" :float) (dummy3 "Ca++ Permeability" :float) (dummy4 "Cl- Permeability" :float)
	       (dummy5 "CANCEL (keep original values)" :boolean))
	     :text (if (perm-ranges-ok)
		     (format nil "Each permeability must be between 0 and 1, with the sum <= 1 (total sum now ~A)." (+ dummy1 dummy2 dummy3 dummy4))
		     (concatenate-strings
		      (format nil "Permeabilities must add up <= 1.0, and each must be between 0.0 and 1.0.~%")
		      (format nil "Permeability for ~A is out of range" (cond ((not (<= 0.0 dummy1 1.0)) "Na+")
									      ((not (<= 0.0 dummy2 1.0)) "K+")
									      ((not (<= 0.0 dummy3 1.0)) "Ca++")
									      ((not (<= 0.0 dummy4 1.0)) "Cl-")))))
	     :label (format nil "Edit Ion Permeabilities For ~A ~A" (type-of element) (element-name element)))
	    (setq ion-permeabilities-ok (or dummy5 (and (perm-ranges-ok) (<= (+ dummy1 dummy2 dummy3 dummy4) 1.0)))))
      (unless dummy5
	(element-slot element :ion-permeabilities (clean-up-list (list (when (> dummy1 0) (list 'na dummy1))
								       (when (> dummy2 0) (list 'k dummy2))
								       (when (> dummy3 0) (list 'ca dummy3))
								       (when (> dummy4 0) (list 'cl dummy4)))))))))

(defun update-and-plot-iv (channel-type)
  (let (*recheck-circuit-elements-parameters*)
    (set-circuit-elements-parameters)
    (plot-ivs (loop for cell in (cells) when (find-channel-type-in-cell cell channel-type) collect (cell-name cell)))))

(defun menu-for-update-and-plot-iv ()
  (let (*recheck-circuit-elements-parameters*)
    (let ((cell-names (select-hash-values-menu (CELL-HASH-TABLE) "Select Cells to Plot Somatic Steady-State IV Curves" :punt-if-only-one-entry t)))
      (when cell-names
	(set-circuit-elements-parameters)
	(plot-ivs cell-names)))))

(defun menu-for-update-and-plot-iv ()	; PLOT-IVS has its own menu.
  (let (*recheck-circuit-elements-parameters*)
    (set-circuit-elements-parameters)
    (plot-ivs)))

(defun find-channel-type-in-cell (cell channel-type)
  (loop for elt in (node-elements (soma-node (cell-soma cell))) thereis (and (channel-p elt) (equal (channel-type elt) channel-type))))

(defun menu-for-channel-type-particle-types (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (when channel-type
      (setf (channel-type-particle-types-and-powers channel-type)
	    (loop for prt-type-and-power in (channel-type-particle-types-and-powers channel-type)
		  collecting (multiple-value-bind (prt-type order)
				 (edit-particle-type (car prt-type-and-power) channel-type (cdr prt-type-and-power))
			       (cons prt-type order)))))))

(defun menu-for-channel-type-conc-particle-types (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (when channel-type
      (setf (channel-type-conc-particle-types-and-powers channel-type)
	    (loop for prt-type-and-power in (channel-type-conc-particle-types-and-powers channel-type)
		  collecting (multiple-value-bind (prt-type order)
				 (edit-conc-particle-type (car prt-type-and-power) channel-type (cdr prt-type-and-power))
			       (cons prt-type order)))))))

(defun pulse-number-from-menu (pulse-lists source-name) ; Returns integer for number of pulses.
  (get-integer (length pulse-lists) 10 0 "Number of pulses:" (format nil "# Pulses For ~a" source-name)))

(defun plot-segments-to-soma-menu (target-elt)
  (plot-segments-to-soma
   target-elt
   (get-integer 0 100 0 (format nil "From ~A (path total number: ~A)" target-elt (length (segments-to-soma target-elt))) "Segment to Soma Path Skip")
   (when *plot-path-nodes* (go-ahead-menu "Clear previous plotted path nodes"))))

(defun constant-current-menu (cell-elements)
  (loop for constant-current-element in
	(choose-list-values-from-keys
	 (loop for cell-element in cell-elements when cell-element collect (list (element-name cell-element) cell-element))
	 nil :label "Select Cell Elements" :text "Edit constant current injection")
	do (constant-current-element-menu constant-current-element)))

(defun constant-current-element-menu (target-elt)
  (let ((dummy1 (ELEMENT-CONSTANT-CURRENT target-elt))
	(dummy2 (or (ELEMENT-CONSTANT-CURRENT target-elt) 0.0)))
    (choose-variable-values
     '((dummy1 "Add/remove constant current source" :boolean)
       (dummy2 "Constant current value [nA]" :float))
     :label (format nil "Edit Constant Current @ ~A" (element-name target-elt)))
    (if dummy1 (add-constant-current-to-element target-elt dummy2) (CLEAR-ELEMENT-CONSTANT-CURRENT target-elt))))

(defun OVERALL-ELEMENT-MENU (target-elt &optional win) ; This is for somas or segments.
  (when (cell-element-p target-elt)
    (let* (dummy1
	   dummy2 (dummy5 :none) dummy6 dummy8 dummy9 dummy10 dummy11 dummy12
	   dummy13 dummy14 dummy15 dummy23
	   (target-elt (element target-elt))
	   (target-element-string
	    (string-downcase (string (type-of target-elt))))
	   (menu-list `((dummy1 "Edit plotting information" :boolean)
			,(when (segment-p target-elt) '(dummy12 "Plot segments on path to soma" :boolean))
			(dummy2 "Add/Remove:" :x-choose (:sources :channels :synapses :axons :electrodes) :rank-margin 3)
			(dummy23 "Mark specific branches, disable/enable branch marking"  :boolean)
			(dummy10 ,(format nil "Move cell ~A" (element-name (element-cell target-elt))) :boolean)
			(dummy8 ,(format nil "Unchoose this ~a" (string-downcase (string (type-of target-elt)))) :boolean)
			(dummy14 "Edit constant current at this node" :boolean)
			(dummy5 ,(format nil "Print parameters of ~a and its elements:" target-element-string)
				:choose (:none :File :Lisp_Window :Info_Window :Log_file) :rank-margin 3)
			(dummy13 "Edit specific elements associated with this node" :boolean))))
      (when nil				; (and (soma-or-segment target-elt) (ARE-THERE-LIGHT-SYNAPSES target-elt))
	;; (setq dummy6 (when (member (element-name target-elt) *SYNAPSE-NAMES-TO-DO-FIRST*) :remove :push))
	(when nil
	  (setq menu-list (nconc menu-list `((dummy6 "Push this node onto the front of the *SYNAPSE-NAMES-TO-DO-FIRST* list:"
						     :choose (:PUsh_onto_cleared_list :PUsh :remove) :vertical))))))
      (choose-variable-values
       menu-list
       :label (format nil "Modifying ~a ~A" (string-downcase (string (type-of target-elt))) (element-name target-elt)))
      (when dummy12 (plot-segments-to-soma-menu target-elt))
      (when dummy10 (MENU-TO-MOVE-CELLS (element-cell target-elt) win))
      (when dummy14 (constant-current-element-menu target-elt))
      ;; (when dummy6 (munge-elt-on-*SYNAPSE-NAMES-TO-DO-FIRST* target-elt dummy6))
      (loop for thing in dummy2 do (case thing
				     (:channels (menu-for-adding-channel-types target-elt))
				     (:synapses (MENU-FOR-ADDING-SYNAPSE-TYPES target-elt))
				     (:electrodes (menu-for-adding-electrodes target-elt))
				     (:sources (sources-for-target-element-menu target-elt))
				     (:axons (axons-for-target-element-menu target-elt))	))
      (cond-every
       ((not (eq :none dummy5)) (information-for-elements-menu target-elt dummy5))
       ((or dummy2 dummy9) (set-circuit-elements-parameters))
       (dummy1 (plot-elements-of-target-element-menu target-elt))
       (dummy13 (edit-elements-of-target-element target-elt))
       (win (cond-every
	     (dummy1 (mark-plotted-nodes win t))
	     (dummy2 (cond-every ((member :electrodes dummy2) (draw-electrodes win (g-value win :draw-electrodes)))
				 ((or (member :electrodes dummy2) (member :sources dummy2)) (draw-sources win t))))
	     (dummy8 (unchoose-chosen-ones win))
	     (dummy23 (mark-segment-chains win t t))))))))

(defun channels-for-target-element-menu (target-elt)
  (let* ((original-channels (channels target-elt))
	 (new-type-names
	  (choose-list-values
	   (loop for library-type-list in (model-parameter-type-library (type-symbol-model 'channel-type)) collect (string (car library-type-list)))
	   (loop for ch in original-channels collect (channel-type-name (channel-type ch)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical :label (format nil "Choose Channel Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for ch in original-channels
	  when (not (string-member (channel-type-name (channel-type ch)) new-type-names)) do (erase-element ch)
	  else do (setq new-type-names (string-remove (channel-type-name (channel-type ch)) new-type-names)))
    (create-channels target-elt new-type-names)))

(defun axons-for-target-element-menu (target-elt)
  (let* ((original-axons (node-axons target-elt))
	 (new-type-names
	  (choose-list-values
	   (loop for library-type-list in (model-parameter-type-library (type-symbol-model 'axon-type)) collect (string (car library-type-list)))
	   (loop for ch in original-axons collect (axon-type-name (axon-type ch)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical :label (format nil "Choose Axon Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for ch in original-axons
	  when (not (string-member (axon-type-name (axon-type ch)) new-type-names)) do (erase-element ch)
	  else do (setq new-type-names (string-remove (axon-type-name (axon-type ch)) new-type-names)))
    (create-axons target-elt new-type-names)))

(defun non-light-synapses-for-target-element-menu (target-elt)
  (let* ((original-synapses (loop for syn in (synapses target-elt) when (not (eq (synapse-type-control (synapse-type syn)) :light)) collect syn))
	 (new-type-names
	  (choose-list-values
	   (loop for library-type-list in (model-parameter-type-library (type-symbol-model 'synapse-type)) collect (string (car library-type-list)))
	   (loop for syn in original-synapses collect (synapse-type-name (synapse-type syn)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Non-Light Synapse Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for syn in original-synapses
	  when (not (string-member (synapse-type-name (synapse-type syn)) new-type-names)) do (erase-element syn)
	  else do (setq new-type-names (string-remove (synapse-type-name (synapse-type syn)) new-type-names)))
    (create-element target-elt new-type-names)))

(defun sources-for-target-element-menu (target-elt)
  (let ((dummy5 (node-isources target-elt))
	(dummy6 (node-vsources target-elt)))
    (choose-variable-values
     '((dummy5 "Include current source" :boolean)
       (dummy6 "Include voltage source" :boolean))
     :label (format nil "Modifying Sources For ~a ~A" (string-downcase (string (type-of target-elt))) (element-name target-elt)))
    (cond-every ((and (node-isources target-elt) (not dummy5)) (erase-elements (node-isources target-elt)))
		((and (node-vsources target-elt) (not dummy6)) (erase-elements (node-vsources target-elt)))
		((and (not (node-isources target-elt)) dummy5) (edit-source-stimulus (add-isource target-elt)))
		((and (not (node-vsources target-elt)) dummy6) (edit-source-stimulus (add-vsource target-elt))))))

(defun choose-associated-elements-types-for-info (&optional associated-elements)
  (choose-list-values
   (no-nils
    (loop for model in (models)
	  when (model-print-routine model)
	  collect (if associated-elements
		    (loop for elt in associated-elements when (eq (model-name model) (type-of elt)) return (model-name model))
		    (when (loop for thing in (list-of-all-things (model-name model)) thereis (element-in-circuit thing))
		      (model-name model)))))
   nil :do-all-at-once t :label "Element classes for information"))

(defun choose-associated-elements-types-for-documentation (&optional associated-elements)
  (choose-list-values
   (no-nils
    (loop for model in (models)
	  when (model-document-routine model)
	  collect (if associated-elements
		    (loop for elt in associated-elements when (eq (model-name model) (type-of elt)) return (model-name model))
		    (when (loop for thing in (list-of-all-things (model-name model)) thereis (element-in-circuit thing))
		      (model-name model)))))
   nil :do-all-at-once t :label "Element classes for documentation"))

(defun choose-associated-elements-types-for-edit (&optional associated-elements)
  (choose-list-values
   (no-nils
    (loop for model in (models)
	  when (model-edit-routine model)
	  collect (if associated-elements
		    (loop for elt in associated-elements when (eq (model-name model) (type-of elt)) return (model-name model))
		    (when (loop for thing in (list-of-all-things (model-name model)) thereis (element-in-circuit thing))
		      (model-name model)))))
   nil :do-all-at-once t :label "Element classes for editing"))

(defun information-for-elements-menu (&optional target-elt (destination :lisp_window))
  (let ((associated-elements (get-associated-elements target-elt t t t)))
    (loop for type-symbol in (choose-associated-elements-types-for-info associated-elements) do
	  (loop for elt in (choose-list-values-from-keys
			    (loop for elt in (if associated-elements
					       (no-nils (loop for elt in associated-elements when (eq type-symbol (type-of elt)) collect elt))
					       (list-of-all-things-in-circuit type-symbol))
				  collect (list (format nil "~A: ~A" (if (electrode-p elt) "ELECTRODE" (type-of elt)) (element-name elt))
						elt))
			    nil :label (format nil "Choose ~a Elements for Information" type-symbol))
		do (case destination
		     (:File (add-element-doc-to-info-file elt))
		     (:Log_file (update-surf-log-file 'print-element (list elt)))
		     (:Info_Window (string-to-info-win
				    (let ((*standard-output* (make-string-output-stream)))
				      (print-element elt)
				      (get-output-stream-string *standard-output*))))
		     (t (print-element elt)))))))

(defun get-associated-elements (target-elt &optional (include-target t) include-cell include-types)
  (let* ((target-elt (element target-elt))
	 (node-elements (concatenate 'list
				     (when include-cell (list (element-cell target-elt)))
				     (when include-target (list target-elt))
				     (when (typecase target-elt (soma t) (segment t))
				       (node-elements (element-physical-node target-elt)))))
	 (node-element-types (when include-types (loop for elt in node-elements collect (element-type elt)))))
    (no-nils (delete-duplicates (nconc node-element-types node-elements) :test #'equal))))

(defun edit-elements-of-target-element (target-elt)
  (let ((associated-elements (get-associated-elements target-elt t t t)))
    (loop for type-symbol in (choose-associated-elements-types-for-edit associated-elements) do
	  (loop for elt in
		(choose-list-values-from-keys
		 (loop for elt in (if associated-elements
				      (no-nils (loop for elt in associated-elements when (eq type-symbol (type-of elt)) collect elt))
				      (list-of-all-things-in-circuit type-symbol))
		       collect (list (format nil "~A: ~A" (if (electrode-p elt) "ELECTRODE" (type-of elt)) (element-name elt)) elt))
		 nil :label (format nil "Choose ~a Elements for Editing" type-symbol))
	do (edit-element elt)))))

(defun check-for-soma-with-tree-for-dendrite-current-plot (element plot-list-info)
  (or (not (equal 'dendrite-current (plot-list-info-structure-slot plot-list-info)))
      (let ((element (element element)))
	(and (soma-p element) (cell-segments (soma-cell element))))))

(defun check-particle-data-slot-for-markov (element plot-list-info)
  (typecase (element-type element)
    (particle-type
     (case (plot-list-info-structure-slot plot-list-info)
       (markov-state (eq (particle-type-class (element-type element)) :markov))
       (state t)))
    (t t)))

(defun plot-elements-of-target-element-menu (target-elt)
  ;; If there is a synapse here, allow plotting of the synapse origin, if it exists.
  (let (element-list pre-synaptic-flags post-synaptic-flags)
    (loop for elt in (get-associated-elements target-elt) do
	  (when (synapse-p elt)
	    (if (not (equal (synapse-cell-element elt) target-elt)) ; target-elt must be pre-synaptic to someone
	      (progn
		(push (synapse-cell-element elt) element-list)
		(push nil pre-synaptic-flags)
		(push t post-synaptic-flags))
	      (when (cell-element-p (synapse-pre-synaptic-element elt))
		(push (synapse-pre-synaptic-element elt) element-list)
		(push t pre-synaptic-flags)
		(push nil post-synaptic-flags))))
	  (push elt element-list)
	  (push nil post-synaptic-flags)
	  (push nil pre-synaptic-flags))
    (let* ((type-list (delete-duplicates (loop for elt in element-list collect (if (electrode-p elt) 'ELECTRODE (type-of elt))) :test #'equal))
	   info-list)
      (loop for type in (choose-list-values type-list nil :label "Edit plotting for these element types" :do-all-at-once t :rank-margin 5 :direction :vertical)
	    do
	    (setq info-list nil)
	    (loop for elt in element-list
		  for pre-synaptic-flag in pre-synaptic-flags
		  for post-synaptic-flag in post-synaptic-flags
		  when (equal (if (electrode-p elt) 'ELECTRODE (type-of elt)) type)
		  collect elt into filtered-element-list
		  and collect pre-synaptic-flag into filtered-pre-synaptic-flags and collect post-synaptic-flag into filtered-post-synaptic-flags
		  finally
		  (loop for element in filtered-element-list
			for pre-synaptic-flag in filtered-pre-synaptic-flags
			for post-synaptic-flag in filtered-post-synaptic-flags
			do
			(let ((element-table (get-model-hash-table element)))
			  (loop for matched-plot-list-info in
				(loop for plot-list-info in *plot-lists-info*
				      when (and
					    (check-for-soma-with-tree-for-dendrite-current-plot element plot-list-info)
					    (check-particle-data-slot-for-markov element plot-list-info)
					    (= (length (plot-list-info-tables plot-list-info)) 1)
					    (loop for table in (plot-list-info-tables plot-list-info)
						  thereis (and (eq element-table table)
							       (or t (not (eq element-table (SYNAPSE-HASH-TABLE)))
								   (not (eq (plot-list-info-structure-slot plot-list-info) 'Event))
								   (and (eq :event (synapse-type-control (synapse-type element)))
									(eq :voltage (synapse-type-control (synapse-type element))))))))
				      collect plot-list-info)
				do (push (list (format nil "~A: ~A~A"
						       (element-name element)
						       (slot-descriptor-string matched-plot-list-info :electrode-p (electrode-p element))
						       (cond
							(pre-synaptic-flag (format nil " (pre-synaptic to ~A)" (element-name target-elt)))
							(post-synaptic-flag (format nil " (post-synaptic to ~A)" (element-name target-elt)))
							(t "")))
					       (list (plot-list-info-names matched-plot-list-info) ; the *plot-??* global
						     element))
					 info-list)))))
	    (let ((already-chosen (loop for elt-info in info-list
					when (string-member (element-name (cadadr elt-info)) (symbol-value (caadr elt-info))) collect (cadr elt-info))))
	      (loop for elt-info in info-list
		    do (setf (symbol-value (caadr elt-info)) (string-remove (element-name (cadadr elt-info)) (symbol-value (caadr elt-info)))))
	      (loop for var-elt in (choose-list-values-from-keys info-list already-chosen
								 :rank-margin 1 :punt-if-only-one-entry nil :do-all-at-once t
								 :rank-margin 2 :direction :horizontal
								 :label (format nil "~A Choose Plotting Related to ~A" (element-name target-elt) type))
		    do (push (element-name (cadr var-elt)) (symbol-value (car var-elt))))
	      (when (cell-element-p target-elt)
		(REMOVE-ENTRY-FROM-*PLOT-NODE-ELEMENTS* (element-name target-elt))))
	    (choose-plot-data)))))

(defun numerical-parameters-menu ()
  (let ((dummy1 (cond (*use-time-list* :use_step_list)
		      (*use-fixed-step* :use_fixed_step)
		      (t :variable_time_step)))
	(dummy2 (if *eval-all-synapses-every-step* :Evaluate_synapses_@_every_step :Enable_synapse_evaluation_skip)))
    (loop do
	  (choose-variable-values
	   `(				; (dummy1 "Choose method to determine time step:" :choose (:variable_time_step :use_fixed_step :use_step_list))
	     (*INTEGRATION-TIME-REFERENCE* :choose (:VARIABLE :FIXED :LIST))
	     ("Parameters for Variable (LTE-based) Time Step" :comment)
	     (*absolute-voltage-error*	; "Absolute voltage error [mV] for LTE-based time step:"
	      :number)
	     (*CONSIDER-PARTICLE-ERROR*	; "Estimate particle state error for LTE-based time step"
	      :boolean)
	     (*absolute-particle-error*	; "Absolute particle error [state]:"
	      :number)
	     (*PUNT-LTE-WHEN-MIN-STEP-REACHED* ; "Ignore LTE estimate if below minimum step"
	       :boolean)
	     (*user-min-step* "Minimum time step [ms] for variable step:" :number)
	     (*user-max-step* "Maximum time step [ms] for variable step:" :number)
	     (*pick-time-step-fudge* "Fudge factor for LTE-based time step (> 0 and <= 1):" :number)
	     (*INCLUDE-VSOURCE-NODES-IN-NODE-ERROR-EST*  :boolean)
	     (*enable-user-breakpoint-list* :boolean)
	     ("Parameters for Fixed Time Step" :comment)
	     (*user-step*		; "Fixed time step [ms]:"
	      :number)
	     ("Parameters for Using Time Step List" :comment)
	     (*auto-refresh-last-sim-reverse-time-list* "Update time step reference list using last simulation" :boolean)
	     ;; ("Synapse evaluation parameters" :comment)
	     ;; (*use-constant-element-matrix-contribution* "Enable element evaluation step skipping" :boolean)
	     ;; (dummy2 "Synapses evaluation criteria:"
	     ;; :choose (:Evaluate_synapses_@_every_step :Enable_synapse_evaluation_skip))
	     ;; (*eval-all-synapses-every-step* "Evaluate the synapses at every iteration/time step" :boolean)
	     ;; (*synapse-evaluation-step* "When skipping enabled, step for synapse evaluations [ms]" :float)
	     ("Concentration Integrator Params (Integration method set from conc-int menus)" :comment)

	     (*CONSIDER-conc-int-ERROR* ,(concatenate 'string
						      (format nil "Estimate concentration integrator error for LTE-based time step~%")
						      (format nil "(only when integrators evaluated in inner loop)"))
	      :boolean)
	     (*absolute-conc-int-error* :number)
	     (*eval-conc-ints-in-inner-loop* :boolean)
	     ("General Parameters" :comment)
	     (*save-data-step* :integer)
	     (*print-numerical-details* :boolean))
	   :text (unless (<= *pick-time-step-fudge* 1.0) (format nil "Time step fudge factor must be <= 1.0!"))
	   :label "Numerical Integration Parameters")
	  until (and (< 0 *pick-time-step-fudge*) (<= *pick-time-step-fudge* 1.0)))
    (case dummy2 (:Evaluate_synapses_@_every_step
		  (setq *eval-all-synapses-every-step* t)))))

(defun windows-parameters-menu ()
  (choose-variable-values
   '((*num-graphics-windows-rows* "Number of graphics windows rows" :integer)
     (*num-graphics-windows-columns* "Number of graphics windows columns" :integer))
   :label "Edit Graphics Windows Parameters")
  (setup-plot-tiling))

(defun debugging-parameters-menu ()
  (choose-variable-values
   `((*use-max-iterations* :boolean)
     (*max-iterations* :integer)
     ;;
     (*break-on-every-step* :boolean)
     (*debug-voltage-error-step-change* :boolean)
     (*debug-particle-error* :boolean)
     (*debug-particle-error-step-change* :boolean)
     (*debug-conc-int-error* :boolean)
     (*debug-conc-int-error-step-change* :boolean)
     ;;
     (*count-error-step-change* :boolean)
     (*debug-time-trace*  :boolean)
     (*debug-backup*  :boolean)
     (*debug-lte :boolean)
     (*debug-at-time-steps*  :boolean)
     (*debug-all-iterations*  :boolean)
     (*debug-node-name*  :string)
     (*debug-hines* :boolean)
     (*print-matrix*  :boolean)
     (*DEBUG-SEGMENTS* :boolean)
     (*DEBUG-NUMERICAL* :boolean)
     ;; (*announce-consing-segments* :boolean)
     (*debug-use-time-list* :boolean)
     ;; (*debug-diag* :boolean) (*debug-dc* :boolean) (*debug-consing :boolean) (*debug-conc-ints :boolean) (*debug-var-e-ca :boolean)
     ;; (*debug-eval-channel-1 :boolean) (*debug-eval-channel-2 :boolean) (*debug-eval-channel-3 :boolean)
     ;; (*debug-hines-step :boolean) (*debug-save-data :boolean) (*debug-set-sources :boolean)
     ;; (*debug-init-all-nodes :boolean) (*debug-eval-all-elements :boolean) (*debug-eval-all-nodes :boolean) (*debug-eval-elements :boolean)
     ;; (*DEBUG-EVAL-SEGMENT :boolean) (*DEBUG-EVAL-channel :boolean) (*DEBUG-EVAL-particle :boolean) (*DEBUG-EVAL-soma :boolean)
     )
   :label "Debugging Parameters"))


(defun globals-menu ()
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7)
    (choose-variable-values
     '((dummy1 "Windows Parameters" :boolean)
       (dummy3 "GC Parameters" :boolean)
       (dummy4 "Miscellaneous Parameters" :boolean)
       (dummy5 "Debugging Parameters" :boolean)
					; (dummy6 "Menu Fonts" :boolean)
       )
     :label "Edit Simulator Global Variables and Miscellaneous Parameters")
    (cond-every
					; (dummy6 (MENU-FONT-MENU))
     (dummy1 (windows-parameters-menu))
     (dummy2 (numerical-parameters-menu))
     (dummy3 (choose-variable-values
	      '((*beep-after-gc* :boolean)
		(*GC-ANNOUNCE-text* :boolean)
		(*BEEP-AFTER-SURF* :boolean)
		(*use-gc-announce-window* :boolean)
		;; (*print-out-gc* :boolean)
		(*log-gc-to-file* :boolean))
	      :label "GC Parameters"))
     (dummy5 (debugging-parameters-menu))
     (dummy4 (miscellaneous-parameters-menu)))))




(defun miscellaneous-parameters-menu ()
  (let ( ;; (dummy1 *particle-look-up-table-precision*)
	)
    (choose-variable-values
     `((*printer* "Specify printer" :string)
       ;; (*print-simulator-time* "Print run and user time stats after simulation" :boolean)
       (*show-time-remaining* "Show time remaining during simulation" :boolean)
       (*time-window-update-time* "Time (actual) in seconds between time window updates." :integer)
       (*session-name* "Name this session (spaces will map to _)" :string)
       (*circuit* "Circuit name" :string)
       (*SIMULATION-PLOT-WINDOW-COMMENT* "Comment for plot windows" :string)
       (*username* "User name" :string)
       (*always-add-host-name-to-windows* :boolean)
       (*displayed-host-name* :string)
       (*kill-extra-messages* "Kill extra messages" :boolean)
       (*KILL-ALL-OUTPUT* "Suppress all screen output" :boolean)
       (*always-clear-models* :boolean) (*always-clear-types* :boolean)
       (*use-node-voltage-initializations* :boolean)
       (*FIND-STEADY-STATE* :boolean)
       (*interpolate-particle-arrays* :boolean)
       ;; (*particle-look-up-table-precision* :number)
       (*estimate-particle-error-with-full-time-step* "Estimate particle LTE over 1/2 back step and full forward step"  :boolean)
       (*print-linear-analysis* :boolean)
       (*print-nonlinear-analysis* :boolean)
       (*integral-base* "Reference voltage for integrating plot traces" :number)
       (*average-integrals* "Average integrals of analyzed nodes" :boolean)
       (*print-axon-spike-times* :boolean)
       (*print-synapse-event-times* :boolean)
       (*print-synapse-total-events* :boolean)
       (*raise-output-windows* :boolean)
       (*deiconify-output-windows* :boolean)
       (*only-load-passive* :boolean)
       ;; (*show-time-divisor* "Number of timer window updates per simulation runh" :integer)
       ;; (*motif-gui-p* "Motif look-and-feel" :boolean)
       )
     :label "Miscellaneous Parameters")
    ;; Redo particle arrays since the precision has changed.
    ;; (unless (= dummy1 *particle-look-up-table-precision*) (make-needed-v-particle-arrays t))
    ))

(defun circuit-file-browser ()
  (or *automatic-run*
      (let ((path (file-browser "Circuit File Browser"
				(or *circuit-directory* *surf-home*)
				'("asc" "fix"
				  "lisp" "sparcf" "fasl" "circuit")
					;                                (case *circuit-file-type*
					;                                  (:neurolucida '("asc" "fix"))
					;                                  ((:ntscable :surf-functions :lisp) '("lisp" "sparcf" "fasl" "circuit")))
				)))
	(if path
	  (let ((file (file-namestring path)))
	    (setq *circuit-file* file
		  *circuit-filename* (namestring path)
		  *filename* (namestring path)
		  *circuit-directory* (directory-namestring path)
		  *circuit* (pathname-name file))
	    (setq *simulation-name* *circuit*
		  *circuit-parts* (list (pathname-name file))))
	  :cancel))))

(defun data-file-browser ()
  (let ((path (file-browser "Data File Browser" (or *data-directory* *surf-home*) '("dat" "elts"))))
    (when path
      (setq *data-directory* (directory-namestring path))
      (load path))))

(defvar *browser-directory* nil)

(defun file-browser-loader (extensions-list label)
  (let ((path (file-browser label (or *lisp-file-directory* *surf-home*) extensions-list)))
    (when path
      (setq *lisp-file-directory* (directory-namestring path))
      (load path))))

(defun doc-file-browser ()
  (let ((path (file-browser "Documentation File Browser"
			    (if (> (length *doc-file*) 0) (pathname *doc-file*) (concatenate-strings *surf-home* "doc/"))
			    '("doc" "tex" "info"))))
    (when path (setq *doc-file* (namestring path)))))

(defun EDIT-all-THINGs-NAME-MENU ()
  (loop for type in
	(choose-list-values (loop for model in (models)
				  when (and (not (eq (model-name model) 'node)) (> (hash-table-count (model-hash-table model)) 0))
				  collect (model-name model))
			    nil :do-all-at-once t :rank-margin 5 :direction :vertical :label (format nil "Select types of objects to change names"))
	do (edit-thing-name-menu type)))

(defun edit-thing-name-menu (type)
  (loop for thing in (element
		      (choose-list-values (element-name (LIST-OF-ALL-THINGS type)) nil
					  ; :do-all-at-once t
					  :max-per-menu 30
					  :rank-margin 5 :direction :vertical :label (format nil "Select ~a names to change" type))
		      type)
	do (let ((dummy1 (element-name thing type)))
	     (choose-variable-values `((dummy1 ,(format nil "Edit ~a name:" type) :string)))
	     (SET-ELEMENT-NAME thing dummy1))))

(defun choose-specific-elements (elements &optional selected-elements &key
					  (label "Choose Some Items") text
					  (punt-if-only-one-entry t) only-one-choice do-all-at-once
					  (max-per-menu 10) rank-margin (direction :vertical) (max-height 440))
  (let ((list-of-key-values (loop for elt in elements collect (list (massage-element-plot-label elt) (element elt))))
	(selected-values (elements selected-elements)))
    (choose-list-values-from-keys list-of-key-values selected-values
				  :label label :text text
				  :punt-if-only-one-entry punt-if-only-one-entry :only-one-choice only-one-choice :do-all-at-once do-all-at-once
				  :max-per-menu max-per-menu :rank-margin rank-margin :direction direction :max-height max-height)))

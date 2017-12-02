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


;;; SYS Source file: declare.lisp

;; ****************************************
;;; Various declarations of types, constants, and global variables.
;;;
;;; Other files may define globals as well.
;; ****************************************


(in-package "SURF-HIPPO")

;; ****************************************
;;
;; Types
;;
;; ****************************************

(deftype nonstring-sequence () '(and (or array cons) (not string)))

(deftype vec-bool () '(vector boolean))

(deftype vec-fix () '(vector fixnum))

(deftype element-fixnums () '(vector fixnum))

(deftype vec-flt () '(SIMPLE-ARRAY SINGLE-FLOAT (*)))

(deftype 1d-flt () '(SIMPLE-ARRAY SINGLE-FLOAT (*)))

(deftype 1d-df () '(SIMPLE-ARRAY DOUBLE-FLOAT (*))
	 ; (vector double-float)
	 )

(deftype vec-df () '(SIMPLE-ARRAY DOUBLE-FLOAT (*))
	 ; (vector double-float)
	 )

(deftype element-floats () '(SIMPLE-ARRAY SINGLE-FLOAT (*)))

(deftype element-double-floats () '(SIMPLE-ARRAY DOUBLE-FLOAT (*))
	 ; (vector double-float)
	 )

(deftype 2dfloat () '(simple-array single-float (* *)))

(deftype 2ddfloat () '(simple-array double-float (* *)))

(deftype matrix-float () '(SIMPLE-ARRAY DOUBLE-FLOAT (*))
	 ; (VECTOR double-FLOAT)
	 )

(deftype markov-particle-transition-array () '(simple-array (or null double-float function vec-df) (* *)))

(deftype ub32 () '(unsigned-byte 32))


;; ****************************************
;;
;; The (not strict) naming convention for the variables is -
;;  
;;   VARIABLE-NAME   = local variable or global constants
;;   *VARIABLE-NAME  = global variable
;;   *VARIABLE-NAME* = global variable
;;
;; However, there are still some globals without asterisks.
;;
;; ****************************************

;; At initialization, this is a list of all the variables defined in the SURF package.
;; This list is used to determine if a given symbol has been defined (e.g. by the user)
;; after initialization.

(defvar *original-surf-variable-symbols* nil)

;; ****************************************
;;
;; Units strings - for assignment to the plist of function names.
;;
;; ****************************************

(defvar microns-string "microns")
(defvar centimeters-string "cm")
(defvar distance-units-string microns-string)

(defvar square-microns-string "um2")
(defvar square-centimeters-string "cm2")
(defvar surface-area-units-string square-microns-string)

(defvar degrees-celcius-string "degC")
(defvar degrees-farenheit-string "degF")
(defvar degrees-kelvin-string "degK")
(defvar temperature-units-string degrees-celcius-string)

(defvar milliseconds-string "ms")
(defvar time-units-string milliseconds-string)
(defvar per-milliseconds-string "1/ms")
(defvar rate-units-string per-milliseconds-string)

(defvar millivolts-string "mV")
(defvar voltage-units-string millivolts-string)
(defvar nanoamperes-string "nA")
(defvar current-units-string nanoamperes-string)

(defvar nanofarads-string "nF")
(defvar capacitance-units-string nanofarads-string)
(defvar microfarad-per-square-centimeter-string "uF/cm2")
(defvar specific-capacitance-units-string microfarad-per-square-centimeter-string)

(defvar megaohms-string "Mohms")
(defvar resistance-units-string megaohms-string)
(defvar ohms-centimeters-string "ohms-cm")
(defvar resistivity-units-string ohms-centimeters-string)

(defvar microsiemans-string "uS")
(defvar conductance-units-string microsiemans-string)
(defvar picosiemans-per-square-centimeter-string "pS/cm2")
(defvar conductance-density-units-string picosiemans-per-square-centimeter-string)

(defvar squar-centimeters-per-second-string "cm2/s")
(defvar cubic-centimeters-per-second-string "cm3/s")
(defvar permeability-units-string cubic-centimeters-per-second-string)
(defvar millimolar-string "mM")
(defvar concentration-units-string millimolar-string)

;; ****************************************
;;
;;  Temperature and Temperature Dependent Parameters
;;
;; ****************************************

(defvar *Temperature* 300.0) ;; Simulation temperature in degrees Kelvin. In general not be set by the user - set *TEMP-CELCIUS* instead. 
(RETURN-VALUE-UNITS '*Temperature* degrees-kelvin-string) 

(defvar *Temp-celcius* 27.0 "Temperature of the simulation in degrees Celcius. This is the global variable that should be changed for setting the temperature.")
(RETURN-VALUE-UNITS '*Temp-celcius* TEMPERATURE-UNITS-STRING)

(defvar *last-simulation-temperature* nil) ; just to make changing the temp easier
(defvar *LAST-SIMULATION-TEMP-celcius* 27.0)

(defvar *update-temperature-dependent-parameters* nil)

(defvar *ignore-q10* nil "When T all assigned q10 values (e.g. rate constants) are taken to be 1.")  ; When this flag is true then the only temperature
					; dependence in the (:HH-EXT class) particle kinetics (rate constant calculation) occurs via the 1/T factor in the
					; exponential argument.  

;; ****************************************
;;
;;  General Simulator Variables
;;
;; ****************************************

#+mp (defvar *enable-process-yield* t)

(defvar  *USERNAME* "")
;; (defvar *comment-string* "")		; Comment with formatting characters
(defvar *circuit* "" "The name of the circuit function or file (string) (w/o file extension).")
(defvar *circuit-function* "" "The name of the just loaded circuit function.")

(defvar *initialize-on-circuit-load* t "Initialize and clear any loaded circuit when loading a new circuit. If NIL, then new circuit will be added to existing one.")
(defvar *initialize-before-next-circuit* t) ; For backward compatibility.

(defvar *multiple-source-circuit* nil "When more than one file or function defined the current circuit.")

(defvar *circuit-loaded* nil "As soon as any circuit is loaded, this is T.")
(defvar *circuit-drawn* nil "As soon as any circuit is drawn, this is T.")
(defvar *circuit-processed* nil) ; To avoid nested calls to PROCESS-CIRCUIT-STRUCTURE.
(defvar *simulation-name* "" "Automatically generated name for the current simulation.")
(defvar *auto-update-sim-name* t)

(defvar *simulation-initialized* nil "T when circuit has been initialized for simulation.")
(defvar *simulation-in-progress* nil "T while a simulation is running.")
(defvar *simulation-started* nil "NIL until the first simulation of the loaded circuit has started.")
(defvar *simulation-finished* nil "T as soon as a new circuit simulation is finished.")

(defvar *simulation-annotation* "")
(defvar *include-simulation-annotation* nil)

(defvar *session-name* "" "A session name can be used to delinate a series of experiments.")
(defvar *surf-interactive* t "True if the program is being run interactively.")
(defvar *load-only* nil)
(defvar *write-log-file* nil "Keep a running log file of all output to the Lisp window.")
(defvar *kill-extra-messages* nil "For text-wise silent simulations.")
(defvar *suppress-element-creation-messages* nil "Suppresses various non-fatal messages generated during element creation.")

(defvar *kill-all-output* nil "For suppressing all output during a simulation.")
(defvar *beep-after-surf* t "Beep after every simulation.")
(defvar *print-simulator-time* nil)

(defvar *scrub-and-gc-on-global-initialization* nil
  "When simulator is initialized for a new circuit definition, run the function SCRUB-AND-GC.  ** NOT VERIFIED**")

(defvar *log-gc-to-file* nil "GC messages will be written to a text file.")

(defvar *use-gc-announce-window* nil)	; problems with save-image version.

(defvar *gc-announce-text* nil)

(defvar *beep-after-gc* t "GC will beep when done.
Useful signal for long simulations to verify machine is breathing.")

(defvar *show-time-remaining* t "Display a timer window every *TIME-WINDOW-UPDATE-TIME* seconds in real time.")

(defvar *last-show-time-remaining-time* 0)

(defvar *time-window-update-time* 10 "Real time in seconds between time window updates [integer>0].")

; (defvar *show-time-divisor* 50 "Number of timer window updates per simulation run.")

(defvar *simulation-actual-time* 0)	; Keeps track of the actual time as given by GET-UNIVERSAL-TIME.

;; ****************************************
;;
;; Related to Circuit Loading and Definition
;;
;; ****************************************

(defvar *always-clear-models* nil "Wipe out the previous list of models whenever a circuit is read in.")

 
(defvar *always-clear-types* nil "Wipe out the previous element type hash tables - synapse, channel, particle, etc. - whenever a circuit is read in.")   

(defvar *input-is-function* t "Whether the circuit description is a compiled function, or the name of file. Normally set automatically.")


(defvar *process-ntscable-list* nil) ;; Set by files generated by ntscable

(defvar *default-cell-type-name* nil "If there is no specified cell type for a newly created cell, for example when loading an anatomy file-based cell,
reference this type.")  

(defvar *cell-name-suffix* nil "When non-NIL, is automatically added as a suffix to the name of a cell created by CREATE-CELL.")
(defvar *next-cell-name* nil "If non-NIL, then this is used for the name of the next created cell, overriding any other specfication. This is cleared after
CREATE-CELL is called.")

(defvar *add-cell-name-to-segs* nil "Add cell name to segment name, unless *USE-SIMPLE-NAMES* is T.")
(defvar *add-cell-name-to-segs-for-tree-dump* t)

(defvar *circuit-file-type* :lisp "Pertaining to the circuit definition file. Values include :NEUROLUCIDA and :LISP.") 

(defvar *circuit-catalog-functions* nil "List of circuit functions that can be selected from the input menus.")
(defvar *circuit-functions* nil) ; backward compatibility - now is called *CIRCUIT-CATALOG-FUNCTIONS*

(defvar *circuit-parts* nil "If loaded circuit is composed of more than on function and/or file, then this list includes the names of the components.")  

(defvar *loaded-circuit-parts* nil "These are the circuits that have actually been loaded.") 

(defvar *circuit-source* :Catalog_Function "A symbol which says how the circuit was loaded into the system. 
Possible values include :CATALOG_FUNCTION, :FUNCTION, :FILE.")  

(defvar *use-simple-names* nil "When true, cells, membrane element and segment names are just integers.") 
(defvar *cell-simple-name-counter* 0)
(defvar *cell-element-simple-name-counter* 0)
(defvar *synapse-simple-name-counter* 0)
(defvar *channel-simple-name-counter* 0)
(defvar *particle-simple-name-counter* 0)
(defvar *conc-particle-simple-name-counter* 0)
(defvar *axon-simple-name-counter* 0)
(defvar *pump-simple-name-counter* 0)
(defvar *buffer-simple-name-counter* 0)
(defvar *isource-simple-name-counter* 0)
(defvar *vsource-simple-name-counter* 0)

(defvar *prompt-for-alternate-element-names* t "Prompt for alternate element names of duplicate elements.")
(defvar *enable-automatic-cell-names* t)
(defvar *allow-duplicate-synaptic-connections* t "Allow duplicate synaptic connections.")

(defvar *allow-duplicate-elements* t "Unless T, only one synapse, channel or other membrane element of a given type can be added to the same cell element.")

;; Tree consolidation
(defvar *maximum-electrotonic-length* 0.25)
(defvar *neuron-tree-consolidated* nil)

(defvar *announce-consing-segments* nil)

;; Steady state determination.
(defvar *advance-sources* t)
(defvar *find-steady-state* nil)
(defvar *minimal-capacitance* 1.0e-6 "Temporary capacitance value used for cell elements during low capacitance steady-state determination [nF].") 
;(defvar *minimal-capacitance* 1.0e-12) ; nF
(defvar *plot-steady-state* nil)
(defvar *pseudo-steady-state-time* 3.0)
(defvar *low-cap-pseudo-steady-state-time* 0.1)
(defvar *run-reg-cap-for-ss* nil)

;; *** Miscellaneous ***

(defvar *electrode* "The last created segment, if any, which has been designated as an electrode.")

(defvar *user-save-data-functions* '() "A list of user specified functions which are called (without arguments) after plot data is saved.")

(defvar *user-output-data-functions* '() "A list of user specified functions,  which are called (without arguments) by SIM-OUTPUT.")

(defvar *cool-hippo-window* nil)
(defvar *cool-hippo-window-p* nil)

(defvar *main-menu-image*
  (create-instance nil opal:bitmap
		   (:filling-style (create-instance nil opal:filling-style (:fill-style :stippled)))
		   (:image (opal:read-image (namestring (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.small.bmp"))))))

;; (defvar *break-during-simulation* nil "Useful for debugging)

; (defvar *active* t "Enables evaluation of all channels, axons, pumps, and concentration integrators.")
(defvar *enable-channels* t "Enables evaluation of all channels, pumps, and concentration integrators.")

(defvar *are-there-synapses* nil)

(defvar *only-load-passive* nil "When T, both channel and synapse creation is blocked when loading a new circuit.")

;; This is used to modulate all cell synapse and channel conductances by a common factor.
(defvar *global-membrane-conductance-factor* 1.0)

;; These may be nil in cases where there is no change in the appropriate type of element between simulations.
(defvar *setup-elements* t)
(defvar *setup-axons* t)
(defvar *setup-synapses* t)
(defvar *setup-channels* t)
(defvar *setup-isources* t)
(defvar *setup-conc-ints* t)
(defvar *setup-pumps* t)
(defvar *setup-conc-particles* t)
(defvar *setup-sources* t)
(defvar *setup-particles* t)

(defvar *enable-reorder-elements* t "Some types of elements (e.g. channels, synapses) must be reordered if instances are created or
destroyed. When T all elements are reordered whenever create-element is called. Reordering is always made, if necessary, at the
start of every simulation.")

;; When these flags are true, the calculation of the dendritic tree input impedance stores locally. values of the input impedance in each segment.
(defvar *store-segment-z-cable-in* nil)
(defvar *store-segment-z-discrete-in* nil)

(defvar *advance-event-elements* t) ; When nil, event-driven elements are always evaluated at their initial state.

;; ****************************************
;;
;; File Related
;;
;; ****************************************

;; These 3 vars are actually defined in surf-hippo/loaders/surf-hippo-loader.lisp
;; (defvar *SURF-USER-DIR* "" "Directory for simulation data, set from the Unix environment variable $SURFUSERHOME." )
;; (defvar *SURF-USER-HOME* "" "User home directory, set from the Unix environment variable $HOME." )
;; (defvar *SURF-HOME* "" "Top-level Surf-Hippo directory, set from the Unix environment variable $SURFHOME." )

(defvar *circuit-filename* nil)		; The full namestring of the circuit file.
(defvar *circuit-file* "")		; The filename string (w/o path, and with extension) of the circuit file.
(defvar *circuit-directory* "" "Default is given by value of *SURF-USER-DIR*.")

(defvar *filename* nil)
(defvar *add-simulation-to-filenames* nil "For files written by the simulator.")

(defvar *lisp-file-directory* nil)	; For various lisp files to be loaded from the menus.
(defvar *data-directory* "" "Default given by concatenation of *SURF-USER-DIR* and /data/.")
(defvar *doc-file* "")
(defvar *plot-directory* "" "Default given by concatenation of *SURF-USER-DIR* and /plot/.")

(defvar *make-circuit-subdir* t)
(defvar *update-*plot-directory* t)
(defvar *update-*data-directory* t)
(defvar *last-simulation-file-path* nil "The name of the last file written (w/o type).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These arrays mirror the corresponding hash-tables for quicker access. NOT ALL OF THE FOLLOWING VARIABLES ARE USED.

(defvar *soma-array* nil) 
(defvar *segment-guts-list* nil)
(defvar *segment-node-2-floats-list* nil)
(defvar *channel-type-list* nil)

(defvar *isource-type-list* nil)
(defvar *vsource-type-list* nil)

;; These lists contain only synapse types that have associated synapses which are to be evaluated in the current simulation.

(defvar *particle-type-list* nil)
(defvar *conc-int-type-list* nil)
(defvar *pump-type-list* nil)
(defvar *conc-particle-type-list* nil) 
(defvar *synapse-type-list* nil)
(defvar *axon-type-list* nil) 
(defvar *vsource-array* nil)
(defvar *fixed-vsource-array* nil) 
(defvar *isource-array* nil)
(defvar *non-ideal-vsource-list* nil)	; Activated non-ideal vsources
(defvar *fixed-vsource-list* nil)	; Activated ideal vsources
(defvar *isource-list* nil)
(defvar *soma-array-length* 0)
(defvar *segment-array-length* 0)

;; Whenever the circuit structure is changed (elements added/removed), the appropriate variable below is set so that the array
;; which points to the type of the changed element can be updated.

(defvar *make-needed-v-particle-arrays* nil)
(defvar *make-needed-v-pump-arrays* nil)
(defvar *make-needed-v-buffer-arrays* nil)
(defvar *make-needed-conc-particle-arrays* nil)

(defvar *make-segment-lists* t)
(defvar *make-node-w/elements-array* t)
(defvar *lte-node-criterium* :all
  "Determines which circuit nodes will be considered for the LTE estimate. Options include :ALL \(default\), :SOMAS, :CHANNELS,
:SYNAPSES, :VSOURCES, :ISOURCES, :AXONS, or a list of circuit elements that may or may not include the afore-mentioned
keywords. If :ALL, then include all circuit nodes with externally-driven elements (e.g. sources or synapses or channels). If
:SOMAS, then include only somas. If :CHANNELS, :SYNAPSES, :VSOURCES, :ISOURCES or :AXONS, include only those nodes with the
appropriate elements.")

(defvar *last-lte-node-criterium* nil)	; For updating *NODE-W/ELEMENTS-ARRAY* whenever *LTE-NODE-CRITERIUM* has been changed.

;; ****************************************
;;
;; Geometry of the Anatomy
;;
;; ****************************************

(defvar *calculate-absolute-locations* nil)

;; ****************************************
;;
;; Lists for ordering the circuit branches
;;
;; ****************************************

(defvar *branch-list* '())
(defvar *branch-array*)
(defvar *last-seg-branch-array*)
(defvar *reverse-branch-array*)
(defvar *branch-array-limit*)

;; ****************************************
;;
;; Arrays for the Circuit Matrix and Nodes and Related Variables 
;;
;; ****************************************

(defvar *diag-double* (make-array '(1) :element-type 'double-float)) ; The diagonal of the matrix.
(defvar *lower-diag-double* (make-array '(1) :element-type 'double-float)) ; The lower diagonal of the matrix.
(defvar *upper-diag-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.
(defvar *v-at-half-step-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.
(defvar *rhs-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.

;; An array of the core nodes. These are the nodes that need evaluating.
(defvar *core-node-array* nil)
(defvar *core-node-array-length*) 
(defvar *core-node-array-length-1*)

(defvar *all-node-array*)
(defvar *all-node-array-length*)
(defvar *all-node-array-length-1*)

(defvar *node-w/elements-array*) ; An array of nodes with membrane elements - for evaluating LTE.
(defvar *node-w/elements-array-length* 0)

(defvar *num-unknowns* 0) ;; The number of unknowns in the circuit.
(defvar *num-nodes* 0 "The number of electrical circuit nodes in the circuit. Not set by user.")

(defvar *ground-node* nil) ;; The ground node, if it has been defined. It has to be defined before the simulation can start.

(defvar *need-to-reorder-matrix* nil) ;; Set true when destroying or creating ideal voltage sources.
;; ****************************************
;;
;; Variables which are used for dumping and referencing simulation results stored in files.
;;
;; ****************************************

(defvar *data-folder* nil)

(defvar *archive-variable-list* '()
  "This is a list of sublists, each of which reference variable symbol names from specific
simulations that have been loaded from a data file. The format is:

      (....
       (circuit-and-simulation-name time-variable-symbol
        (element-data-variable-symbol data-type)
        ...
        (element-data-variable-symbol data-type))
       ....
      )

")

(defmacro archive-variable-list-simulation-name (simulation-var-info-list) `(string (nth 0 ,simulation-var-info-list)))
(defmacro archive-variable-list-simulation-time (simulation-var-info-list) `(symbol-value (nth 1 ,simulation-var-info-list)))
(defmacro archive-variable-list-simulation-symbols-and-data-types (simulation-var-info-list) `(car (last ,simulation-var-info-list)))
(defmacro archive-variable-list-simulation-symbol (symbol-and-data-type) `(car ,symbol-and-data-type))
(defmacro archive-variable-list-simulation-symbol-value (symbol-and-data-type) `(symbol-value (car ,symbol-and-data-type)))
(defmacro archive-variable-list-simulation-data-type (symbol-and-data-type) `(cadr ,symbol-and-data-type))

(defvar *archive-session-results* '() "Set by results files (see analysis.doc). The contents of this list is typically results of
analysis done on circuit data (waveforms) at the end of a simulation, rather than the raw data.") 

(defvar *file-output-variable-list* '()
  "This is a list of the variables and their properties. Each entry has the following format:

      (VAR-SYMB CIRCUIT-ELEMENT DATA-SLOT)

")

;; ****************************************
;;
;; Details for Printing Out Simulation Information and Saving Simulation Data and Info to Files.
;;
;; ****************************************

(defvar *simulation-description-destination* :Info_Window)

(defvar *simulation-print-detail* :terse
  "Detail for PRINT-CIRCUIT.
Values include :NONE :TERSE :MEDIUM :FULL and :FULL_WITH_SEGMENTS.")

(defvar *simulation-file-name* nil)
(defvar *save-simulation-data* t "Enable saving of simulation data.")
(defvar *save-simulation-data-to-file* nil "Enable saving of simulation data to file.")
(defvar *save-simulation-info* nil)
(defvar *save-full-simulation-info* nil)

;; This is set to some form by the user which is then written to the results file, along with *SIMULATION-NAME* to supply
;; convenient tags for later data analysis. 

(defvar *simulation-results-addendum* nil)

;; A list of symbols which are printed out with their values by PRINT-CIRCUIT.

(defvar *documented-user-variables* nil "Global variables printed out by PRINT-CIRCUIT. 
See also *DOCUMENT-ALL-NEW-VARIABLES*.") 

(defvar *document-all-new-variables* nil "For PRINT-CIRCUIT output.
Include bound variables defined after initialization (in SURF package), or in *DOCUMENTED-USER-VARIABLES*.")

(defvar *enable-print-documented-user-variables* t "Enable printing of *DOCUMENTED-USER-VARIABLES*.")

; Enables the short analysis printout at end of simulation.
(defvar *print-out-to-info-window* nil)	; To Info window.
(defvar *print-out-to-lisp* t)		; To lisp window.

(defvar *print-full-to-info-window* nil)

(defvar *print-numerical-details* nil "Include parameters of numerical method in print outs")
(defvar *dump-analysis-to-file* nil)

;; These three are old, better to use *SIMULATION-PRINT-DETAIL*
(defvar *print-full-to-lisp* nil)
(defvar *print-mini-to-lisp* nil)
(defvar *include-segments-w-full-description* nil)

;; ****************************************
;;
;;; Simulation/Circuit Modification Monitoring
;;
;; ****************************************

;;; Whenever the following functions are called, the state of *MONITOR-CIRCUIT-MODIFICATIONS* determines whether a record of the
;;; call is printed out to the lisp listener. This record can then be saved later for helping reconstruct circuits.  
;;;
;;; ADD-CHANNEL-TO-LOCATIONS
;;; ADD-SYNAPSE-TO-LOCATIONS
;;; ADD-CHANNEL-TO-CELLS
;;; ADD-SYNAPSE-TO-CELLS

(defvar *monitor-circuit-modifications* t)

;; ****************************************
;;
;; Graphics and Information Windows
;;
;; ****************************************

(defvar *colorizing-simulation* nil)
(defvar *colorize-simulation* nil "Enable colorization of simulation in some or all histology windows.")
(defvar *monitor-simulation* nil)
(defvar *enable-colorize-time* nil "Enable time display in colorized histology windows.")
(defvar *enable-colorize-scale* nil "Enable color scale display in colorized histology windows.")

(defvar *enable-sparse-data* nil "Store circuit element data on a sparse time grid.
Grid defined by *SPARSE-DATA-STEP*, and referenced by *SPARSE-DATA-TIMES*.")
(defvar *sparse-data-step* 0.5 "Time step target for *SPARSE-DATA-TIMES*.")		; ms
(defvar *last-sparse-data-time* 0.0)	; Used when collecting sparse data during a simulation

(defvar *sparse-data-times* nil)
(defvar *reverse-sparse-data-times* nil)

(defvar *create-new-histology-window* nil)	; Create a new histology window.
(defvar *create-new-info-window* nil)	; Create a new info window.
(defvar *include-channel-type-comment-in-particle-plots* nil "Include channel type Gbar and E-rev in particle type plots.")

(defvar *where-synapse-stimulus-goes* :back)
(defvar *show-light-synapse-rf-centers* t)
(defvar *synapse-type-colors* nil)
(defvar *channel-type-colors* nil)
(defvar *node-graphics-coefficient* 2.0)
(defvar *axon-graphics-diameter* 1.0) ; microns
(defvar *synapse-cxn-graphics-diameter*  1.0) ; um
(defvar *syn-rf-connection-thickness* 1)
(defvar *syn-rf-connection-shading* 100)
(defvar *syn-rf-connection-dash* 4)
(defvar *syn-rf-shape-shading* 25)


(defvar *motion-snapshots* 5 "Number of moving stimulus snapshots for histology graphics.")
(defvar *label-stimulus-times* t "If stimulus drawn, label times of stimulus snapshots.")
(defvar *restrict-snapshot-to-real-time* nil) ;; To draw stimulus during animation.

(defvar *stimulus-graphic-shading-percent* 20)

(defvar *stimulus-graphic-color* (get-color-from-library .25 .25 .25))

(defvar *histology-window-stimulus-time-distance-from-top* 10) ; pixels
(defvar *histology-window-stimulus-time-distance-from-left* 15) ; pixels
(defvar *histology-window-stimulus-time-length* 200) ; pixels

(defvar *default-marker-diameter* 10)	; pixels

(defvar *histology-scale-bar-unit-string* "um")
(defvar *histology-comment* "")

(defvar *soma-histology-fixed-diameter-p* nil "Cell somas drawn with a diameter given by *SOMA-HISTOLOGY-FIXED-DIAMETER-PIXELS*.")
(defvar *soma-histology-fixed-diameter-pixels* 10 "Diameter in pixels for drawing somas, when *SOMA-HISTOLOGY-FIXED-DIAMETER-P* is T.")

(defvar *draw-soma-outline* t)
(defvar *soma-points* )
(defvar *soma-outline* )
(defvar *draw-soma-polylines* t)

(proclaim '(single-float *node-graphics-coefficient*))

;; ****************************************
;;
;; Things Having To Do With Channel Particles
;;
;; ****************************************

;; (defvar *use-particle-look-up-tables* t)
;; (defvar *use-particle-types* t)

;; The nominal particle voltage range is from -200 to 200 mV. Be sure to change the following
;; variables in a consistent manner.
(defconstant *particle-look-up-table-min-voltage* -200.0) ;  mV
(defconstant *particle-look-up-table-max-voltage* 200.0) ;  mV
(defconstant *particle-look-up-table-min-voltage-double* (coerce *particle-look-up-table-min-voltage* 'double-float)) ;  mV
(defconstant *particle-look-up-table-voltage-range* (coerce (- *particle-look-up-table-max-voltage* *particle-look-up-table-min-voltage*) 'single-float)) ; mV

(defconstant *particle-look-up-table-precision* 0.1) ; mV
(proclaim '(single-float *particle-look-up-table-precision*))
(defconstant *particle-look-up-table-length* (round (/ *particle-look-up-table-voltage-range* *particle-look-up-table-precision*)))

(proclaim '(type single-float *particle-look-up-table-min-voltage* *particle-look-up-table-max-voltage* *particle-look-up-table-voltage-range*))
(proclaim '(type double-float *particle-look-up-table-min-voltage-double*))
(proclaim '(type fixnum *particle-look-up-table-length* *particle-look-up-voltage-range*))

(defvar *particle-type-plot-minimum-voltage* -100) ; mV
(defvar *particle-type-plot-maximum-voltage* 50) ; mV
(defvar *particle-kinetics-plot-width* 500) ; pixels
(defvar	*particle-kinetics-plot-height* 500) ; pixels

(defvar *constant-field-equation-exponential-term-array* nil)
(defvar *use-cf-exp-lookup* nil)

;; ****************************************
;;
;; Plotting and Saving Plot Data
;;
;; ****************************************

(defvar *voltage-plot-waterfall-x-offset* 0.0)
(defvar *voltage-plot-waterfall-y-offset* 50.0)
(defvar *voltage-plot-waterfall* nil)
(defvar	*auto-plot-waterfall* t)
(defvar *normalized-gbar-label* (format nil "rel.~%gbar"))
(defvar *add-trace-analysis-to-plot* nil)
(defvar *remove-initial-value-from-plot-trace-analysis-integrals* t)

(defvar *hard-copy-screen* nil "Hardcopy screen after each simulation.")
(defvar *create-new-simulation-plots* nil "Create a new set of plot windows for each simulation.")

(defvar *massage-element-plot-labels* T "Elaborate simple integer element names in data plots.")

(defvar *plot-standard-windows* t "Enable the standard plotting. Does not affect simulation data storage.")
(defvar *hide-plot-windows* nil "Hide simulation plots, even if they're created.")

(defvar *save-conductances-normalized* nil "Save element conductances in normalized form.")

(defvar *plot-channels-by-major-ion* t "Plot channels by major permeant ion.")
(defvar *plot-synapses-by-major-ion* t	; "Plot synapses by major ion."
  )

;; (defvar *position-plots-by-hand nil)

(defvar *default-surf-plot-x-label* "msec")

(defvar *enable-plot-labels* t)
(defvar *use-same-line-style* nil)

(defvar *plot-data-grouping* :cell "If non-nil sets criterium for plotting in separate windows [:CELL, :CELL-TYPE].")

;; Semi-backward compatibility (non-functional).
(defvar *group-plot-data-by-cell* nil)
(defvar *group-plot-data-by-cell-type* nil)

(defvars
  (*overlay-simulations nil)		; This is now superseded by *OVERLAY-PLOTS*, defined in plot-hack-declare.lisp
  (*overlay-simulations-last nil)
  (*custom-plot-lists* '())
  (*plot-custom-windows* t))

(defvar *use-simulation-name-for-simulation-plot-titles* T
  "Base simulation plot titles on the *SIMULATION-NAME*, which changes with every simulation, or if NIL then on *CIRCUIT*, which
is normally constant for a given loaded circuit.") 

(defvar *simulation-plot-window-comment* nil "When a string, comment added to all simulation plot windows, at the position given by
*SIMULATION-PLOT-WINDOW-COMMENT-POSITION*. See also *GLOBAL-PLOT-COMMENT*") 
(defvar *simulation-plot-window-comment-position* :lower-right "Position for *SIMULATION-PLOT-WINDOW-COMMENT* that is added to all simulation plot windows.")

(defvar *traces-per-plot* 6 "Unless equal to 0, constrains the number of traces per plot window.") 

(defvar *max-num-traces-for-plot-trace-labels* 6
  "When non-nil, if the number of traces in a simulation plot is more than this number, then the plot trace labels will be suppressed.") 



(defvar *automatic-voltage-plot-scaling* t)
(defvar *voltage-plot-min* -75.0 "Voltage plot minimum [mV]")
(defvar *voltage-plot-max* -100  "Voltage plot maximum [mV]")
(defvar *automatic-soma-voltage-plot-scaling* t)
(defvar *soma-voltage-plot-min* -75.0 "Soma voltage plot minimum [mV]")
(defvar *soma-voltage-plot-max* 30.0 "Soma voltage plot maximum [mV]")

(defvar *save-data-step* 2 "Simulation steps per saved data point [integer >= 1].") 
(defvar *save-data-step-count*)	; Keeps track of when to save the data.
(defvar *save-data-called-p* nil ;   "Set when the first time element data is saved during a simulation."
  )

(defvar *label-surf-plots* t)
(defvar *update-plots* t)
(defvar *resurrect-plots* nil)
(defvar *visible-plots* t)

(defvar *plot-total-concs-separately* t "For plotting out concentration integrator total concentrations.")
;; (defvar *plot-shell-concs-separately* t "For plotting out concentration integrator concentrations.")

;; These flags enable/disable plotting windows of various types.

(defvar *plot-event-generators* nil)
(defvar *plot-events* t)

(defvar *plot-node-elements* '() "A reference or list of reference, for each everything on the associated cell node will be plotted.")

(defvar *plot-separate-soma-window* nil)

(defvar *plot-lists-cells* '())
(defvar *plot-lists-cell-types* '())

(defvar *total-conductances-data* nil)
(defvar *plot-total-conductances-p* nil "Whether to collect and plot total conductances as defined in *PLOT-TOTAL-CONDUCTANCES*.")
(defvar *debug-total-conductances* nil)
(defvar *plot-total-conductances* nil
  "Plot summed conductances over cells and/or membrane element types.
The format is a list of atoms or lists, as follows:

      :ALL
      TYPE
      CELL
      (CELL TYPE TYPE ...)
      (CELL :ALL)

where TYPE refers to a synapse or channel type and CELL refers to a cell or cell type. If a cell or cell type is indicated, then
the total linear membrane conductance is saved for that cell or for all cells of the cell type, as appropriate. If a cell or cell
type is in a list followed by synapse or channel types, then the linear membrane conductance is summed with the conductance of all
the synapses or channels of the indicated types. If a cell or cell type is in a list followed by :ALL, then the summation is taken
over all synapse and channel types in a given cell or cells of a cell type. In general, use the function SETUP-PLOT-TOTAL-CONDUCTANCES to set up this
variable. If the only entry is :ALL, then the total conductance of all cells in the circuit will be plotted.")

(defvar *plot-total-conductance-specifications* nil)

#|
These flags enable various plots:

*PLOT-MEMBRANE-VOLTAGE-P*
*PLOT-MEMBRANE-VOLTAGE-DERIVATIVE-P*
*PLOT-MEMBRANE-VOLTAGES-P* ...

If one of the following globals is set to 'ALL, then the function CHOOSE-PLOT-DATA will set
it to a list of all the instances of the appropriate circuit element. Otherwise, the lists
should contain the names of the elements to be plotted:

*PLOT-MEMBRANE-VOLTAGES*
*PLOT-MEMBRANE-VOLTAGE-DERIVATIVES*
*PLOT-AXON-VOLTAGES*
*PLOT-AXON-EVENTS* ...

These mirror the previous lists, but hold the associated structures:

*PLOT-MEMBRANE-VOLTAGES-STRUCTURES*
*PLOT-MEMBRANE-VOLTAGE-DERIVATIVES-STRUCTURES*
*PLOT-AXON-VOLTAGES-STRUCTURES*
*PLOT-AXON-EVENTS-STRUCTURES* ...

*PLOT-LISTS-INFO* global variable structure:

    global-var-list-of-structure-names
    list-of-types - (model names)
    structure-slot
    enable-var
    global-var-list-of-structures
    plot-y-label
    extra-info
|#

(defmacro plot-list-info-names (plot-list-info) `(nth 0 ,plot-list-info))
(defmacro plot-list-info-types (plot-list-info) `(nth 1 ,plot-list-info))
(defmacro plot-list-info-tables (plot-list-info) `(mapcar 'get-model-hash-table (plot-list-info-types ,plot-list-info)))
(defmacro plot-list-info-structure-slot (plot-list-info) `(nth 2 ,plot-list-info)) ; e.g. 'conductance, 'current, 'voltage, etc.
(defmacro plot-list-info-enable-var (plot-list-info) `(nth 3 ,plot-list-info))
(defmacro plot-list-info-structures (plot-list-info) `(nth 4 ,plot-list-info))
(defmacro plot-list-info-units-label (plot-list-info) `(nth 5 ,plot-list-info))	; e.g. "nA", "mV", etc.
(defmacro plot-list-info-extra-info (plot-list-info) `(nth 6 ,plot-list-info)) ; A final optional entry to comment the plot type

(defvar *plot-lists-info*
  `((*plot-membrane-voltages* (soma segment) voltage *plot-membrane-voltages-p* *plot-membrane-voltages-structures* "mV")
    (*plot-membrane-voltage-derivatives* (soma segment) dvdt *plot-membrane-voltage-derivatives-p* *plot-membrane-voltage-derivatives-structures* "mV/ms")
;;  (*plot-segment-voltages* (segment) voltage *plot-segment-voltages-p* *plot-segment-voltages-structures* "mV")
;;  (*plot-segment-voltage-derivatives* (segment) dvdt *plot-segment-voltage-derivatives-p* *plot-segment-voltage-derivatives-structures* "mV/ms")
    (*plot-path-nodes* (soma segment) voltage *plot-path-node-voltages-p* *plot-path-nodes-structures* "mV")
    (*plot-capacitance-currents* (soma segment) capacitance-current *plot-capacitance-currents-p* *plot-capacitance-currents-structures* "nA")
    (*plot-LEAK-Currents* (soma segment) leak-current *plot-leak-currents-p* *plot-leak-currents-structures* "nA")
    (*plot-axon-voltages* (axon) voltage *plot-axon-voltages-p* *plot-axon-voltages-structures* "mV")
    (*plot-axon-events* (axon) event *plot-axon-events-p* *plot-axon-events-structures* "Event")
;;  (*plot-soma-voltages* (soma) voltage *plot-soma-voltage-p* *plot-soma-voltages-structures* "mV")
;;  (*plot-soma-voltage-derivatives* (soma) dvdt *plot-soma-voltage-derivative-p* *plot-soma-voltage-derivatives-structures* "mV/ms")
    (*plot-soma-dendrite-currents* (soma) dendrite-current *plot-soma-dendrite-currents-p* *plot-soma-dendrite-currents-structures* "nA")
    (*plot-channel-currents* (channel) current *plot-channel-currents-p* *plot-channel-currents-structures* "nA")
    (*plot-channel-conductances* (channel) conductance *plot-channel-conductances-p* *plot-channel-conductances-structures* "uS")
    (*plot-channel-reversal-potentials* (channel) reversal-potential *plot-channel-reversal-potentials-p* *plot-channel-reversal-potentials-structures* "mV")
    (*plot-channel-type-currents* (channel-type) current *plot-channel-type-currents-p* *plot-channel-type-currents-structures* "nA")
    (*plot-channel-type-conductances* (channel-type) conductance *plot-channel-type-conductances-p* *plot-channel-type-conductances-structures* "uS")
    (*plot-synapse-currents* (synapse) current *plot-synapse-currents-p* *plot-synapse-currents-structures* "nA")
    (*plot-synapse-conductances* (synapse) conductance *plot-synapse-conductances-p* *plot-synapse-conductances-structures* "uS")
    (*plot-synapse-reversal-potentials* (synapse) reversal-potential *plot-synapse-reversal-potentials-p* *plot-synapse-reversal-potentials-structures* "mV")
    (*plot-synapse-events* (SYNAPSE) event *plot-synapse-events-p* *plot-synapse-events-structures* "Event")
    (*plot-synapse-type-currents* (synapse-type) current *plot-synapse-type-currents-p* *plot-synapse-type-currents-structures* "nA")
    (*plot-synapse-type-conductances* (synapse-type) conductance *plot-synapse-type-conductances-p* *plot-synapse-type-conductances-structures* "uS")
    (*plot-conc-1-ints* (conc-int) 1 *plot-shell-1-concentrations-p* *plot-conc-1-ints-structures* "mM")
    (*plot-conc-2-ints* (conc-int) 2 *plot-shell-2-concentrations-p* *plot-conc-2-ints-structures* "mM")
    (*plot-conc-3-ints* (conc-int) 3 *plot-shell-3-concentrations-p* *plot-conc-3-ints-structures* "mM")
    (*plot-conc-ints* (conc-int) total *plot-concentrations-p* *plot-conc-ints-structures* "mM")
    (*plot-buffers* (buffer) concentration *plot-buffer-concentrations-p* *plot-buffers-structures* "mM")
    (*plot-conc-particles* (conc-particle) state *plot-conc-particles-p* *plot-conc-particles-structures* "State")
    (*plot-particles* (particle) state *plot-particles-p* *plot-particles-structures* "State")
    (*plot-markov-particles* (particle) markov-state *plot-markov-particles-p* *plot-markov-particles-structures* "State")
    (*plot-pump-currents* (pump) current *plot-pump-currents-p* *plot-pumps-structures* "mM/ms")
    (*plot-isource-currents* (isource) current *plot-isource-currents-p* *plot-isource-currents-structures* "nA")
    (*plot-vsource-currents* (vsource) current *plot-vsource-currents-p* *plot-vsource-currents-structures* "nA")
    (*plot-vsource-voltages* (vsource) voltage *plot-vsource-voltages-p* *plot-vsource-voltages-structures* "mV")
    (*plot-field-potentials* (extracellular-electrode) field-potential *plot-field-potentials-p* *plot-extracellular-electrodes-structures* "mV")))

(defvar *all-save-voltage-nodes* nil)
(defvar *all-save-dvdt-nodes* nil)
(defvar *all-save-capacitance-current-nodes* nil)
(defvar *all-save-leak-current-nodes* nil)

;; ****************************************
;;
;; Simulation-related variables.
;;
;; ****************************************

(defvar *recheck-circuit-elements-parameters* nil) ;  Used in the menu loop

(defvars
  (*enable-segment-membrane-parameter-update* t)
  (*enable-soma-membrane-parameter-update* t)
  (*enable-conc-integrator-membrane-parameter-update* t)
  (*enable-buffer-membrane-parameter-update* t)
  (*enable-pump-membrane-parameter-update* t)
  (*enable-channel-type-membrane-parameter-update* t)
  (*enable-channel-membrane-parameter-update* t)
  (*enable-axon-membrane-parameter-update* t)
  (*enable-synapse-membrane-parameter-update* t))

(defvar *interpolate-particle-arrays* nil
  "For the evaluation of two-state gating particles.
Iinterpolate values derived from the voltage-dependent kinetic look-up arrays.")

;; (defvar *eval-all-elements-with-hash t)
;; (defvar  *use-new-part-formula t)
;; (defvar *evaluate-particles-with-tau-inf t)

;; ****************************************
;;
;; Current and Voltage Sources
;;
;; ****************************************

;; (defvar *source-pulse-lists* '())	; For both voltage and current sources.

(defvar *minimum-source-transition-time* 1e-3
  "Default minimum pulse transition time in milliseconds for PWL sources. 
If this is too small then source waveforms can be distorted.") 

(defvar *evaluate-fixed-node-voltages* nil)

(defvar *consider-isource-drop* nil
  "For the recorded voltage of elements with current sources.
Consider the IR drop across the source. Used in the function RECORDED-NODE-VOLTAGE.")


(defvar *source-resistance-lists* '())

(defvar *default-waveform-step* 1.0 "Default waveform time steps for sources and synapses [msec].")

(defvar *pwl-isource-di-dt* 100.0 "Default transition slope for pwl isources \(nA/msec\).")
(defvar *isource-electrode-resistance* 0.0 "Mohms")

(defvar *isource-default-reference-magnitude* 0.0 "nA")
(defvar *vsource-default-reference-magnitude* 0.0 "mV")

(defvar *steady-state-linear-vclamp-enable* t
  ;; "When true, cells with single eligible voltage sources will be initialized with a linear single-step steady-state method
  ;; [STEADY-STATE-LINEAR-VOLTAGE-CLAMP]." 
)
 
(defvar *pwl-vsource-dv-dt* 1000.0 "The  default transition slope for pwl vsources (mV/msec).")
(defvar *vsource-resistance* 0.001 "Series resistance, in Mohms, for the non-ideal vclamp. Must be >0.")
(defvar *minimum-vsource-resistance* 0.0001) ; Mohms

(defvar *enable-initial-vsource-slope* t)
(defvar *modify-stimulus* Nil)		; For sources

;; ****************************************
;;
;; Time Step Related Variables, Breakpoints, etc.
;;
;; ****************************************

(defvar *num-nodes-check-break-times* 10)

;; We deal with time in units of *MRT* is to avoid floating point time representations during time steps - this may not really gain much now.

(defconstant most-positive-fixnum-float-version (float most-positive-fixnum))

;; The number of bits in a integer used for internal times.
;; Inherited from webber's surf - works ok.
;; This is not guarranteed. The +cmu figure of 28 was estimated by finding the type-of 2^N, and N=28 was the largest value that gave a FIXNUM.
(defconstant time-word-length #-cmu 32 #+cmu 28)

;; The maximum integer used for internal times. The word length is subtracted by 2, one to allow for 2's complement math, and one for safety if a
;; simulation runs over the stop time. I don't know if the second one is necessary.  Inherited from webber's surf - works ok.

; (defconstant max-integer-time (expt 2 (- time-word-length 2)))

;; Change LBG 8.26.99
;; Quantization error analysis suggests that the correction of "2" above to time-word-length is insufficient, since the conversion of integer time values
;; to real time values via *mrt* can give the same float value for successive integer values. Empirical testing of this conversion, which is more prone to
;; error at the stop time, show that a correction factor of 5 avoids this problem.

(defconstant max-integer-time (expt 2 (- time-word-length 5)))

(defvar *mrt* 1.0) ;; The minimum resolvable time, to convert floating times into integers.
(defvar *user-start-time* 0.0)		;  The start time of the simulation, in milliseconds.
(defvar *start-time* 0) ; The start time in units of *mrt*.
(defvar *user-stop-time* 1.0 "Simulation duration [milliseconds].")
(defvar *last-user-stop-time* 1.0)	; The stop time of the last simulation.
(defvar *int-user-stop-time* 0) ; The integer version of the  time to end the simulation, in milliseconds.
(defvar *stop-time* 0) ; The stop time in units of *mrt*.

(defvar *first-time-step* nil) ;; T just for the first time step of the simulation. Useful in various places, such as evaluation of events.

;; just so something is on the list after stop-time ???
(defvar *extra-time-after-stop-time* 1.0)

(defvar *user-max-step* 5.0 "The maximum time step allowed, in milliseconds. When 0, then *MAX-STEP* is bound by the simulation duration.")
(defvar *max-step* 0) ; The maximum time step in units of *mrt*.

(defvar *user-min-step* 0.0 "Minimum allowed time step [msec].
When 0, then *MIN-STEP* is *MIN-STEP-MRTS*.")
(defvar *user-min-step-double* 0.0d0)
(defvar *MIN-STEP-MRTS* 4)		; When = 2, the smallest half step is 1 mrt, which will choke since the single
					; float coercion of the sum of the double float equivalent and values whose
					; magnitude is greater than 4 give the original value. This shows up for example
					; as

#|
                         2]]]  (*t-prime[n-prime]*)
                         5.000001
                         2]]]  (*t-prime[n-prime-1]*)
                         5.000001
|#

(defvar *min-step* 0) ; The minimum time step in units of *mrt*.

;; These are fixnums, ie in units of *mrt*
(defvar *sim-time-n+1* 0 "Time for the step currently being computed in units of *MRT*. t(n+1)")
(defvar *sim-time-n*   0 "Time for the step already computed in units of *MRT*. t(n)")
(defvar *sim-time-n-1* 0 "Time for one step back in units of *MRT*. t(n-1)")
(defvar *sim-time-n-2* 0 "Time for two steps back in units of *MRT*. t(n-2)")
(defvar *time-step* 1 "Current time step in units of *MRT*.")
(defvar *last-time-step* 1 "The last time step in units of *MRT*.")

;; The *USER-MAX-STEP* constraint may be overruled by the evaluation of some element types, in particular those that are driven by an a-priori waveform. In
;; these cases, the global variable *ELEMENT-TIME-STEP-MAXIMUM* is set to a non-NIL step value (in units of *mrt*) that matches the (smallest) time
;; interval of the waveform(s).

(defvar *element-time-step-maximum* nil)

;; When true, at each time step the circuit inputs (e.g. sources, driven synapses) are evaluated at the midpoint of the step - otherwise, the inputs are
;; evaluated for the end of the step (the prediction time).
(defvar *evaluate-inputs-at-midpoint* nil)

;; To avoid consing, the single and double float globals which track the time steps, errors and other randoms are now stored in arrays:

(defvar *global-double-float-array* (make-array '(10) :element-type 'double-float))

;; For all gating particles.
(defmacro *maximum-particle-error-numerator* () `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-double-float-array*) 0))
(defmacro *markov-particle-state-delta-s-max-time-step* () `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-double-float-array*) 2))
(defmacro *maximum-conc-int-error-numerator* () `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-double-float-array*) 3))

;; This can be used locally in functions for efficient temporary storage.
(defmacro *temporary-double-float-register* () `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-double-float-array*) 4))

(defvar *include-vsource-nodes-in-node-error-est* nil "Include vsource nodes in node error estimate.")

(defvar *global-time-single-float-array* (make-array '(10) :element-type 'single-float))
(defvar *global-time-double-float-array* (make-array '(20) :element-type 'double-float))

;; In general, "prime" associated with a time variable name refers to the staggered time grid used for particle evaluations. "prime" associated with the
;; time index, e.g. "n-prime", refers to the index incremented by 1/2.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros (and associated variables) which are referenced in *GLOBAL-TIME-SINGLE-FLOAT-ARRAY*.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Previously called (*real-time*).
(defmacro *t[n+1]* ()
  "Time during simulation [msec], corresponding to the prediction time t_(n+1)."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 0))

(defvar *real-time* 0.0 "Time during simulation [msec], corresponding to the prediction time t_(n+1).")
(defvar *simulation-max-time* 0.0 "Maximum time reached during simulation (msec).")

(defmacro *fractional-time* ()
  "Fractional part of (*T[N+1]*)."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 1))

(defmacro *input-time* ()
  "Time reference for inputs [msec]."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 2))

;; Previously called (*last-real-time*).
(defmacro *t[n]* ()
  "Time during simulation [msec], corresponding to the current time t_n."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 3))
(defvar *last-real-time* 0.0 "Time during simulation corresponding to the current time (msec)")

(defmacro *t-prime[n+1]* ()
  "Time during simulation [msec] of the staggered grid, corresponding to the prediction time t-prime_(n+1)."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 4))

(defmacro *t-prime[n]* ()
  "Time during simulation [msec] of the staggered grid, corresponding to the current time t-prime_n."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 5))

(defmacro *t-prime[n-prime]* ()
  "Time during simulation [msec] of the staggered grid, halfway between (*T-PRIME[N]*) and (*T-PRIME[N+1]*)."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 6))

(defmacro *t-prime[n-prime-1]* ()
  "Time during simulation [msec] of the staggered grid, halfway between (*T-PRIME[N-1]*) and (*T-PRIME[N]*)."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 7))

(defmacro *t[n]-t-prime[n-prime]* ()
  "Difference between (*T[N]*) and (*T-PRIME[N-PRIME]*) in msec."
  `(aref (the (SIMPLE-ARRAY SINGLE-FLOAT (*)) *global-time-single-float-array*) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros (and associated variables) which are referenced in *GLOBAL-TIME-DOUBLE-FLOAT-ARRAY*.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Previously called (*real-time-step*) ?
(defmacro *delta-t[n]* ()
  "The current time step [t_(n+1) - t_n], in msec."
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 0))

(defmacro *delta-t[n]-squared* ()
  "msec^2"
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 1))

(defmacro *delta-t[n-1]* ()
  "The last time step [t_n - t_(n-1)], in msec."
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 2))

;; Previously called delta-back
(defmacro *half-delta-t[n-1]* ()
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 3))

(defmacro *delta-t-prime[n]* ()
  "The current time step for the staggered grid, [t-prime_(n+1) - t-prime_n], in msec."
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 4))

(defmacro *delta-t-prime[n]-squared* ()
  "msec^2"
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 5))

(defmacro *half-delta-t-prime[n]* ()
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 6))

(defmacro *delta-t-prime[n-1]* ()
  "The last time step for the staggered grid, [t-prime_(n) - t-prime_(n-1)], in msec."
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 7))

(defmacro *2/delta-t[n]* ()
  "The constant to multiply capacitances by for the trapezoidal rule."
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 8))

(defmacro *markov-time-step* ()
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 9))
  
(defmacro *markov-time-step/2* ()
  `(aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*)) *global-time-double-float-array*) 10))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *integer-time* 0 "The integer part of real-time")

(defvar *integration-time-reference* :variable
  ":VARIABLE [LTE], :FIXED [use *USER-STEP*] or :LIST [use *LAST-SIM-REVERSE-TIME-STEP-LIST* if non-NIL].") 

;; backward compatibility
;; "Unless *USE-TIME-LIST* is T and valid, ; when T, use a fixed step integration, ; referencing *USER-STEP*. Otherwise use variable time step."
(defvar *use-fixed-step* nil) 
;; backward compatibility
;; "When T, take integration time points from *LAST-SIM-REVERSE-TIME-STEP-LIST*. If there was no previous simulation, this is ignored."
(defvar *use-time-list* nil)

(defvar *user-step* 0.010 "Fixed time step [ms] for :FIXED step integration.")
(defvar *user-time-step* 1) ; in units of *mrt*

;; These are lists of time points associated with the plot-data for the simulation. *SIM-REVERSE-PLOT-TIME-LIST* is in reverse order, as are all the plot
;; data lists associated with the circuit structures.

(defvar *sim-reverse-plot-time-list* '() "The list of plotted time points for the simulation in the reverse order. Updated during the simulation.")  
(defvar *sim-plot-time-list* '() "After a simulation is complete, the list of plotted time points in the correct order.")

;; *SIM-REVERSE-TIME-STEP-LIST* *SIM-REVERSE-TIME-LIST* *LAST-SIM-REVERSE-TIME-LIST* are in reverse time order.

(defvar *sim-reverse-time-step-list* '() "All the time steps in the simulation in reverse time order. Updated during the simulation.")
(defvar *last-sim-reverse-time-step-list* '() "All the time steps in the last simulation (if saved) in reverse time order.")
(defvar *sim-reverse-time-list* '() "All the time points in the simulation in reverse time order. Updated during the simulation.")
(defvar *last-sim-reverse-time-list* '() "All the time points in the last simulation (if saved) in reverse time order.")
(defvar *auto-refresh-last-sim-reverse-time-list* nil "Set *LAST-SIM-REVERSE-TIME-LIST* to the last simulation's time list.")

;; A list in correct time order of fixed time steps (units of *mrt*) that are used to set each time step when *USE-TIME-LIST* is T.
(defvar *fixed-time-steps* '())

(defvar *use-node-voltage-initializations* nil "Use *NODE-VOLTAGE-INITIALIZATIONS* for setting node voltages at start of simulation.")

(defvar *node-voltage-initializations* '() "A list of dotted pairs, where for each pair the CAR is a node and the CDR is that node's initial value in mV.") 
(defvar *use-conc-int-initializations* nil "Use *CONC-INT-INITIALIZATIONS* to set initial concentrations.")
(defvar *use-buffer-initializations* nil "Use *BUFFER-INITIALIZATIONS* to set initial buffer states.")
(defvar *use-pump-initializations* nil "Use *PUMP-INITIALIZATIONS* to set initial pump states.")

(defvar *conc-int-initializations* '() "A list of dotted pairs, where for each pair the CAR is a concentration integrator and the
CDR is that integrator's initial value in mM.") 

(defvar *buffer-initializations* '() "A list of dotted pairs, where for each pair the CAR is a buffer and the CDR is that buffer's
initial value in mM.")  

(defvar *pump-initializations* '() "A list of dotted pairs, where for each pair the CAR is a pump and the CDR is that pump's
initial value in mM.")  

(defvar *breakpoint-list* '()) ; A list of the break points of pwl sources and other inputs.

(defvar *last-breakpoint-list* '()) ; A list of the break points used in the last simulation.

(defvar *user-breakpoint-list* '() "A list of break points that is specified by the user, and then added to the points
automatically collected into *BREAKPOINT-LIST*.")

(defvar *enable-user-breakpoint-list* t "Breakpoints [in ms] in *USER-BREAKPOINT-LIST* are considered for variable time step.")
  
(defvar *mrt-breakpoint-list* '()) ;; Cleaned up break points, in units of *mrt* - this is what is used by PICK-TIME-STEP. "Cleaned up" means that these
;; are sorted, with no duplicates, and bounded by 0 and the sum of *extra-time-after-stop-time* and *user-stop-time*. 

(defvar *enable-dynamic-breakpoint-generation* t
  "Enable dynamically generated breakpoints.
Breakpoints generated during a simulation by event-based elements such as axons and voltage-dependent synapses.") 

;; Internal enable for breakpoints to be dynamically generated during a simulation by event-based elements such as axons and voltage-dependent synapses. 
(defvar *dynamic-breakpoint-generation* t)

(defvar *total-num-iterations* 0 "The total number of iterations over all time.")
(defvar *total-num-time-points* 0 "The total number of time points taken for the simulation.")
(defvar *use-max-iterations* nil "*MAX-ITERATIONS* constrains time steps.")
(defvar *max-iterations* 1 "Useful for debugging.")

;; Related specifically to synapse evaluation.

(defvar *rectify-synapse-conductances* t "When T halfwave rectify, threshold at 0.
Applied to all synaptic conductances during simulation. For VOLTAGE and EVENT synapse types.") 
(defvar *enable-event-synapse-evaluation-method* t)

;; CAR of (element-parameter type 'evaluation-method-parameters) can be :convolution :2nd-order-integration

(defvar *synapse-evaluation-step* 0.2)	; ms
(defvar *eval-all-synapses-this-step* t)
(defvar *eval-all-synapses-this-iteration* nil)
(defvar *eval-all-synapses-every-step* t)
(defvar *synapse-evaluation-times* '())
(defvar *use-constant-element-matrix-contribution* nil)

;; LBG 7/6/99
;; For VOLTAGE synapses whose conductance waveform is a single exponentional of time constant TAU, *and* when a fixed time step dT is used, the synapse
;; evaluations are simplified by the fact that at each time step the new conductance value is given by the previous step value multiplied by a decay
;; factor, exp -dT/TAU. This decay factor is precalculated for a given type and stored as an element parameter 'CONDUCTANCE-DECAY-FACTOR.
(defvar *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* nil)

;; THESE TWO FLAGS ARE NOT USED NOW.
;; (defvar *pseudo-transient-requested* nil "If t, do a pseudo transient DC solution.")
;; (defvar *dc-solution-computed* nil "Automatically set to t after a DC solution")

;; ****************************************
;;
;; Error Estimation Variables
;;
;; ****************************************

;; When LTE estimate is too large despite using the minimum step, keep going anyway.
(defvar *punt-lte-when-min-step-reached* t "Ignore LTE estimate if below minimum step.")

;; If lte estimate is punted sometime during a simulation, then this will flag it.
(defvar *lte-was-punted* nil)

(defvar *absolute-voltage-error* 0.005 "A reasonable range: 0.001 - 0.1 \(mV\).")
(defvar *absolute-voltage-error-internal* 0.005)
(defvar *relative-voltage-lte* 0.0)	; Ratio of maximum node voltage error / *ABSOLUTE-VOLTAGE-ERROR*.

(defvar *absolute-particle-error* 1e-3 "In terms of particle state [0-1].")
(defvar *absolute-particle-error-internal* 1e-3)
(defvar *relative-particle-lte* 0.0)	; Ratio of maximum particle state error / *ABSOLUTE-PARTICLE-ERROR*.

(defvar *absolute-conc-int-error* 0.001 "mM")
(defvar *absolute-conc-int-error-internal* 0.001)
(defvar *relative-conc-int-lte* 0.0)	; Ratio of maximum conc-int error / *ABSOLUTE-CONC-INT-ERROR*.

(defvar *debug-update-time-step-vars* nil)
(defvar *debug-voltage-error-step-change* nil)
(defvar *debug-particle-error* nil)
(defvar *debug-particle-error-step-change* nil)
(defvar *debug-conc-int-error* nil)
(defvar *debug-conc-int-error-step-change* nil)
(defvar *particle-w-max-error*)
(defvar *conc-int-w-max-error*)
(defvar *count-error-step-change* t)

(defvar *full-error-step-change* nil "Full documentation of error step changes.")

;; Lists of the times in which the error was determined from the appropriate criteria.
(defvar *particle-error-step-changes* '())
(defvar *particle-error-step-less-than-min-step* '())

(defvar *conc-int-error-step-changes* '())
(defvar *voltage-error-step-changes* '())

(defvar *relative-particle-error* 0.01)
(defvar *twice-relative-particle-error* 0.02)

(defvar *particles-are-working* nil)
(defvar *conc-particles-are-working* nil)

(defvar *estimate-particle-error-with-full-time-step* t)

(defvar *relative-conc-particle-error* 0.001)
(defvar *twice-relative-conc-particle-error* 0.002)

(defvar *particle-error-max-time-step* 0) ; for both conc and v-dep particles

(defvar *consider-particle-error* t "Consider LTE for particle states.")
(defvar *calculate-particle-error* t "Actually calculate LTE for particle states.")
(defvar *calculate-markov-particle-error* t "If *CALCULATE-PARTICLE-ERROR* is T, then also calculate LTE for Markov particle states.")

(defvar *consider-conc-particle-error* t "Consider LTE for particle states.")

(defvar *conc-int-error-max-time-step* 0) 

;(defvar *conc-int-error-factor* 1000.0 "Concentration integrator values may use a different error
;criteria than voltage nodes. This coefficient is not finalized.")

(defvar *relative-conc-int-error* 0.01)
(defvar *twice-relative-conc-int-error* 0.02)

(defvar *consider-conc-int-error* nil "Consider LTE for concentration ints as well, using the factor above for concentrations.") 
(defvar *calculate-conc-int-error* nil "Actually calculate it.")

;; In order to allow concentration ints to contribute to LTE, we must evaluate them in the DO-TIME-CONTROL inner loop.
(defvar *eval-conc-ints-in-inner-loop* nil "Evaluate conc-ints in inner loop")
					
(defvar *pick-time-step-fudge* 0.8 "Coefficient for choosing new time step based on lte estimate. Less than one to speed up time
step reduction when lte-ratio = 1")
(defvar *pick-time-step-fudge-internal* 0.8)

;; NOT USED NOW
;; LTE estimate is based on voltage vector magnitude, as opposed to the node with the largest voltage error at a given time step. 
;; (defvar *estimate-vector-error* nil)

;; ****************************************
;;
;; Soma Stuff
;;
;; ****************************************

(defvar *default-soma-diameter* 25.0)	;microns

(defvar *soma-shunt* 1e30 "Default value [ohms] for non-specific soma shunt. An arbitrary large value is used for negligble shunts.")		;ohms
(defvar *soma-shunt 1e30)		;ohms, for backward compatibility

;; ****************************************
;;
;; Concentration Integrator Variables
;;
;; ****************************************

;; Note that other relevant variables are defined elsewhere.

;; (defvar *implicit-conc-int-integration* t "When T, use implicit integration for :MULTI-SHELL integrators.")
(defvar *default-conc-int-type-diffusion-distance* 1.0e1 "microns")

;; ****************************************
;;
;; Synapse and Axon Global Variables
;;
;; ****************************************

(defvar *enable-axons* t "When nil, all axons are blocked.")

(defvar *include-events-in-element-documentation-code* nil "Enables storing of event times in element documentation code, for example for event synapses.")

(defvar *enable-event-generators* t
  "Event generators reduce evaluations for axons and synapses [VOLTAGE, LIGHT and LIGHT-EVENT] whose control parameters are identical for a given simulation.")

(defvar *setup-event-generators-and-followers* t
  "Enables the automatic assignment of event element sets at the beginning of every simulation, as long as
*USER-SPECIFIED-EVENT-ELEMENT-SETS* is NIL. This variable may be set to NIL after a simulation for more efficiency
in subsequent simulations, or may always be NIL as long as the function SETUP-ALL-EVENT-GENERATORS-AND-FOLLOWERS is
explicitly called when the circuit is setup or changed.")

(defvar *user-specified-event-element-sets* nil
  "If this flag is T, then the user has the responsibility to setup event generators and followers, e.g. with calls to
USER-SETUP-EVENT-GENERATORS-AND-FOLLOWERS or SETUP-ALL-EVENT-GENERATORS-AND-FOLLOWERS before a simulation.")

(defvar *maximum-synapse-printed-events* 5
  "Maximum number of event times or delays that will be explicitly printed when printing out a synapse's information.")

;; ****************************************
;;
;; Synapse and Light Stimuli Global Variables
;;
;; ****************************************

;; See also synapse.lisp

(defvar *enable-synapses* t "Enables evaluation of all synapses.")

(defvar *setup-tonic-synapses* t)
(defvar *setup-voltage-synapses* t)
(defvar *setup-light-synapses* t)
(defvar *setup-event-synapses* t)

(defvar *adjust-breakpoints-for-event-synapses* t "Before each simulation, add event synapse event times to the
*BREAKPOINT-LIST* to ensure catching the events.") 

;; When T, all event synapses of a given type on the same cell node are evaluated as one. In this case, plotting the synaptic
;; conductance or current will be incorrect.
(defvar *evaluate-lumped-event-synapses* nil)

(defvar *reuse-synapse-waveforms* nil)

;; ************* Light Stuff *************

(defvar *convert-light-response-to-events-for-each-synapse* nil
  "When event generators are used for light related synapses, this flag causes the light-response ->
event conversion to be done individually for each synapse.")

(defvar *enable-light-event-update* t "When T, renew :EVENT-TIMES slot for LIGHT-EVENT synapses.")

(defvar *constant-light-input-from-negative-infinity* t "Whatever the light input is calculated to
be at time 0, assume that this is the initial conditions \(otherwise, intial light conditions are
taken as zero state\)." ) 

;; Convolve all light driven synapses individually - otherwise if light input is same for two synapses, then each can reference the same wave.
(defvar *compute-all-light-inputs* nil)

;;;  *FAST-RF-BAR* This may be used when the stimulus is an infinite slit along the y-axis, and the bar sweeps
;;;  across the entire cell. When T, the spatial RF is integrated along the x-axis only.
(defvar *fast-rf-bar* nil)

;;;  *FAST-FULL-FIELD-SPOT This may be used when the stimulus is a full field spot, and therefore all the synapse
;;;  waveforms for each synapse type are identical.
(defvar *fast-full-field-spot* nil)

(defvar *enable-light* t "Let there be light.")

;; Variables to modify the relationship between the input light pattern and light-activated synapses.
(defvar *light-input-offset-distance* 0.0 "um")
(defvar *light-input-offset-angle* 0.0 "radians")
(defvar *light-input-delay* 0.0 "Light input delay between light event at offset location and ynaptic response, in milliseconds.") 

(defvar *light-origin-array* (make-array 1 :element-type 'single-float))

;; Light stimulus related parameters

(defvar *light-speed* 0.0 "Microns per millisecond")
(defvar *bar-width* 0.0 "Microns")
(defvar *bar-length* 0.0 "Microns")
;; * (* 90 2.0 (COERCE user::PI 'SINGLE-FLOAT) (/ 1.0 360))
;; 1.5707964
(defvar *light-theta* 1.5707964 "Radians")

(defvar *light-direction* T "T / nil => movement is in the direction of / opposite to *light-theta*.")
(defvar *light-start-position-x* 0.0 "Point of center of stimulus at *motion-start-time* in microns")
(defvar *light-start-position-y* 0.0 "Point of center of stimulus at *motion-start-time* in microns")
(defvar *grating-temporal-period* 1000000.0 "Milliseconds")
(defvar *grating-spatial-period* 1000000.0 "Microns")
(defvar *use-aperture* nil "Consider aperture.")
(defvar *aperture-radius* 300.0 "Microns")
(defvar *aperture-center-x* 0.0 "Microns")
(defvar *aperture-center-y* 0.0 "Microns")

#|
;; problems with declaring single-float to *LIGHT-THETA* when compiled as an arg to cos or sin with ultra 18a lisp
(proclaim '(single-float *light-theta* *bar-width* *bar-length* *light-speed* *light-start-position-x* *light-start-position-y*))
|#
;; Apparent motion stimulus.
(defvar *bar-a-width* 0.0 "microns")
(defvar *bar-a-length* 0.0 "microns")
(defvar *bar-a-intensity* 1.0)
(defvar *bar-a-start-time* 0.0 "milliseconds")
(defvar *bar-a-stop-time* 0.0 "milliseconds")
(defvar *bar-a-position-x* 0.0 "microns")
(defvar *bar-a-position-y* 0.0 "microns")

(defvar *bar-b-width* 0.0 "microns")
(defvar *bar-b-length* 0.0 "microns")
(defvar *bar-b-intensity* 1.0)
(defvar *bar-b-start-time* 0.0 "milliseconds")
(defvar *bar-b-stop-time* 0.0 "milliseconds")
(defvar *bar-b-position-x* 0.0 "microns")
(defvar *bar-b-position-y* 0.0 "microns")

(defvar *syn-rf-normalized-amp* nil) ;; If T then synaptic spatial receptive field gaussian amplitude is normalized, else area (volume) is normalized.

(defvar *light-input-waveform* (make-array 1 :element-type 'single-float))
(defvar *first-non-zero-element-light-input-waveform* 0)
(defvar *last-non-zero-element-light-input-waveform* 0)

;; (proclaim '((type vector single-float) *light-input-waveform*))
	  
;; Format for gaussian spatial receptive field parameter list:
;;
;;       (list gaussian light-input-offset-distance light-input-offset-angle sigma-x sigma-y normalized-amp)

(defvars
  (*synapse-g-leak-ratio* 1.0)
  (*light-stimulus-start-time* 0.0)
  (*light-stimulus-stop-time* 100000.0)
  (*light-stimulus-strength* 1.0) 
  (*light-background* 0.0))

(defvar *light-stimulus-types* '(:SPOT
				 :MOVING-SPOT
				 :ANNULUS
				 :ON-SPOT
				 :OFF-SPOT
				 :ON-MOVING-BAR
				 :OFF-MOVING-BAR 
				 :APPARENT-MOTION :ON-BAR :OFF-BAR 
				 :MOVING-BAR)
  ":MOVING-BAR is equivalent to :ON-MOVING-BAR")

;; Future types: :moving-bar-grating :moving-sine-grating :reversing-bar

(defvar *light-stimulus* nil "Can take a value out of *LIGHT-STIMULUS-TYPES*.")

(defvar *spot-outside-diameter* 0.0)
(defvar *spot-inside-diameter* 0.0)

(defvar *maximum-synaptic-jitter* 20.0)
(defvar *jitter-light-synapse* nil)

(defvar *gaussian-rf-width-in-sigmas* 4)

(defvar *light-stimulus-plane* :XY ":XY for retina, :XZ for radial mount cortical cells.")

;; ****************************************
;;
;; Data Analysis
;;
;; ****************************************

(defvar *print-linear-analysis* nil)
(defvar *print-nonlinear-analysis* nil)
(defvar *print-analysis* t)
(defvar *analysis-nodes* nil)
(defvar *analysis-nodes-structures* nil)

(defvar *print-synapse-event-times* nil)
(defvar *print-synapse-total-events* t)
(defvar *count-active-and-triggered-synapses* t "When non-NIL, the function COUNT-ACTIVE-SYNAPSES, which normally prints out info
at the end of each simulation, also prints the number of synapses actually fired.")

(defvar *print-axon-spike-times* Nil)

(defvar *average-integrals* t)
(defvar  *x-integrate-min* nil)
(defvar *x-integrate-max* nil)

;; ****************************************
;;
;; Random Numbers
;;
;; ****************************************

(defvar *always-intialize-random-gen* nil "Forces a call to GET-REFERENCE-RANDOM-STATE at various places.")

;; ****************************************
;;
;; Type Proclamations
;;
;; ****************************************

(proclaim '(double-float
	    *user-min-step-double*
	    *maximum-particle-error-numerator*))


(proclaim '(single-float
	    most-positive-fixnum-float-version
	    *Temperature*
	    *mrt* 
            zero 

	    *pick-time-step-fudge-internal*
	    
	    *absolute-voltage-error-internal*  *relative-voltage-lte*
	    *absolute-particle-error-internal* *relative-particle-lte*
	    *absolute-conc-int-error-internal* *relative-conc-int-lte*

	    *relative-particle-error* *twice-relative-particle-error*
	    *relative-conc-int-error* *twice-relative-conc-int-error*))

(proclaim '(type fixnum
	    *min-step* *max-step* *time-step* *last-time-step* *integer-time*
	    *particle-error-max-time-step* *conc-int-error-max-time-step*
	    *int-user-stop-time* *sim-time-n+1* *sim-time-n* *sim-time-n-1* *sim-time-n-2*
	    *vsource-array-length* *isource-array-length*)) 

(proclaim '(type (unsigned-byte 32)
	    *num-nodes* *num-unknowns*
	    *start-time* *stop-time* 
	    *total-num-iterations* *total-num-time-points* *fp-exponent-large*
	    *fp-mantissa-large* *fp-exponent-small* *fp-mantissa-small* ))

(proclaim '(type (simple-array single-float (*))
	    *diag* *lower-diag* *upper-diag* *v-at-half-step* *rhs*))

(proclaim '(type (simple-array double-float (*))
	    *diag-double* *lower-diag-double* *upper-diag-double* *v-at-half-step-double* *rhs-double*))

(proclaim '(type list *breakpoint-list*))









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


;;; SYS Source file: structures.lisp
;;;
;;; All the circuit element structure definitions, with some slot macros.
;;;

(in-package "SURF-HIPPO")

; 4-26-92 lbg CMUCL complains when :conc-name defstruct option has no arguments. According to GLSteele, we don't want this option
; at all, since the default for the accessor function names to prefix the structure name onto the slot keyword. This has now been
; removed from all the other system files.  (defstruct (model :conc-name)

; 6-17-92 lbg CMUCL complains when nil is used as default for slot declared as a :type for which nil is not a member. CLTL2 sez
; that the use of type is implementation dependent - it is unknown whether or not our use of :type in all the structures is
; helping things very much. I have removed all the :type declarations in similar slot options in the other system files.

(defstruct model
  name
  hash-table				; Holds all the instances of this model
  parameter-type-library

  ;; A list of global-var-list-of-structures, such as *PLOT-CHANNEL-CURRENTS-STRUCTURES* and the like, that are  processed by the model's :SAVE-OUTPUT-DATA-ROUTINE
  output-data-structure-variables
  
  ;; True or false depending on whether there is a non-nil value in the :OUTPUT-DATA-STRUCTURE-VARIABLES list.
  output-data-enabled

  DATA-TYPES-AND-ACCESS-INFO

  ;; For example, for the SOMA model -
  ;;
  ;; ((VOLTAGE (UNITS "mV") (DATA-PARAM-KEY :VOLTAGE-DATA)
  ;;  (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DATA)
  ;;  (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DATA)
  ;;  (CURRENT-VALUE-FUNCTION RECORDED-ELEMENT-VOLTAGE)
  ;;  (SAVED-DATA-FUNCTION SOMA-VOLTAGE-DATA))
  ;; ((VOLTAGE-DERIVATIVE DVDT NODE-VOLTAGE-DERIVATIVE) (UNITS "mV/ms")
  ;;  (DATA-PARAM-KEY :VOLTAGE-DERIVATIVE-DATA)
  ;;  (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DERIVATIVE-DATA)
  ;;  (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DERIVATIVE-DATA)
  ;;  (CURRENT-VALUE-FUNCTION ELEMENT-DVDT)
  ;;  (SAVED-DATA-FUNCTION SOMA-VOLTAGE-DERIVATIVE-DATA))
  ;;  ...)
  
  ;; Note that the CAR of each entry can be either a single symbol or a list of synonyms. 

  ;; For models of element types, this holds the type of the child elements (e.g 'channel for the 'channel-type model). 
  child-structure-type
  ;; The inverse reference of above (e.g 'channel-type for the 'channel model). 
  parent-structure-type

  top-pointer-symbol				; Contains the global variable symbol for the most recently created instance of the model
  save-output-data-routine
  edit-routine
  eval-routine
  print-routine
  short-print-routine
  document-routine
  create-routine)

;; ********************************************************************
;;     Node
;; ********************************************************************

;;; 4/8/94 node-floats defstruct was fairly consistently choking on system compile. Adding this
;;; seems to help (???). someday this should be figerred out.
(defstruct (dummy-floats (:type (vector single-float)))
  (nuthin 0.0 :type single-float))

(defstruct (dummy-fixnums (:type (vector fixnum)))
  (nuthin 0   :type fixnum))		

(defstruct (dummy-double-floats (:type (vector double-float)))
  (nuthin 0.0d0	:type double-float))

(defstruct (node-double-floats (:type (vector double-float)))
  (voltage-n+1 0.0d0	:type double-float) ; voltage at t(n+1) (millivolts)
  (voltage-n   0.0d0	:type double-float) ; voltage at t(n) (millivolts)
  (voltage-n-1 0.0d0	:type double-float) ; voltage at t(n-1) (millivolts)
  (jacobian 0.0d0	:type double-float)
  (const-jacobian 0.0d0	:type double-float) ; The time-invariant component (Seg G-AXIAL, Seg/Soma G-LEAK).
  (alpha-charge 0.0d0	:type double-float) ; Speed things w/Hines method by multiplying by alpha only once.
  (current 0.0d0	:type double-float) ; nanoamps
  (const-current 0.0d0	:type double-float) ; The time-invariant component (Seg G-AXIAL * v-leak), nanoamps
  (dvdt-n 0.0d0 :type double-float)	; mV/ms
  (dvdt-n-1 0.0d0 :type double-float)	; mV/ms
  )

(deftype node-df-array () '(simple-array double-float (10)))

(defmacro node-double-floats-aref-voltage-n+1 (node-dfs) `(aref (the node-df-array ,node-dfs) 0))
(defmacro node-double-floats-aref-voltage-n (node-dfs) `(aref (the node-df-array ,node-dfs) 1))
(defmacro node-double-floats-aref-voltage-n-1 (node-dfs) `(aref (the node-df-array ,node-dfs) 2))
(defmacro node-double-floats-aref-jacobian (node-dfs) `(aref (the node-df-array ,node-dfs) 3))
(defmacro node-double-floats-aref-const-jacobian (node-dfs) `(aref (the node-df-array ,node-dfs) 4))
(defmacro node-double-floats-aref-alpha-charge (node-dfs) `(aref (the node-df-array ,node-dfs) 5))
(defmacro node-double-floats-aref-current (node-dfs) `(aref (the node-df-array ,node-dfs) 6))
(defmacro node-double-floats-aref-const-current (node-dfs) `(aref (the node-df-array ,node-dfs) 7))
(defmacro node-double-floats-aref-dvdt-n (node-dfs) `(aref (the node-df-array ,node-dfs) 8))
(defmacro node-double-floats-aref-dvdt-n-1 (node-dfs) `(aref (the node-df-array ,node-dfs) 9))

(defmacro node-aref-voltage-n+1 (node-dfs) `(node-double-floats-aref-voltage-n+1 ,node-dfs))
(defmacro node-aref-voltage-n (node-dfs) `(node-double-floats-aref-voltage-n ,node-dfs))
(defmacro node-aref-voltage-n-1 (node-dfs) `(node-double-floats-aref-voltage-n-1 ,node-dfs))
(defmacro node-aref-jacobian (node-dfs) `(node-double-floats-aref-jacobian ,node-dfs))
(defmacro node-aref-const-jacobian (node-dfs) `(node-double-floats-aref-const-jacobian ,node-dfs))
(defmacro node-aref-alpha-charge (node-dfs) `(node-double-floats-aref-alpha-charge ,node-dfs))
(defmacro node-aref-current (node-dfs) `(node-double-floats-aref-current ,node-dfs))
(defmacro node-aref-const-current (node-dfs) `(node-double-floats-aref-const-current ,node-dfs))
(defmacro node-aref-dvdt-n (node-dfs) `(node-double-floats-aref-dvdt-n ,node-dfs))
(defmacro node-aref-dvdt-n-1 (node-dfs) `(node-double-floats-aref-dvdt-n-1 ,node-dfs))

(defmacro node-voltage-n+1 (node) `(node-aref-voltage-n+1 (node-double-floats ,node)))
(defmacro node-voltage-n (node) `(node-aref-voltage-n (node-double-floats ,node)))
(defmacro node-voltage-n-1-double (node) `(node-aref-voltage-n-1 (node-double-floats ,node)))
(defmacro node-jacobian (node) `(node-aref-jacobian (node-double-floats ,node)))
(defmacro node-const-jacobian (node) `(node-aref-const-jacobian (node-double-floats ,node)))
(defmacro node-alpha-charge (node) `(node-aref-alpha-charge (node-double-floats ,node)))
(defmacro node-current (node) `(node-aref-current (node-double-floats ,node)))
(defmacro node-const-current (node) `(node-aref-const-current (node-double-floats ,node)))
(defmacro node-dvdt-n (node) `(node-aref-dvdt-n (node-double-floats ,node)))
(defmacro node-dvdt-n-1 (node) `(node-aref-dvdt-n-1 (node-double-floats ,node)))

;; The prt-v-index is derived (from UPDATE-NODES-V-INDEX) by an estimate of the voltage at time (t~) halfway
;; between midpoints of last step and current step. Depending on size of these two steps, V(t~) is either an
;; interpolation between or an extrapolation beyond V(t-n-1) and V(t-n). This estimate is then translated to an
;; index for the alpha and beta rate constant arrays.

;; At the beginning of each iteration of HINES-STEP the function UPDATE-NODES-V-INDEX steps over
;; all the nodes, and if the :HAS-V-DEP-ELEMENT slot is T, then the :PRT-V-INDEX is updated.
  
(defstruct (node-fixnums (:type (vector fixnum)))
  (prt-v-index 0 :type fixnum))

(deftype node-fixnum-array () '(simple-array fixnum (1)))

(defmacro node-fixnum-aref-prt-v-index (node-fixnums) `(aref (the (simple-array fixnum *) ,node-fixnums) 0))

(defstruct (node (:print-function (lambda (elt stream depth)
				    (declare (ignore depth))
				    (format stream "<Node ~a>" (element-name elt)))))
  name
  cell

  ;; If this node is a physical cell node, i.e. segment or soma. If NIL, then relative-location is irrelevant.
  (is-physical-cell-node nil :type boolean)

  (relative-location '() :type list)	; (X Y Z) relative to the cell origin.
  (absolute-location '() :type list)	; (X Y Z) in brain coordinates, equal to sum of :CELL's :ORIGIN and :RELATIVE-LOCATION.
  (elements '()	:type list)		; A list of elements associated with this node
  
  (has-v-dep-element nil :type boolean)	; So we know which nodes need to have their prt-v-index updated, and to make
					; an array of nodes with membrane elements for which we will use to estimate
					; the maximum LTE. 

  (index -1 :type fixnum)		; For ordering the node equations, set by ORDER-NODES-FROM-HINES-BRANCH-LIST.

  (prt-v-index-rem 0.0 :type single-float) ; This is used if we are interpolating particle voltage-dep functions.

  (has-ideal-voltage-source nil :type boolean)
  
  parameters
  
  (fixnums (make-node-fixnums) :type element-fixnums)
  (double-floats (make-node-double-floats) :type element-double-floats))

(defmacro node-prt-v-index (node) `(node-fixnum-aref-prt-v-index (node-fixnums ,node)))

;; ********************************************************************
;;     Soma
;; ********************************************************************

(defstruct (soma-guts (:type (vector double-float)))
  (capacitance 0.0d0 :type double-float) ; nF
  (g-leak*v-leak 0.0d0 :type double-float)
  (g-leak 0.0d0 :type double-float)	;microsiemens
  )

(deftype soma-df-array () '(simple-array double-float (3)))

(defmacro soma-aref-capacitance (soma-dfs) `(aref (the soma-df-array ,soma-dfs) 0))
(defmacro soma-aref-g-leak*v-leak (soma-dfs) `(aref (the soma-df-array ,soma-dfs) 1))
(defmacro soma-aref-g-leak (soma-dfs) `(aref (the soma-df-array ,soma-dfs) 2))

(defmacro soma-capacitance (soma) "Membrane capacitance in nF." `(soma-aref-capacitance (soma-guts ,soma)))
(defmacro soma-g-leak*v-leak (soma) `(soma-aref-g-leak*v-leak (soma-guts ,soma)))
(defmacro soma-g-leak (soma) "Membrane leak conductance in uS." `(soma-aref-g-leak (soma-guts ,soma)))

(defstruct (soma (:print-function (lambda (elt stream depth) (declare (ignore depth)) (format stream "<Soma ~a>" (element-name elt)))))
  name
  node

  ;; When T, some parameters are inherited from the cell-type when the cell-type parameters are
  ;; edited - otherwise, the soma parameters are changed only by reference to specific somas.
  (inherit-parameters-from-type t)
  
  (diameter zero :type single-float)	;microns
  (include-shunt nil :type boolean)
  (g-shunt zero :type single-float)	;microsiemans
  (current 0.0 :type single-float)
  parameters
  (guts (make-soma-guts)  :type soma-df-array))

(setf (documentation 'soma-diameter 'function) "microns (requires explicit structure)")
(setf (documentation 'soma-g-shunt 'function) "Non-specific shunt in microsiemans (requires explicit structure)")
(setf (documentation 'soma-g-leak 'function) "Leak conductance in microsiemans (requires explicit structure)")
(setf (documentation 'soma-capacitance 'function) "Membrane capacitance in nF (requires explicit structure)")
(setf (documentation 'soma-inherit-parameters-from-type 'function) "When T, membrane parameters are inherited from the cell-type (requires explicit structure).")

(defmacro soma-cell (soma) `(node-cell (soma-node ,soma)))

;; ********************************************************************
;;     Segment
;; ********************************************************************

(defstruct (segment-guts (:type (vector double-float)))
  (capacitance 0.0d0 :type double-float) ; nF
  (g-leak*v-leak 0.0d0 :type double-float)
  (g-axial 0.0d0 :type double-float)	;microsiemens
  (g-leak 0.0d0 :type double-float)	;microsiemens
  )

(deftype segment-df-array () '(simple-array double-float (4)))

(defmacro segment-aref-capacitance (segment-dfs) `(aref (the segment-df-array ,segment-dfs) 0))
(defmacro segment-aref-g-leak*v-leak (segment-dfs) `(aref (the segment-df-array ,segment-dfs) 1))
(defmacro segment-aref-g-axial (segment-dfs) `(aref (the segment-df-array ,segment-dfs) 2))
(defmacro segment-aref-g-leak (segment-dfs) `(aref (the segment-df-array ,segment-dfs) 3))

(defmacro segment-capacitance (segment) "Membrane capacitance in nF." `(segment-aref-capacitance (segment-guts ,segment)))
(defmacro segment-g-leak*v-leak (segment) `(segment-aref-g-leak*v-leak (segment-guts ,segment)))
(defmacro segment-g-axial (segment) "Axial conductance in uS." `(segment-aref-g-axial (segment-guts ,segment)))
(defmacro segment-g-leak (segment) "Membrane leak conductance in uS." `(segment-aref-g-leak (segment-guts ,segment)))

(defstruct (segment (:print-function (lambda (seg stream depth)
				       (declare (ignore depth))
				       (format stream "<Segment ~a: prox node ~a>" (segment-name seg) (node-name (segment-node-1 seg))))))
     
  name					; Segment name is same a distal node name.
  
					;                             g-axial
  node-1				; Proximal node      Prox o---/\/\/\-----+---o Distal
  node-2				; Distal node                            |
					;                                   memb-elements	
					;                                        |
					;                                       Gnd

  dummy-proximal-node-location		; For segments that connect to the soma, this node just gives the physical proximal
					; endpoint location, relative to the cell soma. Also for segments whose physical proximal
					; node location does not match with another element's location, so that the electrical
					; connection is not exactly congruent with the physical location. 

  (length zero :type single-float)	; microns
  (diameter zero :type single-float)	; microns
  (theta zero :type single-float)	; Elevation angle relative to proximal branch orientation.
  (phi zero :type single-float)		; Azimuth angle relative to proximal branch orientation.

  (distance-to-soma zero :type single-float) ; Distance to soma along the direct path via the tree.

  ;; When T, some parameters are inherited from the cell-type when the cell-type parameters are
  ;; edited - otherwise, the segment parameters are changed only by reference to specific segments.

  (inherit-parameters-from-type t)	

  (branch-node-index nil :type (or null fixnum)) ; If this segment is connected to a Hines node, then this is the index for that node. 

  mat-12-point				; core-off-diag
  mat-21-point				; core-off-diag

  ;; Some possible entries in the :PARAMETERS list:
  
  ;; For consolidating two histological segments into one, in order to maintain the constraints that:
  ;;
  ;;   a] the total axial series resistivity is conserved
  ;;  
  ;;   b] the total membrane admittance is conserved
  ;;  
  ;;   c] the end points of the new segment correspond to the end points of the original two segments,
  ;;
  ;; there must be an additional parameter, which we are choosing to be a coefficient for the
  ;; intracellular (cytoplasmic) resistivity Ri.
    
  ;; (... (ri-coefficient . val) ...)


  ;; The following parameters are inherited from the cell-type when the segment is created. 
  ;;  (... (rm . val ) ...) Ohms Square Centimeter
  ;;  (... (cm . val) ...)  uF / Square Centimeter
  ;;  (... (ri . val) ...) ; Ohm Centimeter
  ;;  (... (v-leak . val) ...) millivolts
  

  ;; Other possible entries in this list:
  ;;
  ;;  (... (type . axon) ...)
  ;; 
  ;; Or, for retinal cells: 1 to N for dendrites in the inner plexiform layer. NIL if not in IPL.
  ;;  (... (ipl-stratum . nil) ...)

  ;; A simple method for including spines is to adjust segment capacitance and resistivity by factor related to membrane area of spines. 
  ;;  (... (membrane-area-coefficient . 2.0) ...)

  parameters

  (guts (make-segment-guts)  :type segment-df-array))

(setf (documentation 'segment-length 'function) "microns (requires explicit structure)")
(setf (documentation 'segment-diameter 'function) "microns (requires explicit structure)")
(setf (documentation 'segment-theta 'function) "Elevation angle relative to proximal branch orientation, in radians (requires explicit structure).")
(setf (documentation 'segment-phi 'function) "Azimuth angle relative to proximal branch orientation, in radians (requires explicit structure).")

(defmacro segment-cell (segment) `(node-cell (segment-node-2 ,segment)))

;; ********************************************************************
;;    Isource
;; ********************************************************************

(defstruct (isource-type (:print-function (lambda (elt stream depth)
					     (declare (ignore depth))
					     (format stream "<Current Source Type ~a>" (element-name elt)))))
  name
  (class :AUTONOMOUS)			; E.g. :AUTONOMOUS, :DRIVEN

  ;; For :DRIVEN sources.
  activation-function			; This function should have one argument, the source, and return a number.
  rf-function				; For :DRIVEN sources driven by light stimulus.
  

  ;; *********************************************************************************************************

  first-element			; The first source of this type
  last-element			; The last of this type
  parameters
  )

(defstruct (isource-floats (:type (vector single-float)))
  (current 0.0 :type single-float))

(defmacro isource-current (isrc) `(aref (the (simple-array single-float *) (isource-floats ,isrc)) 0)) ; nA

(defstruct (isource (:print-function (lambda (elt stream depth)
				       (declare (ignore depth))
				       (format stream "<Current Source ~a: type ~a>" (isource-name elt) (isource-type-name (isource-type elt))))))
  name
  type

  cell-element				; What the source is a part of (soma or segment)
  control-element			; For :DRIVEN sources.

  next-element				; The next source of this type
  
  node-1
  node-2
  (blocked nil :type boolean)		; Source is only considered when this is NIL
  (use-pulse-list t)			; Source uses :pwl-list. When NIL, use waveform-array.
  
  (resistance 0.0 :type single-float)	; Series resistance, in Mohms. The voltage measured at this
					; node will be offset by the IR drop across this resistance.

  ;; For :AUTONOMOUS isources:
  ;;
  ;; The piece-wise-linear list contains the breakpoints of the pwl approximation, as follows:
  ;;    `((bp-time1  bp-time2  bp-time3  ... bp-timeN)
  ;;      (bp-value1 bp-value2 bp-value3 ... bp-valueN))
  ;; This is a 2D list ([2,N], where N is the number of breakpoints), with time points in the first sublist and source values in the second sublist.
  (pwl-list nil)			; for PWL sources

  (waveform-array nil)			; for sources driven by some waveform
  (waveform-length 0)
  (waveform-time-interval-inverse 5.0 :type single-float)
  (waveform-time-interval-mrt 1 :type fixnum)

  
  (period zero	:type single-float)	; When PERIOD is not = 0, then the waveform for this source is repeated after every PERIOD milliseconds.
  (delay zero	:type single-float)
  (node-1-pointp nil	:type boolean)	; zero if constant, one if node
  (node-2-pointp nil	:type boolean)	; zero if constant, one if node

  parameters				; Holds information such as reference pulse-list or waveform args

  (floats (make-isource-floats) :type element-floats))

(setf (documentation 'isource-resistance 'function)
      "Series resistance, in Mohms. The voltage measured at this node will be offset by the IR drop across this resistance.")

;; ********************************************************************
;;    Vsource
;; ********************************************************************

(defstruct (vsource-type (:print-function (lambda (elt stream depth)
					     (declare (ignore depth))
					     (format stream "<Voltage Source Type ~a>" (element-name elt)))))
  name
  (class :AUTONOMOUS)			; E.g. :AUTONOMOUS, :DRIVEN

  ;; For :DRIVEN sources.
  activation-function			; This function should have one argument, the controlling element,
					; and return a number.
  rf-function				; For :DRIVEN sources driven by light stimulus.

  ;; *********************************************************************************************************

  first-element			; The first source of this type
  last-element			; The last of this type
  parameters
  )


(defstruct (vsource (:print-function (lambda (elt stream depth)
				       (declare (ignore depth))
				       ; (format stream "<Voltage Source ~a: type ~a>" (vsource-name elt) (vsource-type-name (vsource-type elt)))
				       (format stream "<Voltage Source ~a>" (element-name elt)))))
  name
  type

  cell-element				; What the source is a part of (soma or segment)
  control-element			; For :DRIVEN sources.

  next-element				; The next source of this type

  node					; node 2 is always ground

  (blocked nil :type boolean)		; Source is only considered when this is NIL
  (use-pulse-list t)			; Source uses :pwl-list. When NIL, use waveform-array.
  
  ;; For :AUTONOMOUS vsources:
  ;;
  ;; The piece-wise-linear list contains the breakpoints of the pwl approximation, as follows:
  ;;    `((bp-time1  bp-time2  bp-time3  ... bp-timeN)
  ;;      (bp-value1 bp-value2 bp-value3 ... bp-valueN))
  ;; This is a 2D list ([2,N], where N is the number of breakpoints), with time points in the first sublist and source values in the second sublist.
  (pwl-list nil)			; for PWL sources

  (waveform-array nil)			; for sources driven by some waveform
  (waveform-time-interval-inverse 5.0 :type single-float)
  (waveform-time-interval-mrt 1 :type fixnum)

  (resistance 0.001 :type single-float)	; Series resistance, in Mohms, for the non-ideal vclamp. Must be >0.
  
  (function-list)			; For controlled vsources. not used now.
  
  (period zero :type single-float)	; When PERIOD is not = 0, then the waveform for this source is repeated after every PERIOD milliseconds.
  (delay zero :type single-float)	; The PWL waveform is applied after DELAY milliseconds
  
  (current zero :type single-float)

  ;; These refer to the command voltages
  (last-voltage zero :type single-float)
  (voltage zero	:type single-float)

  (adjacent-nodes-and-g-axials '())	; This list of '((adjacent-seg-node connecting-g-axial) ...) is set at beginning of
					; each simulation for ideal voltage sources, and is subsequently  used for the
					; ideal voltage clamp evaluation algorithm. 
  
  parameters				; Holds information such as reference pulse-list or waveform args
  )

(setf (documentation 'vsource-resistance 'function) "Series resistance, in Mohms, for the non-ideal vsource. Must be >0.")

;; ********************************************************************
;; SYSTEM-OF-DIFFERENTIAL-EQUATIONS Structure (implementation not complete)
;; ********************************************************************

(defstruct system-of-differential-equations

  ;; ***********************************
  ;; Used for example with :GENERAL type conc-ints, where the states can include concentration
  ;; compartments, pumps, buffers... 

  ;; The number N of state variables
  number-of-states

  ;; NxN array of double-floats that define the associated differential equation.

  coefficients-array

  ;; ***********************************

)

;; ********************************************************************
;;    Membrane Element IV Parameters - for channel and synapse types
;; ********************************************************************

(defstruct membrane-element-type-iv

  ;; ***************************************************
  ;; Gbar (:OHMIC models) or Permeability (:CONSTANT-FIELD models) Parameters
  ;; ***************************************************

  (relation :ohmic)			; The interpretation and units for the following slots depend on the value of :RELATION as follows:

					;  Slot                                :RELATION
					;                           :OHMIC                   :CONSTANT-FIELD
					;----------------------------------------------------------------------
					; :RELATION-SOURCE
					;
					;    :ABSOLUTE              Refer to :REFERENCE slot
					;    :DENSITY               Refer to :DENSITY slot
					;
					; :REFERENCE        Fixed Conductance (uS)        Fixed Permeability (cm3/s)
					; :DENSITY           Conductance (pS/um2)        Permeability (1.0e-6cm3/s/um2)
					;
					; Note: pS/um2 = 0.1mS/cm2

  (source :density)			; :ABSOLUTE or :DENSITY

  (reference 0.0 :type single-float)	; If :SOURCE is :ABSOLUTE, then this channel or synapse type has a fixed maximum
					; conductance or permeability in units given above, independent of the cell element that
					; it is a part of. 

  (density 0.0 :type single-float)	; Otherwise, :DENSITY, in units given above, is used to determine the maximum
					; conductance or permeability for a specific channel or synapse's GBAR or PBAR.

  ;; When :INHERIT-PARAMETERS-FROM-TYPE is T, the above parameters are inherited from the channel-type or synapse-type when the
  ;; type parameters are edited - otherwise, the channel or synapse parameters are changed only by reference to specific channels or synapses.
  (inherit-parameters-from-type t)

  ;; ***************************************************
  ;; Reversal Potential - Ion Permeabilities
  ;; ***************************************************

  (use-defined-e-rev nil :type boolean)	; If T, or if the :ION-PERMEABILITIES is NIL, the :E-REV of each channel or synapse
					; instance refers to the channel or synapse type :E-REV slot. Otherwise, the :E-REV of a
					; channel or synapse is calculated from the type :ION-PERMEABILITIES slot and the
					; appropriate :E-species slots of the cell type of the channel or synapse's cell.
  
  (e-rev 0.0 :type single-float)	; mV, when there is a fixed e-rev.

  (variable-e-rev nil :type boolean)	; When T, e-rev for each channel or synapse is referenced from the channel or synapse itself, not the type.

  (ion-permeabilities nil :type list)	; E.g. '((K 0.7) (CA 0.2) (NA 0.1)) The permeabilities should add to 1.0. These values are
					; used to determine what proportion of the total ionic current may be assigned to a
					; concentration integrator of a given ionic species, and in computing the reversal
					; potential when :USE-DEFINED-E-REV is NIL. 

  (conc-int-type-params nil :type list) ; For each ion that passes through this channel or synapse type, an entry for a
					; concentration integrator type, for the shell (1 and/or 2) and the proportion of the
					; current associated with the shell, e.g.: 

					; '((CA-IN (1 0.7) (2 0.24)) (K-EXTRA (1 1)))
  
  (blocked nil :type boolean)		; When true, block channel or synapse type completely
  
  ;; ***************************************************
  ;; Temperature Related (ionic conductance)
  ;; ***************************************************
  
  (Q10 1.0  :type single-float)		; For gbar values
  (reference-temp 27.0 :type single-float) ; Reference temperature for gbar parameters (Centigrade)
  )

(setf (documentation 'membrane-element-type-iv-reference 'function) " uS or cm3/s")
(setf (documentation 'membrane-element-type-iv-density 'function) " pS/um2 or 1.0e-6cm3/s/um2")

;; ********************************************************************
;;    Membrane Element IV Values - for channels and synapses
;; ********************************************************************

(defstruct (membrane-element-iv-values (:type (vector double-float)))
  (conductance/permeability 0.0d0 :type double-float) ; microsiemans or cm3/s, depending on membrane element type
  (gbar/pbar 0.0d0 :type double-float)	; microsiemans or cm3/s, depending on membrane element type
  (e-rev 0.0d0 :type double-float)	; millivolts
  (current 0.0d0 :type double-float)	; nA
  ;; This is an absolute value, before temperature correction, that is referenced only when the membrane-element
  ;; :INHERIT-PARAMETERS-FROM-TYPE slot is NIL. In this case, the units are determined by the associated element type :RELATION
  ;; (:OHMIC or :CONSTANT-FIELD). 
  (gbar/pbar-reference-value 0.0d0 :type double-float) ; microsiemans or cm3/s, depending on membrane element type
  )

(deftype memb-elt-iv-values-array () '(simple-array double-float (5)))

(defmacro memb-elt-iv-conductance (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 0))
(defmacro memb-elt-iv-permeability (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 0))
(defmacro memb-elt-iv-gbar (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 1))
(defmacro memb-elt-iv-pbar (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 1))
(defmacro memb-elt-iv-e-rev (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 2))
(defmacro memb-elt-iv-current (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 3))
(defmacro memb-elt-iv-gbar/perm-reference-value (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 4))
(defmacro memb-elt-iv-gbar-reference-value (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 4))
(defmacro memb-elt-iv-pbar-reference-value (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 4))
(defmacro memb-elt-iv-reference-value (membrane-element-iv-values) `(aref (the memb-elt-iv-values-array ,membrane-element-iv-values) 4))

;; ********************************************************************
;;    Channel Type
;; ********************************************************************

(defstruct (channel-type (:print-function (lambda (type stream depth)
					    (declare (ignore depth))
					    (format stream "<Channel Type ~a>" (channel-type-name type)))))
  name
  
  (iv-parameters (make-membrane-element-type-iv))

  first-element				; The first channel of this type
  last-element				; The last channel of this type
  
  (particle-types-and-powers nil :type list) ; E.g. `((<Particle Type KM-HOFFMAN-ETAL-97-DISTAL> . 4) (<Particle Type KH-HOFFMAN-ETAL-97> . 1))
  (conc-particle-types-and-powers nil :type list)

  parameters)


;; ********************************************************************
;;    Channel
;; ********************************************************************


(defstruct (channel (:print-function (lambda (ch stream depth)
				       (declare (ignore depth))
				       (format stream "<Channel ~a: type ~a>" (channel-name ch) (channel-type-name (channel-type ch))))))
  name
  type
  cell-element				; What the channel is a part of (soma or segment)

  next-element				; The next channel of this type
  (blocked nil :type boolean)		; When true, block completely

  ;; Concentration integrators associated with this channel, with the shells and appropriate coefficients for calculating the e-rev, e.g:
  ;; '((<soma-1-CA-IN> 1 0.6) (<soma-1-CA-IN> 2 0.2) (<soma-1-K-EXTRA> 1 0.2))
  conc-ints-params

  pre-synaptic-element			; When the voltage controlling this channel is referenced from a node different from the
					; channel's cell-element different. 

  ;; When T, some parameters are inherited from the channel-type when the channel-type parameters are edited - otherwise, the
  ;; channel parameters are changed only by reference to specific channels. If :INHERIT-PARAMETERS-FROM-TYPE is NIL, then by
  ;; default the IV-RELATION-REFERENCE for this element is in absolute terms. 
  (inherit-parameters-from-type t)

  particles
  conc-particles

  parameters
  (iv-values (make-membrane-element-iv-values) :type memb-elt-iv-values-array))

(defmacro channel-node (ch)
  `(let ((cell-elt (channel-cell-element ,ch)))
     (typecase cell-elt
       (soma (soma-node cell-elt))
       (segment (segment-node-2 cell-elt)))))

;; ********************************************************************
;;    Particle
;; ********************************************************************


;; old figure (pre 93):
;; Use of look-up tables for the voltage-dependent rate constants (actually, for the time constant
;; and the steady-state value) gives an approximately 20% speed-up for a soma-only cell with 4
;; channels (8 particles), serial version.

(defstruct (particle-type (:print-function (lambda (elt stream depth)
					     (declare (ignore depth))
					     (format stream "<Particle Type ~a>" (element-name elt)))))
  name
  class					; E.g. :HH, :HH-EXT, :HH-EXT-OLD, :MARKOV

  (Q10 1.0  :type single-float)		; For particle kinetics
  (reference-temp 27.0 :type single-float) ; Reference temperature for kinetic parameters (Centigrade)
  (q10-rate-factor 1.0 :type single-float)
  
  ;; *********************************************************************************************************
  ;; Concerning Markov particle types -
  ;;; *********************************************************************************************************

  number-of-states			; The number N of states

  ;; For Markov particles with N states, this is an NxN array of either explicit functions or
  ;; voltage-indexed look up tables (arrays) governing state transitions (entry_ij => transition
  ;; from state i to state j). A NIL entry in location ij means that there is no transition between
  ;; state i and state j.

  state-transition-array
  
  ;; A 1D binary array of length N, where non-NIL entries denote conducting states.

  open-state-array			

  ;; *********************************************************************************************************
  ;; For 2 state particles (including :HH, :HH-EXT, and :HH-EXT-OLD) state transitions are
  ;; parameterized in terms of time constant and steady state functions.
  ;; *********************************************************************************************************

  tau-array				; These values take into account the temperature of the simulation.
  inf-array

  ;; *********************************************************************************************************
  ;; Concerning the :HH-EXT and :HH-EXT-OLD class of particle types
  ;; *********************************************************************************************************

  (valence 0.0 :type single-float)	; particle valence
  (gamma 0.0  :type single-float)	; dimensionless - between 0 and 1
  (base-rate 0.0  :type single-float)	; 1/ms
  (v-half 0.0  :type single-float)	; mV
  (tau-0 0.0  :type single-float)	; ms

  (ignore-tau-voltage-dependence nil :type boolean) ; When T, the :TAU-0 value is used as the v-indep time constant.
  
  ;; *********************************************************************************************************
  ;; For non :HH-EXT class particle types (including :HH class), explicit functions for the rate constant alpha and beta, or for
  ;; the tau and steady-state values may be given in the appropriate PARTICLE-TYPE-DEF component. For no
  ;; particular reason, MAKE-V-PARTICLE-ARRAYS looks to see if the :ALPHA-FUNCTION and :BETA-FUNCTION slots
  ;; can be used - if they are NIL then the :TAU-FUNCTION and :SS-FUNCTION slots are used. Functions written
  ;; for these slots are called with a single VOLTAGE argument (which is a single float) and return a rate constant in 1/ms (for
  ;; the alpha and beta functions), a time constant in ms (for tau), or a value between 0 and 1 (for ss).
  ;; *********************************************************************************************************
  
  alpha-function
  beta-function

  (tau-coefficient 1.0 :type single-float) ; A scaling factor when tau(V) is derived from explicit functions of alpha and beta.
  tau-function
  ss-function				; ss (steady state) = inf (infinity), as above

  ;; *********************************************************************************************************

  first-element			; The first particle of this type
  last-element			; The last of this type
  evaluation-function			; For user defined particle types, this function should take one required argument, the particle
					; type, and one optional argument, flag indicating initial state. The
					; function should loop over all particles of the type and set the :STATE-N+1-DOUBLE slot of each.
  parameters
  )

(setf (documentation 'particle-type-reference-temp 'function) " Degrees C")
(setf (documentation 'particle-type-base-rate 'function) " 1/ms")
(setf (documentation 'particle-type-v-half 'function) " mV")
(setf (documentation 'particle-type-tau-0 'function) " ms")
(setf (documentation 'particle-type-gamma 'function) " dimensionless - between 0 and 1")

(defstruct (particle-double-floats (:type (vector double-float)))
  (state-n+1 0.0d0	:type double-float) ; state at t(n+1)
  (state-n   0.0d0	:type double-float) ; state at t(n)
  (dsdt-n 0.0d0 :type double-float)
  (dsdt-n-1 0.0d0 :type double-float))

(deftype particle-df-array () '(simple-array double-float (4)))

(deftype prt-dfs () 'particle-df-array)

(defmacro particle-aref-state-n+1 (particle-double-floats) `(aref (the prt-dfs ,particle-double-floats) 0))
(defmacro particle-aref-state-n (particle-double-floats) `(aref (the prt-dfs ,particle-double-floats) 1))
(defmacro particle-aref-dsdt-n (particle-double-floats) `(aref (the prt-dfs ,particle-double-floats) 2))
(defmacro particle-aref-dsdt-n-1 (particle-double-floats) `(aref (the prt-dfs ,particle-double-floats) 3))

(defmacro particle-state-n+1-double (particle) `(particle-aref-state-n+1 (particle-double-floats ,particle)))
(defmacro particle-state-n-double (particle) `(particle-aref-state-n (particle-double-floats ,particle)))
(defmacro particle-dsdt-n-double (particle) `(particle-aref-dsdt-n (particle-double-floats ,particle)))
(defmacro particle-dsdt-n-1-double (particle) `(particle-aref-dsdt-n-1 (particle-double-floats ,particle)))

(defmacro particle-double-floats-aref-state-n+1 (particle-double-floats) `(particle-aref-state-n+1 ,particle-double-floats))
(defmacro particle-double-floats-aref-state-n (particle-double-floats) `(particle-aref-state-n ,particle-double-floats))
(defmacro particle-double-floats-aref-dsdt-n (particle-double-floats) `(particle-aref-dsdt-n ,particle-double-floats))
(defmacro particle-double-floats-aref-dsdt-n-1 (particle-double-floats) `(particle-aref-dsdt-n-1 ,particle-double-floats))

(defstruct (particle (:print-function (lambda (elt stream depth)
					(declare (ignore depth))
					(format stream "<Particle ~a: type ~a>"	(element-name elt) (element-name (particle-type elt))))))
  "Model for a gating particle"
  name
  (type nil :type particle-type)

  conc-particle				; For certain particle types with a concentration dependence, the link to this dependence is via this slot.
  channel
  vnode-point				; This points to the circuit node whose voltage controls this particle.

  next-element				; The next particle of this type

  ;; For Markov particles a list of N PARTICLE-DOUBLE-FLOAT arrays, one for each state of the particle.
  state-arrays

  parameters
  (double-floats (make-particle-double-floats) :type element-double-floats) ; Used for two state particles. 
  )


;; ********************************************************************
;;    Conc-part
;; ********************************************************************

(defstruct (conc-particle-type (:print-function (lambda (type stream depth)
						  (declare (ignore depth))
						  (format stream "<Conc Particle Type ~a>" (conc-particle-type-name type)))))
  name

  ;; The state equations of concentration particle types may be governed by the following possibilities:
  ;;
  ;;    Kinetic scheme whose rate functions are arbitrary functions of concentration and voltage (:CLASS = :CONC-VOLT). 
  ;;
  ;;    Kinetic scheme whose forward (alpha) and backward (beta) rate constants are functions of concentration (:CLASS = :NTH-ORDER)
  ;;
  ;;    Kinetic scheme whose rate functions are functions of concentration and voltage as given by Moczydlowski and Latorre 1983 (:CLASS = :ML).   
  ;; 
  ;;    State of particles are a static function of concentration (:CLASS = :STATIC), given by an array in the :CONC-DEPENDENCE slot. 
  ;;
  ;; For all these techniques, the concentration may be raised to the power given in :CONC-POWER, and for the
  ;; kinetic schemes the minimum time constant may be specified in the :TAU-0 slot.
  ;;   -------------------------------------------------------------------------------------------------------------
  ;;    :CLASS      :ALPHA,   :CONC-POWER    :TAU-0  :CONC-DEPENDENCE  :K4,    :ALPHA-FUNCTION, :TAU-FUNCTION,
  ;;                :BETA                                              :K1     :BETA-FUNCTION,  :SS-FUNCTION
  ;;   -------------------------------------------------------------------------------------------------------------
  ;;   :NTH-ORDER    yes          yes          yes       no             no                    no
  ;;   :ML           yes          no           yes       no             arrays                no
  ;;   :STATIC       no           yes          no        array          no                    no
  ;;   :CONC-VOLT    no           yes          yes       no             no     functions for alpha/beta or tau/ss, args are CONC-ARG and VOLTAGE
  ;;   :GENERIC (or T) no         no           no        no             no     functions for alpha/beta or tau/ss, arg is PARTICLE

  ;; In the case of :CONC-VOLT :CLASS :ALPHA-FUNCTION, :BETA-FUNCTION, :TAU-FUNCTION or :SS-FUNCTION, these functions may assume
  ;; that the CONC-ARG and VOLTAGE arguments are both double floats.

  ;; ------------------------------------------------------------------------------
  ;; For the :static-linear model the :parameters slot includes 'CONCENTRATION-MIN and 'CONCENTRATION-SLOPE parameters,
  ;; for use in the function (eval-static-linear-conc-particle cprt type concentration-min concentration-slope initial-state concentration)))
  ;; ------------------------------------------------------------------------------
  ;; For the Moczydlowski and Latorre 1983 model the :PARAMETERS slot includes:
  ;;
  ;; 'DELTA-1   -  The "gamma" of the voltage dependence terms for the forward and backward rate constants.
  ;; 'DELTA-4
  ;;
  ;; 'VALENCE   - Valence of the involved ion (typically +2, for calcium).
  ;;
  ;; 'K1-0      -  Coefficients for the exponential voltage functions for the forward and backward rate
  ;; 'K4-0         constants.
  ;;
  ;; 'BETA-0    -  The base rate coefficient for the forward rate constant.
  ;; 'ALPHA-0   -  The base rate coefficient for the backward rate constant.
  ;; ------------------------------------------------------------------------------

  (class t)
  (alpha 0.0    :type single-float)	; Typically, for the forward rate constant, but for :ML model, for the backward rate constant. 
  (beta 0.0     :type single-float)	; Typically, for the backward rate constant, but for :ML model, for the forward rate constant. 
  (tau-0 0.0d0  :type double-float)
  (conc-power 1 :type fixnum)		

  k1 k4					; Voltage-dep arrays for the :ML model

  alpha-function beta-function		; The arguments for these functions are:
  tau-function	 ss-function		; CONC-ARG VOLTAGE

					; The CONC-ARG (in mM) is given by the formula (a*(CONC-base))^c (applied internally by
					; the function CONC-PARTICLE-CONCENTRATION-ARG), where base is given by the
					; :BASE-CONCENTRATION slot, a by the :CONCENTRATION-COEFFICIENT slot, and c by the
					; :CONC-POWER slot. The VOLTAGE argument is also a double-float. The TYPE is the
					; conc-particle type. Any or all of these arguments may be ignored be the alpha-function,
					; beta-function, etc. The functions must return double-float values.
  
  conc-dependence			; This is a function (why not array?) whose single (non-keyword) double float argument is:
					;
					;  CONC^CONC-POWER
					;
					; as described above.
    
  (shell 1 :type fixnum)		; Which concentration compartment controls the particle (1, 2 or 3). If a negative number
					; (e.g. -1), then concentration is taken as the average (or total) compartment concentration. 

  conc-int-type				; The concentration integrator type that controls the particle.

  (base-concentration 0.0 :type single-float) ; Offset (mM) for the concentration seen by this type.
  (concentration-coefficient 1.0 :type single-float) ; Coefficient for the effective concentration seen by this type.
  
  (Q10 1.0 :type single-float)
  (reference-temp 27.0 :type single-float) ; Reference temperature for kinetic parameters (Centigrade)
  (q10-rate-factor 1.0 :type single-float)

  first-element				; The first conc-particle of this type
  last-element				; The last of this type

  evaluation-function			; For user defined particle types, this function should take one required argument, the conc particle
					; type, and two optional arguments, flag indicating initial state and concentration. The
					; function should loop over all particles of the type and set the :STATE-N+1-DOUBLE slot of each.
  parameters)

(defmacro conc-particle-state-n+1-double (cprt) `(particle-aref-state-n+1 (conc-particle-double-floats ,cprt)))
(defmacro conc-particle-state-n-double (cprt) `(particle-aref-state-n (conc-particle-double-floats ,cprt)))
(defmacro conc-particle-dsdt-n-double (cprt) `(particle-aref-dsdt-n (conc-particle-double-floats ,cprt)))
(defmacro conc-particle-dsdt-n-1-double (cprt) `(particle-aref-dsdt-n-1 (conc-particle-double-floats ,cprt)))

(defmacro conc-particle-aref-state-n+1 (conc-particle-double-floats) `(particle-aref-state-n+1 ,conc-particle-double-floats))
(defmacro conc-particle-aref-state-n (conc-particle-double-floats) `(particle-aref-state-n ,conc-particle-double-floats))
(defmacro conc-particle-aref-dsdt-n (conc-particle-double-floats) `(particle-aref-dsdt-n ,conc-particle-double-floats))
(defmacro conc-particle-aref-dsdt-n-1 (conc-particle-double-floats) `(particle-aref-dsdt-n-1 ,conc-particle-double-floats))

(defstruct (conc-particle (:print-function (lambda (prt stream depth)
					     (declare (ignore depth))
					     (format stream "<Conc Particle ~a: type ~a>"
						     (conc-particle-name prt)
						     (conc-particle-type-name (conc-particle-type prt))))))
  name
  type

  (channel nil)

  next-element				; The next particle of this type
  
  cnode-point				; This points to the circuit node whose concentration (and possibly voltage) controls this particle.

  conc-int				; This is the concentration integrator that controls the particle
  
  parameters
  (double-floats (make-particle-double-floats) :type element-double-floats))


;; ********************************************************************
;;    Synapse Type
;; ********************************************************************

(defstruct (synapse-type (:print-function (lambda (elt stream depth)
					    (declare (ignore depth))
					    (format stream "<Synapse Type ~a>" (synapse-type-name elt)))))
  name

  (iv-parameters (make-membrane-element-type-iv))

  (control :EVENT)

  ;; This synapse type is controlled by
  ;;
  ;; :LIGHT -
  ;;
  ;; :CHANNEL - Synaptic conductance is calculated as if it is a channel, with the reference node for the
  ;;            voltage/concentrations controlling the channel defined by the :PRE-SYNAPTIC-NODE of each synapse.
  ;;
  ;; :VOLTAGE - The voltage at the :PRE-SYNAPTIC-NODE of each synapse triggers a stereotype conductance waveform.
  ;;
  ;; :EVENT - Fixed timing parameters - events - that trigger a stereotype conductance waveform.
  ;;
  ;; :LIGHT-EVENT - The events for an EVENT type are generated from a waveform determined by LIGHT synapse properties.
  ;;
  ;; :TONIC
  ;;
  ;; E.g., for a light dependent synapse (SYNAPSE-TYPE :CONTROL slot is :LIGHT):
  
  ;;  ((IMPULSE-FUNCTION . (DOUBLE-ALPHA 10 60 1)) -> Necessary, must return 1d sequence
  ;;   (IMPULSE . 1d-array) -> The time base interval for this array is 1msec
  ;;   (IMPULSE-NONLINEARITY . THRESHOLD) -> Optional
  ;;   (IMPULSE-NONLINEARITY-PARAMETERS . 0) -> Optional
  ;;   (LINEAR-IMPULSE-FUNCTION . (ALPHA 20)) -> Optional, must return 1d sequence
  ;;   (LINEAR-IMPULSE . 1d-array) -> The time base interval for this array is 1msec
  ;;   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 20 20 1)) -> Optional, must return 2d array
  ;;   (SPATIAL-RF . 2d-array)
  ;;   (LIGHT-OFFSET-DISTANCE . 0) -> Optional
  ;;   (LIGHT-OFFSET-ANGLE . 0) -> Optional 
  ;;   (ADJUST-TO-RF-AREA . t) -> Optional
  ;;           .
  ;;           .
  ;;           .
  
  ;; The IMPULSE and SPATIAL-RF array entries are computed in the setup sequence, according to the appropriate parameters. The
  ;; SPATIAL-RF-FUNCTION entry, for light-dependent synapses, specifies a function which returns a 2-D array that defines the
  ;; spatial receptive field. If this entry does not exist for a light-dependent synapse, then the spatial receptive field is a 2D
  ;; impulse. For the args to GAUSSIAN-RF, see the funspec. If the ADJUST-TO-RF-AREA entry is NIL or missing, then the spatial
  ;; integration is done without compensating for the support of the of the SPATIAL RF 2-D array. Otherwise, the integration is
  ;; multiplied by the area of the support. The first case is used, for example, when the 2-D array sums up to 1.0 or 0.0 (e.g. if
  ;; you have a DOG RF, and the keyword :TOTAL-VOL 0 is included so that the array has zero overall weight).

  ;;  (LIGHT-INPUTS-ARRAY . 2d-waveform-array)
  ;;  (LIGHT-INPUTS-1ST . 1d-fixnum-array)
  ;;  (LIGHT-INPUTS-LAST . 1d-fixnum-array)
  ;;  (LIGHT-INPUTS-BASE-SHIFT . 1d-fixnum-array)
  ;;  (LIGHT-INPUTS-BASE-GAIN . 1d-float-array)
  ;;  (CONDUCTANCE-INPUTS-ARRAY . 2d-waveform-array)
  ;;   )

  ;; LIGHT-INPUTS-ARRAY is a two-dimensional array storing canonical light input waveforms. CONDUCTANCE-INPUTS-ARRAY is an array
  ;; of the same size which holds canonical conductance waveforms each of which, appropriately shifted and scaled, is referenced
  ;; by a subset of the synapses of this type. For N waveforms, and a simulation of T milliseconds, this is an N*T single-float
  ;; array.

  ;; The LIGHT-INPUTS-ARRAY is constructed as follows: For all the synapses of a given type, the light-input profile is computed
  ;; and compared with all the waveforms in LIGHT-INPUTS-ARRAY (it is cleared at the start of the synapse input evaluations). If
  ;; the current waveform is equivalent to one of the stored waveforms, say the Mth waveform in the waveform array, within a
  ;; shifting factor (waveform-shift) and scaling factor (waveform-gain), then the index for the Mth waveform, and the derived
  ;; waveform-shift and waveform-gain values are stored in the current synapse. If the current waveform cannot be matched to any
  ;; of the stored waveforms, then the current waveform is added to the waveform array, and the array index pointing to this
  ;; waveform, and values of 0 (for waveform-shift) and 1.0 (for waveform-gain) are stored in the current synapse.

  ;; Once LIGHT-INPUTS-ARRAY has been constructed, each light input waveform is convolved with the impulse response of the synapse
  ;; type, and the result stored in CONDUCTANCE-INPUTS-ARRAY.

  ;; For a voltage-triggered or event synapse (SYNAPSE-TYPE :CONTROL slot is :VOLTAGE or :EVENT):
  
  ;;  ((WAVEFORM-FUNCTION . (ALPHA-ARRAY 10 :step 0.2))
  ;;   (WAVEFORM . 1d-array) -> The default time base interval for this array is 0.2msec
  ;;   (WAVEFORM-TIME-INTERVAL . 0.2)
  ;;           .
  ;;           .
  ;;           .
  ;;   )
  
  (waveform-time-interval-inverse 5.0 :type single-float)
  (waveform-time-interval-mrt 1 :type fixnum)

  ;; For a voltage-triggered (SYNAPSE-TYPE :CONTROL slot is :VOLTAGE), these parameters have the same meaning as those for axon types:
  
  (input-threshold -45.0d0 :type double-float) ; millivolts
  (refractory-period 0.50 :type single-float) ; milliseconds
  (supra-threshold-duration-min 0.10 :type single-float)

  evaluation-function
  
  first-element				; The first synapse of this type
  last-element				; The last of this type
  parameters)

;; ********************************************************************
;;    Synapse
;; ********************************************************************

(defstruct (synapse (:print-function (lambda (elt stream depth)
				       (declare (ignore depth))
				       (format stream "<Synapse ~a: type ~a" (element-name elt) (element-name (synapse-type elt)))
				       (when (synapse-pre-synaptic-element elt) (format stream ", pre-synaptic-elt ~a" (synapse-pre-synaptic-element elt)))
				       (format stream ">"))))
  name
  type
  cell-element				;What the synapse is a part of (soma or segment).

  next-element				; The next synapse of this type
  
  (blocked nil :type boolean)		; When true, block completely

  conc-ints-params			; Concentration integrators associated with this synapse, with the shells and appropriate
					; coefficients for calculating the  e-rev, e.g:

					; '((<soma-1-CA-IN> 1 0.6) (<soma-1-CA-IN> 2 0.2) (<soma-1-K-EXTRA> 1 0.2))

  pre-synaptic-element			; For light or light-event controlled synapses, a receptive field label. For
					; voltage-dependent synapses, the controlling cell  element, and

  channel				; (possibly) the voltage-dependent channel of the post-synaptic membrane.

  (inherit-parameters-from-type t)	; When T, some parameters are inherited from the synapse-type when the synapse-type
					; parameters are edited - otherwise, the synapse parameters are changed only by reference
					; to specific synapses.
  
  ;; If :INHERIT-PARAMETERS-FROM-TYPE is NIL, then by default the IV-RELATION-REFERENCE for this element is in absolute terms.
  
  ;; The following parameters are normally taken from the synapse-type when the synapse is created. They are only relevent to the
  ;; synapse instance when :inherit-parameters-from-type is NIL:
  
  ;; For a light dependent synapse these parameters will override location parameters implicit in the synapse :NODE location and
  ;; the offset information in the SYNAPSE-TYPE.
  
  ;;   (...(LIGHT-input-x . 0) (LIGHT-input-y . 0)...)


  ;; *********************************************************
  ;; For VOLTAGE, LIGHT-EVENT and EVENT controlled synapses ****
  ;; *********************************************************

  event-times				; This is a list of sf event initiation times (in milliseconds of real time) for VOLTAGE,
					; LIGHT-EVENT and EVENT controlled synapses - this list is analagous to the :SPIKE-TIMES slot of axons.

  event-generator			; For VOLTAGE controlled synapse types, the synapse which acts as an event detector for
					; all other synapses of the same type and with the same pre-synaptic node. For LIGHT and
					; LIGHT-EVENT controlled synapse types, the light response and event generation applied to
					; this synapse is used by all other synapses of the same type with the same receptive
					; field characteristics, defined by the synapse type and the XY coordinates of the RF
					; center. 

  (delay 0.0 :type single-float)	; For event and voltage synapses, the synapse type waveform is applied at each event
					; time plus this delay, in milliseconds. 

  (fixnum-delay 0 :type fixnum)		; Calculated from the above value, in units of the waveform timebase.

  transformed-events			; All events for event synapses are scaled by the inverse of the waveform time
					; interval, sorted, and then put in a list of fixnums stored here for the
					; eval-event-synapse routine. For voltage synapses, this holds event times as they are
					; detected, in a list of fixnums. 

  ;; *********************************************************
  ;; For VOLTAGE controlled synapses *************************
  ;; *********************************************************

  (sub-threshold-time 0.0 :type single-float) ; The last time that the proximal node of the synapse was below threshold, when the
					      ; node is above threshold. When the node is below threshold, then this is the current time.

  ;; *********************************************************
  ;; For LIGHT and LIGHT-EVENT controlled synapses ************
  ;; *********************************************************

  wave-ref				; Either -
  
					;  :NEWBIE - Initial value
					;
  					;  (index shift gain) - List to reference pre-computed conductance waveform array for
					;  light-dependent synapses, or voltage waveform for voltage-dependent synapses. This
					;  array must be type single-float, with length = simulation duration. 
					;
					;  NIL - Synapse should not be evaluated during the simulation, since it is never activated.

  parameters
  (iv-values (make-membrane-element-iv-values) :type memb-elt-iv-values-array))

(defmacro synapse-wave-ref-index (wave-ref) `(the fn (car ,wave-ref)))
(defmacro synapse-wave-index (syn) `(synapse-wave-ref-index (synapse-wave-ref ,syn)))
(defmacro synapse-wave-ref-shift (wave-ref) `(the fn (cadr ,wave-ref)))
(defmacro synapse-wave-shift (syn) `(synapse-wave-ref-shift (synapse-wave-ref ,syn)))
(defmacro synapse-wave-ref-gain (wave-ref) `(the sf (caddr ,wave-ref)))
(defmacro synapse-wave-gain (syn) `(synapse-wave-ref-gain (synapse-wave-ref ,syn)))

(defmacro synapse-node (syn)
  `(let ((cell-elt (synapse-cell-element ,syn)))
     (typecase cell-elt
       (soma (soma-node cell-elt))
       (segment (segment-node-2 cell-elt)))))


;; ********************************************************************
;;    Conc-int
;; ********************************************************************

;; All concentrations are in mM, and reflect concentrations of total ion (other than conc-int data, which is free ion).

(defstruct (conc-int-type (:print-function (lambda (int stream depth)
					     (declare (ignore depth))
					     (format stream "<Conc Int Type ~a>" (conc-int-type-name int)))))
  name
  
  (class :MULTI-SHELL)			; Determines the integration method. Possibilties include :MULTI-SHELL, :GENERAL, :FIRST-ORDER, :GENERIC

					; For :GENERIC, the CONC-INT-TYPE-DEF form must have either:
					;
					; C-N+1-FUNCTION, defined with a single argument being a conc-int instance, and that
					; returns a double-float concentation in mM for the current time step t-N+1. 
					;
					; DCDT-FUNCTION, defined with a single argument being a conc-int instance, and that
					; returns a double-float derivative of the concentation in mM/ms for the current time step t-N+1.
					;
					; If both are function types are given, then C-N+1-FUNCTION takes precedent in the evaluation of the conc-ints.
  
  species				; E.g. CA, K, NA
  (valence 1.0 :type single-float)

  (blocked nil :type boolean)		; Concentration integrators of this type are only advanced during the simulation when this is NIL.
  enabled-for-this-simulation
  
  (intra-p t)				; Is this integration for the intra (T) or extracellular space (NIL)

  ;; ***********************************
  ;; For :GENERAL type conc-ints - NOT IMPLEMENTED YET
  ;; ***********************************
  
  system-of-differential-equations	

  ;; ***********************************
  ;; For Multiple compartment conc-int types
  ;; ***********************************

  ;; Note that there is always a shell 1.

  (shell-2-p nil)
  (shell-3-p nil)
  (core-p nil)

  ;; ***********************************
  ;; Parameters that determine geometry for shells 1, 2 and 3, and the core. 
  ;; ***********************************
  
  volumes				; A list of compartment names and either a number or function for determining their
					; volumes. These functions must take an instance of a concentration integrator as a single
					; arg, and return the appropriate compartment volume in um^3. For example:

					; ((1 (lambda (cint) (* (element-volume cint) .01))) 
					;  (2 (lambda (cint) (* (element-volume cint) .09)))
					;  (3 2.345e3)
					;  (core (lambda (cint) (* (element-volume cint) .5))))

  membrane-areas			; A list of compartment names and either a number or function for determining their
					; membrane areas. Normally this will apply to shells 1 and 2 only, and are used for
					; calculating membrane areas for any pumps associated with a given compartment. These
					; functions must take an instance of a concentration integrator as a single arg, and
					; return the appropriate membrane area in um^2. For example: 

					; ((1 (lambda (cint) (* (element-area cint) .1))) 
					;  (2 2.34e1))

  diffusion-areas			; A list of compartment pairs and either a number or function for determining their
					; diffusional areas. These functions must take an instance of a concentration integrator
					; as a single arg, and return the appropriate membrane area in um^2. For example: 

					; (((1 3) (lambda (cint) (* (element-area cint) .1))) 
					;  ((2 3) 1.342e2)
					;  ((core 3) (lambda (cint) (* (element-area cint) .5))))

  ;; There is no assumed ordering of the compartment pairs.

  (diffusion-distances 1.0e4)		; A single number or a list of compartment pairs and either a number or function for
					; determining their diffusional distance. These functions must take an instance of a
					; concentration integrator as a single arg, and return the appropriate distance in um. If
					; only a single number, then this applies to all compartment pairs. For example: 

					; (((1 3) (lambda (cint) (/ (element-volume cint) (element-area cint))))
					;  ((2 3) 0.1)
					;  ((core 3) 0.2))

  ;; There is no assumed ordering of the compartment pairs. 

  ;; If either :VOLUMES, :DIFFUSION-AREAS or :MEMBRANE-AREAS is NIL, then the following parameters are used to determine compartment volumes or areas -
  
  ;; If juxtamembrane-shell-thickness > 0, then this is the thickness of the interdigitated shells 1 and 2, in microns, otherwise,
  ;; the first (and only) shell volume is equal to the associated cell element volume. 
  (juxtamembrane-shell-thickness 0.0 	:type single-float)
  
  (alpha-s 1.0		:type single-float) ; Proportion of juxta-membrane shell assigned to shell 1.
  
  (inner-shell-thickness 0.25 	:type single-float) ; Thickness of shell 3, microns. If equal to 0, then the volume of shell 3 is
						    ; given by the cell element volume minus the volume of the juxtamembrane
						    ; shell.  
  
  ;; ***********************************
  ;; Inter-compartment diffusion parameters for :MULTI-SHELL type conc-ints -
  ;; ***********************************

  ;; Either a list of compartment pair lists followed by their respective diffusion coefficient, or a single number for all
  ;; pairs. Diffusion coefficients are in cm^2 sec^-1. For example, 

  ;; (((1 2) value) ((2 3) value) ((core 3) value) ... )

  ;; There is no assumed ordering of the compartment pairs.
  
  (diffusion-coefficient *D_CA*)	; Default value is for calcium.
  
  ;; Coefficient for calculating the effective diffusion area between the interdigited shells 1 and 2.  
  (interdigitation-coefficient zero :type single-float) ; 1/microns

  ;; ***********************************
  ;; Pump parameters
  ;; ***********************************

  ;; A list of pump types and associated compartments, e.g.
  ;;
  ;;  '((CA-ATP 1)(CA-ATP 2)(CA-BASAL 1))
  
  pump-type-params			

  ;; ***********************************
  ;; Instantaneous buffer parameters
  ;; ***********************************
  
  instantaneous-buffer-enabled		; Enables the instantaneous shell buffers.

  ;; These ratios are the dimensionless ratio of bound[X]/[X] in each shell. Shells other than 1, 2,
  ;; or 3 are covered by the :GLOBAL-INSTANTANEOUS-BUFFER-RATIO+1 slot.

  (shell-1-instantaneous-buffer-ratio+1 1.0 :type single-float)
  (shell-2-instantaneous-buffer-ratio+1 1.0 :type single-float)
  (shell-3-instantaneous-buffer-ratio+1 1.0 :type single-float)
  (global-instantaneous-buffer-ratio+1 1.0 :type single-float)

  ;; ***********************************
  ;; Concentration values 
  ;; ***********************************

  ;; Concentration of :SPECIES in the constant concentration core compartment.
  (core-conc 5.0e-5 :type single-float) ; mM 
  (core-conc-double 5.0d-5 :type double-float) ; internal use only (mM)

  ;; Concentration of :SPECIES on the other side of the integrator, if there is not another integrator there. 
  (transmembrane-conc 1.0 :type single-float) ; mM
  (transmembrane-conc-double 1.0d0 :type double-float) ; internal use only (mM)

  ;; ***********************************

  (Q10 1.0  :type single-float)		; For diffusion kinetics
  (reference-temp 27.0 :type single-float) ; Reference temperature for kinetic parameters (Centigrade)
  (q10-rate-factor 1.0 :type single-float)
  
  (conc-ints () :type list)		; All the conc-ints of this type
  
  parameters)

(defstruct (conc-int-double-floats (:type (vector double-float)))
  (shell-1-conc-n+1 0.0d0 :type double-float) ; mM
  (shell-1-conc-n 0.0d0 :type double-float) ; mM
  (shell-1-dcdt-n 0.d0 :type double-float) ; mM/ms
  (shell-1-dcdt-n-1 0.d0 :type double-float) ; mM/ms
  
  (shell-2-conc-n+1 0.0d0 :type double-float) ; mM
  (shell-2-conc-n 0.0d0 :type double-float) ; mM
  (shell-2-dcdt-n 0.d0 :type double-float) ; mM/ms
  (shell-2-dcdt-n-1 0.d0 :type double-float) ; mM/ms
  
  (shell-3-conc-n+1 0.0d0 :type double-float) ; mM
  (shell-3-conc-n 0.0d0 :type double-float) ; mM
  
  (total-conc-n 0.0d0 :type double-float) ; mM - used for conc-ints who control a particle which depends on the concentration averaged over the entire element.
  (total-conc-n+1 0.0d0 :type double-float) ; mM
  (e-rev-shell-1 0.0d0 :type double-float) ; millivolts
  (e-rev-shell-2 0.0d0 :type double-float) ; millivolts
  )

(deftype conc-int-df-array () '(simple-array double-float (14)))

(defmacro conc-int-aref-shell-1-conc-n+1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 0))
(defmacro conc-int-aref-shell-1-conc-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 1))
(defmacro conc-int-aref-shell-1-dcdt-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 2))
(defmacro conc-int-aref-shell-1-dcdt-n-1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 3))

(defmacro conc-int-aref-shell-2-conc-n+1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 4))
(defmacro conc-int-aref-shell-2-conc-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 5))
(defmacro conc-int-aref-shell-2-dcdt-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 6))
(defmacro conc-int-aref-shell-2-dcdt-n-1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 7))

(defmacro conc-int-aref-shell-3-conc-n+1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 8))
(defmacro conc-int-aref-shell-3-conc-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 9))

(defmacro conc-int-aref-total-conc-n (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 10))
(defmacro conc-int-aref-total-conc-n+1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 11))

(defmacro conc-int-aref-e-rev-shell-1 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 12))
(defmacro conc-int-aref-e-rev-shell-2 (cint-double-floats) `(aref (the conc-int-df-array ,cint-double-floats) 13))

;; ***********************************

(defmacro conc-int-shell-1-conc-n+1 (cint) `(conc-int-aref-shell-1-conc-n+1 (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-1-conc-n (cint) `(conc-int-aref-shell-1-conc-n (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-1-dcdt-n (cint) `(conc-int-aref-shell-1-dcdt-n (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-1-dcdt-n-1 (cint) `(conc-int-aref-shell-1-dcdt-n-1 (conc-int-double-floats ,cint)))

(defmacro conc-int-shell-2-conc-n+1 (cint) `(conc-int-aref-shell-2-conc-n+1 (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-2-conc-n (cint) `(conc-int-aref-shell-2-conc-n (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-2-dcdt-n (cint) `(conc-int-aref-shell-2-dcdt-n (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-2-dcdt-n-1 (cint) `(conc-int-aref-shell-2-dcdt-n-1 (conc-int-double-floats ,cint)))

(defmacro conc-int-shell-3-conc-n+1 (cint) `(conc-int-aref-shell-3-conc-n+1 (conc-int-double-floats ,cint)))
(defmacro conc-int-shell-3-conc-n (cint) `(conc-int-aref-shell-3-conc-n (conc-int-double-floats ,cint)))

(defmacro conc-int-total-conc-n (cint) `(conc-int-aref-total-conc-n (conc-int-double-floats ,cint)))
(defmacro conc-int-total-conc-n+1 (cint) `(conc-int-aref-total-conc-n+1 (conc-int-double-floats ,cint)))

(defmacro conc-int-e-rev-shell-1 (cint) `(conc-int-aref-e-rev-shell-1 (conc-int-double-floats ,cint)))
(defmacro conc-int-e-rev-shell-2 (cint) `(conc-int-aref-e-rev-shell-2 (conc-int-double-floats ,cint)))

(defstruct (conc-int (:print-function (lambda (int stream depth)
					(declare (ignore depth))
					(format stream "<Conc Int ~a: type ~a>"	(conc-int-name int) (conc-int-type-name (conc-int-type int))))))
  name
  type
  cell-element				; Soma or segment

  (blocked nil :type boolean)		; Integrator is only advanced during the simulation when this is NIL.
  enabled-for-this-simulation
  
  evaluate-total-concentration		; Used for conc-ints who control a particle which depends on the concentration averaged over the entire element.

  (shell-1-volume 0.0d0 :type double-float) ; um^3
  (shell-2-volume 0.0d0 :type double-float) ; um^3
  (shell-3-volume 0.0d0 :type double-float) ; um^3 was called "inner-shell-volume"
  (core-volume 0.0d0 :type double-float) ; um^3
  (total-volume 0.0d0 :type double-float) ; um^3

  transmembrane-integrator		; Concentration integrator for same species on opposite side of membrane. If NIL, then
					; transmembrane concentration of concentration integrator type :SPECIES is given by that
					; type's :TRANSMEMBRANE-CONCENTRATION.   

  ;; For shell 1 and shell 2 (if it exists), the value in the :SHELL-x-PORES slot (where "x" is 1 or 2) is a list of channels and
  ;; synapses that communicate with the shell and a (double float) current coefficient appropriate for that shell, e.g: 

  ;; ((<CHANNEL-1> 0.34d0) (<CHANNEL-2> 0.65d0) (<SYNAPSE-1> 1.0d0) ...)

  ;; The current coefficient is the product of the channel's or synapse's relative permeability to the concentration integrator
  ;; type :SPECIES, the proportion of the channel or synapse current assigned to shell 1 or 2, as appropriate, and a delta
  ;; term. The former value is given in the :ION-PERMEABILITIES slot of the channel or synapse type, and the latter value is given
  ;; in the :CONC-INT-TYPE-PARAMS slot of the channel or synapse type.  The default is 1.0. The delta term is used when a channel
  ;; which is actually distributed over several (say, N) elements is assigned to a fewer (< N) number of elements, possibly only
  ;; one. In this case delta, which is less than or equal to one, compensates for the overestimate of the integrated current in
  ;; the higher density channel distribution. For example, if the total membrane area of the elements for which a given channel is
  ;; postulated to be assigned to is area-true, and the area of the element(s) that the channel is actually assigned to is some
  ;; smaller value area-actual, then
  ;;
  ;;      delta = area-actual/area-true
  ;;
  ;; The default value for delta is 1.0.  

  (shell-1-pores '())
  (shell-2-pores '())	

  (shell-1-pumps '())
  (shell-2-pumps '())	
  (shell-3-pumps '())	

  ;; For non-instantaneous buffers.
  (shell-1-buffers '())
  (shell-2-buffers '())	
  (shell-3-buffers '())

  ;; For :GENERAL conc-int type conc-ints a list of N CONC-INT-DOUBLE-FLOAT arrays, one for each state variable of the conc-int.

  state-arrays

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For :MULTI-SHELL type conc-ints.
  
  ;; Constants of proportionality between either differences of compartments' (i and j) free concentrations, or ionic current
  ;; entering a compartment j, and d[X]_j/dt. For d[X]/dt, the units are mM/ms. Thus, Betas for concentration differences are in
  ;; units of 1/ms.

  (Beta-2-1 0.0d0       :type double-float) ; [X]_2 - [X]_1 and d[X]_1/dt
  (Beta-1-2 0.0d0	:type double-float) ; [X]_1 - [X]_2 and d[X]_2/dt
  
  (Beta-1-3 0.0d0	:type double-float) ; [X]_1 - [X]_3 and d[X]_3/dt
  (Beta-3-1 0.0d0	:type double-float) ; [X]_3 - [X]_1 and d[X]_1/dt
  
  (Beta-2-3 0.0d0	:type double-float) ; [X]_2 - [X]_3 and d[X]_3/dt
  (Beta-3-2 0.0d0	:type double-float) ; [X]_3 - [X]_2 and d[X]_2/dt
  
  (Beta-core-3 0.0d0	:type double-float) ; [X]_core - [X]_3 and d[X]_3/dt
  (Beta-core-1 0.0d0	:type double-float) ; [X]_core - [X]_1 and d[X]_1/dt -> when there is no shell 3
  (Beta-core-2 0.0d0	:type double-float) ; [X]_core - [X]_1 and d[X]_1/dt -> for a shell 2 and no shell 3

  ;; For :GENERIC concentration integrator types, the following two betas are set to 1, 
  (Beta-current-1 0.0d0	:type double-float) ; Currents of conc-int species X and d[X]_1/dt
  (Beta-current-2 0.0d0	:type double-float) ; Currents of conc-int species X and d[X]_2/dt
  
  ;; ***********************************
  parameters
  (double-floats (make-conc-int-double-floats) :type element-double-floats))

(defmacro conc-int-core-conc (cint) `(conc-int-type-core-conc-double (conc-int-type ,cint))) ; mM

;; ********************************************************************
;;    Axon
;; ********************************************************************

(defstruct (axon-type (:print-function (lambda (type stream depth)
					 (declare (ignore depth))  
					 (format stream "<Axon Type ~a>"	(axon-type-name type)))))
  name
  
  (propagation-velocity 1.0 :type single-float) ; meter/second, reference value

  ;; An axonal spike is initiated when the proximal node of the axon is above :INPUT-THRESHOLD for at least
  ;; :SUPRA-THRESHOLD-DURATION-MIN milliseconds after :REFRACTORY-PERIOD milliseconds following the last axonal spike.

  (input-threshold -45.0d0 :type double-float) ; millivolts
  (refractory-period 0.10 :type single-float) ; milliseconds
  (supra-threshold-duration-min 0.10 :type single-float) ; milliseconds

  (output-waveform nil)
  (blocked nil :type boolean)		; When true, block axon type completely
  (Q10 1.0  :type single-float)		; For propagation velocity (*not* waveform).
  (reference-temp 35.0 :type single-float) ; Reference temperature for propagation  velocity (Centigrade)

  (axons () :type list)			; All the axons of this type
  
  (waveform-time-interval-inverse 10.0 :type single-float)
  (waveform-time-interval-mrt 1 :type fixnum)
  parameters)

(setf (documentation 'axon-type-reference-temp 'function) "Centigrade")
(setf (documentation 'axon-type-propagation-velocity 'function) "m/s")
(setf (documentation 'axon-type-input-threshold 'function) "mV")
(setf (documentation 'axon-type-refractory-period 'function) "milliseconds")
(setf (documentation 'axon-type-supra-threshold-duration-min 'function) "milliseconds")

(defstruct (axon-floats (:type (vector single-float)))
  (voltage 0.0 :type single-float)	; not used now 3/1/95
  (sub-threshold-time 0.0 :type single-float)) ; The last time that the proximal node of the axon was below threshold, when the
					       ; node is above threshold. When the node is below threshold, then this is the current time.

(defmacro axon-voltage (axon) `(aref (the (simple-array single-float *) (axon-floats ,axon)) 0))
(defmacro axon-sub-threshold-time (axon) `(aref (the (simple-array single-float *) (axon-floats ,axon)) 1))

(defstruct (axon (:print-function (lambda (axon stream depth)
				    (declare (ignore depth))
				    (format stream "<Axon ~a: type ~a>" (axon-name axon) (axon-type-name (axon-type axon))))))
  name
  type
  node
  proximal-node
  TARGET-SYNAPSE
  
  ;; (TARGET-SYNAPSEs)			; A list of targets -> do this later
  
  (cell-element nil)                    ; What the axon derives from (soma or segment).
  (blocked nil :type boolean)		; When true, block completely

  ;; The output waveform for a given spike, i, contributes to the axon voltage starting at:
  
  ;; t_0_i = t_init_i + (axon_length / velocity) + delay

  ;; Spikes along an axon may be summed at the output (if they are fast enough so that the output
  ;; waveforms overlap in time). Thus, the axon voltage V at time t is given by:

  ;; V(t) = sum_over_i (output_waveform[t-t_0_i])
  

  (length 0.0 :type single-float)       ; microns
  (delay 0.0 :type single-float)        ; milliseconds

  (propagation-delay 0.0 :type single-float) ; ms, adjusted for temperature from axon-type reference value 

  (spike-times () :type list)           ; This is a list of sf axonal spike initiation times at the proximal node of the axon.

  (inherit-parameters-from-type t)	; For establishing axon parameters after they are created.

  event-generator			; The axon that will serve as an event detector for this axon, which must be of the same
					; type and with the same proximal-node. Similar to the same slot for (voltage controlled) synapses.
  parameters
  (floats (make-axon-floats) :type element-floats))

(setf (documentation 'axon-length 'function) "microns")
(setf (documentation 'axon-delay 'function) "milliseconds")
(setf (documentation 'axon-propagation-delay 'function) "milliseconds, adjusted for temperature from axon-type reference value")


;; ********************************************************************
;;    Cell
;; ********************************************************************

(defstruct (cell-type (:print-function (lambda (type stream depth)
					 (declare (ignore depth))
					 (format stream "<Cell Type ~a>" (cell-type-name type)))))
  name
  
  (notes "" :type string)

  (v-leak-soma *v-leak* :type single-float) ; mV
  (v-leak-dendrite *v-leak-dendrite* :type single-float) ; mV

  (rm-soma *rm-soma* :type single-float) ; Ohms Square Centimeter
  (soma-shunt *soma-shunt* :type single-float) ; Ohms
  (rm-dendrite *rm* :type single-float) ; Ohms Square Centimeter

  (ri *ri* :type single-float) ; Ohm Centimeter
  
  (cm-soma *cm* :type single-float) ; uF / Square Centimeter
  (cm-dendrite *cm-dendrite* :type single-float) ; uF / Square Centimeter

  ;; In the :x-CONC-EXTRA-DEPENDENCE slots below, the value may be :FIXED or :FOLLOWS-GLOBAL. :FOLLOWS-GLOBAL means that the
  ;; associated :x-CONC-EXTRA slot is updated with the value of the associated global variable *x-CONC-EXTRA*. 
  
  ;; In the :E-x-DEPENDENCE slots below, the value may be :FIXED, :FOLLOWS-GLOBAL (the reversal potential follows the appropriate
  ;; global variable), or :FOLLOWS-CONCENTRATION (the reversal potential is updated with the Nernst equation using *TEMPERATURE*
  ;; and the cell type :x-CONC-INTRA and :x-CONC-EXTRA slots).  
					 
  (na-conc-intra *na-conc-intra* :type single-float) ; mM  
  (na-conc-extra *na-conc-extra* :type single-float) ; mM
  (na-conc-extra-dependence :FOLLOWS-GLOBAL) ; See above.
  (e-na-dependence :FIXED)		; See above.
  (e-na *e-na* :type single-float)	; mV
  
  (k-conc-intra *k-conc-intra* :type single-float) ; mM  
  (k-conc-extra *k-conc-extra* :type single-float) ; mM
  (k-conc-extra-dependence :FOLLOWS-GLOBAL) ; See above.
  (e-k-dependence :FIXED)		; See above.
  (e-k *e-k* :type single-float)	; mV

  (ca-conc-intra *ca-conc-intra* :type single-float) ; mM  
  (ca-conc-extra *ca-conc-extra* :type single-float) ; mM
  (ca-conc-extra-dependence :FOLLOWS-GLOBAL) ; See above.
  (e-ca-dependence :FIXED)		; See above.
  (e-ca *e-ca* :type single-float)	; mV

  (cl-conc-intra *cl-conc-intra* :type single-float) ; mM  
  (cl-conc-extra *cl-conc-extra* :type single-float) ; mM
  (cl-conc-extra-dependence :FOLLOWS-GLOBAL) ; See above.  
  (e-cl-dependence :FIXED)		; See above.
  (e-cl *e-cl* :type single-float)	; mV
  
  (inherit-parameters-from-type t)	; For establishing segment parameters after they are created.

  (global-membrane-conductance-factor 1.0 :type single-float) ; Factor for all cell channel and synapse conductances.

  (cells nil :type list)		; A list of cells of this cell-type
  parameters)

(defstruct (cell (:print-function (lambda (cell stream depth)
				    (declare (ignore depth))
				    (format stream "<Cell ~a: type ~a>" (cell-name cell) (cell-type-name (cell-type cell))))))
  name
  type
  (origin '(0.0 0.0 0.0))		; (X, Y, Z) of soma in brain coordinates.

  soma					; Every cell has a soma 
  (segments '())			; A list of segments connected to this cell

  ;; These are the maximum and minimum coordinates (microns) for the entire cell.
  (max-x 0.0) (min-x 0.0) (max-y 0.0) (min-y 0.0) (max-z 0.0) (min-z 0.0)

  max-g-in
  z-tree-discrete-in-cell
  z-tree-cable-in-cell
  z-discrete-in-cell
  z-cable-in-cell
  parameters)

;; ********************************************************************
;;    Buffer
;; ********************************************************************

;; All concentrations are in mM
(defstruct (buffer-type (:print-function (lambda (int stream depth)
					   (declare (ignore depth))
					   (format stream "<Buffer Type ~a>" (buffer-type-name int)))))
  name
  class
  species				; E.g. CA, K, NA
  (blocked nil :type boolean)		; Buffers of this type are only advanced during the simulation when this is NIL.
  
  (total-conc 5.0e-5 :type single-float) ; [BX] + [B] (mM)
  (total-conc-double 5.0d-5 :type double-float)

  system-of-differential-equations

  ;;        Rate constants for buffer - ion interaction
  ;;                         k-forward
  ;;               [B] + [X]    <->     [BX]
  ;;                         k-backward
  ;;
  ;;    [B] is concentration of free buffer in the compartment
  ;;   [BX] + [B] is defined by the total-concentration slot for the buffer type.

  (k-forward 0.0d0	:type double-float) ; mM^-1 ms^-1
  (k-backward 0.0d0	:type double-float) ; ms^-1

  (Q10 1.0  :type single-float)		; For buffer kinetics
  (reference-temp 27.0 :type single-float) ; Reference temperature for kinetic parameters (Centigrade)

  (buffers () :type list)		; All the buffers of this type  
  parameters)

(setf (documentation 'buffer-type-total-conc 'function) " [BX] + [B] (mM) (requires explicit structure)")
(setf (documentation 'buffer-type-k-forward 'function) "  mM^-1 ms^-1 (requires explicit structure)")
(setf (documentation 'buffer-type-k-backward 'function) " ms^-1 (requires explicit structure)")

(defstruct (buffer-double-floats (:type (vector double-float)))
  (conc-n+1 0.0d0 :type double-float)	; Concentration of free buffer at prediction time step, mM
  (conc-n 0.0d0 :type double-float)	; Concentration of free buffer at previous time step, mM
  )

(deftype buffer-df-array () '(simple-array double-float (2)))

(defmacro buffer-conc-n+1 (buffer) `(aref (the (simple-array double-float *) (buffer-double-floats ,buffer)) 0))
(defmacro buffer-conc-n (buffer) `(aref (the (simple-array double-float *) (buffer-double-floats ,buffer)) 1))

(defstruct (buffer (:print-function (lambda (int stream depth)
				      (declare (ignore depth))
				      (format stream "<Buffer ~a: type ~a>"	(buffer-name int) (buffer-type-name (buffer-type int))))))
  name
  type

  (blocked nil :type boolean)		; Buffer is only advanced during the simulation when this is NIL.
  enabled-for-this-simulation
  (cell-element nil)			; Soma or segment
  conc-int
  conc-int-compartment
  parameters
  (double-floats (make-buffer-double-floats) :type element-double-floats))


;; ********************************************************************
;;    Pump - typically for moving juxtamembrane ions across the membrane
;; ********************************************************************

;; All concentrations are in mM
(defstruct (pump-type (:print-function (lambda (int stream depth)
					 (declare (ignore depth))
					 (format stream "<Pump Type ~a>" (pump-type-name int)))))
  name
  
  ;; Pump Type Class -
  ;;
  ;; :MM Michaelis-Menton
  ;; :MM-ZADOR
  ;; :FIRST-ORDER
  ;; :FIRST-ORDER-TAU-V (tau depends on voltage of associated cell element)
  ;; :STATE-EQ Pump dynamics defined by dynamical state equations
  
  class
  species				; E.g. CA, K, NA

  (blocked nil :type boolean)		; pumps of this type are only advanced during the simulation when this is NIL.

  (Q10 1.0  :type single-float)		; For pump kinetics
  (reference-temp 27.0 :type single-float) ; Reference temperature for kinetic parameters (Centigrade)

  ;;; For :FIRST-ORDER and :FIRST-ORDER-TAU-V pumps
  (equilibrium-conc 0.0 :type single-float) ; mM
  (tau 0.0 :type single-float)		; ms, for first-order fixed tau pumps
  (tau-array nil)			; ms, for first-order tau(V) pumps
  
  ;;; For :MM pumps
  (v-max 0.0 :type single-float)	; mM ms^-1 cm^-2
  (kd 0.0 :type single-float)		; mM

  ;; For :STATE-EQ pumps

  system-of-differential-equations

  (total-density 0.0 :type single-float) ; millimole/cm2

  ;;;        Rate constants for pump - ion interaction
  ;;;                            k-1
  ;;;               [P] + [X]i   <->     [PX]
  ;;;                            k-2
  ;;;
  ;;;                            k-3
  ;;;                    [PX]    <->  [P] + [X]o
  ;;;                            k-4
  ;;;
  ;;;   [P] is density of free pump in the membrane
  ;;;   [X]i and [X]o are the concentrations of ion X in the compartment and outside, respectively 		 
  ;;;   [PX] + [P] is defined by the total-density slot for the pump type.

  (k-1 0.0d0    :type double-float) 
  (k-2 0.0d0	:type double-float)
  (k-3 0.0d0	:type double-float) 
  (k-4 0.0d0	:type double-float)
  
  parameters
  (pumps () :type list)			; All the pumps of this type  
  )

;; For :STATE-EQ type pumps, we keep track of the concentration of free (unbound) pump

(defstruct (pump-double-floats (:type (vector double-float)))
  (density-n+1 0.0d0 :type double-float) ; Membrane density of free pump at prediction time step, mM/cm2
  (density-n 0.0d0 :type double-float)	; Membrane density of free pump at previous time step, mM/cm2
  (current 0.0d0 :type double-float)	; mM/ms
  )

(deftype pump-df-array () '(simple-array double-float (3)))

(defmacro pump-density-n+1 (pump) `(aref (the (simple-array double-float *) (pump-double-floats ,pump)) 0))
(defmacro pump-density-n (pump) `(aref (the (simple-array double-float *) (pump-double-floats ,pump)) 1))
(defmacro pump-current (pump) `(aref (the (simple-array double-float *) (pump-double-floats ,pump)) 2))

(defstruct (pump (:print-function (lambda (int stream depth)
				    (declare (ignore depth))
				    (format stream "<Pump ~a: type ~a>" (pump-name int) (pump-type-name (pump-type int))))))
  name
  type

  (blocked nil :type boolean)		; Pump is only advanced during the simulation when this is NIL.
  enabled-for-this-simulation    

  (area 0.0d0 :type double-float)	; um2

  ;;; For Michaelis-Menton pumps
  (basal-rate 0.0d0 :type double-float)	; mM/ms
  (mm-coefficent 0.0d0 :type double-float) ; mM/ms

  conc-int
  conc-int-compartment			; 1, 2 or 3
  parameters
  (double-floats (make-pump-double-floats) :type element-double-floats)	; For :STATE-EQ type pumps
  )

(setf (documentation 'pump-area 'function) " micron^2 (requires explicit structure)")

;; ********************************************************************
;;     Extracellular Electrode
;; ********************************************************************

(defstruct (extracellular-electrode (:print-function (lambda (elt stream depth)
						       (declare (ignore depth))
						       (format stream "<extracellular-electrode ~a>" (element-name elt)))))
  name
  (absolute-location '() :type list)	; (X Y Z) in brain coordinates
  parameters)



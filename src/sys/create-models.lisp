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


;;; SYS Source file: create-models.lisp
					
;; Now create all the models.

(in-package "SURF-HIPPO")

;; The order of the model creation sets the search priority for the ELEMENT function, e.g. when more than one element has the same name.
;; Note that element type models are created *after* the element model.

;; The first entry in DATA-TYPES-AND-ACCESS-INFO gives the default data type (e.g. as returned by DEFAULT-DATA-TYPE). If there are synonyms, take the first one.

(create-model 'segment
	      :parent-structure-type nil
	      :output-data-structure-variables nil
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO segment voltage "mV" recorded-element-voltage)
					    ,(DATA-TYPE-ACCESS-INFO segment (VOLTAGE-DERIVATIVE DVDT NODE-VOLTAGE-DERIVATIVE) "mV/ms" ELEMENT-dvdt)
					    ,(DATA-TYPE-ACCESS-INFO segment leak-current "nA")
					    ,(DATA-TYPE-ACCESS-INFO segment CAPACITANCE-CURRENT "nA" element-capacitance-current)))
(create-model 'soma
	      :parent-structure-type nil
	      :output-data-structure-variables
	      '(*all-save-voltage-nodes* *all-save-dvdt-nodes* *all-save-capacitance-current-nodes* *all-save-leak-current-nodes*
					 *plot-soma-dendrite-currents-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO soma voltage "mV" RECORDED-ELEMENT-VOLTAGE)
					    ,(DATA-TYPE-ACCESS-INFO soma (VOLTAGE-DERIVATIVE DVDT NODE-VOLTAGE-DERIVATIVE) "mV/ms" ELEMENT-dvdt)
					    ,(DATA-TYPE-ACCESS-INFO soma leak-current "nA")
					    ,(DATA-TYPE-ACCESS-INFO soma CAPACITANCE-CURRENT "nA" element-capacitance-current)
					    ,(DATA-TYPE-ACCESS-INFO soma dendrite-CURRENT "nA")))

(create-model 'cell :short-print-routine 'print-cell)
(create-model 'cell-type)


(create-model 'channel
	      :output-data-structure-variables
	      '(*plot-channel-currents-structures* *plot-channel-reversal-potentials-structures* *plot-channel-conductances-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO
	      ;; Channel current is not normally saved in the CHANNEL-CURRENT accessor slot, so need GET-CHANNEL-CURRENT function.
	      `(,(DATA-TYPE-ACCESS-INFO channel CURRENT "nA" get-channel-current)
		,(DATA-TYPE-ACCESS-INFO channel REVERSAL-POTENTIAL "mV")
		;; Specify GET-CHANNEL-SAVED-CONDUCTANCE to allow normalization of saved value to gbar if desired.
		,(DATA-TYPE-ACCESS-INFO channel CONDUCTANCE "uS" get-channel-saved-conductance)))

(create-model 'channel-type
	      :output-data-structure-variables
	      '(*plot-channel-type-currents-structures* *plot-channel-type-conductances-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO channel-type CURRENT "nA")
					    ,(DATA-TYPE-ACCESS-INFO channel-type CONDUCTANCE "uS")))



(create-model 'synapse
	      :output-data-structure-variables
	      '(*plot-synapse-currents-structures* *plot-synapse-reversal-potentials-structures* *plot-synapse-conductances-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO
	      ;; Synapse current is not normally saved in the SYNAPSE-CURRENT accessor slot, so need GET-SYNAPSE-CURRENT function.
	      `(,(DATA-TYPE-ACCESS-INFO synapse CURRENT "nA" get-synapse-current)
		,(DATA-TYPE-ACCESS-INFO synapse REVERSAL-POTENTIAL "mV")
		;; Specify GET-SYNAPSE-SAVED-CONDUCTANCE to allow normalization of saved value to gbar if desired.
		,(DATA-TYPE-ACCESS-INFO synapse CONDUCTANCE "uS" get-synapse-saved-conductance)
		,(DATA-TYPE-ACCESS-INFO synapse event "")))

(create-model 'synapse-type
	      :output-data-structure-variables
	      '(*plot-synapse-type-currents-structures* *plot-synapse-type-conductances-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO synapse-type CURRENT "nA")
					    ,(DATA-TYPE-ACCESS-INFO synapse-type CONDUCTANCE "uS")))


(create-model 'particle
	      :output-data-structure-variables '(*plot-particles-structures* *plot-markov-particles-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO particle state "State")
					    ,(DATA-TYPE-ACCESS-INFO particle markov-state "State")))
(create-model 'particle-type)


(create-model 'conc-particle
	      :output-data-structure-variables '(*plot-conc-particles-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO conc-particle state "State")))
(create-model 'conc-particle-type)


(create-model 'conc-int
	      :output-data-structure-variables
	      '(*plot-conc-1-ints-structures* *plot-conc-2-ints-structures* *plot-conc-3-ints-structures* *plot-conc-ints-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO conc-int (TOTAL TOTAL-CONCENTRATION) "mM")
					    ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-1 1 CONCENTRATION-1) "mM")
					    ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-2 2 CONCENTRATION-2) "mM")
					    ,(DATA-TYPE-ACCESS-INFO conc-int (SHELL-3 3 CONCENTRATION-3) "mM")))
(create-model 'conc-int-type)


(create-model 'isource
	      :output-data-structure-variables '(*plot-isource-currents-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO isource current "nA")))
(create-model 'isource-type)



(create-model 'vsource
	      :output-data-structure-variables '(*plot-vsource-currents-structures* *plot-vsource-voltages-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO vsource current "nA" get-vsource-current)
					    ,(DATA-TYPE-ACCESS-INFO vsource voltage "mV" get-vsource-current-vsrc-voltage)))
(create-model 'vsource-type)


(create-model 'axon
	      :output-data-structure-variables '(*plot-axon-voltages-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO axon voltage "mV")))
(create-model 'axon-type)


(create-model 'buffer
	      :output-data-structure-variables '(*plot-buffers-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO buffer CONCENTRATION "mM")))
(create-model 'buffer-type)


(create-model 'pump
	      :output-data-structure-variables '(*plot-pumps-structures*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO pump current "nA")))
(create-model 'pump-type)

;; (create-model 'electrode :parent-structure-type nil)

#|
(create-model 'extracellular-electrode
	      :parent-structure-type nil
	      :output-data-structure-variables '(*plot-field-potentials*)
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO extracellular-electrode FIELD-POTENTIAL "mV")))
|#

(create-model 'extracellular-electrode :parent-structure-type nil
	      :output-data-structure-variables '(*plot-extracellular-electrodes-structures*)
	      ;; :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO extracellular-electrode voltage "mV"))
	      :DATA-TYPES-AND-ACCESS-INFO `(,(DATA-TYPE-ACCESS-INFO extracellular-electrode field-potential "mV"))
	      )

(create-model 'node :parent-structure-type nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defun create-models ()
  `(progn
     ,@(mapcar 
	#'(lambda (sym)
	    (let ((create-function (read-from-string (format nil "create-~A-model" sym))))
	      (when (fboundp create-function) `(,create-function))))
	*model-names*)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following are either redefinitions of functions or macros, or resetting of documentation string of forms defined by evaluating CREATE-MODELS.

;; The definition of the function CELL-TYPES generated by evaluating CREATE-MODELS is redefined below.
(defun cell-types (&optional element)
  "Returns a list of all cell types associated with the cell elements referenced by ELEMENT. If there are no such cell elements,
or ELEMENT is NIL, then a list of all cell-types in circuit is returned."
  (let ((element-cell (element-cell element)))
    (if element-cell
	(coerce-to-list (element-type element-cell))
        (let ((cell-types (hash-table-list (CELL-TYPE-HASH-TABLE))))
	  (delete-if-not 'element-in-circuit cell-types)))))

(setf (documentation 'cell-type-def 'function)
      "Parameter wrapper for cell type library definitions.

Units of typical parameters include the following:

    RM              ohms-cm2 [default given by *RM*]
    RI              ohms-cm  [default given by *RI*]
    CM              uF/cm2   [default given by *CM*]
    V-LEAK          mV       [default given by *V-LEAK*]

Distinct values for somatic versus dendritic segment membrane may be specified using:

    RM-SOMA         ohms-cm2 [default given by RM parameter, otherwise *RM-SOMA*]
    RM-DENRITE      ohms-cm2 [default given by RM parameter, otherwise *RM*]
    CM-SOMA         uF/cm2   [default given by CM parameter, otherwise *CM*]
    CM-DENDRITE     uF/cm2   [default given by CM parameter, otherwise *CM-DENDRITE*]
    V-LEAK-SOMA     mV  [default given by V-LEAK parameter, otherwise *V-LEAK*]
    V-LEAK-DENDRITE mV  [default given by V-LEAK parameter, otherwise *V-LEAK-DENDRITE*]

Optional:

    E-NA            mV [default *E-NA*]
    E-K             mV [default *E-K*]
    E-CA            mV [default *E-CA*]
    E-CL            mV [default *E-CL*]
")

(setf (documentation 'channel-type-def 'function)
  "Parameter wrapper for channel type library definitions.

Units and formats of typical parameters include the following:

One of the following four parameters are required to specify gbar or pbar in terms of absolute value or density. GBAR or
GBAR-DENSITY parameters will apply the :OHMIC iv-relation model, and permeability or permeability-density parameters will apply
the :CONSTANT-FIELD iv-relation model:

    GBAR                    uS for conductance 
    GBAR-DENSITY            pS/um2 for conductance
    PERMEABILITY            cm3/s for permeability
    PERMEABILITY-DENSITY    1.0e-6cm3/s/um2 for permeability

Required:

    E-REV               mV
    PARTICLES           List of lists, each specifying particle type
                        and order, e.g. ((N-HH 2)(Y-HH 1)) (NB: Can also be designated V-PARTICLES

Required for :CONSTANT-FIELD model, optional for :OHMIC model:

    ION-PERMEABILITIES  List of lists, each specifying ion type and
                        permeability (total must add to unity), e.g. ((K 0.8)(NA 0.2))

Optional parameters:

    Q10               For conductance or permeability value, default 1
    REFERENCE-TEMP    Degrees C, for Q10
    GBAR-MODULATION   Optional. Applies to permeability model also.

    CONDUCTANCE-FUNCTION Function name with a single channel argument returning single float numeric value
                         that is multiplied by standard channel conductance value.

    STATIC-VOLTAGE-DEPENDENCE-FUNCTION  Function name or specification which returns a numeric sequence, or an explicit
                                 sequence, where the sequence is used as a look up table of voltage,
                                 corresponding to the values given by *PARTICLE-LOOK-UP-TABLE-MIN-VOLTAGE*,
                                 *PARTICLE-LOOK-UP-TABLE-MAX-VOLTAGE* and *PARTICLE-LOOK-UP-TABLE-PRECISION*,
                                 whose value is then multiplied by standard channel conductance value. Sequence
                                 must be single float.
")

(setf (documentation 'pump-type-def 'function)
  "Parameter wrapper for pump type library definitions.

Units and formats of typical parameters the include following:

     CLASS              :MM, :MM-ZADOR, :FIRST-ORDER, :FIRST-ORDER-TAU-V, :GENERIC

     PUMP-FUNCTION       For :GENERIC pumps, this function is called with args PUMP and concentration (mM),
                         and returns the pump current in millimole/ms.

     V-MAX              For :MM
     K-D                For :MM, :MM-ZADOR
     K-MAX, DENSITY     For :MM-ZADOR
     EQUILIBRIUM-CONC   For :FIRST-ORDER, :FIRST-ORDER-TAU-V

     TAU                For :FIRST-ORDER-TAU-V, Lambda form or function name with single voltage [mV] argument,
                        with return units of ms
                        For :FIRST-ORDER, number [ms]

     Q10               For kinetics, default 1.0
     REFERENCE-TEMP     Degrees C, for Q10
     SPECIES            Ionic species, e.g. NA, K, CA

")

(setf (documentation 'particle-type-def 'function)
  "Parameter wrapper for particle type library definitions.

Units and formats of typical parameters the include following:

     CLASS                       :HH, :HH-EXT, :MARKOV, and other
     CONCENTRATION-PARTICLE-TYPE Type symbol
     Q10                        For kinetics, default 1.0
     REFERENCE-TEMP              Degrees C, for Q10

For :HH-EXT class of particle types (required):

     VALENCE       particle valence, dimensionless
     GAMMA         dimensionless - between 0 and 1
     K             base rate constant, 1/ms
     V-HALF        mV

For :HH-EXT class of particle types (optional):

     TAU-0         ms
     ALPHA_0       V-indep forward rate constant [1/ms]
     BETA_0        V-indep backward rate constant [1/ms] 


For :MARKOV class of particle types:

     STATES
     OPEN-STATES
     STATE-TRANSITIONS

For :HH or :HH-EXT class of particle types:

     ALPHA      Lambda form or function name with single voltage [mV] argument,
                that return value in 1/ms, or number [1/ms].
     BETA       Same as for ALPHA

or

     SS         Lambda form or function name with single voltage [mV] argument,
                that return value between 0 and 1 inclusive, or number.
     TAU        Lambda form or function name with single voltage [mV] argument,
                that return value in milliseconds, or number [ms].

Explicit specifications of either SS and TAU will override :HH-EXT parameter
derived, ALPHA or BETA for the SS and Tau curves, respectively.

For either :HH-EXT or :HH class of particle types:

     LINEAR-MARKOV     (Linear-Markov-N-Value Linear-Markov-M-Value)
     FIXED-BOLTZMANN-REFERENCE-TEMP          degrees C
     IGNORE-TAU-VOLTAGE-DEPENDENCE           T or NIL [default]
     TAU-COEFFICIENT                         Number

For user defined particle type classes:

     EVALUATION-FUNCTION   This function should take one required argument, the particle type, and an optional
                           argument, a flag indicating initial state. The function should iterate over all
                           particles of the type and set the :STATE-N+1-DOUBLE slot of each. 

")

(setf (documentation 'synapse-type-def 'function)
  "Parameter wrapper for synapse type library definitions.

Units and formats of typical parameters the include following:

    CONTROL           :EVENT, :LIGHT, :CHANNEL, :VOLTAGE, :LIGHT-EVENT, :TONIC, other

One and only one of the following four parameters are required to specify gbar or pbar in terms of absolute value
or density. GBAR or GBAR-DENSITY parameters will apply the :OHMIC iv-relation model to the synapse type, and
permeability or permeability-density parameters will apply the :CONSTANT-FIELD iv-relation model:

    GBAR                    uS for conductance 
    GBAR-DENSITY            pS/um2 for conductance

    PERMEABILITY            cm3/s for permeability
    PERMEABILITY-DENSITY    1.0e-6cm3/s/um2 for permeability

Required:

    WAVEFORM-SPEC          Either numeric sequence, or a function symbol (if no required args) or
                           full funspec, either of which returns a numeric sequence.

    E-REV                  mV

Optional:

    SPECIFIED-WAVEFORM-BREAKPOINTS  List of times in milliseconds

    WAVEFORM-TIME-INTERVAL          In milliseconds, otherwise reference *DEFAULT-WAVEFORM-STEP*

    REFERENCE-TEMP                  degC

    ION-PERMEABILITIES              List of lists, each specifying ion type and permeability
                                    (total must add to unity), e.g. ((K 0.8)(NA 0.2))

    CONDUCTANCE-FUNCTION         Function name with a single synapse argument returning single float 
                                 numeric value that is multiplied by standard synapse conductance value.

    STATIC-VOLTAGE-DEPENDENCE    Function specification which returns a single float sequence, or an explicit
                                 single float sequence, where the sequence is used as a look up table of voltage,
                                 corresponding to the values given by *PARTICLE-LOOK-UP-TABLE-MIN-VOLTAGE*,
                                 *PARTICLE-LOOK-UP-TABLE-MAX-VOLTAGE* and *PARTICLE-LOOK-UP-TABLE-PRECISION*,
                                 whose value is then multiplied by standard synapse conductance value.

For user defined synapse type control:

     EVALUATION-FUNCTION   This function should take one required argument, the synapse type, and an optional
                           argument, a flag indicating first iteration. The function should loop over all
                           synapses of the type and apply the FINISH-SYN-EVAL macro to each.

")

(setf (documentation 'conc-int-type-def 'function)
  "Parameter wrapper for concentration integrator type library definitions.

Units and formats of typical parameters include the following:

    CLASS                     :MULTI-SHELL [default], :GENERAL, :FIRST-ORDER, :GENERIC, and other
    SPECIES		      Symbol for ionic species, e.g. 'CA, 'K, 'NA etc.
    VALENCE                   Integer
    INTRA-P                   Concentration integration for the intra (T [default]) or extracellular space (NIL)

For :MULTI-SHELL conc-ints:

    SHELL-2-P                    [default NIL]
    SHELL-3-P                    [default NIL]
    CORE-P                       [default NIL]
    INTERDIGITATION-COEFFICIENT  1/microns [between shells 1 and 2]

Lists of functions or explicit values for determining compartment parameters of :MULTI-SHELL conc-ints (see the User Manual for formats):

    DIFFUSION-COEFFICIENT        cm2/sec 
    VOLUMES                      um3     
    MEMBRANE-AREAS               um2     
    DIFFUSION-AREAS              um2     
    DIFFUSION-DISTANCES          um

If either VOLUMES, DIFFUSION-AREAS or MEMBRANE-AREAS are NIL, then the following parameters are used:

    JUXTAMEMBRANE-SHELL-THICKNESS  microns
    INNER-SHELL-THICKNESS          Thickness of shell 3, microns
    ALPHA-S                        Proportion of juxta-membrane shell assigned to shell 1.

    INSTANTANEOUS-BUFFER-ENABLED   Enables the instantaneous shell buffers.
    SHELLS-W-INSTANTANEOUS-BUFFER
    INSTANTANEOUS-BUFFER-RATIO

    Q10                           For diffusion kinetics, default 1.0
    REFERENCE-TEMP                 Degrees C, for Q10

Concentration reference values, in mM:

    TRANSMEMBRANE-CONCENTRATION
    RESTING-FREE-CONC    

For :FIRST-ORDER types:

    TAU                 milliseconds

For :GENERIC conc-ints, one of the following functions should be included, each of which must
take a single conc-int argument:

    CONC-FUNCTION       This must set the :SHELL-1-CONC-N+1 slot of the conc-int
    C-N+1-FUNCTION      Return value in mM that will be used to set the :SHELL-1-CONC-N+1 slot of the conc-int
    DCDT-FUNCTION	Return value in mM/ms (forward Euler integration)

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Edit slightly the documentation of the TYPE-QUOTED-DEF variants of the model TYPE-DEF macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flet ((set-type-quoted-def-doc (quoted-type-def ref-type-def)
	 (let* ((type-quoted-def-doc-snippet " that allows back-quoted TYPE-DEF constructions (form must be quoted or back-quoted). ")
		(original (documentation ref-type-def 'function)) ; Interestingly, doc strings as part of a DEFMACRO show up in the 'FUNCTION documentation
					; for the macro symbol.
		(marker-string "library definitions" ) 
		(insertion-point (+ (length marker-string) (search marker-string original)))
					; (new-tail (subseq original insertion-point))
		(new-head (subseq original 0 insertion-point)))
	   (setf (documentation quoted-type-def 'function)
		 (concatenate 'string
			      new-head
			      type-quoted-def-doc-snippet
			      (format nil "For units and formats of typical parameters, see ~A." ref-type-def)
					; new-tail
			      )))))
  (loop for model in (models) do 
	(let ((type-def-macro-name (generate-type-def-macro-name (model-name model))))
	  (when (fboundp type-def-macro-name)
	    (SET-TYPE-QUOTED-DEF-DOC
	     (generate-quoted-type-def-macro-name (model-name model))
	     type-def-macro-name)))))

;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

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


;;; SYS Source file: sys-loader.lisp
;;
;;; Loader file for the Surf-Hippo system files.

;;; This loader file was adapted from kr-loader.lisp, part of the Garnet project at CMU.                   

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Sys-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Sys-PathName before loading Sys."))

(Defvar Surf-Hippo-Sys-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "macros"
   "declare"				; Most of the global variables.
   "biophysics-declare"			; Variable values associated with reality, not the code.

   "structures"				; All of the structure definitions and some slot macros
   "structure-macros"

   "models"
   "create-models"
   "models-2"				; Various declarations which require that create-models.lisp and structures.lisp be loaded.
   "declare-2"				; Various declarations which require that declare.lisp, create-models.lisp and models-2.lisp be loaded.

   "element-macros"
;   "structure-data-functions"

   "element-functions-0" "element-functions-1" "element-functions-2"			

   "math"				; Some misc math functions
   "statistics"
   "filters"
   "fft"
   "randoms"
   "renewal-process"
   "waveforms"				; Must be before synapse.lisp

   "misc"

   "pump-preliminaries"			; Need inlined PUMP-CONCENTRATION-CURRENT for conc-ints.
   "conc-int"
   "biophysics"

   "matrix-system-solver"
   "sim"  "circuit-input"  "hines"
   
   "node" "soma" "segment"
   "source" "isource" "vsource"
   "electrode" "extracellular-electrode"
   "general-membrane-elements"
   "channel"				; some inline functions here are also used in synapse.lisp
   "particle"				; some inline functions here are used in conc-part.lisp
   "markov-particle"
   "conc-part"				

   ;; Synapse code
   "synapse-rf"				; NG code
;; "reduced-synapse"
   "synapse"  "light-synapse-functions"  "synapse-events"  "synapse-evaluation"
   ;;
   
   "buffer" "pump" "axon"

   "event-generators"			; applies to (at least) synapses and axons

   "cell"
   "cable-functions"

   "trees"

   "print"
   "analysis"
   "store-plot-data"

   ;; Graphics code
   "cell-graphics-setup"
   "cell-graphics-instances" ; GRAPES, VIRTUAL-SEGMENT, VIRTUAL-CELL-ELEMENT-MARKER, VIRTUAL-CELL-ELEMENT-MARKER-NO-BORDER, and VIRTUAL-SOMA
   "cell-graphics-instances-update-functions"
   "cell-graphics-hack-1"
   "cell-graphics-hack-2"
   "cell-graphics-user-functions"
   "ps-object-methods" ; Relies on virtual soma macros, as defined in cell-graphics-instances.lisp
   
   ;; "builder"

   "sparse-data"
   "colorizing"
   "info-hack"
   "plot"
   "3dplot"
   "trace-functions"

   "menus"

   "data-folder"

   "calc-lte-ratio"			; 1/2/94 Put here since refers to earlier macros.

   "init"
   "step"
   "hacks"			; Also includes some functions that refer to earlier macros.

   "update-models"			; This has to go after all model create routines have been defined.
   
   "raster-plot"
   "protocols"
   "sample-cells"
   "ntscable"
   "neurolucida"
   ;; "init-sys-on-load"
   "debug"
   ))


(compile-source-directory Surf-Hippo-sys-Src
			  Surf-Hippo-sys-pathname Surf-Hippo-Sys-Files
			  :files-to-force-compile-all '("declare" "structures" "structure-macros" "macros" "math" "element-macros")
			  ;; :files-to-touch '("models-2")
			  :enable-compile compile-sys-p)

(setf (get :surf-hippo-modules :sys)  t)




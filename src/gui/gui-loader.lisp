;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;; GUI Source file: gui-loader.lisp


;;
;;; Loader file for the Surf-Hippo gui files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")

;(defparameter Sys-Version-Number "2.2")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Gui-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Gui-PathName before loading Gui."))

(Defvar Surf-Hippo-Gui-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "macros"
   "utilities"
   "declare"
   "math"
   "strings"
   "sequences"
   "colors"				; Needs to be loaded before (at least) window-hack.lisp
   "linestyles"
   ;; "windows-hack_2_9o" ;;
   "windows-hack"
   "print-windows"
   "files"
   ;; "menu-hack_2_9o" ;;
   "menu-hack"
   "file-browser"
   "show-image"
   "plot-hack-declare"
   "virtual-things"			; VIRTUAL-PLOTTING-CIRCLE, VIRTUAL-RECTANGLE

					; VIRTUAL-CENTERED-H-LINE, VIRTUAL-CENTERED-V-LINE, VIRTUAL-X-CENTER, VIRTUAL-Y-CENTER,
					; VIRTUAL-X-LEFT, VIRTUAL-X-RIGHT, VIRTUAL-Y-TOP, VIRTUAL-Y-BOTTOM, VIRTUAL-LINESTYLE,
					; VIRTUAL-FILLSTYLE

					; VIRTUAL-IMMEDIATE-CENTERED-H-LINE, VIRTUAL-IMMEDIATE-CENTERED-V-LINE,
					; VIRTUAL-IMMEDIATE-X-CENTER, VIRTUAL-IMMEDIATE-Y-CENTER, VIRTUAL-IMMEDIATE-X-LEFT,
					; VIRTUAL-IMMEDIATE-X-RIGHT, VIRTUAL-IMMEDIATE-Y-TOP, VIRTUAL-IMMEDIATE-Y-BOTTOM,
					; VIRTUAL-IMMEDIATE-LINESTYLE, VIRTUAL-IMMEDIATE-FILLSTYLE

					; VIRTUAL-DOT, *VIRTUAL-CROSS-LINE-STYLE*, VIRTUAL-CROSS, VIRTUAL-TILTED-CROSS,
					; VIRTUAL-UP-TRIANGLE, VIRTUAL-DOWN-TRIANGLE, VIRTUAL-BOX, VIRTUAL-DIAMOND,
					; *SCATTER-SYMBOLS*, *SCATTER-SYMBOLS-CLOSED-CURVES*, *SCATTER-SYMBOLS-OPEN-CURVES*,
					; SCATTER-SYMBOL-TO-PROTOTYPE, SCATTER-PROTOTYPE-TO-SYMBOL,
					; SCATTER-PROTOTYPE-TO-NAMESTRING, VIRTUAL-POLYLINE

					; VIRTUAL-LINE, VIRTUAL-CIRCLE, and VIRTUAL-CIRCLE-W/O-BORDER
   
    "virtual-line-update"		; UPDATE-VIRTUAL-LINE function - :update-function of VIRTUAL-LINE and which calls
					; GEM::X-DRAW-LINE-FAST.
   
    "virtual-circle-update"		; DRAW-VIRTUAL-CIRCLE, UPDATE-VIRTUAL-CIRCLE function (:update-function of VIRTUAL-CIRCLE)
    "virtual-polyline-update"		; UPDATE-VIRTUAL-POLYLINE function (:update-function of VIRTUAL-polyline)
    

   "plot-hack"
   "plot-hack-top"
   "annotation-file"
   "tracer"
   ))


(compile-source-directory Surf-Hippo-gui-Src
			  Surf-Hippo-gui-pathname Surf-Hippo-gui-Files
			  :files-to-force-compile-all '("macros" "declare" "math" "plot-hack")
			  :enable-compile compile-gui-p)


(setf (get :surf-hippo-modules :gui)  t)



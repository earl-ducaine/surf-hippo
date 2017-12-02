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

;;; Loader file for the Surf-Hippo GARNET fixes files.

;;; This loader file was adapted from kr-loader.lisp, part of the Garnet project at CMU.                   

(in-package "USER")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Garnet-Fixes-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Garnet-Fixes-PathName before loading Garnet-Fixes."))

(Defvar Surf-Hippo-Garnet-Fixes-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "x"					; Fixing mouse wheel bug, courtesy of Edi Weitz. Add *debug-x-event-handler* code.
   ; Also; for T23 (Thinkpad), running RedHat 7.3, double button presses seem to be broken, so disable them by setting inter:*double-click-time* NIL.
   
   "defs"				; For setting :color-p of opal::color to T on X initialization.

;;   "color"				; Set :color-p of opal::color to T.
   
   "new-macros"				; FIXNUM-MAX, FIXNUM-MIN, FN-GV, and SF-GV macros (exported)
   
   #+GARNET-V3.0   "i-windows-fix"
					;   #-GARNET-V3.0
   
;;   "clx-hack"				; READ-CARD8, WRITER-CARD8 and related macros (from target:clx/buffer.lisp and
					; target:clx/bufmac.lisp). COMPARE-REQUEST and PUT-ITEMS macros, and GET-PUT-ITEMS
					; DRAW-LINE-FAST, DRAW-LINE-RELATIVE functions (from target:clx/macros.lisp)

   #-GARNET-V3.0 "clx-colorized-seg"

   "new-defs-hack"			; OPAL::DESTROY-ME-METHOD-VIEW-OBJECT, and MERGE-BBOX, BBOX-DIMS-DIFFER, UPDATE-BBOX,
					; BBOX-INTERSECT-P, BBOX-TO-CLIP-MASK macros. Also commented out version of
					; OPAL::DRAW-METHOD-POLYLINE. 
   
   #-GARNET-V3.0 "virtual-aggregates-hack-pre-3-0"

   #+GARNET-V3.0 "gem-hack"		; GEM::X-DRAW-LINE, GEM::X-DRAW-LINE-FAST (which calls XLIB::DRAW-LINE-FAST)


   #+GARNET-V3.0 "virtual-aggregates-hack"
   ;; BIT-SETTER, UPDATE-INFO-DIRTY-P, UPDATE-INFO-AGGREGATE-P, UPDATE-INFO-INVALID-P, UPDATE-INFO-FORCE-COMPUTATION-P,
   ;; UPDATE-INFO-ON-FASTDRAW-LIST-P, UPDATE-BBOX-FAST, GET-OLD-THICKNESS, DO-IN-CLIP-RECT, BBOX-TO-CLIP-MASK-COMPONENTS,
   ;; DO-IN-CLIP-RECT-COMPONENTS macros.

   ;; UPDATE-INFO-DIRTY-P, UPDATE-INFO-AGGREGATE-P, UPDATE-INFO-INVALID-P, UPDATE-INFO-FORCE-COMPUTATION-P,
   ;; UPDATE-INFO-ON-FASTDRAW-LIST-P defsetf forms.

   ;; SET-THINGS-SIZE, INITIALIZE-ITEM-BBOX, INITIALIZE-ITEM-BBOX-FAST, RECALCULATE-VIRTUAL-AGGREGATE-BBOXES,
   ;; COPY-FROM-BUFFER-TO-DRAWABLE functions.

   ;; UB29 deftype.

   ;; OPAL::DRAW-METHOD-CIRCLE, OPAL::DRAW-METHOD-LINE, OPAL::UPDATE-METHOD-VIRTUAL-AGGREGATE,
   ;; OPAL::UPDATE-METHOD-AGGREGATE.

   ;; OPAL::UPDATE-METHOD-VIRTUAL-AGGREGATE here looks for an :UPDATE-FUNCTION for the :DUMMY-ITEM, if not proceed generically.

   ;; src/sys/virtual-aggregate-update-method.lisp has another version (pre 3.0) with explicit branches for different types of graphic objects
   
   ;; COPY-FROM-BUFFER-TO-DRAWABLE, previously a macro(?) is now defined here as a function
   
 
   ;; For Garnet postscripts files
   #-GARNET-V3.0 "ps-virtual-methods"
   "make-ps-file-hack"			; DATE-STRING, WRITE-PS-INCLUDE-EXTRA-INFO, WRITE-PS-TO-FILE, MAKE-PS-FILE, PS-WINDOW
					; functions. *PRINT-NON-VISIBLE-WINDOWS* 

   #-GARNET-V3.0 "misc-garnet-fixes"
   #-GARNET-V3.0 "raise-lower-wind-fix" ; 8/21/98 LBG Make sure that this is not needed in v3.0

   #+GARNET-V3.0 "virtual-aggregate-w-aggregadgets-fix"	; OPAL::UPDATE-METHOD-GRAPHICAL-OBJECT,
							; OPAL::INITIALIZE-METHOD-VIRTUAL-AGGREGATE,
							; OPAL::MAKE-VIRTUAL-AGG-BBOX-ARRAY, OPAL::DESTROY-ME-METHOD-VIRTUAL-AGGREGATE

   "register-fns"			; 11/4/98
   ;; OPAL::PARSE-ARBITRARY-FILL-PATTERN-REQUIRED-PS-FNS, OPAL::FULL-PARSE-PS-REGISTER-FN, OPAL::REGISTER-FNS-IN-WIN,
   ;; OPAL::PS-REGISTER-FN-METHOD-VIRTUAL-AGGREGATE, OPAL::PS-REGISTER-FN-METHOD-AGGREGADGET

;;   "make-image"				; chmod of image file to 755
  
   "garnet-pathnames"			; If DIR doesn't exist, default to *SURF-HOME*.
   "utils"				; A small change to garnet-restart-function
   ;; LG Change 16.11.2016
   "opal-hack"				; smallest-SIGNED-BYTE-16, largest-SIGNED-BYTE-16, LIMIT-NUM-TO-SIGNED-BYTE-16, :update opal::window, validate-window-reference-slot

   ))

(defvar garnet-fixes-files-to-force-compile-all nil)



(setq garnet-fixes-files-to-force-compile-all '("clx-hack"
						"new-macros"
						"new-defs-hack"
						#-GARNET-V3.0 "virtual-aggregates-hack"))
  
(compile-source-directory Surf-Hippo-garnet-Fixes-Src
			  Surf-Hippo-garnet-Fixes-pathname Surf-Hippo-garnet-Fixes-Files
			  :files-to-force-compile-all garnet-fixes-files-to-force-compile-all
			  :enable-compile compile-garnet-fixes-p)

(setf (get :surf-hippo-modules :garnet-fixes)  t)


(ext::gc) ; for some reason loading a compiled system chokes after the clmath sys - this prevents that??

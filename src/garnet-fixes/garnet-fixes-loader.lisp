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

(in-package :garnet-user)


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Garnet-Fixes-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Garnet-Fixes-PathName before loading Garnet-Fixes."))

(Defvar Surf-Hippo-Garnet-Fixes-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "x"
   "defs"
   "new-macros"
   "i-windows-fix"
   "new-defs-hack"
   "virtual-aggregates-hack"
   "make-ps-file-hack"
   "virtual-aggregate-w-aggregadgets-fix"
   "register-fns"
   "garnet-pathnames"
   "utils"
   "opal-hack"
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

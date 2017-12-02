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



;;; Loader file for the Surf-Hippo cmucl-fixes files.

;;; This loader file was adapted from kr-loader.lisp, part of the Garnet project at CMU.                   

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Cmucl-Fixes-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Cmucl-Fixes-PathName before loading Cmucl-Fixes."))

(Defvar Surf-Hippo-Cmucl-Fixes-Files ; We use a defvar rather than defparameter so that a setq of this variable before loading this 
					; file will override the definition here, if we want to compile only some of these files.
  (list

   #+cmu20D "garnet-x-fix"		; Verified 28.08.2016

  ;;VERIFY From CMUCL Event Fix suggested by Andrew Mickish (see Garnet README)
   ; "serve-event"			

   ;;  Optimized xlib::draw-arc
;   "clx-fixes"

   #|
   ;; VERIFY this is a fix to cmucl suggested by Andrew.Mickish@a.gp.cs.cmu.edu
   "fix-1-13-92"


 
   ;; a-mickish again, 2/22/94, for lisp crashing on a remote server
   "fd-stream-read-n-bytes"


   ;; for the mit cmucl environment (???)
   "coerce"
   ;; just a terser output %time function.
   "time-w-new-output"
   ;; for solaris
   "run-time"
   ;;  complex-fix breaks on 20D
 #-cmu20D  "complex-fix"
   ;; BREAKS 20D? a simple optimization of the "with hash-value..." construct.

   #-(or cmu20D cmu18) "hash-looper"
   ;; Redefinition of UNPROFILE macro.
   "profile"				
   ;; Fix to RANDOM from Pierpaolo Bernardi
   #-new-random   "rand-mt19937-small-int" 
|#
   ;; optimize one kind of truncate
   #+(or cmu20 cmu18) "numbers"
   #+(or cmu20 cmu18) "18a-fix-merge-types-aux"

   ))

(compile-source-directory Surf-Hippo-cmucl-Fixes-Src
			  Surf-Hippo-cmucl-Fixes-pathname Surf-Hippo-cmucl-Fixes-Files
			  :enable-compile compile-cmucl-fixes-p)

(setf (get :surf-hippo-modules :cmucl-fixes)  t)


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


;; Loader file for the Surf-Hippo Retina files.

;;; This loader file was adapted from kr-loader.lisp, part of the Garnet project at CMU.

(in-package "USER")

(defparameter Retina-Version-Number "1.0")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Retina-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Retina-PathName before loading Retina."))


;; ---- Load Retina files

(Defvar Surf-Hippo-Retina-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  '(
    "star-amacrine"
    "star-amacrine-functions"
;;    "network"
    ))


(compile-source-directory Surf-Hippo-retina-Src
			  Surf-Hippo-retina-pathname Surf-Hippo-Retina-Files
			  :enable-compile compile-retina-p)


(setf (get :surf-hippo-modules :retina)  t)



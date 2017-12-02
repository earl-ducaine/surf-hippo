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


(in-package "USER")


;(defparameter Roylance-Clmath-Version-Number "1.0")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Roylance-Clmath-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Roylance-Clmath-PathName before loading Roylance-Clmath."))


;; ---- Load Roylance-Clmath files

(Defvar Surf-Hippo-Roylance-Clmath-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  '(

    "attrib"
    "gamma"
    "bessel"
    "horner"
    "beta"
    "import"
    "binomial"
    "integr"
    "bisection"
    "marq"
    "combin"
    "matrix"
    "matrix-double"
    "consts"
    "mod"
    "dft"
;;    "modules"
    "ellip"
    "poisson"
    "erf"
    "regres"
    "factor"
    "falsep"
    "fib"
    "runge"
    "fit"
    "statis"
    "fmfp"


    
    ))

(compile-source-directory Surf-Hippo-roylance-clmath-Src
			  Surf-Hippo-roylance-clmath-pathname Surf-Hippo-roylance-clmath-Files
			  :enable-compile compile-roylance-clmath-p)



(setf (get :surf-hippo-modules :roylance-clmath)  t)



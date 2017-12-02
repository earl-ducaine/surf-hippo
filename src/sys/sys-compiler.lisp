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


;;; SYS Source file: sys-compiler.lisp

;; Loader file for the Surf-Hippo System files.

;;; This loader file was adapted from kr-compiler.lisp, part of the Garnet project at CMU.

(in-package "USER")

;; (break)
;;(proclaim '(optimize (speed 3)(safety 0)(space 0)))
(with-compilation-unit (:optimize `(optimize (speed 3)(safety 0)(space 0)(extensions:inhibit-warnings 0)))
  (dolist (file Surf-Hippo-Sys-Files)
    (compile-file (merge-pathnames file Surf-Hippo-Sys-Src))
    #+(or allegro explorer lispworks lucid cmu)
    (load (merge-pathnames file Surf-Hippo-Sys-Src))))


#+(or allegro explorer lispworks lucid cmu)
(setf (get :surf-hippo-modules :sys) T)




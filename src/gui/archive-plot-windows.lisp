;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

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


;; GUI Source file: archive-plot-windows.lisp



(in-package "SURF-HIPPO")


(defun archive-plot-windows (&optional windows)
  (loop for win in (or windows (win-menu "Choose Windows to Archive" (windows-of-mode '(:standard-plot :2dplot)))) do
	(typecase (g-value win :mode)
	  (:standard-plot
	   (plot-xy-data
	    (list (g-value win :x-lists) (g-value win :y-lists))
	    (g-value win :label-list))
	    
		       

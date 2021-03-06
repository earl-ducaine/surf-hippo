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


;; Parameters for some Cl- concentration integrators.


(in-package "SURF-HIPPO")

; Concentration shell occupies entire element volume.
(conc-int-type-quoted-def
 `(cl-in
   (class . :first-order)
   (species . cl)
   (valence . -1)
   (intra-p . t)
   (tau . 100)				; milliseconds (to be determined)
   (diffusion-coefficient . ,*D_Cl*)	; Units for diffusion coefficients are cm^2 sec^-1
   (core-conc . ,*cl-conc-intra*)))


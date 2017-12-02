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

(in-package "SURF-HIPPO")

#|

Soma - short cable reverse engineered geometry from: 

@ARTICLE{Bro-Fri-Per-81,
	Author  = {Brown, T. H. and Fricke, R. A. and Perkel, D. H.},
  Title   = {Passive electrical constants in three classes of hippocampal neurons},
  Journal = {Journal of Neurophysiology},
  Year    = {1981},
  Volume  = {46},
  Number  = {4},
  Pages   = {812-827},
  Month   = {October}
  }
|#

(cell-type-def
 '(Brown-81
   (rm . 19000)
   (ri  . 75)
   (cm . 1)))

(defun brown-81 ()
  (add-isource
   (cell-soma (ball-and-sticks :name "Brown-81"
			       :soma-diameter (* 2 42)
			       :apical-dendrite-length 1800
			       :apical-dendrite-diameter (* 2 3)
			       :apical-total-segs 20
			       :cell-type 'Brown-81))))


#|
some results from this structure:

* (electrotonic-length 1800.0 6.0 75.0 1.0 19000.0)
0.9233805
* 
* (rho)
1.1856153
* (print-element *cell*)
Cell Brown-81 (soma @ [0.0 0.0 0.0])  -  Created from BROWN-81
      20 Segments, 1 Trunk, 1 Terminal, Total Membrane Area 5.61e+4um^2
      Passive R-in from soma (actual / cable model) = 39.22 / 38.86 Mohms 
       Max G-in / Min R-in = 0.03 uS / 33.87 Mohms 
       Soma passive R-in = 85.71 Mohms
       Dendritic tree passive R-in (actual / cable model) = 72.29 / 71.08 Mohms
       Coupling R's from individual compartments [single-leg]
NIL
*

tau-0 from simulation:

* (/ 77.834  4.1)
18.983902
* 

|#

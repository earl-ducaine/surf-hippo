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

Derived from -

@PHDTHESIS{Zad-93,
	AUTHOR = {Zador, A. M.},
	TITLE = {Biophysics of computation in single hippocampal neurons},
	SCHOOL = {Yale University},
	YEAR = {1993}
}

see also surf-hippo/src/parameters/zador-chs.lisp

|#


(cell-type-def
 '(AMARAL-CA1
   (rm . 150000)			; also 15e3
   (ri . 200)
   (cm . 1)
   (v-leak . -70)))

(setq *nts-cell-type* 'AMARAL-CA1
      *nts-cell-name* "tz_c12861.ca1")
		  
(read-in-circuit (concatenate 'string *surf-user-dir* "/" "anatomy/misc/" "c12861.ca1.lisp"))
	 
(create-element *soma* 'dr-tz-ca1-hh 'na-tz-ca1-hh)

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
These modifications to the original Working Model defintions (parameters/working-hpc.lisp) allow the
Ca++ concentration integration system to be installed on an arbitrary cell element, with the element
dimension dependent parameters taken as if the element was the soma (35um diameter) of the original
model. When set to non-NIL the ABSOLUTE-MODEL parameter of the function

           ADD-WORKING-HPC-CHANNELS (&optional (element *soma*) absolute-model)

enables these substitutions.

|#

#|
(conc-int-type-def
 (let* ((working-model-soma-radius (/ 35.0 2))
	(working-model-soma-area (sphere-area working-model-soma-radius))
	(working-model-soma-volume (sphere-volume working-model-soma-radius))
	(interdigitation-coefficient 1.0)
	(alpha-s 10.0e-5))
   `(CA-IN-HPC-ABSOLUTE
     (parent-type . CA-IN-HPC)

     ;; These formulas recapitulate those for the :MULTI-SHELL class.

     (membrane-areas . ((1 ,(* alpha-s working-model-soma-area))
			(2  ,(* (- 1 alpha-s) working-model-soma-area))))
     (volumes . ((1 ,(* alpha-s working-model-soma-area interdigitation-coefficient))
		 (2 ,(* (- 1 alpha-s) working-model-soma-area interdigitation-coefficient))
		 (3 ,(- working-model-soma-volume (* working-model-soma-area interdigitation-coefficient)))))
     (diffusion-areas . (((1 2) ,(* interdigitation-coefficient working-model-soma-area))
			 ((2 3) ,working-model-soma-area))))))
			 

			 




(let* ((working-model-soma-radius (/ 35.0 2))
       (working-model-soma-area (sphere-area working-model-soma-radius))
       (working-model-soma-volume (sphere-volume working-model-soma-radius))
       (interdigitation-coefficient 1.0)
       (alpha-s 10.0e-5)
       (body `(CA-IN-HPC-ABSOLUTE
	       (parent-type . CA-IN-HPC)

	       ;; These formulas recapitulate those for the :MULTI-SHELL class.

	       (membrane-areas . ((1 ,(* alpha-s working-model-soma-area))
				  (2  ,(* (- 1 alpha-s) working-model-soma-area))))
	       (volumes . ((1 ,(* alpha-s working-model-soma-area interdigitation-coefficient))
			   (2 ,(* (- 1 alpha-s) working-model-soma-area interdigitation-coefficient))
			   (3 ,(- working-model-soma-volume (* working-model-soma-area interdigitation-coefficient)))))
	       (diffusion-areas . (((1 2) ,(* interdigitation-coefficient working-model-soma-area))
				   ((2 3) ,working-model-soma-area))))))
  (list `conc-int-type-def body
		     ))

|#
			 
(let* ((working-model-soma-radius (/ 35.0 2))
       (working-model-soma-area (sphere-area working-model-soma-radius))
       (working-model-soma-volume (sphere-volume working-model-soma-radius))
       (interdigitation-coefficient 1.0)
       (alpha-s 10.0e-5))
  (conc-int-type-quoted-def
   `(CA-IN-HPC-ABSOLUTE
     (parent-type . CA-IN-HPC)

     ;; These formulas recapitulate those for the :MULTI-SHELL class.

     (membrane-areas . ((1 ,(* alpha-s working-model-soma-area))
			(2  ,(* (- 1 alpha-s) working-model-soma-area))))
     (volumes . ((1 ,(* alpha-s working-model-soma-area interdigitation-coefficient))
		 (2 ,(* (- 1 alpha-s) working-model-soma-area interdigitation-coefficient))
		 (3 ,(- working-model-soma-volume (* working-model-soma-area interdigitation-coefficient)))))
     (diffusion-areas . (((1 2) ,(* interdigitation-coefficient working-model-soma-area))
			 ((2 3) ,working-model-soma-area))))))

(channel-type-def
 (KAHP-HPC-ABSOLUTE
   (parent-type . KAHP-HPC)
   (conc-particles . ((KAHPO-HPC-ABSOLUTE 2)))))

(conc-particle-type-def
 '(KAHPO-HPC-ABSOLUTE
   (parent-type . KAHPO-HPC)
   (conc-int-type . CA-IN-HPC-ABSOLUTE)))

(channel-type-def
 '(CA-L-HPC-ABSOLUTE
   (parent-type . CA-L-HPC)
   (conc-int-type-params . ((CA-IN-HPC-ABSOLUTE (1 1.0d0))))))

(channel-type-def
 '(CA-T-HPC-ABSOLUTE
   (parent-type . CA-T-HPC)
   (conc-int-type-params . ((CA-IN-HPC-ABSOLUTE (1 1.0d0))))))

(channel-type-def
 '(CA-N-HPC-ABSOLUTE
   (parent-type . CA-N-HPC)
   (conc-int-type-params . ((CA-IN-HPC-ABSOLUTE (1 1.0d0))))))

(channel-type-def
 '(KCT-HPC-ABSOLUTE
   (parent-type . KCT-HPC)
   (v-particles . ((KCTX-HPC-ABSOLUTE 1)))))

(particle-type-def
 `(KCTX-HPC-ABSOLUTE
   (parent-type . KCTX-HPC)
   (concentration-particle-type . KCTX-HPC-CA-ABSOLUTE)))
   
(conc-particle-type-def
 '(KCTX-HPC-CA-ABSOLUTE
   (parent-type . KCTX-HPC-CA)
   (conc-int-type . CA-IN-HPC-ABSOLUTE)))


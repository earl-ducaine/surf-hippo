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

;; From the article:
;; A Model For Dendritic Ca++ Accumulation in Hippocampal Pyramidal Neurons Based on Fluorescence
;; Imaging Measurements
;; D. B. Jaffe, W. N. Ross, J. E. Lisman, N. Lasser-Ross, H.Miyakawa, D. Johnston
;; J. Neurophysiol. 1994, vol. 71, no. 3, 1065 - 1077

;; see also parameters/jaffe94-chs.lisp 
	 
(defun jaffe94-hippo ()
  (hippo)
  (process-circuit-structure)		; So we can work with the processed geometry.
  (let* ((apical-segments (segments-out (car (trunk-segments))))
	 (na-segs (loop for seg in apical-segments
			when (< (distance-to-soma seg) 0 ; apical-extent-of-na-k
				)
			collect seg)))
    (CELL-TYPE-PARAMETER (cell-type *cell*) :rm 60000)
    (create-element na-segs *soma* 'na-Jaffe94)
    (jaff94-plot-settings-hippo)
    (create-element (cell-elements) '(ca-t-Jaffe ca-n-Jaffe ca-l-Jaffe kdr-Jaffe94 kc-Jaffe94))))

(cell-type-def
 '(JAFFE94-HPC
   (rm . 60000)				; ohms-cm2
   (ri  . 200)				; ohm-cm
   (cm . 1)				; uF/cm2
   (v-leak . -65))			; mV
 )

(defun jaffe94-nak-soma (&optional (apical-extent-of-na-k 150))
  (let ((cell (ca1-max-red :cell-type 'JAFFE94-HPC :name "jaffe94-c12861-ca1")))
    (process-circuit-structure)
    (let* ((apical-segments (segments-out (element "46"))) ; By eye.
	   (na-segs (loop for seg in apical-segments
			  when (< (distance-to-soma seg) apical-extent-of-na-k)
			  collect seg)))
      (add-isource *soma*)
      (create-element *soma* na-segs 'na-Jaffe94)
      (create-element (cell-elements)
		      '(ca-t-Jaffe ca-n-Jaffe ca-l-Jaffe kdr-Jaffe94 kc-Jaffe94))))
  ;; These give a pretty good trade off of speed/accuracy.
  (setq *RELATIVE-VOLTAGE-ERROR 0.5
	*RELATIVE-PARTICLE-ERROR* 0.05)
  (jaff94-plot-settings))


(defun jaff94-plot-settings ()
  (setq *plot-channels-by-major-ion* t)
  (setq *PLOT-NODES* 
	`( "106" "144"))
  (setq *PLOT-SOMA-NODES* 
	`( "jaffe94-c12861-ca1-soma"  )
	)

  (setq *PLOT-CHANNEL-CURRENTS* 
	`("106-KC-JAFFE94"
	  "106-KDR-JAFFE94"
	  "106-CA-L-JAFFE"
	  "106-CA-N-JAFFE" "106-CA-T-JAFFE" "144-KC-JAFFE94"
	  "144-KDR-JAFFE94" "144-CA-L-JAFFE" "144-CA-N-JAFFE"
	  "144-CA-T-JAFFE"
	  "144-NA-JAFFE94"  "jaffe94-c12861-ca1-soma-KC-JAFFE94"
	  "jaffe94-c12861-ca1-soma-KDR-JAFFE94"  "jaffe94-c12861-ca1-soma-CA-L-JAFFE"
	  "jaffe94-c12861-ca1-soma-CA-N-JAFFE"  "jaffe94-c12861-ca1-soma-CA-T-JAFFE"
	  "jaffe94-c12861-ca1-soma-NA-JAFFE94"  )  
	)
  (setq *PLOT-CHANNEL-CONDUCTANCES* 
	`("106-KC-JAFFE94"  "106-KDR-JAFFE94"
	  "106-CA-L-JAFFE"  "106-CA-N-JAFFE"
	  "106-CA-T-JAFFE"  "144-KC-JAFFE94"
	  "144-KDR-JAFFE94"  "144-CA-L-JAFFE"
	  "144-CA-N-JAFFE"  "144-CA-T-JAFFE"    
	  "144-NA-JAFFE94"  "jaffe94-c12861-ca1-soma-KC-JAFFE94"
	  "jaffe94-c12861-ca1-soma-KDR-JAFFE94"  "jaffe94-c12861-ca1-soma-CA-L-JAFFE"
	  "jaffe94-c12861-ca1-soma-CA-N-JAFFE"  "jaffe94-c12861-ca1-soma-CA-T-JAFFE"
	  "jaffe94-c12861-ca1-soma-NA-JAFFE94"  )  )

  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS* 
	`( "106-KC-JAFFE94"  "106-KDR-JAFFE94"
	  "106-CA-L-JAFFE"  "106-CA-N-JAFFE"
	  "106-CA-T-JAFFE"  "144-KC-JAFFE94"
	  "144-KDR-JAFFE94"  "144-CA-L-JAFFE"
	  "144-CA-N-JAFFE"  "144-CA-T-JAFFE"   
	  "144-NA-JAFFE94"  "jaffe94-c12861-ca1-soma-KC-JAFFE94"
	  "jaffe94-c12861-ca1-soma-KDR-JAFFE94"  "jaffe94-c12861-ca1-soma-CA-L-JAFFE"
	  "jaffe94-c12861-ca1-soma-CA-N-JAFFE"  "jaffe94-c12861-ca1-soma-CA-T-JAFFE"
	  "jaffe94-c12861-ca1-soma-NA-JAFFE94"  )  )


  (setq *PLOT-CONC-1-INTS* 
	`( "106-CA-IN-JAFFE-94"  "144-CA-IN-JAFFE-94"
	  "jaffe94-c12861-ca1-soma-CA-IN-JAFFE-94"  ) )

  (setq *PLOT-NODE-VOLTAGES-P* T)
  (setq *PLOT-NODE-VOLTAGE-DERIVATIVES-P* NIL)
  (setq *PLOT-PATH-NODE-VOLTAGES-P* NIL)
  (setq *PLOT-AXON-VOLTAGES-P* NIL)
  (setq *PLOT-AXON-EVENTS-P* NIL)
  (setq *PLOT-SYNAPSE-EVENTS-P* NIL)
  (setq *PLOT-SOMA-VOLTAGE-P* T)
  (setq *PLOT-SOMA-VOLTAGE-DERIVATIVE-P* NIL)
  (setq *PLOT-SOMA-DENDRITE-CURRENTS-P* NIL)
  (setq *PLOT-CHANNEL-CURRENTS-P* T)
  (setq *PLOT-CHANNEL-CONDUCTANCES-P* NIL)
  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS-P* NIL)
  (setq *PLOT-SYNAPSE-CURRENTS-P* NIL)
  (setq *PLOT-SYNAPSE-CONDUCTANCES-P* NIL)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-P* T)
  (setq *PLOT-SHELL-2-CONCENTRATIONS-P* NIL)
  (setq *PLOT-SHELL-3-CONCENTRATIONS-P* NIL)
  (setq *PLOT-CONCENTRATIONS-P* NIL)
  (setq *PLOT-BUFFER-CONCENTRATIONS-P* NIL)
  (setq *PLOT-CONC-PARTICLES-P* NIL)
  (setq *PLOT-PARTICLES-P* NIL)
  (setq *PLOT-PUMP-CURRENTS-P* NIL)
  (setq *PLOT-ISOURCE-CURRENTS-P* NIL)
  (setq *PLOT-VSOURCE-CURRENTS-P* NIL))



(defun jaff94-plot-settings-hippo ()
  (setq *plot-channels-by-major-ion* t)
  (setq *PLOT-NODES* 
	`( "Hippo-5"  )
	)

  (setq *PLOT-SOMA-NODES* 
	`( "Hippo-soma"  )
	)

  (setq *PLOT-CHANNEL-CURRENTS* 
	`( "Hippo-soma-KC-JAFFE94"  "Hippo-soma-KDR-JAFFE94"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-NA-JAFFE94"  ) 
	)

  (setq *PLOT-CHANNEL-CONDUCTANCES* 
	`( "Hippo-soma-KC-JAFFE94"  "Hippo-soma-KDR-JAFFE94"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-NA-JAFFE94"  ) 
	)

  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS* 
	`( "Hippo-soma-KC-JAFFE94"  "Hippo-soma-KDR-JAFFE94"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-NA-JAFFE94"  ) 
	)

  (setq *PLOT-CONC-1-INTS* 
	`( "Hippo-soma-CA-IN-JAFFE-94"  )
	)

  (setq *PLOT-ISOURCE-CURRENTS* 
	`( "Hippo-soma-isrc"  )
	)

  (setq *PLOT-NODE-VOLTAGES-P* T)
  (setq *PLOT-NODE-VOLTAGE-DERIVATIVES-P* NIL)
  (setq *PLOT-PATH-NODE-VOLTAGES-P* NIL)
  (setq *PLOT-AXON-VOLTAGES-P* NIL)
  (setq *PLOT-AXON-EVENTS-P* NIL)
  (setq *PLOT-SYNAPSE-EVENTS-P* NIL)
  (setq *PLOT-SOMA-VOLTAGE-P* T)
  (setq *PLOT-SOMA-VOLTAGE-DERIVATIVE-P* NIL)
  (setq *PLOT-SOMA-DENDRITE-CURRENTS-P* NIL)
  (setq *PLOT-CHANNEL-CURRENTS-P* T)
  (setq *PLOT-CHANNEL-CONDUCTANCES-P* NIL)
  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS-P* NIL)
  (setq *PLOT-SYNAPSE-CURRENTS-P* NIL)
  (setq *PLOT-SYNAPSE-CONDUCTANCES-P* NIL)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-P* T)
  (setq *PLOT-SHELL-2-CONCENTRATIONS-P* NIL)
  (setq *PLOT-SHELL-3-CONCENTRATIONS-P* NIL)
  (setq *PLOT-CONCENTRATIONS-P* NIL)
  (setq *PLOT-BUFFER-CONCENTRATIONS-P* NIL)
  (setq *PLOT-CONC-PARTICLES-P* NIL)
  (setq *PLOT-PARTICLES-P* NIL)
  (setq *PLOT-PUMP-CURRENTS-P* NIL)
  (setq *PLOT-ISOURCE-CURRENTS-P* NIL)
  (setq *PLOT-VSOURCE-CURRENTS-P* NIL))



  

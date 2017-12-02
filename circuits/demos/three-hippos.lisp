;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Laboratoire Neurophysique et Physiologie du Système Moteur, CNRS
UMR 8119, UFR Biomédicale de l'Université René Descartes, Paris
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;; This demonstrates a toy network with three spiking hippocampal pyramidal cells, connected with various synaptic types. The initial stimulus is provided
;; by a current source to the soma of one cell.


(in-package "SURF-HIPPO")

(defun three-HIPPOs ()
  (let* ((cell1 (working-hpc :name "Hippo-1" :origin '(-300 0 100)))
	 (cell2 (working-hpc :name "Hippo-2"  :origin '(0 200 -300)))
	 (cell3 (working-hpc :name "Hippo-3"  :origin '(400 -200 0)))
	 (axon-1-3 (create-axon (cell-soma cell1) 'simple :mid-points '((-210 -100 200)	(0 -250 100) (280 35 -200) (200 820 -100)))))
    (setf (axon-type-propagation-velocity (element-type axon-1-3)) 20.0)
    (create-synapse "Hippo-3-4" 'fast-ex-alpha-voltage axon-1-3)
    (create-synapse "Hippo-2-4" 'fast-ex-alpha-voltage (cell-soma cell1))
    (events (create-synapse "Hippo-1-5" 'fast-ex-alpha) (poisson-events 0.2 40 100))

    (element-type-parameter 'fast-ex-alpha-voltage :gbar-source :absolute)
    (element-type-parameter 'fast-ex-alpha-voltage :gbar-ref 0.05)
	
    (pulse-list (add-isource (cell-soma cell1)) '(10 20 2))

    (setq *user-stop-time* 250)

    (plot-segments-to-soma "Hippo-1-5")
    (plot-segments-to-soma "Hippo-2-5")
    (plot-segments-to-soma "Hippo-3-5")

    (enable-element-plot `(soma axon ,(cell-element 'synapse)))
    (enable-element-plot `(axon synapse) 'event)

    (setq *group-plot-data-by-cell* nil)

    (set-type-graphics 'fast-ex-alpha-voltage 'yellow)
    (set-type-graphics 'fast-ex-alpha 'cyan)
  
    nil))


(circuit-load 'three-HIPPOs)

(just-draw)

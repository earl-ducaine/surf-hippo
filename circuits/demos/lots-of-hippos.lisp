;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;;                   The Surf-Hippo Neuron Simulator                                ;;; 
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                  ;;; 
;;; This code was written as part of the Surf-Hippo Project at Center for Biological ;;; 
;;; Information Processing, Department of Brain and Cognitive Sciences,              ;;; 
;;; Massachusetts Institute of Technology, and currently at the Unite de             ;;; 
;;; Neurosciences Integratives et Computationnelles, Institut Federatif de           ;;; 
;;; Neurobiologie Alfred Fessard, CNRS.                                              ;;; 
;;;                                                                                  ;;; 
;;; Permission to use, copy, modify, and distribute this software and its            ;;; 
;;; documentation for any purpose and without fee is hereby granted, provided that   ;;; 
;;; this software is cited in derived published work, and the copyright notice       ;;; 
;;; appears in all copies and in supporting documentation. The Surf-Hippo Project    ;;; 
;;; makes no representations about the suitability of this software for any          ;;; 
;;; purpose. It is provided "as is" without express or implied warranty.             ;;; 
;;;                                                                                  ;;; 
;;; If you are using this code or any part of Surf-Hippo, please contact             ;;; 
;;; surf-hippo@ai.mit.edu to be put on the mailing list.                             ;;; 
;;;                                                                                  ;;; 
;;; Copyright (c) 1989 - 2003, Lyle J. Graham                                        ;;;                                                       
;;;                                                                                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A bunch of passive hippos on an XZ grid, with some scatter in the Y dimension. This file should be CIRCUIT-LOADed into Surf-Hippo.

;; Generate a bunch of passive hippos on an XZ grid. 
(let ((count 0)				; Use this variable to count the created cells.
      (x-index-min -25)
      (x-index-max 25)
      (z-index-min -10)
      (z-index-max 10))
  (loop for x from x-index-min to x-index-max do
	(loop for z from z-index-min to z-index-max do
	      (let ((*cell-name-suffix* (format nil "-~D" (incf count)))) ; Add a suffix to the created cell names.
		(move-cell
		 (dead-hippo)		; DEAD-HIPPO returns the created cell.
		 (list (* x 200)	; New X location in microns.
		       (- (random 200) 100) ; New (random) Y location in microns taken from a uniform PDF from -100 to 100.
		       (* z 400))))))	; New Z location in microns.
  )


;; Barneyize 30 percent hippos at random.
(color-cell-element (random-nth (cells) (* 0.30 (length (cells)))) 'purple)

;; Shift the drawing view to see the entire grid and draw.
(just-draw :scale 20.0			; microns/pixel
	   :phi-deg 40.0
	   :theta-deg 40.0)



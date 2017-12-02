;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

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


;;; SYS Source file: synapse-rf.lisp
(in-package "SURF-HIPPO")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code written by Nicolas Gazeres.
;;;
;;; Functions for efficient light synapse RF convolutions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;    (when circular-dog-p (format T " Note: RF Spatial Mask is a CIRCULAR DOG.~%"))
;;   (when circular-gaussian-p (format T " Note: RF Spatial Mask is a CIRCULAR GAUSSIAN.~%"))        
;; (when (and circular-dog-p circular-gaussian-p)(format T "ERROR : more than one light-predicate is true.~%"))
  
  ;;----------------------------------------
  ;; Optimizations for CIRCULAR DOG RFs
  ;;----------------------------------------
  ;; Static Bar
;;;  ((and circular-dog-p
;;;        (get-a-value 'OPTIMIZE  params)
;;;        (find *light-stimulus '(:BAR :OFF-BAR :ON-BAR)))                    
;;;   (format T "Optimizing ~A for Circular DOG.~%" (synapse-name syn))
;;;   (when (DOG-STATIC-BAR-COMPUTE-WAVEFORM syn)
;;;                        (compare-light-synapse-waveform syn)))
;; (element-parameter (car (synapse-types)) 'FREQUENCY-FACTOR 1.0)
;; (setq *light-stimulus-start-time* 50.0)
;; (setq a1 (list-to-array (loop for time from 0.0 to 500.0
;;    collect (* 1000.0 (- (creneau-*-expo time 1.0 10.0) (* 1.0 (creneau-*-expo time 1.0 15.0)))))))
;; (setq a1 (list-to-array a1))
;;(let ((alpha 2.0))
;;  (plot-timed-data (loop for time from 0.0 to 20.0 by 0.5
;;                         collect (- 1.0 (* (1+ (/ time alpha)) (exp (- (/ time alpha))))))))
;;      (CIRCULAR-DOG-P
;;       (and (eq (car filter) 'DOG)
;;            (= (second rf-info) (third rf-info))
;;            (= (fourth rf-info) (fifth rf-info))))



(defun 1-expo-x2/2 (x)  (- 1.0 (exp (- (* x x 0.5)))))


(defun T-MAX-DO-GAUSSIANS (diameter tau-1 tau-2 sigma-1 sigma-2 rapport-1/2)
  (let* ((a (/ (1-expo-x2/2 (/ diameter 2.0 sigma-2))
	       (1-expo-x2/2 (/ diameter 2.0 sigma-1))))
	 (b (/ tau-1 tau-2 rapport-1/2))
	 (c (* a b))
	 (d (log c)))
    (/ d (- (/ 1.0 tau-2) (/ 1.0 tau-1)))))


(defun VAL-OF-DOG-1 (time diameter tau-1 tau-2 sigma-1 sigma-2 rapport-1/2)
  (- (* rapport-1/2 (- 1.0 (exp (- (/ time tau-1)))) (1-expo-x2/2 (/ diameter 2.0 sigma-1)))
     (* (- 1.0 (exp (- (/ time tau-2)))) (1-expo-x2/2 (/ diameter 2.0 sigma-2)))))


(defun VAL-OF-DOG-2 (time diameter tau-1 tau-2 sigma-1 sigma-2 factor-1 factor-2)
  (- (* factor-1 (- 1.0 (exp (- (/ time tau-1)))) (1-expo-x2/2 (/ diameter 2.0 sigma-1)))
     (* factor-2 (- 1.0 (exp (- (/ time tau-2)))) (1-expo-x2/2 (/ diameter 2.0 sigma-2)))))



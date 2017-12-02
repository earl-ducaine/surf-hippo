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


;;; SYS Source file: statistics.lisp

(in-package "SURF-HIPPO")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptive Statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plot-sequence-pdf (sequence &key (bins 20) x-max x-min x-are-fns (x-axis-tick-skip 3))
  (plot-histogram sequence :bins bins :x-max x-max :x-min x-min
		  :x-axis-tick-skip x-axis-tick-skip
		  :x-are-fns x-are-fns
		  :annotate-all t))

(defun construct-pdf (xy-samples &key (x-inc 1) histogram-x-min histogram-x-max plot-pdf-histogram return-pdf-xy-lists
				 x-are-fns y-are-fns pdf-title x-label y-label x-axis-tick-skip (y-axis-tick-skip 0) y-inc)
  ;; Consider a non-ordered list XY-SAMPLES sample pairs of a function f(x) = y,
  ;;       ((x0 y0) (x1 y1) ... (xn yn))
  (loop for xy-value in xy-samples
	maximizing (nth 0 xy-value) into x-max
	minimizing (nth 0 xy-value) into x-min
	finally (return (let* ((pdf-length (ceiling (/ (- x-max x-min) x-inc)))
			       (pdf-array (make-array (list pdf-length))))
			  (loop for xy-value in xy-samples
				do (incf (aref pdf-array (min (1- pdf-length) (round (/ (- (nth 0 xy-value) x-min) x-inc)))) (nth 1 xy-value))
				finally
				(return
				 (let ((y-list
					(loop for y-total across pdf-array
					      sum y-total into pdf-integral
					      collect y-total)))
				   (when plot-pdf-histogram
				     (plot-histogram
				      y-list
				      :x-are-fns x-are-fns :y-are-fns y-are-fns
				      :bin-width x-inc :y-inc y-inc
				      :x-axis-tick-skip x-axis-tick-skip :y-axis-tick-skip y-axis-tick-skip
				      :title pdf-title
				      :x-label x-label
				      :y-label y-label
				      :x-min (or histogram-x-min x-min)
				      :x-max (or histogram-x-max x-max)))
				   (when return-pdf-xy-lists xy-lists))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mean-sd (list)
  (loop for value in list
	summing (float value) into sum
	summing (square (float value)) into sum-sq
	finally
	(let* ((data-length (length list))
	       (mean (/ sum data-length))
	       (var-p (- (/ sum-sq data-length) (square (/ sum data-length))))
	       (var-s (* (/ data-length (1- data-length)) var-p))
	       (sd (sqrt var-p)))
	  (format t "~%This set of ~a values has a mean = ~a (SEM=~a) and sd = ~a (var-s = ~a, var-p = ~a).~%"
		  data-length mean (/ sd (sqrt data-length)) sd var-s var-p))))

(defvar *t-values* '(
		   (6.314		; 10% significance (two-tailed), from 1 to 10 df
		    2.920
		    2.353
		    2.132
		    2.015
		    1.943
		    1.895
		    1.860
		    1.833
		    1.812)
		   (12.706		; 5% significance (two-tailed), from 1 to 10 df
		    4.303
		    3.182
		    2.776
		    2.571
		    2.447
		    2.365
		    2.306
		    2.262
		    2.228)
		   (63.657		; 1% significance (two-tailed), from 1 to 10 df
		    9.925
		    5.841
		    4.604
		    4.032
		    3.707
		    3.499
		    3.355
		    3.250
		    3.169)))

(defun get-t-value (pct-index df)
    (setq df (min df (length (nth pct-index *t-values*))))
    (nth (1- df) (nth pct-index *t-values*)))


;; We need this test since the data collection may underestimate the mean square for small means
;; (rounding error?).
(defun sample-s-sq (mean mean-of-sq n)
  (if (<= n 1)
      0
      (max 0 (/ (* n (- mean-of-sq (square mean))) (1- n)))))


(defun t-of-diff (mean1 mean-of-sq1 mean2 mean-of-sq2 n &optional (mu-diff 0))
;  "Returns the t of the difference."
  (let ((denominator (sqrt (+ (sample-s-sq mean1 mean-of-sq1 n) (sample-s-sq mean2 mean-of-sq2 n)))))
    (unless (= denominator 0)
      (/ (* (sqrt n) 
	    (abs (- (- mean1 mean2) mu-diff)))
	 denominator))))

(defun t-of-diff-from-sde (mean1 standard-error-sq1 mean2 standard-error-sq2 n &optional (mu-diff 0))
  (let ((denominator (sqrt (+ standard-error-sq1 standard-error-sq2))))
    (unless (= denominator 0)
      (/ (* (sqrt n) 
	    (abs (- (- mean1 mean2) mu-diff)))
	 denominator))))


(defun t-value (data-list &optional (mu0 0))
    (format t "The t-value = ~a, with ~a df.~%"
	    (/ (* (sqrt (length data-list))
		  (abs (- (/ (apply '+ data-list)
			     (float (length data-list))) mu0)))
	       (sample-s data-list))
	    (1- (length data-list))))

(defun sample-s (data-list)
  "Returns the sample sigma of the values in DATA-LIST."
  (let ((mean (/ (apply '+ data-list)
		 (float (length data-list)))))
    (sqrt (/ (loop for x in data-list summing (square (- x mean)))
	     (1- (float (length data-list)))))))



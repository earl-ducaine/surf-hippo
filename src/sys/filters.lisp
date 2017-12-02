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


;;; SYS Source file: filters.lisp

(in-package "SURF-HIPPO")

;; Some intial attempts at linear filter routines.


;; CONVOLVE-INPUT-ARRAY INPUT-ARRAY may be a one dimensional or two dimensional single-float array - the latter case requires a
;; fixnum :2D-INPUT-INDEX argument to specify the 1d slice (first array dimension) of the input that will be used for the
;; convolution. This function returns an array whose length is equal to the length of a 1d INPUT-ARRAY or the length of the second
;; array dimension of a 2d INPUT-ARRAY. IMPULSE is a one-dimensional single-float array. A nonlinearity may be applied to the
;; result of the convolution - the meaning of the :NONLINEARITY and :NONLINEARITY-PARAMETERS arguments correspond to the
;; :NONLINEARITY and :PARAMETER arguments of the function NONLINEARITY defined above.
;;
;; Whenever any of the following are not equal to -1, then CONVOLVE-INPUT-ARRAY uses them to speed
;; up the convolution by avoiding integrations that will just give 0 output:
;;
;;   :FIRST-IMPULSE
;;   :FIRST-INPUT
;;   :LAST-IMPULSE
;;   :LAST-INPUT
;;
;; CONVOLVE-INPUT-ARRAY is called by SETUP-LIGHT-SYNAPSES. Array arguments must be SIMPLE-ARRAYs.
(defun convolve-input-array (impulse input-array &key nonlinearity (nonlinearity-parameters 0.0)
				     2d-input-index
				     2d-output-index destination-array
				     (constant-input-from-negative-infinity
				      *constant-light-input-from-negative-infinity*) 
				     (plot-output *plot-convolve-input-array-output*)
				     (first-impulse -1) (first-input -1) (last-impulse -1) (last-input -1))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((sum 0.0)
	 (initial-input-value (if 2d-input-index
				  (aref (the 2dfloat input-array) 2d-input-index 0)
				  (aref (the 1d-flt input-array) 0)))
	 (impulse-length (length (the 1d-flt impulse)))
	 (input-length (if 2d-input-index
			   (array-dimension (the 2dfloat input-array) 1)
			   (array-dimension (the 1d-flt input-array) 0))))
    (declare (single-float sum initial-input-value))
    (declare (fixnum first-impulse first-input last-impulse last-input))
    (unless destination-array (setq destination-array (make-array input-length :element-type 'single-float)))
    (cond-every
     ((= first-impulse -1) (setq first-impulse 0))
     ((= first-input -1) (setq first-input 0))
     ((and constant-input-from-negative-infinity (not (= 0.0 initial-input-value))) (setq first-input (- impulse-length)))
     ((= last-impulse -1) (setq last-impulse (1- (length impulse))))
     ((= last-input -1) (setq last-input (1- input-length))))
    (dotimes (i (the fn input-length))
      (declare (fixnum i))
      (setq sum 0.0)
      (do ((j first-input (1+ j)))
	  ((>= j (min last-input (the fn (1+ i)))))
	(declare (fixnum j))
	(let ((i-j (the fn (- i j))))
	  (declare (fixnum i-j))
	  (when (and (< i-j impulse-length) (<= first-impulse i-j last-impulse))
	    (setq sum (+ sum (* (if (< j 0)
				    initial-input-value
				    (if 2d-input-index
					(aref (the 2dfloat input-array) 2d-input-index j)
					(aref (the 1d-flt input-array) j)))
				(aref (the 1d-flt impulse) i-j)))))))
      
      (if 2d-output-index
	  (setf (aref (the 2dfloat destination-array) (the (UNSIGNED-BYTE 32) 2d-output-index) i)
		(nonlinearity sum nonlinearity nonlinearity-parameters))
	  (setf (aref (the 1d-flt destination-array) i)
		(nonlinearity sum nonlinearity nonlinearity-parameters))))
    (when plot-output
      (plot-timed-data
       (if 2d-output-index
	   (loop for i from 0 to (1- (array-dimension (the 2dfloat destination-array) 1))
		 collect (aref (the 2dfloat destination-array) 2d-output-index i))
	   destination-array)
       nil nil :title "*plot-convolve-input-array-output*")
      (break))
    destination-array))

(defvar *plot-convolve-input-array-output* nil)

(defun 3-point-avg (wave)
  ;; Triangle weighting
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-1 nil this-x)
	  (x-2 nil x-1)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (when x-2
	(push (/ (+ (* 0.5 x-2) x-1 (* 0.5 this-x)) 2.0) output)))))

(defun 3-point-boxcar (wave)
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-1 nil this-x)
	  (x-2 nil x-1)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (when x-2
	(push (/ (+ x-2 x-1 this-x) 3.0) output)))))

(defun 5-point-avg (wave)
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-1 nil this-x)
	  (x-2 nil x-1)
	  (x-3 nil x-2)
	  (x-4 nil x-3)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (cond
       (x-4
	(push (/ (+ (* 0.33 x-4) (* 0.67 x-3) x-2 (* 0.67 x-1) (* 0.33 this-x)) 3.0) output))
       (x-3 (push x-3 output))
       (x-2 (push x-2 output))
       (this-x (push this-x output))))))

(defun 5-point-boxcar (wave)
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-4 nil x-3)
	  (x-3 nil x-2)
	  (x-2 nil x-1)
	  (x-1 nil this-x)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (cond
       (x-4
	(push (/ (+ x-4 x-3 x-2 x-1 this-x) 5.0) output))
       (x-3 (push x-3 output))
       (x-2 (push x-2 output))
       (this-x (push this-x output))))))

(defun boxcar (wave boxcar-length)
  (let ((output))
    (do ((wave wave (cdr wave)))
	((null wave) (reverse output))
      (push (/ (loop for val in wave
		     repeat boxcar-length
		     sum val)
	       boxcar-length)
	    output))))

(defun boxcar-sf (wave boxcar-length)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((output))
    (do ((wave wave (cdr wave)))
	((null wave) (reverse output))
      (push (/ (loop for val single-float in wave
		     repeat (the fn boxcar-length)
		     sum val into result single-float
		     finally (return result))
	       boxcar-length)
	    output))))
      
(defun 4-point-boxcar (wave)
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-3 nil x-2)
	  (x-2 nil x-1)
	  (x-1 nil this-x)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (cond
       (x-3
					;	   (format t "x-3 ~a, x-2 ~a, x-1 ~a, this-x ~a~%" x-3 x-2 x-1 this-x)
	(push (/ (+ x-3 x-2 x-1 this-x) 4.0) output))
       (x-2 (push x-2 output))
       (this-x (push this-x output))))))

(defun 5-point-boxcar-sf (wave)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((output))
    (do* ((wave wave (cdr wave))
	  (x-1 nil this-x)
	  (x-2 nil x-1)
	  (x-3 nil x-2)
	  (x-4 nil x-3)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (cond
       (x-4
	(push (/ (+ (the sf x-4)(the sf x-3)(the sf x-2)(the sf x-1)(the sf this-x)) 5.0) output))
       (x-3 (push x-3 output))
       (x-2 (push x-2 output))
       (this-x (push this-x output))))))

(defun 1-pole (original-data original-time cutoff-freq sample-freq &optional offset-w-first-sample)
  (let* (
	 ;; (original-dt .1)
	 ;; (sine-freq .1)			;khz
	 ;; (sine-duration 50)		;msec
	 ;; (w (* 2 pi-single sine-freq))
	 ;; (cutoff-freq .02)			;khz
	 (dt (/ 1.0 sample-freq))
	 ;; (original-data (loop for time from 0.0 to sine-duration by original-dt
	 ;;            collect (sin (* time w))))
	 ;; (original-time (loop for time from 0.0 to sine-duration by original-dt
	 ;; 		    collect time))
	 (*create-new-plot-windows* t)
	 (tau (/ 1.0  cutoff-freq
					; (* 2 pi-single cutoff-freq)
		 ))
	 (initial-value (/ (exp (- (/ 0.0 tau)))
			   (/ tau dt)))
	 (value initial-value)
	   
	 (impulse (list-to-array (loop for time from 0.0 by dt
				       until (< value (* 0.01 initial-value))
				       collect (setq value (/ (exp (- (/ time tau)))
							      (/ tau dt))))))
	 (impulse-time (loop for time from 0.0 by dt
			     for i from 0 to (length impulse)
			     collect time))
	 (impulse-integral (apply '+ (array-to-list impulse)))       
	 (data-time (convert-data-time-lists original-data original-time dt t t))
	 (sample-time (nth 1 data-time))
	 (sample-data (nth 0 data-time))
	 (dc-bias (if offset-w-first-sample (first sample-data)))
	 (filter-data (progn
			(loop for i from 0 to (1- (length impulse))
			      do (setf (aref impulse i) (/  (aref impulse i) impulse-integral)))


			(array-to-list (convolve impulse (list-to-array sample-data) :threshold nil
						 :dc-bias dc-bias))
					; (sinc-filter-list data 1 cutoff)
			))
	 (filter-name (format nil "Cutoff ~akhz" cutoff-freq))
	 (original-name (format nil "original sampled @~Akhz" (/ 1.0  dt))))
	  
					;    (print (apply '+ (array-to-list impulse)))
    (plot-xy-data 
     (list  (list sample-time filter-data)
	    (list sample-time sample-data)
	    (list original-time original-data)
	    (list  impulse-time (array-to-list impulse)))
     (list  filter-name 
	    original-name "Original" "Impulse")
     :title (concatenate-strings filter-name "-" original-name) :height 600 :width 600)))

(defun get-triangle-kernel (support)
  ;; assume support is odd, returns triangle list with unit area
  (let* ((list
	  (loop for i from 0 to (1- (round support))
		collect (float (if (<= i (/ (1- (round support)) 2))
				 i
				 (- (1- (round support)) i)))))
	 (integral (apply '+ list)))
    (loop for i in list
	  collect (/ i integral))))

(defun filter-list (data time-step kernel-break-freq)
  ;; freq units are the inverse of the time step units
  (let ((kernel-width (/ 1.0 (* time-step kernel-break-freq))))
    (convolver data (get-triangle-kernel (1+ (* 2.0 kernel-width))))))

(defun sinc-filter-list (data time-step kernel-break-freq)
  (let* ((sample-freq (/ 1.0 time-step)))
    ;; (break)
    (convolver data (get-sinc-kernel sample-freq kernel-break-freq))))

(defun get-sinc-kernel (f-s f-c)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((f-s (float f-s))
	 (f-c (float f-c))
	 (length (* 2.0 (round (the sf (* 5 (the sf (/ f-s f-c)))))))
	 (length/2 (the sf (/ length 2.0)))
	 (sinc-co (the sf (* 0.5 (the sf (/ f-c f-s)))))
	 (list  (loop for i single-float from 0.0 to length collect (the sf (sinc (- i length/2) sinc-co))))
	 (integral (the sf (apply '+ list))))
    (loop for value in list collect (the sf (/ (the sf value) integral)))))

(defun sinc (x f)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x f))
  (if (= x 0)
      (* 2.0 f)
      (/ (sin (* f x (* 2 pi-single))) (* pi-single x))))

(defun get-flat-kernel (support)
  (loop for i from 1 to support collect (/ 1.0 support)))

(defun convolver (list1 list2)
  (let* ((array-1 (list-to-array list1))
	 (array-2 (list-to-array list2)))
    (array-to-list (convolve-input-array array-2 array-1))))

(defun convolver (list1 list2)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((array-1 (the 1d-flt (s-flt-array list1)))
	 (array-2 (the 1d-flt (s-flt-array list2)))
	 (length1 (the fn (length list1)))
	 (length2 (the fn (length list2)))
	 (length1-1 (the fn (1- length1)))
	 (length2-1 (the fn (1- length2)))
	 (half-length2 (round (/ length2 2)))
	 (shifted-filtered-data
	  (loop for i fixnum from 0 to length1-1 collect
		(loop for j fixnum from 0 to length2-1
		      summing (* (aref array-2 (1- (- length2 j))) (aref array-1 (if (< i j) (- (+ i length1-1) j) (- i j))))
		      into result single-float finally (return result)))))
    (loop for i fixnum from 0 to length1-1
	  collect (nth (mod (the fn (+ i half-length2)) length1-1) shifted-filtered-data))))

(defun convolve (impulse input &key threshold dc-bias output-length resting-value return-list 
			 first-non-zero-impulse-idx first-non-zero-input-idx
			 last-non-zero-impulse-idx last-non-zero-input-idx)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((sum 0.0)
	 (dc-bias (if dc-bias (float dc-bias) 0.0))
	 (input-array (s-flt-array input))
	 (input-length (the fn (length (the 1d-flt input-array))))
	 (impulse-array (s-flt-array impulse))
	 (impulse-length (the fn (length (the 1d-flt impulse-array))))
	 (impulse-area (loop for x across impulse-array sum x))
	 (output-length (the fn (or output-length (+ -1 impulse-length input-length))))
	 (output-array (make-array output-length :element-type 'single-float))
	 (first-non-zero-impulse-idx (or first-non-zero-impulse-idx 0))
	 (first-non-zero-input-idx (or first-non-zero-input-idx 0))
	 (last-non-zero-impulse-idx (or last-non-zero-impulse-idx (1- impulse-length)))
	 (last-non-zero-input-idx (or last-non-zero-input-idx (1- input-length))))
    (declare (single-float sum))      
    (when (numberp resting-value)
      (loop for index from 0 to (1- input-length)
	    do (setf (aref input-array index) (- (aref input-array index) resting-value))))
    (dotimes (i (the fn output-length))
      (declare (fixnum i))
      (setq sum dc-bias)
      (do ((j (the fn first-non-zero-input-idx) (1+ j)))
	  ((> j (min (the fn last-non-zero-input-idx) (the fn (1+ i)))))
	(declare (fixnum j))
	(let ((i-j (the fn (- i j))))
	  (declare (fixnum i-j))
					; (format t "i ~A, i-j ~a ~%" i i-j)
	  (when (and (< i-j impulse-length)
		     (<= (the fn first-non-zero-impulse-idx) i-j (the fn last-non-zero-impulse-idx)))
					; (format t "  summing input array j ~A, impulse i-j ~a ~%" j i-j)
	    (setq sum (+ sum (* (aref (the 1d-flt input-array) j) (aref (the 1d-flt impulse-array) i-j)))))))
      (setf (aref (the 1d-flt output-array) i) (if threshold (max (the sf threshold) sum) sum)))
    
    (when (numberp resting-value)
      (let ((convolved-resting-value (* resting-value impulse-area)))
	(loop for index from 0 to (1- output-length)
	      do (setf (aref output-array index) (+ (aref output-array index) convolved-resting-value)))))
    (if return-list
      (s-flt-list output-array)
      output-array)))


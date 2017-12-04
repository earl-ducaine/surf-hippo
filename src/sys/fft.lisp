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


;;; SYS Source file: fft.lisp

(in-package "SURF-HIPPO")

#|
user::dft-init
user::dft-array
user::dft-forward
user::dft-reverse
|#

(in-package "USER")
(export '(dft-init
	  dft-array
	  dft-forward
	  dft-reverse))

(in-package "SURF-HIPPO")

(defvar *enable-dft-tables-hash-table* t)
(defvar *dft-tables* (make-hash-table :test #'equal))
(defun load-*dft-tables* ()
  (loop for log-size from 1 to 20
     do (setf (gethash log-size *dft-tables*)
	      (roylance-clmath::dft-init log-size))))
(load-*dft-tables*)

(defun get-dft-tables (dft-input-sequence-or-length)
  (let* ((wave-length (if (numberp dft-input-sequence-or-length) dft-input-sequence-or-length (length dft-input-sequence-or-length)))
	 (log-size  (ceiling (log wave-length 2))))
    (or (and *ENABLE-DFT-TABLES-HASH-TABLE* (gethash log-size *dft-tables*))
	(roylance-clmath::dft-init log-size))))

;; Pads WAVE with PAD until its length reaches 2^N, where N is an integer.
(defun dft-wave (wave &key (pad 0.0))
  (let* ((wave (sequence-to-list wave))
	 (log-size (ceiling (log (length wave) 2)))
	 (n        (expt 2 log-size))
	 (tables   (get-dft-tables wave))
	 (x-mag   (make-array n :element-type 'dft-float))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float)))
    (declare (fixnum log-size n)
	     (type (dft-array (*)) x-real x-imag))
    (loop for i from 0
	  for val in wave do
	  (setf (aref x-real i) val)
	  (setf (aref x-imag i) 0.0))
    (loop for i from (length wave) to (1- n) do
	  (setf (aref x-real i) pad)
	  (setf (aref x-imag i) 0.0))
    (plot-timed-data x-real nil nil :title "Time")
    (dft-forward x-real x-imag tables)
    (loop for i from 0 to (1- n) do
	  (setf (aref x-mag i) (sqrt (+ (square (aref x-real i))
					(square (aref x-imag i))))))
    (plot-timed-data x-real nil nil :title "Frequency Real")
    (plot-timed-data x-mag nil nil :title "Frequency Mag" :y-min 0.0)
    (plot-timed-data x-imag nil nil :title "Frequency Imag")
    (dft-reverse x-real x-imag tables)))



#|


(setq wave  g t)
(dft-stretch-wave wave)
(dft-stretch-wave (list-head
		   (mapcar '+
			   (sequence-to-list (sinewave 1 4300 0.4 :step 0.1))
			   (sequence-to-list (sinewave 1 4300 4 :step 0.1)))
					; 2048
		   )
		  :plot-input-wave t
		  :delta-t 0.1)


;; Random process from uniform distribution, filtered.
(defun foo ()
  (let* ((delta-t 0.125)
	 (impulse (exponential-array-unit-area 10 delta-t))
	 (wave (loop for time from 0 by delta-t to (1- (* (expt 2 15) delta-t))
		     collect (- (random 1.0) 0.5))))
    (dft-stretch-wave (sequence-head (convolve impulse wave) (- (length wave) (length impulse)))
		      :delta-t delta-t
		      :remove-dc t
		      :plot-input-wave t)))

(defun foo ()
  (let* ((delta-t 0.125)		; ms
	 (frequency 500)		; hz
	 (impulse (exponential-array-unit-area 10 delta-t))
	 (wave (loop for time from 0 by delta-t to (1- (* (expt 2 11) delta-t))
		     collect (+ (* 0.5
					; (sin (* 2 pi frequency time 0.001))
				   (signum (sin (* 2 pi frequency time 0.001)))
				   )
					; (- (random 1.0) 0.5)
				))))
    (dft-stretch-wave wave		; (sequence-head (convolve impulse wave) (- (length wave) (length impulse)))
		      :delta-t delta-t
		      :remove-dc t
		      :plot-input-wave t)))


|#

#|
  "Plot the frequency magnitude (when PLOT-MAG [default T], normalized if NORMALIZE-MAG [default
NIL], up to MAX-FREQ [hz, default NIL] if non-NIL) and phase (when PLOT-PHASE [default T]) of the
time sequence INPUT-WAVE (plotted if PLOT-INPUT-WAVE [default NIL]), which has a time step DELTA-T
[milliseconds, default 1.0]. If REMOVE-DC [default NIL] then the DC component is set to zero;
otherwise a DC-OFFSET [default 0.0] is applied to INPUT-WAVE before the dft. Plot titles include the
string TITLE-PREFIX [default NIL]. This function processes any length of INPUT-WAVE by resampling the time
sequence to fit a 2^N array that may be DFTed. A list of strings in LABELS is used to label plot
traces. Note that if a time sequence must be resampled then the effective delta-t will be smaller
than the given DELTA-T. Also, this resampling is done by simple linear interpolation between known
points, and thus may introduce spurious high frequencies into the spectrum. Maximum length of a time
sequence in INPUT-WAVE is 2^16.

Given a INPUT-WAVE input, if RETURN-WAVES is T [default NIL] then returns as values lists for the input and outputs in the form:

 (INPUT-WAVE MAG PHASE REAL-PART IMAG-PART FREQUENCIES)

The LOG-PLOT-MAG and LOG-PLOT-FREQ arguments control plotting of the magnitude and phase transform."
|#

#|
(defun dft-stretch-wave (input-wave &key (delta-t 1.0) (dc-offset 0.0) remove-dc normalize-mag max-freq log-plot-mag log-plot-freq
			 label title-prefix plot-input-wave (plot-mag t) (plot-phase t) plot-real-wave plot-imag-wave plot-imaginary-wave-in
			 (forward t) (time-shift 0) unfold-phase return-waves imaginary-wave)
  (let* ((wave-length (length input-wave))
	 (log-size (ceiling (log wave-length 2))))
    (when (> log-size 16)
      (sim-error (format nil "DFT can only handle log2(N) <= 16 (here rounded exponent is ~D, for wave-length ~A)" log-size wave-length)))
    (let* ((title-prefix (string (or title-prefix "")))
	   (data-x-label (if forward "ms" "Hz"))
	   (txfmed-x-label (if forward "Hz" "ms"))
	   (true-max-freq (/ 1 (* 2 0.001 delta-t)))
	   (tables   (get-dft-tables wave-length)) ; Need this below in DFT-FORWARD.
	   (n        (expt 2 log-size))
	   (x-real   (make-array n :element-type 'dft-float :initial-element 0.0)) ; Default is 0.0
	   (x-imag   (make-array n :element-type 'dft-float :initial-element 0.0))
	   (freq-step (/ 1.0 (* wave-length 0.001 delta-t)))) ; hz
      (declare (fixnum log-size n)
	       (type (dft-array (*)) x-real x-imag))
      (let* ((dc-offset (if remove-dc 0.0 (s-flt dc-offset)))
	     (wave (if (zerop dc-offset) (s-flt-array input-wave) (sequence-to-float-list-w-offset input-wave dc-offset)))
	     (stretched-wave (CONVERT-DATA-dt-LISTS wave 1.0 n t nil t)) ;; Stretch time data to fit 2^N length array
	     (imaginary-wave (when imaginary-wave (s-flt-list imaginary-wave)))
	     (stretched-imaginary-wave (when imaginary-wave (CONVERT-DATA-dt-LISTS imaginary-wave 1.0 n t nil t))))
	;; Transfer stretched time values to real array, and clear imag array for DFT. Note that the length of stretched-wave is not always exactly equal to n.
	;; (format t "DFT length ~A, freq step ~A, stretched-wave length ~A ~%" n freq-step (length stretched-wave))
	;; (format t "length stretched-imaginary-wave: ~A stretched-wave: ~A~%" (length stretched-imaginary-wave) (length stretched-wave))
	(move-list-to-array stretched-wave x-real)
	(when stretched-imaginary-wave (move-list-to-array stretched-imaginary-wave x-imag))
	(if stretched-imaginary-wave (dft-reverse x-real x-imag tables) (dft-forward x-real x-imag tables))
	(let* ((true-real (sequence-to-list x-real))
	       (true-imag (sequence-to-list x-imag))
	       (true-mag (loop for real in true-real for imag in true-imag for freq-count from 0
			       unless (and (= freq-count 0) remove-dc) collect (* (sqrt (+ (square real) (square imag))) (* 0.001 delta-t))))
	       (true-phase (let ((phase-shift 0) phase last-imag last-real)
			     (loop for real in true-real for imag in true-imag for mag in true-mag for freq-count from 0 by freq-step
				   unless (and (= freq-count 0) remove-dc)
				   do (setq phase
					    (if (or (= real 0) (= mag 0))
						0.0
						(+ (if nil ; unfold-phase
						       (progn
							 (when last-real
							   (cond ((> (signum (* last-imag last-real))
								     (signum (* imag real)))
					; (format t "Phase shifting up...(freq-count ~A)~%" freq-count)
								  (setq phase-shift (+ phase-shift pi-single)))
								 ((< (signum (* last-imag last-real))
								     (signum (* imag real)))
					; (format t "Phase shifting down...(freq-count ~A)~%" freq-count)
								  (setq phase-shift (- phase-shift pi-single)))))
					; (format t "Phase shift ~A~%" phase-shift)
							 phase-shift)
						       0.0)
						   (if unfold-phase
						       (- (mod (+ (- (atan imag real)  (* freq-count time-shift)) pi)
							       (* 2 pi))
							  pi)
						       (atan (/ imag real))))))
				   (setq last-real real last-imag imag)
				   and collect phase))))
	  (when normalize-mag (setq true-mag (normalize-sequence true-mag)))

	  (let* ((max-freq (min (or max-freq (* freq-step (/ (length true-mag) 2))) true-max-freq))
		 (plot-max-freq (if log-plot-freq (log max-freq 10) max-freq))
		 (plot-min-freq (if log-plot-freq (log freq-step 10) 0.0))
		 (plot-freq-inc (/ plot-max-freq 5))
		 (delta-freq-start (if (or log-plot-freq remove-dc) freq-step 0.0))
		 (fixed-title-prefix (concatenate 'string title-prefix (if (> (length title-prefix) 0) ": " "")))
		 (true-length (- (round (/ max-freq freq-step)) (if (or log-plot-freq remove-dc) 1 0))))
	    ;; Get rid of symmetric top half
	    (setq true-phase (list-head true-phase true-length)
		  true-mag   (list-head true-mag   true-length))

	    (cond-every
	     (plot-input-wave
	      (plot-timed-data wave label delta-t :title (concatenate 'string fixed-title-prefix "DFT Input Real Part") :x-label data-x-label))
	     (plot-imaginary-wave-in
	      (plot-timed-data imaginary-wave label delta-t :title (concatenate 'string fixed-title-prefix "DFT Input Imaginary Part") :x-label data-x-label))
	     (plot-real-wave
	      (plot-timed-data true-real label freq-step :title (concatenate 'string fixed-title-prefix "DFT Output Real Part") :delta-t-start delta-freq-start
			       ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-imag-wave
	      (plot-timed-data true-imag label freq-step :title (concatenate 'string fixed-title-prefix "DFT Output Imaginary Part") :delta-t-start delta-freq-start
			       ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-phase
	      (plot-timed-data true-phase label freq-step :title (concatenate 'string fixed-title-prefix "Phase") :delta-t-start delta-freq-start :log-base 10
			       :y-min (unless unfold-phase (- (/ pi-single 2))) :y-max (unless unfold-phase (/ pi-single 2))
			       :y-inc (/ pi-single 4) :y-label "Radians"
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-mag
	      (plot-timed-data true-mag label freq-step :title (concatenate 'string fixed-title-prefix "Magnitude") :delta-t-start delta-freq-start :log-base 10
			       :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :x-label-h-position :right
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq)))
	    (when return-waves
	      (let ((frequencies (list-of-nums (length true-mag) delta-freq-start freq-step)))
		(values wave true-mag true-phase true-real true-imag frequencies)))))))))
|#

(defun dft-stretch-wave (input-wave &key (delta-t 1.0) (dc-offset 0.0) remove-dc normalize-mag max-freq log-plot-mag log-plot-freq
			 label title-prefix plot-input-wave (plot-mag t) (plot-phase t) plot-real-wave plot-imag-wave plot-imaginary-wave-in
			 (forward t) (time-shift 0) unfold-phase return-waves imaginary-wave)
  (let* ((wave-length (length input-wave))
	 (log-size (ceiling (log wave-length 2))))
    (when (> log-size 16)
      (sim-error (format nil "DFT can only handle log2(N) <= 16 (here rounded exponent is ~D, for wave-length ~A)" log-size wave-length)))
    (let* ((title-prefix (string (or title-prefix "")))
	   (data-x-label (if forward "ms" "Hz"))
	   (txfmed-x-label (if forward "Hz" "ms"))
	   (true-max-freq (/ 1 (* 2 0.001 delta-t)))
	   (tables   (get-dft-tables wave-length)) ; Need this below in DFT-FORWARD.
	   (n        (expt 2 log-size))
	   (x-real   (make-array n :element-type 'dft-float :initial-element 0.0)) ; Default is 0.0
	   (x-imag   (make-array n :element-type 'dft-float :initial-element 0.0))
	   (freq-step (/ 1.0 (* wave-length 0.001 delta-t)))) ; hz
      (declare (fixnum log-size n)
	       (type (dft-array (*)) x-real x-imag))
      (let* ((dc-offset (if remove-dc 0.0 (s-flt dc-offset)))
	     (wave (if (zerop dc-offset) (s-flt-array input-wave) (sequence-to-float-list-w-offset input-wave dc-offset)))
	     (stretched-wave (CONVERT-DATA-dt-LISTS wave 1.0 n t nil t)) ;; Stretch time data to fit 2^N length array
	     (imaginary-wave (when imaginary-wave (s-flt-list imaginary-wave)))
	     (stretched-imaginary-wave (when imaginary-wave (CONVERT-DATA-dt-LISTS imaginary-wave 1.0 n t nil t))))
	;; Transfer stretched time values to real array, and clear imag array for DFT. Note that the length of stretched-wave is not always exactly equal to n.
	;; (format t "DFT length ~A, freq step ~A, stretched-wave length ~A ~%" n freq-step (length stretched-wave))
	;; (format t "length stretched-imaginary-wave: ~A stretched-wave: ~A~%" (length stretched-imaginary-wave) (length stretched-wave))
	(move-list-to-array stretched-wave x-real)
	(when stretched-imaginary-wave (move-list-to-array stretched-imaginary-wave x-imag))
	(if stretched-imaginary-wave (dft-reverse x-real x-imag tables) (dft-forward x-real x-imag tables))
	(let* ((true-real (sequence-to-list x-real))
	       (true-imag (sequence-to-list x-imag))
	       (true-mag (loop for real in true-real for imag in true-imag for freq-count from 0
			       unless (and (= freq-count 0) remove-dc) collect (* (sqrt (+ (square real) (square imag))) (* 0.001 delta-t))))
	       (true-phase (let ((phase-shift 0) phase last-imag last-real)
			     (loop for real in true-real for imag in true-imag for mag in true-mag for freq-count from 0 by freq-step
				   unless (and (= freq-count 0) remove-dc)
				   do (setq phase
					    (if (or (= real 0) (= mag 0))
						0.0
						(+ (if nil ; unfold-phase
						       (progn
							 (when last-real
							   (cond ((> (signum (* last-imag last-real))
								     (signum (* imag real)))
					; (format t "Phase shifting up...(freq-count ~A)~%" freq-count)
								  (setq phase-shift (+ phase-shift pi-single)))
								 ((< (signum (* last-imag last-real))
								     (signum (* imag real)))
					; (format t "Phase shifting down...(freq-count ~A)~%" freq-count)
								  (setq phase-shift (- phase-shift pi-single)))))
					; (format t "Phase shift ~A~%" phase-shift)
							 phase-shift)
						       0.0)
						   (if unfold-phase
						       (- (mod (+ (- (atan imag real)  (* freq-count time-shift)) pi)
							       (* 2 pi))
							  pi)
						       (atan (/ imag real))))))
				   (setq last-real real last-imag imag)
				   and collect phase))))
	  (when normalize-mag (setq true-mag (normalize-sequence true-mag)))

	  (let* ((max-freq (min (or max-freq (* freq-step (/ (length true-mag) 2))) true-max-freq))
		 (plot-max-freq (if log-plot-freq (log max-freq 10) max-freq))
		 (plot-min-freq (if log-plot-freq (log freq-step 10) 0.0))
		 (plot-freq-inc (/ plot-max-freq 5))
		 (delta-freq-start (if (or log-plot-freq remove-dc) freq-step 0.0))
		 (fixed-title-prefix (concatenate 'string title-prefix (if (> (length title-prefix) 0) ": " "")))
		 (true-length (- (round (/ max-freq freq-step)) (if (or log-plot-freq remove-dc) 1 0))))
	    ;; Get rid of symmetric top half
	    (setq true-phase (list-head true-phase true-length)
		  true-mag   (list-head true-mag   true-length))

	    (cond-every
	     (plot-input-wave
	      (plot-timed-data wave label delta-t :title (concatenate 'string fixed-title-prefix "DFT Input Real Part") :x-label data-x-label))
	     (plot-imaginary-wave-in
	      (plot-timed-data imaginary-wave label delta-t :title (concatenate 'string fixed-title-prefix "DFT Input Imaginary Part") :x-label data-x-label))
	     (plot-real-wave
	      (plot-timed-data true-real label freq-step :title (concatenate 'string fixed-title-prefix "DFT Output Real Part") :delta-t-start delta-freq-start
			       ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-imag-wave
	      (plot-timed-data true-imag label freq-step :title (concatenate 'string fixed-title-prefix "DFT Output Imaginary Part") :delta-t-start delta-freq-start
			       ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-phase
	      (plot-timed-data true-phase label freq-step :title (concatenate 'string fixed-title-prefix "Phase") :delta-t-start delta-freq-start :log-base 10
			       :y-min (unless unfold-phase (- (/ pi-single 2))) :y-max (unless unfold-phase (/ pi-single 2))
			       :y-inc (/ pi-single 4) :y-label "Radians"
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq :x-label-h-position :right
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
	     (plot-mag
	      (plot-timed-data true-mag label freq-step :title (concatenate 'string fixed-title-prefix "Magnitude") :delta-t-start delta-freq-start :log-base 10
			       :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :x-label-h-position :right
			       :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq
			       :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq)))
	    (when return-waves
	      (let ((frequencies (list-of-nums (length true-mag) delta-freq-start freq-step)))
		(values wave true-mag true-phase true-real true-imag frequencies)))))))))


(defun element-data-dft (element &key data-type type state-index (delta-t 1.0) (reference-time-list (CURRENT-SIM-PLOT-TIME-LIST)) (dc-offset 0.0))
  "Plot magnitude and phase of the DFT of ELEMENT data, resampled on a regular grid given by DELTA-T [milliseconds, default
1.0]. DC-OFFSET [default 0.0] is subtracted from the data before the DFT. Remaining arguments are as for ELEMENT-DATA-DTED. DFT
processing done by DFT-STRETCH-WAVE, which may also resample the data."
  (dft-stretch-wave (element-data-dted element delta-t data-type type reference-time-list state-index)
		    :title-prefix (format nil "~A ~A ~A" *simulation-name* (element-name element) (or data-type (default-data-type element)))
		    :delta-t delta-t :dc-offset dc-offset))

(defun transfer-function (input-element output-element &key input-element-data-type output-element-data-type
			  (plot-waves t)
			  (log-plot-freq t)
			  (log-plot-mag t)
			  (delta-t 1.0)
			  (min-rel-input-mag-for-xfer-function 0.0)
			  return-waves)
  "Derive the frequency domain, linear estimate of the transfer function between the data of INPUT-ELEMENT-DATA-TYPE of INPUT-ELEMENT, and that of
OUTPUT-ELEMENT-DATA-TYPE of OUTPUT-ELEMENT. The DC component is *not* considered. If data types are not specified, default types are those as used by the
function ELEMENT-DATA. The transfer function is computed starting from the lowest frequency, until the relative amplitude of the input frequency magnitude
is less than that given by MIN-REL-INPUT-MAG-FOR-XFER-FUNCTION . [default 0.0]. This allows a lower limit to be applied to the computation to avoid
spurious values from very small frequency components of the input. Frequency magnitude and phase of the input data, the output data, and the transfer
function are plotted when PLOT-WAVES is T [default], and in log coordinates according to LOG-PLOT-FREQ and LOG-PLOT-MAG [both default T]. When RETURN-WAVES
is T [default NIL], then the following are returned as values:

   (TRANSFER-FUNCTION-MAGNITUDE TRANSFER-FUNCTION-PHASE TRANSFER-FUNCTION-FREQUENCIES)

"
  (let* ((input-wave (element-data-dted input-element delta-t input-element-data-type))
	 (output-wave (element-data-dted output-element delta-t output-element-data-type))
	 (wave-length (length input-wave))
	 (freq-step (/ 1.0 (* wave-length 0.001 delta-t)))) ; hz

    (multiple-value-bind (input-WAVE input-MAG input-PHASE input-REAL-PART input-IMAG-PART frequencies)
	(dft-stretch-wave input-wave :remove-dc t
			  :log-plot-mag log-plot-mag :log-plot-freq log-plot-freq :plot-phase plot-waves :plot-mag plot-waves
			  :return-waves t :title-prefix "Transfer Function System Input")
      (multiple-value-bind (output-WAVE output-MAG output-PHASE output-REAL-PART output-IMAG-PART frequencies)
	  (dft-stretch-wave output-wave :remove-dc t
			    :log-plot-mag log-plot-mag :log-plot-freq log-plot-freq :plot-phase plot-waves :plot-mag plot-waves
			    :return-waves t :title-prefix "Transfer Function System Output")
	(let* ((input-mag-cutoff (* min-rel-input-mag-for-xfer-function (max-of-list input-mag))))
	  (loop for input-mag-value in input-mag
		for input-phase-value in input-phase
		for output-mag-value in output-mag
		for output-phase-value in output-phase
		for frequency in frequencies
		when (> input-mag-value input-mag-cutoff)
		collect (/ output-mag-value input-mag-value) into transfer-mag
		and collect (- output-phase-value input-phase-value) into transfer-phase
		and collect frequency into transfer-frequencies
		else do (setq frequencies nil)
		finally

		(plot-timed-data transfer-mag nil transfer-frequencies :title "Transfer Function Magnitude"
				 :x-log log-plot-freq :y-log log-plot-mag :log-base 10
				 :y-min (unless log-plot-mag 0.0)
				 :x-min (if log-plot-freq (log freq-step 10) 0.0)
				 :x-are-fns (not log-plot-freq)
				 :x-label "Hz")
		(plot-timed-data transfer-phase nil transfer-frequencies :title "Transfer Function Phase"
				 :y-min (/ pi -2) :y-max (/ pi 2)
				 :y-inc (/ pi-single 4) :y-label "Radians"
				 :x-log log-plot-freq :log-base 10
				 :x-min (if log-plot-freq (log freq-step 10) 0.0)
				 :x-are-fns (not log-plot-freq)
				 :x-label "Hz")
		(when return-waves
		  (return (values TRANSFER-MAG TRANSFER-PHASE TRANSFER-FREQUENCIES)))))))))

#|
(defun dft-forward-complete (data delta-t &key plot return-values title-prefix)
  ;; Assumes only real input data (as DATA).
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((wave-length (length data))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size (ceiling (log wave-length 2)))
	 (n        (expt 2 log-size))
	 (tables   (get-dft-tables wave-length)) ; Need this below in DFT-FORWARD.
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t)))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float))
	 (data (s-flt-list data)))
    (loop for i from 0 to (1- n)
	  for val in data do
	  (setf (aref x-real i) val)
	  (setf (aref x-imag i) 0.0))
    (dft-forward x-real x-imag tables)
    (setq x-real (s-flt-list x-real)
	  x-imag (s-flt-list x-imag))
    (let (mag)
      (loop for real in x-real
	    for imag in x-imag
	    do (setq mag (* 1		; (* 0.001 delta-t) ; convert to Hz, assuming that delta-t is in ms.
			    (sqrt (+ (square real) (square imag)))))
	    collect mag into mags
	    collect (if (or (= real 0) (= mag 0)) 0.0 (atan imag real)) into phases
	    finally
	    (when plot
	      (plot-timed-data (list data) nil delta-t :x-label "ms" :title (format nil "~ADFT Input Data" title-prefix))
	      (plot-timed-data (list (fold-wave x-real) (fold-wave x-imag)) '(dft-real dft-imag) delta-freq
			       :delta-t-start (round (/ max-freq -2)) :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-label "Hz" :x-label-h-position :right :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
			       :title (format nil "~ADFT Real and Imag Output" title-prefix))
	      (plot-timed-data (fold-wave mags) nil delta-freq :delta-t-start (round (/ max-freq -2)) :title (format nil "~ADFT Magnitude" title-prefix)
			       :x-label "Hz" :x-label-h-position :right :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2)))
	      (plot-timed-data (fold-wave phases) nil delta-freq :delta-t-start (round (/ max-freq -2)) :title (format nil "~ADFT Phase" title-prefix)
			       :x-label "Hz" :x-label-h-position :right :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
			       :y-origin (- pi-single) :y-inc (/ pi-single 2) :y-min (- pi-single) :y-max pi-single))
	    (when return-values (return (values x-real x-imag mags phases)))))))

;; (dft-forward-complete  '(1.0 -1.0 1.0 -1.0) 1.0  :plot t :return-values t)
|#

(defun dft-reverse-complete-basic (data-real data-imag)
  (dft-reverse data-real data-imag (get-dft-tables data-real))
  (values data-real data-imag))

#|
(defun dft-reverse-complete (data-real data-imag delta-hz
				       &key plot return-values (return-values-as-lists t)
				       (output :magnitude-phase) ;  :real-imag, :all
				       title-prefix)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((wave-length (length data-real))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size  (ceiling (log wave-length 2)))
	 (n         (expt 2 log-size))
	 (delta-hz  (s-flt delta-hz))	; Hz
	 (delta-t   (/ 1 (* 0.001 wave-length delta-hz))) ; ms
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t))))
    (declare (fixnum log-size n)
	     (single-float delta-hz delta-t max-time max-freq))
    (when plot
      (plot-timed-data (list (fold-wave data-real) (fold-wave data-imag)) '(data-real data-imag) delta-hz :delta-t-start (round (/ max-freq -2))
		       :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
		       :x-label-h-position :right :x-label "Hz" :title (format nil "~AReverse DFT Input Data" title-prefix)))
    (let (x-real x-imag)
      (if (and (arrayp data-real) (arrayp data-imag))
	(setq x-real data-real x-imag data-imag)
	(progn
	  (setq x-imag  (make-array n :element-type 'dft-float)
		x-real  (make-array n :element-type 'dft-float))
	  (loop for i fixnum from 0 to (1- n)
		for real single-float in data-real
		for imag single-float in data-imag
		do (setf (aref (the (simple-array single-float (*)) x-real) i) real
			 (aref (the (simple-array single-float (*)) x-imag) i) imag))))
      (dft-reverse x-real x-imag (get-dft-tables data-real))
      (when plot
	(plot-timed-data (list (fold-wave x-real) (fold-wave x-imag)) '(dft-real dft-imag) delta-t :delta-t-start 0
			 :x-label-h-position :right :x-label "ms" :title (format nil "~AReverse DFT Real and Imag Output" title-prefix)))
      (let* ((conserve-arrays (not (eq output :all)))
	     (mag-array (if conserve-arrays x-real (make-array n :element-type 'dft-float)))
	     (phase-array (if conserve-arrays x-imag (make-array n :element-type 'dft-float))))
	(declare (type (simple-array single-float (*)) mag-array phase-array))
	(when (or (eq output :all) (eq output :magnitude-phase))
	  (loop for real single-float across (the (simple-array single-float (*)) x-real)
		for imag single-float across (the (simple-array single-float (*)) x-imag)
		for index fixnum from 0
		do (let* ((mag (mag-from-real-imag real imag))
			  (phase (phase-from-real-imag real imag mag)))
		     (declare (single-float mag phase))
		     (setf (aref mag-array index) mag)
		     (setf (aref phase-array index) phase))))
	(when return-values
	  (values-list
	   (let ((out (no-nils
		       (list
			(when (or (eq output :all) (eq output :real-imag)) x-real)
			(when (or (eq output :all) (eq output :real-imag)) x-imag)
			(when (or (eq output :all) (eq output :magnitude-phase)) mag-array)
			(when (or (eq output :all) (eq output :magnitude-phase)) phase-array)))))
	     (if return-values-as-lists (mapcar '#(lambda (array) (coerce array 'list)) out) out))))))))

|#

(defun dft-forward-complete (data delta-t &key plot return-values title-prefix)
  ;; Assumes only real input data (as DATA).
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((wave-length (length data))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size (ceiling (log wave-length 2)))
	 (n        (expt 2 log-size))
	 (tables   (get-dft-tables wave-length)) ; Need this below in DFT-FORWARD.
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t)))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float))
	 (data (s-flt-list data)))
    (loop for i from 0 to (1- n)
	  for val in data do
	  (setf (aref x-real i) val)
	  (setf (aref x-imag i) 0.0))
    (dft-forward x-real x-imag tables)
    (setq x-real (s-flt-list x-real)
	  x-imag (s-flt-list x-imag))
    (let (mag)
      (loop for real in x-real
	    for imag in x-imag
	    do (setq mag (* 1		; (* 0.001 delta-t) ; convert to Hz, assuming that delta-t is in ms.
			    (sqrt (+ (square real) (square imag)))))
	    collect mag into mags
	    collect (if (or (= real 0) (= mag 0)) 0.0 (atan imag real)) into phases
	    finally
	    (when plot
	      (plot-timed-data (list data) nil delta-t :x-label "ms" :title (format nil "~ADFT Input Data" title-prefix))
	      (plot-timed-data (list (fold-wave x-real) (fold-wave x-imag)) '(dft-real dft-imag) delta-freq
			       :delta-t-start (round (/ max-freq -2)) :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-label "Hz" :x-label-h-position :right :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
			       :title (format nil "~ADFT Real and Imag Output" title-prefix))
	      (plot-timed-data (fold-wave mags) nil delta-freq :delta-t-start (round (/ max-freq -2)) :title (format nil "~ADFT Magnitude" title-prefix)
			       :x-label "Hz" :x-label-h-position :right :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2)))
	      (plot-timed-data (fold-wave phases) nil delta-freq :delta-t-start (round (/ max-freq -2)) :title (format nil "~ADFT Phase" title-prefix)
			       :x-label "Hz" :x-label-h-position :right :reference-ticks-to-origin t
			       :x-axis-root 0.0 :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
			       :y-origin (- pi-single) :y-inc (/ pi-single 2) :y-min (- pi-single) :y-max pi-single))
	    (when return-values (return (values x-real x-imag mags phases)))))))

(defun dft-reverse-complete (data-real data-imag delta-hz
				       &key plot return-values (return-values-as-lists t)
				       (output :magnitude-phase) ;  :real-imag, :all
				       title-prefix)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((wave-length (length data-real))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size  (ceiling (log wave-length 2)))
	 (n         (expt 2 log-size))
	 (delta-hz  (s-flt delta-hz))	; Hz
	 (delta-t   (/ 1 (* 0.001 wave-length delta-hz))) ; ms
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t))))
    (declare (fixnum log-size n)
	     (single-float delta-hz delta-t max-time max-freq))
    (when plot
      (plot-timed-data (list (fold-wave data-real) (fold-wave data-imag)) '(data-real data-imag) delta-hz :delta-t-start (round (/ max-freq -2))
		       :x-origin 0.0 :x-min (round (/ max-freq -2)) :x-max (round (/ max-freq 2))
		       :x-label-h-position :right :x-label "Hz" :title (format nil "~AReverse DFT Input Data" title-prefix)))
    (let (x-real x-imag)
      (if (and (arrayp data-real) (arrayp data-imag))
	(setq x-real data-real x-imag data-imag)
	(progn
	  (setq x-imag  (make-array n :element-type 'dft-float)
		x-real  (make-array n :element-type 'dft-float))
	  (loop for i fixnum from 0 to (1- n)
		for real single-float in data-real
		for imag single-float in data-imag
		do (setf (aref (the (simple-array single-float (*)) x-real) i) real
			 (aref (the (simple-array single-float (*)) x-imag) i) imag))))
      (dft-reverse x-real x-imag (get-dft-tables data-real))
      (when plot
	(plot-timed-data (list (fold-wave x-real) (fold-wave x-imag)) '(dft-real dft-imag) delta-t :delta-t-start 0
			 :x-label-h-position :right :x-label "ms" :title (format nil "~AReverse DFT Real and Imag Output" title-prefix)))
      (let* ((conserve-arrays (not (eq output :all)))
	     (mag-array (if conserve-arrays x-real (make-array n :element-type 'dft-float)))
	     (phase-array (if conserve-arrays x-imag (make-array n :element-type 'dft-float))))
	(declare (type (simple-array single-float (*)) mag-array phase-array))
	(when (or (eq output :all) (eq output :magnitude-phase))
	  (loop for real single-float across (the (simple-array single-float (*)) x-real)
		for imag single-float across (the (simple-array single-float (*)) x-imag)
		for index fixnum from 0
		do (let* ((mag (mag-from-real-imag real imag))
			  (phase (phase-from-real-imag real imag mag)))
		     (declare (single-float mag phase))
		     (setf (aref mag-array index) mag)
		     (setf (aref phase-array index) phase))))
	(when return-values
	  (values-list
	   (let ((out (no-nils
		       (list
			(when (or (eq output :all) (eq output :real-imag)) x-real)
			(when (or (eq output :all) (eq output :real-imag)) x-imag)
			(when (or (eq output :all) (eq output :magnitude-phase)) mag-array)
			(when (or (eq output :all) (eq output :magnitude-phase)) phase-array)))))
	     (if return-values-as-lists (mapcar '#(lambda (array) (coerce array 'list)) out) out))))))))

(defun bode-plot-mag (magnitude delta-f start-freq title)
  (loop for freq from start-freq by delta-f
	for mag in magnitude
	when (>= freq 0) collect (if (= freq 0) (* 0.1 delta-f) freq) into out-frequencies
	collect mag into out-magnitudes
	finally	(plot-timed-data out-magnitudes nil out-frequencies :y-log t :log-base 10 :x-log t :title (format nil "Bode Plot: ~A" title))))

#|
(defun deconvolve (data-x data-y delta-t &key cutoff-freq return-values plot-x plot-y plot-h)
  (let* ((title-prefix (if cutoff-freq (format nil "DATA-H (Cutoff ~AHz)" cutoff-freq) 'data-h))
	 (wave-length (length data-y))
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-freq (* delta-freq wave-length))
	 (max-time (* wave-length delta-t))
	 (data-y (s-flt-list data-y))
	 (data-x (s-flt-list data-x)))
    (multiple-value-bind (dft-x-real dft-x-imag dft-x-mag dft-x-phase)
	(dft-forward-complete data-x delta-t :return-values t :plot plot-x :title-prefix 'data-x)
      (multiple-value-bind (dft-y-real dft-y-imag dft-y-mag dft-y-phase)
	  (dft-forward-complete data-y delta-t :return-values t :plot plot-y :title-prefix 'data-y)
	(let (h-mag h-phase)
	  (loop for y-mag in dft-y-mag
		for x-mag in dft-x-mag
		for y-phase in dft-y-phase
		for x-phase in dft-x-phase
		for freq from 0.0 by delta-freq
		do (setq h-mag (if (or (not cutoff-freq)
				       (< freq cutoff-freq)
				       (> freq (- max-freq cutoff-freq)))
				   (/ y-mag x-mag) 0.0)
			 h-phase (- y-phase x-phase))
		collect (real-from-mag-phase h-mag h-phase) into h-reals
		collect (imag-from-mag-phase h-mag h-phase) into h-imags
		collect h-mag into h-mags
		collect h-phase into h-phases
		finally
		(when plot-h
		  (plot-timed-data (fold-wave h-mags) nil delta-freq
				   :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz" :x-label-h-position :right
				   :title (format nil "~A Magnitude" title-prefix))
		  (plot-timed-data (fold-wave h-phases) nil delta-freq :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz" :x-label-h-position :right
				   :title (format nil "~A Phase" title-prefix)))
		(multiple-value-bind (dft-h-real dft-h-imag dft-h-mag dft-h-phase)
		    (dft-reverse-complete h-reals h-imags delta-freq :return-values t :plot plot-h :title-prefix title-prefix :output :all)
		  (when return-values (return (values dft-h-real dft-h-imag dft-h-mag dft-h-phase))))))))))

|#


(defun deconvolve (data-x data-y delta-t &key cutoff-freq return-values plot-x plot-y plot-h)
  (let* ((title-prefix (if cutoff-freq (format nil "DATA-H (Cutoff ~AHz)" cutoff-freq) 'data-h))
	 (wave-length (length data-y))
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-freq (* delta-freq wave-length))
	 (max-time (* wave-length delta-t))
	 (data-y (s-flt-list data-y))
	 (data-x (s-flt-list data-x)))
    (multiple-value-bind (dft-x-real dft-x-imag dft-x-mag dft-x-phase)
	(dft-forward-complete data-x delta-t :return-values t :plot plot-x :title-prefix 'data-x)
      (multiple-value-bind (dft-y-real dft-y-imag dft-y-mag dft-y-phase)
	  (dft-forward-complete data-y delta-t :return-values t :plot plot-y :title-prefix 'data-y)
	(let (h-mag h-phase)
	  (loop for y-mag in dft-y-mag
		for x-mag in dft-x-mag
		for y-phase in dft-y-phase
		for x-phase in dft-x-phase
		for freq from 0.0 by delta-freq
		do (setq h-mag (if (or (not cutoff-freq)
				       (< freq cutoff-freq)
				       (> freq (- max-freq cutoff-freq)))
				   (/ y-mag x-mag) 0.0)
			 h-phase (- y-phase x-phase))
		collect (real-from-mag-phase h-mag h-phase) into h-reals
		collect (imag-from-mag-phase h-mag h-phase) into h-imags
		collect h-mag into h-mags
		collect h-phase into h-phases
		finally
		(when plot-h
		  (plot-timed-data (fold-wave h-mags) nil delta-freq
				   :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz" :x-label-h-position :right
				   :title (format nil "~A Magnitude" title-prefix))
		  (plot-timed-data (fold-wave h-phases) nil delta-freq :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz" :x-label-h-position :right
				   :title (format nil "~A Phase" title-prefix)))
		(multiple-value-bind (dft-h-real dft-h-imag dft-h-mag dft-h-phase)
		    (dft-reverse-complete h-reals h-imags delta-freq :return-values t :plot plot-h :title-prefix title-prefix :output :all)
		  (when return-values (return (values dft-h-real dft-h-imag dft-h-mag dft-h-phase))))))))))


;; works properly for even length sequences. ie
;; * (FOLD-WAVE '(1 2 a b))
;; (A B 1 2)
;; * (FOLD-WAVE '(1 2 a b c))
;; (A B 1 2 C)
;; Returns copy of wave.
(defun fold-wave (wave)
  (let* ((wave-is-arrayp (arrayp wave))
	 (input-array (sequence-to-gen-array wave))
	 (output-array (make-array (list (length wave))))
	 (wave-length (length wave))
	 (lhl (floor (/ wave-length 2))))
    (loop for i from 0 to (1- lhl)
	  do (let ((left-element (aref input-array i))
		   (right-element (aref input-array (+ lhl i))))
	       (setf (aref output-array i) right-element
		     (aref output-array (+ lhl i)) left-element)))
    (if wave-is-arrayp output-array (sequence-to-list output-array))))

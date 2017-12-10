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


;;; SYS Source file: math.lisp
(in-package :surf-hippo)

;; Some miscellaneous math functions - there may be others scattered through the source files (e.g.
;; misc.lisp, analysis.lisp, and others. also see gui/math.lisp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Geometric stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sphere-diameter-from-capacitance (capacitance &optional (specific-capacitance 1.0))
  "Returns the diameter in microns of a sphere with CAPACITANCE in pF, assuming a SPECIFIC-CAPACITANCE in uF/cm2 [default 1]."
  (sphere-diameter-from-area
   (* 1e8				; um2/cm2
      (/ (* capacitance			; pF
	    1e-6)			; uF/pF
	 specific-capacitance		; uF/cm2
	 ))))

(defun sphere-area (radius-microns)
  "Sphere surface area is in um2 - RADIUS-MICRONS is in micrometers."
  (* 4.0 pi-single radius-microns radius-microns))

(defun sphere-area-cm2 (radius-microns)
  "Sphere surface area is in cm2 - RADIUS-MICRONS is in micrometers."
  (* 1.0e-8 (sphere-area radius-microns)))

(defun sphere-area-from-diameter (diameter)
  "Sphere surface area is in cm2 - DIAMETER is in micrometers."
  (let ((radius (/ diameter 2)))
    (* 4.0 pi-single radius radius 1.0e-8)))

(defun sphere-diameter-from-area (area)
  "Returns the diameter in microns of a sphere with AREA in um2."
  (sqrt (/ area pi-single)))

(defun sphere-volume (radius)
  "Returns volume in um3 of sphere with RADIUS (single float) in um (single float)."
  (* (/ 4 3.0) pi-single radius radius radius))

(defun sphere-volume-from-diameter (diameter)
  "Returns volume in um3 of sphere with DIAMETER (single float) in um (single float)."
  (sphere-volume (/ diameter 2)))

(defun cylinder-volume (length diameter)
  "Returns volume in um3 of cylinder with LENGTH and DIAMETER (both single floats) in um (single float)."
  (let ((radius (/ diameter 2)))
    (* pi-single radius radius length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline fast-fractional-part))
(defun fast-fractional-part (number)
  "Returns second result of TRUNCATE, where NUMBER is double float."
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind (tru rem) (truncate (the df number) 1.0)
    (declare (ignore tru))
    rem))

(defun single-float-signum (num)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float num))
  (cond ((= 0 num) 0.0)
	((> num 0) 1.0)
	(t -1.0)))

;; ************* ************* ************* *************
;;
;;   Trigonometric and Complex Number Related Functions
;;
;; ************* ************* ************* *************

(proclaim '(inline real-from-mag-phase imag-from-mag-phase))

(defun real-from-mag-phase (mag phase)
  "Return the single value real part of the complex number described by MAG and PHASE."
  (* mag (cos phase)))

(defun imag-from-mag-phase (mag phase)
  "Return the imaginary part of the complex number described by MAG and PHASE."
  (* mag (sin phase)))

(defun real-and-imaginary-from-mag-and-phase (mag phase)
  ;; Both MAG and PHASE must be single float lists.
  (values (mapcar #'real-from-mag-phase mag phase)
	  (mapcar #'imag-from-mag-phase mag phase)))

(defun mag-from-real-imag (real imag)
  (declare (optimize (safety 2) (speed 3) (space 0))
	   (single-float real imag))
  (* 2 pi-single (sqrt (+ (square real) (square imag)))))

(defun phase-from-real-imag (real imag &optional (mag (the sf (mag-from-real-imag real imag))))
  (declare (optimize (safety 2) (speed 3) (space 0))
	   (single-float real imag mag))
  (if (or (= real 0) (= mag 0)) 0.0 (atan imag real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline df-real-log))
(defun df-real-log (number base)
  ;; Return the logarithm of the double float NUMBER in the double float BASE.
  (declare (optimize (speed 3) (space 0))
	   (double-float number base))
  (the df (/ (the df (log number))
	     (the df (log base)))))

(defun df-real-ln (number)
  ;; Return the natural logarithm of the double float NUMBER.
  (declare (optimize (speed 3) (space 0))
	   (double-float number))
  (the df (log number)))

(defun log-10 (number) (log number 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline float-mod))
(defun float-mod (number divisor)
  "Returns second result of FLOOR. NUMBER and DIVISOR must be single floats."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float number divisor))
  (let ((rem (the sf (rem number divisor))))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

(defun gaussian (x mean variance)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float x mean variance))
  "All arguments must be single floats. Returns a single float."
  ;;  (format t "x ~A, mean ~A, variance ~A~%" x mean variance)
  (the (single-float 0.0 *)
       (without-floating-underflow-traps
	   (let ((x-shifted (- x mean)))
	     (* (/ 1.0 (sqrt (the (single-float 0.0 *) (* (+ pi-single pi-single) variance))))
		(exp-w-limits (the (single-float * 0.0) (- (/ (square x-shifted) (+ variance variance))))))))))

(proclaim '(inline nonlinearity))
(defun nonlinearity (input &optional nonlinearity (parameter 0.0))
  "Pass the single float INPUT through a nonlinearity and return the single float result. Default for PARAMETER is 0.0 (must be a single-float).

 :NONLINEARITY           :PARAMETER               COMMENT

     NIL                    n/a                   linear
 :THRESHOLD              Threshold    (if INPUT >= threshold then INPUT, else 0.0)
 :NEGATE-THRESHOLD       Threshold    (if INPUT >= threshold then (- INPUT), else 0.0)
 :BELOW-THRESHOLD        Threshold    (if INPUT <= threshold then INPUT, else 0.0)
 :NEGATE-BELOW-THRESHOLD Threshold    (if INPUT <= threshold then (- INPUT), else 0.0)
 :RECTIFY                   n/a          full wave, i.e. x>0 -> x, x<0 -> -x

"
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float input parameter))
  (case nonlinearity
    ((threshold :threshold) (the sf (if (>= input parameter) input 0.0)))
    (:negate-threshold (the sf (if (>= input parameter) (- input) 0.0)))
    (:negate-below-threshold (the sf (if (<= input parameter) (- input) 0.0)))
    (:below-threshold (the sf (if (<= input parameter) input 0.0)))
    (:rectify (the sf (abs input)))
    (t input)))

(proclaim '(inline single-float-min))
(defun single-float-min (number1 number2)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float number1 number2))
  (if (< number1 number2) number1 number2))

(proclaim '(inline single-float-max))
(defun single-float-max (number1 number2)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float number1 number2))
  (if (> number1 number2) number1 number2))

(proclaim '(inline double-float-min))
(defun double-float-min (number1 number2)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float number1 number2))
  (if (< number1 number2) number1 number2))

(proclaim '(inline double-float-max))
(defun double-float-max (number1 number2)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float number1 number2))
  (if (> number1 number2) number1 number2))

;;; These are for avoiding over or underflows in calls to exp.
(defconstant exp-upper-arg
  (float (floor (log most-positive-single-float))))

(defconstant exp-lower-arg -30.0)	; empirical

(defconstant exp-upper-arg-double (float (floor (log most-positive-double-float)) 0.0d0))
(defconstant exp-lower-arg-double -300.0d0) ; empirical

(defconstant exp-upper (exp exp-upper-arg))
(defconstant exp-lower (exp exp-lower-arg))

(defconstant exp-upper-double (exp exp-upper-arg-double))
(defconstant exp-lower-double (exp exp-lower-arg-double))

(defvar *notify-exp-limit* nil "When T print out message when one of EXP-W-LIMITS functions punts.")

(proclaim '(inline notify-exp-limit))
(defun notify-exp-limit (arg)
  (when *notify-exp-limit* (format *error-output* "EXP-W-LIMITS punted with ~A~%" arg)))

(proclaim '(inline exp-w-limits))
(defun exp-w-limits (arg)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float arg))
  (cond ((> arg exp-upper-arg) (notify-exp-limit arg) EXP-UPPER)
	((> arg exp-lower-arg) (exp arg))
	(t (notify-exp-limit arg) EXP-lower)))

(defun exp-w-limits-generic (arg)
  (cond ((> arg exp-upper-arg) (notify-exp-limit arg) EXP-UPPER)
	((> arg exp-lower-arg) (exp arg))
	(t (notify-exp-limit arg) EXP-lower)))

(proclaim '(inline exp-w-limits-double))
(defun exp-w-limits-double (arg)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float arg))
  (cond ((> arg exp-upper-arg-double) (notify-exp-limit arg) EXP-UPPER-DOUBLE)
	((> arg exp-lower-arg-double) (exp arg))
	(t (notify-exp-limit arg) EXP-lower-DOUBLE)))

(defun in-btwn (lower num upper) (and (<= lower num) (< num upper)))

(defun my-xor (a b)
  (if a
      (if b nil t)
      (if b t nil)))

(proclaim '(inline rectify))
(defun rectify (number)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float number))
  (max number 0.0))

(defun close-to (first-float second-float &optional (resolution 0.001))
  ;; Returns T if the arguments are equal at the level of RESOLUTION.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (= (round (/ (the sf first-float) (the sf resolution)))
     (round (/ (the sf second-float)(the sf resolution)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3x3 array routines. Used in histology graphics transforms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline clear-3-by-3-array fill-3-by-3-array copy-3-by-3-array abtoc))
(defun clear-3-by-3-array (array)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) array))
  (dotimes (i 3)
    (declare (fixnum i))
    (dotimes (j 3)
      (declare (fixnum j))
      (setf (aref array i j) 0.0))))

(defun fill-3-by-3-identity (array)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) array))
  (clear-3-by-3-array array)
  (dotimes (i 3)
    (declare (fixnum i))
    (setf (aref array i i) 1.0))
  array)

(defun copy-3-by-3-array (from-array to-array)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) from-array to-array))
  (dotimes (i 3)
    (declare (fixnum i))
    (dotimes (j 3)
      (declare (fixnum j))
      (setf (aref to-array i j)
	    (aref from-array i j)))))

(defun abtoc (a b c)
  ;; Perform A*B->C on these three 3X3 matrices.
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type (simple-array single-float (3 3)) a b c))
  (loop for i fixnum from 0 to 2 do
       (loop for j fixnum from 0 to 2 do
	    (setf (aref c i j)
		  (loop for k fixnum from 0 to 2 sum (* (aref a i k) (aref b k j)) into result single-float
		     finally (return result))))))

;;;;;;;;;;;;;;;;;;;;;;

(defun permute (l)
  (if (null l) ; I don't like this. I suspect that there is a more
      '(nil)   ; natural way to make the function return '(nil) for nil.
      (mapcan #'(lambda (e-arg)
		  (mapcar #'(lambda (p) (cons e-arg p))
			  (permute (remove e-arg l))))
	      l)))

#|
Geert-Jan
--
Geert-Jan van Opdorp
AI-Engineering
Amsterdam, The Netherlands
geert@aie.nl

|#

(defun dolist-permute (input)
  (when input
    (let ((result (list input)))
      (when (> (length input) 1)
	(dolist (head input result)
	  (nconc result
		 (mapcar #'(lambda (list)
			     (cons head list))
			 (dolist-permute (remove head input :count 1))))))
      (remove-duplicates result :test #'equal))))

#|
Strictly speaking, I suppose you do not need to provide for the possibility of
the same element occurring more than once (:count 1 and remove-duplicates),
since, if I remember correctly, for a permutation you assume that all elements
are distinct. Still, it seems a small price to pay for adding generality.

Cheers,

Yo

--
------------------------------------------------------------------------------
_
V                    Johan Peeters
-------------------           Alcatel Broadband Systems
|  A L C A T E L  |           Excelsiorlaan 44-46 - 1930 Zaventem - Belgium
-------------------           Phone: +32 2 718 7119 - Fax: +32 2 718 7000
BBS		       Email: peeterjo@bsg.bel.alcatel.be

------------------------------------------------------------------------------

|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar sin-2d-array
  ;; 2d array whose values are comprised of the sin of the product of the x and y indices.
  (float-2dlist-to-array
   (loop for x from -2.0 to 2.0 collect
	(loop for y from -2.0 to 2.0 collect
	     (sin (* x y))))))

(defvar xy-2d-array
  ;; 2d array whose values are comprised of the product of the x and y indices, referenced to the center.
  (float-2dlist-to-array
   (loop for x from -20.0 to 20.0 collect
	(loop for y from -20.0 to 20.0 collect
	     (* x y)))))

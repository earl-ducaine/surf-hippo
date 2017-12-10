;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-


;; The Surf-Hippo Neuron Simulator System
;;
;; This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
;; Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
;; Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
;;
;; Permission to use, copy, modify, and distribute this software and its documentation for any purpose
;; and without fee is hereby granted, provided that this software is cited in derived published work,
;; and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
;; Project makes no representations about the suitability of this software for any purpose. It is
;; provided "as is" without express or implied warranty.
;;
;; If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
;; on the mailing list.
;;
;; Copyright (c) 1989 - 2003, Lyle J. Graham
;;
;; GUI Source file: math.lisp


(in-package :windows-hack)

;; Some math and logical functions. Check the macros.lisp file for
;; some math macros, and sequences.lisp for some sequence-oriented
;; math functions.

(defun half (number)
  (/ number 2))

(proclaim '(inline float-floor))

(defun float-floor (number &optional (divisor 1.0))
  ;; Returns the greatest integer not greater than number, or
  ;; number/divisor. The second returned value is (mod number
  ;; divisor).
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (single-float number divisor))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
	       (plusp number)
	       (minusp number)))
      (values (1- tru) (+ rem divisor))
      (values tru rem))))

(defun round-up-to-power-of-2 (number)
  "Return nearest value greater than or equel to NUMBER that is a integer power of 2."
  (expt 2 (ceiling (log number 2))))

(defun s-flt-and-copy-list (list)
  ;; Returns a copy of a LIST of numbers with all values coerced to single-floats.
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (loop for val in list collect (s-flt val)))

(defun single-float-<-merge-lists* (list-1 list-2 &optional key)
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (do* ((result (list :header))
	(P result))			; P points to last cell of result
      ((or (null list-1) (null list-2)) ; done when either list used up
       (if (null list-1)		; in which case, append the
	 (rplacd p list-2)		;   other list
	 (rplacd p list-1))
       (do ((drag p lead)
	    (lead (cdr p) (cdr lead)))
	   ((null lead)
	    (values (prog1 (cdr result) ; return the result sans header
		      (rplacd result nil)) ; (free memory, be careful)
		    drag))))		; and return pointer to last element
    (cond ((let ((one (car list-2))
		 (two (car list-1)))
	     (if key
	       (< (the sf (funcall (the compiled-function key) one))
		  (the sf (funcall (the compiled-function key) two)))
	       (< (the sf one) (the sf two))))
	   (rplacd p list-2)		; append the lesser list to last cell of
	   (setq p (cdr p))		;   result.  Note: test must bo done for
	   (pop list-2))		;   list-2 < list-1 so merge will be
	  (T (rplacd p list-1)		;   stable for list-1
	     (setq p (cdr p))
	     (pop list-1)))))

(defun single-float-ascending-sort-list (list &optional key)
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (let ((head (cons :header list))	; head holds on to everything
	(n 1)				; bottom-up size of lists to be merged
	unsorted			; unsorted is the remaining list to be
					;   broken into n size lists and merged
	list-1				; list-1 is one length n list to be merged
	last)				; last points to the last visited cell
    (declare (fixnum n))
    (loop
     ;; start collecting runs of n at the first element
     (setf unsorted (cdr head))
     ;; tack on the first merge of two n-runs to the head holder
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
	(setf list-1 unsorted)
	(let ((temp (nthcdr n-1 list-1))
	      list-2)
	  (cond (temp
		 ;; there are enough elements for a second run
		 (setf list-2 (cdr temp))
		 (setf (cdr temp) nil)
		 (setf temp (nthcdr n-1 list-2))
		 (cond (temp
			(setf unsorted (cdr temp))
			(setf (cdr temp) nil))
		       ;; the second run goes off the end of the list
		       (t (setf unsorted nil)))
		 (multiple-value-bind (merged-head merged-last)
		     (single-float-<-merge-lists* list-1 list-2 key)
		   (setf (cdr last) merged-head)
		   (setf last merged-last))
		 (if (null unsorted) (return)))
		;; if there is only one run, then tack it on to the end
		(t (setf (cdr last) list-1)
		   (return)))))
       (setf n (ash n 1))		; (+ n n)
       ;; If the inner loop only executed once, then there were only enough elements for two runs given n, so all the elements
       ;; have been merged into one list.  This may waste one outer iteration to realize.
       (if (eq list-1 (cdr head))
	 (return list-1))))))

(defun closest-n-1-vals (list)
  "Takes a LIST of N numbers and returns a sorted list of the N-1 closest values."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind (median sorted-list)
      (median list)
    (declare (single-float median)
	     (cons sorted-list))
    (if (> (- median (the sf (car sorted-list)))
	   (- (the sf (car (last sorted-list))) median))
      (cdr sorted-list)
      (butlast sorted-list))))

(defun average-closest-n-1-vals (list)
  "Takes a LIST of N single-float numbers and returns the single float average of the N-1 closest values."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (loop for val single-float in list
	maximizing val into max single-float
	minimizing val into min single-float
	summing val into total single-float
	finally (return (let* ((length (length (the cons list)))
			       (average (/ total length)))
			  (/ (if (> (- average min) (- max average))
			       (- total min)
			       (- total max))
			     (1- length))))))

(defun in-middle-kludge (x-min x x-max)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x))
  (cond ((and x-min x-max) (<= (the sf x-min) x (the sf x-max)))
	(x-min (<= (the sf x-min) x))
	(x-max (<= x (the sf x-max)))))

(defun get-nice-mag (number &optional mag (decimals 2))
  (cond ((= number 0.0) 0.0)
	((and (numberp mag) (= mag 0)) number)
	(t
	 (let* ((mag (or mag number))
		(alpha (- (round (log (abs mag) 10)) decimals)))
	   (* (round number (expt 10.0 alpha))
	      (expt 10.0 alpha))))))

(defun msd-round (num)
  (if (= num 0) num
      (let ((factor (expt 10 (floor (log (abs num) 10)))))
	(float (* factor (round num factor))))))

(defun msd-floor (num)
  (if (= num 0) num
      (let ((factor (expt 10 (floor (log (abs num) 10)))))
	(float (* factor (floor num factor))))))

(defun msd-ceiling (num)
  (if (= num 0) num
      (let ((factor (expt 10 (floor (log (abs num) 10)))))
	(float (* factor (ceiling num factor))))))

(defun coerce-to-even-int (num)
  (* 2 (round (/ num 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cos-degrees (degrees) (cos (* 2.0 pi-single (/ degrees 360.0))))

(defun sin-degrees (degrees) (sin (* 2.0 pi-single (/ degrees 360.0))))

(defun rad-to-deg (angle-in-radians)
  "Return the angle in degrees corresponding to ANGLE-IN-RADIANS."
  (let ((angle (* angle-in-radians degrees/radian)))
    (if (< angle 0) (+ 360 angle) angle)))

(defun deg-to-rad (angle-in-degrees)
  "Return the angle in radians corresponding to ANGLE-IN-DEGREES."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (s-flt (* angle-in-degrees radians/degree)))


(defun square (x)
  (declare (single-float x))
  (* x x))

(defun df-square (x)
  (declare (double-float x))
  (* x x))

(proclaim '(inline cartesian-distance-float))
(defun cartesian-distance-float (x1 y1  &optional (x2 0.0) (y2 0.0))
  "Return as a single float value the cartesian distance between the two single float points [X1 Y1] and the optional [X2 Y2]. X2 and Y2 default to the
origin."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x1 x2 y1 y2))
  (let ((dx (- x1 x2))
	(dy (- y1 y2)))
    (sqrt (the (single-float * 0.0) (+ (* dx dx) (* dy dy))))))

#|
(defun cartesian-distance (x1 y1 &optional (x2 0.0) (y2 0.0))
  "Return the cartesian distance between the two points [X1 Y1] and the optional [X2 Y2]. X2 and Y2 default to the origin."
  (cartesian-distance-float (float x1) (float y1) (float x2) (float y2)))
|#

(defun cartesian-distance (x1 y1  &optional (x2 0.0) (y2 0.0))
  "Return as a single float value the cartesian distance between the two single float points [X1 Y1] and the optional [X2 Y2]. X2 and Y2 default to the
origin."
  (let ((dx (- x1 x2))
	(dy (- y1 y2)))
    (sqrt (+ (* dx dx) (* dy dy)))))


(defun cartesian-vector-distance (x1 x2)
  "Return the cartesian distance between the two lists of numbers, X1 and X2, of the same arbitrary length. Returns a single float
value."
  (sqrt (loop for comp1 in x1
	      for comp2 in x2
	      summing (square (float (- comp1 comp2))))))

(defun cartesian-vector-distance-float (x1 x2)
  "Return the cartesian distance between the two lists of single float
   numbers, X1 and X2, of the same arbitrary length. Returns a single
   float value."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (do* ((x1 (the cons x1) (cdr x1))
	(x2 (the cons x2) (cdr x2))
	(sum (the sf (square (the sf (- (the sf (car x1)) (the sf (car x2))))))
	     (the sf (+ sum (the sf (square (the sf (- (the sf (car x1))
						       (the sf (car x2))))))))))
       ((null x1)
	(cond ((< sum 0)
	       0.0)
	      (t
	       (the sf (coerce (sqrt (coerce sum 'double-float)) 'single-float)))))))


(defun max-of-only-nums (&rest args) (apply 'max (no-nils args)))
(defun min-of-only-nums (&rest args) (apply 'min (no-nils args)))

(export '(MAX-OF-ONLY-NUMS MIN-OF-ONLY-NUMS))

(proclaim '(inline bound-int-val))
(defun bound-int-val (val max min)
  (declare (type fixnum val max min))
  (min (max val min) max))

(proclaim '(inline bound-val))
(defun bound-val (val max min) (min (max val min) max))

;;;;;;;;;;;;;;;
;; Array
;;;;;;;;;;;;;;;

(defun 3-by-3-identity ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((array (make-array '(3 3) :initial-element 0.0 :element-type 'single-float)))
    (declare (type (simple-array single-float (3 3)) array))
    (dotimes (i 3)
      (declare (fixnum i))
      (setf (aref array i i) 1.0))
    array))

;;;;;;;;;;;;;;;
;; Logical
;;;;;;;;;;;;;;;

(defun t-or-nil-p (thing)
  "Predicate for the explicit symbols T or NIL."
  (or (equal thing t)
      (equal thing nil)))

(defun xor (a b) (or (and a (not b)) (and (not a) b)))

(defun true-p (thing) (not (null thing)))

(defun negative-p (num) (< num 0))

(defun positive-p (num) (> num 0))

(defun non-negative-p (num) (>= num 0))

(defun non-positive-p (num) (<= num 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linear Regression Code
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lxx (sequence)
  ;; Return the population variance of the number sequence.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((list (sequence-to-list sequence))
	 (df-values (double-float-in-list-p list))
	 (df-list (d-flt-gen list))
	 (n (length (the cons list)))
	 (sumx (sum-double-float-list df-list))
	 (sumxx (sum-double-float-listx*listx df-list))
	 (lxx (lxx-from-sumx-sumxx-n sumxx sumx n)))
    (if df-values lxx (s-flt lxx))))

(defun lxx-sf-array (array)
  ;; Return the population variance of the number array.
  (declare (optimize (safety 2) (speed 3) (space 0))
	   (type (vector single-float) array))
  (let* ((n (length array))
	 (sumx (sum-single-float-array array))
	 (sumxx (sum-single-float-arrayx*arrayx array))
	 (lxx (lxx-from-sumx-sumxx-n sumxx sumx n)))
    (when (< lxx 0) (sim-error (format nil "lxx-sf-array with ARRAY ~A gives negative answer ~A" array lxx)))
    lxx))

(defun lxx-df-array (array)
  ;; Return the population variance of the number array.
  (declare (optimize (safety 2) (speed 3) (space 0))
	   (type (vector double-float) array))
  (let* ((n (length array))
	 (sumx (sum-double-float-array array))
	 (sumxx (sum-double-float-arrayx*arrayx array))
	 (lxx (lxx-from-sumx-sumxx-n sumxx sumx n)))
    (when (< lxx 0) (sim-error (format nil "lxx-sf-array with ARRAY ~A gives negative answer ~A" array lxx)))
    lxx))

(defun std-dev (sequence) (sqrt (lxx sequence)))

(defun std-dev-sf-array (array)
  (sqrt (LXX-SF-ARRAY array)))

(defun std-dev-df-array (array)
  (sqrt (LXX-dF-ARRAY array)))


(defun lxx-from-sumx-sumxx-n (sumxx sumx n)
  (/ (- sumxx (/ (* sumx sumx) n)) n))

(defun lxy-from-sumx-sumxy-sumy-n (sumx sumxy sumy n)
  (/ (- sumxy (/ (* sumx sumy) n)) n))

(defun lxy (sequence-x sequence-y)
  ;; Return the covariance of the number lists.
  (let* ((listx (sequence-to-list sequence-x))
	 (listy (sequence-to-list sequence-y))
	 (df-values (loop for list in (list listx listy) thereis (double-float-in-list-p list)))
	 (df-listx (d-flt-gen listx))
	 (df-listy (d-flt-gen listy))
	 (n (length (the cons listx)))
	 (sumx (sum-double-float-list df-listx))
	 (sumy (sum-double-float-list df-listy))
	 (sumxy (sum-double-float-listx*listy df-listx df-listy))
	 (lxy (lxy-from-sumx-sumxy-sumy-n sumx sumxy sumy n)))
    (if df-values lxy (s-flt lxy))))

(defun r (sequence-x sequence-y)
  "Return the correlation coefficient of the number sequences."
  (/ (lxy sequence-x sequence-y) (sqrt (* (lxx sequence-x) (lxx sequence-y)))))

(defun r-from-lxx-lxy-lyy (lxx lxy lyy)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float lxx lxy lyy))
  (if (or (= lxy lyy 0) (= lxy lxx 0))
    0.0d0
    (if (= 0 (* lxx lyy))
      1.0d0
      (/ lxy (the df (sqrt (the df (* lxx lyy))))))))

(defun lin-reg (xylists &optional print-results)
  "Calculates linear regression values for XYLISTS, which has the format ((X-LIST) (Y-LIST)). Internally calculations are done in double precision,
but returned number types are either single or double float depending on the types of the arguments. When PRINT-RESULTS is T then linear regression
info is printed to the standard output. Returns values as (SLOPE INTERCEPT CORRELATION-COEFF). SLOPE may be a number, :UNDEFINED or :INFINITE. The
INTERCEPT [on the X axis] may be a number or :UNDEFINED."
  (let* ((listx (car xylists))
	 (listy (cadr xylists))
	 (df-arrayx (d-flt-array listx))
	 (df-arrayy (d-flt-array listy)))
    (lin-reg-core df-arrayx df-arrayy :print-results print-results)))

(defvar *lin-reg-slope-ok* 0)
(defvar	*lin-reg-slope-undefined* 1)
(defvar *lin-reg-slope-infinite* 2)
(proclaim '(fixnum *lin-reg-slope-ok* *lin-reg-slope-undefined *lin-reg-slope-infinite))

(defun lin-reg-slope-ok-p (slope-annotation) (= slope-annotation *lin-reg-slope-ok*))
(defun lin-reg-slope-infinite-p (slope-annotation) (= slope-annotation *lin-reg-slope-infinite*))
(defun lin-reg-slope-undefined-p (slope-annotation) (= slope-annotation *lin-reg-slope-undefined*))

(defun lin-reg-slope-annotation-string (slope-annotation)
  (cond ((lin-reg-slope-undefined-p slope-annotation) "(slope undefined)")
	((lin-reg-slope-infinite-p slope-annotation) "(slope infinite)")
	((lin-reg-slope-ok-p slope-annotation) "(slope ok)")
	(t "")))

(export '(lin-reg-slope-ok-p lin-reg-slope-infinite-p lin-reg-slope-undefined-p lin-reg-slope-annotation-string))


(defun lin-reg-core (df-arrayx df-arrayy &key print-results debug (equal-xs nil equal-xs-supplied) (equal-ys nil equal-ys-supplied))
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type (vector double-float) df-arrayx df-arrayy)
	   )
  (let* ((n (length df-arrayx))
	 (equal-xs (if equal-xs-supplied equal-xs (EQUAL-DOUBLE-FLOAT-ARRAY df-arrayx)))
	 (equal-ys (if equal-ys-supplied equal-ys (EQUAL-DOUBLE-FLOAT-ARRAY df-arrayy)))
	 (sumx 0.0d0)
	 (sumy 0.0d0)
	 (sumxy 0.0d0)
	 (sumxx 0.0d0)
	 (sumyy 0.0d0)

	 (slope 0.0d0)
	 (intercept 0.0d0)
	 (meanx 0.0d0)
	 (meany 0.0d0)
	 (lxy 0.0d0)
	 (lxx 0.0d0)
	 (lyy 0.0d0)
	 (r 0.0d0)
	 (slope-num 0.0d0)
	 (slope-den 0.0d0)
	 (slope-annotation *lin-reg-slope-ok*) ; see 3 entries below
	 (slope-ok *lin-reg-slope-ok*)
	 (slope-undefined *lin-reg-slope-undefined*)
	 (slope-infinite *lin-reg-slope-infinite*))
    (declare (fixnum n slope-annotation slope-ok slope-undefined slope-infinite)
	     (double-float sumx sumy sumxy sumxx sumyy meanx meany slope-num slope-den))
    (setq sumx (sum-double-float-array df-arrayx)
	  sumy (sum-double-float-array df-arrayy))
    (if (= n 2)
      (setq slope-num (- (the df (aref df-arrayy 0)) (the df (aref df-arrayy 1)))
	    slope-den (- (the df (aref df-arrayx 0)) (the df (aref df-arrayx 1))))
      (setq
       sumxy (sum-double-float-arrayx*arrayy df-arrayx df-arrayy)
       sumxx (sum-double-float-arrayx*arrayx df-arrayx)
       sumyy (sum-double-float-arrayx*arrayx df-arrayy)
       slope-num (- (* n sumxy) (* sumy sumx))
       slope-den (- (* n sumxx) (* sumx sumx))))
    (when debug (format t "n: ~A sumx: ~A sumy: ~A sumxy: ~a sumxx: ~A num: ~A den: ~A~%" n sumx sumy sumxy sumxx slope-num slope-den))

    (cond
     ((or (and equal-xs equal-ys) (= 0 slope-den slope-num)) (setq slope-annotation slope-undefined))
     ((or equal-xs (= 0 slope-den)) (setq slope-annotation slope-infinite) (setq intercept (aref df-arrayx 0)))
     (equal-ys 0.0d0)			; SLOPE is initialized above to 0.0d0
     (t (setq slope (/ slope-num slope-den))))
    (setq intercept (/ (- sumy (* slope sumx)) n))
    (unless (= n 2)
      (setq meanx (/ sumx n)
	    meany (/ sumy n))
      (setq lxy (- sumxy (/ (* sumx sumy) n))
	    lxx (- sumxx (/ (* sumx sumx) n))
	    lyy (- sumyy (/ (* sumy sumy) n)))
      (when debug (format t "lxy: ~A lxx: ~A lyy: ~a~%" lxy lxx lyy)))
    (setq r (if (= n 2) 1.0d0 (r-from-lxx-lxy-lyy lxx lxy lyy)))
    (when print-results
      (format t "~%mx=~a, varx=~a, my=~a, vary=~a, covxy=~a~%" meanx (lxx df-arrayx) meany (lxx df-arrayy) (lxy df-arrayx df-arrayy))
      (format t "Slp=~a, Intcpt= ~a, r = ~a ~A~%" slope intercept r (lin-reg-slope-annotation-string slope-annotation)))
    (values (s-flt slope) (s-flt intercept) (s-flt r) slope-annotation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptive Statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mean (sequence)
  "Returns single-float mean of SEQUENCE."
  (float (/ (integrate-sequence sequence) (length sequence))))

(defun integrate-sequence (sequence)
  (typecase sequence
    (cons  (loop for val in sequence sum val))
    (array (loop for val across sequence sum val))))

(defun integrate-sf-sequence (sequence)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (typecase sequence
    (cons  (loop for val single-float in sequence sum val into out single-float finally
		 (return out)))
    (array (loop for val single-float across (the (vector single-float) sequence)
		 sum val into out single-float finally
		 (return out)))))

(defun integrate-df-sequence (sequence)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (typecase sequence
    (cons  (loop for val double-float in sequence sum val into out double-float finally
		 (return out)))
    (array (loop for val double-float across (the (vector double-float) sequence)
		 sum val into out double-float finally
		 (return out)))))

(defun mean-sf (sequence)
  "Returns single-float mean of single-float SEQUENCE, which may be a list or simple array."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (/ (the sf (INTEGRATE-SF-SEQUENCE SEQUENCE))
     (the fixnum (length SEQUENCE))))

(defun mean-df (sequence)
  "Returns double-float mean of single-float SEQUENCE, which may be a list or simple array."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (/ (the df (INTEGRATE-dF-SEQUENCE SEQUENCE))
     (the fixnum (length SEQUENCE))))

(defun ss (sequence)
  "Returns single-float sum of squares of SEQUENCE."
  (s-flt (apply '+ (mapcar 'square (s-flt-list sequence)))))

(defun ss-sf (sequence)
  "Returns single-float sum of squares of the single-float SEQUENCE, which may be a list or simple array."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (typecase sequence
    (cons (loop for val single-float in sequence sum (* val val) into out single-float finally (return out)))
    (array (loop for val single-float across (the (vector single-float) sequence) sum (* val val) into out single-float finally (return out)))))

(defun rms-sf (sequence)
  "Returns single-float root mean square of the single-float SEQUENCE, which may be a list or simple array."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (sqrt (/ (the sf (ss-sf sequence))
	   (the fixnum (length sequence)))))

(defun rms (sequence)
  "Returns root mean square of SEQUENCE."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (sqrt (/ (the sf (ss sequence))
	   (the fixnum (length sequence)))))

(defun median (list)
  "Returns the single-float median of LIST - when the length of LIST is even, then the median is the average of the values
flanking the true median. Also returns a copy of LIST, converted to single floats and sorted."
  (declare (optimize (safety 2) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((length (length (the cons list)))
	 (length/2 (/ length 2))
	 (sorted-list (single-float-ascending-sort-list (s-flt-and-copy-list list))))
    (values (if (evenp length)
	      (/ (the sf (+ (the sf (nth (1- (the fn length/2)) sorted-list))
			    (the sf (nth (the fn length/2) sorted-list)))) 2)
	      (the sf(nth (if (= length 3) 1 (round length/2)) sorted-list)))
	    sorted-list)))

(defun integrate-x-y (y-list x-list &key x-max x-min (y-base 0.0) average)
  ;; Lists must contain single floats. X-LIST may be a list of numbers or a single delta-x value.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((x-list (if (consp x-list) x-list (list-of-nums (length y-list) 0.0 x-list))))
    (when y-list
      (when x-max (setq x-max (s-flt x-max)))
      (when x-min (setq x-min (s-flt x-min)))
      (setq y-base (s-flt y-base))
      (let ((sum 0.0))
	(do* ((x-n (car x-list) x-n-1)
	      (x-n-1 (cadr x-list) (car x-list))
	      (x-list (cddr x-list) (cdr x-list))
	      (y-n (car y-list) y-n-1)
	      (y-n-1 (cadr y-list) (car y-list))
	      (y-list (cddr y-list) (cdr y-list)))
	    ((null x-n-1) sum)
	  (declare (single-float y-base sum))
	  (setq sum (+ sum (if (and (or (not x-max) (> (the sf x-max) (the sf x-n)))
				    (or (not x-min) (< (the sf x-min) (the sf x-n))))
			     (* (abs (- (the sf x-n) (the sf x-n-1))) (- (/ (+ (the sf y-n) (the sf y-n-1)) 2.0) y-base))
			     0.0))))
	(if average (/ sum (abs (- (the sf (first x-list)) (the sf (car (last x-list)))))) sum)))))

(export '(LXX-DF-ARRAY STD-DEV-DF-ARRAY INTEGRATE-DF-SEQUENCE MEAN-DF INTEGRATE-SEQUENCE))

(export '(half t-or-nil-p
	  float-floor
	  round-up-to-power-of-2
	  TRUE-P negative-p positive-p non-negative-p non-positive-p
	  s-flt-and-copy-list
	  single-float-<-merge-lists*
	  single-float-ascending-sort-list
	  average-closest-n-1-vals
	  closest-n-1-vals
	  in-middle-kludge
	  GET-NICE-MAG
	  msd-round MSD-CEILING MSD-FLOOR

	  cos-degrees sin-degrees
	  coerce-to-even-int
	  rad-to-deg deg-to-rad
	  xor
	  square
	  df-square
	  cartesian-distance cartesian-distance-float cartesian-vector-distance cartesian-vector-distance-float
	  bound-val
	  bound-int-val
	  3-by-3-identity

	  	  lxx
		  LXX-SF-ARRAY
	  STD-DEV STD-DEV-SF-ARRAY
	  lxy
	  r
	  ss ss-sf rms rms-sf
	  INTEGRATE-SF-SEQUENCE MEAN-SF

	  mean median
	  lin-reg
	  lin-reg-float-core-array
;	  LIN-REG-FLOAT
;	  LIN-REG-FLOAT-VALUES
	  LIN-REG-CORE
	  integrate-x-y
	  ))

;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;;; Discrete Fourier Transform

;;;  (c) Copyright Gerald Roylance 1983, 1984
;;;      All Rights Reserved.
;;;  This file may be distributed noncommercially provided
;;;  that this notice is not removed.

;;; Reference
;;;   Alan Oppenheim and Ronald Schafer, 
;;;     Digital Signal Processing
;;;     Prentice Hall, 1975

;;; Bugs and Fixes
;;;   do 4x and 8x butterfly instead of just 2x
;;;   use complex arithmetic
;;;   sign error fixed:  W = e^{- 2 pi / N}

(in-package "USER")

(export '(dft-array dft-float
		    dft-init
		    dft-forward
		    dft-reverse
		    dft-reverse-array
		    dft-610
		    dft-618))

(provide "DFT")

;;; Transform conventions

;;; time -> frequency
;;;   bit reverse the time array
;;;   output is in correct order

;;; frequency -> time
;;;    freq in correct order
;;;    time is output in bit reversed order

(deftype dft-array (size) `(simple-array single-float ,size))
(deftype dft-float (    ) '              single-float       )


;;;; Initialization Code

;;; Precompute Tables of complex exponential
;;;
(defstruct (dft-precomputed)
  (size     0 :type fixnum)
  (w-real NIL)
  (w-imag NIL))

(defun dft-init (log-size)
  (declare (type fixnum log-size))
  (let* ((size   (ash 1 log-size))
	 (size2  (ash size -1))
	 (w-real (make-array size2 :element-type 'dft-float))
	 (w-imag (make-array size2 :element-type 'dft-float))
	 (struct (make-dft-precomputed :size   log-size
				       :w-real w-real
				       :w-imag w-imag)))
    (declare (fixnum size size2)
	     (type (dft-array (*)) w-real w-imag))

    (do ((i 0 (1+ i))
	 (angle (/ (* -2.0 (float pi 1.0)) (float size))))
	((>= i size2))
      (declare (fixnum i)
	       (float angle))

      (setf (aref w-real i) (cos (* (float i) angle)))
      (setf (aref w-imag i) (sin (* (float i) angle))))

    struct))


;;;; Forward transform

;;; figure 6-10 in Oppenheim
;;; time is bit-reversed, frequency is in order

(defun dft-610 (x-real x-imag tables)
  (declare (type (dft-array (*)) x-real x-imag))
  (let* ((ln       (dft-precomputed-size tables))
	 (n        (ash 1 ln))
	 (n-over-2 (ash n -1))
	 (w-n-real (dft-precomputed-w-real tables))
	 (w-n-imag (dft-precomputed-w-imag tables)))
    (declare (fixnum ln n n-over-2)
	     (type (dft-array (*)) w-n-real w-n-imag))
    (do ((i                1      (1+ i))	; each stage
	 (butter-dis       1      (the fixnum (*  butter-dis 2)))
	 (separation       2      (the fixnum (*  separation 2)))
	 (repetition  (ash n -1) (ash repetition -1))
	 (w-real 0.0) (w-imag 0.0)
	 (x0     0.0) (x1     0.0))
	((> i ln))
      (declare (fixnum i butter-dis separation repetition)
	       (float w-real w-imag x0 x1))
      (do ((k 0 (+ k repetition))		; each W factor
	   (l 0 (1+ l)))
	  ((>= k n-over-2))
	(declare (fixnum k l))
	(setq w-real (aref w-n-real k))
	(setq w-imag (aref w-n-imag k))
	(do ((j l (+ j separation)))		; each butterfly
	    ((>= j n))
	  (declare (fixnum j))
	  ;; multiply x[j+butter-dis] by W
	  (setq x0 (aref x-real (+ j butter-dis)))
	  (setq x1 (aref x-imag (+ j butter-dis)))
	  (setf (aref x-real (+ j butter-dis))
		(- (* w-real x0) (* w-imag x1)))
	  (setf (aref x-imag (+ j butter-dis))
		(+ (* w-real x1) (* w-imag x0)))

	  ;; butterfly j and j+butter-dis
	  (setq x0 (aref x-real (+ j 0)))
	  (setq x1 (aref x-real (+ j butter-dis)))
	  (setf (aref x-real (+ j          0)) (+ x0 x1))
	  (setf (aref x-real (+ j butter-dis)) (- x0 x1))

	  (setq x0 (aref x-imag (+ j 0)))
	  (setq x1 (aref x-imag (+ j butter-dis)))
	  (setf (aref x-imag (+ j          0)) (+ x0 x1))
	  (setf (aref x-imag (+ j butter-dis)) (- x0 x1))
	  )))))


;;;; Reverse transform

;;; figure 6-18 in Oppenheim
;;; frequency is in order, time is bit-reversed

(defun dft-618 (x-real x-imag tables)
  (declare (type (dft-array (*)) x-real x-imag))
  (let* ((ln       (dft-precomputed-size   tables))
	 (n        (ash 1 ln))
	 (n-over-2 (ash n -1))
	 (w-n-real (dft-precomputed-w-real tables))
	 (w-n-imag (dft-precomputed-w-imag tables)))
    (declare (fixnum ln n n-over-2)
	     (type (dft-array (*)) w-n-real w-n-imag))
    (do ((i                 1    (1+ i))	; each stage
	 (butter-dis   (ash n -1)(the fixnum (ash   butter-dis -1)))
	 (separation        n    (the fixnum (ash   separation -1)))
	 (repetition        1    (*     repetition  2))
	 (w-real 0.0) (w-imag 0.0)
	 (x0     0.0) (x1     0.0))
	((> i ln))
      (declare (fixnum i butter-dis separation repetition)
	       (float w-real w-imag x0 x1)) 
      (do ((k 0 (+ k repetition))		; each W factor
	   (l 0 (1+ l)))
	  ((>= k n-over-2))
	(declare (fixnum k l))
	;; hacked for W**-k
	(setq w-real     (aref w-n-real k))
	(setq w-imag (- (aref w-n-imag k)))
	(do ((j l (+ j separation)))		; each butterfly
	    ((>= j n))
	  (declare (fixnum j))
	  ;; butterfly j and j+butter-dis
	  (setq x0 (aref x-real (+ j 0)))
	  (setq x1 (aref x-real (+ j butter-dis)))
	  (setf (aref x-real (+ j          0)) (+ x0 x1))
	  (setf (aref x-real (+ j butter-dis)) (- x0 x1))

	  (setq x0 (aref x-imag (+ j 0)))
	  (setq x1 (aref x-imag (+ j butter-dis)))
	  (setf (aref x-imag (+ j          0)) (+ x0 x1))
	  (setf (aref x-imag (+ j butter-dis)) (- x0 x1))
	  
	  ;; multiply x[j+butter-dis] by W
	  (setq x0 (aref x-real (+ j butter-dis)))
	  (setq x1 (aref x-imag (+ j butter-dis)))
	  (setf (aref x-real (+ j butter-dis))
		(- (* w-real x0) (* w-imag x1)))
	  (setf (aref x-imag (+ j butter-dis))
		(+ (* w-real x1) (* w-imag x0)))
	  )))
    (do ((i 0 (1+ i)))
	((>= i n))
      (declare (fixnum i))
      (setf (aref x-real i) (/ (aref x-real i) (float n)))
      (setf (aref x-imag i) (/ (aref x-imag i) (float n))))
    ))


;;;; Reverse Bits

;;; The basic bit reverse calculation is fast, so using a bit-reverse
;;; table isn't necessary (and may be detrimental -- it's a 128KB table).

(defun dft-bit-rev-init (array)
  (flet ((brev (n)
	   (let ((r     n)
		 (mask1 #2r0101010101010101)
		 (mask2 #2r0011001100110011)
		 (mask4 #2r0000111100001111)
		 (mask8 #2r0000000011111111))
	     (declare (type (unsigned-byte 16) r mask1 mask2 mask4 mask8)
		      (ftype (function ((unsigned-byte 16) (unsigned-byte 16))
				       (unsigned-byte 16))
			     logior logand ash))

	     (setq r (logior (logand (ash r -1) mask1) (ash (logand r mask1)  1)))
	     (setq r (logior (logand (ash r -2) mask2) (ash (logand r mask2)  2)))
	     (setq r (logior (logand (ash r -4) mask4) (ash (logand r mask4)  4)))
	     (setq r (logior (logand (ash r -8) mask8) (ash (logand r mask8)  8))))))

    (dotimes (i (length array))
      (setf (aref array i) (brev i)))

    array))

(defvar *dft-bit-reverse-16*
	(dft-bit-rev-init (make-array (ash 1 16) :element-type '(unsigned-byte 16))))

(proclaim '(inline dft-reverse-bits-16))

(defun dft-reverse-bits-16 (n)
  (the (unsigned-byte 16)
       (aref (the (simple-array (unsigned-byte 16) (*)) *dft-bit-reverse-16*) n)))

(defun dft-reverse-array (array tables)
  (declare (type (dft-array (*)) array))
  (let* ((log-size (dft-precomputed-size tables))
	 (size     (ash 1 log-size)))
    (declare (fixnum log-size size))
    (if (or (< log-size  0.)
	    (> log-size 16.)
	    (> (array-dimension array 0) size))
	(error "DFT-REVERSE-ARRAY given bad args"))
    (do ((i      0 (1+ i))
	 (j      0)
	 (shift  (- log-size 16.))
	 (temp 0.0))
	((>= i size))
      (declare (type fixnum i j shift)
	       (type float temp))
      (setq j (ash (dft-reverse-bits-16 i) shift))
      (when (< i j)
	(setq temp                         (aref array i))
	(setf (aref array i) (aref array j))
	(setf (aref array j) temp)))))


;;;; Forward and Reverse Transforms

;;; So you don't have to worry about bit-reversing ...

(defun dft-forward (x-real x-imag tables)
  (dft-reverse-array x-real tables)
  (dft-reverse-array x-imag tables)
  (dft-610 x-real x-imag tables))

(defun dft-reverse (x-real x-imag tables)
  (dft-618 x-real x-imag tables)
  (dft-reverse-array x-real tables)
  (dft-reverse-array x-imag tables))


;;;; DFT for a 2D array (not setup well...)

;;; Following David Beymer
;;;
;;; NxN image,
;;;   dft (k,l) = \sum_x \sum_y f(x,y) exp(-j*2*pi*x*k/N) exp(-j*2*pi*l*y/N)
;;;
;;; The above expression is separable in k and l, so the 2D dft can be broken
;;; up into N 1D dft's along the rows, and N 1D dft's along the columns.
;;;
;;; The 2D idft is similar to the 2D dft, except for a change in sign in the
;;; exponential terms and a division by N^2 out in front.

(defun dft-2d-sub-for (2d-real 2d-imag 1d-real 1d-imag precomp)
  (declare (type (dft-array (* *)) 2d-real 2d-imag)
	   (type (dft-array   (*)) 1d-real 1d-imag))

  (unless (and (arrayp 2d-real) (arrayp 2d-imag)
	       (arrayp 1d-real) (arrayp 1d-imag)
	       (= (array-rank 2d-real) (array-rank 2d-imag) 2)
	       (= (array-rank 1d-real) (array-rank 1d-imag) 1)
	       (= (array-dimension 2d-real 0) (array-dimension 2d-real 1)
		  (array-dimension 2d-imag 0) (array-dimension 2d-imag 1)
		  (array-dimension 1d-real 0) (array-dimension 2d-imag 1)))
    (error "dft-2d-sub-for:  bad array arguments"))

  (let ((n (array-dimension 2d-real 0)))

    (dotimes (i n)				; do each row
      ;; copy the row into the 1d work vectors
      (dotimes (j n)
	(setf (aref 1d-real j) (aref 2d-real i j))
	(setf (aref 1d-imag j) (aref 2d-imag i j)))

      ;; do the forward dft
      (dft-forward 1d-real 1d-imag precomp)

      ;; copy the row back to the array
      (dotimes (j n)
	(setf (aref 2d-real i j) (aref 1d-real j))
	(setf (aref 2d-imag i j) (aref 1d-imag j))))

    (dotimes (j n)				; do each column
      ;; copy the column into the 1d work vectors
      (dotimes (i n)
	(setf (aref 1d-real i) (aref 2d-real i j))
	(setf (aref 1d-imag i) (aref 2d-imag i j)))

      ;; do the forward dft
      (dft-forward 1d-real 1d-imag precomp)

      ;; copy the column back to the array
      (dotimes (i n)
	(setf (aref 2d-real i j) (aref 1d-real i))
	(setf (aref 2d-imag i j) (aref 1d-imag i))))
    ))

(defun dft-foo (log-n)
  (let* ((precomp (dft-init log-n))
	 (size    (ash 1 log-n))
	 (2d-real (make-array `(,size ,size) :element-type 'single-float :initial-element 0.0))
	 (2d-imag (make-array `(,size ,size) :element-type 'single-float :initial-element 0.0))
	 (1d-real (make-array          size  :element-type 'single-float :initial-element 0.0))
	 (1d-imag (make-array          size  :element-type 'single-float :initial-element 0.0)))
    (setf (aref 2d-real 0 0) 1.0)
    (time 
      (dft-2d-sub-for 2d-real 2d-imag 1d-real 1d-imag precomp))
    (values 2d-real 2d-imag)))


;;;; Tests

(defun dft-test-print-array (array)
  (declare (type (dft-array (*)) array))
  (terpri)
  (do ((i 0 (1+ i))
       (n (array-dimension array 0)))
      ((>= i n))
    (declare (fixnum i n))
    (format t "~3,1,8$" (aref array i))))

(defun dft-test-print (title x y)
  (declare (type (dft-array (*)) x y))
  (print title)
  (dft-test-print-array x)
  (dft-test-print-array y))

(defun dft-test ()
  (let* ((log-size 3)
	 (n        (expt 2 log-size))
	 (tables   (dft-init log-size))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float)))
    (declare (fixnum log-size n)
	     (type (dft-array (*)) x-real x-imag))

    (do ((i 0 (1+ i)))
	((>= i n))
      (declare (fixnum i))
      (setf (aref x-real i)
	    (cos (/ (*  4.0 (float pi 1.0) (float i)) (float n))))
      (setf (aref x-imag i) 0.0))

    (dft-test-print 'time-domain x-real x-imag)
    (dft-forward x-real x-imag tables)
    (dft-test-print 'frequency   x-real x-imag)
    (dft-reverse x-real x-imag tables)
    (dft-test-print 'time-domain x-real x-imag)
    ))


;;;; Speed Tests

(defun dft-usec (time1 time0)
  (round (* 1000000 (- time1 time0))
	 internal-time-units-per-second))

(defun dft-speed-test (dft-log-size)
  (declare (fixnum dft-log-size))
  (let ((dft-tables (dft-init dft-log-size))
	(x-real     (make-array (the fixnum (ash 1 dft-log-size)) :element-type 'dft-float))
	(x-imag     (make-array (the fixnum (ash 1 dft-log-size)) :element-type 'dft-float))
	(time0 0)
	(time1 0)
	(time2 0)
	(time3 0))
    (declare (fixnum time0 time1 time2 time3)
	     (type (dft-array (*)) x-real x-imag))

    (dotimes (i (the fixnum (ash 1 dft-log-size)))
      (setf (aref x-real i) 1.0)
      (setf (aref x-imag i) 0.0))
      
    (setq time0 (get-internal-run-time))
    (dft-reverse-array x-real dft-tables)
    (setq time1 (get-internal-run-time))
    (dft-610 x-real x-imag dft-tables)
    (setq time2 (get-internal-run-time))
    (setq time3 (get-internal-run-time))

    (format t "~%  Bit reverse time ~8D microseconds" (dft-usec time1 time0))
    (format t "~%          DFT time ~8D microseconds" (dft-usec time2 time1))
    (format t "~%        Total time ~8D microseconds" (dft-usec time2 time0))
    (format t "~% Call to (GET-INTERNAL-RUN-TIME) ~8D microseconds" (dft-usec time3 time2))
    (format t "~%             Speed ~8F N LOG2(N)"
	    (/ (float (dft-usec time2 time1))
	       (float dft-log-size)
	       (float (the fixnum (ash 1 dft-log-size)))))
    ))

(defun mpy-time (n)
  (declare (type fixnum n))
  (do ((time (get-internal-run-time))
       (i    0 (1+ i))
       (x    3.14159))
      ((>= i n)
       (/ (float (dft-usec (get-internal-run-time) time))
	  (float n)))
    (declare (fixnum i time) (float x))
    (* x x))
  )

(defun add-time (n)
  (declare (type fixnum n))
  (do ((time (get-internal-run-time))
       (i    0 (1+ i))
       (x    3.14159))
      ((>= i n)
       (/ (float (dft-usec (get-internal-run-time) time))
	  (float n)))
    (declare (fixnum i time) (float x))
    (+ x x)))

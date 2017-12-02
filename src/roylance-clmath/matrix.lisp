;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;;; Matrix Routines

;;; References
;;;   David G. Luenberger,
;;;     Introduction to Linear and Nonlinear Programming,
;;;     Addison Wesley, Reading, MA, 1965.

;;;  (c) Copyright Gerald Roylance 1983, 1985, 1986
;;;      All Rights Reserved.
;;;  This file may be distributed noncommercially provided
;;;  that this notice is not removed.

;;; Bugs and Fixes
;;;   Use BLAS
;;;   Is it possible to recover sparseness?
;;;     at the elimination step, check if coeff=0
;;;     also, might count number of zeros in pivot column and
;;;      remember nonzero indices in an array.  if lots of zeros,
;;;      then do it sparsely
;;;   ROW-COL (inner product) and COL-ROW (outer product) matrix multiplies don't exist

(in-package "USER")

(export '(matrix-element-type			; types
	   matrix-array

	   matrix-identity			; functions
	   matrix-copy
	   matrix-transpose
	   matrix-add
	   matrix-sub
	   matrix-multiply
	   matrix-multiply-mat-mat
	   matrix-multiply-row-mat
	   matrix-multiply-mat-col
	   matrix-inverse
	   matrix-solve-triangle-lower
	   matrix-decompose
	   matrix-solve
	   matrix-solve-LUP))

(provide "MATRIX")


;;;; Type Definitions

;;; There is an age old efficiency issue:  generic arrays versus
;;; specialized ones.  We would like to multiply integer arrays and to
;;; invert (complex double-float) arrays, but the cost is probably too
;;; high.

;;; We define some ad-hoc types so one can make these routines more
;;; general with recompilation.

;;; Right now are (simple-array float).  On Suns that causes severe
;;; problems with floating underflows when one manipulates a
;;; single-float array.

(defconstant matrix-element-type-var 'single-float)

(deftype matrix-element-type ()
  matrix-element-type-var)

;;; in the strict sense, I want
;;;   (or (simple-array single-float ...) (simple-array double-float ...))
;;; instead of (simple-array float ...)

;;; lucid cannot hack (simple-array matrix-element-type ..)
;;;
(deftype matrix-array (&optional dims)
  `(simple-array ,matrix-element-type-var ,dims))

(defun make-matrix (&rest dims)
  (make-array dims
	      :element-type    matrix-element-type-var
	      :initial-element 0.0))

;;; In the long run, there should be separate routines for the usual
;;; cases:
;;;
;;;	single-float
;;;	double-float
;;;	(complex single)
;;;	(complex double)


;;; One wonders if the following compilation strategy will work:
;;;   make an inline definition of matrix-multiply using generic arith.
;;;   make the specialized version call it.


;;;; Matrix Printing Routines

(defun matrix-print-vector (a)
  (declare (type (matrix-array   (*)) a))
  (let ((n (array-dimension a 0)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (format t "~%~5,1,10$ " (aref a i)))
    a))

(defun matrix-print-matrix (a)
  (declare (type (matrix-array (* *)) a))
  (let ((n (array-dimension a 0))
	(m (array-dimension a 1)))
    (declare (fixnum m n))
    (dotimes (i n)
      (declare (fixnum i))
      (terpri)
      (dotimes (j m)
	(declare (fixnum j))
	(format t "~5,1,10$ " (aref a i j))))
     (terpri)
     a))

(defun matrix-print (a)
  (declare (type (matrix-array *) a))
  (let ((dims (array-rank a)))
    (declare (fixnum dims))
    (format t "~%; Matrix ~A =~%" a)
    (case dims
      (1 (matrix-print-vector a))
      (2 (matrix-print-matrix a))
      (otherwise
	(error "MATRIX-PRINT given more than two dimensions")))))


;;;; Fundamental Matrix Operations

;;; Generate an NxN identity matrix
;;;
(defun matrix-identity (n &optional matrix)
  (declare (fixnum n)
	   (type (matrix-array (* *)) matrix))

  (when (null matrix)
    (setq matrix (make-matrix n n)))
  (dotimes (i n)				; fill in the diagonal
    (setf (aref matrix i i) 1.0))
  matrix)

;;; Copy an arbitrary matrix
;;;
(defun matrix-copy (from &optional to)
  (declare (type          (matrix-array *)  from)
	   (type (or null (matrix-array *)) to))
  (when (null to)
    (setq to (make-array (array-dimensions from)
			 :element-type (array-element-type from))))
  (case (array-rank from)
    (1 (replace to from))
    (2
      (locally
	(declare (type (matrix-array (* *)) from)
		 (type (matrix-array (* *)) to  ))
	(dotimes (i (array-dimension from 0))
	  (dotimes (j (array-dimension from 1))
	    (setf (aref to i j) (aref from i j))))))
    (otherwise
      (error "Matrix-Copy: bad rank for ~s" from)))
  to)

;;; Copy just the diagonal elements of a square matrix
;;;
(defun matrix-copy-diagonal (from &optional to)
  (declare (type          (matrix-array (* *))  from)
	   (type (or null (matrix-array (* *))) to))
  (let* ((n  (array-dimension from 0)))
    (declare (fixnum n))

    (if (null to)
	(setq to (make-matrix n n)))
    (locally 
      (declare (type (matrix-array (* *)) to))
      (dotimes (i n)
	(setf (aref to   i i) (aref from i i))))
    to))

;;; Swap rows or columns

(proclaim '(inline matrix-swap-rows
		   matrix-swap-cols))

(defun matrix-swap-rows (matrix n i k)
  (declare (type (matrix-array (* *)) matrix)
	   (type fixnum n i k))
  (dotimes (m n)
    (declare (fixnum m))
    (rotatef (aref matrix i m)
	     (aref matrix k m))))

(defun matrix-swap-cols (matrix n j k)
  (declare (type (matrix-array (* *)) matrix)
	   (type fixnum n j k))
  (dotimes (m n)
    (declare (fixnum m))
    (rotatef (aref matrix m j)
	     (aref matrix m k))))


;;;; Matrix Transpose

;;; The matrix TO may be the same as FROM.

(defun matrix-transpose (from &optional to)
  (declare (type          (matrix-array (* *))  from)
	   (type (or null (matrix-array (* *))) to))

  (unless (= (array-rank from) 2)
    (error "Matrix-Transpose:  FROM matrix must have rank 2, from = ~a" from))

  ;; get the matrix size
  (let ((n (array-dimension from 0))
	(m (array-dimension from 1)))
    (declare (fixnum n m))

    ;; make the target if we need it
    (if (null to)
	(setq to (make-matrix m n)))

    (locally
      (declare (type (matrix-array (* *)) to))
      ;; sanity check
      (unless (and (= (array-rank to) 2)
		   (= (array-dimension from 0) (array-dimension to   1))
		   (= (array-dimension from 1) (array-dimension to   0)))
	(error "Matrix-Transpose:  bad TO matrix dimensions ~a" to))

      (if (eq from to)

	  ;; transpose in place
	  ;;   the matrix must be square
	  (do ((i 0 (1+ i)))			; do each row
	      ((>= i n))
	    (declare (fixnum i))
	    (do ((j (1+ i) (1+ j)))		; do upper columns
		((>= j m))
	      (declare (fixnum j))
	      (rotatef (aref from i j) (aref from j i))))

	  ;; transpose to another matrix
	  (dotimes (i n)
	    (dotimes (j m)
	      (setf (aref to j i) (aref from i j))))))

    to))


;;;; Matrix Addition and Subtraction

;;; The matrices must be two dimensional.

;;; The optional result matrix C may be either A or B.

;;; Add 2 Matrices
;;;
(defun matrix-add (a b &optional c)
  (declare (type          (matrix-array (* *))  A B)
	   (type (or null (matrix-array (* *))) C))
  (let* ((n (array-dimension a 0))
	 (m (array-dimension a 1)))
    (declare (fixnum m n))

    (if (null c)
	(setq c (make-matrix n m)))

    (locally 
      (declare (type (matrix-array (* *)) C))
      (unless (and (= n (array-dimension b 0))
		   (= m (array-dimension b 1))
		   (= n (array-dimension c 0))
		   (= m (array-dimension c 1)))
	(error "Matrix-Add:  bad dimensions"))

      (dotimes (i n)
	(declare (fixnum i))
	(dotimes (j m)
	  (declare (fixnum j))
	  (setf (aref c i j)
		(+ (aref a i j)
		   (aref b i j))))))
    c))

;;; Subtract 2 Matrices
;;;
(defun matrix-sub (a b &optional c)
  (declare (type          (matrix-array (* *))  A B)
	   (type (or null (matrix-array (* *))) C))
  (let* ((n (array-dimension a 0))
	 (m (array-dimension a 1)))
    (declare (fixnum m n))

    (if (null c)
	(setq c (make-matrix n m)))

    (locally 
      (declare (type (matrix-array (* *)) C))

      (unless (and (= n (array-dimension b 0))
		   (= m (array-dimension b 1))
		   (= n (array-dimension c 0))
		   (= m (array-dimension c 1)))
	(error "Matrix-Sub:  bad dimensions"))

      (dotimes (i n)
	(declare (fixnum i))
	(dotimes (j m)
	  (declare (fixnum j))
	  (setf (aref c i j)
		(- (aref a i j)
		   (aref b i j))))))
    c))


;;;; Multiply MAT x MAT

;;; MAT * MAT
;;;
(defun matrix-multiply-mat-mat (b c &optional a)
  (declare (type          (matrix-array (* *))  B)
	   (type          (matrix-array (* *))  C)
	   (type (or null (matrix-array   (*))) A))
  (let ((l (array-dimension b 0))		; rows of B
	(m (array-dimension c 0))		; rows of C
	(n (array-dimension c 1)))		; cols of C
    (declare (fixnum l m n))
    
    (if (null a)
	(setq a (make-matrix l n)))

    (unless (and (= l (array-dimension a 0))
		 (= n (array-dimension a 1))
		 (= m (array-dimension b 1)))
	(error "Matrix-Multiply-Mat-Mat: given bad dimensions"))
    
    (dotimes (i l)
      (dotimes (j n)

	(let ((sum 0.0))
	  (declare (type matrix-element-type sum))

	  (dotimes (k m)
	    (setq sum (+ sum (* (aref b i k)
				(aref c k j)))))

	  (setf (aref a i j) sum))))

    a))


;;;; Multiply ROW x MAT and MAT x COL

;;; ROW * MAT
;;; 
(defun matrix-multiply-row-mat (b c &optional a)
  (declare (type          (matrix-array   (*))  B)
	   (type          (matrix-array (* *))  C)
	   (type (or null (matrix-array   (*))) A))
  (let ((n (array-dimension c 1))
	(m (array-dimension b 0)))
    (declare (fixnum m n))

    (if (null a)
	(setq a (make-matrix n)))

    (locally
      (declare (type (matrix-array   (*)) A))

      (unless (and (= n (array-dimension a 0))
		   (= m (array-dimension c 0)))
	(error "Matrix-Multiply-Row-Mat: given bad dimensions"))

      (dotimes (i n)
	(let ((sum 0.0))
	  (declare (type matrix-element-type sum))

	  (dotimes (j m)
	    (setq sum (+ sum (* (aref b j)
				(aref c j i)))))
      
	  (setf (aref a i) sum))))

    a))

;;; MAT * COL
;;;
(defun matrix-multiply-mat-col (b c &optional a)
  (declare (type          (matrix-array (* *))  B)
	   (type          (matrix-array   (*))  C)
	   (type (or null (matrix-array   (*))) A))
  (let ((n (array-dimension b 0))
	(m (array-dimension c 0)))
    (declare (fixnum m n))

    (if (null a)
	(setq a (make-matrix n)))

    (locally
      (declare (type (matrix-array   (*)) A))

      (unless (and (= n (array-dimension a 0))
		   (= m (array-dimension b 1)))
	(error "Matrix-Multiply-Mat-Col: given bad dimensions"))

      (dotimes (i n)
	(let ((sum 0.0))
	  (declare (type matrix-element-type sum))

	  (dotimes (j m)
	    (setq sum (+ sum (* (aref b i j)
				(aref c j)))))
	
	  (setf (aref a i) sum))))

    a))


;;;; Multiply Matrix by a Scalar

;;; B is a scalar and C is a scalar, vector, or matrix

;;; The matrix A may be the same as C.

(defun matrix-multiply-num-mat (b c &optional a)
  (declare (type matrix-element-type b)
	   (type (or matrix-element-type
		     (matrix-array *)) c)
	   (type (or null
		     (matrix-array *)) a))

  (cond ((numberp c)
	 (locally
	   (declare (type matrix-element-type c))
	   (* b c)))			; SCALAR * SCALAR

	((= (array-rank c) 1)			; SCALAR * VECTOR
	 (if (null a)
	     (setq a (make-matrix (array-dimension c 0))))
	 (unless (= (array-dimension c 0) (array-dimension a 0))
	   (error "Matrix-Multiply-Num-Mat:  bad dimensions"))
	 (locally
	   (declare (type (matrix-array (*)) c a))
	   (dotimes (i (array-dimension c 0) a)
	     (setf (aref a i)
		   (* b (aref c i))))))

	((= (array-rank c) 2)			; SCALAR * MATRIX
	 (if (null a)
	     (setq a (make-matrix (array-dimension c 0) (array-dimension c 1))))
	 (unless (and (= (array-dimension c 0) (array-dimension a 0))
		      (= (array-dimension c 1) (array-dimension a 1)))
	   (error "Matrix-Multiply-Num-Mat:  bad dimensions"))
	 (locally
	   (declare (type (matrix-array (* *)) c a))
	   (dotimes (i (array-dimension c 0) a)
	     (dotimes (j (array-dimension c 0))
	       (setf (aref a i j)
		     (* b (aref c i j)))))))

	(t
	 (error "Matrix-Multiply-Num-Mat:  not given vector or matrix"))))
  

;;;; Matrix Multiply

;;; There are several varieties of matrix multiply:
;;;
;;; scalar x matrix --> matrix		MATRIX-MULTIPLY-NUM-MAT
;;; matrix x matrix --> matrix		MATRIX-MULTIPLY-MAT-MAT
;;; row    x column --> scalar		MATRIX-MULTIPLY-ROW-COL
;;; column x row    --> matrix		MATRIX-MULTIPLY-COL-ROW
;;; row    x matrix --> row		MATRIX-MULTIPLY-ROW-MAT
;;; matrix x column --> column		MATRIX-MULTIPLY-MAT-COL
;;;
;;; so try to use a reasonable one based on the argument types.
;;;
;;; For two vectors, we don't know if the caller wants the inner or the
;;; outer product, so just err.

(defun matrix-multiply (b c &optional a)
  (declare (type (or matrix-element-type
		     (matrix-array *)) b)
	   (type (or matrix-element-type
		     (matrix-array *)) c)
	   (type (or null
		     (matrix-array *)) a))

  (cond ((numberp b)
	 (matrix-multiply-num-mat b c a))

	((numberp c)
	 (matrix-multiply-num-mat c b a))

	((and (= 2 (array-rank b))
	      (= 2 (array-rank c)))
	 (matrix-multiply-mat-mat b c a))

	((and (= 1 (array-rank b))
	      (= 2 (array-rank c)))
	 (matrix-multiply-row-mat b c a))

	((and (= 2 (array-rank b))
	      (= 1 (array-rank c)))
	 (matrix-multiply-mat-col b c a))

	(t
	 (error "Matrix-Multiply: bad arguments"))
	))


;;;; Generate Random Vectors and Matrices

#|

(defun matrix-random-vector (n)
  (declare (fixnum n))
  (let ((v (make-matrix n)))
    (declare (type (array matrix-element-type (*)) v))
    (dotimes (i n)
      (setf (aref v i) (random 1.0)))
    v))

(defun matrix-random-matrix (n &optional (m n))
  (declare (fixnum n m))
  (let ((A (make-matrix n m)))
    (declare (type (array matrix-element-type (* *)) A))
    (dotimes (i n)
      (dotimes (j m)
	(setf (aref A i j) (random 1.0))))
    A))

(defun matrix-random-lower (n &optional (m n))
  (declare (fixnum n m))
  (let ((A (make-matrix n m)))
    (declare (type (array matrix-element-type (* *)) A))
    (dotimes (i n)
      (do ((j 0 (1+ j)))
	  ((> j i))
	(declare (fixnum j))
	(setf (aref A i j) (random 1.0))))
    A))

(defun matrix-random-upper (n &optional (m n))
  (declare (fixnum n m))
  (let ((A (make-matrix n m)))
    (declare (type (array matrix-element-type (* *)) A))
    (dotimes (i n)
      (do ((j i (1+ j)))
	  ((>= j m))
	(declare (fixnum j))
	(setf (aref A i j) (random 1.0))))
    A))

|#


;;;; Solving Lower Triangular Systems

;;; These operations are order n**2

;;; Presume all diagonal elements are nonzero (determinant .ne. 0)
;;; There is a flag to treat the main diagonal as ones.
;;; That lets us put an LU decomposition into one matrix

;;; solve Ax=b, put result in b
;;;
(defun matrix-solve-triangle-lower (A b &optional (MD-unity NIL))
  (declare (type (matrix-array (* *)) A)
	   (type (matrix-array   (*)) b)
	   (type (member NIL T) MD-unity))
  (let ((n (array-dimension A 0)))
    (declare (fixnum n))

    (dotimes (i n)				; do for each row
      (declare (fixnum i))
      
      (if (not MD-unity)			; main diagonal not unity?
	  (setf (aref b i)			; solution for xi
		(/ (aref b i)
		   (aref A i i))))
      
      (do ((j (1+ i) (1+ j)))			; do for all the x[i] we know
	  ((>= j n))
	(declare (fixnum j))
	(setf (aref b j)
	      (- (aref b j)
		 (* (aref A j i)
		    (aref b i))))))
    b))

#+ignore
(defun matrix-solve-lower-test (n)
  (declare (fixnum n))
  (let ((L (matrix-random-lower  n))
	(x (make-matrix n))
	(b (matrix-random-vector n)))
    (declare (type (array matrix-element-type (* *)) L)
	     (type (array matrix-element-type   (*)) x b))
    (matrix-print L)
    (matrix-print b)
    (replace x b)
    (matrix-solve-triangle-lower L x)
    (matrix-print x)
    (matrix-print (matrix-multiply L x))))


;;;; Solving Upper Triangular Systems

;;; These operations are order n**2

;;; Presume all diagonal elements are nonzero (determinant .ne. 0)
;;; There is a flag to treat the main diagonal as ones.
;;; That lets us put an LU decomposition into one matrix

;;; solve Ax=b, put result in b
;;;
(defun matrix-solve-triangle-upper (A b &optional (MD-unity NIL))
  (declare (type (matrix-array (* *)) A)
	   (type (matrix-array   (*)) b)
	   (type (member NIL T) MD-unity))
  (let ((n (array-dimension A 0)))
    (declare (fixnum n))

    (do ((i (1- n) (1- i)))			; do for each row
	((< i 0))
      (declare (fixnum i))
      
      (if (not MD-unity)			; main diagonal not unity?
	  (setf (aref b i)			; solution for xi
		(/ (aref b i)
		   (aref A i i))))
      
      (do ((j (1- i) (1- j)))			; do for all the x[i] we know
	  ((< j 0))
	(declare (fixnum j))
	(setf (aref b j)
	      (- (aref b j)
		 (* (aref A j i)
		    (aref b i))))))
    b))

#+ignore
(defun matrix-solve-upper-test (n)
  (declare (fixnum n))
  (let ((U (matrix-random-upper  n))
	(x (make-matrix n))
	(b (matrix-random-vector n)))
    (declare (type (array matrix-element-type (* *)) U)
	     (type (array matrix-element-type   (*)) x b))
    (matrix-print U)
    (matrix-print b)
    (replace x b)
    (matrix-solve-triangle-upper U x)
    (matrix-print x)
    (matrix-print (matrix-multiply U x))))
  

;;;; Matrix Inversion

;;; let A be the matrix we are interested in
;;;    A = A
;;; premultiply both sides by a matrix M1 that makes column 1 look like I
;;;    (M1 A) = M1 A
;;; now use M2 to fix up column 2, etc
;;;    (Mn ... M3 M2 M1 A) = (Mn ... M3 M2 M1) A
;;; we turned the lhs into I, so (Mn ... M3 M2 M1) must be A'

;;; each Mi looks like                and M(i-1) A
;;;     [ 1 ... 0   ... 0   0   0]    [ 1 ... a0i ... *   *   *]
;;;     [ 0 ... mii ... 0   0   0]    [ 0 ... aii ... *   *   *]
;;;     [ 0 ... m** ... 1   0   0]    [ 0 ... a*i ... *   *   *]
;;;     [ 0 ... m** ... 0   1   0]    [ 0 ... a*i ... *   *   *]
;;;     [ 0 ... m** ... 0   0   1]    [ 0 ... a*i ... *   *   *]
;;;
;;; where the element mji = 0         if i<j
;;;                       =    1/aii  if i=j
;;;                       = -aji/aii  if i>j
;;;
;;; The matrix inversion algorithm must compute both (M A) and M.
;;; We can share the space (put M and (M A) in the same matrix)
;;; (M A) only needs the lower right corner; the rest are 0, 1, and m**
;;; M needs the other 3 corners
;;;  so what we have to do is
;;;    for i = 1 to n
;;;      make the mji for j >= i
;;;      perform the multiplication by Mi as
;;;          mkj = mki mij + mkj    for k#i, j#i
;;;                                  (row i of Mi has the mij we are multiplying by)
;;;                                  (col i of Mi has the mki we are multiplying by)
;;;          mij = mij*mii          for i#j
;;;                                  (row i of Mi which we skipped above)
;;;                                  (mii has the value we are multiplying by)


;;;; LUP Decomposition

;;; LU Decomposition is pretty much the same as inversion except we let
;;; mii = 1 instead of 1/aii

;;; Luenberger
;;;   Appendix C, page 340.

;;; Each gaussian elimination step is finding a matrix M_i
;;;   M_i A x = M_i b
;;; let M be M_n * M_n-1 *...* M_1
;;;    M A x = M b
;;;    M is lower triangular
;;;    M A is upper triangular, call it U
;;;    U x = M b
;;;    M**-1 U x = b
;;;    M**-1 is lower triangular


;;; throw in permutation:

;;;  Mn ... M3 M2 M1 A P1 P2 P3 ... Pn = Mn ... M3 M2 M1 A P1 P2 P3 ... Pn 

;;; where the Pi swap columns
;;;   L and M are as before but U includes the Pi
;;;
;;;      U P1 P2 P3 ... Pn = M A P1 P2 P3 ... Pn
;;;    L U                 =   A P1 P2 P3 ... Pn
;;;    L U                 =   A P
;;;    L U P'              =   A
;;;        A  x = b
;;;    L U P' x = b
;;;       solve  LUP'x=b
;;;        by solving  Lz=b     (z=UP'x)
;;;           solving  Uy=z     (y= P'x)
;;;           solving  P'x=y    (ie, x = P y)


;;;; LUP Decomposition

;;; It is possible to stuff both L and U into same array by assuming
;;; main diagonal of L is ones.  Just call the procedure with L = A.

(defun matrix-decompose (A L P)
  (declare (type (matrix-array  (* *)) A L)
	   (type (simple-array fixnum   (*)) P))
  (let* ((n  (array-dimension A 0)))
    (declare (fixnum n))
    
    (dotimes (i n)				; initial permutation
      (setf (aref P i) i))

    (do ((i 0 (1+ i)))
	((>= i n))
      (declare (fixnum i))
      
      ;; partial pivot -- swap columns!
      (let ((maxv (abs (aref A i i)))
	    (maxk i))
	(declare (type matrix-element-type  maxv)
		 (type fixnum maxk))
	(do ((k i (1+ k)))
	    ((>= k n))
	  (declare (fixnum k))
	  (when (> (abs (aref A i k)) maxv)
	    (setq maxv (abs (aref A i k))
		  maxk k)))
	(if (= maxv 0.0)			; but there might still be a decomposition
	    (error "Matrix-Decompose:  Singular Matrix"))
	(when (/= maxk i)
	  (rotatef (aref P i) (aref P maxk))
	  (matrix-swap-cols A n i maxk)))

      ;; "compute" Mi in our heads -- it's the identity plus the (-$ mki) terms
      
      ;; now set A to Mi * A
      ;;   because Mi is an identity in the upper left and lower right
      ;;   do for each row beyond i
      ;;     do for every element
      
      (do ((k   (1+ i) (1+ k))			; Mi is the identity for first i rows
	   (mki 0.0))
	  ((>= k n))
	(declare (fixnum k) (type matrix-element-type mki))
	(setq mki (/ (aref A k i)
		     (aref A i i)))
	
	(do ((j  i (1+ j)))			; calculate all the columns
	    ((>= j n))
	  (declare (fixnum j))
	  (setf (aref A k j)
		(+ (aref A k j)			; main diagonal of Mi has a 1
		   (* (- mki)			; col i has only other nonzero element
		      (aref A i j)))))
	
	(setf (aref L  k i) mki )		; clever L calculation -- see Luenberger
						; SETF after DO so L can be eq A
	))
    L))

#+ignore
(let* ((A  #2a((1.0 0.0 3.0)
	       (0.0 1.0 0.0)
	       (0.0 0.0 1.0)))
       (n  (array-dimension A 0))
       (LU (matrix-copy A))
       (P  (make-array n :element-type 'fixnum)))
  (matrix-decompose LU LU P)

  (let ((L (matrix-copy LU))
	(U (matrix-copy LU)))
    (dotimes (i n)
      (dotimes (j n)
	(cond ((< i j)				; upper
	       (setf (aref L i j) 0.0))
	      ((= i j)
	       (setf (aref L i j) 1.0))
	      (t				; lower
	       (setf (aref U i j) 0.0)))))

    (print 'A)
    (matrix-print A )
    (matrix-print LU)
    (print 'L)
    (matrix-print L)
    (print 'U)
    (matrix-print U)
    (print (list 'P (coerce P 'list)))
    (matrix-print (matrix-multiply L U))
    ))


;;;; Solving Matrix Problems with LUP Decomposition

;;; Solve the system Ax=b
;;; Factor A into LUP
;;;   then LUPx=b which can be associated as L(U(Px))=b
;;;     solve Ly=b 
;;;     solve Uz=y
;;;     solve Px=z

;;; Undo the permutation.
;;;   P[i] = k   means b'[k] = b[i]
;;;
;;; Recurse down the array so we don't need another temporary array.
;;; Alternatively, P could have been an array of matrix-element-types ....
;;;
(defun matrix-perm-bang (P b)
  (declare (type (simple-array fixnum (*)) P)
	   (type (matrix-array  (*)) b))
  (let ((n (length P)))
    (declare (type fixnum n))
    (labels ((perm (i)
	       (declare (type fixnum i))
	       (when (< i n)
		 (let ((v (aref b i)))		; store values on way down
		   (perm (1+ i))
		   (setf (aref b (aref P i)) v)	; write on way back up
		   NIL))))
      (perm 0))))

;;; This allows arbitrary main diagonals for L and U
;;;
(defun matrix-solve-LUP (L U P b)		; solve LUPx=b, result in b
  (declare (type (matrix-array  (* *)) L U)
	   (type (simple-array fixnum   (*)) P)
	   (type (matrix-array    (*)) b))
  (matrix-solve-triangle-lower L b)		; solve   Ly=b, result in b
  (matrix-solve-triangle-upper U b)		; solve   Ux=y, result in b
  (matrix-perm-bang            P b)
  b)

;;; if we are just solving, don't compute L,
;;;     just solve Ly=b directly for y

(defun matrix-solve (A P b)			; don't need to create L
  (declare (type (matrix-array  (* *)) A)
	   (type (simple-array fixnum   (*)) P)
	   (type (matrix-array    (*)) b))
  (matrix-decompose A A P)			; stuff L and U into A
  (matrix-solve-triangle-lower A b T  )		; solve  Ly=b, put result in b
  (matrix-solve-triangle-upper A b NIL)		; solve  Ux=y, put result in b
  (matrix-perm-bang            P b    )
  b)

#+ignore
(defun matrix-solve-test (n)
  (declare (fixnum n))
  (let* ((A (matrix-random-matrix n))
	 (P (make-array n :element-type 'fixnum))
	 (x (matrix-random-vector n))
	 (b (matrix-random-vector n)))
    (matrix-multiply A x b)
    (matrix-solve A P b)
    (matrix-print x)
    (matrix-print b)
    (print (coerce P 'list))))


;;;; Matrix-Inversion:  Matrix Process

;;; Recursively chomp on a submatrix.

(defun matrix-process (A n k pivot)
  (declare (type (matrix-array  (* *)) A)
	   (type fixnum n k)
	   (type matrix-element-type pivot))
  
  (dotimes (i n)				; divide column by minus pivot
    (declare (fixnum i))
    (setf (aref a k i)
	  (/ (aref a k i) (- pivot))))
  (setf (aref a k k) pivot)			; got smashed (don't need this statement)
  
  (dotimes (i n)				; reduce matrix
    (declare (fixnum i))
    (if (not (= i k))
	(do ((temp (aref a k i))
	     (j    0 (1+ j)))
	    ((>= j n))
	  (declare (fixnum j) (type matrix-element-type temp))
	  (if (not (= j k))
	      (setf (aref a j i)
		    (+ (* temp (aref a j k))
		       (aref a j i)))))))
  
  (dotimes (j n)				; divide row by pivot
    (declare (fixnum j))
    (setf (aref a j k)
	  (/ (aref a j k) pivot)))
  
  ;; replace pivot by reciprocal
  (setf (aref a k k) (/ 1.0 pivot))
  
  nil)


;;;; Matrix Inversion

;;; In place matrix inversion.  Returns the determinant.

;;; let A be the matrix to invert
;;; let R be a permutation matrix that swaps rows    i and k
;;; let C be a permutation matrix that swaps columns j and k
;;; the (C A R) is the matrix with the rows and columns interchanged
;;; I = (CAR) * (CAR)**-1
;;; I = C A R   (CAR)**-1
;;; R**-1 A**-1 C**-1 = (CAR)**-1        premultiply both sides
;;; A**-1 = R (CAR)**-1 C                pre and post multiply
;;;  (but notice that swapping columns and rows roles have reversed)
;;;  two swaps do not change the sign of the determinant

(proclaim '(ftype (function ((matrix-array  (* *)) fixnum fixnum) matrix-element-type)
		  matrix-subr))

#+lucid
(proclaim '(restrictive-ftype
	     (function ((matrix-array  (* *)) fixnum fixnum) matrix-element-type)
	     matrix-subr))

(defun matrix-subr (matrix n k)
  (declare (type (matrix-array  (* *)) matrix)
	   (type fixnum n k))
  (if (>= k n)
      1.0					; we are done
      (let* ((i k)				; find largest A[i,j]
	     (j k)				;   in lower right corner
	     (max (abs (aref matrix i j))))
	(declare (fixnum i j) (type matrix-element-type max))
	
	(do ((i0 k (1+ i0)))			; find the max
	    ((>= i0 n))
	  (declare (fixnum i0))
	  (do ((j0 k (1+ j0)))
	      ((>= j0 n))
	    (declare (fixnum j0))
	    (if (> (abs (aref matrix i0 j0)) max)
		(setq i i0 j j0 max (abs (aref matrix i0 j0))))))
	
	(let ((pivot (aref matrix i j))
	      (d     0.0))
	  (declare (type matrix-element-type pivot d))
	  (if (= pivot 0.0)
	      0.0				; singular matrix!
	      (progn
		(matrix-swap-rows matrix n i k)	; put pivot in right place
		(matrix-swap-cols matrix n j k)	; matrix = C A R
		
		(matrix-process matrix n k pivot)	; invert(C A R)
		;; determinant is the recursive product of pivots
		(setq d (* pivot (matrix-subr matrix n (1+ k))))
		
		(matrix-swap-rows matrix n j k)	; undo permutation
		(matrix-swap-cols matrix n i k)	; matrix = R (CAR)**-1 C
		
		d))
	  ))))

(defun matrix-inverse (matrix)
  (declare (type (matrix-array  (* *)) matrix))
  (matrix-subr matrix (array-dimension matrix 0) 0))

#+ignore
(defun matrix-invert-random-matrix (n)
  (declare (fixnum n))
  (let* ((A (matrix-random-matrix n))
	 (B (matrix-copy A)))
    (declare (type (array matrix-element-type (* *)) A B))
    (matrix-print A)
    (print (list 'det (matrix-inverse B)))
    (matrix-print B)
    (matrix-print (matrix-multiply-mat-mat A B))))

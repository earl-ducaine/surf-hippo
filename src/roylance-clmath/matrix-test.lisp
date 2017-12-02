;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

(in-package 'user)

(defun matrix-random-matrix (n &optional (m n))
  (declare (fixnum n m))
  (let ((A (user::make-matrix n m)))
    (declare (type (array matrix-element-type (* *)) A))
    (dotimes (i n)
      (dotimes (j m)
	(setf (aref A i j) (random 1.0))))
    A))

(defun matrix-random-matrix-double (n &optional (m n))
  (declare (fixnum n m))
  (let ((A (user::make-matrix-double n m)))
    (declare (type (array matrix-element-type-double (* *)) A))
    (dotimes (i n)
      (dotimes (j m)
	(setf (aref A i j) (random 1.0d0))))
    A))


(deftype matrix-array-double (&optional dims)
  `(simple-array double-float ,dims))

(defvar A-matrix (make-array '(6 5) :element-type 'double-float))
(defvar P-vector (make-array '(6) :element-type 'fixnum))
(defvar B-vector (make-array '(6) :element-type 'double-float))
;(matrix-solve-double a-matrix p-vector b-vector)
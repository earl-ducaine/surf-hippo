#|

From: marcoxa@mosaic.nyu.edu (Marco Antoniotti)
Newsgroups: comp.lang.lisp
Subject: Re: Serious bug in complex arithmetic, CMULISP 17f
Date: 25 Aug 1995 12:32:37 GMT
Organization: Courant Institute of Mathematical Science, NYU
NNTP-Posting-Host: mosaic.cs.nyu.edu
In-reply-to: Richard Hodges's message of 24 Aug 1995 23:34:22 GMT


A fix to complex numbers handling in CMUCL 17f was posted by Rob
MacLachlan some time ago. Credit should also go to Takafumi Hayashi
for having spotted the problem and proposed a fix.


-- 
Marco G. Antoniotti - Resistente Umano
-------------------------------------------------------------------------------
Robotics Lab		| room: 1220 - tel. #: (212) 998 3370
Courant Institute NYU	| e-mail: marcoxa@cs.nyu.edu
			| WWW:    http://found.cs.nyu.edu/marcoxa

...e` la semplicita` che e` difficile a farsi.
...it is simplicity that is difficult to make.
				Bertholdt Brecht



Fix complex - real.

diff -c -r1.25 numbers.lisp
*** 1.25	1994/10/31 04:11:27
--- numbers.lisp	1995/02/13 19:41:16
***************
*** 353,361 ****
  	(canonical-complex (,op (realpart x) (realpart y))
  			   (,op (imagpart x) (imagpart y))))
         (((foreach bignum fixnum ratio single-float double-float) complex)
! 	(complex (,op x (realpart y)) (imagpart y)))
         ((complex (or rational float))
! 	(complex (,op (realpart x) y) (imagpart x)))
         
         (((foreach fixnum bignum) ratio)
  	(let* ((dy (denominator y))
--- 353,361 ----
  	(canonical-complex (,op (realpart x) (realpart y))
  			   (,op (imagpart x) (imagpart y))))
         (((foreach bignum fixnum ratio single-float double-float) complex)
! 	(complex (,op x (realpart y)) (,op (imagpart y))))
         ((complex (or rational float))
! 	(complex (,op (realpart x) y) (,op (imagpart x))))
         
         (((foreach fixnum bignum) ratio)
  	(let* ((dy (denominator y))

|#

(in-package "KERNEL")

;;;; Binary operation dispatching utilities:

(eval-when (compile eval)

;;; FLOAT-CONTAGION  --  Internal
;;;
;;;    Return NUMBER-DISPATCH forms for rational X float.
;;;
(defun float-contagion (op x y &optional (rat-types '(fixnum bignum ratio)))
  `(((single-float single-float) (,op ,x ,y))
    (((foreach ,@rat-types) (foreach single-float double-float))
     (,op (coerce ,x '(dispatch-type ,y)) ,y))
    (((foreach single-float double-float) (foreach ,@rat-types))
     (,op ,x (coerce ,y '(dispatch-type ,x))))
    (((foreach single-float double-float) double-float)
     (,op (coerce ,x 'double-float) ,y))
    ((double-float single-float)
     (,op ,x (coerce ,y 'double-float)))))


;;; BIGNUM-CROSS-FIXNUM  --  Internal
;;;
;;;    Return NUMBER-DISPATCH forms for bignum X fixnum.
;;;
(defun bignum-cross-fixnum (fix-op big-op)
  `(((fixnum fixnum) (,fix-op x y))
    ((fixnum bignum)
     (,big-op (make-small-bignum x) y))
    ((bignum fixnum)
     (,big-op x (make-small-bignum y)))
    ((bignum bignum)
     (,big-op x y))))

); Eval-When (Compile Eval)


(eval-when (compile)

(defmacro two-arg-+/- (name op big-op)
  `(defun ,name (x y)
     (number-dispatch ((x number) (y number))
       (bignum-cross-fixnum ,op ,big-op)
       (float-contagion ,op x y)
       
       ((complex complex)
	(canonical-complex (,op (realpart x) (realpart y))
			   (,op (imagpart x) (imagpart y))))
;; LG fix 19.08.2016
;;       (((foreach bignum fixnum ratio single-float double-float) complex)
       (((foreach bignum fixnum ratio single-float double-float double-double-float) complex)
	(complex (,op x (realpart y)) (,op (imagpart y))))
       ((complex (or rational float))
	(complex (,op (realpart x) y) (,op (imagpart x))))
       
       (((foreach fixnum bignum) ratio)
	(let* ((dy (denominator y))

	       (n (,op (* x dy) (numerator y))))
	  (%make-ratio n dy)))
       ((ratio integer)
	(let* ((dx (denominator x))
	       (n (,op (numerator x) (* y dx))))
	  (%make-ratio n dx)))
       ((ratio ratio)
	(let* ((nx (numerator x))
	       (dx (denominator x))
	       (ny (numerator y))
	       (dy (denominator y))
	       (g1 (gcd dx dy)))
	  (if (eql g1 1)
	      (%make-ratio (,op (* nx dy) (* dx ny)) (* dx dy))
	      (let* ((t1 (,op (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
		     (g2 (gcd t1 g1))
		     (t2 (truncate dx g1)))
		(cond ((eql t1 0) 0)
		      ((eql g2 1)
		       (%make-ratio t1 (* t2 dy)))
		      (T (let* ((nn (truncate t1 g2))
				(t3 (truncate dy g2))
				(nd (if (eql t2 1) t3 (* t2 t3))))
			   (if (eql nd 1) nn (%make-ratio nn nd))))))))))))
  
)

(two-arg-+/- two-arg-+ + add-bignums)
(two-arg-+/- two-arg-- - subtract-bignum)

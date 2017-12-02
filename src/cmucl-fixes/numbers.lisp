(in-package "KERNEL")

;; Not sure that this is worth anything....



(proclaim '(inline sf-sb32-truncate))
(defun sf-sb32-truncate (number &optional (divisor 1))
  "Returns number (or number/divisor) as an integer, rounded toward 0.
  The second returned value is the remainder."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (single-float number))
  (macrolet ((truncate-float (rtype)
	       `(let* ((float-div (coerce divisor ',rtype))
		       (res (the (SIGNED-BYTE 32) (%unary-truncate (/ number float-div)))))
		  (values res
			  (- number
			     (* (coerce res ',rtype) float-div))))))
    (number-dispatch ((number real) (divisor real))
      ((fixnum fixnum) (truncate number divisor))
      (((foreach fixnum bignum) ratio)
       (truncate (* number (denominator divisor))
		 (numerator divisor)))
      ((fixnum bignum)
       (values 0 number))
      ((ratio (or float rational))
       (let ((q (truncate (numerator number)
			  (* (denominator number) divisor))))
	 (values q (- number (* q divisor)))))
      ((bignum fixnum)
       (bignum-truncate number (make-small-bignum divisor)))
      ((bignum bignum)
       (bignum-truncate number divisor))
      
      (((foreach single-float double-float) (or rational single-float))
       (if (eql divisor 1)
	   (let ((res (the (SIGNED-BYTE 32) (%unary-truncate number))))
	     (values res (- number (coerce res '(dispatch-type number)))))
	   (truncate-float (dispatch-type number))))
      ((double-float (or single-float double-float))
       (truncate-float double-float))
      ((single-float double-float)
       (truncate-float double-float))
      (((foreach fixnum bignum ratio) (foreach single-float double-float))
       (truncate-float (dispatch-type divisor))))))

(export '(sf-sb32-truncate))


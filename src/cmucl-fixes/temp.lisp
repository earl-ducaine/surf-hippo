(in-package "KERNEL")

#|
(macrolet
    ((frob ()
       `(progn
	  ,@(mapcar #'(lambda (pred)
			`(defun ,pred (object)
			   ,(format nil
				    "Return T if OBJECT is a~:[~;n~] ~(~A~) ~
				     and NIL otherwise."
				    (find (schar (string pred) 0) "AEIOUaeiou")
				    (string pred))
			   (,pred object)))
		    primitive-predicates))))
  (frob))
|#

(macrolet ((frob (name &rest args)
		 `(defun ,name ,args
		    (,name ,@args))))
  (frob 32bit-logical-not x)
  (frob 32bit-logical-and x y)
  (frob 32bit-logical-or x y)
  (frob 32bit-logical-xor x y)
  (frob 32bit-logical-nor x y)
  (frob 32bit-logical-eqv x y)
  (frob 32bit-logical-nand x y)
  (frob 32bit-logical-andc1 x y)
  (frob 32bit-logical-andc2 x y)
  (frob 32bit-logical-orc1 x y)
  (frob 32bit-logical-orc2 x y))


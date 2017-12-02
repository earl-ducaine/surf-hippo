


;; Reparse Douglas 3d format file so that each field is right justified - necessary because of
;; primitive input structure of ntscable. IN-FILE should only have the coordinate lines, in other
;; words create a file that strips off all header info, including  the "3d" line. The output of this
;; function should then have the header info restored before processing by ntscable. This also
;; eliminates lines which start with "join", since ntscable doesn't like these. Also, type "ne" is
;; converted to "mae" and "tto" is converted to "mto". Eliminates 0 diameter lines.
(defun fix-rd3-file (in-file &optional (out-file "/tmp/devnull") debug)
  (with-open-stream
   (in-stream (open in-file :direction :input))
   (with-open-stream
    (out-stream (open out-file :direction :output :if-exists :supersede))
    (let (x y z d type (eof (gensym)))
      (loop while t
  
	    for count from 1
	    do
	    (setq type (read in-stream nil eof)
		  x (read in-stream nil eof)
		  y (read in-stream nil eof)
		  z (read in-stream nil eof)
		  d (read in-stream nil eof))
	    when (or (eq x eof)
		     (eq y eof)
		     (eq z eof)
		     (eq d eof)
		     (eq type eof))
	    do (return)
	    do
	    (when debug (format t "Line ~A, type ~A: x ~A, y ~A, z ~A, d ~A~%" count type x y z d))
	    (when (<= d 0)
	      (format t "Line ~A: d is ~A~%" count d))
	    (when (and (numberp x)
		       (numberp y)
		       (numberp z)
		       (numberp d)
		       type
		       (not (eq type 'join))
		       (not (= d 0))
		       )

	      (when debug (format t "writing to output..~%"))
	      (format out-stream "~5@<~(~s~)~>~11<~D~>~11<~D~>~11<~D~>~11<~D~>~%"
		      (case type
			(ne 'mae)
			(tto 'mto)
			(t type))
		      (round (* 10 x))	; x
		      (round (* 10 y))	; y
		      (round (* 10 z))	; z
		      (round (* 10 d))	; d
		      )))))))

(defun check-nts-file (in-filename)
  (with-open-stream
   (in-stream (open in-filename :direction :input))
   (let (this-x this-y this-z this-d
		that-x that-y that-z this-type)
     (loop for count from 1 do
	   (setq this-type  (read in-stream)
		 this-x (read in-stream)
		 this-y (read in-stream)
		 this-z (read in-stream)
		 this-d (read in-stream))
	   (when (and that-x
		      (= that-x this-x)
		      (= that-y this-y)
		      (= that-z this-z))
	     (format t "Line ~D duplicates previous line (~A, ~A ~A ~A, ~A)~%"
		     count
		     this-type
		     this-x
		     this-y
		     this-z
		     this-d))
	    
	   (setq that-x this-x
		 that-y this-y
		 that-z this-z))

     )))
		    


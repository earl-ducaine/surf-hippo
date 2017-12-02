(in-package "LISP")

;
;This may be the same problem we been having with 17c. I belive that it
;is the function fd-stream-read-n-bytes thats having problems with
;large reads. Were just got it working by compiling the old (16f)
;fd-stream-read-n-bytes. I have not had time to look at the code and
;try to understand that is wrong yet. Put hopefully someone of the
;wizards (William?) can fix this in a more correct way.
;
;A simple way to generate the error is to run
;
;(xlib:list-font-names (xlib:open-display "hostname") "*")
;
;This fails for me on all X servers i have access to.
;
;Here is the old 16c code so that you can test. Just compile and load
;this in the lisp package.

;David Axmark
;EMAIL:  davida@isil.detron.se
;MAIL:   Detron HB, Petterslundsg 11 A, 753 28, UPPSALA, SWEDEN
;PHONE:  + (46) 18 - 11 07 80



(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type stream stream) (type index start requested))
  (let* ((sap (fd-stream-ibuf-sap stream))
	 (elsize (fd-stream-element-size stream))
	 (offset (* elsize start))
	 (requested-bytes (* elsize requested))
	 (head (fd-stream-ibuf-head stream))
	 (tail (fd-stream-ibuf-tail stream))
	 (available (- tail head))
	 (copy (min requested-bytes available)))
    (declare (type index elsize offset requested-bytes head tail available
		   copy))
    (unless (zerop copy)
      (if (typep buffer 'system-area-pointer)
	  (system-area-copy sap (* head vm:byte-bits)
			    buffer (* offset vm:byte-bits)
			    (* copy vm:byte-bits))
	  (copy-from-system-area sap (* head vm:byte-bits)
				 buffer (+ (* offset vm:byte-bits)
					   (* vm:vector-data-offset
					      vm:word-bits))
				 (* copy vm:byte-bits)))
      (incf (fd-stream-ibuf-head stream) copy))
    (cond
      ((> requested-bytes available)
       (setf (fd-stream-ibuf-head stream) 0)
       (setf (fd-stream-ibuf-tail stream) 0)
       (setf (fd-stream-listen stream) nil)
       (let ((now-needed (- requested-bytes copy))
	     (len (fd-stream-ibuf-length stream)))
	 (declare (type index now-needed len))
	 (cond
	   ((> now-needed len)
	    (system:without-gcing
	     (loop
	       (multiple-value-bind
		     (count err)
		   (unix:unix-read (fd-stream-fd stream)
				   (sap+ (if (typep buffer 'system-area-pointer)
					     buffer
					     (vector-sap buffer))
					 (+ offset copy))
				   now-needed)
		 (declare (type (or index null) count))
		 (unless count
		   (error "Error reading ~S: ~A" stream
			  (unix:get-unix-error-msg err)))
		 (when (zerop count)
		   (if eof-error-p
		       (error "Unexpected eof on ~S." stream)
		       (return (- requested (truncate now-needed elsize)))))
		 (decf now-needed count)
		 (when (zerop now-needed) (return requested))
		 (incf offset count)))))
	   (t
	    (loop
	      (multiple-value-bind
		    (count err)
		  (unix:unix-read (fd-stream-fd stream) sap len)
		(declare (type (or index null) count))
		(unless count
		  (error "Error reading ~S: ~A" stream
			 (unix:get-unix-error-msg err)))
		(incf (fd-stream-ibuf-tail stream) count)
		(when (zerop count)
		  (if eof-error-p
		      (error "Unexpected eof on ~S." stream)
		      (return (- requested (truncate now-needed elsize)))))
		(let* ((copy (min now-needed count))
		       (copy-bits (* copy vm:byte-bits))
		       (buffer-start-bits
			(* (+ offset available) vm:byte-bits)))
		  (declare (type index copy copy-bits buffer-start-bits))
		  (if (typep buffer 'system-area-pointer)
		      (system-area-copy sap 0
					buffer buffer-start-bits
					copy-bits)
		      (copy-from-system-area sap 0 
					     buffer (+ buffer-start-bits
						       (* vm:vector-data-offset
							  vm:word-bits))
					     copy-bits))
		  (incf (fd-stream-ibuf-head stream) copy)
		  (decf now-needed copy)
		  (when (zerop now-needed) (return requested))
		  (incf offset copy))))))))
      (t
       requested))))
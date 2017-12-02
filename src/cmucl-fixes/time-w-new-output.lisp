;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: time.lisp,v 1.14 93/08/25 01:15:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;  A modification of the core %time function with simpler output.
;;;
(in-package "LISP")

(defun %time (fun)
  (let ((fun (massage-time-function fun))
	old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        old-page-faults
        new-page-faults
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        page-faults-overhead
        old-bytes-consed
        new-bytes-consed
        cons-overhead)
    ;; Calculate the overhead...
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (let ((start-gc-run-time *gc-run-time*))
    (multiple-value-prog1
        ;; Execute the form and return its values.
        (funcall fun)
      (multiple-value-setq
	  (new-run-utime new-run-stime new-page-faults new-bytes-consed)
	(time-get-sys-info))
      (setq new-real-time (- (get-internal-real-time) real-time-overhead))
      (let ((gc-run-time (max (- *gc-run-time* start-gc-run-time) 0)))
	(format *trace-output*
		"~&Evaluation took ~S/~S/~S of real/user/sys run time~%  ~
                 ~@[[Run times include ~S second~:P GC run time]~%  ~]~
		 ~S page fault~:P, ~S bytes consed.~%"
		(max (/ (- new-real-time old-real-time)
			(float internal-time-units-per-second))
		     0.0)
		(max (/ (- new-run-utime old-run-utime) 1000000.0) 0.0)
		(max (/ (- new-run-stime old-run-stime) 1000000.0) 0.0)
		(unless (zerop gc-run-time)
		  (/ (float gc-run-time)
		     (float internal-time-units-per-second)))
		(max (- new-page-faults old-page-faults) 0)
		(max (- new-bytes-consed old-bytes-consed) 0)))))))
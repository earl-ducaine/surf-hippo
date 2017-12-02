;;;
;;; Fixes for CMU CL
;;;
;;; $Header: /home/sprr/stolcke/boogie/RCS/cmu17-fix.cl,v 1.4 1994/11/25 09:55:24 stolcke Exp $
;;;

;;;
;;; Addition to code/unix.lisp: support for times(2) system call
;;;

(in-package "UNIX")

(export '(unix-times unix-time-units-per-second))

;;; From sys/times.h

(def-alien-type nil
  (struct tms
    (tms-utime #-alpha long #+alpha int)	; user time used
    (tms-stime #-alpha long #+alpha int)	; system time used.
    (tms-cutime #-alpha long #+alpha int)	; user time, children
    (tms-cstime #-alpha long #+alpha int)))	; system time, children

(defconstant unix-time-units-per-second 100
  "Machine-dependent units in which unix-times results are returned,
  not necessarily equal to internal-time-units-per-second")

#+(and sparc svr4)
(defun unix-times ()
  "Unix-times returns information about the cpu time usage
   of the process and its children. NIL and an error number
   is returned if the call fails."
  (with-alien ((usage (struct tms)))
    (syscall* ("times" (* (struct tms)))
	      (values t
		      (slot usage 'tms-utime)
		      (slot usage 'tms-stime)
		      (slot usage 'tms-cutime)
		      (slot usage 'tms-cstime))
	      (addr usage))))

;;;
;;; Fix to code/time.lisp
;;;

(in-package "LISP")

;;; Get-Internal-Run-Time  --  Public
;;;
#+(and sparc svr4)
(defun get-internal-run-time ()
  "Return the run time in the internal time format.  This is useful for
  finding CPU usage."
  (declare (values (unsigned-byte 32)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore utime stime cutime cstime)
			 (unix:unix-times)
      (declare (ignore ignore cutime cstime)
	       (type (unsigned-byte 31) utime stime))
      (values (truncate (* (the (unsigned-byte 32) (+ utime stime))
                           internal-time-units-per-second)
                        unix:unix-time-units-per-second)))))

;;;
;;; Fix to code/<osname>-os.lisp
;;;

(in-package "SYSTEM")

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
#+(and sparc svr4)
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime cutime cstime)
      (unix:unix-times)
    (declare (ignore cutime cstime))
    (cond ((null err?)
	   (error "Unix system call times failed: ~A."
		  (unix:get-unix-error-msg utime)))
	  (T
	   ;; return times in microseconds for getrusage compatibility
	   ;; bummer: page fault statistics not supported
	   (values (truncate (* utime 1000000) unix:unix-time-units-per-second)
	           (truncate (* stime 1000000) unix:unix-time-units-per-second)
		   0)))))

#+osf1
;; this definition is botched in the original
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err?
      (error "Unix system call getrusage failed: ~A."
	     (unix:get-unix-error-msg utime)))
    
    (values utime stime majflt)))



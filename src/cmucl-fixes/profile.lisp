;;; -*- Package: Profile -*-
;;;
;; Change to cmucl /src/code/profile.lisp

(in-package "PROFILE")

;;; UNPROFILE  --  Public
;;;

;; Avoid consing count overflow by zeroing total bytes consed count with any unprofile. Remains to be seen if this is not so
;; smart.

(defmacro unprofile (&rest names)
  "Unwraps the profiling code around the named functions.  Names defaults to
  the list of all currently profiled functions."
  `(progn (setq lisp::*total-bytes-consed* 0)
	  (dolist (name ,(if names `',names '*timed-functions*) (values))
	    (unprofile-1-function name))))

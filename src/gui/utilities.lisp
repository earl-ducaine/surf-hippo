;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

;; The Surf-Hippo Neuron Simulator System
;;
;; This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
;; Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
;; Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
;;
;; Permission to use, copy, modify, and distribute this software and its documentation for any purpose
;; and without fee is hereby granted, provided that this software is cited in derived published work,
;; and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
;; Project makes no representations about the suitability of this software for any purpose. It is
;; provided "as is" without express or implied warranty.
;;
;; If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
;; on the mailing list.
;;
;; Copyright (c) 1989 - 2003, Lyle J. Graham


(in-package :windows-hack)

;;; Utility functions

(defvar *abort-on-sim-error* t
  "When T SIM-ERROR as default aborts to top level; otherwise
   SIM-ERROR goes to the debugger.")

(defun sim-error (&optional (message "") (abort-on-error *abort-on-sim-error*))
  "A fatal error occurred, print MESSAGE and abort if ABORT-ON-ERROR
   [default *ABORT-ON-SIM-ERROR*]; otherwise stay in debugger."
  (inter:beep)
  (inter:beep)
  (inter:beep)
  (format *error-output* "~%~% Fatal Error in Function ~A:~%")
  (break)
  (if abort-on-error
    (progn
      (format *error-output* "~%  ~a~%" message)
      (format *error-output* "~% Set *ABORT-ON-SIM-ERROR* to NIL to invoke debugger next time.~%")
      (format *error-output* "~% ** Restarting from Top Level **~%")
      (abort))
    (error "~%  ~a~%" message)))

;; Stolen from CMUCL17C code/query.lisp.
(defun yes-or-no-p-default-no (&optional format-string &rest arguments &key default)
  "Clears the input, beeps, prints the message, if any, and reads
   characters from *QUERY-IO* until the user enters YES (case
   insensitive) as an affirmative. If user only RETURNs or types
   something else, then returns DEFAULT [modification from cmucl 17c
   query.lisp]."
  (clear-input *query-io*)
  (inter:beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments)
    (force-output *query-io*))
  (if (string-equal (string-trim " 	" (read-line *query-io*)))
      "yes"
      default))

(defun y-or-n-p-default-no (&optional format-string &rest arguments &key default)
  "Clears the input, beeps, prints the message, if any, and reads
   characters from *QUERY-IO* until the user enters Y \(case
   insensitive\) as an affirmative. If user only RETURNs or types
   something else, then returns DEFAULT [modification from cmucl 17c
   query.lisp]."
  (clear-input *query-io*)
  (inter:beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments)
    (force-output *query-io*))
  (if (string-equal (string-trim " 	" (read-line *query-io*)))
      "Y"
      default))

(defun quit (&optional finish-function)
  ;; (Yes-OR-No-P-DEFAULT-NO "Do you really want to quit LISP? (RETURN for NO, yes/YES for YES): ")
  ;;
  ;; May 22 2002. CMU Common Lisp release x86-linux 2.4.20 10 March
  ;; 2000 build 498 (RedHat 7.3) Seem to need the RECKLESSLY-P flag -
  ;; otherwise get Error in function UNIX::SIGSEGV-HANDLER:
  ;; Segmentation Violation at #x4000BB04.
  (when finish-function
    (funcall finish-function))
  (surf-hippo-ext:quit))

(defvar *show-csh-result* nil)
(defvar *announce-shell-exec* nil "Announce execution of shell command by SHELL-EXEC.")
(defvar *wait-for-run-program* nil)

(defun shell-exec (command &optional (show-result *show-csh-result*))
  "Execute the COMMAND string in the shell, announcing the process
   when *ANNOUNCE-SHELL-EXEC* is T, and announcing the result if
   SHOW-RESULT is T [default *SHOW-CSH-RESULT*]."
  (when *announce-shell-exec*
    (format t "~%;;; Shell command: ~s~%" command))
  (let* ((output-stream (make-string-output-stream))
	 (process (uiop:run-program  command :output output-stream)))
    (when show-result
      (format t "~A~%" (get-output-stream-string output-stream)))
    (surf-hippo-ext::process-close process)
    nil))

;; LG 05.04.2012
#|
(defun software-version ()
  "Returns a string describing version of the supporting software."
  (string-trim '(#\newline) (with-output-to-string (stream) (ext::run-program "uname" '("-sr") :output stream))))
|#

(defun sim-warning (message &rest args)
  "A warning has occurred, print MESSAGE and continue."
  (inter::beep)  (inter::beep)  (inter::beep)
  (warn message))

;;; From the Common Lisp Cookbook
#|
 (build-symbol [(:package p)] -pieces-)

   builds a symbol by concatenating the given pieces and interns it as specified by p. For each element of pieces, if it is a ...

    * ... string: The string is added to the new symbol's name.
    * ... symbol: The name of the symbol is added to the new symbol's name.
    * ... expression of the form (:< e): e should evaluate to a string, symbol, or number; the characters of the value of e (as printed by princ) are
					 concatenated into the new symbol's name.
    * ... expression of the form (:++ p): p should be a place expression (i.e., appropriate as the first argument to setf), whose value is an integer; the
					  value is incremented by 1, and the new value is concatenated intot he new symbol's name.

If the :package specification is omitted, it defaults to the value of *package*. If p is nil, the symbol is interned nowhere. Otherwise, it should evaluate
to a package designator (usually, a keyword whose name is the same of a package). For example, (build-symbol (:< x) "-" (:++ *x-num*)), when x = foo and
*x-num* = 8, sets *x-num* to 9 and evaluates to FOO-9. If evaluated again, the result will be FOO-10, and so forth.

|#

#|
(defmacro build-symbol (&rest l)
   (let ((p (find-if (lambda (x) (and (consp x) (eq (car x) ':package)))
                     l)))
      (cond (p
             (setq l (remove p l))))
      (let ((pkg (cond ((eq (cadr p) 'nil)
                        nil)
                       (t `(find-package ',(cadr p))))))
         (cond (p
                (cond (pkg
                       `(values (intern ,(symstuff l) ,pkg)))
                      (t
                       `(make-symbol ,(symstuff l)))))
               (t
                `(values (intern ,(symstuff l))))))))

(defun symstuff (l)
   `(concatenate 'string
      ,@(for (x :in l)
           (cond ((stringp x)
                  `',x)
                 ((atom x)
                  `',(format nil "~a" x))
                 ((eq (car x) ':<)
                  `(format nil "~a" ,(cadr x)))
                 ((eq (car x) ':++)
                  `(format nil "~a" (incf ,(cadr x))))
                 (t
                  `(format nil "~a" ,x))))))
|#

(defmacro build-symbol (&rest l)
  (let ((p (find-if (lambda (x) (and (consp x) (eq (car x) ':package)))
		    l)))
    (cond (p
	   (setq l (remove p l))))
    (let ((pkg (unless (eq (cadr p) 'nil) `(find-package ',(cadr p))))
	  (symstuff-l (symstuff l)))
      (cond (p
	     (if pkg `(values (intern ,symstuff-l ,pkg)) `(make-symbol ,symstuff-l)))
	    (t
	     `(values (intern ,symstuff-l)))))))

(defun symstuff (l)
   `(concatenate 'string
      ,@(loop for x in l collect
           (cond ((stringp x)
                  `',x)
                 ((atom x)
                  `',(format nil "~a" x))
                 ((eq (car x) ':<)
                  `(format nil "~a" ,(cadr x)))
                 ((eq (car x) ':++)
                  `(format nil "~a" (incf ,(cadr x))))
                 (t
                  `(format nil "~a" ,x))))))


(defun make-var (symbol &optional (package *package*)) (intern (string-upcase symbol) package))

(defun create-output-symbol (name &rest names)
  (let ((symbol (loop for extra in names
		      do (setq name (format nil "~a-~a" name extra))
		      finally (return (make-var name)))))
    (export symbol)
    symbol))

;;(declaim (inline quote-only-symbol))

;;(defmacro quote-only-symbol (val) `(if (and (symbolp ,val) (not (keywordp ,val))) `',,val ,val))

(declaim (inline quote-symbol-cons))

(defun quote-symbol-cons (val)
  ;; Returns VAL, preceeded by ' if VAL is a symbol and not a keyword. Used for constructing forms when a component may be a symbol, cons, string or
  ;; number, to prevent evaluation of the symbol or cons, and avoid the uneccessary (and non-pristine) quoting of strings and numbers.
  (if (t-or-nil-p val)
      val
      (if (or (consp val)
	      (and (symbolp val)
		   (not (keywordp val))))
	  `(quote ,val)
	  val)))

(defun unless-number-then-string (thing)
  ;; If THING is number, return it. Otherwise try to convert to string using FORMAT.
  (typecase thing
    (number thing)
    (t (format nil "~A" thing))))

(defun number-sequence-p (sequence) (and (sequencep sequence) (every 'numberp sequence)))

(defun limit-sig-figs (number &optional (sig-figs 3) (rounding :round))
  ;; Hack to trim off floating point NUMBER to SIG-FIGS, according to the rule given by ROUNDING , either :ROUND [defaul] :FLOOR or :CEILING.
  (if (zerop number)
      number
      (let* ((num-power (floor (log (abs number) 10.0)))
	     (sig-figs (1- sig-figs))
	     (divisor (expt 10 (- num-power sig-figs))))
	(* (funcall (case rounding
		      (:round 'round)
		      (:floor 'floor)
		      (:ceiling 'ceiling))
		    number divisor)
	   divisor))))

(defun funcalls (funspecs)
  "Call FUNCALL on the atom or list of FUNSPECS."
  ;; (mapcar 'funcall (coerce-to-list funspecs))
  (mapcar #'(lambda (funspec)
	      (typecase funspec
		(cons (apply (car funspec) (rest funspec)))
		(atom (funcall funspec))
		(t nil)))
	  (coerce-to-list funspecs)))

#|
(defun evals (funspecs)
  "Call EVAL on the atom or list of FUNSPECS."
  (mapcar 'eval (coerce-to-list funspecs)))
|#

(defun max-depth (tree)
  (if (atom tree)
      0
      (1+ (reduce #'max (mapcar #'max-depth tree)))))

(export '(max-depth
	  number-sequence-p yes-or-no-p-default-no Y-OR-N-P-DEFAULT-NO quit *announce-shell-exec* *show-csh-result* shell-exec
	  *wait-for-run-program* build-symbol quote-symbol-cons
	  *abort-on-sim-error*
	  ;;sim-error
	  software-version create-output-symbol
	  unless-number-then-string
	  sim-warning
	  limit-sig-figs funcalls ;; evals
	  ))

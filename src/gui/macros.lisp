;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;; GUI Source file: macros.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Contains various macros and utilities that are required by the MENU-HACK and PLOT-HACK systems.


(defmacro type-of-seq (sequence)
  `(typecase ,sequence
     ;; LG fix 2.9.2016
     ;; (array 'simple-array)
     (array 'vector)
     (vector 'vector)
     (cons 'cons)))

;; Dosen't work?
(defmacro map-type-conserving (function sequence &rest more-sequences)
  `(let ((internal-sequence ,sequence))
     (map (type-of-seq internal-sequence) ,function internal-sequence ,more-sequences)))

(defmacro mapcar-return-no-nils (target &body body)
;; Loop through the result of applying FLATTEN-LIST to TARGET, via the local variable VALUE, collect the results of BODY if non-nil and return them.
  `(loop for value in (flatten-list ,target)
	 do (setq value (progn . ,body))
	 when value collect value))

(defmacro string-case (keyform &body cases)
;; STRING-CASE Keyform {({(Key*) | Key} Form*)}*
;; Evaluates the Forms in the first clause with a Key STRING= to the value of
;; Keyform.  If a singleton key is T then the clause is a default clause.
  (COMMON-LISP::CASE-BODY 'case keyform cases t 'string= nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro generic-numeric-coerce (num-or-num-seq result-type result-numeric-type)
  ;; If NUM-OR-NUM-SEQ is a number, coerce to RESULT-NUMERIC-TYPE, or if a numeric sequence, returns a sequence of RESULT-NUMERIC-TYPE. If NUM-OR-NUM-SEQ
  ;; is a sequence, the returned sequence is of the same sequence type (list or array), unless RESULT-TYPE is specified explicitly as 'CONS, 'LIST, 'ARRAY
  ;; or 'SIMPLE-ARRAY. If RESULT-TYPE is 'NUMBER, then NUM-OR-NUM-SEQ must be a number.
  (if (equal result-type (quote 'NUMBER)) ; This seems stupid. why does this need to be double quoted?
      (if (equal result-numeric-type (quote 'FIXnum))
	  `(round ,num-or-num-seq)
	  `(coerce ,num-or-num-seq ,result-numeric-type))
      `(let* ((internal-num-or-num-seq ,num-or-num-seq)
	      (num-or-num-seq-type-spec (type-of internal-num-or-num-seq))
	      (internal-result-type ,result-type)
	      (internal-result-numeric-type ,result-numeric-type)
	      (result-type-arrayp (or (member internal-result-type '(ARRAY SIMPLE-ARRAY SIMPLE-VECTOR)) ; RESULT-TYPE specifies array
				      (and (not internal-result-type) (arrayp internal-num-or-num-seq)))) ; NIL RESULT-TYPE and input is array 
	      (num-or-num-seq-type-ok-arrayp (and result-type-arrayp ; We can use the input array for the output.
						  (arrayp internal-num-or-num-seq)
						  (member internal-result-numeric-type num-or-num-seq-type-spec)))
	      (out-sequence (when result-type-arrayp
			      (if (or (consp internal-num-or-num-seq) (not num-or-num-seq-type-ok-arrayp))
				  (make-array (length internal-num-or-num-seq) :element-type ,result-numeric-type)
				  internal-num-or-num-seq))))
	(if (or (eql internal-result-type num-or-num-seq-type-spec) ; If the input type matches the output spec, just return the input.
		(and (consp num-or-num-seq-type-spec)
		     (member internal-result-type num-or-num-seq-type-spec)
		     (member internal-result-numeric-type num-or-num-seq-type-spec)))
	    internal-num-or-num-seq
	    (flet ((internal-coercion (val index)
		     (let ((coerced-val (case internal-result-numeric-type
					  (fixnum (round val))
					  (t (coerce val internal-result-numeric-type)))))
		       (if result-type-arrayp
			   (setf (aref out-sequence index) coerced-val)
			   (push coerced-val out-sequence)))))
	      (typecase internal-num-or-num-seq
		(number (coerce internal-num-or-num-seq ,result-numeric-type))
		(cons (loop for val in internal-num-or-num-seq for index fixnum from 0
			    do (internal-coercion val index) finally (return (typecase out-sequence (cons (reverse out-sequence)) (t out-sequence)))))
		(array (loop for val across internal-num-or-num-seq for index fixnum from 0
			     do (internal-coercion val index) finally (return (typecase out-sequence (cons (reverse out-sequence)) (t out-sequence)))))))))))
	   
(defmacro d-flt-gen (arg &optional result-type) `(generic-numeric-coerce ,arg ,result-type 'double-float))
(defmacro d-flt (arg) "Coerce number ARG to a double-float." `(d-flt-gen ,arg 'number))
(defmacro d-flt-num (arg) `(d-flt-gen ,arg 'number))
(defmacro d-flt-list (arg) "Coerce numeric sequence ARG to a list of double-floats." `(d-flt-gen ,arg 'list))
(defmacro d-flt-array (arg) "Coerce numeric sequence ARG to an array of double-floats." `(d-flt-gen ,arg 'simple-array))

(defmacro s-flt-gen (arg &optional result-type) `(generic-numeric-coerce ,arg ,result-type 'single-float))
(defmacro s-flt (arg) "Coerce number ARG to a single-float." `(s-flt-gen ,arg 'number))
(defmacro s-flt-num (arg) `(s-flt-gen ,arg 'number))
(defmacro s-flt-list (arg) "Coerce numeric sequence ARG to a list of single-floats." `(s-flt-gen ,arg 'list))
(defmacro s-flt-array (arg) "Coerce numeric sequence ARG to an array of single-floats." `(s-flt-gen ,arg 'simple-array))

(defmacro fix-gen (arg &optional result-type) `(generic-numeric-coerce ,arg ,result-type 'fixnum))
(defmacro fix (arg) "Coerce number ARG to a fixnum." `(fix-gen ,arg 'number))
(defmacro fix-num (arg) `(fix-gen ,arg 'number))
(defmacro fix-list (arg) "Coerce numeric sequence ARG to a list of fixnums." `(fix-gen ,arg 'list))
(defmacro fix-array (arg) "Coerce numeric sequence ARG to an array of fixnums." `(fix-gen ,arg 'simple-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFVARS and DEFVAR-W-VALUE are macros that allow the declaring and assigning of more than one global variable at a time.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defvars (&rest vals)
  ;; VALS may consist of atoms denoting unbound global variables and lists whose CAR is the variable symbol and whose CADR is the
  ;; assigned value as set by DEFVAR. 
  `(progn
     ,@(loop for elt in vals
	     collect (if (consp elt)
		       `(defvar ,(nth 0 elt) ,(nth 1 elt))
		       `(defvar ,elt)))))

(defmacro defvars-w-list (vals-list)
  ;; VALS may consist of atoms denoting unbound global variables and lists whose CAR is the variable symbol and whose CADR is the
  ;; assigned value as set by DEFVAR. 
  `(progn
     ,@(loop for elt in `,vals-list
	  collect (if (consp elt)
		      `(defvar ,(nth 0 elt) ,(nth 1 elt))
		      `(defvar ,elt)))))


(defmacro defvars-w-value (&rest list)
   `(progn ,@(loop for (variable value) in list collect `(defvar ,variable ,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cond-every (&rest list)
  `(progn ,@(loop for clause in list collect `(when ,(car clause) ,(cons 'progn  (cdr clause))))))


(defvar *break-on-printvars* nil "Useful for tracking lost PRINTVARS statements.")

(defmacro printvars (&rest vals)
  "Useful for debugging - print out (local or global) symbol and its value, thus

 (let ((foo ...) (bar ...))
   ... (printvars foo bar) ... )

will give

 PRINTVARS: FOO value, BAR value

Literal strings as arguments will be printed as such.

See also *BREAK-ON-PRINTVARS*.
"
  `(progn
    (format t "PRINTVARS: ")
    (format t "~{~<~:; ~a ~a~>~^, ~}~%" (list ,@(loop for elt in vals
						      when (stringp elt)
						      collect elt and collect ""
						      else
						      collect `(quote ,elt) and collect elt)))
    (when *break-on-printvars* (break))
    (terpri)))



#|
Newsgroups: comp.lang.lisp
From: hall@aplcenmp.apl.jhu.edu (Marty Hall)
Subject: Re: Help
Organization: JHU/APL AI Lab, Hopkins P/T CS Faculty
Date: Mon, 2 Oct 1995 13:29:40 GMT

In article <KANDERSO.95Sep29123338@lager.bbn.com>
kanderso@lager.bbn.com (Ken Anderson) writes:
>Becareful when doing such experiments since results can be misleading.

Ken has an award-winning (*) paper on Lisp benchmarking/profiling that
I would recommend, except that the FTP site where I got it
(wheaton.bbn.com) is no longer accessible.

(*) Cleverest-title Award: "Courage in Profiles" :-)

>Generally both your test DEPTH2  function  and the function that calls TIME
>should be compiled.  The test  function should iterated enough time to make
>the times reported meaningful.

On the other hand, this is a nontrivial matter also. If the function
you are timing is small, a LOOP can skew your results, since the LOOP
overhead is included in your timing. Furthermore, if you compile a
LOOP, the compiler is free to take out things that don't affect the
result. For instance, (loop repeat 10000 do (* Var1 (+ Var2 (sqrt Var3))))
could be taken out altogether at compile time and replaced by NIL.

What I typically use is a macro that expands into a PROGN repeating
the function call N times. Here it is:

|#
;;; Part of http://www.apl.jhu.edu/~hall/lisp/Simple-Metering.lisp

;;;===========================================================================
;;; Time-Form: Takes a single form and optionally an integer N (default 20). It
;;; =========  runs the form N times and prints out the average time. Useful
;;;            for timing very small, fast functions when the time-step of
;;;            the builtin TIME function is too coarse.
;;;            > (Time-Form (Foo))     ; Call (Foo) 20 times + print avg time
;;;            > (Time-Form (Bar) 100) ; Call (Bar) 100 times + print avg time
;;; 1994 Marty Hall.

(defmacro Time-Form (Form &optional (Repetitions 20))
  "Runs FORM N times, printing avg execution time and returning FORM's value"
  (declare (optimize speed (safety 1) (compilation-speed 0)))
  `(let* ((Start (get-internal-run-time))
	  (Value (progn ,@(loop for I from 1 to Repetitions collecting Form)))
	  (Stop (get-internal-run-time)))
    (format t "~%Time to do ~S is ~0,5F sec."
     ',Form
     (float (/ (- Stop Start)
	       (* ,Repetitions internal-time-units-per-second))))
    Value))

#|
Example: (pprint (macroexpand-1 '(Time-Form (Foo 1 2 3) 10)))
(LET* ((START (GET-INTERNAL-RUN-TIME))
       (VALUE
        (PROGN
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)))
       (STOP (GET-INTERNAL-RUN-TIME)))
  (FORMAT T "~%Time to do ~S is ~0,5F sec." '(FOO 1 2 3)
          (FLOAT
           (/ (- STOP START) (* 10 INTERNAL-TIME-UNITS-PER-SECOND))))
  VALUE) 

Note that if you really care, you could subtract off the time of one
call to get-internal-run-time. But since this only happens once, not
for each N, I usually ignore it.
						- Marty
(proclaim '(inline skates))

|#
(export '(TYPE-OF-SEQ
	  map-type-conserving
	  mapcar-return-no-nils
          string-case
	  generic-numeric-coerce
	  d-flt-gen d-flt d-flt-num d-flt-list d-flt-array
	  s-flt-gen s-flt s-flt-num s-flt-list s-flt-array
	  fix-gen fix fix-num fix-list fix-array
	  defvars defvars-w-value make-var 
	  *break-on-printvars* printvars
	  cond-every
	  Time-Form
	  ))

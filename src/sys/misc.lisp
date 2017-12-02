;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;;; SYS Source file: misc.lisp

(in-package "SURF-HIPPO")

(defun update-cool-hippo-window ()
  ;; Called by BLANKET-INITIALIZE.
  (if *cool-hippo-window-p*
    (unless (opal-obj-exists *cool-hippo-window*)
      (SHOW-IMAGE (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.half.bmp") :crop t :left :right :top :bottom :title "Surf's Up")
      (setq *cool-hippo-window* (SHOW-IMAGE (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.half.bmp") :crop t :left :right :top :bottom :title "Surf's Up")))
    (when *cool-hippo-window*
      (clear-window *cool-hippo-window*)
      (setq *cool-hippo-window* nil)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GET-ORIGINAL-SURF-VARIABLE-SYMBOLS and GET-NEW-SURF-VARIABLE-SYMBOLS should be rewritten for general packages.
(defun get-original-surf-variable-symbols ()
  ;; Called by BLANKET-INITIALIZE.
  (let ((surf (find-package 'surf)))
    (or *original-surf-variable-symbols*
	(do-symbols (sym surf *original-surf-variable-symbols*)
	  (when (and (eq surf (symbol-package sym))
		     (not (fboundp sym))
		     (not (member sym *original-surf-variable-symbols*)))
	    (push sym *original-surf-variable-symbols*))))))

(defun get-new-surf-variable-symbols (&optional only-bound)
  ;; Called by PRINT-DOCUMENTED-USER-VARIABLES.
  (let ((surf (find-package 'surf))
	new-symbols)
    (do-symbols (sym surf new-symbols)
      (when (and (eq surf (symbol-package sym))
		 (not (fboundp sym))
		 (or (not only-bound) (boundp sym))
		 (not (member sym *original-surf-variable-symbols*)))
	(push sym new-symbols)))
    (mapcar #'cadr (sort (mapcar #'(lambda (sym) (list (symbol-name sym) sym)) new-symbols) #'string-lessp :key #'car))))

(defun print-package-symbols (&optional (package-reference *package*))
  "Print the number of documented functions, non-documented functions and other symbols in PACKAGE-REFERENCE [default *PACKAGE]."
  (let ((package (find-package package-reference)))
    (if (not package)
	(format t "FIND-PACKAGE fails to find a package associated with ~S!~%" (quote-symbol-cons package-reference))
	(let ((documented-functions 0)
	      (non-documented-functions 0)
	      (documented-symbols 0)
	      (non-documented-symbols 0))
	  (do-symbols (sym package)
	    (when (eq package (symbol-package sym))
	      (if (fboundp sym)
		  (if (documentation sym 'function)
		      (incf documented-functions)
		      (incf non-documented-functions))
		  (if (documentation sym 'variable)
		      (incf documented-symbols)
		      (incf non-documented-symbols)))))
	  (format t "The ~A package includes ~A documented functions, ~A non-documented functions, ~A documented variables, and ~A other symbols.~%"
		  (package-name package)
		  documented-functions
		  non-documented-functions
		  documented-symbols
		  non-documented-symbols)))))

(defun clear-user-variables (&optional variables-to-keep)
  "Unintern global variables defined during the current session, other than those given by the symbol or list of symbols in
VARIABLES-TO-KEEP [default NIL]."
  (let ((variables-to-keep (coerce-to-list variables-to-keep)))
    (loop for new-surf-variable-symbol in (get-new-surf-variable-symbols)
	  unless (member new-surf-variable-symbol variables-to-keep)
	  do (unintern new-surf-variable-symbol))))
  
(defun non-empty-hash-tables (tables)
  (loop for table in tables unless (HASH-TABLE-EMPTY table) collect table))




;; ************* ************* ************* *************
;;
;;   Time Stamp Related Functions
;;
;; ************* ************* ************* *************

(defun stamped-circuit-name ()
  (if (> (length *circuit*) 0)
    (format nil "~a-~a" *circuit* *time-stamp*)
    (format nil "~a" *time-stamp*)))

(defun update-simulation-time-stamp-and-name ()
  (encode-time-stamp)
  (setq *simulation-name* (stamped-circuit-name)))

;;; Generic function to display times

(defun display-time() (format t "Simulation time: ~5,0d. Stop time: ~5,0d~%" *real-time* *user-stop-time*))

#|
Subject: Re: format-time-string?
Date: 13 Nov 1997 09:09:45 -0800
From: tar@sevak.isi.edu (Thomas A. Russ)
Organization: USC-ISI
CC: sshteingold@cctrading.com
Newsgroups: comp.lang.lisp

Here's something to get you started.  It takes a universal time and
produces a formatted version.  It has some options, but not an awful
lot.  If you want to have a string returned instead of printed out, then
pass NIL to the stream argument with style :string.
|#
(defvar *format-time-smallest-unit* :second
  "One of :second, :minute, :hour, :day, :month, :year")
(defvar *format-time-style* :string
  "One of :string, :s-expression, nil.  Nil means no formatting.")
(defvar *format-time-include-date-p* t "Whether the date is included.")
(defvar *format-time-long-date-p* nil "t => February 22, 1958; nil => 2/22/58")

(defun format-time (universalTime &optional (stream t)
                    &key (smallest-unit *format-time-smallest-unit*)
                         (include-date-p *format-time-include-date-p*)
                         (long-date-p *format-time-long-date-p*)
                         (style *format-time-style*))
  ;; Formats "universalTime" on the stream "stream";
  ;; ":smallest-unit" is one of :second, :minute, :hour, :day, :month, :year
  ;;    and controls the smallest unit used in the formated time;
  ;; ":include-date-p" if non-nil means the date is included in the time
  ;;    representation.
  ;; ":long-date-p" if non-nil means to use the long form of the date;
  ;; ":style" is :string for string format, :s-expression for keyword list format, nil
  ;;    for no formatting.
  (unless (and (numberp universalTime)
               (plusp universalTime)    ; Safety valve.
               (member style '(:string :s-expression)))
    (format stream "~S" universalTime)
    (return-from format-time universalTime))
  (labels ((month-name (month)
            (aref '#("January" "February" "March" "April" "May" "June" "July"
                     "August" "September" "October" "November" "December")
                  (1- month)))
           (year-not-close-enough-p (year)
            ;; "year" is not close enough to the current year to abbreviate!
            (> (abs (- year 1997)) 45))
         (format-string-time (second minute hour date month year)
           (if long-date-p
               (ecase smallest-unit
                 (:second (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D:~2,'0D:~2,'0D"
                                  include-date-p (month-name month) date year
                                  hour minute second))
                 (:minute (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D:~2,'0D"
                                  include-date-p (month-name month) date year hour minute))
                 (:hour (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D"
                                include-date-p (month-name month) date year hour))
                 (:day (format stream "~:[~3*~;~A ~D, ~D~]"
                               include-date-p (month-name month) date year))
                 (:month (format stream "~:[~2*~;~A ~D~]"
                                 include-date-p (month-name month) year))
                 (:year (format stream "~:[~*~;~D~]" include-date-p year)))
               (ecase smallest-unit
                 (:second (format stream
                                  "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D:~2,'0D:~2,'0D"
                                  include-date-p month date year hour minute second))
                 (:minute (format stream
                                  "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D:~2,'0D"
                                  include-date-p month date year hour minute))
                 (:hour (format stream "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D"
                                include-date-p month date year hour))
                 (:day (format stream "~:[~3*~;~2,'0D/~2,'0D/~2,'0D~]"
                               include-date-p month date year))
                 (:month (format stream "~:[~2*~;~2,'0D/~2,'0D~]"
                                 include-date-p month year))
                 (:year (format stream "~:[~*~;~D~]" include-date-p year)))))
         (format-s-expression-date (second minute hour date month year)
           (format stream "~S"
                   (ecase smallest-unit
                     (:second (if include-date-p
                                  `(:year ,year :month ,month :day ,date
                                    :hour ,hour :minute ,minute :second ,second)
                                  `(:hour ,hour :minute ,minute :second ,second)))
                     (:minute (if include-date-p
                                  `(:year ,year :month ,month :day ,date
                                    :hour ,hour :minute ,minute)
                                  `(:hour ,hour :minute ,minute)))
                     (:hour (if include-date-p
                                  `(:year ,year :month ,month :day ,date :hour ,hour)
                                  `(:hour ,hour)))
                     (:day (if include-date-p
                               `(:year ,year :month ,month :day ,date)
                               nil))
                     (:month (if include-date-p
                                 `(:year ,year :month ,month)
                                 nil))
                     (:year (if include-date-p
                                `(:year ,year)
                                 nil))))))

    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time universalTime)
      (unless (or long-date-p
                  (eq smallest-unit :year)
                  (year-not-close-enough-p year))
        (setq year (mod year 100)))
      (case style
        (:string (format-string-time second minute hour date month year))
        (:s-expression (format-s-expression-date second minute hour date month year))
        (t (format stream "~S" universalTime))) )))

;; ************* ************* ************* *************
;;
;;   File Related Functions
;;
;; ************* ************* ************* *************


;; lib/system-variables.lisp  in *SURF-USER-DIR*
(defvar *system-variables* '(*circuit-directory*
			     *lisp-file-directory*
			     *data-directory*
			     *plot-directory*
			     *last-simulation-file-path*
			     *lte-node-criterium*
			     *simulation-print-detail*
			     *save-simulation-data*
			     *save-simulation-data-to-file*
			     *DOCUMENT-all-new-VARIABLES*
			     *colorize-simulation*
			     *enable-colorize-time*
			     *enable-colorize-scale*
			     *enable-sparse-data*
			     *sparse-data-step*


			     *include-channel-type-comment-in-particle-plots*
			     *SAVE-CONDUCTANCES-normalized*
			     *plot-standard-windows*
			     *HIDE-plot-WINDOWS*
			     *create-new-simulation-plots*
			     *plot-data-grouping*
			     *use-simulation-name-for-simulation-plot-titles*
			     *simulation-plot-window-comment*
			     *simulation-plot-window-comment-position*
			     *traces-per-plot*
			     *max-num-traces-for-plot-trace-labels*
			     *save-data-step*
			     *plot-total-concs-separately*
			     *plot-node-elements*
			     *plot-total-conductances-p*

			     *plot-total-conductances*
			     *interpolate-particle-arrays*
			     *minimum-source-transition-time*
			     *consider-isource-drop*
			     *default-waveform-step*
			     *pwl-isource-di-dt*
			     *isource-default-reference-magnitude*
			     *vsource-default-reference-magnitude*
			     *pwl-vsource-dv-dt*
			     *vsource-resistance*
			     *user-stop-time*
			     *user-max-step*
			     *user-min-step*
			     *user-step*
			     *integration-time-reference*
			     *use-node-voltage-initializations*
			     *use-conc-int-initializations*
			     *use-buffer-initializations*
			     *use-pump-initializations*
			     *enable-dynamic-breakpoint-generation*
			     *absolute-voltage-error*
			     *absolute-particle-error*
			     *absolute-conc-int-error*
			     *FULL-ERROR-STEP-CHANGE*
			     *consider-particle-error*
			     *calculate-particle-error*
			     *calculate-markov-particle-error*
			     *consider-conc-particle-error*
			     *consider-conc-int-error*
			     *calculate-conc-int-error*
			     *eval-conc-ints-in-inner-loop*
			     *pick-time-step-fudge*

			     *enable-axons*
			     *include-events-in-element-documentation-code*
			     *enable-event-generators*
			     *SETUP-EVENT-GENERATORS-AND-FOLLOWERS*
			     *USER-SPECIFIED-EVENT-ELEMENT-SETS*
			     *maximum-synapse-printed-events*
			     *enable-synapses*
			     *adjust-breakpoints-for-event-synapses*
			     *convert-light-response-to-events-for-each-synapse*
			     *enable-light-event-update*
			     *constant-light-input-from-negative-infinity*
			     *enable-light*
			     *count-active-and-triggered-synapses*
			     *always-intialize-random-gen*))

(defvar *read-system-variables-file-on-startup* nil "When T, on startup Surf-Hippo evaluates the system-variables.lisp file, that was written into the lib
directory under *SURF-USER-DIR* at the end of the last session.")

(defun read-system-variables-file ()	
  (load (system-variables-filename))
  nil)

(defun write-system-variables-file ()
  (create-path (concatenate-strings *surf-user-dir* "lib/"))
  (with-open-stream (stream (open (system-variables-filename) :direction :output :if-exists :supersede))
    (let ((*standard-output* stream))
      (format t ";;; -*- Package: SURF; Mode: LISP -*-")
      (terpri)(terpri)(terpri)
      (format t "#|~%")
      (format t "This file is automatically written when quitting Surf-Hippo, and is loaded on startup.~%~%")
      (format t "Last written: ~a~%~%" (print-date nil t))
      (format t "|#~%")
      (terpri)(terpri)(terpri)
      (print-variables-setq-form '*read-system-variables-file-on-startup*)
      (print-VARIABLES-setq-form *system-variables* "when *READ-SYSTEM-VARIABLES-FILE-ON-STARTUP*")
      (terpri)(terpri)(terpri))))

(defun update-surf-log-file (info-function &optional (info-function-args '()))
  (update-annotation-file info-function (concatenate-strings *surf-user-dir* "logs") "log" info-function-args))

(defun load-and-compile-user-source (candidates &key src-dir bin-dir)
  "For the file names in CANDIDATES (namestrings w/o extensions), look in the SRC-DIR (if not supplied, then the \"circuits\"
directory under *SURF-USER-DIR*), compile file and write binary to the BIN-DIR (if not supplied, same directory as above), and
load binary."
  (let ((CIRCUIT-DIR (concatenate-strings *SURF-USER-DIR* "/circuits/")))
    (compile-source-directory (or src-dir circuit-dir) (or bin-dir circuit-dir) candidates)))

(defun get-surf-directory (&optional sub-directory-name (time-stamp *actual-time-stamp*))
  (let ((pathname-directory (REMOVE-SPACES (concatenate-strings *surf-user-dir* "/" sub-directory-name "/"))))
    (if *make-circuit-subdir* (setq pathname-directory (concatenate-strings pathname-directory *circuit* "/")))
    (get-dated-directory pathname-directory time-stamp *make-circuit-subdir*)))

(defun get-top-level-surf-directory (&optional sub-directory-name) 
  (let ((pathname-directory (concatenate-strings *surf-user-dir* "/" (REMOVE-SPACES sub-directory-name) "/")))
    (get-dated-directory pathname-directory)))
    
(defun get-surf-data-directory ()
  "Create a directory based on the value of *SURF-USER-DIR*, and return its namestring. If *MAKE-CIRCUIT-SUBDIR* if T, incoporate
the current value of *CIRCUIT*:

   *SURF-USER-DIR*/data/*CIRCUIT*/M_D_Y/

where \"M_D_Y\" is the date. If *MAKE-CIRCUIT-SUBDIR* if NIL:

   *SURF-USER-DIR*/data/

"
  (get-surf-directory "data/"))

(defun get-surf-plot-directory ()
  "Create a directory based on the value of *SURF-USER-DIR*, and return its namestring. If *MAKE-CIRCUIT-SUBDIR* if T, incoporate
the current value of *CIRCUIT*:

   *SURF-USER-DIR*/plot/*CIRCUIT*/M_D_Y/

where \"M_D_Y\" is the date. If *MAKE-CIRCUIT-SUBDIR* if NIL:

   *SURF-USER-DIR*/plot/

"
  (get-surf-directory "plot/"))

;; GET-PLOT-DIRECTORY and GET-DATA-DIRECTORY are originally defined in gui/windows-hack.lisp

(defun get-plot-directory () (get-surf-directory "plot/"))

(defun get-data-directory () (get-surf-directory "data/"))

#|
(defun load-file-from-directory (filename dir)
  (let ((filename (REMOVE-SPACES filename)))
    (load
     (if (search "~" filename)
	 (if (= (search "~" filename) 0)
	     (concatenate-strings dir "/" (string-remove-head filename 1))
	     (sim-error (format nil "Can't parse this filename ~A" filename)))
	 filename))))
|#



(defun load-from-reference-directory (filename &rest filenames)
  "Loads FILENAME (and any other FILENAMES) which must be in the directory given by *REFERENCE-DIRECTORY*."
  (mapcar #'(lambda (filename) (load (REDUCE-REPEATS (concatenate-strings *reference-directory* "/" filename) #\/)))
	  (cons filename filenames)))

(defun load-from-this-directory (filename &rest filenames)
  "Loads FILENAME (and any other FILENAMES) which must be in the directory of the file being loaded which contains this form."
  (let ((*reference-directory* (DIRECTORY-NAMESTRING *load-truename*)))
    (mapcar 'load-from-reference-directory (cons filename filenames))))

(defun load-surf-user-file (filename &rest filenames)
  "Loads FILENAME (and any other FILENAMES) which must be in the Surf-Hippo user directory given by *SURF-USER-HOME*."
  (let ((*REFERENCE-DIRECTORY*  *surf-user-home*))
    (mapcar 'load-from-reference-directory (cons filename filenames))))

(defun load-surf-home-file (filename &rest filenames)
  "Loads FILENAME (and any other FILENAMES) which must be in the Surf-Hippo home directory given by *SURF-HOME*."
  (let ((*REFERENCE-DIRECTORY*  *surf-home*))
    (mapcar 'load-from-reference-directory (cons filename filenames))))


(defun funcalls-according-to-flags (flags-and-functions)
  ;; Handy when the flags might be changed by some of the functions, for example with CHOOSE-VARIABLE-VALUES menus.
  (loop for flag-and-function in flags-and-functions
	for flag in (loop for flag-and-function in flags-and-functions 
			  collecting (symbol-value (car flag-and-function))) ; This preserves the boolean flags.
	when flag do (funcall (cadr flag-and-function))))

;; ************* ************* ************* *************
;;
;;    Some Window/Message Notification Related Functions
;;
;; ************* ************* ************* *************

(defun announce-warning (message)
  (unless *automatic-run*
    (let ((surf-NOTIFY-WINDOW (create-instance nil
					       inter:interactor-window
					       (:visible nil)
					       (:background-color opal:red)
					       (:title (GET-win-TITLE-STRING "SURF-HIPPO WARNING!"))
					       (:text-obj (create-instance nil opal:text
									   (:visible t) (:top 0) (:left 10)
									   (:font (opal:get-standard-font :serif :bold-italic :large))))
					       (:width (o-formula (+ (gvl :text-obj :width)
								     (gv wh::*ok-button* :width) 5 10 15)))
					       (:height (o-formula (+ 10 (max (gvl :text-obj :height)
									      (gv wh::*ok-button* :height)))))
					       (:aggregate (create-instance nil opal:aggregate)))))
      (add-ok-button surf-notify-window (g-value surf-notify-window :aggregate))
      (s-value surf-NOTiFY-WINDOW :visible t)
      (s-value surf-NOTiFY-WINDOW :text-obj :string message)
      (opal:add-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
      (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)
      (inter:wait-interaction-complete)
      (s-value surf-NOTiFY-WINDOW :visible nil)
      (opal:remove-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
      (opal:update surf-NOTiFY-WINDOW t))))

(defun running-window ()
  (let ((surf-NOTiFY-WINDOW (create-instance nil
					     inter:interactor-window
					     (:visible nil)
					     (:background-color opal:green)
					     (:title (GET-win-TITLE-STRING "SURF-HIPPO"))
					     (:text-obj (create-instance nil opal:text
									 (:visible t) (:top 0) (:left 0)
									 (:font (opal:get-standard-font :serif :bold-italic :large))))
					     (:width (o-formula (gvl :text-obj :width)))
					     (:height (o-formula (gvl :text-obj :height)))						
					     (:aggregate (create-instance nil opal:aggregate)))))
    (s-value surf-NOTiFY-WINDOW :visible t)
    (s-value surf-NOTiFY-WINDOW :text-obj :string "Simulation running")
    (opal:add-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
    (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)
    (opal:update surf-NOTiFY-WINDOW t)
    surf-NOTiFY-WINDOW))


(defun surf-window-print-mini-info-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (setq *standard-info-output* (create-info-window))
  (let ((win (g-value interactor :window)))
    (s-value *standard-info-output* :title (concatenate-strings "Information Window " (g-value win :title)))
    (OUTPUT-TEXT-TO-INFO-WIN  (list 'g-value win :mini-info) t)))

(create-instance 'window-info-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t) 
		 ;; LG fix 01.09.2016 CMUCL 20 reads :CONTROL-LATIN_CAPITAL_G instead of :CONTROL-G
		 ;; (:start-event :control-\i)
		 (:start-event '(:control-\i :CONTROL-LATIN_SMALL_LETTER_I))
		 (:final-function #'surf-window-print-mini-info-inter-function))

(defun wipeout ()			    
  ;; Destroy all the created windows.
  (clear-plot-windows)
  (clear-histology-windows)
  (clear-info-windows)
  (dolist (win (garnet-debug:windows))
    (opal:destroy win)))

#|
(defvar w '())
(defvar phase-hi '())
(defvar phase-lo '())
(defvar mag-hi '())
(defvar mag-lo '())
(defun test-phase ()
  (setq w '() phase-hi '() phase-lo '() mag-hi '() mag-lo '())
  (do ((omega 0.01 (+ omega 0.01)))
      ((> omega 50.0))
    (setq w (cons omega w)
	  phase-lo (cons  (atan (- omega)) phase-lo)
	  phase-hi (cons  (atan (/ 1.0 omega)) phase-hi)
	  mag-lo (cons  (/ 1.0 (expt (+ 1 omega) 0.5))
			mag-lo)
	  mag-hi (cons  (/ 1.0 (expt (+ 1 (/ 1.0 omega)) 0.5))  mag-hi))))
  
(defun set-create-parameters (parameter-name model parameters)
  (let (parameter-value temp-parameter-value junk)
    (setf 
     parameter-value (eval (cdr-assoc parameter-name (model-default-params model)))
     temp-parameter-value (eval (cdr-assoc parameter-name (model-changed-params model)))
     junk (when temp-parameter-value (setf parameter-value temp-parameter-value))
     temp-parameter-value (eval (cdr-assoc parameter-name parameters))
     junk (when temp-parameter-value (setf parameter-value temp-parameter-value )))
    parameter-value))

|#

;; Some array related functions.

(defun change-2d-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (array single-float (* *)) old-array)) :element-type 'single-float)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (array single-float (* *)) old-array))))))
      (declare (fixnum i))
      (do ((j 0 (1+ j)))
	  ((= j (the fn (cadr (array-dimensions (the (array single-float (* *)) old-array))))))
	(declare (fixnum j))
	(setf (aref new-array i j) (aref (the (array single-float (* *)) old-array) i j))))
    new-array))

(defun change-2d-array-to-adjustable (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (simple-array single-float (* *)) old-array))
			       :element-type 'single-float :adjustable t)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (simple-array single-float (* *)) old-array))))))
      (declare (fixnum i))
      (do ((j 0 (1+ j)))
	  ((= j (the fn (cadr (array-dimensions (the (simple-array single-float (* *)) old-array))))))
	(declare (fixnum j))
	(setf (aref new-array i j) (aref (the (simple-array single-float (* *)) old-array) i j))))
    new-array))

(defun change-1d-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions old-array))))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions old-array)))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref old-array i)))
    new-array))

(defun change-1d-float-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions old-array) :element-type 'single-float)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions old-array)))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref old-array i)))
    new-array))

(defun change-1d-array-to-adjustable (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (simple-array single-float (*)) old-array))
			       :element-type 'single-float :adjustable t)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (simple-array single-float (*)) old-array))))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref (the (simple-array single-float (*)) old-array) i)))
    new-array))

(defun test-top-two-of-lists (list1 list2)
  (and (eq (car list1) (car list2))
       (eq (cadr list1) (cadr list2))))


;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; This file loads all the Surf-Hippo modules.
;;; 
;;; ** To prevent certain parts from being loaded, first set
;;;      user::load-XX-p to NIL.
;;; ** To get some of the parts which are not loaded by default to be loaded,
;;;    set user::load-XX-p to T.
;;; ** If you are a non-MIT user, set Your-Surf-Hippo-Pathname to be your local
;;;    Surf-Hippo directory.
;;; ** To override where something is loaded from, set Surf-Hippo-xx-PathName
;;;    before loading this file and/or Surf-Hippo-xx-src
;;;
;;; The controlling variables are:
;;; 
;;;      load-garnet-fixes-p         (Default: T   => garnet fixes loaded)
;;;      load-roylance-clmath-p   (Default: T   => roylance-clmath loaded)
;;;      load-gui-p           (Default: T   => gui loaded)
;;;      load-sys-p           (Default: T   => sys loaded)
;;;      load-parameters-p           (Default: T   => parameters loaded)
;;;      load-hippocampus-p   (Default: T   => hippocampus loaded)
;;;      load-retina-p        (Default: T   => retina loaded)
;;;      load-development-p   (Default: T   => development loaded)
;;;
;;; The first part of this file lists the file names where the various
;;; parts of Surf-Hippo come from.  This will need to be modified for each new
;;; installation of Surf-Hippo.
;;;
;;; To override any particular file name place, it is only necessary to
;;; assign the variable name Surf-Hippo-XX-Pathname before this file is loaded
;;; (since they are defined here using defvar, the old name will stay in
;;; affect).
;;;

;;; This loader file was adapted from garnet-loader.lisp,
;;; part of the Garnet project at CMU.



#|
============================================================
Change log:

============================================================
|#

(if (and (or *clear-sh-packages* ; *force-all-compile*
	     ) (find-package "WH"))
    (do-symbols (foo (find-package "WH")) (unintern foo (find-package "WH")))
  (unless (find-package "WH")
    (make-package "WH"
		  :Use '("GARNET-GADGETS" "LISP" "COMMON-LISP-USER" "COMMON-LISP" "KR")
		  :nicknames '("WINDOWS" "WINDOWS-HACK"))))

(if (and (or *clear-sh-packages* ; *force-all-compile*
	     ) (find-package "PH"))
    (do-symbols (foo (find-package "PH")) (unintern foo (find-package "PH")))
  (unless (find-package "PH")
    (make-package "PH"
		  :Use '("WH" "LISP" "COMMON-LISP-USER" "COMMON-LISP" "KR")
		  :nicknames '("SON-OF-PLOT-HACK" "PLOT" "PLOT-HACK"))))

(if (and (or *clear-sh-packages* ; *force-all-compile*
	     ) (find-package "SURF-HIPPO"))
    (do-symbols (foo (find-package "SURF-HIPPO")) (unintern foo (find-package "SURF-HIPPO")))
  (unless (find-package "SURF-HIPPO")
    (make-package "SURF-HIPPO" :use '("COMMON-LISP-USER" "COMMON-LISP" "KR" "WH" "PH" "LISP" "USER")
		  :nicknames '("SURF" "SH"))))

;; For the attrib.lisp file in /roylance-clmath
(unless (find-package "FS")
  (make-package "FS" :use '("COMMON-LISP-USER" "COMMON-LISP" "LISP" "USER")
		:nicknames '()))



;;; $$$$ CUSTOMIZE $$$$
;; In general, CMUCL GC requires that the swap space be least twice as large as the heap. Also, Lisp
;; starts to thrash when the heap size gets significantly bigger than physical memory (advice from
;; Rob MacLachlan at CMU). For my machine (96MB), Lisp crashes when the heap gets around 41M. Edit
;; the following line according to your machine capacity.

(defvar surf::*gc-bytes-retained-warning-threshold* 40e6)

;;; Uncomment the following line and change the default value shown if you think it will help make
;;; the overall GC process faster.

; (setq ext::*bytes-consed-between-gcs* 2000000)





;;;                            *** Future Feature ***
;;;
;;; The :SURF-HIPPO-DEBUG option allows many different kinds of run-time checking, and also loads
;;; some extra test code.  After you have debugged your code and want it to run faster, remove
;;; :SURF-HIPPO-DEBUG from the *features* list and RECOMPILE all of Surf-Hippo and your code.  The
;;; result will be smaller and somewhat faster.  To remove :SURF-HIPPO-DEBUG from the *features*
;;; list, either defvar Surf-Hippo-Surf-Hippo-Debug to NIL before you load the Surf-Hippo-loader, or
;;; simply comment out the next few lines.
(defvar Surf-Hippo-Debug nil)
(if Surf-Hippo-Debug
    (pushnew :Surf-Hippo-debug *features*)
    (setf *features* (delete :Surf-Hippo-debug *features*)))


;;; load-XX-p control whether the various parts are loaded or not
;;; Because these use defvar, if they are set before this file is
;;; loaded, their original value will be used. Likewise with the
;;; compile-XX-p flags.


(defvar load-garnet-fixes-p t)
(defvar load-roylance-clmath-p t)
(defvar load-gui-p t)
(defvar load-sys-p t)
(defvar load-parameters-p t)
(defvar load-hippocampus-p t)
(defvar load-retina-p t)
(defvar load-surf-hippo-debug-p #+Surf-Hippo-debug T #-Surf-Hippo-debug NIL)
(defvar load-development-p t)

(defvar compile-garnet-fixes-p nil)
(defvar compile-roylance-clmath-p nil)
(defvar compile-gui-p nil)
(defvar compile-sys-p nil)
(defvar compile-parameters-p nil)
(defvar compile-hippocampus-p nil)
(defvar compile-retina-p nil)
(defvar compile-surf-hippo-debug-p nil)
(defvar compile-development-p nil)

(update-main-system-variables)

(defvar Surf-Hippo-Pathnames)
(setq Surf-Hippo-Pathnames
  `(Surf-Hippo-garnet-fixes-PathName
    Surf-Hippo-garnet-fixes-Src
    Surf-Hippo-Roylance-Clmath-PathName
    Surf-Hippo-Roylance-Clmath-Src
    Surf-Hippo-gui-PathName
    Surf-Hippo-gui-Src
    Surf-Hippo-SYS-PathName
    Surf-Hippo-SYS-Src
    Surf-Hippo-PARAMETERS-PathName
    Surf-Hippo-PARAMETERS-Src
    Surf-Hippo-Hippocampus-PathName
    Surf-Hippo-Hippocampus-Src
    Surf-Hippo-Retina-PathName
    Surf-Hippo-Retina-Src
    Surf-Hippo-Debug-PathName
    Surf-Hippo-Debug-Src
    Surf-Hippo-Development-PathName
    Surf-Hippo-Development-Src))

(defun update-surf-hippo-pathnames ()
  (setq
   Surf-Hippo-garnet-fixes-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/garnet-fixes/"))

   Surf-Hippo-garnet-fixes-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/garnet-fixes/"))

   Surf-Hippo-Roylance-Clmath-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/roylance-clmath/"))

   Surf-Hippo-Roylance-Clmath-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/roylance-clmath/"))

   Surf-Hippo-gui-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/gui/"))

   Surf-Hippo-gui-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/gui/"))

   Surf-Hippo-SYS-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/sys/"))

   Surf-Hippo-SYS-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/sys/"))

   Surf-Hippo-PARAMETERS-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/parameters/"))

   Surf-Hippo-PARAMETERS-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/parameters/"))

   Surf-Hippo-Hippocampus-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/hippocampus/"))

   Surf-Hippo-Hippocampus-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/hippocampus/"))

   Surf-Hippo-Retina-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/retina/"))

   Surf-Hippo-Retina-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/retina/"))

   Surf-Hippo-Debug-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/debug/"))

   Surf-Hippo-Debug-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/debug/"))

   Surf-Hippo-Development-PathName (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/bin/development/"))

   Surf-Hippo-Development-Src (fixup-pathname (concatenate 'string Surf-Hippo-Pathname "/src/development/"))))

(eval (cons 'defvars Surf-Hippo-Pathnames))

(update-surf-hippo-pathnames)


(loop for pathname-value in Surf-Hippo-Pathnames do
      ;; Normally, the installed tar file should make all the right directories, but this is for
      ;; those who just install bits and pieces.
      (unix:unix-mkdir (ext:unix-namestring (symbol-value pathname-value) nil) #o777))

;;;----------------------------------------------------------

;;; When compiling, the binaries will be in the same directories as the
;;; source files, so make all the path names be the same
;;;
;;; After compilation is finished, the user should move all the binaries
;;; into their own directories, as specified the the pathnames above.
(defvar *Surf-Hippo-Going-To-Compile*)

;; some misc. stuff -  lbg

(defvar *CONSOLE-OUTPUT t)

;; Avoid underflow traps
(ext:set-floating-point-modes :traps '(:OVERFLOW :INVALID :DIVIDE-BY-ZERO))

(if (string= (cdr (assoc :DISPLAY lisp::*environment-list*)) ":0.0")
    (push (cons :DISPLAY (concatenate 'string (cdr (assoc :HOST lisp::*environment-list*)) ":0.0"))
	  lisp::*environment-list*))

;; To make the debugger not depend on motif junk...
(defvar DEBUG::*IN-WINDOWING-DEBUGGER* t)
(when (member :motif *environment-list*) (setq DEBUG::*IN-WINDOWING-DEBUGGER* t))


#+cmu
(defun prompt-and-read (type prompt)
  (declare (ignore type))
  (format *console-output (string prompt))
  (read))

;; cmucl needs this apparently - this was taken from clx.l lbg 4-25-92
#+cmu
(deftype boolean () '(or null (not null)))

(defun display-message (m) (format *console-output (concatenate 'string m "~%")))

(defun error-message (m &rest args)
  (format *error-output* (concatenate 'string m "~%") args))

#|
(defmacro sim-error (formatted-message-string &rest args)
  "A fatal error occurred, print the message and abort."
  `(progn
    ;;   (declare (ignore args))
    (inter:beep)  (inter:beep)  (inter:beep)
    ;;  (warn (format nil "~%  ~a~%~%  ** Restarting from Top Level **~%" formatted-message-string))
    (format *error-output* "~%  ~a~%~%" ,formatted-message-string)
    (format *error-output* "~% ** Restarting from Top Level **~%~%"))
;    (error (format nil "~% ** Restarting from Top Level **~%"))
    (abort)
    )
|#

(defun sim-error (message)
  "A fatal error occurred, print the message and abort."
  ;;   (declare (ignore args))
  (inter:beep)  (inter:beep)  (inter:beep)
  (format *error-output* "~%~% Fatal Error in Function ~A:~%" (kernel:find-caller-name))
  (format *error-output* "~%  ~a~%" message)
  (format *error-output* "~% ** Restarting from Top Level **~%")
  (abort))



(update-main-system-variables)

;; Directory names in Surf-Hippo end with "/", contrary to the UNIX convention.

(defun init-surf (&optional kill-the-hippo)
  ;; opal:make-image seems to enable the underflow trap
  (sys::set-floating-point-modes :traps '(:OVERFLOW :INVALID :DIVIDE-BY-ZERO)) ;'(:INVALID :DIVIDE-BY-ZERO)
  (in-package "SURF")
  (update-main-system-variables)
  ;; If there is a patches.lisp file in the lib directory, then load it.
  (load (concatenate 'string *Surf-home* "lib/patches.lisp") :if-does-not-exist nil :verbose nil)

  (unless (and (stringp *Surf-user-dir*) (probe-file *Surf-user-dir*))
    (setq *Surf-user-dir*
	  (fixup-pathname
	   (if (assoc :SURFUSERHOME lisp::*environment-list*)
	       (concatenate 'string (cdr (assoc :SURFUSERHOME lisp::*environment-list*)) "/")
	     (concatenate 'string (cdr (assoc :HOME lisp::*environment-list*)) "/surf-hippo/")))))

  (setq *surf-user-home* (fixup-pathname (cdr (assoc :HOME lisp::*environment-list*))))
  (wh::create-path *surf-user-dir*)

  ;; $$$$ CUSTOMIZE $$$$
  ;; If there is a customs.lisp file in the *SURF-USER-DIR* directory, then load it.
  (load (concatenate 'string *Surf-user-dir* "customs.lisp") :if-does-not-exist nil :verbose nil)

  (setq surf::*circuit-directory* *Surf-User-Dir*)
  (wh::create-path surf::*circuit-directory*)
  (setq wh::*data-directory* (concatenate 'string *Surf-User-Dir* "data/"))
  (wh::create-path wh::*data-directory*)
  (setq wh::*plot-directory* (concatenate 'string *Surf-User-Dir* "plot/"))
  (wh::create-path wh::*plot-directory*)
  (setq surf::*use-gc-announce-window* nil) ; problems with save-image version.
  (when (or (assoc :logname lisp::*environment-list*) (assoc :user lisp::*environment-list*))
    (setq surf::*username*
	  (or (cdr (assoc :logname lisp::*environment-list*))
	      (cdr (assoc :user lisp::*environment-list*)))))
  (surf::blanket-initialize kill-the-hippo)
  (wh::encode-time-stamp))


;;; make this accessible from surf package. 4-25-92 lbg
(export '(*CONSOLE-OUTPUT
	  boolean  display-message error-message  prompt-and-read
	  init-surf sim-error))


(defun surf (&rest args)
  (when args (format t "~%SURF function args allowed only after the 2nd invocation in a fresh Lisp.~%"))
  (init-surf t)
  (surf::surf))

(defun start (&optional kill-the-hippo)
  (init-surf kill-the-hippo)
  nil)

(defun help ()
  (load
   (concatenate 'string Surf-Hippo-Pathname "/loaders/initial-message.lisp")))
   
  
#|
(defun help () 
  (init-surf)
  (surf::help))
|#

;;;----------------------------------------------------------

;;; Set up the search lists - we may junk this technique, and just use globals for the pathnames.
#+cmu
(progn
  (setf (ext:search-list "garnet-fixes:")
	(list Surf-Hippo-garnet-fixes-PathName))
  (setf (ext:search-list "garnet-fixes-src:")
	(list Surf-Hippo-garnet-fixes-Src))

  (setf (ext:search-list "roylance-clmath:")
	(list Surf-Hippo-Roylance-Clmath-PathName))
  (setf (ext:search-list "roylance-clmath-src:")
	(list Surf-Hippo-Roylance-Clmath-Src))

  (setf (ext:search-list "gui:")
	(list Surf-Hippo-gui-PathName))
  (setf (ext:search-list "gui-src:")
	(list Surf-Hippo-gui-Src))

  (setf (ext:search-list "sys:")
	(list Surf-Hippo-SYS-PathName))
  (setf (ext:search-list "sys-src:")
	(list Surf-Hippo-SYS-Src))

  (setf (ext:search-list "parameters:")
	(list Surf-Hippo-PARAMETERS-PathName))
  (setf (ext:search-list "parameters-src:")
	(list Surf-Hippo-PARAMETERS-Src))

  (setf (ext:search-list "hippocampus:")
	(list Surf-Hippo-Hippocampus-PathName))
  (setf (ext:search-list "hippocampus-src:")
	(list Surf-Hippo-Hippocampus-Src))

  (setf (ext:search-list "retina:")
	(list Surf-Hippo-Retina-PathName))
  (setf (ext:search-list "retina-src:")
	(list Surf-Hippo-Retina-Src))
  
  (setf (ext:search-list "debug:")
	(list Surf-Hippo-Debug-PathName))
  (setf (ext:search-list "debug-src:")
	(list Surf-Hippo-Debug-Src))


  (setf (ext:search-list "development:")
	(list Surf-Hippo-Development-PathName))
  (setf (ext:search-list "development-src:")
	(list Surf-Hippo-Development-Src))
)



(defparameter Surf-Hippo-garnet-fixes-Loader nil)
(setq Surf-Hippo-garnet-fixes-Loader (merge-pathnames "garnet-fixes-loader"
						      #+cmu "garnet-fixes-src:" ;"garnet-fixes:"
						      #+(not cmu) Surf-Hippo-garnet-fixes-PathName))

(defparameter Surf-Hippo-Roylance-Clmath-Loader nil)
(setq Surf-Hippo-Roylance-Clmath-Loader (merge-pathnames "roylance-clmath-loader"
							 #+cmu "roylance-clmath-src:" ; "roylance-clmath:"
							 #+(not cmu) Surf-Hippo-Roylance-Clmath-PathName))

(defparameter Surf-Hippo-gui-Loader nil)
(setq Surf-Hippo-gui-Loader (merge-pathnames "gui-loader"
					     #+cmu "gui-src:" ;"gui:"
					     #+(not cmu) Surf-Hippo-gui-PathName))

(defparameter Surf-Hippo-SYS-Loader nil)
(setq Surf-Hippo-SYS-Loader (merge-pathnames "sys-loader"
					     #+cmu "sys-src:" ;"sys:"
					     #+(not cmu) Surf-Hippo-SYS-PathName))

(defparameter Surf-Hippo-PARAMETERS-Loader nil)
(setq Surf-Hippo-PARAMETERS-Loader (merge-pathnames "parameters-loader"
						    #+cmu "parameters-src:" ;"parameters:"
						    #+(not cmu) Surf-Hippo-PARAMETERS-PathName))

(defparameter Surf-Hippo-Hippocampus-Loader nil)
(setq Surf-Hippo-Hippocampus-Loader (merge-pathnames "hippocampus-loader"
						     #+cmu "hippocampus-src:" ;"hippocampus:"
						     #+(not cmu) Surf-Hippo-Hippocampus-PathName))

(defparameter Surf-Hippo-Retina-Loader nil)
(setq Surf-Hippo-Retina-Loader (merge-pathnames "retina-loader"
						#+cmu "retina-src:" ; "retina:"
						#+(not cmu) Surf-Hippo-Retina-PathName))

(defparameter Surf-Hippo-Debug-Loader nil)
(setq Surf-Hippo-Debug-Loader (merge-pathnames "debug-loader"
					       #+cmu "debug-src:" ; "debug:"
					       #+(not cmu) Surf-Hippo-Debug-PathName))

(defparameter Surf-Hippo-Development-Loader nil)
(setq Surf-Hippo-Development-Loader (merge-pathnames "development-loader"
						     #+cmu "development-src:" ; "development:"
						     #+(not cmu) Surf-Hippo-Development-PathName))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;            Load The Surf-Hippo System Files
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(format T "** Loading Surf-Hippo Version ~a~%" Surf-Hippo-Version-Number)

(setf *load-verbose* t)

(loop for *enable-surf-hippo-compile* in '(nil t) do
      (setq *force-all-compile* nil)
      (loop for load-module-p in
	    (list load-garnet-fixes-p load-roylance-clmath-p load-gui-p load-sys-p load-parameters-p load-hippocampus-p
		  load-retina-p load-development-p)
	    for module-key in
	    (list :garnet-fixes :roylance-clmath :gui :sys :parameters :hippocampus :retina :development)
	    for module-loader in
	    (list Surf-Hippo-GARNET-FIXES-Loader Surf-Hippo-Roylance-Clmath-Loader Surf-Hippo-GUI-Loader
		  Surf-Hippo-SYS-Loader
		  Surf-Hippo-PARAMETERS-Loader Surf-Hippo-Hippocampus-Loader Surf-Hippo-Retina-Loader
		  Surf-Hippo-Development-Loader)
	    do
	    (if load-module-p
		(if (and *dont-load-modules-twice* (get :Surf-Hippo-modules module-key))
		    (when *enable-surf-hippo-compile*
		      (format T "~%****** ~A-FIXES already loaded *******~%" module-key))
		    (progn
		      (when *enable-surf-hippo-compile*
			(format T "~% %%%%%%%% Loading ~A %%%%%%%%~%" module-key))
		      (load module-Loader)
		      (when *enable-surf-hippo-compile*
			(format T "~% %%%%%%%% Done Loading ~A %%%%%%%%~%" module-key))))
		(when *enable-surf-hippo-compile*
		  (format T "~%****** NOT Loading ~A *******~%" module-key)))))
	
	 
	 




(format t "~%... Surf-Hippo Load Complete ...~%")






#|

;; THESE FUNCTIONS ARE NOT CURRENTLY USED. we also need to consider using the compile file functions
;; defined in src/sys/clmisc.lisp, including the
;;
;;      (with-compilation-unit (:optimize '(optimize (speed 0)))
;;
;; construct.

(defun user::Surf-Hippo-Load (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (prefix (cond
			 ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-PathName)
			 ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-PathName)
			 ((string= head "gui") Surf-Hippo-gui-PathName)
			 ((string= head "sys") Surf-Hippo-SYS-PathName)
			 ((string= head "parameters") Surf-Hippo-PARAMETERS-PathName)
			 ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
			 ((string= head "retina") Surf-Hippo-Retina-PathName)
;			 ((string= head "debug") Surf-Hippo-Debug-PathName)
			 ((string= head "development") Surf-Hippo-Development-PathName)
			 (t (error "Bad prefix ~%" head))))
	       (finalname (merge-pathnames tail prefix)))
	  (format T "Loading ~s~%" finalname)
	  (load finalname))
	;; else no colon, load regular
	(progn
	  (format T "NO COLON, Loading ~s~%" filename)
	  (load filename)))))

;;; 
;;; This function will compile your Surf-Hippo files while keeping the sources and binaries
;;; separated.  If you want to just compile one file from Surf-Hippo, then you could use this
;;; function to compile the source file and automatically save the binary file in the bin directory.
;;;
;;; Example:
;;;    (Surf-Hippo-compile "sys:main") 
;;;    Takes the source file from Surf-Hippo-Sys-Src, compiles it, and
;;;    saves the binary file in Surf-Hippo-Sys-Pathname (the binary
;;;    sys directory).
;;;
(defvar *compiler-extension*
  #+(and cmu sparc)       ".sparcf"
  #+(and cmu (not sparc)) ".fasl")

(defun user::Surf-Hippo-Compile (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (src-prefix (cond
			     ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-Src)
			     ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-Src)
			     ((string= head "gui") Surf-Hippo-gui-Src)
			     ((string= head "sys") Surf-Hippo-sys-Src)
			     ((string= head "parameters") Surf-Hippo-PARAMETERS-Src)
			     ((string= head "hippocampus") Surf-Hippo-Hippocampus-Src)
			     ((string= head "retina") Surf-Hippo-Retina-Src)
			     ((string= head "development") Surf-Hippo-Development-Src)
			     (t (error (concatenate 'string "Bad prefix " head)))))
	       (bin-prefix (cond
			     ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-PathName)
			     ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-PathName)
			     ((string= head "gui") Surf-Hippo-gui-PathName)
			     ((string= head "sys") Surf-Hippo-SYS-PathName)
			     ((string= head "parameters") Surf-Hippo-PARAMETERS-PathName)
			     ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
			     ((string= head "retina") Surf-Hippo-Retina-PathName)
			     ((string= head "development") Surf-Hippo-Development-PathName)
			     (t (error (concatenate 'string "Bad prefix " head)))))
	       (src-finalname (merge-pathnames
			       (concatenate 'string tail ".lisp")
			       src-prefix))
	       (bin-finalname (merge-pathnames
			       (concatenate 'string tail *compiler-extension*)
			       bin-prefix)))
	  (format T "Compiling ~s~%" src-finalname)
	  (format T "for output to ~s~%" bin-finalname)
	  (compile-file src-finalname :output-file bin-finalname))
	;; else no colon, abort
	(error "NO COLON, aborting compile"))))
|#






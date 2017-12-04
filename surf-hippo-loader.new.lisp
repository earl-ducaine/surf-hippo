;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
;;; The Surf-Hippo Neuron Simulator
;;;
;;; This code was written as part of the Surf-Hippo project at the
;;; Center for Biological Information Processing, Department of Brain
;;; and Cognitive Sciences, Massachusetts Institute of Technology, and
;;; currently at the Equipe Cognisciences, Institut Alfred Fessard,
;;; CNRS and has been placed in the public domain.  If you are using
;;; this code or any part of Surf-Hippo, please contact
;;; surf-hippo@ai.mit.edu to be put on the mailing list.
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
;;; To override any particular file name place, it is only necessary
;;; to assign the variable name Surf-Hippo-XX-Pathname before this
;;; file is loaded (since they are defined here using defvar, the old
;;; name will stay in affect).
;;;
;;; This loader file was adapted from garnet-loader.lisp, part of the
;;; Garnet project at CMU.

;;; Future Feature
;;;
;;; The :surf-hippo-debug option allows many different kinds of
;;; run-time checking, and also loads some extra test code.  After you
;;; have debugged your code and want it to run faster, remove
;;; :surf-hippo-debug from the *features* list and recompile all of
;;; Surf-Hippo and your code.  The result will be smaller and somewhat
;;; faster.  To remove :surf-hippo-debug from the *features* list,
;;; either evaluate the following at the Lisp repl:
;;;
;;; (defvar surf-hippo-surf-hippo-debug nil)
;;;
;;; Before you load the surf-hippo-loader, or simply comment out the
;;; next few lines.

(in-package :surf)

;;; eed, it looks as though debug in disabled.  We should enable it if
;;; possible.  Especially given the current instability migrating -->
;;; SBCL and ASDF
(defvar surf-hippo-debug nil)

(if surf-hippo-debug
    (pushnew :surf-hippo-debug *features*)
    (setf *features* (delete :surf-hippo-debug *features*)))

;;; compile/load everything, even debug.
(defparameter load-garnet-fixes-p t)
(defparameter load-roylance-clmath-p t)
(defparameter load-gui-p t)
(defparameter load-sys-p t)
(defparameter load-parameters-p t)
(defparameter load-hippocampus-p t)
(defparameter load-retina-p t)
(defparameter load-surf-hippo-debug-p t)
(defparameter load-development-p t)
(defparameter console-output t)


(defparameter Surf-Hippo-Version-Number "4.1-sub-alpha")

(defun display-message (m)
  (format *console-output (concatenate 'string m "~%")))

(defun error-message (m &rest args)
  (format *error-output* (concatenate 'string m "~%") args))

(defun sim-error (message)
  "A fatal error occurred, print the message and abort."
  (inter:beep)  (inter:beep)  (inter:beep)
  (format *error-output* "~%~% Fatal Error~%")
  (format *error-output* "~%  ~a~%" message)
  (format *error-output* "~% ** Restarting from Top Level **~%")
  (abort))

(defparameter *surf-user-dir* (asdf:system-source-directory :surf-hippo))
(defparameter *surf-user-home* "~/")
(defparameter *circuit-directory* *surf-user-dir*)
(defparameter *username* nil)
(defparameter *surf-home* *surf-user-dir*
  "The pathname for the top-level Surf-Hippo home directory. Set by
   the SURFHOME environment variable, if exists, otherwise by the HOME
   environment variable.")




(defun str (&rest rest)
  (apply #'concatenate `(string ,@rest)))

(defun init-surf (&optional kill-the-hippo)
  ;; If there is a customs.lisp file in the *SURF-USER-DIR* directory,
  ;; then load it.
  (let ((surf-user-custom (str *surf-user-dir* "custom.lisp")))
    (when (probep surf-user-custom)
      (load surf-user-custom)))
  (wh::create-path surf::*circuit-directory*)
  (setq wh::*data-directory* (str *surf-user-dir* "data/"))
  (wh::create-path wh::*data-directory*)
  (setq wh::*plot-directory* (str *surf-user-dir* "plot/"))
  (wh::create-path wh::*plot-directory*)
  (setq surf::*use-gc-announce-window* nil)
  (setq *username* "hippo-user")
  (blanket-initialize kill-the-hippo)
  (wh::encode-time-stamp))


(defun surf (&rest args)
  (when args
    (format t (str "~%surf function args allowed only after "
		   " the 2nd invocation in a fresh Lisp.~%")))
  (init-surf t)
  (surf))

(defun start (&optional kill-the-hippo)
  (init-surf kill-the-hippo)
  nil)

(defun help ()
  (load
   (str Surf-Hippo-Pathname "/loaders/initial-message.lisp")))

;;; Load The Surf-Hippo System Files

(format T "** Loading Surf-Hippo Version ~a~%" Surf-Hippo-Version-Number)

(setf *load-verbose* t)

;; Surf-Hippo-GARNET-FIXES-Loader
;; /home/rett/surf-hippo/surf-hippo-v40/src/roylance-clmath
;; Surf-Hippo-Roylance-Clmath-Loader
;; (load "/home/rett/surf-hippo/surf-hippo-v40/src/roylance-clmath/attrib.lisp")
;;     "gamma"
;;     "bessel"
;;     "horner"
;;     "beta"
;;     "import"
;;     "binomial"
;;     "integr"
;;     "bisection"
;;     "marq"
;;     "combin"
;;     "matrix"
;;     "matrix-double"
;;     "consts"
;;     "mod"
;;     "dft"
;; ;;    "modules"
;;     "ellip"
;;     "poisson"
;;     "erf"
;;     "regres"
;;     "factor"
;;     "falsep"
;;     "fib"
;;     "runge"
;;     "fit"
;;     "statis"
;;     "fmfp"
;;
;;
;; surf-hippo-gui-loader
;;  "macros"
;;  "utilities"
;;  "declare"
;;  "math"
;;  "strings"
;;  "sequences"
;;  "colors"
;;  "linestyles"
;;  ;; "windows-hack_2_9o" ;;
;;  "windows-hack"
;;  "print-windows"
;;  "files"
;;  ;; "menu-hack_2_9o" ;;
;;  "menu-hack"
;;  "file-browser"
;;  "show-image"
;;  "plot-hack-declare"
;;  "virtual-things"
;;  "virtual-line-update"
;;  "virtual-circle-update"
;;  "virtual-polyline-update"
;;  "plot-hack"
;;  "plot-hack-top"
;;  "annotation-file"
;;  "tracer"
;;
;; surf-hippo-sys-loader
;; (list
;;  "macros"
;;  "declare"
;;  "biophysics-declare"
;;  "structures"
;;  "structure-macros"
;;  "models"
;;  "create-models"
;;  "models-2"
;;  "declare-2"
;;  "element-macros"
;;  "element-functions-0"
;;  "element-functions-1"
;;  "element-functions-2"
;;  "math"
;;  "statistics"
;;  "filters"
;;  "fft"
;;  "randoms"
;;  "renewal-process"
;;  "waveforms"
;;  "misc"
;;  "pump-preliminaries"
;;  "conc-int"
;;  "biophysics"
;;  "matrix-system-solver"
;;  "sim"
;;  "circuit-input"
;;  "hines"
;;  "node" "soma"
;;  "segment"
;;  "source"
;;  "isource"
;;  "vsource"
;;  "electrode"
;;  "extracellular-electrode"
;;  "general-membrane-elements"
;;  "channel"
;;  "particle"
;;  "markov-particle"
;;  "conc-part"
;;  "synapse-rf"
;;  "synapse"
;;  "light-synapse-functions"
;;  "synapse-events"
;;  "synapse-evaluation"
;;  "buffer"
;;  "pump"
;;  "axon"
;;  "event-generators"
;;  "cell"
;;  "cable-functions"
;;  "trees"
;;  "print"
;;  "analysis"
;;  "store-plot-data"
;;  "cell-graphics-setup"
;;  "cell-graphics-instances"
;;  "cell-graphics-instances-update-functions"
;;  "cell-graphics-hack-1"
;;  "cell-graphics-hack-2"
;;  "cell-graphics-user-functions"
;;  "ps-object-methods"
;;  "sparse-data"
;;  "colorizing"
;;  "info-hack"
;;  "plot"
;;  "3dplot"
;;  "trace-functions"
;;  "menus"
;;  "data-folder"
;;  "calc-lte-ratio"
;;  "init"
;;  "step"
;;  "hacks"
;;  "update-models"
;;  "raster-plot"
;;  "protocols"
;;  "sample-cells"
;;  "ntscable"
;;  "neurolucida"
;;  "debug"
;;  )
;;
;; 'Surf-Hippo-PARAMETERS-Loader
;;   (list
;;    "isources"
;;    "buffers"
;;    "pumps"
;;    "axons"
;;    "conc-ints"
;;    "syns"
;;    "light-syns"
;;    "bernander-etal-91-syns"
;;    "hodgkin-huxley"
;;    "NEURON-k-chs"
;;    "hippo-TR1161"
;;    "traub91-chs"
;;    "traub94-chs"
;;    "warman94-chs"
;;    "jaffe94-chs"
;;    "migliore95-chs"
;;    "lytton-chs"
;;    "sah-french-etal-na"
;;    "barnes-hille-cone-chs"
;;    "kuo-bean-na"
;;    "vandenberg-bezanilla"
;;    "k-chs"
;;    "working-hpc"
;;    "working-hpc-absolute"
;;    "working-fs"
;;    )
;;
;; 'Surf-Hippo-Hippocampus-Loader
;;   '(
;;     "working-hpc"
;;     "hippos"
;;     "n120-max-red"
;;     )
;;
;; 'Surf-Hippo-Retina-Loader
;;     "star-amacrine"
;;     "star-amacrine-functions"

;; 'Surf-Hippo-Development-Loader



;; THESE FUNCTIONS ARE NOT CURRENTLY USED. we also need to consider using the compile file functions
;; defined in src/sys/clmisc.lisp, including the
;;
;;      (with-compilation-unit (:optimize '(optimize (speed 0)))
;;
;; construct.
;;
;; (defun user::Surf-Hippo-Load (filename)
;;   (let ((pos (position #\: filename)))
;;     (if pos
;; 	(let* ((head (subseq filename 0 pos))
;; 	       (tail (subseq filename (1+ pos)))
;; 	       (prefix (cond
;; 			 ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-PathName)
;; 			 ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-PathName)
;; 			 ((string= head "gui") Surf-Hippo-gui-PathName)
;; 			 ((string= head "sys") Surf-Hippo-SYS-PathName)
;; 			 ((string= head "parameters") Surf-Hippo-PARAMETERS-PathName)
;; 			 ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
;; 			 ((string= head "retina") Surf-Hippo-Retina-PathName)
;; ;			 ((string= head "debug") Surf-Hippo-Debug-PathName)
;; 			 ((string= head "development") Surf-Hippo-Development-PathName)
;; 			 (t (error "Bad prefix ~%" head))))
;; 	       (finalname (merge-pathnames tail prefix)))
;; 	  (format T "Loading ~s~%" finalname)
;; 	  (load finalname))
;; 	;; else no colon, load regular
;; 	(progn
;; 	  (format T "NO COLON, Loading ~s~%" filename)
;; 	  (load filename)))))

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
;; (defvar *compiler-extension*
;;   #+(and cmu sparc)       ".sparcf"
;;   #+(and cmu (not sparc)) ".fasl")
;;
;; (defun user::Surf-Hippo-Compile (filename)
;;   (let ((pos (position #\: filename)))
;;     (if pos
;; 	(let* ((head (subseq filename 0 pos))
;; 	       (tail (subseq filename (1+ pos)))
;; 	       (src-prefix (cond
;; 			     ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-Src)
;; 			     ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-Src)
;; 			     ((string= head "gui") Surf-Hippo-gui-Src)
;; 			     ((string= head "sys") Surf-Hippo-sys-Src)
;; 			     ((string= head "parameters") Surf-Hippo-PARAMETERS-Src)
;; 			     ((string= head "hippocampus") Surf-Hippo-Hippocampus-Src)
;; 			     ((string= head "retina") Surf-Hippo-Retina-Src)
;; 			     ((string= head "development") Surf-Hippo-Development-Src)
;; 			     (t (error (concatenate 'string "Bad prefix " head)))))
;; 	       (bin-prefix (cond
;; 			     ((string= head "garnet-fixes") Surf-Hippo-garnet-fixes-PathName)
;; 			     ((string= head "roylance-clmath") Surf-Hippo-Roylance-Clmath-PathName)
;; 			     ((string= head "gui") Surf-Hippo-gui-PathName)
;; 			     ((string= head "sys") Surf-Hippo-SYS-PathName)
;; 			     ((string= head "parameters") Surf-Hippo-PARAMETERS-PathName)
;; 			     ((string= head "hippocampus") Surf-Hippo-Hippocampus-PathName)
;; 			     ((string= head "retina") Surf-Hippo-Retina-PathName)
;; 			     ((string= head "development") Surf-Hippo-Development-PathName)
;; 			     (t (error (concatenate 'string "Bad prefix " head)))))
;; 	       (src-finalname (merge-pathnames
;; 			       (concatenate 'string tail ".lisp")
;; 			       src-prefix))
;; 	       (bin-finalname (merge-pathnames
;; 			       (concatenate 'string tail *compiler-extension*)
;; 			       bin-prefix)))
;; 	  (format T "Compiling ~s~%" src-finalname)
;; 	  (format T "for output to ~s~%" bin-finalname)
;; 	  (compile-file src-finalname :output-file bin-finalname))
;; 	;; else no colon, abort
;; 	(error "NO COLON, aborting compile"))))






(defpackage :roylance-clmath
  (:use common-lisp))

(defpackage :surf-hippo-ext
  (:use common-lisp)
  (:export quit))

(defpackage :wh
  (:use garnet-gadgets common-lisp kr)
  (:nicknames windows
	      windows-hack))

(defpackage :ph
  (:use common-lisp wh kr)
  (:nicknames son-of-plot-hack
	      plot
	      plot-hack))

(defpackage :surf-hippo
  (:use common-lisp kr wh ph)
  (:nicknames surf sh)
  (:export *console-output
	   boolean
	   display-message
	   error-message
	   prompt-and-read
	   init-surf
	   sim-error))

(defpackage :surf-hippo-user
  (:use common-lisp surf-hippo)
  ;;; dubiously good idea to have a package named 'user'
  (:nicknames user)
  (:export *console-output
	   boolean
	   display-message
	   error-message
	   prompt-and-read
	   init-surf
	   sim-error))

(defpackage :fs
  (:use common-lisp))

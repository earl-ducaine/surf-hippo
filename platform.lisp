


;; Misc. platform dependant code that doesn't have maintained
;; compatibility wrapper libraries

(in-package :surf-hippo-ext)

;; This group from "clocc-port/ext.lisp"
(defun quit (&optional code)
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      ;; XXX Or is it LISP::QUIT?
      #+gcl (lisp:bye code)
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:quit
              :unix-code (typecase code
			   (number code)
			   (null 0)
			   (t 1)))
      ;; This group from Maxima
      ;; XXX Does this take an arg?
      #+kcl (lisp::bye)
      ;; XXX Pretty sure this *does*.
      #+scl (ext:quit code)
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; XXX Does this take an arg?
      #+poplog (poplog::bye)
      ;; This group from <heb...@math.uni.wroc.pl>
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code)))



;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :org.xoanonos
  :depends-on (:clx ;; :zpb-ttf
		    :cl-vectors ;; :cl-paths-ttf
		    :cl-aa :cl-fad
		    :cl-store :trivial-features :cl-aa-misc :alexandria :cl-ppcre)
  :components
  ((:file "package")
   (:module utils
	    :pathname ""
	    :depends-on (:package)

	    :components
	    ((:file "garnet-loader"
		    )
	     (:file "src/utils/general")
	     (:file "src/utils/global")))
   (:module kr
   	    :pathname "src/kr"
   	    :depends-on (:utils)
   	    :components
	    ((:file "kr-macros")
	     (:file "kr-doc")
	     (:file "kr")
	     (:file "constraints")))))

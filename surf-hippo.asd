

;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

;; asdf:defsystem style guide:
;;
;; - Everything, filenames, modules, package names, etc, should be
;;   lower-case and hyphen separated unless there is a reason for it
;;   to be otherwise, e.g. to emphasis some sources are C++ or to obey
;;   Java conventions..
;;
;; - defsystem names should be keywords unless using a system where
;;   keywords cause problems.  #:my-symbol should be avoided as being
;;   barbaric!
;;
;; - all other places where where asdf:defsystem allows a choice,
;;   symbols should be prefered over keywords to empasis the defsystem
;;   syntax. E.g.,
;;
;;   :depends-on (package platform gui roylance-clmath)
;;
;;   rather than,
;;
;;   :depends-on (package :platform :gui :roylance-clmath)
;;
;; - when lists of longer than one line (75 characters) exist, break
;;   them into multiple lines, one element per line, e.g.,
;;
;;   ...
;;   :components
;;   ((:file "attrib")
;;    (:file "gamma")
;;    (:file "bessel")
;;    (:file "horner")
;;    (:file "beta")
;;    (:file "import")
;;    ...
;;
;;    preferably sorting the elements alphabetecally and using
;;    :depends-on to enforce order of processing. (But that's not
;;    always practicle.)

(asdf:defsystem :surf-hippo
  :depends-on (:clx ;; :zpb-ttf
	       :cl-vectors ;; :cl-paths-ttf
	       :cl-aa :cl-fad
	       :cl-store :trivial-features :cl-aa-misc :alexandria :cl-ppcre :org.xoanonos.gui.garnet)
  :components
  ((:module init-files
   	    :pathname "./"
   	    :components
	    ((:file "package")
	     (:file "platform" :depends-on (package))
	     (:file "surf-hippo-loader.new" :depends-on (platform))))
   (:module roylance-clmath
   	    :pathname "src/roylance-clmath"
   	    :depends-on (init-files)
   	    :components
   	    ((:file "attrib")
	     (:file "gamma")
	     (:file "bessel")
	     (:file "horner")
	     (:file "beta")
	     (:file "import")
	     (:file "binomial")
	     (:file "integr")
	     (:file "bisection")
	     (:file "marq")
	     (:file "combin")
	     (:file "matrix")
	     (:file "matrix-double")
	     (:file "consts")
	     (:file "mod")
	     (:file "dft")
	     (:file "ellip")
	     (:file "poisson")
	     (:file "erf")
	     (:file "regres")
	     (:file "factor")
	     (:file "falsep")
	     (:file "fib")
	     (:file "runge")
	     (:file "fit")
	     (:file "statis")
	     (:file "fmfp")))
   (:module gui
   	    :pathname "src/gui"
   	    :depends-on (init-files)
   	    :components
   	    ((:file "macros")
	     (:file "utilities")
	     (:file "declare")
	     (:file "math")
	     (:file "strings")
	     (:file "sequences")
	     (:file "colors")
	     ;; window-hack.lisp
	     (:file "linestyles")
	     ;; "windows-hack_2_9o" ;;
	     ;; Depends on colors.lisp?
	     (:file "windows-hack")
	     (:file "print-windows")
	     (:file "files")
	     ;; "menu-hack_2_9o" ;;
	     (:file "menu-hack")
	     (:file "file-browser")
	     (:file "show-image")
	     (:file "plot-hack-declare")
	     (:file "virtual-things")
	     (:file "virtual-line-update")
	     (:file "virtual-circle-update")
	     (:file "virtual-polyline-update")
	     (:file "plot-hack")
	     (:file "plot-hack-top")
	     (:file "annotation-file")
	     (:file "tracer")))
   (:module sys
   	    :pathname "src/sys"
   	    :depends-on (init-files gui roylance-clmath)
   	    :components
   	    ( (:file "macros")
	      (:file "declare")
	      (:file "biophysics-declare")
	      (:file "structures")
	      (:file "structure-macros")
	      (:file "models")
	      (:file "create-models")
	      (:file "models-2")
	      (:file "declare-2")
	      (:file "element-macros")
	      (:file "element-functions-0")
	      (:file "element-functions-1" :depends-on (cell segment electrode))
	      (:file "element-functions-2")
	      (:file "math")
	      (:file "statistics")
	      (:file "filters")
	      (:file "fft")
	      (:file "randoms")
	      (:file "renewal-process")
	      (:file "waveforms")
	      (:file "misc")
	      (:file "pump-preliminaries")
	      (:file "conc-int")
	      (:file "biophysics")
	      (:file "matrix-system-solver")
	      (:file "sim")
	      (:file "circuit-input")
	      (:file "hines")
	      (:file "node")
	      (:file "soma")
	      (:file "segment")
	      (:file "source")
	      (:file "isource")
	      (:file "vsource")
	      (:file "electrode")
	      (:file "extracellular-electrode")
	      (:file "general-membrane-elements")
	      (:file "channel")
	      (:file "particle")
	      (:file "markov-particle")
	      (:file "conc-part")
	      (:file "synapse-rf")
	      (:file "synapse")
	      (:file "light-synapse-functions")
	      (:file "synapse-events")
	      (:file "synapse-evaluation")
	      (:file "buffer")
	      (:file "pump")
	      (:file "axon")
	      (:file "event-generators")
	      (:file "cell")
	      (:file "cable-functions")
	      (:file "trees")
	      (:file "print")
	      (:file "analysis")
	      (:file "store-plot-data")
	      (:file "cell-graphics-setup")
	      (:file "cell-graphics-instances")
	      (:file "cell-graphics-instances-update-functions")
	      (:file "cell-graphics-hack-1")
	      (:file "cell-graphics-hack-2")
	      (:file "cell-graphics-user-functions")
	      (:file "ps-object-methods")
	      (:file "sparse-data")
	      (:file "colorizing")
	      (:file "info-hack")
	      (:file "plot")
	      (:file "3dplot")
	      (:file "trace-functions")
	      (:file "menus")
	      (:file "data-folder")
	      (:file "calc-lte-ratio")
	      (:file "init")
	      (:file "step")
	      (:file "hacks")
	      (:file "update-models")
	      (:file "raster-plot")
	      (:file "protocols")
	      (:file "sample-cells")
	      (:file "ntscable")
	      (:file "neurolucida")
	      (:file "debug")))
   (:module parameters
   	    :pathname "src/parameters"
   	    :depends-on (sys)
   	    :components
   	    ((:file "isources")
	     (:file "buffers")
	     (:file "pumps")
	     (:file "axons")
	     (:file "conc-ints")
	     (:file "syns")
	     (:file "light-syns")
	     (:file "bernander-etal-91-syns")
	     (:file "hodgkin-huxley")
	     (:file "NEURON-k-chs")
	     (:file "hippo-TR1161")
	     (:file "traub91-chs")
	     (:file "traub94-chs")
	     (:file "warman94-chs")
	     (:file "jaffe94-chs")
	     (:file "migliore95-chs")
	     (:file "lytton-chs")
	     (:file "sah-french-etal-na")
	     (:file "barnes-hille-cone-chs")
	     (:file "kuo-bean-na")
	     (:file "vandenberg-bezanilla")
	     (:file "k-chs")
	     (:file "working-hpc")
	     (:file "working-hpc-absolute")
	     (:file "working-fs")))
   (:module hippocampus
   	    :pathname "src/hippocampus"
   	    :depends-on (parameters)
   	    :components
   	    ((:file "working-hpc")
	     (:file "hippos")
	     (:file "n120-max-red")))
   (:module retina
   	    :pathname "src/retina"
   	    :depends-on (parameters)
   	    :components
   	    ((:file "star-amacrine")
	     (:file "star-amacrine-functions")))
   (:module garnet-fixes
   	    :pathname "src/garnet-fixes"
   	    :depends-on (parameters)
   	    :components
   	    (
;;	     (:file "x")
	     (:file "defs")
	     (:file "new-macros")
	     (:file "new-defs-hack")
	     (:file "gem-hack")
	     (:file "virtual-aggregates-hack")
	     (:file "make-ps-file-hack")
	     (:file "virtual-aggregate-w-aggregadgets-fix")
	     (:file "register-fns")
	     (:file "garnet-pathnames")
	     (:file "utils")
	     (:file "opal-hack")))
   ))


   ;; (:file "x")
   ;; (:file "defs")
   ;; (:file "new-macros")
   ;; (:file "new-defs-hack")
   ;; (:file "gem-hack")
   ;; (:file "virtual-aggregates-hack")
   ;; (:file "make-ps-file-hack")
   ;; (:file "virtual-aggregate-w-aggregadgets-fix")
   ;; (:file "register-fns")
   ;; (:file "garnet-pathnames")
   ;; (:file "utils")
   ;; (:file "opal-hack")



;; (:module kr
   ;; 	    :pathname "src/kr"
   ;; 	    :depends-on (:utils)
   ;; 	    :components
   ;; 	    ((:file "kr-macros")
   ;; 	     (:file "kr-doc")
   ;; 	     (:file "kr")
   ;; 	     (:file "constraints")))))

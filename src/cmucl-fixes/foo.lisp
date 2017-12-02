;(plot-timed-data '(1 2))
;(lock-windows)
;(plot-timed-data '(1 2))
(plot-scatter (loop for x from 1 to 10000 collect (list (random 1.0) (random 1.0)))) ; nope
(plot-scatter (loop for x from 1 to 1000 collect (list (random 1.0) (random 1.0))))  ; yep
(plot-scatter (loop for x from 1 to 5000 collect (list (random 1.0) (random 1.0))))  ; yep

(trace OPAL::UPDATE-METHOD-AGGREGATE :break t)

(COMMON-LISP-USER::RANDOM-DOTS-BASIC-WIN 500)

(profile::profile OPAL::UPDATE-METHOD-AGGREGATE)
(progn
  (Defvar Surf-Hippo-Sys-Files   (list
   "macros"
   "declare"				; Most of the global variables.
   "biophysics-declare"			; Variable values associated with reality, not the code.
   "structures"				; All of the structure definitions and some slot macros
   "structure-macros"
   "models"
   "init"
   "structure-data-functions"
   "math"				; Some misc math functions
   "some-statistics"
   "filters"
   "fft"
   "randoms"
   "renewal-process"
   "waveforms"				; Must be before synapse.lisp
   ;;  manipulating 500 dot win - ok up to here

   "misc"
   "debug"
   "biophysics-conc-int"
   "pump-preliminaries"			; Need inlined PUMP-CONCENTRATION-CURRENT for conc-ints.
   "conc-int"
   "biophysics"
   "matrix-system-solver"
   "sim"  "circuit-input"  "hines"
   "node" "soma" "segment"
   "source" "isource" "vsource"
   "electrode" "extracellular-electrode"
   "general-membrane-elements"
   "channel"				; some inline functions here are also used in synapse.lisp
   "particle"				; some inline functions here are used in conc-part.lisp
   "markov-particle"
   "conc-part"				
   "synapse-rf"				; NG code
   "reduced-synapse"
   "synapse"
   "light-synapse-functions"
   "synapse-events"
   "synapse-evaluation"
   "buffer" "pump" "axon"
   "event-generators"			; applies to (at least) synapses and axons
   "cell"
   "cable_functions"   "element_functions"   "trees"
   "print"   "analysis"   "store-plot-data"

   ;;  manipulating 500 dot win - ok up to here

   "histology-xfrms"   "histology-hack"
   "sparse-data"
   "colorizing"
   "cell-graphics-setup"
   "cell-graphics-virtual-schema"
   "cell-graphics-grapes"
   "cell-graphics"
   "user-cell-graphics"
      ;;  manipulating 500 dot win - ok up to here


;;   #-GARNET-V3.0 "virtual-aggregate-update-method"

   "info-hack"   "plot"   "3dplot"   "trace-functions"
   "menus"   "data-folder"
   "calc-lte-ratio"	   "step"
      ;;  manipulating 500 dot win - ok up to here
   
   "hacks"			; Also includes some functions that refer to earlier macros.

   ;; is this it?
   ;(setq ext:*gc-notify-after* #'ANNOUNCE-GC-done)
   
   ;(setq ext:*gc-notify-before* #'ANNOUNCE-GC)
   
   #|
  "raster-plot"   "protocols"   "sample-cells"   "ntscable"   "neurolucida"
|#

   ))
  ;; manipulating 500 dot win -ok with nothing
;  (defvar load-cmucl-fixes-p nil) ; manipulating 500 dot win - ok
;  (defvar load-garnet-fixes-p nil) ; manipulating 500 dot win - ok
;  (defvar load-roylance-clmath-p nil) ; manipulating 500 dot win - ok
;  (defvar load-gui-p nil) ; manipulating 500 dot win - ok
;  (defvar load-sys-p nil) ; manipulating 500 dot win - this chokes
  (defvar load-parameters-p nil)
  (defvar load-hippocampus-p nil)
  (defvar load-rabbit-p nil)
  (defvar load-surf-hippo-debug-p nil)
  (defvar load-development-p nil))



;; this seems to work ok for at least 10,000.
(defun random-dots-basic-win (total-dots)
  (let* ((win (create-instance nil inter:interactor-window (:width 200) (:height 200)))
	 (agg (s-value win :aggregate (create-instance nil opal:aggregate))))
    (dotimes (x total-dots)
      (opal:add-component agg
			  (create-instance nil opal:circle (:diameter 4) (:width 4) (:height 4)
					   (:left (round (* 200 (random 1.0))))
					   (:top (round (* 200 (random 1.0)))))))
    (opal:update win)))
  

(defun random-dots-plot-win (total-dots)
  (let* ((win (ph::get-plot-window 'test :xy nil :width 200 :height 200))
	 (agg (s-value win :aggregate (create-instance nil opal:aggregate))))
    (dotimes (x total-dots)
      (opal:add-component agg
			  (create-instance nil opal:circle (:diameter 4) (:width 4) (:height 4)
					   (:left (round (* 200 (random 1.0))))
					   (:top (round (* 200 (random 1.0)))))))
    (s-value win :visible t)
    (opal:update win)))
  


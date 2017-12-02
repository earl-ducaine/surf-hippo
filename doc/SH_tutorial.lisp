;; This file contains forms that are referenced in the tutorial (SH_tutorial.pdf).
;; Each form delineated by parentheses, or variables (e.g. *cell*) can be directly
;; submitted to Lisp by cutting and pasting to the Lisp prompt and hitting the
;; RETURN or ENTER key.


(+ 1 1 2)


(surf)


(circuit-load)


(create-cell "Simple" :cell-type 'cortical :soma-diameter 30)


(element "Simple")


(element 'cortical)


(create-cell "Simple" :cell-type 'cortical :soma-diameter 30)


*cell*


(erase-element *cell*)


*cell*


(print-element "Simple")


(create-element *soma* 'na-hh-ext 'dr-hh-ext)


(element-type "Simple-soma-DR-HH-EXT")


(edit-element (element-type "Simple-soma-DR-HH-EXT"))


(channels)


(particle-types)


(enable-element-plot (channels))


(print-element 'particle-type)


(describe 'pulse-list)


(circuit-load "circuits/demos/hh-demo.lisp")


(print-element *cell*)


(element-parameter 'na-hh-ext 'color 'red)


(element-parameter 'dr-hh-ext 'color 'blue)


(just-draw :scale 5 :mark-elements :all :mark-all-nodes t)


(run)


(let ((*overlay-plots* nil)             ; First simulation clears plots.
      (*accomodate-overlays* t))        ; Everything can fit.
  ;; Iterate using a local variable CURRENT.
  (loop for current from 0 to 1 by 0.2 do
       (add-pulse-list *isource* (list 10 20 current))   ; 10 to 20ms pulse
       (run)                                             ; Run simulation
       (setq *overlay-plots* t)))   ; Subsequent simulations overlay on each other.


(caows)



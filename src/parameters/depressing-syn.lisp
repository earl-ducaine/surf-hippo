;; Markram synapse dynamics


(synapse-type-def
 `(basic-depressing
   (parent-type . auto-fast-ex-double-exp-abs)
   (1st-order-depressing-dynamics . t)
   (tau-recovery . 800)			;ms
   (release-fraction . 0.2)))










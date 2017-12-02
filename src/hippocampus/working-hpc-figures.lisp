(progn 
  (topload 'working-hpc-test)
  (enable-element-plot 'channel)
  (enable-element-plot 'conc-int :all)
  (pulse-list *isource* '(1 2.5 1))
  (setq *user-stop-time* 10))


;; Figure 12

(defun foo ()
  (let ((*user-stop-time* 2000)
	(*create-new-simulation-plots* t)
	(*OVERLAY-ALL-PLOTS* nil)
	(*accomodate-overlays* t))
    (topload 'working-hpc-test)
    (enable-element-plot 'channel)
    (enable-element-plot 'conc-int :all)))
    
(loop for mag from -0.6 to 0.2 by 0.2
	  do (pulse-list *isource* (list 100 1100 mag))
	  (goferit)
	  (setq *create-new-simulation-plots* nil
		*OVERLAY-ALL-PLOTS* t))))

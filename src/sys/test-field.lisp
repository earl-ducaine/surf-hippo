(circuit-load "circuits/demos/lots-of-hippos.lisp")
(create-extracellular-electrode 'field-probe 0 0 3e3)
(enable-element-plot 'field-probe)
(mapcar 'add-working-hpc-channels (somas))

(let ((min-x+y (apply 'min (mapcar #'(lambda (cell) (+ (where-x cell) (where-y cell))) (cells)))))
      (loop for cell in (cells) do 
          (let ((start (- (+ (where-x cell) (where-y cell)) min-x+y))) (add-isource cell :pulse-list (list start (+ start 10) 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(circuit-load 'working-hpc-test)
(create-extracellular-electrode 'test 0 0 0)
(enable-element-plot (element 'test))
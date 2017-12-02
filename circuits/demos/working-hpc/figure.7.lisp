
(in-package "SURF-HIPPO")

#|

This file reproduces traces in Figure 7 of:

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 13, Cortical
Models", edited by P.S. Ulinski, E.G. Jones and A. Peters, Plenum Press, 1998.

This file can be loaded directly into Surf-Hippo for demonstration.

|#


(circuit-load 'working-hpc)
(setq *user-stop-time* 10)

(std-setup)				; Plot soma voltage and add soma current source.
(pulse-list *isource* '(1 2.5 0.9))	; subthreshold 0.9nA from 1 to 2.5 ms.
(run)

(pulse-list *isource* '(1 2.5 1))	; 1nA from 1 to 2.5 ms.

(enable-element-plot '(CA-T-HPC CA-N-HPC CA-L-HPC KA-HPC KCT-HPC KDR-HPC NA-HPC))
(enable-element-plot 'soma 'DENDRITE-CURRENT) ; Plot the soma-dendrite current.
(enable-element-plot 'conc-int '(1 total)) ; Plot shell 1 and the total (averaged) concentration.

(let ((*OVERLAY-PLOTS* t)
      (*ACCOMODATE-OVERLAYS* t)
      (*traces-per-plot* 0)		; Normally, this is 6 - when set to 0 there is no limit.
      (*plot-channels-by-major-ion* nil) ; So all currents are on same plot.
      (*plot-total-concs-separately* nil)) ; Likewise, plot shell 1 and total concentrations together. 
  (run))


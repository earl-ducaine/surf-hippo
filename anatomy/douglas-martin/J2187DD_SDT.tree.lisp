;;; -*- Package: SURF; Mode: LISP -*-

#|

 *******  Dump Tree for Cell ntscable  ******* 


    Cell originally created from j2187dd_sdt.lisp


************************************
 Thursday, 9/18/97 07:58:00 pm EDT   User: lyle
************************************
Simulation 'j2187dd_sdt-9731588' ** Loaded circuit not simulated yet **
[File: /home/lyle/surf-hippo/anatomy/douglas-martin/j2187dd_sdt.lisp]

Trees consolidated with a max electrotonic length of 1.5
1 cell type, 1 cell, 79 nodes. Temperature 27.0 degrees(C). 1.0 ms simulation.
There is 1 soma, and 78 segments.

  Cell-type ntscable:
   Rm 40000.0, Rm-sm 40000.0 (ohm-cm2), Ra 200.0 ohm-cm, Cm-sm 0.7, Cm-den 0.7 (uF/cm2) 
   E-soma-leak -70.0 mV, E-dendrite-leak -70.0 mV 
   E-Na -95.00 mV, E-K -95.00 mV, E-Ca 110.00 mV, E-Cl -90.00 mV

  Cell ntscable (soma @ [0.0 0.0 0.0])  -  Created from j2187dd_sdt.lisp
      78 Segments, 37 Branch points, 4 Trunks, 41 Terminals, Total Membrane Area 6.30e+3um^2
      Passive R-in from soma (actual / cable model) = 727.87 / 694.70 Mohms 
       Soma passive R-in = 4061.44 Mohms
       Dendritic tree passive R-in (actual / cable model) = 886.80 / 838.05 Mohms
       Coupling R's from individual compartments [single-leg]


|#

(defun j2187dd_sdt ()
  (defvar NTSCABLE-CELL-TREE)
  (setq NTSCABLE-CELL-TREE 
	`(
	  ( "3-4-2^3-4-3" 
	   "3-5-2^3-5-67" 
	   33.7 
	   24.1 
	   -89.8 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.9 
	      )
	    )
	   )
    
	  ( "3-4-2^3-4-3" 
	   "3-4-4^3-4-96" 
	   -14.9 
	   72.1 
	   -57.4 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      9.4 
	      )
	    )
	   )
    
	  ( "3-3-8^3-3-10" 
	   "3-4-2^3-4-3" 
	   -0.3 
	   -0.5 
	   -41.2 
	   0.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      2.5 
	      )
	    )
	   )
    
	  ( "3-3-8^3-3-10" 
	   "3-3-11^3-3-126" 
	   -69.9 
	   -33.1 
	   -51.8 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      9.0 
	      )
	    )
	   )
    
	  ( "3-3-2^3-3-7" 
	   "3-3-8^3-3-10" 
	   0.1 
	   -1.9 
	   -40.0 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.0 
	      )
	    )
	   )
    
	  ( "3-3-2^3-3-7" 
	   "3-6-2^3-6-110" 
	   -10.1 
	   -93.5 
	   -50.4 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.8 
	      )
	    )
	   )
    
	  ( "3-1-2^3-1-4" 
	   "3-3-2^3-3-7" 
	   1.9 
	   -2.9 
	   -32.0 
	   1.1 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.0 
	      )
	    )
	   )
    
	  ( "3-1-5^3-1-19" 
	   "3-1-21^3-1-34" 
	   13.3 
	   -33.5 
	   -41.4 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.5 
	      )
	    )
	   )
    
	  ( "3-1-5^3-1-19" 
	   "3-2-2^3-2-93" 
	   26.5 
	   -19.9 
	   -119.0 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.9 
	      )
	    )
	   )
    
	  ( "3-1-2^3-1-4" 
	   "3-1-5^3-1-19" 
	   9.1 
	   -21.9 
	   -33.6 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.7 
	      )
	    )
	   )
    
	  ( SOMA 
	   "3-1-2^3-1-4" 
	   1.9 
	   -8.5 
	   -14.8 
	   0.8 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      2.0 
	      )
	    )
	   )
    
	  ( "1-3-8^1-3-57" 
	   "1-4-2^1-4-53" 
	   -25.7 
	   130.1 
	   -57.0 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.2 
	      )
	    )
	   )
    
	  ( "1-3-8^1-3-57" 
	   "1-3-59^1-3-89" 
	   -33.1 
	   100.3 
	   -74.6 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.4 
	      )
	    )
	   )
    
	  ( "1-3-2^1-3-7" 
	   "1-3-8^1-3-57" 
	   -24.9 
	   78.5 
	   -54.2 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.6 
	      )
	    )
	   )
    
	  ( "1-5-36^1-5-44" 
	   "1-6-2^1-6-24" 
	   -51.9 
	   63.1 
	   -82.6 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.8 
	      )
	    )
	   )
    
	  ( "1-5-36^1-5-44" 
	   "1-5-45^1-5-91" 
	   -41.7 
	   66.1 
	   -144.2 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.8 
	      )
	    )
	   )
    
	  ( "1-5-2^1-5-35" 
	   "1-5-36^1-5-44" 
	   -39.7 
	   46.7 
	   -94.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.4 
	      )
	    )
	   )
    
	  ( "1-5-2^1-5-35" 
	   "1-7-2^1-7-44" 
	   -43.1 
	   46.1 
	   -143.2 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.7 
	      )
	    )
	   )
    
	  ( "1-3-2^1-3-7" 
	   "1-5-2^1-5-35" 
	   -38.5 
	   37.9 
	   -92.0 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.2 
	      )
	    )
	   )
    
	  ( "1-2-2^1-2-7" 
	   "1-3-2^1-3-7" 
	   -20.3 
	   25.5 
	   -43.8 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.6 
	      )
	    )
	   )
    
	  ( "1-2-2^1-2-7" 
	   "1-2-8^1-2-136" 
	   -79.7 
	   -1.3 
	   -93.6 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      11.0 
	      )
	    )
	   )
    
	  ( "1-1-9^1-1-12" 
	   "1-2-2^1-2-7" 
	   -19.7 
	   17.1 
	   -19.6 
	   1.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.3 
	      )
	    )
	   )
    
	  ( "1-1-9^1-1-12" 
	   "1-1-13^1-1-58" 
	   -37.1 
	   -9.3 
	   0.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.4 
	      )
	    )
	   )
    
	  ( "1-1-2^1-1-8" 
	   "1-1-9^1-1-12" 
	   -15.5 
	   17.5 
	   -9.2 
	   1.8 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.6 
	      )
	    )
	   )
    
	  ( "1-8-8^1-8-20" 
	   "1-8-21^1-8-45" 
	   -7.9 
	   20.9 
	   -9.0 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.0 
	      )
	    )
	   )
    
	  ( "1-8-8^1-8-20" 
	   "1-9-2^1-9-52" 
	   -21.3 
	   73.7 
	   20.2 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      8.0 
	      )
	    )
	   )
    
	  ( "1-8-2^1-8-7" 
	   "1-8-8^1-8-20" 
	   -10.5 
	   31.5 
	   6.6 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      8.0 
	      )
	    )
	   )
    
	  ( "1-10-12^1-10-23" 
	   "1-10-25^1-10-69" 
	   -63.1 
	   65.7 
	   36.2 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.2 
	      )
	    )
	   )
    
	  ( "1-11-36^1-11-62" 
	   "1-11-63^1-11-80" 
	   -73.3 
	   105.9 
	   79.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.5 
	      )
	    )
	   )
    
	  ( "1-11-36^1-11-62" 
	   "1-12-2^1-12-50" 
	   -88.1 
	   93.7 
	   143.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.7 
	      )
	    )
	   )
    
	  ( "1-11-28^1-11-34" 
	   "1-11-36^1-11-62" 
	   -66.7 
	   90.7 
	   83.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.8 
	      )
	    )
	   )
    
	  ( "1-11-28^1-11-34" 
	   "1-13-2^1-13-17" 
	   -46.3 
	   92.9 
	   68.2 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.4 
	      )
	    )
	   )
    
	  ( "1-11-2^1-11-26" 
	   "1-11-28^1-11-34" 
	   -43.9 
	   77.9 
	   70.0 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      8.0 
	      )
	    )
	   )
    
	  ( "1-11-2^1-11-26" 
	   "1-14-2^1-14-92" 
	   -1.5 
	   111.7 
	   154.4 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.4 
	      )
	    )
	   )
    
	  ( "1-10-12^1-10-23" 
	   "1-11-2^1-11-26" 
	   -39.3 
	   74.9 
	   68.8 
	   1.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      8.3 
	      )
	    )
	   )
    
	  ( "1-10-2^1-10-10" 
	   "1-10-12^1-10-23" 
	   -31.5 
	   46.7 
	   35.4 
	   1.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.4 
	      )
	    )
	   )
    
	  ( "1-10-2^1-10-10" 
	   "1-15-2^1-15-23" 
	   -40.9 
	   37.5 
	   -2.0 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.1 
	      )
	    )
	   )
    
	  ( "1-8-2^1-8-7" 
	   "1-10-2^1-10-10" 
	   -25.3 
	   34.1 
	   11.2 
	   1.2 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      2.4 
	      )
	    )
	   )
    
	  ( "1-1-2^1-1-8" 
	   "1-8-2^1-8-7" 
	   -17.3 
	   23.9 
	   2.8 
	   1.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      2.0 
	      )
	    )
	   )
    
	  ( SOMA 
	   "1-1-2^1-1-8" 
	   -12.3 
	   16.3 
	   -4.0 
	   2.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.4 
	      )
	    )
	   )
    
	  ( "4-2-2^4-2-26" 
	   "4-3-2^4-3-11" 
	   26.7 
	   -40.7 
	   19.6 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.7 
	      )
	    )
	   )
    
	  ( "4-2-2^4-2-26" 
	   "4-2-27^4-2-65" 
	   51.5 
	   -32.7 
	   18.8 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.7 
	      )
	    )
	   )
    
	  ( "4-1-6^4-1-25" 
	   "4-2-2^4-2-26" 
	   24.3 
	   -33.5 
	   23.2 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      8.3 
	      )
	    )
	   )
    
	  ( "4-1-6^4-1-25" 
	   "4-1-27^4-1-92" 
	   34.3 
	   -83.7 
	   -33.2 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.8 
	      )
	    )
	   )
    
	  ( "4-1-2^4-1-5" 
	   "4-1-6^4-1-25" 
	   4.5 
	   -25.3 
	   16.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.5 
	      )
	    )
	   )
    
	  ( "4-4-41^4-4-45" 
	   "4-4-46^4-4-71" 
	   6.9 
	   41.7 
	   75.0 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.7 
	      )
	    )
	   )
    
	  ( "4-4-41^4-4-45" 
	   "4-5-2^4-5-100" 
	   35.7 
	   82.5 
	   14.0 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      13.5 
	      )
	    )
	   )
    
	  ( "4-4-8^4-4-40" 
	   "4-4-41^4-4-45" 
	   10.7 
	   27.1 
	   52.4 
	   0.8 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      10.1 
	      )
	    )
	   )
    
	  ( "4-4-8^4-4-40" 
	   "4-6-2^4-6-40" 
	   19.3 
	   16.9 
	   97.2 
	   0.4 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.2 
	      )
	    )
	   )
    
	  ( "4-4-6^4-4-7" 
	   "4-4-8^4-4-40" 
	   10.5 
	   24.1 
	   51.6 
	   1.0 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      11.8 
	      )
	    )
	   )
    
	  ( "4-4-6^4-4-7" 
	   "4-7-2^4-7-55" 
	   -15.3 
	   32.5 
	   74.0 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.3 
	      )
	    )
	   )
    
	  ( "4-4-2^4-4-5" 
	   "4-4-6^4-4-7" 
	   4.9 
	   0.1 
	   29.0 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.1 
	      )
	    )
	   )
    
	  ( "4-8-9^4-8-15" 
	   "4-9-2^4-9-92" 
	   23.5 
	   -81.1 
	   85.8 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.5 
	      )
	    )
	   )
    
	  ( "4-8-9^4-8-15" 
	   "4-8-17^4-8-83" 
	   38.5 
	   25.9 
	   30.4 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      9.2 
	      )
	    )
	   )
    
	  ( "4-8-2^4-8-8" 
	   "4-8-9^4-8-15" 
	   9.7 
	   -10.7 
	   30.4 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.9 
	      )
	    )
	   )
    
	  ( "4-10-2^4-10-9" 
	   "4-10-10^4-10-50" 
	   -25.3 
	   13.7 
	   39.4 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.1 
	      )
	    )
	   )
    
	  ( "4-10-2^4-10-9" 
	   "4-11-2^4-11-59" 
	   -3.9 
	   24.3 
	   100.4 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.1 
	      )
	    )
	   )
    
	  ( "4-8-2^4-8-8" 
	   "4-10-2^4-10-9" 
	   -0.5 
	   -5.3 
	   38.4 
	   0.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      2.5 
	      )
	    )
	   )
    
	  ( "4-4-2^4-4-5" 
	   "4-8-2^4-8-8" 
	   4.7 
	   -7.7 
	   29.0 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.3 
	      )
	    )
	   )
    
	  ( "4-1-2^4-1-5" 
	   "4-4-2^4-4-5" 
	   4.1 
	   -2.1 
	   25.8 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.8 
	      )
	    )
	   )
    
	  ( SOMA 
	   "4-1-2^4-1-5" 
	   3.3 
	   -5.5 
	   20.0 
	   1.1 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.3 
	      )
	    )
	   )
    
	  ( "2-7-2^2-7-18" 
	   "2-7-19^2-7-74" 
	   80.7 
	   -13.1 
	   -41.8 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.0 
	      )
	    )
	   )
    
	  ( "2-8-2^2-8-37" 
	   "2-8-38^2-8-84" 
	   106.9 
	   -4.7 
	   -66.0 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      6.3 
	      )
	    )
	   )
    
	  ( "2-8-2^2-8-37" 
	   "2-9-2^2-9-57" 
	   109.3 
	   -17.9 
	   -66.6 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.2 
	      )
	    )
	   )
    
	  ( "2-7-2^2-7-18" 
	   "2-8-2^2-8-37" 
	   67.5 
	   -6.7 
	   -52.4 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      10.4 
	      )
	    )
	   )
    
	  ( "2-6-2^2-6-10" 
	   "2-7-2^2-7-18" 
	   41.9 
	   -11.5 
	   -35.6 
	   1.0 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      10.9 
	      )
	    )
	   )
    
	  ( "2-6-2^2-6-10" 
	   "2-6-11^2-6-95" 
	   92.9 
	   -64.1 
	   -60.8 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      4.6 
	      )
	    )
	   )
    
	  ( "2-5-2^2-5-13" 
	   "2-6-2^2-6-10" 
	   33.7 
	   -23.3 
	   -25.2 
	   0.8 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      19.5 
	      )
	    )
	   )
    
	  ( "2-5-2^2-5-13" 
	   "2-5-14^2-5-98" 
	   47.3 
	   50.7 
	   -10.8 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      9.1 
	      )
	    )
	   )
    
	  ( "2-2-2^2-2-9" 
	   "2-5-2^2-5-13" 
	   28.7 
	   -19.3 
	   -23.6 
	   1.3 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      11.0 
	      )
	    )
	   )
    
	  ( "2-2-38^2-2-65" 
	   "2-3-2" 
	   13.7 
	   -67.1 
	   -47.0 
	   0.2 
	   )
    
	  ( "2-2-38^2-2-65" 
	   "2-2-66^2-2-102" 
	   8.9 
	   -94.3 
	   -58.0 
	   0.5 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.0 
	      )
	    )
	   )
    
	  ( "2-2-10^2-2-37" 
	   "2-2-38^2-2-65" 
	   13.3 
	   -67.9 
	   -47.4 
	   0.8 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      9.5 
	      )
	    )
	   )
    
	  ( "2-2-10^2-2-37" 
	   "2-4-2^2-4-20" 
	   32.1 
	   -54.1 
	   -40.8 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      5.7 
	      )
	    )
	   )
    
	  ( "2-2-2^2-2-9" 
	   "2-2-10^2-2-37" 
	   24.1 
	   -45.3 
	   -34.6 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.0 
	      )
	    )
	   )
    
	  ( "2-1-2^2-1-6" 
	   "2-2-2^2-2-9" 
	   18.5 
	   -16.9 
	   -23.4 
	   1.0 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      3.4 
	      )
	    )
	   )
    
	  ( "2-1-2^2-1-6" 
	   "2-1-7^2-1-90" 
	   -23.1 
	   -33.1 
	   -94.6 
	   0.6 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      7.1 
	      )
	    )
	   )
    
	  ( SOMA 
	   "2-1-2^2-1-6" 
	   11.5 
	   -11.7 
	   -21.2 
	   0.7 
     
	   (
	    ( CYTOPLASMIC-RESISTIVITY-COEFF 
	      1.6 
	      )
	    )
	   )
	  )
	)



  (create-tree
   (create-soma 
    :cell (create-cell "j2187dd_sdt-max-red"
		       :origin '(0.0 0.0 0.0)
		       :cell-type 'j2187dd_sdt)
    :diameter 17.705774)		; Soma diameter in um
   ntscable-cell-tree)

  )

(push 'j2187dd_sdt *CIRCUIT-FUNCTIONS*)




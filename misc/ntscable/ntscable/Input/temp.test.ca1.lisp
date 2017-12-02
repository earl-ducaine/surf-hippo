;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF; Base: 10; -*- 

(in-package "SURF-HIPPO")

#|
/*----------------------------------------------------------------
%W%  %G%
temp.test.ca1  translated Wed Sep 17 19:48:44 1997 by ntscable %I%
source file syntax: Eutectic
output file syntax: NEURON
soma: diameter = 0 um  length = 0 um  area = 0 um2
      0 three-D points
1 primary neurites
0 branches totaling 420.892 um in length, 1643.99 um2 in area
33 tree points translated to 2 segments (1 requested)
Neurites divided into segments of equal dx over each entire branch.
Segment length constrained to be < 420.892 um.
@(#)c12861.ca1	1.3 9/25/89
ASCII version of CA1 cell C12861 received 14-APR-89 from D. Amaral.
Edited 17-APR-89 by JCW: changed soma outline point types to match 
NTS 3.0 convention; deleted axon and contours of vetricular surface 
and hippocampal fissure.  22-SEP-89 by WWL: increased thickness of
all dendrites under 1 micron to 1.0

No. points   1269
No. trees       9
Text  File sent from PDP-11                                                                                                   
 
----------------------------------------------------------------*/


						/* soma geometry */
    soma {
        nseg = 1
        pt3dclear()
        for j = 1, fscan() {
            pt3dadd(fscan(),fscan(),fscan(),fscan())
        }
    }



SOMA COORDINATES AND DIAMETERS:

    0


|#




;; No soma outline. 

;; No soma 3D points.
(setq  *nts-radius* (sqrt (/ 0 (* 3.14159 4))))   
 
(setq *ntscable-list* '(

 ;;  Number of segs - 1, Number of xyzd points - 33
;; Section 1, parent index -1
 
  ((1 1 0)  SOMA  (   3.5     16    0.5) 3.7)
 
  ((1 1 1) (1 1 0)  (   2.5     29      2) 2.3)
 
  ((1 1 2) (1 1 1)  (   0.5   37.5    1.5) 1.7)
 
  ((1 1 3) (1 1 2)  (     0   53.5    5.5) 1.7)
 
  ((1 1 4) (1 1 3)  (  -0.5   59.5      7) 1.7)
 
  ((1 1 5) (1 1 4)  (     3   69.5      8) 1.6)
 
  ((1 1 6) (1 1 5)  (   7.5     86   10.5) 1.5)
 
  ((1 1 7) (1 1 6)  (   9.5     89   10.5) 1.5)
 
  ((1 1 8) (1 1 7)  (    10     91   10.5) 1.6)
 
  ((1 1 9) (1 1 8)  (    14  112.5     13) 1.6)
 
  ((1 1 10) (1 1 9)  (    15  123.5   14.5) 1.2)
 
  ((1 1 11) (1 1 10)  (    15  128.5     15) 1.5)
 
  ((1 1 12) (1 1 11)  (  17.5    133     16) 1.2)
 
  ((1 1 13) (1 1 12)  (    17    140     17) 1.2)
 
  ((1 1 14) (1 1 13)  (  18.5  155.5     18) 1.2)
 
  ((1 1 15) (1 1 14)  (    18    164     17)   1)
 
  ((1 1 16) (1 1 15)  (    19    181     17) 1.1)
 
  ((1 1 17) (1 1 16)  (    19  198.5   19.5) 1.1)
 
  ((1 1 18) (1 1 17)  (    20  223.5     20) 1.1)
 
  ((1 1 19) (1 1 18)  (  20.5    231     21)   1)
 
  ((1 1 20) (1 1 19)  (    21    233     21)   1)
 
  ((1 1 21) (1 1 20)  (    21    236   20.5)   1)
 
  ((1 1 22) (1 1 21)  (    23  256.5     22)   1)
 
  ((1 1 23) (1 1 22)  (  24.5  267.5     22)   1)
 
  ((1 1 24) (1 1 23)  (  24.5    278     24)   1)
 
  ((1 1 25) (1 1 24)  (  24.5  296.5     25)   1)
 
  ((1 1 26) (1 1 25)  (  23.5  322.5     27)   1)
 
  ((1 1 27) (1 1 26)  (    21  349.5   26.5)   1)
 
  ((1 1 28) (1 1 27)  (    21  355.5     26)   1)
 
  ((1 1 29) (1 1 28)  (    19  369.5   29.5)   1)
 
  ((1 1 30) (1 1 29)  (     6  384.5   26.5)   1)
 
  ((1 1 31) (1 1 30)  ( -15.5    397   24.5)   1)
 
  ((1 1 32) (1 1 31)  ( -24.5    409   19.5)   1)
;;; End of section ********* 
;;; End of neurite ********* 

))


(setq *process-ntscable-list* t)
(setq *nts-cell-type* "ntscable")
(setq *nts-cell-name* "ntscable")
(setq *nts-r-mem* 40000.0)
(setq *nts-soma-r-mem* 40000.0)
(setq *nts-r-a* 200.0)

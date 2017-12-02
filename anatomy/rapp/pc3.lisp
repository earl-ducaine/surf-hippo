;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF; Base: 10; -*- 

(in-package "SURF-HIPPO")

#|
/*----------------------------------------------------------------
%W%  %G%
pc3  translated Wed Mar 20 10:00:39 1996 by ntscable %I%
source file syntax: Eutectic
output file syntax: NEURON
soma: diameter = 17.8068 um  length = 29.6204 um  area = 1657.02 um2
      23 three-D points; 29 outline points numbered 1-29
      outline diameter = 23.595 um
1 primary neurites
418 branches totaling 8128.17 um in length, 44602.6 um2 in area
1384 tree points translated to 419 segments (1 requested)
Neurites divided into segments of equal dx over each entire branch.
Segment length constrained to be < 8128.17 um.
No. points   1413
No. trees       2
Text  20_3a                                                                                                                   
 
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

    23
   -12.5     17.5        0        0
   -12.5    18.75        0      2.5
     -12    18.25        0      5.5
     -12    17.25        0      7.5
   -11.5       17        0       10
     -11     16.5        0       13
    -9.5     16.5        0       16
      -9     15.5        0       19
    -8.5    15.25        0     20.5
      -8   14.938        0   21.875
    -6.5    14.25        0     25.5
    -4.5     14.5        0       28
    -3.5    14.35        0     28.7
      -2     14.5    0.125   29.001
    -0.5     14.5     0.25   29.004
     0.5    14.55      0.2   28.903
     2.5     13.4      0.1   26.201
       4   12.475    0.025    24.05
     4.5   11.917        0   22.833
       5   12.083        0   20.167
     5.5    12.75        0     16.5
     7.5    12.75        0      9.5
     7.5        8        0        0


|#




(setq *soma-outline*

 '(
  (-0.5 0 0.5)
  (-3.5 0 0)
  (-6.5 1.5 0)
  (-8 4 0)
  (-9 6 0)
  (-9.5 8.5 0)
  (-11 10 0)
  (-11.5 12 0)
  (-12 13.5 0)
  (-12 15.5 0)
  (-12.5 17.5 0)
  (-12.5 20 0)
  (-11 23 0)
  (-8.5 25.5 0)
  (-6.5 27 0)
  (-4.5 28.5 0)
  (-2 29 0)
  (0.5 29 0)
  (2.5 26.5 0)
  (4 24.5 0)
  (5.5 21 0)
  (7.5 17.5 0)
  (6 12.5 0)
  (5.5 11 0)
  (7.5 8 0)
  (5.5 4.5 0)
  (5 2 0)
  (4.5 0.5 0)
  (-0.5 0 0.5)

 ))

(setq *soma-points*

 '(
  (-12.5 17.5 0 0)
  (-12.5 18.75 0 2.5)
  (-12 18.25 0 5.5)
  (-12 17.25 0 7.5)
  (-11.5 17 0 10)
  (-11 16.5 0 13)
  (-9.5 16.5 0 16)
  (-9 15.5 0 19)
  (-8.5 15.25 0 20.5)
  (-8 14.9375 0 21.875)
  (-6.5 14.25 0 25.5)
  (-4.5 14.5 0 28)
  (-3.5 14.35 0 28.7)
  (-2 14.5 0.125 29.0011)
  (-0.5 14.5 0.25 29.0043)
  (0.5 14.55 0.2 28.9028)
  (2.5 13.4 0.1 26.2008)
  (4 12.475 0.025 24.0501)
  (4.5 11.9167 0 22.8333)
  (5 12.0833 0 20.1667)
  (5.5 12.75 0 16.5)
  (7.5 12.75 0 9.5)
  (7.5 8 0 0)

 ))
(setq  *nts-radius* (sqrt (/ 1657.02 (* 3.14159 4))))   
 
(setq *ntscable-list* '(

 ;;  Number of segs - 1, Number of xyzd points - 11
;; Section 1, parent index -1
 
  ((1 1 0)  SOMA  (     0      0      0) 5.5)
 
  ((1 1 1) (1 1 0)  (     4   -3.5    0.5) 4.8)
 
  ((1 1 2) (1 1 1)  (     7     -7    0.5) 4.8)
 
  ((1 1 3) (1 1 2)  (   9.5     -9    0.5) 4.8)
 
  ((1 1 4) (1 1 3)  (  11.5    -12    1.5)   6)
 
  ((1 1 5) (1 1 4)  (    13  -15.5    1.5) 7.9)
 
  ((1 1 6) (1 1 5)  (    17  -13.5    1.5) 4.2)
 
  ((1 1 7) (1 1 6)  (  19.5  -10.5    1.5)   2)
 
  ((1 1 8) (1 1 7)  (  21.5   -8.5    1.5)   2)
 
  ((1 1 9) (1 1 8)  (    23     -9    1.5) 1.5)
 
  ((1 1 10) (1 1 9)  (    27   -6.5    1.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 9
;; Section 2, parent index 0
 
  ((1 2 0) (BRANCH-PT 1 1 8)  (  21.5   -8.5    1.5)   2)
 
  ((1 2 1) (1 2 0)  (  21.5   -6.5    1.5) 1.3)
 
  ((1 2 2) (1 2 1)  (  21.5     -5    1.5) 1.3)
 
  ((1 2 3) (1 2 2)  (  17.5   -3.5    1.5) 1.3)
 
  ((1 2 4) (1 2 3)  (  15.5   -3.5      3) 1.3)
 
  ((1 2 5) (1 2 4)  (    14   -3.5      3) 1.3)
 
  ((1 2 6) (1 2 5)  (    12   -3.5      3) 1.3)
 
  ((1 2 7) (1 2 6)  (    10     -4      1) 1.3)
 
  ((1 2 8) (1 2 7)  (   9.5     -4      1) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 12
;; Section 3, parent index 0
 
  ((1 3 0) (BRANCH-PT 1 1 6)  (    17  -13.5    1.5) 4.2)
 
  ((1 3 1) (1 3 0)  (  22.5    -16    1.5) 3.2)
 
  ((1 3 2) (1 3 1)  (  29.5    -19    1.5) 3.2)
 
  ((1 3 3) (1 3 2)  (  37.5    -20    2.5) 5.1)
 
  ((1 3 4) (1 3 3)  (  40.5  -15.5    2.5) 2.4)
 
  ((1 3 5) (1 3 4)  (    42  -14.5    2.5) 1.7)
 
  ((1 3 6) (1 3 5)  (    45    -15    2.5) 1.7)
 
  ((1 3 7) (1 3 6)  (    49  -15.5    2.5) 1.7)
 
  ((1 3 8) (1 3 7)  (    51  -13.5    2.5) 1.7)
 
  ((1 3 9) (1 3 8)  (    52    -11    2.5) 1.7)
 
  ((1 3 10) (1 3 9)  (  55.5     -8    2.5) 1.7)
 
  ((1 3 11) (1 3 10)  (  57.5     -4    2.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 23
;; Section 4, parent index 2
 
  ((1 4 0) (BRANCH-PT 1 3 5)  (    42  -14.5    2.5) 1.7)
 
  ((1 4 1) (1 4 0)  (    44    -13    2.5) 1.7)
 
  ((1 4 2) (1 4 1)  (    48    -13    2.5) 1.7)
 
  ((1 4 3) (1 4 2)  (  51.5    -13    2.5) 1.7)
 
  ((1 4 4) (1 4 3)  (    54    -13    2.5) 1.7)
 
  ((1 4 5) (1 4 4)  (    53   -9.5    2.5) 1.7)
 
  ((1 4 6) (1 4 5)  (    52   -7.5    2.5) 1.7)
 
  ((1 4 7) (1 4 6)  (    52   -7.5    2.5) 1.4)
 
  ((1 4 8) (1 4 7)  (  49.5   -7.5    2.5) 1.2)
 
  ((1 4 9) (1 4 8)  (  48.5     -6    2.5) 1.2)
 
  ((1 4 10) (1 4 9)  (    47     -5      3) 1.4)
 
  ((1 4 11) (1 4 10)  (  44.5     -4      3) 1.4)
 
  ((1 4 12) (1 4 11)  (  41.5     -4      3) 1.4)
 
  ((1 4 13) (1 4 12)  (  39.5     -4      3) 1.4)
 
  ((1 4 14) (1 4 13)  (    39     -3      3)   1)
 
  ((1 4 15) (1 4 14)  (    36   -1.5      3)   1)
 
  ((1 4 16) (1 4 15)  (    34     -1      3)   1)
 
  ((1 4 17) (1 4 16)  (  31.5     -1      3)   1)
 
  ((1 4 18) (1 4 17)  (    28      1      3)   1)
 
  ((1 4 19) (1 4 18)  (    27      3      3) 1.4)
 
  ((1 4 20) (1 4 19)  (    27      6      3) 1.4)
 
  ((1 4 21) (1 4 20)  (    27      8      3) 1.4)
 
  ((1 4 22) (1 4 21)  (    26   10.5    2.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 5, parent index 3
 
  ((1 5 0) (BRANCH-PT 1 4 17)  (  31.5     -1      3)   1)
 
  ((1 5 1) (1 5 0)  (  28.5   -1.5      3) 1.1)
 
  ((1 5 2) (1 5 1)  (  23.5   -2.5      3) 1.1)
 
  ((1 5 3) (1 5 2)  (  20.5     -2      3) 1.1)
 
  ((1 5 4) (1 5 3)  (  17.5     -1      3) 0.9)
 
  ((1 5 5) (1 5 4)  (    16      0      3) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 6, parent index 3
 
  ((1 6 0) (BRANCH-PT 1 4 10)  (    47     -5      3) 1.4)
 
  ((1 6 1) (1 6 0)  (    49     -3      3)   1)
 
  ((1 6 2) (1 6 1)  (  49.5    0.5      3)   1)
 
  ((1 6 3) (1 6 2)  (  52.5    3.5      3)   1)
 
  ((1 6 4) (1 6 3)  (  53.5      6      3)   1)
 
  ((1 6 5) (1 6 4)  (  54.5    8.5      3)   1)
 
  ((1 6 6) (1 6 5)  (    56   12.5      3) 1.1)
 
  ((1 6 7) (1 6 6)  (    58     14      3) 1.3)
 
  ((1 6 8) (1 6 7)  (    58     17      3) 1.3)
 
  ((1 6 9) (1 6 8)  (  59.5     19      3) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 7, parent index 5
 
  ((1 7 0) (BRANCH-PT 1 6 7)  (    58     14      3) 1.3)
 
  ((1 7 1) (1 7 0)  (    61   14.5      3) 1.1)
 
  ((1 7 2) (1 7 1)  (  61.5     14      3) 1.1)
 
  ((1 7 3) (1 7 2)  (  63.5     13      3) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 8, parent index 6
 
  ((1 8 0) (BRANCH-PT 1 7 2)  (  61.5     14      3) 1.1)
 
  ((1 8 1) (1 8 0)  (  64.5     17      3) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 16
;; Section 9, parent index 3
 
  ((1 9 0) (BRANCH-PT 1 4 7)  (    52   -7.5    2.5) 1.4)
 
  ((1 9 1) (1 9 0)  (  56.5   -3.5    4.5) 1.1)
 
  ((1 9 2) (1 9 1)  (    59     -1    4.5) 1.1)
 
  ((1 9 3) (1 9 2)  (    61    0.5    4.5)   1)
 
  ((1 9 4) (1 9 3)  (    59    4.5    4.5)   1)
 
  ((1 9 5) (1 9 4)  (    59      6    4.5) 1.1)
 
  ((1 9 6) (1 9 5)  (    54    7.5    4.5) 1.1)
 
  ((1 9 7) (1 9 6)  (    51    7.5    4.5) 1.1)
 
  ((1 9 8) (1 9 7)  (  47.5      8    4.5) 1.1)
 
  ((1 9 9) (1 9 8)  (    47     10    3.5) 1.1)
 
  ((1 9 10) (1 9 9)  (    48     15    3.5) 1.1)
 
  ((1 9 11) (1 9 10)  (    48     17    3.5) 1.1)
 
  ((1 9 12) (1 9 11)  (  50.5     20    3.5) 1.1)
 
  ((1 9 13) (1 9 12)  (    52   21.5    3.5) 1.1)
 
  ((1 9 14) (1 9 13)  (    52     25    3.5) 1.1)
 
  ((1 9 15) (1 9 14)  (    50   28.5    3.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 10, parent index 8
 
  ((1 10 0) (BRANCH-PT 1 9 3)  (    61    0.5    4.5)   1)
 
  ((1 10 1) (1 10 0)  (  61.5    2.5      4) 1.1)
 
  ((1 10 2) (1 10 1)  (  60.5      7      4) 1.1)
 
  ((1 10 3) (1 10 2)  (    58   10.5      4) 1.1)
 
  ((1 10 4) (1 10 3)  (  55.5   14.5      4) 1.1)
 
  ((1 10 5) (1 10 4)  (  52.5   17.5      4) 1.1)
 
  ((1 10 6) (1 10 5)  (  51.5   18.5      4) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 11, parent index 9
 
  ((1 11 0) (BRANCH-PT 1 10 1)  (  61.5    2.5      4) 1.1)
 
  ((1 11 1) (1 11 0)  (  64.5    5.5      4) 1.1)
 
  ((1 11 2) (1 11 1)  (  66.5      9    7.5) 1.1)
 
  ((1 11 3) (1 11 2)  (    69     11    7.5) 1.1)
 
  ((1 11 4) (1 11 3)  (    67   14.5    7.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 12, parent index 10
 
  ((1 12 0) (BRANCH-PT 1 11 3)  (    69     11    7.5) 1.1)
 
  ((1 12 1) (1 12 0)  (  71.5     11    7.5) 1.1)
 
  ((1 12 2) (1 12 1)  (  74.5     11    7.5) 1.1)
 
  ((1 12 3) (1 12 2)  (  74.5   15.5    7.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 13, parent index 11
 
  ((1 13 0) (BRANCH-PT 1 12 2)  (  74.5     11    7.5) 1.1)
 
  ((1 13 1) (1 13 0)  (  77.5     12    7.5) 1.1)
 
  ((1 13 2) (1 13 1)  (    82     12    7.5) 1.1)
 
  ((1 13 3) (1 13 2)  (    82     12    7.5) 1.1)
 
  ((1 13 4) (1 13 3)  (    84   10.5    7.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 14, parent index 12
 
  ((1 14 0) (BRANCH-PT 1 13 3)  (    82     12    7.5) 1.1)
 
  ((1 14 1) (1 14 0)  (    84   14.5    7.5) 1.1)
 
  ((1 14 2) (1 14 1)  (    82     17    7.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 15, parent index 13
 
  ((1 15 0) (BRANCH-PT 1 14 1)  (    84   14.5    7.5) 1.1)
 
  ((1 15 1) (1 15 0)  (  87.5   15.5      7) 0.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 11
;; Section 16, parent index 0
 
  ((1 16 0) (BRANCH-PT 1 1 5)  (    13  -15.5    1.5) 7.9)
 
  ((1 16 1) (1 16 0)  (  38.5  -19.5    6.5) 1.2)
 
  ((1 16 2) (1 16 1)  (    44    -17    6.5) 1.1)
 
  ((1 16 3) (1 16 2)  (  48.5    -17    6.5) 1.1)
 
  ((1 16 4) (1 16 3)  (    52    -17    6.5) 1.1)
 
  ((1 16 5) (1 16 4)  (  54.5    -16    6.5) 1.2)
 
  ((1 16 6) (1 16 5)  (  57.5  -13.5    6.5) 1.2)
 
  ((1 16 7) (1 16 6)  (    58    -11    6.5) 1.2)
 
  ((1 16 8) (1 16 7)  (    60   -9.5    6.5) 1.5)
 
  ((1 16 9) (1 16 8)  (    60   -6.5      6) 1.5)
 
  ((1 16 10) (1 16 9)  (  60.5     -2      6) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 17, parent index 15
 
  ((1 17 0) (BRANCH-PT 1 16 9)  (    60   -6.5      6) 1.5)
 
  ((1 17 1) (1 17 0)  (  62.5     -5      6) 1.3)
 
  ((1 17 2) (1 17 1)  (    65     -4      6) 1.3)
 
  ((1 17 3) (1 17 2)  (  68.5     -3      6) 1.3)
 
  ((1 17 4) (1 17 3)  (    69     -3      6) 1.3)
 
  ((1 17 5) (1 17 4)  (    73      0      6) 1.3)
 
  ((1 17 6) (1 17 5)  (  78.5      3      6) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 18, parent index 16
 
  ((1 18 0) (BRANCH-PT 1 17 4)  (    69     -3      6) 1.3)
 
  ((1 18 1) (1 18 0)  (  71.5   -3.5    8.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 19, parent index 15
 
  ((1 19 0) (BRANCH-PT 1 16 8)  (    60   -9.5    6.5) 1.5)
 
  ((1 19 1) (1 19 0)  (    66  -11.5    7.5) 1.1)
 
  ((1 19 2) (1 19 1)  (  68.5  -13.5      7) 1.1)
 
  ((1 19 3) (1 19 2)  (  67.5  -17.5    7.5) 1.1)
 
  ((1 19 4) (1 19 3)  (    68  -19.5      7) 1.1)
 
  ((1 19 5) (1 19 4)  (    66  -21.5      7) 1.1)
 
  ((1 19 6) (1 19 5)  (    64    -20      7) 1.1)
 
  ((1 19 7) (1 19 6)  (    61    -20      7) 1.1)
 
  ((1 19 8) (1 19 7)  (    55  -20.5      7) 1.1)
 
  ((1 19 9) (1 19 8)  (  50.5    -20      7) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 20, parent index 18
 
  ((1 20 0) (BRANCH-PT 1 19 5)  (    66  -21.5      7) 1.1)
 
  ((1 20 1) (1 20 0)  (    67    -23      6) 1.1)
 
  ((1 20 2) (1 20 1)  (    72    -24      7) 1.1)
 
  ((1 20 3) (1 20 2)  (    74    -23      7) 1.1)
 
  ((1 20 4) (1 20 3)  (  78.5    -20      7) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 15
;; Section 21, parent index 15
 
  ((1 21 0) (BRANCH-PT 1 16 1)  (  38.5  -19.5    6.5) 1.2)
 
  ((1 21 1) (1 21 0)  (  40.5    -24    3.5) 3.1)
 
  ((1 21 2) (1 21 1)  (  44.5  -26.5      5) 3.1)
 
  ((1 21 3) (1 21 2)  (  46.5  -28.5      5) 3.1)
 
  ((1 21 4) (1 21 3)  (    49    -31      5) 3.4)
 
  ((1 21 5) (1 21 4)  (  52.5    -34      6) 6.8)
 
  ((1 21 6) (1 21 5)  (  57.5    -31      6) 2.7)
 
  ((1 21 7) (1 21 6)  (    62  -28.5      6) 2.7)
 
  ((1 21 8) (1 21 7)  (    65  -27.5      6) 3.7)
 
  ((1 21 9) (1 21 8)  (    68    -24      9) 1.9)
 
  ((1 21 10) (1 21 9)  (  71.5    -20      9) 1.9)
 
  ((1 21 11) (1 21 10)  (  75.5  -16.5      9) 1.9)
 
  ((1 21 12) (1 21 11)  (  76.5  -11.5    8.5) 1.7)
 
  ((1 21 13) (1 21 12)  (    68     -6      9) 1.7)
 
  ((1 21 14) (1 21 13)  (    63   -3.5      9) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 22, parent index 20
 
  ((1 22 0) (BRANCH-PT 1 21 13)  (    68     -6      9) 1.7)
 
  ((1 22 1) (1 22 0)  (  68.5     -1      9) 1.3)
 
  ((1 22 2) (1 22 1)  (  72.5    3.5      9) 1.3)
 
  ((1 22 3) (1 22 2)  (    72    6.5      9) 1.3)
 
  ((1 22 4) (1 22 3)  (    70    6.5      9) 1.3)
 
  ((1 22 5) (1 22 4)  (  66.5      5      9) 1.3)
 
  ((1 22 6) (1 22 5)  (  64.5      2      9) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 23, parent index 21
 
  ((1 23 0) (BRANCH-PT 1 22 4)  (    70    6.5      9) 1.3)
 
  ((1 23 1) (1 23 0)  (  68.5    9.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 24, parent index 20
 
  ((1 24 0) (BRANCH-PT 1 21 12)  (  76.5  -11.5    8.5) 1.7)
 
  ((1 24 1) (1 24 0)  (    73   -5.5   11.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 25, parent index 20
 
  ((1 25 0) (BRANCH-PT 1 21 11)  (  75.5  -16.5      9) 1.9)
 
  ((1 25 1) (1 25 0)  (    80    -13     10) 1.3)
 
  ((1 25 2) (1 25 1)  (    81  -10.5     10) 1.3)
 
  ((1 25 3) (1 25 2)  (  80.5   -6.5     10) 1.3)
 
  ((1 25 4) (1 25 3)  (  79.5   -2.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 26, parent index 24
 
  ((1 26 0) (BRANCH-PT 1 25 3)  (  80.5   -6.5     10) 1.3)
 
  ((1 26 1) (1 26 0)  (    83   -2.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 27, parent index 24
 
  ((1 27 0) (BRANCH-PT 1 25 2)  (    81  -10.5     10) 1.3)
 
  ((1 27 1) (1 27 0)  (  85.5  -10.5   13.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 28, parent index 20
 
  ((1 28 0) (BRANCH-PT 1 21 8)  (    65  -27.5      6) 3.7)
 
  ((1 28 1) (1 28 0)  (    68    -29      7) 1.3)
 
  ((1 28 2) (1 28 1)  (    74  -28.5      7) 2.5)
 
  ((1 28 3) (1 28 2)  (  77.5  -24.5    7.5) 1.6)
 
  ((1 28 4) (1 28 3)  (  80.5  -19.5    7.5) 1.6)
 
  ((1 28 5) (1 28 4)  (  79.5  -16.5    7.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 29, parent index 27
 
  ((1 29 0) (BRANCH-PT 1 28 4)  (  80.5  -19.5    7.5) 1.6)
 
  ((1 29 1) (1 29 0)  (  84.5    -14      9) 1.6)
 
  ((1 29 2) (1 29 1)  (  87.5  -11.5    9.5) 1.6)
 
  ((1 29 3) (1 29 2)  (  91.5     -8    9.5) 1.6)
 
  ((1 29 4) (1 29 3)  (    92     -5    9.5) 1.6)
 
  ((1 29 5) (1 29 4)  (  86.5   -3.5    9.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 11
;; Section 30, parent index 28
 
  ((1 30 0) (BRANCH-PT 1 29 4)  (    92     -5    9.5) 1.6)
 
  ((1 30 1) (1 30 0)  (  93.5   -3.5    9.5)   2)
 
  ((1 30 2) (1 30 1)  (  92.5    0.5    6.5) 1.7)
 
  ((1 30 3) (1 30 2)  (    92      5    6.5) 1.7)
 
  ((1 30 4) (1 30 3)  (  88.5      8    6.5) 1.7)
 
  ((1 30 5) (1 30 4)  (    90   13.5    6.5) 1.7)
 
  ((1 30 6) (1 30 5)  (    90   16.5    6.5) 1.7)
 
  ((1 30 7) (1 30 6)  (    90     19    6.5) 1.3)
 
  ((1 30 8) (1 30 7)  (    90   23.5      7) 1.1)
 
  ((1 30 9) (1 30 8)  (  92.5     25      7) 0.6)
 
  ((1 30 10) (1 30 9)  (  95.5   27.5      7) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 31, parent index 29
 
  ((1 31 0) (BRANCH-PT 1 30 9)  (  92.5     25      7) 0.6)
 
  ((1 31 1) (1 31 0)  (    95     24      7) 0.6)
 
  ((1 31 2) (1 31 1)  (    96   22.5      7) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 32, parent index 29
 
  ((1 32 0) (BRANCH-PT 1 30 3)  (    92      5    6.5) 1.7)
 
  ((1 32 1) (1 32 0)  (  91.5      9    5.5) 1.2)
 
  ((1 32 2) (1 32 1)  (    99     14      7) 1.2)
 
  ((1 32 3) (1 32 2)  (  95.5   17.5      7) 1.2)
 
  ((1 32 4) (1 32 3)  (    99   20.5      7) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 33, parent index 31
 
  ((1 33 0) (BRANCH-PT 1 32 2)  (    99     14      7) 1.2)
 
  ((1 33 1) (1 33 0)  (   100     16      7) 0.8)
 
  ((1 33 2) (1 33 1)  ( 103.5   18.5      7) 0.8)
 
  ((1 33 3) (1 33 2)  (   101     24      7) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 34, parent index 29
 
  ((1 34 0) (BRANCH-PT 1 30 1)  (  93.5   -3.5    9.5)   2)
 
  ((1 34 1) (1 34 0)  (  98.5   -0.5      8)   1)
 
  ((1 34 2) (1 34 1)  (   101      2      8)   1)
 
  ((1 34 3) (1 34 2)  (   105      4    7.5) 0.9)
 
  ((1 34 4) (1 34 3)  ( 108.5    5.5    7.5) 0.9)
 
  ((1 34 5) (1 34 4)  (   108      8    7.5) 0.9)
 
  ((1 34 6) (1 34 5)  ( 105.5      9    7.5) 0.9)
 
  ((1 34 7) (1 34 6)  (   107     12    7.5)   1)
 
  ((1 34 8) (1 34 7)  (   107     16    7.5) 1.1)
 
  ((1 34 9) (1 34 8)  ( 105.5   17.5    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 35, parent index 33
 
  ((1 35 0) (BRANCH-PT 1 34 5)  (   108      8    7.5) 0.9)
 
  ((1 35 1) (1 35 0)  ( 109.5    9.5      6) 1.4)
 
  ((1 35 2) (1 35 1)  (   114     13      6) 1.4)
 
  ((1 35 3) (1 35 2)  (   115   13.5      6) 1.2)
 
  ((1 35 4) (1 35 3)  ( 116.5     17      6) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 36, parent index 34
 
  ((1 36 0) (BRANCH-PT 1 35 3)  (   115   13.5      6) 1.2)
 
  ((1 36 1) (1 36 0)  (   119     14      7) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 37, parent index 33
 
  ((1 37 0) (BRANCH-PT 1 34 3)  (   105      4    7.5) 0.9)
 
  ((1 37 1) (1 37 0)  (   111    4.5    7.5)   1)
 
  ((1 37 2) (1 37 1)  (   111      3    7.5)   1)
 
  ((1 37 3) (1 37 2)  (   117      4      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 38, parent index 36
 
  ((1 38 0) (BRANCH-PT 1 37 1)  (   111    4.5    7.5)   1)
 
  ((1 38 1) (1 38 0)  (   115      6    7.5)   1)
 
  ((1 38 2) (1 38 1)  ( 124.5      5    7.5)   1)
 
  ((1 38 3) (1 38 2)  (   128      7    7.5)   1)
 
  ((1 38 4) (1 38 3)  (   131    7.5    7.5) 0.7)
 
  ((1 38 5) (1 38 4)  ( 134.5      6      7) 0.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 39, parent index 37
 
  ((1 39 0) (BRANCH-PT 1 38 4)  (   131    7.5    7.5) 0.7)
 
  ((1 39 1) (1 39 0)  ( 133.5    9.5    7.5) 0.7)
 
  ((1 39 2) (1 39 1)  ( 141.5    6.5      7) 0.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 40, parent index 33
 
  ((1 40 0) (BRANCH-PT 1 34 1)  (  98.5   -0.5      8)   1)
 
  ((1 40 1) (1 40 0)  (    97     -5      8) 0.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 41, parent index 27
 
  ((1 41 0) (BRANCH-PT 1 28 2)  (    74  -28.5      7) 2.5)
 
  ((1 41 1) (1 41 0)  (    78    -31    7.5) 0.7)
 
  ((1 41 2) (1 41 1)  (  81.5  -29.5    7.5)   1)
 
  ((1 41 3) (1 41 2)  (    85  -30.5    7.5) 0.6)
 
  ((1 41 4) (1 41 3)  (    83  -34.5    7.5) 0.6)
 
  ((1 41 5) (1 41 4)  (    76    -33    7.5) 0.6)
 
  ((1 41 6) (1 41 5)  (    75  -32.5      8) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 42, parent index 40
 
  ((1 42 0) (BRANCH-PT 1 41 4)  (    83  -34.5    7.5) 0.6)
 
  ((1 42 1) (1 42 0)  (    80    -32    7.5) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 43, parent index 40
 
  ((1 43 0) (BRANCH-PT 1 41 2)  (  81.5  -29.5    7.5)   1)
 
  ((1 43 1) (1 43 0)  (    85  -26.5    7.5) 0.9)
 
  ((1 43 2) (1 43 1)  (    89    -22      8) 0.9)
 
  ((1 43 3) (1 43 2)  (  93.5  -23.5      8) 0.9)
 
  ((1 43 4) (1 43 3)  (  95.5    -25      8) 0.9)
 
  ((1 43 5) (1 43 4)  (    95    -29      8) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 44, parent index 42
 
  ((1 44 0) (BRANCH-PT 1 43 4)  (  95.5    -25      8) 0.9)
 
  ((1 44 1) (1 44 0)  (   102  -25.5      8) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 45, parent index 42
 
  ((1 45 0) (BRANCH-PT 1 43 2)  (    89    -22      8) 0.9)
 
  ((1 45 1) (1 45 0)  (    92  -19.5    8.5) 0.9)
 
  ((1 45 2) (1 45 1)  ( 101.5    -18      9) 0.9)
 
  ((1 45 3) (1 45 2)  (   104    -13      9) 0.9)
 
  ((1 45 4) (1 45 3)  (   104    -10      9) 0.9)
 
  ((1 45 5) (1 45 4)  (   102   -8.5      9) 1.4)
 
  ((1 45 6) (1 45 5)  (  97.5     -6    8.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 46, parent index 44
 
  ((1 46 0) (BRANCH-PT 1 45 5)  (   102   -8.5      9) 1.4)
 
  ((1 46 1) (1 46 0)  (   104   -3.5      9) 1.1)
 
  ((1 46 2) (1 46 1)  (   108   -2.5      9) 0.8)
 
  ((1 46 3) (1 46 2)  ( 111.5   -1.5      9) 0.8)
 
  ((1 46 4) (1 46 3)  (   113      0      9) 0.8)
 
  ((1 46 5) (1 46 4)  (   111    0.5      9) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 47, parent index 45
 
  ((1 47 0) (BRANCH-PT 1 46 4)  (   113      0      9) 0.8)
 
  ((1 47 1) (1 47 0)  (   117     -1      9) 0.8)
 
  ((1 47 2) (1 47 1)  ( 120.5    0.5    9.5) 0.8)
 
  ((1 47 3) (1 47 2)  (   127   -0.5    9.5) 0.8)
 
  ((1 47 4) (1 47 3)  (   132     -2    9.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 48, parent index 44
 
  ((1 48 0) (BRANCH-PT 1 45 2)  ( 101.5    -18      9) 0.9)
 
  ((1 48 1) (1 48 0)  (   105    -18      9)   1)
 
  ((1 48 2) (1 48 1)  ( 111.5  -13.5      9)   1)
 
  ((1 48 3) (1 48 2)  (   116  -11.5    9.5)   1)
 
  ((1 48 4) (1 48 3)  (   120   -9.5     10)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 49, parent index 47
 
  ((1 49 0) (BRANCH-PT 1 48 3)  (   116  -11.5    9.5)   1)
 
  ((1 49 1) (1 49 0)  (   120  -11.5   10.5)   1)
 
  ((1 49 2) (1 49 1)  ( 122.5   -7.5    9.5)   1)
 
  ((1 49 3) (1 49 2)  ( 122.5     -3    9.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 50, parent index 47
 
  ((1 50 0) (BRANCH-PT 1 48 1)  (   105    -18      9)   1)
 
  ((1 50 1) (1 50 0)  (   106  -19.5    8.5)   1)
 
  ((1 50 2) (1 50 1)  ( 103.5  -23.5      9)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 51, parent index 49
 
  ((1 51 0) (BRANCH-PT 1 50 1)  (   106  -19.5    8.5)   1)
 
  ((1 51 1) (1 51 0)  (   111  -22.5    8.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 52, parent index 40
 
  ((1 52 0) (BRANCH-PT 1 41 1)  (    78    -31    7.5) 0.7)
 
  ((1 52 1) (1 52 0)  (  84.5    -35    7.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 53, parent index 20
 
  ((1 53 0) (BRANCH-PT 1 21 5)  (  52.5    -34      6) 6.8)
 
  ((1 53 1) (1 53 0)  (    52  -38.5    7.5) 4.9)
 
  ((1 53 2) (1 53 1)  (  44.5  -38.5    6.5) 2.5)
 
  ((1 53 3) (1 53 2)  (  40.5  -41.5    5.5) 1.4)
 
  ((1 53 4) (1 53 3)  (  36.5    -40    5.5) 1.4)
 
  ((1 53 5) (1 53 4)  (    36  -39.5    5.5) 1.3)
 
  ((1 53 6) (1 53 5)  (    32    -35    5.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 54, parent index 52
 
  ((1 54 0) (BRANCH-PT 1 53 5)  (    36  -39.5    5.5) 1.3)
 
  ((1 54 1) (1 54 0)  (  30.5  -37.5    5.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 55, parent index 52
 
  ((1 55 0) (BRANCH-PT 1 53 2)  (  44.5  -38.5    6.5) 2.5)
 
  ((1 55 1) (1 55 0)  (    42  -35.5      7) 1.1)
 
  ((1 55 2) (1 55 1)  (    38  -36.5      7) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 56, parent index 54
 
  ((1 56 0) (BRANCH-PT 1 55 1)  (    42  -35.5      7) 1.1)
 
  ((1 56 1) (1 56 0)  (  38.5  -31.5      7) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 9
;; Section 57, parent index 52
 
  ((1 57 0) (BRANCH-PT 1 53 1)  (    52  -38.5    7.5) 4.9)
 
  ((1 57 1) (1 57 0)  (    52  -42.5    7.5) 4.8)
 
  ((1 57 2) (1 57 1)  (  48.5    -43    7.5) 1.5)
 
  ((1 57 3) (1 57 2)  (    48    -47    7.5) 1.5)
 
  ((1 57 4) (1 57 3)  (  46.5    -48    7.5) 1.6)
 
  ((1 57 5) (1 57 4)  (  42.5  -45.5    7.5) 1.2)
 
  ((1 57 6) (1 57 5)  (    39    -43    7.5) 1.2)
 
  ((1 57 7) (1 57 6)  (  31.5  -43.5    7.5) 1.2)
 
  ((1 57 8) (1 57 7)  (    28    -42    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 58, parent index 56
 
  ((1 58 0) (BRANCH-PT 1 57 6)  (    39    -43    7.5) 1.2)
 
  ((1 58 1) (1 58 0)  (    39    -40    7.5) 1.2)
 
  ((1 58 2) (1 58 1)  (    39    -38    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 59, parent index 56
 
  ((1 59 0) (BRANCH-PT 1 57 4)  (  46.5    -48    7.5) 1.6)
 
  ((1 59 1) (1 59 0)  (  40.5  -51.5    7.5) 1.2)
 
  ((1 59 2) (1 59 1)  (    37  -49.5    7.5) 1.2)
 
  ((1 59 3) (1 59 2)  (    35    -47    7.5) 1.2)
 
  ((1 59 4) (1 59 3)  (  32.5  -47.5    6.5) 1.2)
 
  ((1 59 5) (1 59 4)  (  29.5    -46    6.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 60, parent index 58
 
  ((1 60 0) (BRANCH-PT 1 59 4)  (  32.5  -47.5    6.5) 1.2)
 
  ((1 60 1) (1 60 0)  (  30.5  -48.5    6.5) 1.2)
 
  ((1 60 2) (1 60 1)  (    27  -49.5    6.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 61, parent index 59
 
  ((1 61 0) (BRANCH-PT 1 60 1)  (  30.5  -48.5    6.5) 1.2)
 
  ((1 61 1) (1 61 0)  (  31.5  -51.5    6.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 62, parent index 58
 
  ((1 62 0) (BRANCH-PT 1 59 1)  (  40.5  -51.5    7.5) 1.2)
 
  ((1 62 1) (1 62 0)  (    37  -53.5    7.5) 1.2)
 
  ((1 62 2) (1 62 1)  (    35  -55.5    7.5) 1.2)
 
  ((1 62 3) (1 62 2)  (  39.5  -57.5    7.5) 1.2)
 
  ((1 62 4) (1 62 3)  (  39.5  -60.5    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 63, parent index 61
 
  ((1 63 0) (BRANCH-PT 1 62 3)  (  39.5  -57.5    7.5) 1.2)
 
  ((1 63 1) (1 63 0)  (  41.5  -57.5    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 64, parent index 61
 
  ((1 64 0) (BRANCH-PT 1 62 2)  (    35  -55.5    7.5) 1.2)
 
  ((1 64 1) (1 64 0)  (    35  -58.5    7.5) 1.2)
 
  ((1 64 2) (1 64 1)  (    27  -58.5    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 65, parent index 63
 
  ((1 65 0) (BRANCH-PT 1 64 1)  (    35  -58.5    7.5) 1.2)
 
  ((1 65 1) (1 65 0)  (  33.5    -62    7.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 66, parent index 56
 
  ((1 66 0) (BRANCH-PT 1 57 1)  (    52  -42.5    7.5) 4.8)
 
  ((1 66 1) (1 66 0)  (  56.5  -47.5      6) 3.9)
 
  ((1 66 2) (1 66 1)  (    60    -51      6) 3.9)
 
  ((1 66 3) (1 66 2)  (  64.5    -53    7.5) 3.9)
 
  ((1 66 4) (1 66 3)  (  67.5  -50.5      8)   3)
 
  ((1 66 5) (1 66 4)  (  69.5    -44      8) 1.7)
 
  ((1 66 6) (1 66 5)  (  68.5    -42      8) 1.7)
 
  ((1 66 7) (1 66 6)  (  66.5  -38.5      8) 1.7)
 
  ((1 66 8) (1 66 7)  (    66    -32      8) 1.7)
 
  ((1 66 9) (1 66 8)  (    63    -31      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 67, parent index 65
 
  ((1 67 0) (BRANCH-PT 1 66 8)  (    66    -32      8) 1.7)
 
  ((1 67 1) (1 67 0)  (  66.5    -29      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 68, parent index 65
 
  ((1 68 0) (BRANCH-PT 1 66 7)  (  66.5  -38.5      8) 1.7)
 
  ((1 68 1) (1 68 0)  (    63    -36      8)   1)
 
  ((1 68 2) (1 68 1)  (    63  -34.5      8)   1)
 
  ((1 68 3) (1 68 2)  (    63  -31.5      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 69, parent index 67
 
  ((1 69 0) (BRANCH-PT 1 68 2)  (    63  -34.5      8)   1)
 
  ((1 69 1) (1 69 0)  (    60  -33.5      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 70, parent index 65
 
  ((1 70 0) (BRANCH-PT 1 66 6)  (  68.5    -42      8) 1.7)
 
  ((1 70 1) (1 70 0)  (    64    -43      8)   1)
 
  ((1 70 2) (1 70 1)  (  63.5    -40      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 71, parent index 69
 
  ((1 71 0) (BRANCH-PT 1 70 1)  (    64    -43      8)   1)
 
  ((1 71 1) (1 71 0)  (  62.5    -43      8)   1)
 
  ((1 71 2) (1 71 1)  (    62  -38.5      8) 1.2)
 
  ((1 71 3) (1 71 2)  (    59  -34.5      8) 1.2)
 
  ((1 71 4) (1 71 3)  (  59.5  -32.5      8) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 72, parent index 70
 
  ((1 72 0) (BRANCH-PT 1 71 3)  (    59  -34.5      8) 1.2)
 
  ((1 72 1) (1 72 0)  (  55.5  -34.5      8) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 73, parent index 70
 
  ((1 73 0) (BRANCH-PT 1 71 1)  (  62.5    -43      8)   1)
 
  ((1 73 1) (1 73 0)  (  60.5    -45      8) 0.6)
 
  ((1 73 2) (1 73 1)  (    57    -42      8) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 74, parent index 65
 
  ((1 74 0) (BRANCH-PT 1 66 5)  (  69.5    -44      8) 1.7)
 
  ((1 74 1) (1 74 0)  (  65.5  -43.5    6.5) 0.6)
 
  ((1 74 2) (1 74 1)  (    63  -46.5    6.5) 0.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 75, parent index 65
 
  ((1 75 0) (BRANCH-PT 1 66 3)  (  64.5    -53    7.5) 3.9)
 
  ((1 75 1) (1 75 0)  (    68  -55.5    7.5)   5)
 
  ((1 75 2) (1 75 1)  (    78  -47.5    7.5) 1.6)
 
  ((1 75 3) (1 75 2)  (  81.5  -42.5    7.5) 1.3)
 
  ((1 75 4) (1 75 3)  (    78  -42.5    7.5) 1.3)
 
  ((1 75 5) (1 75 4)  (    71  -43.5    7.5) 1.3)
 
  ((1 75 6) (1 75 5)  (  71.5    -46    7.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 76, parent index 74
 
  ((1 76 0) (BRANCH-PT 1 75 5)  (    71  -43.5    7.5) 1.3)
 
  ((1 76 1) (1 76 0)  (    74    -41    7.5) 1.3)
 
  ((1 76 2) (1 76 1)  (  71.5  -35.5    7.5) 1.3)
 
  ((1 76 3) (1 76 2)  (  70.5    -32    7.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 77, parent index 75
 
  ((1 77 0) (BRANCH-PT 1 76 1)  (    74    -41    7.5) 1.3)
 
  ((1 77 1) (1 77 0)  (  78.5    -41    7.5) 1.3)
 
  ((1 77 2) (1 77 1)  (  80.5    -38      7) 1.3)
 
  ((1 77 3) (1 77 2)  (    73  -32.5    7.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 78, parent index 76
 
  ((1 78 0) (BRANCH-PT 1 77 2)  (  80.5    -38      7) 1.3)
 
  ((1 78 1) (1 78 0)  (    83  -35.5      7) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 79, parent index 74
 
  ((1 79 0) (BRANCH-PT 1 75 4)  (    78  -42.5    7.5) 1.3)
 
  ((1 79 1) (1 79 0)  (  83.5    -40      7) 1.3)
 
  ((1 79 2) (1 79 1)  (  85.5  -35.5    7.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 80, parent index 74
 
  ((1 80 0) (BRANCH-PT 1 75 3)  (  81.5  -42.5    7.5) 1.3)
 
  ((1 80 1) (1 80 0)  (  84.5  -42.5    7.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 81, parent index 74
 
  ((1 81 0) (BRANCH-PT 1 75 2)  (    78  -47.5    7.5) 1.6)
 
  ((1 81 1) (1 81 0)  (  85.5    -47    7.5) 1.3)
 
  ((1 81 2) (1 81 1)  (    84  -44.5      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 82, parent index 80
 
  ((1 82 0) (BRANCH-PT 1 81 1)  (  85.5    -47    7.5) 1.3)
 
  ((1 82 1) (1 82 0)  (    89  -46.5      8)   1)
 
  ((1 82 2) (1 82 1)  (    95    -43    7.5)   2)
 
  ((1 82 3) (1 82 2)  (  95.5    -37      8) 1.7)
 
  ((1 82 4) (1 82 3)  (    88  -40.5    7.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 83, parent index 81
 
  ((1 83 0) (BRANCH-PT 1 82 3)  (  95.5    -37      8) 1.7)
 
  ((1 83 1) (1 83 0)  (  94.5    -35    8.5) 1.9)
 
  ((1 83 2) (1 83 1)  (    93  -30.5    8.5) 1.5)
 
  ((1 83 3) (1 83 2)  (    87  -32.5      8) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 84, parent index 82
 
  ((1 84 0) (BRANCH-PT 1 83 2)  (    93  -30.5    8.5) 1.5)
 
  ((1 84 1) (1 84 0)  (  92.5    -26    8.5) 1.1)
 
  ((1 84 2) (1 84 1)  (  93.5  -23.5    8.5) 1.1)
 
  ((1 84 3) (1 84 2)  (  92.5    -22    8.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 85, parent index 83
 
  ((1 85 0) (BRANCH-PT 1 84 2)  (  93.5  -23.5    8.5) 1.1)
 
  ((1 85 1) (1 85 0)  (    99  -23.5    8.5) 1.1)
 
  ((1 85 2) (1 85 1)  (   106    -25    8.5) 1.1)
 
  ((1 85 3) (1 85 2)  ( 109.5  -20.5    8.5) 1.1)
 
  ((1 85 4) (1 85 3)  ( 112.5  -15.5      8) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 86, parent index 84
 
  ((1 86 0) (BRANCH-PT 1 85 2)  (   106    -25    8.5) 1.1)
 
  ((1 86 1) (1 86 0)  ( 113.5  -24.5    8.5) 0.9)
 
  ((1 86 2) (1 86 1)  (   118  -20.5    8.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 87, parent index 85
 
  ((1 87 0) (BRANCH-PT 1 86 1)  ( 113.5  -24.5    8.5) 0.9)
 
  ((1 87 1) (1 87 0)  (   119    -28      8) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 88, parent index 82
 
  ((1 88 0) (BRANCH-PT 1 83 1)  (  94.5    -35    8.5) 1.9)
 
  ((1 88 1) (1 88 0)  (   103  -29.5      9) 0.9)
 
  ((1 88 2) (1 88 1)  (   111    -28      9) 0.9)
 
  ((1 88 3) (1 88 2)  ( 113.5    -26    8.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 89, parent index 87
 
  ((1 89 0) (BRANCH-PT 1 88 1)  (   103  -29.5      9) 0.9)
 
  ((1 89 1) (1 89 0)  ( 109.5    -32     11) 0.9)
 
  ((1 89 2) (1 89 1)  ( 114.5  -29.5   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 90, parent index 88
 
  ((1 90 0) (BRANCH-PT 1 89 1)  ( 109.5    -32     11) 0.9)
 
  ((1 90 1) (1 90 0)  ( 119.5  -30.5     11) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 91, parent index 81
 
  ((1 91 0) (BRANCH-PT 1 82 2)  (    95    -43    7.5)   2)
 
  ((1 91 1) (1 91 0)  (    99    -43    6.5) 1.4)
 
  ((1 91 2) (1 91 1)  (   102  -38.5      5) 1.4)
 
  ((1 91 3) (1 91 2)  (   107    -32    5.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 92, parent index 90
 
  ((1 92 0) (BRANCH-PT 1 91 2)  (   102  -38.5      5) 1.4)
 
  ((1 92 1) (1 92 0)  (   107  -36.5      6) 1.4)
 
  ((1 92 2) (1 92 1)  (   101  -32.5      6) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 93, parent index 91
 
  ((1 93 0) (BRANCH-PT 1 92 1)  (   107  -36.5      6) 1.4)
 
  ((1 93 1) (1 93 0)  ( 108.5    -37      6) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 94, parent index 90
 
  ((1 94 0) (BRANCH-PT 1 91 1)  (    99    -43    6.5) 1.4)
 
  ((1 94 1) (1 94 0)  (   102    -43    6.5) 1.5)
 
  ((1 94 2) (1 94 1)  (   109    -46    6.5) 1.5)
 
  ((1 94 3) (1 94 2)  ( 112.5  -40.5      7) 1.5)
 
  ((1 94 4) (1 94 3)  ( 115.5  -34.5      8) 1.5)
 
  ((1 94 5) (1 94 4)  (   121  -32.5      8) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 95, parent index 93
 
  ((1 95 0) (BRANCH-PT 1 94 3)  ( 112.5  -40.5      7) 1.5)
 
  ((1 95 1) (1 95 0)  ( 121.5    -36      7) 1.5)
 
  ((1 95 2) (1 95 1)  (   122    -30      7) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 96, parent index 94
 
  ((1 96 0) (BRANCH-PT 1 95 1)  ( 121.5    -36      7) 1.5)
 
  ((1 96 1) (1 96 0)  (   126    -36    6.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 97, parent index 93
 
  ((1 97 0) (BRANCH-PT 1 94 2)  (   109    -46    6.5) 1.5)
 
  ((1 97 1) (1 97 0)  (   109    -46    8.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 98, parent index 93
 
  ((1 98 0) (BRANCH-PT 1 94 1)  (   102    -43    6.5) 1.5)
 
  ((1 98 1) (1 98 0)  ( 102.5  -48.5     11) 1.5)
 
  ((1 98 2) (1 98 1)  (  97.5  -49.5     11)   1)
 
  ((1 98 3) (1 98 2)  (  90.5  -48.5     11)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 99, parent index 81
 
  ((1 99 0) (BRANCH-PT 1 82 1)  (    89  -46.5      8)   1)
 
  ((1 99 1) (1 99 0)  (    89  -48.5      8)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 100, parent index 74
 
  ((1 100 0) (BRANCH-PT 1 75 1)  (    68  -55.5    7.5)   5)
 
  ((1 100 1) (1 100 0)  (  71.5  -56.5      7) 6.6)
 
  ((1 100 2) (1 100 1)  (    81    -53     10) 3.5)
 
  ((1 100 3) (1 100 2)  (  79.5    -48     10) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 101, parent index 99
 
  ((1 101 0) (BRANCH-PT 1 100 2)  (    81    -53     10) 3.5)
 
  ((1 101 1) (1 101 0)  (  87.5  -55.5     10)   5)
 
  ((1 101 2) (1 101 1)  (  91.5    -51     10)   2)
 
  ((1 101 3) (1 101 2)  ( 101.5    -53    9.5) 2.6)
 
  ((1 101 4) (1 101 3)  (   109  -49.5    9.5) 1.9)
 
  ((1 101 5) (1 101 4)  (   111    -45    9.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 102, parent index 100
 
  ((1 102 0) (BRANCH-PT 1 101 4)  (   109  -49.5    9.5) 1.9)
 
  ((1 102 1) (1 102 0)  ( 114.5  -48.5    9.5) 1.7)
 
  ((1 102 2) (1 102 1)  ( 119.5    -43    9.5) 1.7)
 
  ((1 102 3) (1 102 2)  (   124  -37.5    9.5) 1.2)
 
  ((1 102 4) (1 102 3)  (   124  -33.5    9.5) 1.2)
 
  ((1 102 5) (1 102 4)  (   122    -25    9.5) 1.2)
 
  ((1 102 6) (1 102 5)  ( 120.5    -25    9.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 103, parent index 101
 
  ((1 103 0) (BRANCH-PT 1 102 5)  (   122    -25    9.5) 1.2)
 
  ((1 103 1) (1 103 0)  (   122  -18.5     10) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 104, parent index 101
 
  ((1 104 0) (BRANCH-PT 1 102 4)  (   124  -33.5    9.5) 1.2)
 
  ((1 104 1) (1 104 0)  (   126    -28    9.5) 1.2)
 
  ((1 104 2) (1 104 1)  ( 128.5  -23.5     10) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 105, parent index 101
 
  ((1 105 0) (BRANCH-PT 1 102 2)  ( 119.5    -43    9.5) 1.7)
 
  ((1 105 1) (1 105 0)  (   129    -37     10) 1.3)
 
  ((1 105 2) (1 105 1)  (   136  -28.5     10) 1.3)
 
  ((1 105 3) (1 105 2)  ( 136.5  -19.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 106, parent index 104
 
  ((1 106 0) (BRANCH-PT 1 105 2)  (   136  -28.5     10) 1.3)
 
  ((1 106 1) (1 106 0)  (   144  -26.5     10) 1.3)
 
  ((1 106 2) (1 106 1)  (   148  -27.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 107, parent index 104
 
  ((1 107 0) (BRANCH-PT 1 105 1)  (   129    -37     10) 1.3)
 
  ((1 107 1) (1 107 0)  (   141  -33.5     10) 1.3)
 
  ((1 107 2) (1 107 1)  ( 150.5  -29.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 108, parent index 106
 
  ((1 108 0) (BRANCH-PT 1 107 1)  (   141  -33.5     10) 1.3)
 
  ((1 108 1) (1 108 0)  ( 148.5  -36.5    8.5) 1.1)
 
  ((1 108 2) (1 108 1)  (   164    -39     10) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 109, parent index 101
 
  ((1 109 0) (BRANCH-PT 1 102 1)  ( 114.5  -48.5    9.5) 1.7)
 
  ((1 109 1) (1 109 0)  (   129  -47.5     11) 1.1)
 
  ((1 109 2) (1 109 1)  ( 140.5  -49.5   10.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 110, parent index 108
 
  ((1 110 0) (BRANCH-PT 1 109 1)  (   129  -47.5     11) 1.1)
 
  ((1 110 1) (1 110 0)  (   147  -43.5     11) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 111, parent index 100
 
  ((1 111 0) (BRANCH-PT 1 101 3)  ( 101.5    -53    9.5) 2.6)
 
  ((1 111 1) (1 111 0)  (   106  -58.5     11) 1.1)
 
  ((1 111 2) (1 111 1)  ( 122.5  -54.5     11) 1.1)
 
  ((1 111 3) (1 111 2)  (   123  -49.5     10)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 112, parent index 110
 
  ((1 112 0) (BRANCH-PT 1 111 2)  ( 122.5  -54.5     11) 1.1)
 
  ((1 112 1) (1 112 0)  (   127  -57.5     11) 1.8)
 
  ((1 112 2) (1 112 1)  (   136  -57.5     11) 1.8)
 
  ((1 112 3) (1 112 2)  (   138  -57.5    9.5) 1.8)
 
  ((1 112 4) (1 112 3)  (   144    -53    9.5) 1.8)
 
  ((1 112 5) (1 112 4)  ( 149.5  -52.5     10) 1.7)
 
  ((1 112 6) (1 112 5)  (   149  -58.5     10) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 113, parent index 111
 
  ((1 113 0) (BRANCH-PT 1 112 5)  ( 149.5  -52.5     10) 1.7)
 
  ((1 113 1) (1 113 0)  ( 155.5    -54     10) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 114, parent index 111
 
  ((1 114 0) (BRANCH-PT 1 112 3)  (   138  -57.5    9.5) 1.8)
 
  ((1 114 1) (1 114 0)  (   138  -58.5    9.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 115, parent index 111
 
  ((1 115 0) (BRANCH-PT 1 112 1)  (   127  -57.5     11) 1.8)
 
  ((1 115 1) (1 115 0)  (   127    -60      9) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 116, parent index 110
 
  ((1 116 0) (BRANCH-PT 1 111 1)  (   106  -58.5     11) 1.1)
 
  ((1 116 1) (1 116 0)  (   113  -60.5     10) 1.1)
 
  ((1 116 2) (1 116 1)  (   116    -66   10.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 117, parent index 115
 
  ((1 117 0) (BRANCH-PT 1 116 1)  (   113  -60.5     10) 1.1)
 
  ((1 117 1) (1 117 0)  ( 124.5  -63.5     10) 1.1)
 
  ((1 117 2) (1 117 1)  (   132  -62.5     10) 1.2)
 
  ((1 117 3) (1 117 2)  ( 140.5    -61   10.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 118, parent index 116
 
  ((1 118 0) (BRANCH-PT 1 117 2)  (   132  -62.5     10) 1.2)
 
  ((1 118 1) (1 118 0)  ( 138.5  -66.5     10)   1)
 
  ((1 118 2) (1 118 1)  (   148  -65.5     10)   1)
 
  ((1 118 3) (1 118 2)  (   157  -67.5   10.5) 0.9)
 
  ((1 118 4) (1 118 3)  (   157    -71   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 119, parent index 117
 
  ((1 119 0) (BRANCH-PT 1 118 3)  (   157  -67.5   10.5) 0.9)
 
  ((1 119 1) (1 119 0)  ( 160.5    -68   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 120, parent index 116
 
  ((1 120 0) (BRANCH-PT 1 117 1)  ( 124.5  -63.5     10) 1.1)
 
  ((1 120 1) (1 120 0)  ( 129.5    -70     10) 0.9)
 
  ((1 120 2) (1 120 1)  ( 134.5    -72   10.5) 0.9)
 
  ((1 120 3) (1 120 2)  ( 138.5    -72   10.5) 0.9)
 
  ((1 120 4) (1 120 3)  ( 149.5  -87.5   10.5) 0.9)
 
  ((1 120 5) (1 120 4)  ( 149.5  -87.5   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 121, parent index 119
 
  ((1 121 0) (BRANCH-PT 1 120 3)  ( 138.5    -72   10.5) 0.9)
 
  ((1 121 1) (1 121 0)  (   143    -72   10.5) 0.9)
 
  ((1 121 2) (1 121 1)  (   145    -74     11) 0.9)
 
  ((1 121 3) (1 121 2)  ( 152.5  -74.5     12) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 122, parent index 120
 
  ((1 122 0) (BRANCH-PT 1 121 2)  (   145    -74     11) 0.9)
 
  ((1 122 1) (1 122 0)  (   151  -80.5   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 12
;; Section 123, parent index 100
 
  ((1 123 0) (BRANCH-PT 1 101 1)  (  87.5  -55.5     10)   5)
 
  ((1 123 1) (1 123 0)  (  96.5  -65.5     11) 3.7)
 
  ((1 123 2) (1 123 1)  ( 109.5  -71.5   10.5)   3)
 
  ((1 123 3) (1 123 2)  (   120  -75.5   10.5)   3)
 
  ((1 123 4) (1 123 3)  (   123  -80.5      9)   4)
 
  ((1 123 5) (1 123 4)  ( 126.5    -82     11) 1.8)
 
  ((1 123 6) (1 123 5)  ( 128.5    -89     11) 1.8)
 
  ((1 123 7) (1 123 6)  ( 131.5  -87.5     12) 1.1)
 
  ((1 123 8) (1 123 7)  ( 131.5    -81   11.5) 1.1)
 
  ((1 123 9) (1 123 8)  (   126    -76   11.5) 1.1)
 
  ((1 123 10) (1 123 9)  (   124    -74   11.5) 1.1)
 
  ((1 123 11) (1 123 10)  ( 128.5    -72   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 124, parent index 122
 
  ((1 124 0) (BRANCH-PT 1 123 10)  (   124    -74   11.5) 1.1)
 
  ((1 124 1) (1 124 0)  (   117    -70     11) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 125, parent index 122
 
  ((1 125 0) (BRANCH-PT 1 123 6)  ( 128.5    -89     11) 1.8)
 
  ((1 125 1) (1 125 0)  ( 130.5    -90   11.5) 1.1)
 
  ((1 125 2) (1 125 1)  (   135    -90     13) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 126, parent index 124
 
  ((1 126 0) (BRANCH-PT 1 125 1)  ( 130.5    -90   11.5) 1.1)
 
  ((1 126 1) (1 126 0)  (   127    -94     12) 1.1)
 
  ((1 126 2) (1 126 1)  ( 130.5  -97.5     12) 1.1)
 
  ((1 126 3) (1 126 2)  (   137    -94   11.5) 1.1)
 
  ((1 126 4) (1 126 3)  ( 137.5  -89.5   11.5) 1.1)
 
  ((1 126 5) (1 126 4)  (   138    -85   11.5) 1.1)
 
  ((1 126 6) (1 126 5)  ( 139.5    -76     12) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 127, parent index 125
 
  ((1 127 0) (BRANCH-PT 1 126 5)  (   138    -85   11.5) 1.1)
 
  ((1 127 1) (1 127 0)  (   142  -80.5   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 128, parent index 125
 
  ((1 128 0) (BRANCH-PT 1 126 4)  ( 137.5  -89.5   11.5) 1.1)
 
  ((1 128 1) (1 128 0)  ( 143.5    -90     11) 1.1)
 
  ((1 128 2) (1 128 1)  (   146    -84   10.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 129, parent index 127
 
  ((1 129 0) (BRANCH-PT 1 128 1)  ( 143.5    -90     11) 1.1)
 
  ((1 129 1) (1 129 0)  (   148  -91.5     12) 1.1)
 
  ((1 129 2) (1 129 1)  ( 154.5  -83.5     12) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 130, parent index 128
 
  ((1 130 0) (BRANCH-PT 1 129 1)  (   148  -91.5     12) 1.1)
 
  ((1 130 1) (1 130 0)  ( 157.5    -97     12) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 131, parent index 125
 
  ((1 131 0) (BRANCH-PT 1 126 3)  (   137    -94   11.5) 1.1)
 
  ((1 131 1) (1 131 0)  (   143    -95   11.5) 1.1)
 
  ((1 131 2) (1 131 1)  ( 149.5    -99   11.5) 1.1)
 
  ((1 131 3) (1 131 2)  (   148    -94   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 132, parent index 130
 
  ((1 132 0) (BRANCH-PT 1 131 2)  ( 149.5    -99   11.5) 1.1)
 
  ((1 132 1) (1 132 0)  ( 151.5  -99.5   11.5) 1.1)
 
  ((1 132 2) (1 132 1)  (   152   -107   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 133, parent index 131
 
  ((1 133 0) (BRANCH-PT 1 132 1)  ( 151.5  -99.5   11.5) 1.1)
 
  ((1 133 1) (1 133 0)  ( 160.5 -104.5   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 134, parent index 125
 
  ((1 134 0) (BRANCH-PT 1 126 2)  ( 130.5  -97.5     12) 1.1)
 
  ((1 134 1) (1 134 0)  (   134 -106.5     12) 1.1)
 
  ((1 134 2) (1 134 1)  (   134 -111.5   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 9
;; Section 135, parent index 122
 
  ((1 135 0) (BRANCH-PT 1 123 4)  (   123  -80.5      9)   4)
 
  ((1 135 1) (1 135 0)  ( 119.5  -95.5     10) 4.2)
 
  ((1 135 2) (1 135 1)  (   116    -97     10) 1.6)
 
  ((1 135 3) (1 135 2)  ( 112.5    -87     11) 1.6)
 
  ((1 135 4) (1 135 3)  (   104    -90     11) 1.3)
 
  ((1 135 5) (1 135 4)  (    95  -88.5     11) 1.3)
 
  ((1 135 6) (1 135 5)  (    89    -81     11) 1.3)
 
  ((1 135 7) (1 135 6)  (  88.5    -75     11) 1.3)
 
  ((1 135 8) (1 135 7)  (    81  -64.5     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 136, parent index 134
 
  ((1 136 0) (BRANCH-PT 1 135 3)  ( 112.5    -87     11) 1.6)
 
  ((1 136 1) (1 136 0)  ( 113.5    -83     11) 1.1)
 
  ((1 136 2) (1 136 1)  ( 113.5    -79   10.5) 1.1)
 
  ((1 136 3) (1 136 2)  ( 111.5    -78     11) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 137, parent index 135
 
  ((1 137 0) (BRANCH-PT 1 136 2)  ( 113.5    -79   10.5) 1.1)
 
  ((1 137 1) (1 137 0)  (   119  -80.5   10.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 138, parent index 135
 
  ((1 138 0) (BRANCH-PT 1 136 1)  ( 113.5    -83     11) 1.1)
 
  ((1 138 1) (1 138 0)  ( 109.5  -83.5     11) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 139, parent index 134
 
  ((1 139 0) (BRANCH-PT 1 135 1)  ( 119.5  -95.5     10) 4.2)
 
  ((1 139 1) (1 139 0)  (   120  -98.5     11) 4.3)
 
  ((1 139 2) (1 139 1)  (   116 -105.5   11.5) 1.9)
 
  ((1 139 3) (1 139 2)  (   110 -118.5     11) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 140, parent index 138
 
  ((1 140 0) (BRANCH-PT 1 139 2)  (   116 -105.5   11.5) 1.9)
 
  ((1 140 1) (1 140 0)  ( 116.5   -120   11.5) 1.8)
 
  ((1 140 2) (1 140 1)  ( 113.5   -130     11) 1.2)
 
  ((1 140 3) (1 140 2)  ( 112.5   -136     11) 1.2)
 
  ((1 140 4) (1 140 3)  (   101   -137   11.5) 1.2)
 
  ((1 140 5) (1 140 4)  (  94.5   -145   11.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 141, parent index 139
 
  ((1 141 0) (BRANCH-PT 1 140 3)  ( 112.5   -136     11) 1.2)
 
  ((1 141 1) (1 141 0)  (   112   -141     11) 1.2)
 
  ((1 141 2) (1 141 1)  ( 108.5   -145   11.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 142, parent index 140
 
  ((1 142 0) (BRANCH-PT 1 141 1)  (   112   -141     11) 1.2)
 
  ((1 142 1) (1 142 0)  (   114   -146     11) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 143, parent index 139
 
  ((1 143 0) (BRANCH-PT 1 140 1)  ( 116.5   -120   11.5) 1.8)
 
  ((1 143 1) (1 143 0)  (   118   -138     12) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 14
;; Section 144, parent index 138
 
  ((1 144 0) (BRANCH-PT 1 139 1)  (   120  -98.5     11) 4.3)
 
  ((1 144 1) (1 144 0)  (   120   -108     13) 3.1)
 
  ((1 144 2) (1 144 1)  (   120   -116     13) 3.1)
 
  ((1 144 3) (1 144 2)  (   121   -120     13) 3.1)
 
  ((1 144 4) (1 144 3)  (   119   -123   12.5) 1.6)
 
  ((1 144 5) (1 144 4)  (   118 -128.5   12.5) 1.6)
 
  ((1 144 6) (1 144 5)  (   121   -133     13) 1.6)
 
  ((1 144 7) (1 144 6)  (   121   -140   12.5) 1.6)
 
  ((1 144 8) (1 144 7)  ( 119.5 -145.5   12.5) 1.6)
 
  ((1 144 9) (1 144 8)  ( 123.5   -151    9.5) 1.8)
 
  ((1 144 10) (1 144 9)  ( 128.5   -162     10) 1.7)
 
  ((1 144 11) (1 144 10)  (   132   -171     10) 1.2)
 
  ((1 144 12) (1 144 11)  ( 136.5 -171.5     10) 1.2)
 
  ((1 144 13) (1 144 12)  (   137 -176.5     10) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 145, parent index 143
 
  ((1 145 0) (BRANCH-PT 1 144 11)  (   132   -171     10) 1.2)
 
  ((1 145 1) (1 145 0)  ( 132.5   -173     10) 0.9)
 
  ((1 145 2) (1 145 1)  (   136 -175.5     10) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 146, parent index 144
 
  ((1 146 0) (BRANCH-PT 1 145 1)  ( 132.5   -173     10) 0.9)
 
  ((1 146 1) (1 146 0)  ( 131.5   -177    8.5) 0.9)
 
  ((1 146 2) (1 146 1)  ( 125.5 -179.5    8.5) 0.9)
 
  ((1 146 3) (1 146 2)  (   124   -183    8.5) 0.9)
 
  ((1 146 4) (1 146 3)  ( 122.5 -187.5      9) 0.9)
 
  ((1 146 5) (1 146 4)  ( 122.5   -190      9) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 147, parent index 145
 
  ((1 147 0) (BRANCH-PT 1 146 4)  ( 122.5 -187.5      9) 0.9)
 
  ((1 147 1) (1 147 0)  ( 128.5 -192.5    9.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 148, parent index 145
 
  ((1 148 0) (BRANCH-PT 1 146 3)  (   124   -183    8.5) 0.9)
 
  ((1 148 1) (1 148 0)  ( 128.5 -192.5    9.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 149, parent index 143
 
  ((1 149 0) (BRANCH-PT 1 144 9)  ( 123.5   -151    9.5) 1.8)
 
  ((1 149 1) (1 149 0)  ( 123.5 -158.5   10.5) 0.9)
 
  ((1 149 2) (1 149 1)  (   123 -161.5    9.5) 0.9)
 
  ((1 149 3) (1 149 2)  ( 120.5 -161.5    9.5) 0.9)
 
  ((1 149 4) (1 149 3)  (   120   -160    9.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 150, parent index 148
 
  ((1 150 0) (BRANCH-PT 1 149 3)  ( 120.5 -161.5    9.5) 0.9)
 
  ((1 150 1) (1 150 0)  (   117   -167      9) 0.9)
 
  ((1 150 2) (1 150 1)  (   120 -171.5      9) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 151, parent index 149
 
  ((1 151 0) (BRANCH-PT 1 150 1)  (   117   -167      9) 0.9)
 
  ((1 151 1) (1 151 0)  ( 114.5   -171      9) 0.9)
 
  ((1 151 2) (1 151 1)  (   115 -178.5   10.5) 0.9)
 
  ((1 151 3) (1 151 2)  (   115   -190    8.5) 0.9)
 
  ((1 151 4) (1 151 3)  (   115   -192    8.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 152, parent index 150
 
  ((1 152 0) (BRANCH-PT 1 151 3)  (   115   -190    8.5) 0.9)
 
  ((1 152 1) (1 152 0)  (   119   -192    8.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 153, parent index 148
 
  ((1 153 0) (BRANCH-PT 1 149 2)  (   123 -161.5    9.5) 0.9)
 
  ((1 153 1) (1 153 0)  (   125 -166.5     10) 0.9)
 
  ((1 153 2) (1 153 1)  (   119 -175.5   10.5) 0.9)
 
  ((1 153 3) (1 153 2)  (   115   -186   10.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 154, parent index 152
 
  ((1 154 0) (BRANCH-PT 1 153 1)  (   125 -166.5     10) 0.9)
 
  ((1 154 1) (1 154 0)  (   128 -170.5      9) 0.9)
 
  ((1 154 2) (1 154 1)  (   124   -174      9) 0.9)
 
  ((1 154 3) (1 154 2)  (   121   -182    9.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 155, parent index 143
 
  ((1 155 0) (BRANCH-PT 1 144 3)  (   121   -120     13) 3.1)
 
  ((1 155 1) (1 155 0)  ( 123.5   -125     12) 5.4)
 
  ((1 155 2) (1 155 1)  ( 130.5 -123.5     12) 1.8)
 
  ((1 155 3) (1 155 2)  (   135   -121     12) 1.2)
 
  ((1 155 4) (1 155 3)  ( 133.5   -118     12)   1)
 
  ((1 155 5) (1 155 4)  (   135   -113     12)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 156, parent index 154
 
  ((1 156 0) (BRANCH-PT 1 155 4)  ( 133.5   -118     12)   1)
 
  ((1 156 1) (1 156 0)  (   128 -113.5     12)   1)
 
  ((1 156 2) (1 156 1)  (   129 -105.5     12)   1)
 
  ((1 156 3) (1 156 2)  ( 127.5  -98.5     12)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 157, parent index 154
 
  ((1 157 0) (BRANCH-PT 1 155 3)  (   135   -121     12) 1.2)
 
  ((1 157 1) (1 157 0)  (   139 -113.5     11)   1)
 
  ((1 157 2) (1 157 1)  (   139 -101.5   11.5)   1)
 
  ((1 157 3) (1 157 2)  ( 137.5  -95.5   11.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 158, parent index 156
 
  ((1 158 0) (BRANCH-PT 1 157 2)  (   139 -101.5   11.5)   1)
 
  ((1 158 1) (1 158 0)  (   141 -100.5   11.5)   1)
 
  ((1 158 2) (1 158 1)  (   141    -98     12)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 159, parent index 154
 
  ((1 159 0) (BRANCH-PT 1 155 2)  ( 130.5 -123.5     12) 1.8)
 
  ((1 159 1) (1 159 0)  (   136 -124.5     12)   1)
 
  ((1 159 2) (1 159 1)  (   139   -120   12.5)   1)
 
  ((1 159 3) (1 159 2)  ( 143.5 -114.5   12.5)   1)
 
  ((1 159 4) (1 159 3)  (   143   -100     14)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 160, parent index 158
 
  ((1 160 0) (BRANCH-PT 1 159 3)  ( 143.5 -114.5   12.5)   1)
 
  ((1 160 1) (1 160 0)  ( 144.5   -115     12)   1)
 
  ((1 160 2) (1 160 1)  (   149 -116.5     13)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 161, parent index 159
 
  ((1 161 0) (BRANCH-PT 1 160 1)  ( 144.5   -115     12)   1)
 
  ((1 161 1) (1 161 0)  ( 149.5   -114     12)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 162, parent index 158
 
  ((1 162 0) (BRANCH-PT 1 159 1)  (   136 -124.5     12)   1)
 
  ((1 162 1) (1 162 0)  (   149   -119     11)   1)
 
  ((1 162 2) (1 162 1)  ( 152.5   -118     12)   1)
 
  ((1 162 3) (1 162 2)  ( 152.5   -115   12.5)   1)
 
  ((1 162 4) (1 162 3)  ( 151.5   -114   12.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 163, parent index 161
 
  ((1 163 0) (BRANCH-PT 1 162 3)  ( 152.5   -115   12.5)   1)
 
  ((1 163 1) (1 163 0)  ( 155.5 -112.5   12.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 164, parent index 161
 
  ((1 164 0) (BRANCH-PT 1 162 2)  ( 152.5   -118     12)   1)
 
  ((1 164 1) (1 164 0)  ( 158.5   -116     12)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 165, parent index 154
 
  ((1 165 0) (BRANCH-PT 1 155 1)  ( 123.5   -125     12) 5.4)
 
  ((1 165 1) (1 165 0)  (   129   -127   11.5) 3.4)
 
  ((1 165 2) (1 165 1)  (   133 -128.5     11) 3.4)
 
  ((1 165 3) (1 165 2)  (   131 -133.5     11) 1.5)
 
  ((1 165 4) (1 165 3)  ( 137.5 -137.5     11) 1.5)
 
  ((1 165 5) (1 165 4)  ( 141.5 -142.5     11) 1.5)
 
  ((1 165 6) (1 165 5)  ( 147.5   -144     11) 1.2)
 
  ((1 165 7) (1 165 6)  (   152   -147     11) 1.2)
 
  ((1 165 8) (1 165 7)  (   156 -149.5     11) 1.2)
 
  ((1 165 9) (1 165 8)  ( 160.5 -156.5     11) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 166, parent index 164
 
  ((1 166 0) (BRANCH-PT 1 165 6)  ( 147.5   -144     11) 1.2)
 
  ((1 166 1) (1 166 0)  ( 151.5 -141.5     11) 1.2)
 
  ((1 166 2) (1 166 1)  (   152   -139     11) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 167, parent index 165
 
  ((1 167 0) (BRANCH-PT 1 166 1)  ( 151.5 -141.5     11) 1.2)
 
  ((1 167 1) (1 167 0)  ( 153.5 -145.5     11) 1.2)
 
  ((1 167 2) (1 167 1)  (   159   -149     11)   1)
 
  ((1 167 3) (1 167 2)  (   160   -151     11)   1)
 
  ((1 167 4) (1 167 3)  (   160   -148     11)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 168, parent index 166
 
  ((1 168 0) (BRANCH-PT 1 167 3)  (   160   -151     11)   1)
 
  ((1 168 1) (1 168 0)  (   160 -153.5     11)   1)
 
  ((1 168 2) (1 168 1)  ( 164.5   -160     11)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 12
;; Section 169, parent index 164
 
  ((1 169 0) (BRANCH-PT 1 165 2)  (   133 -128.5     11) 3.4)
 
  ((1 169 1) (1 169 0)  ( 142.5 -126.5   12.5) 1.6)
 
  ((1 169 2) (1 169 1)  (   150 -123.5   12.5) 1.1)
 
  ((1 169 3) (1 169 2)  ( 156.5 -119.5     13) 1.1)
 
  ((1 169 4) (1 169 3)  ( 160.5 -116.5     13) 1.1)
 
  ((1 169 5) (1 169 4)  ( 165.5 -109.5   13.5) 1.1)
 
  ((1 169 6) (1 169 5)  (   168   -112   13.5) 1.5)
 
  ((1 169 7) (1 169 6)  (   173   -112     13) 1.5)
 
  ((1 169 8) (1 169 7)  (   179 -108.5   12.5) 1.5)
 
  ((1 169 9) (1 169 8)  (   185   -114     13) 1.2)
 
  ((1 169 10) (1 169 9)  (   189 -112.5   12.5) 1.2)
 
  ((1 169 11) (1 169 10)  (   190 -112.5   12.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 170, parent index 168
 
  ((1 170 0) (BRANCH-PT 1 169 8)  (   179 -108.5   12.5) 1.5)
 
  ((1 170 1) (1 170 0)  (   184   -106   12.5) 1.1)
 
  ((1 170 2) (1 170 1)  ( 179.5 -102.5   12.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 171, parent index 169
 
  ((1 171 0) (BRANCH-PT 1 170 1)  (   184   -106   12.5) 1.1)
 
  ((1 171 1) (1 171 0)  (   187 -106.5   12.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 172, parent index 168
 
  ((1 172 0) (BRANCH-PT 1 169 6)  (   168   -112   13.5) 1.5)
 
  ((1 172 1) (1 172 0)  (   167   -119     12) 1.1)
 
  ((1 172 2) (1 172 1)  (   170   -123     12) 1.1)
 
  ((1 172 3) (1 172 2)  ( 175.5   -124     12) 1.1)
 
  ((1 172 4) (1 172 3)  (   183   -124     12) 1.1)
 
  ((1 172 5) (1 172 4)  (   186 -127.5     12) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 173, parent index 168
 
  ((1 173 0) (BRANCH-PT 1 169 2)  (   150 -123.5   12.5) 1.1)
 
  ((1 173 1) (1 173 0)  (   160 -121.5   11.5) 1.1)
 
  ((1 173 2) (1 173 1)  (   168   -125   11.5) 0.9)
 
  ((1 173 3) (1 173 2)  ( 171.5 -125.5   11.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 174, parent index 172
 
  ((1 174 0) (BRANCH-PT 1 173 2)  (   168   -125   11.5) 0.9)
 
  ((1 174 1) (1 174 0)  ( 169.5 -122.5   11.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 175, parent index 168
 
  ((1 175 0) (BRANCH-PT 1 169 1)  ( 142.5 -126.5   12.5) 1.6)
 
  ((1 175 1) (1 175 0)  (   153 -130.5     12) 1.1)
 
  ((1 175 2) (1 175 1)  (   161   -129     14) 1.1)
 
  ((1 175 3) (1 175 2)  ( 167.5 -132.5     14) 1.5)
 
  ((1 175 4) (1 175 3)  ( 170.5 -135.5   13.5) 1.5)
 
  ((1 175 5) (1 175 4)  (   174 -134.5   13.5) 1.5)
 
  ((1 175 6) (1 175 5)  ( 176.5 -129.5     14) 1.5)
 
  ((1 175 7) (1 175 6)  (   180 -118.5     15) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 176, parent index 174
 
  ((1 176 0) (BRANCH-PT 1 175 1)  (   153 -130.5     12) 1.1)
 
  ((1 176 1) (1 176 0)  ( 161.5 -137.5     12) 1.2)
 
  ((1 176 2) (1 176 1)  (   169 -144.5   12.5) 1.2)
 
  ((1 176 3) (1 176 2)  ( 168.5   -153     13) 1.2)
 
  ((1 176 4) (1 176 3)  (   175 -155.5   13.5) 1.2)
 
  ((1 176 5) (1 176 4)  (   186   -154     11) 1.2)
 
  ((1 176 6) (1 176 5)  (   201   -153     11) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 177, parent index 99
 
  ((1 177 0) (BRANCH-PT 1 100 1)  (  71.5  -56.5      7) 6.6)
 
  ((1 177 1) (1 177 0)  (  72.5    -62     10) 4.2)
 
  ((1 177 2) (1 177 1)  (  79.5    -68     10) 4.2)
 
  ((1 177 3) (1 177 2)  (    84    -66     12) 2.2)
 
  ((1 177 4) (1 177 3)  (  91.5    -74     13) 2.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 178, parent index 176
 
  ((1 178 0) (BRANCH-PT 1 177 3)  (    84    -66     12) 2.2)
 
  ((1 178 1) (1 178 0)  (  92.5  -72.5     12) 1.7)
 
  ((1 178 2) (1 178 1)  (  98.5    -81   11.5) 1.7)
 
  ((1 178 3) (1 178 2)  (   102    -81   11.5) 1.7)
 
  ((1 178 4) (1 178 3)  ( 107.5  -78.5   11.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 179, parent index 177
 
  ((1 179 0) (BRANCH-PT 1 178 3)  (   102    -81   11.5) 1.7)
 
  ((1 179 1) (1 179 0)  (   105  -83.5   11.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 180, parent index 177
 
  ((1 180 0) (BRANCH-PT 1 178 2)  (  98.5    -81   11.5) 1.7)
 
  ((1 180 1) (1 180 0)  (  98.5  -87.5   11.5) 1.7)
 
  ((1 180 2) (1 180 1)  (  98.5    -95   11.5) 1.7)
 
  ((1 180 3) (1 180 2)  (    99    -99   11.5) 1.4)
 
  ((1 180 4) (1 180 3)  ( 101.5 -101.5   11.5) 1.4)
 
  ((1 180 5) (1 180 4)  (   106 -107.5     11) 1.4)
 
  ((1 180 6) (1 180 5)  (   110   -113     11) 1.4)
 
  ((1 180 7) (1 180 6)  ( 105.5   -120     11) 1.1)
 
  ((1 180 8) (1 180 7)  ( 104.5 -126.5     11) 1.1)
 
  ((1 180 9) (1 180 8)  (   109 -130.5     12) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 181, parent index 179
 
  ((1 181 0) (BRANCH-PT 1 180 7)  ( 105.5   -120     11) 1.1)
 
  ((1 181 1) (1 181 0)  (   101 -127.5   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 182, parent index 176
 
  ((1 182 0) (BRANCH-PT 1 177 2)  (  79.5    -68     10) 4.2)
 
  ((1 182 1) (1 182 0)  (  80.5    -70     10) 3.4)
 
  ((1 182 2) (1 182 1)  (  88.5    -78    9.5) 1.8)
 
  ((1 182 3) (1 182 2)  (  85.5    -85   11.5) 1.8)
 
  ((1 182 4) (1 182 3)  (  87.5  -89.5     11) 1.3)
 
  ((1 182 5) (1 182 4)  (    90  -91.5     11) 1.3)
 
  ((1 182 6) (1 182 5)  (  90.5    -96   12.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 183, parent index 181
 
  ((1 183 0) (BRANCH-PT 1 182 5)  (    90  -91.5     11) 1.3)
 
  ((1 183 1) (1 183 0)  (  96.5    -97     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 184, parent index 181
 
  ((1 184 0) (BRANCH-PT 1 182 2)  (  88.5    -78    9.5) 1.8)
 
  ((1 184 1) (1 184 0)  (    95    -77   11.5) 2.1)
 
  ((1 184 2) (1 184 1)  (   100    -86     13) 1.7)
 
  ((1 184 3) (1 184 2)  (  97.5  -97.5     12) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 185, parent index 183
 
  ((1 185 0) (BRANCH-PT 1 184 1)  (    95    -77   11.5) 2.1)
 
  ((1 185 1) (1 185 0)  (   101  -76.5     12) 1.5)
 
  ((1 185 2) (1 185 1)  ( 109.5  -73.5     14) 1.5)
 
  ((1 185 3) (1 185 2)  ( 114.5  -67.5    9.5) 1.5)
 
  ((1 185 4) (1 185 3)  ( 117.5  -66.5    9.5) 1.5)
 
  ((1 185 5) (1 185 4)  (   121    -64   10.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 186, parent index 184
 
  ((1 186 0) (BRANCH-PT 1 185 4)  ( 117.5  -66.5    9.5) 1.5)
 
  ((1 186 1) (1 186 0)  ( 122.5    -68    9.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 187, parent index 181
 
  ((1 187 0) (BRANCH-PT 1 182 1)  (  80.5    -70     10) 3.4)
 
  ((1 187 1) (1 187 0)  (  80.5    -78     12) 3.4)
 
  ((1 187 2) (1 187 1)  (  78.5    -86     12)   6)
 
  ((1 187 3) (1 187 2)  (  73.5  -85.5     12) 2.1)
 
  ((1 187 4) (1 187 3)  (  70.5  -89.5     12) 1.4)
 
  ((1 187 5) (1 187 4)  (    64  -91.5     12) 1.4)
 
  ((1 187 6) (1 187 5)  (  55.5  -95.5     12) 1.4)
 
  ((1 187 7) (1 187 6)  (    54  -97.5   11.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 188, parent index 186
 
  ((1 188 0) (BRANCH-PT 1 187 3)  (  73.5  -85.5     12) 2.1)
 
  ((1 188 1) (1 188 0)  (  71.5    -84     12) 1.3)
 
  ((1 188 2) (1 188 1)  (    63    -83   12.5) 1.3)
 
  ((1 188 3) (1 188 2)  (    59    -86   12.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 189, parent index 187
 
  ((1 189 0) (BRANCH-PT 1 188 1)  (  71.5    -84     12) 1.3)
 
  ((1 189 1) (1 189 0)  (    68  -70.5     13) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 190, parent index 186
 
  ((1 190 0) (BRANCH-PT 1 187 2)  (  78.5    -86     12)   6)
 
  ((1 190 1) (1 190 0)  (    80  -90.5     12) 3.9)
 
  ((1 190 2) (1 190 1)  (    78    -99   11.5) 5.8)
 
  ((1 190 3) (1 190 2)  (  71.5   -101   12.5)   2)
 
  ((1 190 4) (1 190 3)  (    74 -108.5   12.5)   2)
 
  ((1 190 5) (1 190 4)  (  78.5   -110     12) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 191, parent index 189
 
  ((1 191 0) (BRANCH-PT 1 190 3)  (  71.5   -101   12.5)   2)
 
  ((1 191 1) (1 191 0)  (    65   -102     12) 1.5)
 
  ((1 191 2) (1 191 1)  (  63.5 -101.5     12) 1.5)
 
  ((1 191 3) (1 191 2)  (  63.5   -107     12) 1.5)
 
  ((1 191 4) (1 191 3)  (  58.5   -121   12.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 192, parent index 190
 
  ((1 192 0) (BRANCH-PT 1 191 3)  (  63.5   -107     12) 1.5)
 
  ((1 192 1) (1 192 0)  (    70   -116     13) 1.5)
 
  ((1 192 2) (1 192 1)  (  63.5   -130     13) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 193, parent index 190
 
  ((1 193 0) (BRANCH-PT 1 191 2)  (  63.5 -101.5     12) 1.5)
 
  ((1 193 1) (1 193 0)  (    57  -99.5   13.5) 1.5)
 
  ((1 193 2) (1 193 1)  (  50.5 -112.5   13.5) 1.5)
 
  ((1 193 3) (1 193 2)  (  43.5 -115.5   13.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 194, parent index 192
 
  ((1 194 0) (BRANCH-PT 1 193 1)  (    57  -99.5   13.5) 1.5)
 
  ((1 194 1) (1 194 0)  (  49.5    -98     13) 1.5)
 
  ((1 194 2) (1 194 1)  (  46.5   -103     13) 1.5)
 
  ((1 194 3) (1 194 2)  (  44.5   -105     13) 1.5)
 
  ((1 194 4) (1 194 3)  (    39 -102.5     13) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 195, parent index 193
 
  ((1 195 0) (BRANCH-PT 1 194 2)  (  46.5   -103     13) 1.5)
 
  ((1 195 1) (1 195 0)  (    46 -111.5     13) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 196, parent index 193
 
  ((1 196 0) (BRANCH-PT 1 194 1)  (  49.5    -98     13) 1.5)
 
  ((1 196 1) (1 196 0)  (    45  -96.5     13) 1.5)
 
  ((1 196 2) (1 196 1)  (  39.5    -94     13) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 197, parent index 195
 
  ((1 197 0) (BRANCH-PT 1 196 1)  (    45  -96.5     13) 1.5)
 
  ((1 197 1) (1 197 0)  (    40  -99.5     13) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 198, parent index 189
 
  ((1 198 0) (BRANCH-PT 1 190 2)  (    78    -99   11.5) 5.8)
 
  ((1 198 1) (1 198 0)  (  83.5   -107   11.5)   3)
 
  ((1 198 2) (1 198 1)  (  87.5   -111   11.5)   3)
 
  ((1 198 3) (1 198 2)  (    90 -111.5   11.5) 2.7)
 
  ((1 198 4) (1 198 3)  (    96   -109   11.5) 1.5)
 
  ((1 198 5) (1 198 4)  ( 101.5   -107     12) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 199, parent index 197
 
  ((1 199 0) (BRANCH-PT 1 198 4)  (    96   -109   11.5) 1.5)
 
  ((1 199 1) (1 199 0)  (    99 -112.5   11.5) 1.5)
 
  ((1 199 2) (1 199 1)  ( 102.5 -112.5   11.5) 1.5)
 
  ((1 199 3) (1 199 2)  ( 106.5 -121.5     12) 1.5)
 
  ((1 199 4) (1 199 3)  (   104   -127     12) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 200, parent index 197
 
  ((1 200 0) (BRANCH-PT 1 198 3)  (    90 -111.5   11.5) 2.7)
 
  ((1 200 1) (1 200 0)  (  93.5   -117   12.5) 3.5)
 
  ((1 200 2) (1 200 1)  (    94   -123   12.5) 3.5)
 
  ((1 200 3) (1 200 2)  (    92   -126   12.5) 3.5)
 
  ((1 200 4) (1 200 3)  ( 102.5   -132   12.5) 2.1)
 
  ((1 200 5) (1 200 4)  (   107 -130.5     12) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 201, parent index 199
 
  ((1 201 0) (BRANCH-PT 1 200 4)  ( 102.5   -132   12.5) 2.1)
 
  ((1 201 1) (1 201 0)  (   106   -135     13) 1.2)
 
  ((1 201 2) (1 201 1)  (   107   -138     13) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 12
;; Section 202, parent index 199
 
  ((1 202 0) (BRANCH-PT 1 200 3)  (    92   -126   12.5) 3.5)
 
  ((1 202 1) (1 202 0)  (  86.5   -130     12) 2.9)
 
  ((1 202 2) (1 202 1)  (  80.5   -136     12) 2.9)
 
  ((1 202 3) (1 202 2)  (    79   -142     12) 2.9)
 
  ((1 202 4) (1 202 3)  (  78.5   -148   11.5)   4)
 
  ((1 202 5) (1 202 4)  (  75.5   -152   11.5) 1.7)
 
  ((1 202 6) (1 202 5)  (    73 -155.5   11.5) 1.4)
 
  ((1 202 7) (1 202 6)  (    69   -154   10.5) 1.4)
 
  ((1 202 8) (1 202 7)  (  60.5   -150   10.5) 1.3)
 
  ((1 202 9) (1 202 8)  (    55 -143.5   10.5) 1.3)
 
  ((1 202 10) (1 202 9)  (    50   -147   10.5) 1.3)
 
  ((1 202 11) (1 202 10)  (    50 -149.5     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 203, parent index 201
 
  ((1 203 0) (BRANCH-PT 1 202 9)  (    55 -143.5   10.5) 1.3)
 
  ((1 203 1) (1 203 0)  (    50   -141   11.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 204, parent index 201
 
  ((1 204 0) (BRANCH-PT 1 202 7)  (    69   -154   10.5) 1.4)
 
  ((1 204 1) (1 204 0)  (    63   -160   10.5) 1.3)
 
  ((1 204 2) (1 204 1)  (    61   -161     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 205, parent index 201
 
  ((1 205 0) (BRANCH-PT 1 202 5)  (  75.5   -152   11.5) 1.7)
 
  ((1 205 1) (1 205 0)  (    78   -158     12) 1.6)
 
  ((1 205 2) (1 205 1)  (  69.5   -163     12) 1.6)
 
  ((1 205 3) (1 205 2)  (  67.5 -161.5   12.5) 1.3)
 
  ((1 205 4) (1 205 3)  (  67.5 -158.5     10) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 206, parent index 204
 
  ((1 206 0) (BRANCH-PT 1 205 3)  (  67.5 -161.5   12.5) 1.3)
 
  ((1 206 1) (1 206 0)  (  63.5   -162   10.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 207, parent index 204
 
  ((1 207 0) (BRANCH-PT 1 205 2)  (  69.5   -163     12) 1.6)
 
  ((1 207 1) (1 207 0)  (  69.5 -168.5     13) 1.3)
 
  ((1 207 2) (1 207 1)  (    65   -171     11) 1.3)
 
  ((1 207 3) (1 207 2)  (  62.5 -173.5     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 208, parent index 204
 
  ((1 208 0) (BRANCH-PT 1 205 1)  (    78   -158     12) 1.6)
 
  ((1 208 1) (1 208 0)  (  79.5 -164.5     12) 1.3)
 
  ((1 208 2) (1 208 1)  (    71 -170.5     12) 1.3)
 
  ((1 208 3) (1 208 2)  (    67   -177     10) 1.3)
 
  ((1 208 4) (1 208 3)  (  63.5 -178.5     10)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 209, parent index 207
 
  ((1 209 0) (BRANCH-PT 1 208 3)  (    67   -177     10) 1.3)
 
  ((1 209 1) (1 209 0)  (  69.5   -178     10)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 210, parent index 201
 
  ((1 210 0) (BRANCH-PT 1 202 4)  (  78.5   -148   11.5)   4)
 
  ((1 210 1) (1 210 0)  (    81   -149   11.5) 4.8)
 
  ((1 210 2) (1 210 1)  (    87   -149   11.5) 2.6)
 
  ((1 210 3) (1 210 2)  (  88.5 -146.5   11.5) 1.6)
 
  ((1 210 4) (1 210 3)  (  95.5 -149.5     12) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 211, parent index 209
 
  ((1 211 0) (BRANCH-PT 1 210 3)  (  88.5 -146.5   11.5) 1.6)
 
  ((1 211 1) (1 211 0)  (  85.5   -142   11.5) 1.4)
 
  ((1 211 2) (1 211 1)  (  82.5   -142   11.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 212, parent index 210
 
  ((1 212 0) (BRANCH-PT 1 211 1)  (  85.5   -142   11.5) 1.4)
 
  ((1 212 1) (1 212 0)  (  86.5   -139     12) 1.2)
 
  ((1 212 2) (1 212 1)  (    87   -133   12.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 213, parent index 211
 
  ((1 213 0) (BRANCH-PT 1 212 1)  (  86.5   -139     12) 1.2)
 
  ((1 213 1) (1 213 0)  (    93 -133.5   12.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 214, parent index 209
 
  ((1 214 0) (BRANCH-PT 1 210 2)  (    87   -149   11.5) 2.6)
 
  ((1 214 1) (1 214 0)  (  91.5 -153.5     13) 3.3)
 
  ((1 214 2) (1 214 1)  (   104   -150     13) 1.4)
 
  ((1 214 3) (1 214 2)  (   109   -154   12.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 215, parent index 213
 
  ((1 215 0) (BRANCH-PT 1 214 2)  (   104   -150     13) 1.4)
 
  ((1 215 1) (1 215 0)  ( 106.5   -146   13.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 216, parent index 213
 
  ((1 216 0) (BRANCH-PT 1 214 1)  (  91.5 -153.5     13) 3.3)
 
  ((1 216 1) (1 216 0)  (  93.5   -156   12.5) 4.4)
 
  ((1 216 2) (1 216 1)  (   103   -158     12) 2.6)
 
  ((1 216 3) (1 216 2)  (   104 -160.5     12) 1.7)
 
  ((1 216 4) (1 216 3)  (   100 -168.5   11.5) 1.3)
 
  ((1 216 5) (1 216 4)  (   101 -173.5   11.5) 1.3)
 
  ((1 216 6) (1 216 5)  (   103 -177.5   11.5) 1.3)
 
  ((1 216 7) (1 216 6)  (   104 -178.5   10.5) 1.3)
 
  ((1 216 8) (1 216 7)  ( 107.5   -174   10.5) 0.8)
 
  ((1 216 9) (1 216 8)  (   106   -172   10.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 217, parent index 215
 
  ((1 217 0) (BRANCH-PT 1 216 8)  ( 107.5   -174   10.5) 0.8)
 
  ((1 217 1) (1 217 0)  (   108   -173   10.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 218, parent index 215
 
  ((1 218 0) (BRANCH-PT 1 216 7)  (   104 -178.5   10.5) 1.3)
 
  ((1 218 1) (1 218 0)  (   105 -185.5   10.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 219, parent index 215
 
  ((1 219 0) (BRANCH-PT 1 216 3)  (   104 -160.5     12) 1.7)
 
  ((1 219 1) (1 219 0)  (   110   -169     11) 0.8)
 
  ((1 219 2) (1 219 1)  ( 110.5   -167     11) 0.8)
 
  ((1 219 3) (1 219 2)  ( 106.5   -161   11.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 220, parent index 218
 
  ((1 220 0) (BRANCH-PT 1 219 1)  (   110   -169     11) 0.8)
 
  ((1 220 1) (1 220 0)  ( 111.5 -171.5     11) 0.8)
 
  ((1 220 2) (1 220 1)  (   113 -169.5   10.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 221, parent index 219
 
  ((1 221 0) (BRANCH-PT 1 220 1)  ( 111.5 -171.5     11) 0.8)
 
  ((1 221 1) (1 221 0)  ( 111.5 -177.5     12) 0.8)
 
  ((1 221 2) (1 221 1)  ( 110.5 -186.5     12) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 222, parent index 215
 
  ((1 222 0) (BRANCH-PT 1 216 2)  (   103   -158     12) 2.6)
 
  ((1 222 1) (1 222 0)  (   112   -158     12) 0.8)
 
  ((1 222 2) (1 222 1)  (   114 -155.5     12) 1.3)
 
  ((1 222 3) (1 222 2)  (   112   -152     12) 1.3)
 
  ((1 222 4) (1 222 3)  (   110   -149   11.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 223, parent index 221
 
  ((1 223 0) (BRANCH-PT 1 222 3)  (   112   -152     12) 1.3)
 
  ((1 223 1) (1 223 0)  (   114 -149.5     12) 1.3)
 
  ((1 223 2) (1 223 1)  (   114   -148     12) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 224, parent index 222
 
  ((1 224 0) (BRANCH-PT 1 223 1)  (   114 -149.5     12) 1.3)
 
  ((1 224 1) (1 224 0)  ( 116.5 -148.5   12.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 225, parent index 221
 
  ((1 225 0) (BRANCH-PT 1 222 2)  (   114 -155.5     12) 1.3)
 
  ((1 225 1) (1 225 0)  ( 116.5 -152.5   10.5) 1.1)
 
  ((1 225 2) (1 225 1)  ( 116.5   -150     11)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 226, parent index 224
 
  ((1 226 0) (BRANCH-PT 1 225 1)  ( 116.5 -152.5   10.5) 1.1)
 
  ((1 226 1) (1 226 0)  (   119 -153.5   11.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 227, parent index 221
 
  ((1 227 0) (BRANCH-PT 1 222 1)  (   112   -158     12) 0.8)
 
  ((1 227 1) (1 227 0)  ( 117.5   -160     11) 1.3)
 
  ((1 227 2) (1 227 1)  (   119   -160     11) 1.3)
 
  ((1 227 3) (1 227 2)  (   121   -156     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 228, parent index 226
 
  ((1 228 0) (BRANCH-PT 1 227 2)  (   119   -160     11) 1.3)
 
  ((1 228 1) (1 228 0)  ( 122.5   -165     11) 1.3)
 
  ((1 228 2) (1 228 1)  (   125   -167     11) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 229, parent index 226
 
  ((1 229 0) (BRANCH-PT 1 227 1)  ( 117.5   -160     11) 1.3)
 
  ((1 229 1) (1 229 0)  ( 115.5   -164   10.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 230, parent index 215
 
  ((1 230 0) (BRANCH-PT 1 216 1)  (  93.5   -156   12.5) 4.4)
 
  ((1 230 1) (1 230 0)  (  97.5   -164     12) 1.5)
 
  ((1 230 2) (1 230 1)  (  99.5   -167     12) 1.5)
 
  ((1 230 3) (1 230 2)  (   106   -170     12) 1.5)
 
  ((1 230 4) (1 230 3)  ( 108.5 -176.5   12.5) 1.5)
 
  ((1 230 5) (1 230 4)  (   108   -182   12.5) 1.5)
 
  ((1 230 6) (1 230 5)  (   106   -187   11.5) 1.5)
 
  ((1 230 7) (1 230 6)  (   110   -192    9.5) 1.5)
 
  ((1 230 8) (1 230 7)  (   113 -194.5   10.5) 1.5)
 
  ((1 230 9) (1 230 8)  (   115 -194.5   10.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 231, parent index 229
 
  ((1 231 0) (BRANCH-PT 1 230 8)  (   113 -194.5   10.5) 1.5)
 
  ((1 231 1) (1 231 0)  ( 112.5   -204   12.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 232, parent index 229
 
  ((1 232 0) (BRANCH-PT 1 230 7)  (   110   -192    9.5) 1.5)
 
  ((1 232 1) (1 232 0)  ( 106.5   -202      9) 1.5)
 
  ((1 232 2) (1 232 1)  (   112 -215.5    9.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 233, parent index 231
 
  ((1 233 0) (BRANCH-PT 1 232 1)  ( 106.5   -202      9) 1.5)
 
  ((1 233 1) (1 233 0)  ( 101.5   -202    6.5) 1.2)
 
  ((1 233 2) (1 233 1)  ( 102.5 -206.5      6) 1.6)
 
  ((1 233 3) (1 233 2)  ( 106.5   -209      7) 1.6)
 
  ((1 233 4) (1 233 3)  ( 106.5   -216      7) 1.4)
 
  ((1 233 5) (1 233 4)  (   110 -216.5    6.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 234, parent index 232
 
  ((1 234 0) (BRANCH-PT 1 233 4)  ( 106.5   -216      7) 1.4)
 
  ((1 234 1) (1 234 0)  ( 107.5 -217.5    7.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 235, parent index 232
 
  ((1 235 0) (BRANCH-PT 1 233 2)  ( 102.5 -206.5      6) 1.6)
 
  ((1 235 1) (1 235 0)  ( 103.5   -217      6) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 236, parent index 232
 
  ((1 236 0) (BRANCH-PT 1 233 1)  ( 101.5   -202    6.5) 1.2)
 
  ((1 236 1) (1 236 0)  (    96   -200    6.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 9
;; Section 237, parent index 209
 
  ((1 237 0) (BRANCH-PT 1 210 1)  (    81   -149   11.5) 4.8)
 
  ((1 237 1) (1 237 0)  (    81 -155.5   12.5) 1.8)
 
  ((1 237 2) (1 237 1)  (  88.5 -167.5   11.5) 3.3)
 
  ((1 237 3) (1 237 2)  (    92 -171.5     12) 2.1)
 
  ((1 237 4) (1 237 3)  (    91 -174.5     12) 2.6)
 
  ((1 237 5) (1 237 4)  (  87.5 -178.5   11.5) 1.9)
 
  ((1 237 6) (1 237 5)  (  84.5   -182     11) 1.6)
 
  ((1 237 7) (1 237 6)  (  70.5   -189     11) 1.6)
 
  ((1 237 8) (1 237 7)  (  67.5   -190     11) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 238, parent index 236
 
  ((1 238 0) (BRANCH-PT 1 237 6)  (  84.5   -182     11) 1.6)
 
  ((1 238 1) (1 238 0)  (  86.5   -187     11) 1.6)
 
  ((1 238 2) (1 238 1)  (    89 -197.5   12.5) 1.6)
 
  ((1 238 3) (1 238 2)  (    92 -199.5   11.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 239, parent index 236
 
  ((1 239 0) (BRANCH-PT 1 237 4)  (    91 -174.5     12) 2.6)
 
  ((1 239 1) (1 239 0)  (    91 -185.5     11) 1.6)
 
  ((1 239 2) (1 239 1)  (  84.5   -192     11) 1.6)
 
  ((1 239 3) (1 239 2)  (  84.5 -199.5   11.5) 1.6)
 
  ((1 239 4) (1 239 3)  (    92 -207.5    9.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 240, parent index 238
 
  ((1 240 0) (BRANCH-PT 1 239 1)  (    91 -185.5     11) 1.6)
 
  ((1 240 1) (1 240 0)  (    93   -195     13) 1.6)
 
  ((1 240 2) (1 240 1)  (    94 -201.5   15.5) 1.6)
 
  ((1 240 3) (1 240 2)  (  93.5 -208.5    8.5) 1.6)
 
  ((1 240 4) (1 240 3)  (    99   -211    8.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 241, parent index 239
 
  ((1 241 0) (BRANCH-PT 1 240 3)  (  93.5 -208.5    8.5) 1.6)
 
  ((1 241 1) (1 241 0)  (    90   -210    8.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 242, parent index 239
 
  ((1 242 0) (BRANCH-PT 1 240 2)  (    94 -201.5   15.5) 1.6)
 
  ((1 242 1) (1 242 0)  (  96.5   -204      7) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 243, parent index 236
 
  ((1 243 0) (BRANCH-PT 1 237 2)  (  88.5 -167.5   11.5) 3.3)
 
  ((1 243 1) (1 243 0)  (  82.5 -172.5   11.5) 1.6)
 
  ((1 243 2) (1 243 1)  (  74.5 -176.5   11.5) 1.9)
 
  ((1 243 3) (1 243 2)  (    76   -193   10.5) 1.9)
 
  ((1 243 4) (1 243 3)  (  81.5   -205     14) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 244, parent index 242
 
  ((1 244 0) (BRANCH-PT 1 243 3)  (    76   -193   10.5) 1.9)
 
  ((1 244 1) (1 244 0)  (    71 -201.5     11) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 245, parent index 242
 
  ((1 245 0) (BRANCH-PT 1 243 2)  (  74.5 -176.5   11.5) 1.9)
 
  ((1 245 1) (1 245 0)  (    68   -184   11.5) 1.9)
 
  ((1 245 2) (1 245 1)  (  64.5   -201   12.5) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 246, parent index 0
 
  ((1 246 0) (BRANCH-PT 1 1 4)  (  11.5    -12    1.5)   6)
 
  ((1 246 1) (1 246 0)  (    13  -15.5      1) 5.4)
 
  ((1 246 2) (1 246 1)  (    12  -22.5    0.5) 4.1)
 
  ((1 246 3) (1 246 2)  (  13.5    -33     -1) 3.9)
 
  ((1 246 4) (1 246 3)  (     7    -40     -1) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 247, parent index 245
 
  ((1 247 0) (BRANCH-PT 1 246 3)  (  13.5    -33     -1) 3.9)
 
  ((1 247 1) (1 247 0)  (  13.5  -39.5      1) 4.3)
 
  ((1 247 2) (1 247 1)  (    19    -39      1) 4.2)
 
  ((1 247 3) (1 247 2)  (  23.5    -43      1) 4.2)
 
  ((1 247 4) (1 247 3)  (    26  -41.5      1) 1.7)
 
  ((1 247 5) (1 247 4)  (  28.5    -40    1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 248, parent index 246
 
  ((1 248 0) (BRANCH-PT 1 247 4)  (    26  -41.5      1) 1.7)
 
  ((1 248 1) (1 248 0)  (    24    -35      1) 1.7)
 
  ((1 248 2) (1 248 1)  (    24    -25    1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 249, parent index 247
 
  ((1 249 0) (BRANCH-PT 1 248 1)  (    24    -35      1) 1.7)
 
  ((1 249 1) (1 249 0)  (  21.5  -30.5      1) 1.7)
 
  ((1 249 2) (1 249 1)  (    20  -24.5      1) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 250, parent index 246
 
  ((1 250 0) (BRANCH-PT 1 247 3)  (  23.5    -43      1) 4.2)
 
  ((1 250 1) (1 250 0)  (  24.5  -47.5      1) 4.4)
 
  ((1 250 2) (1 250 1)  (  28.5  -50.5      0) 2.2)
 
  ((1 250 3) (1 250 2)  (  29.5    -54      1) 2.2)
 
  ((1 250 4) (1 250 3)  (    31  -60.5    1.5) 2.2)
 
  ((1 250 5) (1 250 4)  (    35    -63    1.5) 2.2)
 
  ((1 250 6) (1 250 5)  (    35  -65.5      1) 1.5)
 
  ((1 250 7) (1 250 6)  (  34.5    -70      1) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 251, parent index 249
 
  ((1 251 0) (BRANCH-PT 1 250 6)  (    35  -65.5      1) 1.5)
 
  ((1 251 1) (1 251 0)  (  40.5  -67.5      1) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 252, parent index 249
 
  ((1 252 0) (BRANCH-PT 1 250 5)  (    35    -63    1.5) 2.2)
 
  ((1 252 1) (1 252 0)  (    41    -61    2.5) 1.5)
 
  ((1 252 2) (1 252 1)  (  43.5    -67    2.5) 1.5)
 
  ((1 252 3) (1 252 2)  (    42    -71    2.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 253, parent index 251
 
  ((1 253 0) (BRANCH-PT 1 252 2)  (  43.5    -67    2.5) 1.5)
 
  ((1 253 1) (1 253 0)  (  48.5  -67.5    2.5) 1.5)
 
  ((1 253 2) (1 253 1)  (  52.5  -66.5      3) 1.3)
 
  ((1 253 3) (1 253 2)  (    57    -71      3) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 254, parent index 252
 
  ((1 254 0) (BRANCH-PT 1 253 2)  (  52.5  -66.5      3) 1.3)
 
  ((1 254 1) (1 254 0)  (  54.5    -62      3) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 255, parent index 251
 
  ((1 255 0) (BRANCH-PT 1 252 1)  (    41    -61    2.5) 1.5)
 
  ((1 255 1) (1 255 0)  (    43  -59.5    2.5) 1.5)
 
  ((1 255 2) (1 255 1)  (  46.5  -57.5    2.5) 1.6)
 
  ((1 255 3) (1 255 2)  (    53  -56.5    2.5) 1.8)
 
  ((1 255 4) (1 255 3)  (  57.5  -56.5    2.5) 4.1)
 
  ((1 255 5) (1 255 4)  (    57  -48.5    2.5) 2.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 256, parent index 254
 
  ((1 256 0) (BRANCH-PT 1 255 4)  (  57.5  -56.5    2.5) 4.1)
 
  ((1 256 1) (1 256 0)  (    64  -57.5    2.5) 3.2)
 
  ((1 256 2) (1 256 1)  (  72.5  -57.5    1.5)   2)
 
  ((1 256 3) (1 256 2)  (    81  -57.5      2)   2)
 
  ((1 256 4) (1 256 3)  (    82    -63      2)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 257, parent index 255
 
  ((1 257 0) (BRANCH-PT 1 256 3)  (    81  -57.5      2)   2)
 
  ((1 257 1) (1 257 0)  (  87.5  -57.5      2)   2)
 
  ((1 257 2) (1 257 1)  (    99    -56      2)   2)
 
  ((1 257 3) (1 257 2)  ( 113.5  -54.5      2)   2)
 
  ((1 257 4) (1 257 3)  (   123    -53    1.5) 1.7)
 
  ((1 257 5) (1 257 4)  ( 123.5    -43      2) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 258, parent index 256
 
  ((1 258 0) (BRANCH-PT 1 257 4)  (   123    -53    1.5) 1.7)
 
  ((1 258 1) (1 258 0)  ( 127.5  -55.5    1.5) 1.7)
 
  ((1 258 2) (1 258 1)  ( 131.5    -54    1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 259, parent index 255
 
  ((1 259 0) (BRANCH-PT 1 256 1)  (    64  -57.5    2.5) 3.2)
 
  ((1 259 1) (1 259 0)  (    69    -63    2.5)   2)
 
  ((1 259 2) (1 259 1)  (    70    -67    2.5) 1.6)
 
  ((1 259 3) (1 259 2)  (  70.5    -69    1.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 260, parent index 258
 
  ((1 260 0) (BRANCH-PT 1 259 1)  (    69    -63    2.5)   2)
 
  ((1 260 1) (1 260 0)  (  76.5  -64.5    2.5) 1.6)
 
  ((1 260 2) (1 260 1)  (    86    -64    3.5) 1.6)
 
  ((1 260 3) (1 260 2)  (    92    -68      5) 1.6)
 
  ((1 260 4) (1 260 3)  (    95    -70    5.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 261, parent index 249
 
  ((1 261 0) (BRANCH-PT 1 250 1)  (  24.5  -47.5      1) 4.4)
 
  ((1 261 1) (1 261 0)  (    21    -53   -1.5) 3.3)
 
  ((1 261 2) (1 261 1)  (  20.5    -68   -1.5)   5)
 
  ((1 261 3) (1 261 2)  (  15.5  -68.5   -1.5)   2)
 
  ((1 261 4) (1 261 3)  (  11.5    -71   -1.5)   2)
 
  ((1 261 5) (1 261 4)  (  11.5  -71.5   -1.5)   2)
 
  ((1 261 6) (1 261 5)  (     9  -75.5   -1.5)   2)
 
  ((1 261 7) (1 261 6)  (  -0.5  -72.5   -1.5)   2)
 
  ((1 261 8) (1 261 7)  (  -6.5    -71   -1.5)   2)
 
  ((1 261 9) (1 261 8)  ( -12.5  -69.5   -1.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 262, parent index 260
 
  ((1 262 0) (BRANCH-PT 1 261 7)  (  -0.5  -72.5   -1.5)   2)
 
  ((1 262 1) (1 262 0)  (  -2.5    -77   -1.5)   2)
 
  ((1 262 2) (1 262 1)  (  -6.5    -78   -1.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 263, parent index 261
 
  ((1 263 0) (BRANCH-PT 1 262 1)  (  -2.5    -77   -1.5)   2)
 
  ((1 263 1) (1 263 0)  (  -0.5  -82.5     -1)   2)
 
  ((1 263 2) (1 263 1)  (   3.5    -84      0)   2)
 
  ((1 263 3) (1 263 2)  (   6.5    -98    0.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 264, parent index 260
 
  ((1 264 0) (BRANCH-PT 1 261 5)  (  11.5  -71.5   -1.5)   2)
 
  ((1 264 1) (1 264 0)  (     7    -72   -1.5)   2)
 
  ((1 264 2) (1 264 1)  (     4  -66.5   -1.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 265, parent index 260
 
  ((1 265 0) (BRANCH-PT 1 261 2)  (  20.5    -68   -1.5)   5)
 
  ((1 265 1) (1 265 0)  (  23.5    -73     -1) 3.1)
 
  ((1 265 2) (1 265 1)  (    17  -75.5   -1.5) 1.9)
 
  ((1 265 3) (1 265 2)  (    13    -79   -1.5) 1.9)
 
  ((1 265 4) (1 265 3)  (     0  -82.5   -1.5) 1.9)
 
  ((1 265 5) (1 265 4)  (    -4  -82.5   -1.5) 1.9)
 
  ((1 265 6) (1 265 5)  (  -4.5  -80.5   -1.5) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 266, parent index 264
 
  ((1 266 0) (BRANCH-PT 1 265 3)  (    13    -79   -1.5) 1.9)
 
  ((1 266 1) (1 266 0)  (  16.5  -81.5     -1) 1.8)
 
  ((1 266 2) (1 266 1)  (    25  -82.5   -0.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 267, parent index 265
 
  ((1 267 0) (BRANCH-PT 1 266 1)  (  16.5  -81.5     -1) 1.8)
 
  ((1 267 1) (1 267 0)  (  16.5    -89   -0.5) 1.5)
 
  ((1 267 2) (1 267 1)  (  14.5  -95.5   -0.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 268, parent index 264
 
  ((1 268 0) (BRANCH-PT 1 265 1)  (  23.5    -73     -1) 3.1)
 
  ((1 268 1) (1 268 0)  (    26    -78   -0.5) 2.3)
 
  ((1 268 2) (1 268 1)  (  32.5  -83.5      0) 2.3)
 
  ((1 268 3) (1 268 2)  (    37    -80      0) 1.2)
 
  ((1 268 4) (1 268 3)  (  40.5  -75.5    0.5) 1.2)
 
  ((1 268 5) (1 268 4)  (  44.5    -72    0.5) 1.2)
 
  ((1 268 6) (1 268 5)  (  43.5    -69    0.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 269, parent index 267
 
  ((1 269 0) (BRANCH-PT 1 268 5)  (  44.5    -72    0.5) 1.2)
 
  ((1 269 1) (1 269 0)  (    49    -73      1) 1.2)
 
  ((1 269 2) (1 269 1)  (    54  -76.5    1.5) 1.2)
 
  ((1 269 3) (1 269 2)  (  51.5    -80    1.5) 1.2)
 
  ((1 269 4) (1 269 3)  (    49  -77.5    1.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 270, parent index 268
 
  ((1 270 0) (BRANCH-PT 1 269 3)  (  51.5    -80    1.5) 1.2)
 
  ((1 270 1) (1 270 0)  (  60.5  -78.5      2) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 271, parent index 267
 
  ((1 271 0) (BRANCH-PT 1 268 2)  (  32.5  -83.5      0) 2.3)
 
  ((1 271 1) (1 271 0)  (  35.5    -88   -0.5)   2)
 
  ((1 271 2) (1 271 1)  (    37    -95   -0.5) 2.7)
 
  ((1 271 3) (1 271 2)  (  41.5    -97   -0.5) 1.9)
 
  ((1 271 4) (1 271 3)  (    51  -95.5      0) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 272, parent index 270
 
  ((1 272 0) (BRANCH-PT 1 271 2)  (    37    -95   -0.5) 2.7)
 
  ((1 272 1) (1 272 0)  (  35.5   -103   -0.5)   3)
 
  ((1 272 2) (1 272 1)  (    34   -114    0.5) 5.8)
 
  ((1 272 3) (1 272 2)  (  44.5 -117.5   -1.5) 3.3)
 
  ((1 272 4) (1 272 3)  (    50   -115   -0.5) 2.2)
 
  ((1 272 5) (1 272 4)  (    52 -112.5   -1.5) 2.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 273, parent index 271
 
  ((1 273 0) (BRANCH-PT 1 272 4)  (    50   -115   -0.5) 2.2)
 
  ((1 273 1) (1 273 0)  (  54.5 -115.5      1) 2.1)
 
  ((1 273 2) (1 273 1)  (    53   -119      1) 1.8)
 
  ((1 273 3) (1 273 2)  (  48.5 -122.5    0.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 274, parent index 272
 
  ((1 274 0) (BRANCH-PT 1 273 2)  (    53   -119      1) 1.8)
 
  ((1 274 1) (1 274 0)  (    57   -123      1) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 275, parent index 272
 
  ((1 275 0) (BRANCH-PT 1 273 1)  (  54.5 -115.5      1) 2.1)
 
  ((1 275 1) (1 275 0)  (    59 -115.5      1) 1.6)
 
  ((1 275 2) (1 275 1)  (    67 -116.5    1.5) 1.5)
 
  ((1 275 3) (1 275 2)  (  74.5 -116.5    1.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 276, parent index 274
 
  ((1 276 0) (BRANCH-PT 1 275 2)  (    67 -116.5    1.5) 1.5)
 
  ((1 276 1) (1 276 0)  (  82.5   -110      3) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 277, parent index 271
 
  ((1 277 0) (BRANCH-PT 1 272 3)  (  44.5 -117.5   -1.5) 3.3)
 
  ((1 277 1) (1 277 0)  (  45.5   -123     -1) 1.5)
 
  ((1 277 2) (1 277 1)  (    54 -128.5      1) 1.5)
 
  ((1 277 3) (1 277 2)  (    48 -132.5      1) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 278, parent index 276
 
  ((1 278 0) (BRANCH-PT 1 277 2)  (    54 -128.5      1) 1.5)
 
  ((1 278 1) (1 278 0)  (    61 -135.5      3) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 11
;; Section 279, parent index 271
 
  ((1 279 0) (BRANCH-PT 1 272 2)  (    34   -114    0.5) 5.8)
 
  ((1 279 1) (1 279 0)  (    30   -119     -1) 4.2)
 
  ((1 279 2) (1 279 1)  (  26.5   -123     -1) 4.2)
 
  ((1 279 3) (1 279 2)  (    22 -126.5   -1.5) 4.2)
 
  ((1 279 4) (1 279 3)  (    19 -132.5   -1.5) 4.2)
 
  ((1 279 5) (1 279 4)  (    22   -136   -1.5) 3.3)
 
  ((1 279 6) (1 279 5)  (    29 -137.5   -0.5) 2.9)
 
  ((1 279 7) (1 279 6)  (  32.5 -135.5   -0.5) 1.9)
 
  ((1 279 8) (1 279 7)  (    38 -137.5      0) 1.9)
 
  ((1 279 9) (1 279 8)  (    42   -139      0) 1.8)
 
  ((1 279 10) (1 279 9)  (    45   -140      0) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 280, parent index 278
 
  ((1 280 0) (BRANCH-PT 1 279 9)  (    42   -139      0) 1.8)
 
  ((1 280 1) (1 280 0)  (    44 -135.5    0.5) 1.8)
 
  ((1 280 2) (1 280 1)  (    44 -130.5    0.5) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 281, parent index 278
 
  ((1 281 0) (BRANCH-PT 1 279 6)  (    29 -137.5   -0.5) 2.9)
 
  ((1 281 1) (1 281 0)  (  32.5 -139.5    0.5) 2.7)
 
  ((1 281 2) (1 281 1)  (    40   -143      1)   2)
 
  ((1 281 3) (1 281 2)  (    45   -149      3)   2)
 
  ((1 281 4) (1 281 3)  (  44.5   -149     -1)   2)
 
  ((1 281 5) (1 281 4)  (  57.5 -150.5      0)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 282, parent index 280
 
  ((1 282 0) (BRANCH-PT 1 281 4)  (  44.5   -149     -1)   2)
 
  ((1 282 1) (1 282 0)  (    48   -154   -1.5) 1.4)
 
  ((1 282 2) (1 282 1)  (  53.5   -152     -1) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 283, parent index 281
 
  ((1 283 0) (BRANCH-PT 1 282 1)  (    48   -154   -1.5) 1.4)
 
  ((1 283 1) (1 283 0)  (  53.5 -163.5     -1) 1.4)
 
  ((1 283 2) (1 283 1)  (    56   -165     -1) 1.4)
 
  ((1 283 3) (1 283 2)  (    59   -164     -1) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 284, parent index 282
 
  ((1 284 0) (BRANCH-PT 1 283 2)  (    56   -165     -1) 1.4)
 
  ((1 284 1) (1 284 0)  (    58   -169   -0.5) 1.2)
 
  ((1 284 2) (1 284 1)  (    60 -171.5   -0.5) 1.2)
 
  ((1 284 3) (1 284 2)  (  63.5 -177.5      0) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 285, parent index 283
 
  ((1 285 0) (BRANCH-PT 1 284 2)  (    60 -171.5   -0.5) 1.2)
 
  ((1 285 1) (1 285 0)  (    59 -174.5   -0.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 286, parent index 283
 
  ((1 286 0) (BRANCH-PT 1 284 1)  (    58   -169   -0.5) 1.2)
 
  ((1 286 1) (1 286 0)  (  55.5 -171.5   -0.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 287, parent index 280
 
  ((1 287 0) (BRANCH-PT 1 281 2)  (    40   -143      1)   2)
 
  ((1 287 1) (1 287 0)  (  41.5 -151.5    2.5) 1.2)
 
  ((1 287 2) (1 287 1)  (    48   -157      4) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 11
;; Section 288, parent index 280
 
  ((1 288 0) (BRANCH-PT 1 281 1)  (  32.5 -139.5    0.5) 2.7)
 
  ((1 288 1) (1 288 0)  (    36   -148   -1.5)   2)
 
  ((1 288 2) (1 288 1)  (  36.5 -152.5     -4) 4.3)
 
  ((1 288 3) (1 288 2)  (  39.5   -156   -3.5) 3.7)
 
  ((1 288 4) (1 288 3)  (  43.5 -157.5   -3.5) 2.4)
 
  ((1 288 5) (1 288 4)  (  48.5 -162.5     -3) 2.4)
 
  ((1 288 6) (1 288 5)  (  51.5 -169.5     -2) 2.4)
 
  ((1 288 7) (1 288 6)  (  54.5   -172     -2) 2.4)
 
  ((1 288 8) (1 288 7)  (  57.5   -176     -3) 2.4)
 
  ((1 288 9) (1 288 8)  (  62.5   -180     -3) 2.4)
 
  ((1 288 10) (1 288 9)  (  62.5 -192.5     -3) 2.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 289, parent index 287
 
  ((1 289 0) (BRANCH-PT 1 288 9)  (  62.5   -180     -3) 2.4)
 
  ((1 289 1) (1 289 0)  (  66.5 -182.5     -3) 2.4)
 
  ((1 289 2) (1 289 1)  (  68.5 -190.5   -2.5) 2.4)
 
  ((1 289 3) (1 289 2)  (  66.5   -198   -2.5)   2)
 
  ((1 289 4) (1 289 3)  (    68   -206     -5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 290, parent index 288
 
  ((1 290 0) (BRANCH-PT 1 289 3)  (  66.5   -198   -2.5)   2)
 
  ((1 290 1) (1 290 0)  (    63 -202.5   -2.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 291, parent index 287
 
  ((1 291 0) (BRANCH-PT 1 288 6)  (  51.5 -169.5     -2) 2.4)
 
  ((1 291 1) (1 291 0)  (  50.5 -172.5   -3.5) 1.9)
 
  ((1 291 2) (1 291 1)  (  53.5   -182   -2.5) 1.9)
 
  ((1 291 3) (1 291 2)  (  53.5   -187   -2.5) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 292, parent index 287
 
  ((1 292 0) (BRANCH-PT 1 288 3)  (  39.5   -156   -3.5) 3.7)
 
  ((1 292 1) (1 292 0)  (  39.5   -161     -3) 1.7)
 
  ((1 292 2) (1 292 1)  (    46 -171.5     -3) 1.7)
 
  ((1 292 3) (1 292 2)  (  51.5   -177     -2) 1.6)
 
  ((1 292 4) (1 292 3)  (    55 -187.5   -2.5) 1.6)
 
  ((1 292 5) (1 292 4)  (    57   -191   -2.5) 1.6)
 
  ((1 292 6) (1 292 5)  (    60   -198   -2.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 293, parent index 291
 
  ((1 293 0) (BRANCH-PT 1 292 5)  (    57   -191   -2.5) 1.6)
 
  ((1 293 1) (1 293 0)  (  53.5   -198     -3) 1.6)
 
  ((1 293 2) (1 293 1)  (  54.5   -203     -3) 1.6)
 
  ((1 293 3) (1 293 2)  (    55   -204     -3) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 294, parent index 291
 
  ((1 294 0) (BRANCH-PT 1 292 2)  (    46 -171.5     -3) 1.7)
 
  ((1 294 1) (1 294 0)  (    47   -181   -2.5) 1.6)
 
  ((1 294 2) (1 294 1)  (    49   -191   -2.5) 1.5)
 
  ((1 294 3) (1 294 2)  (  53.5   -193     -2) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 295, parent index 293
 
  ((1 295 0) (BRANCH-PT 1 294 2)  (    49   -191   -2.5) 1.5)
 
  ((1 295 1) (1 295 0)  (    51 -212.5   -2.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 296, parent index 287
 
  ((1 296 0) (BRANCH-PT 1 288 2)  (  36.5 -152.5     -4) 4.3)
 
  ((1 296 1) (1 296 0)  (    33   -161   -4.5) 2.3)
 
  ((1 296 2) (1 296 1)  (  36.5 -169.5     -3) 2.3)
 
  ((1 296 3) (1 296 2)  (    41   -179     -4) 2.3)
 
  ((1 296 4) (1 296 3)  (  40.5   -186     -3) 2.3)
 
  ((1 296 5) (1 296 4)  (    45   -194     -3)   2)
 
  ((1 296 6) (1 296 5)  (    51 -200.5   -2.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 297, parent index 295
 
  ((1 297 0) (BRANCH-PT 1 296 5)  (    45   -194     -3)   2)
 
  ((1 297 1) (1 297 0)  (    49   -209   -0.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 298, parent index 295
 
  ((1 298 0) (BRANCH-PT 1 296 4)  (  40.5   -186     -3) 2.3)
 
  ((1 298 1) (1 298 0)  (  39.5   -192   -3.5)   2)
 
  ((1 298 2) (1 298 1)  (    42 -197.5   -3.5)   2)
 
  ((1 298 3) (1 298 2)  (    39   -204   -3.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 299, parent index 297
 
  ((1 299 0) (BRANCH-PT 1 298 1)  (  39.5   -192   -3.5)   2)
 
  ((1 299 1) (1 299 0)  (  36.5 -194.5   -3.5)   2)
 
  ((1 299 2) (1 299 1)  (  31.5   -200     -4)   2)
 
  ((1 299 3) (1 299 2)  (    26 -198.5   -3.5)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 300, parent index 295
 
  ((1 300 0) (BRANCH-PT 1 296 2)  (  36.5 -169.5     -3) 2.3)
 
  ((1 300 1) (1 300 0)  (  35.5   -180     -4)   2)
 
  ((1 300 2) (1 300 1)  (  35.5 -181.5     -4)   2)
 
  ((1 300 3) (1 300 2)  (  28.5 -180.5     -4) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 301, parent index 299
 
  ((1 301 0) (BRANCH-PT 1 300 2)  (  35.5 -181.5     -4)   2)
 
  ((1 301 1) (1 301 0)  (  30.5   -189   -4.5)   2)
 
  ((1 301 2) (1 301 1)  (    25   -189   -4.5) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 302, parent index 300
 
  ((1 302 0) (BRANCH-PT 1 301 1)  (  30.5   -189   -4.5)   2)
 
  ((1 302 1) (1 302 0)  (    31 -194.5   -3.5) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 303, parent index 278
 
  ((1 303 0) (BRANCH-PT 1 279 5)  (    22   -136   -1.5) 3.3)
 
  ((1 303 1) (1 303 0)  (    20 -144.5   -2.5) 1.8)
 
  ((1 303 2) (1 303 1)  (    25 -149.5   -2.5) 1.6)
 
  ((1 303 3) (1 303 2)  (  29.5   -152     -2) 1.6)
 
  ((1 303 4) (1 303 3)  (    32   -156     -2) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 304, parent index 302
 
  ((1 304 0) (BRANCH-PT 1 303 3)  (  29.5   -152     -2) 1.6)
 
  ((1 304 1) (1 304 0)  (  28.5 -157.5     -2) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 305, parent index 302
 
  ((1 305 0) (BRANCH-PT 1 303 2)  (    25 -149.5   -2.5) 1.6)
 
  ((1 305 1) (1 305 0)  (  25.5 -154.5     -3) 1.6)
 
  ((1 305 2) (1 305 1)  (    24 -156.5     -3) 1.6)
 
  ((1 305 3) (1 305 2)  (    24   -162     -3) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 306, parent index 304
 
  ((1 306 0) (BRANCH-PT 1 305 2)  (    24 -156.5     -3) 1.6)
 
  ((1 306 1) (1 306 0)  (  14.5   -163     -4) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 307, parent index 246
 
  ((1 307 0) (BRANCH-PT 1 247 1)  (  13.5  -39.5      1) 4.3)
 
  ((1 307 1) (1 307 0)  (    12  -41.5      1) 3.6)
 
  ((1 307 2) (1 307 1)  (   1.5    -43      1) 1.9)
 
  ((1 307 3) (1 307 2)  (    -6    -46      0) 1.9)
 
  ((1 307 4) (1 307 3)  (     0    -46    0.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 308, parent index 306
 
  ((1 308 0) (BRANCH-PT 1 307 3)  (    -6    -46      0) 1.9)
 
  ((1 308 1) (1 308 0)  ( -19.5  -47.5     -1) 1.4)
 
  ((1 308 2) (1 308 1)  ( -24.5    -46      0) 1.4)
 
  ((1 308 3) (1 308 2)  (   -24    -39    0.5) 1.4)
 
  ((1 308 4) (1 308 3)  (   -22  -26.5    1.5) 1.4)
 
  ((1 308 5) (1 308 4)  (   -30    -27    1.5) 1.4)
 
  ((1 308 6) (1 308 5)  ( -35.5    -23    2.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 309, parent index 307
 
  ((1 309 0) (BRANCH-PT 1 308 4)  (   -22  -26.5    1.5) 1.4)
 
  ((1 309 1) (1 309 0)  ( -27.5  -22.5      2) 1.4)
 
  ((1 309 2) (1 309 1)  ( -29.5    -21      2) 1.4)
 
  ((1 309 3) (1 309 2)  (   -24    -15      2) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 310, parent index 308
 
  ((1 310 0) (BRANCH-PT 1 309 1)  ( -27.5  -22.5      2) 1.4)
 
  ((1 310 1) (1 310 0)  ( -16.5    -18      2) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 311, parent index 307
 
  ((1 311 0) (BRANCH-PT 1 308 3)  (   -24    -39    0.5) 1.4)
 
  ((1 311 1) (1 311 0)  (   -10  -31.5      1) 1.4)
 
  ((1 311 2) (1 311 1)  ( -15.5  -27.5    1.5) 1.2)
 
  ((1 311 3) (1 311 2)  (   -11    -20    1.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 312, parent index 310
 
  ((1 312 0) (BRANCH-PT 1 311 1)  (   -10  -31.5      1) 1.4)
 
  ((1 312 1) (1 312 0)  (  -2.5    -28      1) 1.2)
 
  ((1 312 2) (1 312 1)  (  -5.5  -26.5      1) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 313, parent index 311
 
  ((1 313 0) (BRANCH-PT 1 312 1)  (  -2.5    -28      1) 1.2)
 
  ((1 313 1) (1 313 0)  (     5    -26      1) 1.2)
 
  ((1 313 2) (1 313 1)  (   4.5  -20.5    1.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 314, parent index 307
 
  ((1 314 0) (BRANCH-PT 1 308 2)  ( -24.5    -46      0) 1.4)
 
  ((1 314 1) (1 314 0)  ( -27.5    -46      0) 1.2)
 
  ((1 314 2) (1 314 1)  (   -33    -46      0) 1.2)
 
  ((1 314 3) (1 314 2)  (   -36    -39      0) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 315, parent index 313
 
  ((1 315 0) (BRANCH-PT 1 314 1)  ( -27.5    -46      0) 1.2)
 
  ((1 315 1) (1 315 0)  ( -28.5    -48      0) 1.5)
 
  ((1 315 2) (1 315 1)  ( -40.5  -50.5      0) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 316, parent index 314
 
  ((1 316 0) (BRANCH-PT 1 315 1)  ( -28.5    -48      0) 1.5)
 
  ((1 316 1) (1 316 0)  ( -28.5    -49      0) 1.5)
 
  ((1 316 2) (1 316 1)  (   -28  -52.5      0) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 317, parent index 315
 
  ((1 317 0) (BRANCH-PT 1 316 1)  ( -28.5    -49      0) 1.5)
 
  ((1 317 1) (1 317 0)  (   -42  -56.5      0) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 318, parent index 306
 
  ((1 318 0) (BRANCH-PT 1 307 1)  (    12  -41.5      1) 3.6)
 
  ((1 318 1) (1 318 0)  (     8  -46.5   -0.5) 2.9)
 
  ((1 318 2) (1 318 1)  (    10  -50.5   -0.5) 1.7)
 
  ((1 318 3) (1 318 2)  (    12  -51.5   -0.5) 1.7)
 
  ((1 318 4) (1 318 3)  (    16  -49.5      0) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 319, parent index 317
 
  ((1 319 0) (BRANCH-PT 1 318 1)  (     8  -46.5   -0.5) 2.9)
 
  ((1 319 1) (1 319 0)  (   2.5    -50   -0.5) 2.2)
 
  ((1 319 2) (1 319 1)  (    -9    -54    2.5) 3.7)
 
  ((1 319 3) (1 319 2)  (   -13    -50      3)   2)
 
  ((1 319 4) (1 319 3)  (   -17  -48.5      3)   2)
 
  ((1 319 5) (1 319 4)  ( -23.5    -48      3)   2)
 
  ((1 319 6) (1 319 5)  (   -24  -41.5    3.5)   2)
 
  ((1 319 7) (1 319 6)  ( -17.5  -36.5      4)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 320, parent index 318
 
  ((1 320 0) (BRANCH-PT 1 319 3)  (   -13    -50      3)   2)
 
  ((1 320 1) (1 320 0)  ( -20.5    -53      3)   2)
 
  ((1 320 2) (1 320 1)  ( -25.5  -55.5      2)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 321, parent index 318
 
  ((1 321 0) (BRANCH-PT 1 319 2)  (    -9    -54    2.5) 3.7)
 
  ((1 321 1) (1 321 0)  (   -15  -60.5    2.5) 3.2)
 
  ((1 321 2) (1 321 1)  ( -17.5    -64    2.5) 4.4)
 
  ((1 321 3) (1 321 2)  ( -25.5    -63    1.5) 2.3)
 
  ((1 321 4) (1 321 3)  (   -25    -58      2) 2.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 322, parent index 320
 
  ((1 322 0) (BRANCH-PT 1 321 3)  ( -25.5    -63    1.5) 2.3)
 
  ((1 322 1) (1 322 0)  ( -30.5  -61.5    1.5) 2.3)
 
  ((1 322 2) (1 322 1)  ( -36.5  -65.5    1.5) 2.3)
 
  ((1 322 3) (1 322 2)  ( -38.5  -67.5      2) 2.3)
 
  ((1 322 4) (1 322 3)  (   -39    -75      2) 2.2)
 
  ((1 322 5) (1 322 4)  ( -46.5  -79.5    1.5) 2.2)
 
  ((1 322 6) (1 322 5)  ( -48.5  -76.5    1.5) 2.3)
 
  ((1 322 7) (1 322 6)  ( -63.5    -81    1.5) 2.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 323, parent index 321
 
  ((1 323 0) (BRANCH-PT 1 322 5)  ( -46.5  -79.5    1.5) 2.2)
 
  ((1 323 1) (1 323 0)  (   -55  -90.5      0) 2.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 324, parent index 321
 
  ((1 324 0) (BRANCH-PT 1 322 3)  ( -38.5  -67.5      2) 2.3)
 
  ((1 324 1) (1 324 0)  (   -42  -69.5    1.5) 2.3)
 
  ((1 324 2) (1 324 1)  ( -44.5  -66.5    1.5) 2.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 325, parent index 323
 
  ((1 325 0) (BRANCH-PT 1 324 1)  (   -42  -69.5    1.5) 2.3)
 
  ((1 325 1) (1 325 0)  ( -45.5  -73.5    1.5) 2.3)
 
  ((1 325 2) (1 325 1)  (   -51    -74    1.5) 2.3)
 
  ((1 325 3) (1 325 2)  (   -53  -77.5    1.5) 1.7)
 
  ((1 325 4) (1 325 3)  (   -63    -81    1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 326, parent index 324
 
  ((1 326 0) (BRANCH-PT 1 325 2)  (   -51    -74    1.5) 2.3)
 
  ((1 326 1) (1 326 0)  ( -53.5    -73    1.5) 1.7)
 
  ((1 326 2) (1 326 1)  (   -55    -66      2) 1.7)
 
  ((1 326 3) (1 326 2)  ( -54.5    -61    1.5) 1.7)
 
  ((1 326 4) (1 326 3)  ( -57.5    -52      2) 1.7)
 
  ((1 326 5) (1 326 4)  ( -55.5  -50.5      2) 1.7)
 
  ((1 326 6) (1 326 5)  ( -48.5  -54.5      3) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 327, parent index 325
 
  ((1 327 0) (BRANCH-PT 1 326 5)  ( -55.5  -50.5      2) 1.7)
 
  ((1 327 1) (1 327 0)  ( -52.5    -46    2.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 328, parent index 325
 
  ((1 328 0) (BRANCH-PT 1 326 2)  (   -55    -66      2) 1.7)
 
  ((1 328 1) (1 328 0)  (   -60    -62    2.5) 1.7)
 
  ((1 328 2) (1 328 1)  ( -63.5  -58.5      3) 1.7)
 
  ((1 328 3) (1 328 2)  (   -78    -62      4) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 329, parent index 327
 
  ((1 329 0) (BRANCH-PT 1 328 2)  ( -63.5  -58.5      3) 1.7)
 
  ((1 329 1) (1 329 0)  (   -65    -55      3) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 330, parent index 325
 
  ((1 330 0) (BRANCH-PT 1 326 1)  ( -53.5    -73    1.5) 1.7)
 
  ((1 330 1) (1 330 0)  ( -60.5    -73    2.5) 1.7)
 
  ((1 330 2) (1 330 1)  ( -66.5    -65      3) 1.7)
 
  ((1 330 3) (1 330 2)  (   -73    -67      3) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 331, parent index 329
 
  ((1 331 0) (BRANCH-PT 1 330 1)  ( -60.5    -73    2.5) 1.7)
 
  ((1 331 1) (1 331 0)  ( -65.5    -74    2.5) 1.7)
 
  ((1 331 2) (1 331 1)  (   -70    -77    2.5) 1.7)
 
  ((1 331 3) (1 331 2)  (   -79  -74.5    2.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 332, parent index 330
 
  ((1 332 0) (BRANCH-PT 1 331 1)  ( -65.5    -74    2.5) 1.7)
 
  ((1 332 1) (1 332 0)  ( -70.5    -72    2.5) 1.7)
 
  ((1 332 2) (1 332 1)  ( -83.5    -80    1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 333, parent index 320
 
  ((1 333 0) (BRANCH-PT 1 321 2)  ( -17.5    -64    2.5) 4.4)
 
  ((1 333 1) (1 333 0)  ( -14.5    -75      0) 3.8)
 
  ((1 333 2) (1 333 1)  (    -9  -79.5   -0.5) 2.8)
 
  ((1 333 3) (1 333 2)  (  -5.5    -84    0.5) 2.4)
 
  ((1 333 4) (1 333 3)  (     4    -83    1.5) 2.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 334, parent index 332
 
  ((1 334 0) (BRANCH-PT 1 333 3)  (  -5.5    -84    0.5) 2.4)
 
  ((1 334 1) (1 334 0)  (    -7  -92.5   -0.5) 2.4)
 
  ((1 334 2) (1 334 1)  (    -5   -101      0) 2.1)
 
  ((1 334 3) (1 334 2)  (     0 -103.5   -1.5) 2.1)
 
  ((1 334 4) (1 334 3)  (    10 -105.5      0)   2)
 
  ((1 334 5) (1 334 4)  (   5.5 -113.5      1)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 335, parent index 333
 
  ((1 335 0) (BRANCH-PT 1 334 4)  (    10 -105.5      0)   2)
 
  ((1 335 1) (1 335 0)  (  12.5   -107      0)   2)
 
  ((1 335 2) (1 335 1)  (    13 -115.5      0)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 336, parent index 334
 
  ((1 336 0) (BRANCH-PT 1 335 1)  (  12.5   -107      0)   2)
 
  ((1 336 1) (1 336 0)  (    19   -105    0.5)   2)
 
  ((1 336 2) (1 336 1)  (    25   -108    0.5)   2)
 
  ((1 336 3) (1 336 2)  (  25.5   -113      1)   2)
 
  ((1 336 4) (1 336 3)  (  20.5   -117      1)   2)
 
  ((1 336 5) (1 336 4)  (    16   -117      1)   2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 337, parent index 333
 
  ((1 337 0) (BRANCH-PT 1 334 1)  (    -7  -92.5   -0.5) 2.4)
 
  ((1 337 1) (1 337 0)  (   -13   -104      0) 2.7)
 
  ((1 337 2) (1 337 1)  (   -14   -110      0) 2.7)
 
  ((1 337 3) (1 337 2)  (   -10   -115    1.5) 2.7)
 
  ((1 337 4) (1 337 3)  (  -5.5   -119      1) 2.7)
 
  ((1 337 5) (1 337 4)  (    -1   -121    1.5) 2.6)
 
  ((1 337 6) (1 337 5)  (   6.5   -120    1.5) 2.6)
 
  ((1 337 7) (1 337 6)  (    10   -118    1.5) 1.8)
 
  ((1 337 8) (1 337 7)  (     5   -115      2) 1.8)
 
  ((1 337 9) (1 337 8)  (     2   -109      2) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 338, parent index 336
 
  ((1 338 0) (BRANCH-PT 1 337 7)  (    10   -118    1.5) 1.8)
 
  ((1 338 1) (1 338 0)  (  14.5   -118    1.5) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 339, parent index 336
 
  ((1 339 0) (BRANCH-PT 1 337 5)  (    -1   -121    1.5) 2.6)
 
  ((1 339 1) (1 339 0)  (    -2   -126      1) 1.8)
 
  ((1 339 2) (1 339 1)  (  -3.5   -133      1) 1.8)
 
  ((1 339 3) (1 339 2)  (  -1.5   -137      1) 1.8)
 
  ((1 339 4) (1 339 3)  (   2.5 -140.5    1.5) 1.8)
 
  ((1 339 5) (1 339 4)  (   6.5 -140.5      1) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 340, parent index 336
 
  ((1 340 0) (BRANCH-PT 1 337 4)  (  -5.5   -119      1) 2.7)
 
  ((1 340 1) (1 340 0)  (    -6 -119.5   -2.5) 2.1)
 
  ((1 340 2) (1 340 1)  (   -12 -117.5   -2.5) 1.2)
 
  ((1 340 3) (1 340 2)  (   -12 -120.5   -2.5) 1.2)
 
  ((1 340 4) (1 340 3)  (  -9.5 -123.5   -2.5)   1)
 
  ((1 340 5) (1 340 4)  (    -9   -127     -3)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 341, parent index 339
 
  ((1 341 0) (BRANCH-PT 1 340 3)  (   -12 -120.5   -2.5) 1.2)
 
  ((1 341 1) (1 341 0)  (   -12 -122.5   -2.5)   1)
 
  ((1 341 2) (1 341 1)  (   -12   -127     -2)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 342, parent index 340
 
  ((1 342 0) (BRANCH-PT 1 341 1)  (   -12 -122.5   -2.5)   1)
 
  ((1 342 1) (1 342 0)  (   -19 -125.5     -3)   1)
 
  ((1 342 2) (1 342 1)  ( -22.5   -133     -2)   1)
 
  ((1 342 3) (1 342 2)  ( -26.5 -137.5   -2.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 343, parent index 341
 
  ((1 343 0) (BRANCH-PT 1 342 1)  (   -19 -125.5     -3)   1)
 
  ((1 343 1) (1 343 0)  ( -27.5   -125     -3)   1)
 
  ((1 343 2) (1 343 1)  ( -31.5   -128     -3)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 344, parent index 339
 
  ((1 344 0) (BRANCH-PT 1 340 1)  (    -6 -119.5   -2.5) 2.1)
 
  ((1 344 1) (1 344 0)  (    -5   -125   -2.5) 2.1)
 
  ((1 344 2) (1 344 1)  (  -6.5   -131     -3) 2.1)
 
  ((1 344 3) (1 344 2)  (  -8.5   -131     -3)   1)
 
  ((1 344 4) (1 344 3)  (  -9.5   -135   -3.5)   1)
 
  ((1 344 5) (1 344 4)  ( -11.5 -133.5   -3.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 345, parent index 343
 
  ((1 345 0) (BRANCH-PT 1 344 4)  (  -9.5   -135   -3.5)   1)
 
  ((1 345 1) (1 345 0)  (  -9.5 -138.5   -3.5)   1)
 
  ((1 345 2) (1 345 1)  ( -12.5 -137.5   -3.5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 346, parent index 343
 
  ((1 346 0) (BRANCH-PT 1 344 2)  (  -6.5   -131     -3) 2.1)
 
  ((1 346 1) (1 346 0)  (  -8.5 -139.5   -2.5) 2.6)
 
  ((1 346 2) (1 346 1)  (    -9   -144   -2.5) 2.1)
 
  ((1 346 3) (1 346 2)  (  -4.5 -148.5   -6.5) 1.4)
 
  ((1 346 4) (1 346 3)  (   1.5 -150.5   -6.5) 1.2)
 
  ((1 346 5) (1 346 4)  (     5 -151.5   -6.5) 1.2)
 
  ((1 346 6) (1 346 5)  (   6.5 -154.5     -6) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 347, parent index 345
 
  ((1 347 0) (BRANCH-PT 1 346 5)  (     5 -151.5   -6.5) 1.2)
 
  ((1 347 1) (1 347 0)  (   8.5 -150.5   -6.5) 1.2)
 
  ((1 347 2) (1 347 1)  (    13   -154     -6) 1.2)
 
  ((1 347 3) (1 347 2)  (    11   -166     -6) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 348, parent index 346
 
  ((1 348 0) (BRANCH-PT 1 347 2)  (    13   -154     -6) 1.2)
 
  ((1 348 1) (1 348 0)  (  13.5 -154.5     -6) 1.2)
 
  ((1 348 2) (1 348 1)  (    15 -157.5     -5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 349, parent index 347
 
  ((1 349 0) (BRANCH-PT 1 348 1)  (  13.5 -154.5     -6) 1.2)
 
  ((1 349 1) (1 349 0)  (  19.5 -154.5   -5.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 350, parent index 345
 
  ((1 350 0) (BRANCH-PT 1 346 3)  (  -4.5 -148.5   -6.5) 1.4)
 
  ((1 350 1) (1 350 0)  (  -4.5   -152   -6.5) 0.8)
 
  ((1 350 2) (1 350 1)  (     4   -155     -6) 0.8)
 
  ((1 350 3) (1 350 2)  (   9.5 -161.5   -4.5) 0.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 351, parent index 345
 
  ((1 351 0) (BRANCH-PT 1 346 2)  (    -9   -144   -2.5) 2.1)
 
  ((1 351 1) (1 351 0)  (  -9.5 -149.5   -5.5) 3.2)
 
  ((1 351 2) (1 351 1)  (  -4.5   -158   -5.5) 2.2)
 
  ((1 351 3) (1 351 2)  (  -0.5 -159.5     -5) 1.6)
 
  ((1 351 4) (1 351 3)  (   4.5 -158.5     -5) 1.6)
 
  ((1 351 5) (1 351 4)  (     6   -162     -6) 1.6)
 
  ((1 351 6) (1 351 5)  (   4.5 -167.5     -6) 1.6)
 
  ((1 351 7) (1 351 6)  (   9.5 -173.5   -5.5) 1.3)
 
  ((1 351 8) (1 351 7)  (  13.5 -175.5     -5) 1.2)
 
  ((1 351 9) (1 351 8)  (    15   -178     -5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 352, parent index 350
 
  ((1 352 0) (BRANCH-PT 1 351 8)  (  13.5 -175.5     -5) 1.2)
 
  ((1 352 1) (1 352 0)  (    13 -176.5   -5.5) 1.1)
 
  ((1 352 2) (1 352 1)  (  10.5 -178.5   -5.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 353, parent index 350
 
  ((1 353 0) (BRANCH-PT 1 351 7)  (   9.5 -173.5   -5.5) 1.3)
 
  ((1 353 1) (1 353 0)  (  13.5   -180     -8) 1.1)
 
  ((1 353 2) (1 353 1)  (    19   -184   -7.5) 1.1)
 
  ((1 353 3) (1 353 2)  (    23   -188     -7) 1.1)
 
  ((1 353 4) (1 353 3)  (    24 -193.5   -6.5) 1.1)
 
  ((1 353 5) (1 353 4)  (    29   -196   -6.5) 1.1)
 
  ((1 353 6) (1 353 5)  (  32.5   -200   -6.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 354, parent index 352
 
  ((1 354 0) (BRANCH-PT 1 353 4)  (    24 -193.5   -6.5) 1.1)
 
  ((1 354 1) (1 354 0)  (    24 -196.5   -5.5) 1.1)
 
  ((1 354 2) (1 354 1)  (    24 -199.5     -8) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 355, parent index 352
 
  ((1 355 0) (BRANCH-PT 1 353 2)  (    19   -184   -7.5) 1.1)
 
  ((1 355 1) (1 355 0)  (  21.5 -190.5     -8) 1.1)
 
  ((1 355 2) (1 355 1)  (  21.5 -197.5     -8) 1.1)
 
  ((1 355 3) (1 355 2)  (    25 -205.5   -8.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 356, parent index 354
 
  ((1 356 0) (BRANCH-PT 1 355 2)  (  21.5 -197.5     -8) 1.1)
 
  ((1 356 1) (1 356 0)  (  16.5   -202     -7) 1.1)
 
  ((1 356 2) (1 356 1)  (    17 -207.5   -7.5) 1.1)
 
  ((1 356 3) (1 356 2)  (  21.5   -213   -7.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 357, parent index 352
 
  ((1 357 0) (BRANCH-PT 1 353 1)  (  13.5   -180     -8) 1.1)
 
  ((1 357 1) (1 357 0)  (  13.5 -186.5     -9) 1.1)
 
  ((1 357 2) (1 357 1)  (  17.5   -196   -8.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 358, parent index 356
 
  ((1 358 0) (BRANCH-PT 1 357 1)  (  13.5 -186.5     -9) 1.1)
 
  ((1 358 1) (1 358 0)  (     8   -195     -9) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 359, parent index 350
 
  ((1 359 0) (BRANCH-PT 1 351 6)  (   4.5 -167.5     -6) 1.6)
 
  ((1 359 1) (1 359 0)  (     6   -179     -9) 1.1)
 
  ((1 359 2) (1 359 1)  (     8   -183   -8.5) 1.1)
 
  ((1 359 3) (1 359 2)  (   5.5 -188.5   -8.5) 1.1)
 
  ((1 359 4) (1 359 3)  (   2.5 -192.5   -8.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 360, parent index 350
 
  ((1 360 0) (BRANCH-PT 1 351 2)  (  -4.5   -158   -5.5) 2.2)
 
  ((1 360 1) (1 360 0)  (  -3.5   -161   -6.5) 1.1)
 
  ((1 360 2) (1 360 1)  (  -3.5 -165.5     -5) 1.1)
 
  ((1 360 3) (1 360 2)  (    -1   -169   -4.5) 1.1)
 
  ((1 360 4) (1 360 3)  (     3 -170.5   -4.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 361, parent index 359
 
  ((1 361 0) (BRANCH-PT 1 360 3)  (    -1   -169   -4.5) 1.1)
 
  ((1 361 1) (1 361 0)  (     2 -173.5   -4.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 362, parent index 359
 
  ((1 362 0) (BRANCH-PT 1 360 1)  (  -3.5   -161   -6.5) 1.1)
 
  ((1 362 1) (1 362 0)  (    -8   -161   -6.5) 1.1)
 
  ((1 362 2) (1 362 1)  (  -9.5   -166   -6.5) 1.1)
 
  ((1 362 3) (1 362 2)  (  -4.5 -173.5   -6.5) 1.1)
 
  ((1 362 4) (1 362 3)  (  -2.5   -174   -5.5) 1.1)
 
  ((1 362 5) (1 362 4)  (     1   -175     -6) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 363, parent index 361
 
  ((1 363 0) (BRANCH-PT 1 362 4)  (  -2.5   -174   -5.5) 1.1)
 
  ((1 363 1) (1 363 0)  (    -1   -177     -4) 1.1)
 
  ((1 363 2) (1 363 1)  (     4   -182   -6.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 364, parent index 362
 
  ((1 364 0) (BRANCH-PT 1 363 1)  (    -1   -177     -4) 1.1)
 
  ((1 364 1) (1 364 0)  (     1 -184.5     -4) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 365, parent index 350
 
  ((1 365 0) (BRANCH-PT 1 351 1)  (  -9.5 -149.5   -5.5) 3.2)
 
  ((1 365 1) (1 365 0)  (   -11   -154   -5.5) 1.7)
 
  ((1 365 2) (1 365 1)  ( -10.5 -158.5   -5.5) 1.4)
 
  ((1 365 3) (1 365 2)  ( -15.5 -160.5     -6) 1.4)
 
  ((1 365 4) (1 365 3)  (   -14 -163.5     -6) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 366, parent index 364
 
  ((1 366 0) (BRANCH-PT 1 365 1)  (   -11   -154   -5.5) 1.7)
 
  ((1 366 1) (1 366 0)  ( -17.5   -161   -5.5) 1.4)
 
  ((1 366 2) (1 366 1)  (   -18   -167   -5.5) 1.4)
 
  ((1 366 3) (1 366 2)  (   -17   -173   -5.5) 1.4)
 
  ((1 366 4) (1 366 3)  (   -17   -175   -5.5) 1.4)
 
  ((1 366 5) (1 366 4)  (  -7.5 -178.5     -5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 367, parent index 365
 
  ((1 367 0) (BRANCH-PT 1 366 4)  (   -17   -175   -5.5) 1.4)
 
  ((1 367 1) (1 367 0)  (   -19 -180.5     -5) 1.4)
 
  ((1 367 2) (1 367 1)  ( -14.5   -183     -5) 1.4)
 
  ((1 367 3) (1 367 2)  ( -12.5   -191     -5) 1.4)
 
  ((1 367 4) (1 367 3)  (   -10 -195.5   -4.5) 1.4)
 
  ((1 367 5) (1 367 4)  ( -11.5 -199.5     -5) 1.4)
 
  ((1 367 6) (1 367 5)  ( -14.5 -201.5     -5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 368, parent index 366
 
  ((1 368 0) (BRANCH-PT 1 367 5)  ( -11.5 -199.5     -5) 1.4)
 
  ((1 368 1) (1 368 0)  (  -9.5   -202   -4.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 369, parent index 366
 
  ((1 369 0) (BRANCH-PT 1 367 4)  (   -10 -195.5   -4.5) 1.4)
 
  ((1 369 1) (1 369 0)  (    -5 -198.5   -4.5) 1.4)
 
  ((1 369 2) (1 369 1)  (    -3 -198.5     -4) 1.4)
 
  ((1 369 3) (1 369 2)  (   0.5 -203.5     -4) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 370, parent index 368
 
  ((1 370 0) (BRANCH-PT 1 369 2)  (    -3 -198.5     -4) 1.4)
 
  ((1 370 1) (1 370 0)  (  -0.5 -197.5     -4) 1.4)
 
  ((1 370 2) (1 370 1)  (   4.5   -200   -3.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 371, parent index 369
 
  ((1 371 0) (BRANCH-PT 1 370 1)  (  -0.5 -197.5     -4) 1.4)
 
  ((1 371 1) (1 371 0)  (  -1.5   -196     -4) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 372, parent index 366
 
  ((1 372 0) (BRANCH-PT 1 367 2)  ( -14.5   -183     -5) 1.4)
 
  ((1 372 1) (1 372 0)  (    -9   -185   -5.5) 0.9)
 
  ((1 372 2) (1 372 1)  (    -8 -187.5   -3.5) 0.9)
 
  ((1 372 3) (1 372 2)  ( -10.5 -187.5   -3.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 373, parent index 371
 
  ((1 373 0) (BRANCH-PT 1 372 2)  (    -8 -187.5   -3.5) 0.9)
 
  ((1 373 1) (1 373 0)  (    -8 -190.5   -3.5) 0.9)
 
  ((1 373 2) (1 373 1)  (   -14   -195   -3.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 374, parent index 372
 
  ((1 374 0) (BRANCH-PT 1 373 1)  (    -8 -190.5   -3.5) 0.9)
 
  ((1 374 1) (1 374 0)  (    -2 -194.5   -3.5) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 375, parent index 366
 
  ((1 375 0) (BRANCH-PT 1 367 1)  (   -19 -180.5     -5) 1.4)
 
  ((1 375 1) (1 375 0)  (   -25   -183   -6.5) 1.6)
 
  ((1 375 2) (1 375 1)  (   -20 -186.5   -6.5) 1.6)
 
  ((1 375 3) (1 375 2)  (   -16   -187   -6.5) 1.6)
 
  ((1 375 4) (1 375 3)  ( -10.5 -193.5   -6.5) 1.6)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 376, parent index 374
 
  ((1 376 0) (BRANCH-PT 1 375 1)  (   -25   -183   -6.5) 1.6)
 
  ((1 376 1) (1 376 0)  ( -31.5 -185.5   -6.5) 1.6)
 
  ((1 376 2) (1 376 1)  ( -37.5   -185   -6.5) 1.6)
 
  ((1 376 3) (1 376 2)  (   -44 -188.5   -6.5) 1.6)
 
  ((1 376 4) (1 376 3)  (   -44   -193     -6) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 377, parent index 375
 
  ((1 377 0) (BRANCH-PT 1 376 2)  ( -37.5   -185   -6.5) 1.6)
 
  ((1 377 1) (1 377 0)  (   -42 -182.5   -6.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 378, parent index 332
 
  ((1 378 0) (BRANCH-PT 1 333 1)  ( -14.5    -75      0) 3.8)
 
  ((1 378 1) (1 378 0)  ( -14.5    -80      0) 3.1)
 
  ((1 378 2) (1 378 1)  (   -21  -79.5      0) 2.1)
 
  ((1 378 3) (1 378 2)  (   -24    -77    0.5) 1.9)
 
  ((1 378 4) (1 378 3)  (   -28    -72    0.5) 1.9)
 
  ((1 378 5) (1 378 4)  (   -28  -66.5      1) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 379, parent index 377
 
  ((1 379 0) (BRANCH-PT 1 378 4)  (   -28    -72    0.5) 1.9)
 
  ((1 379 1) (1 379 0)  (   -31  -68.5      1) 1.9)
 
  ((1 379 2) (1 379 1)  (   -34  -69.5      1) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 380, parent index 377
 
  ((1 380 0) (BRANCH-PT 1 378 3)  (   -24    -77    0.5) 1.9)
 
  ((1 380 1) (1 380 0)  ( -33.5  -79.5      0) 1.7)
 
  ((1 380 2) (1 380 1)  (   -37  -81.5      0) 1.7)
 
  ((1 380 3) (1 380 2)  ( -43.5    -80      0) 1.7)
 
  ((1 380 4) (1 380 3)  ( -49.5  -76.5      0) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 381, parent index 379
 
  ((1 381 0) (BRANCH-PT 1 380 2)  (   -37  -81.5      0) 1.7)
 
  ((1 381 1) (1 381 0)  (   -44    -93      3) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 382, parent index 377
 
  ((1 382 0) (BRANCH-PT 1 378 2)  (   -21  -79.5      0) 2.1)
 
  ((1 382 1) (1 382 0)  (   -27  -89.5   -0.5) 1.7)
 
  ((1 382 2) (1 382 1)  (   -34    -93   -0.5) 1.7)
 
  ((1 382 3) (1 382 2)  ( -48.5   -100   -0.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 383, parent index 377
 
  ((1 383 0) (BRANCH-PT 1 378 1)  ( -14.5    -80      0) 3.1)
 
  ((1 383 1) (1 383 0)  ( -12.5  -86.5    0.5) 3.2)
 
  ((1 383 2) (1 383 1)  (   -17  -92.5    0.5) 3.2)
 
  ((1 383 3) (1 383 2)  ( -24.5  -95.5    0.5) 3.2)
 
  ((1 383 4) (1 383 3)  ( -20.5   -103   -2.5)   2)
 
  ((1 383 5) (1 383 4)  (   -15 -100.5     -2)   2)
 
  ((1 383 6) (1 383 5)  (   -18   -110     -2)   2)
 
  ((1 383 7) (1 383 6)  ( -15.5   -119     -2) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 384, parent index 382
 
  ((1 384 0) (BRANCH-PT 1 383 4)  ( -20.5   -103   -2.5)   2)
 
  ((1 384 1) (1 384 0)  ( -23.5 -107.5     -2) 1.4)
 
  ((1 384 2) (1 384 1)  (   -23   -115      0) 1.4)
 
  ((1 384 3) (1 384 2)  ( -19.5 -121.5   -0.5) 1.4)
 
  ((1 384 4) (1 384 3)  (   -19   -125    0.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 385, parent index 383
 
  ((1 385 0) (BRANCH-PT 1 384 3)  ( -19.5 -121.5   -0.5) 1.4)
 
  ((1 385 1) (1 385 0)  ( -26.5   -118   -0.5) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 386, parent index 383
 
  ((1 386 0) (BRANCH-PT 1 384 1)  ( -23.5 -107.5     -2) 1.4)
 
  ((1 386 1) (1 386 0)  ( -29.5   -118     -1) 1.3)
 
  ((1 386 2) (1 386 1)  ( -29.5 -121.5     -1) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 387, parent index 382
 
  ((1 387 0) (BRANCH-PT 1 383 3)  ( -24.5  -95.5    0.5) 3.2)
 
  ((1 387 1) (1 387 0)  ( -31.5    -99    0.5) 3.5)
 
  ((1 387 2) (1 387 1)  (   -35  -96.5      1) 1.9)
 
  ((1 387 3) (1 387 2)  (   -41   -101      1) 1.9)
 
  ((1 387 4) (1 387 3)  (   -42  -98.5      2) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 388, parent index 386
 
  ((1 388 0) (BRANCH-PT 1 387 3)  (   -41   -101      1) 1.9)
 
  ((1 388 1) (1 388 0)  (   -49 -107.5      1) 1.5)
 
  ((1 388 2) (1 388 1)  (   -53   -105      1) 1.3)
 
  ((1 388 3) (1 388 2)  ( -54.5 -108.5      1) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 389, parent index 386
 
  ((1 389 0) (BRANCH-PT 1 387 1)  ( -31.5    -99    0.5) 3.5)
 
  ((1 389 1) (1 389 0)  ( -38.5   -110      0) 3.3)
 
  ((1 389 2) (1 389 1)  ( -35.5   -113      0) 1.5)
 
  ((1 389 3) (1 389 2)  ( -31.5   -115      1) 1.4)
 
  ((1 389 4) (1 389 3)  (   -27   -117      2) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 390, parent index 388
 
  ((1 390 0) (BRANCH-PT 1 389 2)  ( -35.5   -113      0) 1.5)
 
  ((1 390 1) (1 390 0)  (   -35   -124    0.5) 1.2)
 
  ((1 390 2) (1 390 1)  ( -32.5 -126.5    0.5) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 391, parent index 388
 
  ((1 391 0) (BRANCH-PT 1 389 1)  ( -38.5   -110      0) 3.3)
 
  ((1 391 1) (1 391 0)  (   -43 -117.5    0.5) 3.6)
 
  ((1 391 2) (1 391 1)  (   -42   -127    1.5) 4.6)
 
  ((1 391 3) (1 391 2)  (   -47   -135      0) 1.9)
 
  ((1 391 4) (1 391 3)  ( -46.5   -141     -1) 1.9)
 
  ((1 391 5) (1 391 4)  (   -52   -153     -2) 1.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 392, parent index 390
 
  ((1 392 0) (BRANCH-PT 1 391 4)  ( -46.5   -141     -1) 1.9)
 
  ((1 392 1) (1 392 0)  (   -42 -147.5     -1) 1.8)
 
  ((1 392 2) (1 392 1)  (   -43   -153     -1) 1.7)
 
  ((1 392 3) (1 392 2)  ( -47.5   -154     -1) 1.7)
 
  ((1 392 4) (1 392 3)  ( -49.5   -157   -1.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 393, parent index 390
 
  ((1 393 0) (BRANCH-PT 1 391 2)  (   -42   -127    1.5) 4.6)
 
  ((1 393 1) (1 393 0)  ( -38.5 -133.5      2) 3.5)
 
  ((1 393 2) (1 393 1)  ( -32.5 -131.5    1.5) 1.9)
 
  ((1 393 3) (1 393 2)  (   -25   -132      2) 1.9)
 
  ((1 393 4) (1 393 3)  ( -19.5   -129    2.5) 1.4)
 
  ((1 393 5) (1 393 4)  ( -19.5   -126      3) 1.4)
 
  ((1 393 6) (1 393 5)  (   -25 -125.5    3.5) 1.8)
 
  ((1 393 7) (1 393 6)  (   -29   -122    3.5) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 394, parent index 392
 
  ((1 394 0) (BRANCH-PT 1 393 5)  ( -19.5   -126      3) 1.4)
 
  ((1 394 1) (1 394 0)  ( -18.5   -123      3) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 395, parent index 392
 
  ((1 395 0) (BRANCH-PT 1 393 1)  ( -38.5 -133.5      2) 3.5)
 
  ((1 395 1) (1 395 0)  (   -36   -144     -1) 3.4)
 
  ((1 395 2) (1 395 1)  ( -21.5   -145     -1) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 396, parent index 394
 
  ((1 396 0) (BRANCH-PT 1 395 1)  (   -36   -144     -1) 3.4)
 
  ((1 396 1) (1 396 0)  (   -35 -145.5     -3) 4.1)
 
  ((1 396 2) (1 396 1)  (   -32   -149   -2.5) 1.7)
 
  ((1 396 3) (1 396 2)  (   -32   -156   -2.5) 1.7)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 8
;; Section 397, parent index 395
 
  ((1 397 0) (BRANCH-PT 1 396 2)  (   -32   -149   -2.5) 1.7)
 
  ((1 397 1) (1 397 0)  (   -29 -151.5   -2.5) 1.6)
 
  ((1 397 2) (1 397 1)  (   -22 -151.5   -0.5) 1.6)
 
  ((1 397 3) (1 397 2)  ( -17.5   -148     -1) 1.6)
 
  ((1 397 4) (1 397 3)  ( -17.5 -144.5     -1) 1.2)
 
  ((1 397 5) (1 397 4)  ( -20.5   -140     -1) 1.2)
 
  ((1 397 6) (1 397 5)  ( -21.5   -136     -1) 1.2)
 
  ((1 397 7) (1 397 6)  (   -18   -133     -1) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 398, parent index 396
 
  ((1 398 0) (BRANCH-PT 1 397 4)  ( -17.5 -144.5     -1) 1.2)
 
  ((1 398 1) (1 398 0)  ( -15.5 -140.5     -2) 1.2)
 
  ((1 398 2) (1 398 1)  (   -17   -136     -2) 1.2)
 
  ((1 398 3) (1 398 2)  (   -15 -132.5     -2) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 399, parent index 396
 
  ((1 399 0) (BRANCH-PT 1 397 2)  (   -22 -151.5   -0.5) 1.6)
 
  ((1 399 1) (1 399 0)  ( -17.5 -154.5     -4) 1.3)
 
  ((1 399 2) (1 399 1)  (   -16   -158     -4) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 400, parent index 398
 
  ((1 400 0) (BRANCH-PT 1 399 1)  ( -17.5 -154.5     -4) 1.3)
 
  ((1 400 1) (1 400 0)  (   -14   -153     -4) 0.9)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 401, parent index 396
 
  ((1 401 0) (BRANCH-PT 1 397 1)  (   -29 -151.5   -2.5) 1.6)
 
  ((1 401 1) (1 401 0)  ( -29.5   -154   -2.5) 0.9)
 
  ((1 401 2) (1 401 1)  ( -23.5 -157.5   -1.5) 1.1)
 
  ((1 401 3) (1 401 2)  (   -21 -158.5   -1.5) 1.3)
 
  ((1 401 4) (1 401 3)  ( -17.5 -157.5   -1.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 402, parent index 400
 
  ((1 402 0) (BRANCH-PT 1 401 3)  (   -21 -158.5   -1.5) 1.3)
 
  ((1 402 1) (1 402 0)  ( -17.5 -159.5     -1) 1.4)
 
  ((1 402 2) (1 402 1)  (   -11 -165.5     -1) 1.4)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 7
;; Section 403, parent index 401
 
  ((1 403 0) (BRANCH-PT 1 402 1)  ( -17.5 -159.5     -1) 1.4)
 
  ((1 403 1) (1 403 0)  ( -19.5   -162   -0.5) 1.4)
 
  ((1 403 2) (1 403 1)  ( -17.5 -164.5   -3.5) 1.3)
 
  ((1 403 3) (1 403 2)  (   -12 -167.5   -3.5) 1.3)
 
  ((1 403 4) (1 403 3)  (    -7   -173   -3.5) 1.3)
 
  ((1 403 5) (1 403 4)  (  -4.5   -180   -3.5) 1.3)
 
  ((1 403 6) (1 403 5)  (    -1   -184     -3) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 404, parent index 402
 
  ((1 404 0) (BRANCH-PT 1 403 5)  (  -4.5   -180   -3.5) 1.3)
 
  ((1 404 1) (1 404 0)  (     2   -181   -3.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 405, parent index 402
 
  ((1 405 0) (BRANCH-PT 1 403 2)  ( -17.5 -164.5   -3.5) 1.3)
 
  ((1 405 1) (1 405 0)  ( -14.5   -168   -3.5) 1.3)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 6
;; Section 406, parent index 395
 
  ((1 406 0) (BRANCH-PT 1 396 1)  (   -35 -145.5     -3) 4.1)
 
  ((1 406 1) (1 406 0)  (   -37   -149     -3) 3.6)
 
  ((1 406 2) (1 406 1)  (   -38   -157   -3.5) 4.2)
 
  ((1 406 3) (1 406 2)  ( -43.5 -163.5     -4) 2.3)
 
  ((1 406 4) (1 406 3)  (   -47 -167.5     -4) 1.1)
 
  ((1 406 5) (1 406 4)  ( -48.5   -172   -4.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 407, parent index 405
 
  ((1 407 0) (BRANCH-PT 1 406 3)  ( -43.5 -163.5     -4) 2.3)
 
  ((1 407 1) (1 407 0)  ( -41.5 -165.5     -4) 1.3)
 
  ((1 407 2) (1 407 1)  (   -36   -169     -4) 1.3)
 
  ((1 407 3) (1 407 2)  ( -31.5   -173     -4) 1.1)
 
  ((1 407 4) (1 407 3)  ( -31.5 -175.5   -5.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 408, parent index 406
 
  ((1 408 0) (BRANCH-PT 1 407 1)  ( -41.5 -165.5     -4) 1.3)
 
  ((1 408 1) (1 408 0)  ( -41.5   -172   -4.5) 1.1)
 
  ((1 408 2) (1 408 1)  ( -35.5 -176.5     -3) 1.1)
 
  ((1 408 3) (1 408 2)  ( -35.5 -178.5   -2.5) 1.1)
 
  ((1 408 4) (1 408 3)  ( -30.5 -180.5     -1) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 409, parent index 407
 
  ((1 409 0) (BRANCH-PT 1 408 3)  ( -35.5 -178.5   -2.5) 1.1)
 
  ((1 409 1) (1 409 0)  ( -45.5   -182     -1) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 10
;; Section 410, parent index 405
 
  ((1 410 0) (BRANCH-PT 1 406 2)  (   -38   -157   -3.5) 4.2)
 
  ((1 410 1) (1 410 0)  ( -34.5 -161.5   -3.5) 2.6)
 
  ((1 410 2) (1 410 1)  ( -31.5 -162.5   -5.5) 1.6)
 
  ((1 410 3) (1 410 2)  ( -25.5 -165.5   -5.5) 1.6)
 
  ((1 410 4) (1 410 3)  ( -20.5 -171.5     -5) 1.5)
 
  ((1 410 5) (1 410 4)  (   -17 -175.5     -5) 1.5)
 
  ((1 410 6) (1 410 5)  (   -12   -178     -5) 1.5)
 
  ((1 410 7) (1 410 6)  (    -7   -183   -4.5) 1.5)
 
  ((1 410 8) (1 410 7)  (    -3   -186   -3.5) 1.1)
 
  ((1 410 9) (1 410 8)  (  -3.5   -190   -3.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 411, parent index 409
 
  ((1 411 0) (BRANCH-PT 1 410 8)  (    -3   -186   -3.5) 1.1)
 
  ((1 411 1) (1 411 0)  (   0.5 -187.5   -6.5) 1.1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 5
;; Section 412, parent index 409
 
  ((1 412 0) (BRANCH-PT 1 410 1)  ( -34.5 -161.5   -3.5) 2.6)
 
  ((1 412 1) (1 412 0)  ( -33.5   -167     -4) 3.2)
 
  ((1 412 2) (1 412 1)  ( -29.5   -171     -5) 2.5)
 
  ((1 412 3) (1 412 2)  ( -27.5 -172.5   -4.5) 2.5)
 
  ((1 412 4) (1 412 3)  ( -27.5   -167     -4) 1.5)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 413, parent index 411
 
  ((1 413 0) (BRANCH-PT 1 412 3)  ( -27.5 -172.5   -4.5) 2.5)
 
  ((1 413 1) (1 413 0)  (   -25   -174     -4) 2.6)
 
  ((1 413 2) (1 413 1)  (   -26 -178.5     -4) 1.2)
 
  ((1 413 3) (1 413 2)  (   -25   -185     -4) 1.2)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 12
;; Section 414, parent index 412
 
  ((1 414 0) (BRANCH-PT 1 413 1)  (   -25   -174     -4) 2.6)
 
  ((1 414 1) (1 414 0)  ( -21.5   -175   -4.5) 2.2)
 
  ((1 414 2) (1 414 1)  ( -17.5 -177.5     -4)   3)
 
  ((1 414 3) (1 414 2)  ( -11.5 -180.5     -3) 1.9)
 
  ((1 414 4) (1 414 3)  (  -6.5 -184.5     -3) 1.9)
 
  ((1 414 5) (1 414 4)  (    -4   -190     -2) 1.9)
 
  ((1 414 6) (1 414 5)  (  -2.5   -192   -7.5) 1.9)
 
  ((1 414 7) (1 414 6)  (     1 -193.5     -8) 1.3)
 
  ((1 414 8) (1 414 7)  (     4 -195.5     -7) 1.2)
 
  ((1 414 9) (1 414 8)  (   7.5   -196     -7) 1.2)
 
  ((1 414 10) (1 414 9)  (    10   -197   -6.5) 1.2)
 
  ((1 414 11) (1 414 10)  (  15.5   -197     -7)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 415, parent index 413
 
  ((1 415 0) (BRANCH-PT 1 414 10)  (    10   -197   -6.5) 1.2)
 
  ((1 415 1) (1 415 0)  (    12 -203.5   -5.5)   1)
 
  ((1 415 2) (1 415 1)  (  15.5 -209.5     -5)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 4
;; Section 416, parent index 413
 
  ((1 416 0) (BRANCH-PT 1 414 6)  (  -2.5   -192   -7.5) 1.9)
 
  ((1 416 1) (1 416 0)  (  -2.5 -196.5   -7.5)   1)
 
  ((1 416 2) (1 416 1)  (    -6 -199.5     -7)   1)
 
  ((1 416 3) (1 416 2)  (    -8 -197.5     -7)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 3
;; Section 417, parent index 415
 
  ((1 417 0) (BRANCH-PT 1 416 2)  (    -6 -199.5     -7)   1)
 
  ((1 417 1) (1 417 0)  (    -6   -203     -7)   1)
 
  ((1 417 2) (1 417 1)  (  -8.5   -205     -7)   1)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 2
;; Section 418, parent index 413
 
  ((1 418 0) (BRANCH-PT 1 414 2)  ( -17.5 -177.5     -4)   3)
 
  ((1 418 1) (1 418 0)  ( -17.5   -182     -4) 1.6)
;;; End of section ********* 
;;; End of neurite ********* 

))


(setq *process-ntscable-list* t)
(setq *nts-cell-type* "ntscable")
(setq *nts-cell-name* "ntscable")
(setq *nts-r-mem* 40000.0)
(setq *nts-soma-r-mem* 40000.0)
(setq *nts-r-a* 200.0)

;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF; Base: 10; -*- 

(in-package "SURF-HIPPO")

#|
/*----------------------------------------------------------------
%W%  %G%
/home/lyle/surf-hippo/anatomy/claiborne/DG-DBR  translated Thu May  2 20:58:59 1996 by ntscable %I%
source file syntax: Nevin
output file syntax: NEURON
soma: diameter = 6.31994 um  length = 20.0913 um  area = 398.907 um2
      5 three-D points numbered 1-5
2 primary neurites
12 branches totaling 2836.29 um in length, 16389.2 um2 in area
467 tree points translated to 14 segments (1 requested)
Neurites divided into segments of equal dx over each entire branch.
Segment length constrained to be < 2836.29 um.

/* 
DIG version 5.19  3-28-89  Rocky Nevin  
Comments: Very good defined spines male 49 days

ref: Rihn and Claiborne  Dev. Brain Research 54:115-124   1990

from: Mike O'Boyle

line 4 changed from "P" to "B"
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

    5
    3.75   -11.13        0     5.39
    0.66    -5.51        0    10.33
   -0.12     1.22        0     4.04
   -0.89     4.81        0     4.04
   -1.41     7.06     2.26     4.04


|#




;; No soma outline. 

(setq *soma-points*

 '(
  (3.75 -11.13 0 5.39)
  (0.66 -5.51 0 10.33)
  (-0.12 1.22 0 4.04)
  (-0.89 4.81 0 4.04)
  (-1.41 7.06 2.26 4.04)

 ))
(setq  *nts-radius* (sqrt (/ 398.907 (* 3.14159 4))))   
 
(setq *ntscable-list* '(

 ;;  Number of segs - 1, Number of xyzd points - 40
;; Section 1, parent index -1
 
  ((1 1 0)  SOMA  ( -0.89  13.79    2.9) 2.69)
 
  ((1 1 1) (1 1 0)  (  0.03  17.67  -0.17) 2.69)
 
  ((1 1 2) (1 1 1)  (  0.81  22.16  -0.14) 2.69)
 
  ((1 1 3) (1 1 2)  (  1.06   25.3  -0.74) 2.69)
 
  ((1 1 4) (1 1 3)  (  0.03  31.14  -5.31) 2.69)
 
  ((1 1 5) (1 1 4)  ( -1.26   36.3  -5.31) 2.69)
 
  ((1 1 6) (1 1 5)  ( -2.12  41.89  -4.26) 1.8)
 
  ((1 1 7) (1 1 6)  ( -2.63  42.56 -11.06) 1.8)
 
  ((1 1 8) (1 1 7)  ( -0.31  47.05 -12.62) 1.8)
 
  ((1 1 9) (1 1 8)  (  1.24  51.99 -18.09) 1.8)
 
  ((1 1 10) (1 1 9)  (  2.01  54.69 -22.73) 1.8)
 
  ((1 1 11) (1 1 10)  (  4.33  58.05 -27.14) 1.8)
 
  ((1 1 12) (1 1 11)  (   4.2   61.6 -30.06) 1.8)
 
  ((1 1 13) (1 1 12)  (  5.49  66.54 -30.16) 1.8)
 
  ((1 1 14) (1 1 13)  (  6.01  68.78 -35.14) 1.8)
 
  ((1 1 15) (1 1 14)  (  3.74  72.47 -35.14) 1.8)
 
  ((1 1 16) (1 1 15)  (  3.74  74.94 -42.74) 1.8)
 
  ((1 1 17) (1 1 16)  (  3.74  75.83 -42.74) 1.8)
 
  ((1 1 18) (1 1 17)  (  5.03   77.4 -42.74) 1.8)
 
  ((1 1 19) (1 1 18)  (   5.8  82.34 -44.28) 1.8)
 
  ((1 1 20) (1 1 19)  (   5.8  84.14 -47.14) 1.8)
 
  ((1 1 21) (1 1 20)  (  4.77   89.3 -48.61) 1.8)
 
  ((1 1 22) (1 1 21)  (  3.65  91.97 -49.03) 1.8)
 
  ((1 1 23) (1 1 22)  (  3.14  93.76 -49.03) 1.8)
 
  ((1 1 24) (1 1 23)  (  4.17  98.48  -53.9) 1.8)
 
  ((1 1 25) (1 1 24)  (  4.17 101.85  -53.9) 1.8)
 
  ((1 1 26) (1 1 25)  (  3.65 107.91 -55.53) 1.8)
 
  ((1 1 27) (1 1 26)  (  2.19 113.04 -58.79) 1.8)
 
  ((1 1 28) (1 1 27)  (  1.94 116.18 -62.14) 1.8)
 
  ((1 1 29) (1 1 28)  (  2.19 120.45 -65.52) 1.8)
 
  ((1 1 30) (1 1 29)  (  2.19 125.16 -64.19) 1.8)
 
  ((1 1 31) (1 1 30)  (  2.19 126.06 -64.19) 1.8)
 
  ((1 1 32) (1 1 31)  (  2.08 126.07 -65.43) 1.8)
 
  ((1 1 33) (1 1 32)  (  6.21  137.3 -68.56) 1.8)
 
  ((1 1 34) (1 1 33)  (  8.53 141.79 -69.95) 1.8)
 
  ((1 1 35) (1 1 34)  (  9.31 145.16 -73.26) 1.8)
 
  ((1 1 36) (1 1 35)  ( 13.49 154.37 -88.67) 2.24)
 
  ((1 1 37) (1 1 36)  (  13.8 156.74 -88.67) 2.24)
 
  ((1 1 38) (1 1 37)  ( 16.38 160.11 -89.93) 2.24)
 
  ((1 1 39) (1 1 38)  ( 18.18 163.03 -89.93) 2.24)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 45
;; Section 2, parent index 0
 
  ((1 2 0) (BRANCH-PT 1 1 32)  (  2.08 126.07 -65.43) 1.8)
 
  ((1 2 1) (1 2 0)  ( -0.38 127.89 -68.53) 1.8)
 
  ((1 2 2) (1 2 1)  ( -2.18 136.65 -68.53) 1.8)
 
  ((1 2 3) (1 2 2)  ( -4.51 146.08 -74.23) 1.8)
 
  ((1 2 4) (1 2 3)  (  -4.2 148.54 -74.23) 1.8)
 
  ((1 2 5) (1 2 4)  ( -5.75 151.91 -74.23) 1.8)
 
  ((1 2 6) (1 2 5)  ( -7.29  156.4 -75.98) 1.8)
 
  ((1 2 7) (1 2 6)  ( -8.33 162.69 -75.35) 1.8)
 
  ((1 2 8) (1 2 7)  ( -9.62 167.85 -75.55) 1.8)
 
  ((1 2 9) (1 2 8)  (-11.01 171.69 -77.04) 1.8)
 
  ((1 2 10) (1 2 9)  (-11.01 175.73 -77.04) 1.8)
 
  ((1 2 11) (1 2 10)  (-11.01 179.55 -77.04) 1.8)
 
  ((1 2 12) (1 2 11)  (-11.52 184.04 -77.04) 1.8)
 
  ((1 2 13) (1 2 12)  (-12.81 189.43 -77.04) 1.8)
 
  ((1 2 14) (1 2 13)  (-14.09 193.79 -77.04) 1.8)
 
  ((1 2 15) (1 2 14)  (-14.61 201.87 -77.04) 1.8)
 
  ((1 2 16) (1 2 15)  (-14.35 207.93 -77.04) 1.8)
 
  ((1 2 17) (1 2 16)  (-14.77 213.26 -78.26) 1.8)
 
  ((1 2 18) (1 2 17)  (-16.32  219.1 -78.26) 1.8)
 
  ((1 2 19) (1 2 18)  (-17.35 225.38 -78.26) 1.8)
 
  ((1 2 20) (1 2 19)  (-18.29 234.23 -78.69) 1.8)
 
  ((1 2 21) (1 2 20)  (-20.09 239.39 -78.69) 1.8)
 
  ((1 2 22) (1 2 21)  (-21.64 243.21 -78.69) 1.8)
 
  ((1 2 23) (1 2 22)  (-22.49 244.62 -78.69) 1.8)
 
  ((1 2 24) (1 2 23)  (-22.49 252.05 -78.32) 1.8)
 
  ((1 2 25) (1 2 24)  (-25.58 258.56 -77.98) 1.8)
 
  ((1 2 26) (1 2 25)  ( -25.3  261.2 -80.39) 1.8)
 
  ((1 2 27) (1 2 26)  (-25.83  262.2 -86.64) 1.8)
 
  ((1 2 28) (1 2 27)  (-25.32 265.56 -90.94) 1.8)
 
  ((1 2 29) (1 2 28)  (-24.54 271.18 -93.14) 1.8)
 
  ((1 2 30) (1 2 29)  ( -24.8 276.11  -93.9) 1.8)
 
  ((1 2 31) (1 2 30)  ( -24.8 279.71 -98.59) 1.8)
 
  ((1 2 32) (1 2 31)  ( -24.4 282.62 -100.5) 1.8)
 
  ((1 2 33) (1 2 32)  (-27.24 285.09 -101.64) 1.8)
 
  ((1 2 34) (1 2 33)  (-27.49 291.15 -105.4) 1.8)
 
  ((1 2 35) (1 2 34)  (-27.24 296.31 -106.21) 1.8)
 
  ((1 2 36) (1 2 35)  (-29.56 301.25 -105.38) 1.8)
 
  ((1 2 37) (1 2 36)  (-30.23 301.52 -108.37) 1.8)
 
  ((1 2 38) (1 2 37)  (-31.52 306.01 -107.87) 1.35)
 
  ((1 2 39) (1 2 38)  (-31.78 308.71 -107.87) 1.35)
 
  ((1 2 40) (1 2 39)  (-34.36 311.63 -107.87) 1.35)
 
  ((1 2 41) (1 2 40)  (-34.36 315.67 -107.87) 1.35)
 
  ((1 2 42) (1 2 41)  (-33.58  320.6 -108.39) 1.35)
 
  ((1 2 43) (1 2 42)  (-33.72 324.31 -108.39) 1.35)
 
  ((1 2 44) (1 2 43)  (-33.73 324.31 -108.39) 1.35)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 9
;; Section 3, parent index 1
 
  ((1 3 0) (BRANCH-PT 1 2 37)  (-30.23 301.52 -108.37) 1.8)
 
  ((1 3 1) (1 3 0)  (-35.73 305.96 -108.37) 1.8)
 
  ((1 3 2) (1 3 1)  (-36.76 310.93 -109.79) 1.8)
 
  ((1 3 3) (1 3 2)  (-38.05 315.19 -113.02) 1.8)
 
  ((1 3 4) (1 3 3)  (-37.28 317.21 -113.02) 1.8)
 
  ((1 3 5) (1 3 4)  (-38.05  322.6 -113.02) 1.8)
 
  ((1 3 6) (1 3 5)  (-38.12 323.12 -113.02) 1.8)
 
  ((1 3 7) (1 3 6)  (-38.89 326.96 -113.43) 1.8)
 
  ((1 3 8) (1 3 7)  (-38.37 329.88 -116.75) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 23
;; Section 4, parent index 1
 
  ((1 4 0) (BRANCH-PT 1 2 27)  (-25.83  262.2 -86.64) 1.8)
 
  ((1 4 1) (1 4 0)  (-24.43 270.14 -77.78) 2.24)
 
  ((1 4 2) (1 4 1)  (-25.21 272.84 -79.82) 2.24)
 
  ((1 4 3) (1 4 2)  (-26.24 277.33 -78.32) 1.8)
 
  ((1 4 4) (1 4 3)  (-26.77 279.97 -80.22) 1.8)
 
  ((1 4 5) (1 4 4)  (-27.55 284.01  -80.5) 1.8)
 
  ((1 4 6) (1 4 5)  (-28.58 286.03  -80.5) 1.8)
 
  ((1 4 7) (1 4 6)  (-28.84 291.87  -80.5) 1.8)
 
  ((1 4 8) (1 4 7)  (-29.61 297.93 -81.42) 1.8)
 
  ((1 4 9) (1 4 8)  (-30.43 302.86 -81.87) 1.8)
 
  ((1 4 10) (1 4 9)  (-31.21 305.33 -81.87) 1.8)
 
  ((1 4 11) (1 4 10)  (-31.98  307.8 -81.87) 1.8)
 
  ((1 4 12) (1 4 11)  (-32.75 310.05 -81.87) 1.8)
 
  ((1 4 13) (1 4 12)  (-33.01 311.39 -81.87) 1.8)
 
  ((1 4 14) (1 4 13)  (-32.75 312.29 -81.87) 1.8)
 
  ((1 4 15) (1 4 14)  (-34.04 314.54 -81.87) 1.8)
 
  ((1 4 16) (1 4 15)  (-34.82 316.56 -80.44) 1.8)
 
  ((1 4 17) (1 4 16)  (-36.72 318.72 -80.44) 1.8)
 
  ((1 4 18) (1 4 17)  (-37.49 321.41 -79.47) 1.8)
 
  ((1 4 19) (1 4 18)  (-36.98 323.66 -79.47) 1.8)
 
  ((1 4 20) (1 4 19)  (-36.98 324.56 -79.47) 1.8)
 
  ((1 4 21) (1 4 20)  (-38.01 327.25 -79.47) 1.8)
 
  ((1 4 22) (1 4 21)  (-38.01 327.25 -77.56) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 61
;; Section 5, parent index 0
 
  ((1 5 0) (BRANCH-PT 1 1 7)  ( -2.63  42.56 -11.06) 1.8)
 
  ((1 5 1) (1 5 0)  ( -4.26  48.56  -5.36) 1.8)
 
  ((1 5 2) (1 5 1)  ( -4.92  54.77  -5.36) 1.8)
 
  ((1 5 3) (1 5 2)  ( -5.95  59.49  -5.36) 1.8)
 
  ((1 5 4) (1 5 3)  ( -6.73  63.98  -5.36) 1.8)
 
  ((1 5 5) (1 5 4)  ( -7.69  70.16  -2.91) 1.8)
 
  ((1 5 6) (1 5 5)  ( -7.69  70.83  -2.91) 1.8)
 
  ((1 5 7) (1 5 6)  ( -5.11  78.92  -4.18) 1.8)
 
  ((1 5 8) (1 5 7)  ( -4.08     87  -6.65) 1.8)
 
  ((1 5 9) (1 5 8)  ( -3.34  90.47  -6.65) 1.8)
 
  ((1 5 10) (1 5 9)  ( -1.79  93.61  -7.26) 1.8)
 
  ((1 5 11) (1 5 10)  (  0.53  99.89   -7.4) 1.8)
 
  ((1 5 12) (1 5 11)  (  0.53 103.04   -9.3) 1.8)
 
  ((1 5 13) (1 5 12)  (  0.78 109.32 -10.41) 1.8)
 
  ((1 5 14) (1 5 13)  (  2.75 116.01 -10.24) 1.8)
 
  ((1 5 15) (1 5 14)  (  3.78 122.29 -12.23) 1.8)
 
  ((1 5 16) (1 5 15)  (  4.81 129.48 -12.23) 1.8)
 
  ((1 5 17) (1 5 16)  (     5 134.31 -13.79) 1.8)
 
  ((1 5 18) (1 5 17)  (     5 134.54 -13.79) 1.8)
 
  ((1 5 19) (1 5 18)  (  1.65  141.5 -17.86) 1.8)
 
  ((1 5 20) (1 5 19)  (  0.62 147.56 -17.86) 1.8)
 
  ((1 5 21) (1 5 20)  ( -1.71  153.4 -18.06) 1.8)
 
  ((1 5 22) (1 5 21)  ( -4.24 159.76 -22.67) 1.8)
 
  ((1 5 23) (1 5 22)  ( -4.75 163.15 -22.67) 1.8)
 
  ((1 5 24) (1 5 23)  ( -4.92 166.64 -21.07) 1.8)
 
  ((1 5 25) (1 5 24)  ( -6.21 171.13 -22.82) 1.8)
 
  ((1 5 26) (1 5 25)  ( -6.98 173.83 -22.82) 1.8)
 
  ((1 5 27) (1 5 26)  ( -7.49 179.44 -27.44) 1.8)
 
  ((1 5 28) (1 5 27)  ( -8.27 182.13 -27.44) 1.8)
 
  ((1 5 29) (1 5 28)  ( -9.61 185.44 -26.76) 1.8)
 
  ((1 5 30) (1 5 29)  ( -9.72 186.62 -29.06) 1.8)
 
  ((1 5 31) (1 5 30)  (-10.23 189.99 -29.06) 1.8)
 
  ((1 5 32) (1 5 31)  (-11.78  195.6 -29.12) 1.8)
 
  ((1 5 33) (1 5 32)  (-12.55 200.54 -29.12) 1.8)
 
  ((1 5 34) (1 5 33)  (   -12  207.4 -31.46) 1.8)
 
  ((1 5 35) (1 5 34)  (-12.26 213.46 -37.34) 1.8)
 
  ((1 5 36) (1 5 35)  (-12.52 217.05 -37.34) 1.8)
 
  ((1 5 37) (1 5 36)  (-14.06 223.34 -37.34) 1.8)
 
  ((1 5 38) (1 5 37)  (-14.58 229.18  -37.7) 1.8)
 
  ((1 5 39) (1 5 38)  (-15.43 235.14 -41.82) 1.8)
 
  ((1 5 40) (1 5 39)  (-16.46 240.97 -41.95) 1.8)
 
  ((1 5 41) (1 5 40)  (-17.24 247.48 -41.59) 1.8)
 
  ((1 5 42) (1 5 41)  (-17.32 255.13 -45.11) 1.8)
 
  ((1 5 43) (1 5 42)  (-17.57 261.64 -45.11) 1.8)
 
  ((1 5 44) (1 5 43)  (-18.09 266.36 -45.31) 1.8)
 
  ((1 5 45) (1 5 44)  (-19.92 275.23 -45.33) 1.8)
 
  ((1 5 46) (1 5 45)  (-22.24 279.74 -49.87) 1.8)
 
  ((1 5 47) (1 5 46)  (-24.31 286.93 -49.87) 1.8)
 
  ((1 5 48) (1 5 47)  (-26.26 292.93 -49.87) 1.8)
 
  ((1 5 49) (1 5 48)  (-27.55 296.98 -49.87) 1.8)
 
  ((1 5 50) (1 5 49)  (-28.58 301.47 -49.87) 1.8)
 
  ((1 5 51) (1 5 50)  (-29.36 310.22 -50.88) 1.8)
 
  ((1 5 52) (1 5 51)  (   -29 313.02 -52.75) 1.8)
 
  ((1 5 53) (1 5 52)  (-30.81 316.38 -52.75) 1.8)
 
  ((1 5 54) (1 5 53)  (-32.61 317.51 -52.75) 1.8)
 
  ((1 5 55) (1 5 54)  (-36.48 318.85 -53.45) 1.8)
 
  ((1 5 56) (1 5 55)  (-39.57 320.65 -53.45) 1.8)
 
  ((1 5 57) (1 5 56)  (-42.15 321.32 -53.45) 1.8)
 
  ((1 5 58) (1 5 57)  (-44.22 322.89 -53.45) 1.8)
 
  ((1 5 59) (1 5 58)  (-46.28 324.69 -53.45) 1.8)
 
  ((1 5 60) (1 5 59)  (-46.28 324.69 -55.18) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 31
;; Section 6, parent index 4
 
  ((1 6 0) (BRANCH-PT 1 5 18)  (     5 134.54 -13.79) 1.8)
 
  ((1 6 1) (1 6 0)  (  7.46 147.05  -15.7) 1.8)
 
  ((1 6 2) (1 6 1)  (  7.46 152.88 -16.36) 1.8)
 
  ((1 6 3) (1 6 2)  (  8.32 159.41 -16.36) 1.8)
 
  ((1 6 4) (1 6 3)  (  8.32 165.47 -17.27) 1.8)
 
  ((1 6 5) (1 6 4)  (  9.61 173.78 -17.27) 1.8)
 
  ((1 6 6) (1 6 5)  ( 11.03 180.86 -18.89) 1.8)
 
  ((1 6 7) (1 6 6)  ( 14.64 188.94    -20) 1.8)
 
  ((1 6 8) (1 6 7)  ( 16.96  195.9 -19.82) 1.8)
 
  ((1 6 9) (1 6 8)  ( 22.56 209.59 -22.51) 1.8)
 
  ((1 6 10) (1 6 9)  ( 23.33 213.41 -22.51) 1.8)
 
  ((1 6 11) (1 6 10)  ( 26.64 220.43 -23.78) 1.8)
 
  ((1 6 12) (1 6 11)  ( 28.96 225.37 -23.78) 1.8)
 
  ((1 6 13) (1 6 12)  ( 29.73 233.01 -22.24) 1.8)
 
  ((1 6 14) (1 6 13)  (  35.1 241.85 -25.79) 1.8)
 
  ((1 6 15) (1 6 14)  ( 38.19 251.51 -30.15) 1.8)
 
  ((1 6 16) (1 6 15)  ( 40.66 257.76 -30.15) 1.8)
 
  ((1 6 17) (1 6 16)  ( 43.24 266.29 -30.15) 1.8)
 
  ((1 6 18) (1 6 17)  ( 44.53 271.45 -30.15) 1.8)
 
  ((1 6 19) (1 6 18)  ( 46.29 277.58 -30.15) 1.8)
 
  ((1 6 20) (1 6 19)  ( 47.58 282.52 -30.15) 1.8)
 
  ((1 6 21) (1 6 20)  (  49.9  290.6 -30.15) 1.8)
 
  ((1 6 22) (1 6 21)  ( 52.21 297.91 -30.15) 1.8)
 
  ((1 6 23) (1 6 22)  ( 53.75 304.42 -30.29) 1.8)
 
  ((1 6 24) (1 6 23)  ( 54.78 308.23 -31.91) 1.8)
 
  ((1 6 25) (1 6 24)  ( 58.55 316.83  -32.3) 1.8)
 
  ((1 6 26) (1 6 25)  ( 59.07 320.42  -32.3) 1.8)
 
  ((1 6 27) (1 6 26)  ( 60.87 324.69  -32.3) 1.8)
 
  ((1 6 28) (1 6 27)  ( 61.13 330.52 -30.73) 1.8)
 
  ((1 6 29) (1 6 28)  ( 60.83 339.03 -28.15) 1.8)
 
  ((1 6 30) (1 6 29)  ( 60.31 340.38 -28.15) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 44
;; Section 7, parent index 4
 
  ((1 7 0) (BRANCH-PT 1 5 6)  ( -7.69  70.83  -2.91) 1.8)
 
  ((1 7 1) (1 7 0)  (-10.04  77.88  -1.36) 1.8)
 
  ((1 7 2) (1 7 1)  (-10.81  82.15  -1.36) 1.8)
 
  ((1 7 3) (1 7 2)  (-12.36  86.19  -1.36) 1.8)
 
  ((1 7 4) (1 7 3)  (-15.19   91.8  -1.36) 1.8)
 
  ((1 7 5) (1 7 4)  (-16.61  96.39  -1.36) 1.8)
 
  ((1 7 6) (1 7 5)  (-18.15 100.88  -1.36) 1.8)
 
  ((1 7 7) (1 7 6)  (-18.67 108.06  -0.55) 1.8)
 
  ((1 7 8) (1 7 7)  (-19.18 113.67  -0.55) 1.8)
 
  ((1 7 9) (1 7 8)  (-18.03 119.96  -0.55) 1.8)
 
  ((1 7 10) (1 7 9)  (-18.28 124.67  -0.55) 1.8)
 
  ((1 7 11) (1 7 10)  (-19.32 132.76  -0.55) 1.8)
 
  ((1 7 12) (1 7 11)  (-20.81 142.33  -0.17) 1.8)
 
  ((1 7 13) (1 7 12)  (-21.84 151.08   1.02) 1.8)
 
  ((1 7 14) (1 7 13)  (-23.21 157.41   2.95) 1.8)
 
  ((1 7 15) (1 7 14)  (-23.72  162.8   2.95) 1.8)
 
  ((1 7 16) (1 7 15)  (-24.75 169.08   5.86) 1.8)
 
  ((1 7 17) (1 7 16)  (-25.43 174.71   5.86) 1.8)
 
  ((1 7 18) (1 7 17)  (-21.31  179.2   4.62) 1.8)
 
  ((1 7 19) (1 7 18)  (-20.79 185.04   7.41) 1.8)
 
  ((1 7 20) (1 7 19)  (-20.27 188.18   7.41) 1.8)
 
  ((1 7 21) (1 7 20)  (-19.03 191.67   7.41) 1.8)
 
  ((1 7 22) (1 7 21)  (-18.51 195.48   7.41) 1.8)
 
  ((1 7 23) (1 7 22)  (-17.22 201.09   7.41) 1.8)
 
  ((1 7 24) (1 7 23)  (-16.45 206.93   9.46) 1.8)
 
  ((1 7 25) (1 7 24)  (-17.03 213.16   9.46) 1.8)
 
  ((1 7 26) (1 7 25)  (-16.52 221.47  12.46) 1.8)
 
  ((1 7 27) (1 7 26)  (-15.51 225.58  12.46) 1.8)
 
  ((1 7 28) (1 7 27)  (-14.99 232.57   13.5) 1.8)
 
  ((1 7 29) (1 7 28)  (-14.74 233.47  10.92) 1.8)
 
  ((1 7 30) (1 7 29)  (-18.61 237.51  10.92) 1.8)
 
  ((1 7 31) (1 7 30)  (-20.26 239.27  10.92) 1.8)
 
  ((1 7 32) (1 7 31)  (-21.81 241.74  10.92) 1.8)
 
  ((1 7 33) (1 7 32)  (-26.19  247.8  10.92) 1.8)
 
  ((1 7 34) (1 7 33)  (-29.29 252.96  12.14) 1.8)
 
  ((1 7 35) (1 7 34)  ( -32.9 259.03   8.33) 1.8)
 
  ((1 7 36) (1 7 35)  (-37.07 263.56   8.99) 1.8)
 
  ((1 7 37) (1 7 36)  (-41.72 271.42   8.99) 1.8)
 
  ((1 7 38) (1 7 37)  (-44.04 275.46   8.99) 1.8)
 
  ((1 7 39) (1 7 38)  (-47.98 284.25   2.46) 1.8)
 
  ((1 7 40) (1 7 39)  (-49.79 290.09   2.46) 1.8)
 
  ((1 7 41) (1 7 40)  (-51.34 294.36   2.46) 1.8)
 
  ((1 7 42) (1 7 41)  (-51.52 300.56    3.6) 1.8)
 
  ((1 7 43) (1 7 42)  (-51.52 300.56   1.39) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 17
;; Section 8, parent index 6
 
  ((1 8 0) (BRANCH-PT 1 7 29)  (-14.74 233.47  10.92) 1.8)
 
  ((1 8 1) (1 8 0)  (-13.11 241.36  15.24) 2.24)
 
  ((1 8 2) (1 8 1)  (-11.82 246.53  15.24) 1.8)
 
  ((1 8 3) (1 8 2)  (-11.82 251.24  15.24) 1.8)
 
  ((1 8 4) (1 8 3)  (-12.07  257.3  15.24) 1.8)
 
  ((1 8 5) (1 8 4)  (-13.41 262.44  15.24) 1.8)
 
  ((1 8 6) (1 8 5)  (-13.66 267.15  15.24) 1.8)
 
  ((1 8 7) (1 8 6)  (-13.66 276.36  15.24) 1.8)
 
  ((1 8 8) (1 8 7)  (-16.55 286.15  15.24) 1.8)
 
  ((1 8 9) (1 8 8)  (-20.16  298.5   17.5) 1.8)
 
  ((1 8 10) (1 8 9)  (-21.86 307.93  18.58) 1.8)
 
  ((1 8 11) (1 8 10)  (-23.41 314.89  18.58) 1.8)
 
  ((1 8 12) (1 8 11)  (-23.93  318.7  18.58) 1.8)
 
  ((1 8 13) (1 8 12)  (-26.95 327.68  18.58) 1.8)
 
  ((1 8 14) (1 8 13)  (-27.47 329.25  18.58) 1.8)
 
  ((1 8 15) (1 8 14)  (-27.48 329.25  18.58) 1.8)
 
  ((1 8 16) (1 8 15)  (-27.49 329.25  18.58) 1.8)
;;; End of section ********* 
;;; End of neurite ********* 

 ;;  Number of segs - 1, Number of xyzd points - 62
;; Section 1, parent index -1
 
  ((2 1 0)  SOMA  (  -8.6  12.42   8.32) 2.24)
 
  ((2 1 1) (2 1 0)  (-12.99  16.69   8.32) 2.24)
 
  ((2 1 2) (2 1 1)  (-14.79  18.48   9.34) 2.24)
 
  ((2 1 3) (2 1 2)  (-16.34  19.83   9.34) 2.24)
 
  ((2 1 4) (2 1 3)  (-19.69  23.87  18.22) 1.8)
 
  ((2 1 5) (2 1 4)  (-22.27  26.79  19.67) 1.8)
 
  ((2 1 6) (2 1 5)  (-25.11  29.26  22.79) 1.8)
 
  ((2 1 7) (2 1 6)  (-25.63  30.16  25.19) 1.8)
 
  ((2 1 8) (2 1 7)  (-27.69  32.18  29.32) 1.8)
 
  ((2 1 9) (2 1 8)  (-28.46  33.97  29.32) 1.8)
 
  ((2 1 10) (2 1 9)  (-27.95  35.77  33.82) 1.8)
 
  ((2 1 11) (2 1 10)  (-30.53  40.93  37.94) 1.8)
 
  ((2 1 12) (2 1 11)  (-35.36  47.18  43.14) 1.8)
 
  ((2 1 13) (2 1 12)  (-38.71  53.69   47.2) 1.8)
 
  ((2 1 14) (2 1 13)  (-39.23  57.74   47.2) 1.8)
 
  ((2 1 15) (2 1 14)  (-41.81  62.67  50.85) 1.8)
 
  ((2 1 16) (2 1 15)  (-43.21  64.72  50.85) 1.8)
 
  ((2 1 17) (2 1 16)  (-43.98  66.74  50.85) 1.8)
 
  ((2 1 18) (2 1 17)  (-43.99  66.74  50.85) 1.8)
 
  ((2 1 19) (2 1 18)  (-42.95   74.6  51.01) 1.8)
 
  ((2 1 20) (2 1 19)  (-41.31  84.33  51.01) 1.8)
 
  ((2 1 21) (2 1 20)  (-40.28  90.39  51.01) 1.8)
 
  ((2 1 22) (2 1 21)  (-40.28   95.1  51.01) 1.8)
 
  ((2 1 23) (2 1 22)  (-40.57 101.14  51.01) 1.8)
 
  ((2 1 24) (2 1 23)  (-43.15  107.2  51.26) 1.8)
 
  ((2 1 25) (2 1 24)  (-46.25 117.97  53.27) 1.8)
 
  ((2 1 26) (2 1 25)  (-48.06 122.84  53.94) 1.8)
 
  ((2 1 27) (2 1 26)  (-50.13 131.14  53.94) 1.8)
 
  ((2 1 28) (2 1 27)  (-51.93 137.43  53.94) 1.8)
 
  ((2 1 29) (2 1 28)  (-54.95 142.44  53.94) 1.8)
 
  ((2 1 30) (2 1 29)  (-55.98 146.93  53.94) 1.8)
 
  ((2 1 31) (2 1 30)  ( -56.5 147.83  54.07) 1.8)
 
  ((2 1 32) (2 1 31)  (-57.79 151.87  56.98) 1.8)
 
  ((2 1 33) (2 1 32)  (-58.31 156.58  56.02) 1.8)
 
  ((2 1 34) (2 1 33)  (-58.72 162.09  56.02) 1.8)
 
  ((2 1 35) (2 1 34)  (-59.49 166.58  56.02) 1.8)
 
  ((2 1 36) (2 1 35)  (-60.52 171.51  56.02) 1.8)
 
  ((2 1 37) (2 1 36)  (-60.72    179  57.24) 1.8)
 
  ((2 1 38) (2 1 37)  (-60.72 183.04  58.19) 1.8)
 
  ((2 1 39) (2 1 38)  (-60.35 190.29  61.53) 1.8)
 
  ((2 1 40) (2 1 39)  (-59.83 194.56  61.53) 1.8)
 
  ((2 1 41) (2 1 40)  (-58.37  206.8  61.63) 1.8)
 
  ((2 1 42) (2 1 41)  (-57.34 215.33   63.7) 1.8)
 
  ((2 1 43) (2 1 42)  (   -57 227.17  65.97) 1.8)
 
  ((2 1 44) (2 1 43)  (-58.03 236.38   67.5) 1.8)
 
  ((2 1 45) (2 1 44)  (-59.15 243.68   67.5) 1.8)
 
  ((2 1 46) (2 1 45)  (-58.63 247.27   67.5) 1.8)
 
  ((2 1 47) (2 1 46)  (-57.34 251.32   67.5) 1.8)
 
  ((2 1 48) (2 1 47)  (-56.05 255.13   67.5) 1.8)
 
  ((2 1 49) (2 1 48)  (-57.41 265.63  68.22) 1.8)
 
  ((2 1 50) (2 1 49)  (-59.98 272.58  68.22) 1.8)
 
  ((2 1 51) (2 1 50)  (-62.18 289.76  68.22) 1.8)
 
  ((2 1 52) (2 1 51)  (-62.69 296.94  68.22) 1.8)
 
  ((2 1 53) (2 1 52)  (-63.23 301.36  68.22) 1.8)
 
  ((2 1 54) (2 1 53)  (-63.75 305.63  68.22) 1.8)
 
  ((2 1 55) (2 1 54)  (-63.23 317.75  68.22) 1.8)
 
  ((2 1 56) (2 1 55)  (-63.57 321.07  68.39) 1.8)
 
  ((2 1 57) (2 1 56)  (-67.18 325.78  69.94) 1.8)
 
  ((2 1 58) (2 1 57)  (-68.99 331.39   70.8) 1.8)
 
  ((2 1 59) (2 1 58)  (-70.28 334.98   71.6) 1.8)
 
  ((2 1 60) (2 1 59)  (-72.89 337.32  71.18) 1.8)
 
  ((2 1 61) (2 1 60)  (-73.15 337.32  71.18) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 31
;; Section 2, parent index 0
 
  ((2 2 0) (BRANCH-PT 2 1 31)  ( -56.5 147.83  54.07) 1.8)
 
  ((2 2 1) (2 2 0)  (-55.26 152.29  54.07) 1.8)
 
  ((2 2 2) (2 2 1)  (-56.03 163.74  54.07) 1.8)
 
  ((2 2 3) (2 2 2)  (-57.06 172.94  54.46) 1.8)
 
  ((2 2 4) (2 2 3)  (-58.92  183.2  54.46) 1.8)
 
  ((2 2 5) (2 2 4)  (-59.95 191.28  54.46) 1.8)
 
  ((2 2 6) (2 2 5)  (-60.37 201.43  53.35) 1.8)
 
  ((2 2 7) (2 2 6)  (-60.37 209.51  53.35) 1.8)
 
  ((2 2 8) (2 2 7)  (-60.26 221.37  52.74) 1.8)
 
  ((2 2 9) (2 2 8)  (-60.26 227.66  52.74) 1.8)
 
  ((2 2 10) (2 2 9)  (-59.54 233.06  52.74) 1.8)
 
  ((2 2 11) (2 2 10)  (-57.99 241.37  52.74) 1.8)
 
  ((2 2 12) (2 2 11)  (-56.71 251.25  48.83) 1.8)
 
  ((2 2 13) (2 2 12)  (-54.85  260.5  47.55) 1.8)
 
  ((2 2 14) (2 2 13)  (-54.59 267.01  47.49) 1.8)
 
  ((2 2 15) (2 2 14)  (-55.72 273.27  45.97) 1.8)
 
  ((2 2 16) (2 2 15)  (-57.26 277.31  45.97) 1.8)
 
  ((2 2 17) (2 2 16)  (-58.04 281.57  43.94) 1.8)
 
  ((2 2 18) (2 2 17)  (-58.81 287.63  43.94) 1.8)
 
  ((2 2 19) (2 2 18)  (-58.66 295.33  40.88) 1.8)
 
  ((2 2 20) (2 2 19)  ( -58.4 301.17  42.52) 1.8)
 
  ((2 2 21) (2 2 20)  (-58.66  308.8  42.52) 1.8)
 
  ((2 2 22) (2 2 21)  (-58.46 316.98  36.58) 1.8)
 
  ((2 2 23) (2 2 22)  (-58.72 324.61  29.73) 1.8)
 
  ((2 2 24) (2 2 23)  (-57.68  329.1  29.73) 1.8)
 
  ((2 2 25) (2 2 24)  (-57.46 332.22  29.73) 1.8)
 
  ((2 2 26) (2 2 25)  (-57.97 336.93  29.73) 1.8)
 
  ((2 2 27) (2 2 26)  (-59.26  341.2  29.73) 1.8)
 
  ((2 2 28) (2 2 27)  (-60.55 342.77  28.15) 1.8)
 
  ((2 2 29) (2 2 28)  ( -62.1 345.24  25.71) 1.8)
 
  ((2 2 30) (2 2 29)  (-62.11 345.24  25.71) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 34
;; Section 3, parent index 0
 
  ((2 3 0) (BRANCH-PT 2 1 18)  (-43.99  66.74  50.85) 1.8)
 
  ((2 3 1) (2 3 0)  (-49.53  71.84  52.93) 2.24)
 
  ((2 3 2) (2 3 1)  (-54.17  77.67  58.75) 2.24)
 
  ((2 3 3) (2 3 2)  (-57.53  82.61  62.97) 2.24)
 
  ((2 3 4) (2 3 3)  (-59.33  85.76  62.97) 2.24)
 
  ((2 3 5) (2 3 4)  (-66.22  92.83  67.38) 2.24)
 
  ((2 3 6) (2 3 5)  (-70.61  98.45  67.98) 2.24)
 
  ((2 3 7) (2 3 6)  (-74.99 104.73  67.88) 2.24)
 
  ((2 3 8) (2 3 7)  (-80.79 111.61  67.88) 2.24)
 
  ((2 3 9) (2 3 8)  (-86.09 124.81  69.45) 2.24)
 
  ((2 3 10) (2 3 9)  (-94.61 138.06  69.45) 1.8)
 
  ((2 3 11) (2 3 10)  (-105.81 154.71  74.08) 1.8)
 
  ((2 3 12) (2 3 11)  (-108.64 162.12  78.18) 1.8)
 
  ((2 3 13) (2 3 12)  (-114.65 178.24  78.18) 1.8)
 
  ((2 3 14) (2 3 13)  (-115.94 181.61  78.18) 1.8)
 
  ((2 3 15) (2 3 14)  (-121.27 194.27  79.12) 1.8)
 
  ((2 3 16) (2 3 15)  (-125.4  202.8  80.96) 1.8)
 
  ((2 3 17) (2 3 16)  (-137.64 227.14  85.53) 1.8)
 
  ((2 3 18) (2 3 17)  (-141.33 233.81  85.53) 1.8)
 
  ((2 3 19) (2 3 18)  (-143.39 243.91  85.53) 1.8)
 
  ((2 3 20) (2 3 19)  (-146.23 256.48  85.53) 1.8)
 
  ((2 3 21) (2 3 20)  (-150.18 266.38  84.86) 1.8)
 
  ((2 3 22) (2 3 21)  (-151.73 270.19  84.86) 1.8)
 
  ((2 3 23) (2 3 22)  (-152.76 279.85  82.77) 1.8)
 
  ((2 3 24) (2 3 23)  (-154.72 285.98  82.77) 1.8)
 
  ((2 3 25) (2 3 24)  (-157.56 289.13  82.77) 1.8)
 
  ((2 3 26) (2 3 25)  (-158.07 292.49  82.77) 1.8)
 
  ((2 3 27) (2 3 26)  (-160.91 294.96  82.77) 1.8)
 
  ((2 3 28) (2 3 27)  (-163.49 304.84  79.96) 1.8)
 
  ((2 3 29) (2 3 28)  (-164.29 310.71  79.54) 1.8)
 
  ((2 3 30) (2 3 29)  (-164.29 319.02  79.54) 1.8)
 
  ((2 3 31) (2 3 30)  (-161.97 325.75  79.54) 1.8)
 
  ((2 3 32) (2 3 31)  (-159.79 328.79  81.29) 1.8)
 
  ((2 3 33) (2 3 32)  (-159.8 328.79  81.29) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 54
;; Section 4, parent index 0
 
  ((2 4 0) (BRANCH-PT 2 1 3)  (-16.34  19.83   9.34) 2.24)
 
  ((2 4 1) (2 4 0)  (-21.42  25.76   14.1) 2.24)
 
  ((2 4 2) (2 4 1)  (-23.48  28.45   14.1) 2.24)
 
  ((2 4 3) (2 4 2)  ( -25.8  30.92   14.1) 2.24)
 
  ((2 4 4) (2 4 3)  (-27.09  34.51   14.1) 1.8)
 
  ((2 4 5) (2 4 4)  (-28.64  38.11   14.1) 1.8)
 
  ((2 4 6) (2 4 5)  (-31.04  46.83   18.3) 1.8)
 
  ((2 4 7) (2 4 6)  (-32.63  52.58  17.76) 1.8)
 
  ((2 4 8) (2 4 7)  (-33.92  58.87  20.09) 1.8)
 
  ((2 4 9) (2 4 8)  (-35.73  65.85  20.09) 1.8)
 
  ((2 4 10) (2 4 9)  (-37.72  68.79  20.09) 1.8)
 
  ((2 4 11) (2 4 10)  (-41.33  72.38  20.09) 1.8)
 
  ((2 4 12) (2 4 11)  (-44.94  75.75  20.09) 1.8)
 
  ((2 4 13) (2 4 12)  (-49.84  79.34  20.09) 1.8)
 
  ((2 4 14) (2 4 13)  (-53.71  82.04  20.09) 1.8)
 
  ((2 4 15) (2 4 14)  (-59.13  87.65  17.02) 1.8)
 
  ((2 4 16) (2 4 15)  (-63.18  90.17  17.02) 1.8)
 
  ((2 4 17) (2 4 16)  (-66.79  93.76  17.02) 1.8)
 
  ((2 4 18) (2 4 17)  (-70.92  98.47  17.02) 1.8)
 
  ((2 4 19) (2 4 18)  (-74.02 102.07  17.02) 1.8)
 
  ((2 4 20) (2 4 19)  ( -78.4 108.58  17.02) 1.8)
 
  ((2 4 21) (2 4 20)  (-79.34 107.76  17.02) 1.8)
 
  ((2 4 22) (2 4 21)  (-82.95 114.27  17.02) 1.8)
 
  ((2 4 23) (2 4 22)  (-87.59 121.01  17.02) 1.8)
 
  ((2 4 24) (2 4 23)  (-91.46 126.85  17.02) 1.8)
 
  ((2 4 25) (2 4 24)  (-94.81 132.01  17.02) 1.8)
 
  ((2 4 26) (2 4 25)  (-101.56 139.06  17.02) 1.8)
 
  ((2 4 27) (2 4 26)  (-104.66 146.24  14.96) 1.8)
 
  ((2 4 28) (2 4 27)  (-105.17 148.04  14.96) 1.8)
 
  ((2 4 29) (2 4 28)  (-107.24 150.73  17.33) 1.8)
 
  ((2 4 30) (2 4 29)  (-107.06 150.92  14.19) 1.8)
 
  ((2 4 31) (2 4 30)  (-108.09 157.66  17.22) 1.8)
 
  ((2 4 32) (2 4 31)  (-110.41 165.74  17.22) 1.8)
 
  ((2 4 33) (2 4 32)  (-113.25 172.92  21.29) 1.8)
 
  ((2 4 34) (2 4 33)  (-117.18 179.38  21.29) 1.8)
 
  ((2 4 35) (2 4 34)  (-119.76 184.55  21.29) 1.8)
 
  ((2 4 36) (2 4 35)  (-123.63 190.61  23.99) 1.8)
 
  ((2 4 37) (2 4 36)  (-125.78 197.88  23.99) 1.8)
 
  ((2 4 38) (2 4 37)  (-127.84 202.37  23.99) 1.8)
 
  ((2 4 39) (2 4 38)  (-129.39 206.89  23.99) 1.8)
 
  ((2 4 40) (2 4 39)  (-130.94 209.58  23.99) 1.8)
 
  ((2 4 41) (2 4 40)  (-133.18 213.07  23.99) 1.8)
 
  ((2 4 42) (2 4 41)  (-134.21 217.33  23.99) 1.8)
 
  ((2 4 43) (2 4 42)  (-134.47 221.82  23.99) 1.8)
 
  ((2 4 44) (2 4 43)  (-135.5 229.45  25.62) 1.8)
 
  ((2 4 45) (2 4 44)  (-138.55 237.13  24.45) 1.8)
 
  ((2 4 46) (2 4 45)  (-141.13 242.52  27.07) 1.8)
 
  ((2 4 47) (2 4 46)  (-144.74  249.7  31.63) 1.8)
 
  ((2 4 48) (2 4 47)  (-146.21 253.27  31.63) 1.8)
 
  ((2 4 49) (2 4 48)  (-147.49 257.98  31.63) 1.8)
 
  ((2 4 50) (2 4 49)  (-148.53 261.35  31.63) 1.8)
 
  ((2 4 51) (2 4 50)  (-149.82 265.39  31.63) 1.8)
 
  ((2 4 52) (2 4 51)  (-152.23 267.53  31.63) 1.8)
 
  ((2 4 53) (2 4 52)  (-152.75  268.2  31.63) 1.8)
;;; End of section ********* 

 ;;  Number of segs - 1, Number of xyzd points - 27
;; Section 5, parent index 3
 
  ((2 5 0) (BRANCH-PT 2 4 30)  (-107.06 150.92  14.19) 1.8)
 
  ((2 5 1) (2 5 0)  (-109.75 154.49  14.19) 1.8)
 
  ((2 5 2) (2 5 1)  (-112.33 158.76  14.19) 1.8)
 
  ((2 5 3) (2 5 2)  (-114.39  162.8  14.19) 1.8)
 
  ((2 5 4) (2 5 3)  (-119.29 168.41  11.51) 1.8)
 
  ((2 5 5) (2 5 4)  (-121.89 170.97  11.54) 1.8)
 
  ((2 5 6) (2 5 5)  (-123.7 176.36  11.54) 1.8)
 
  ((2 5 7) (2 5 6)  (-126.54  178.6  11.54) 1.8)
 
  ((2 5 8) (2 5 7)  (-129.89 183.54  11.54) 1.8)
 
  ((2 5 9) (2 5 8)  (-133.92 187.68  11.21) 1.8)
 
  ((2 5 10) (2 5 9)  (-138.56 193.07  11.21) 1.8)
 
  ((2 5 11) (2 5 10)  (-144.75 198.23   8.98) 1.8)
 
  ((2 5 12) (2 5 11)  (-148.62  201.6   5.47) 1.8)
 
  ((2 5 13) (2 5 12)  (-151.2 204.51   5.47) 1.8)
 
  ((2 5 14) (2 5 13)  (-154.81 208.78   5.47) 1.8)
 
  ((2 5 15) (2 5 14)  (-160.71 213.01   1.67) 1.8)
 
  ((2 5 16) (2 5 15)  (-166.12 218.85   1.67) 1.8)
 
  ((2 5 17) (2 5 16)  (-169.47 222.44   1.67) 1.8)
 
  ((2 5 18) (2 5 17)  (-175.41 229.63  -3.92) 1.8)
 
  ((2 5 19) (2 5 18)  (-180.42 235.98  -3.92) 1.8)
 
  ((2 5 20) (2 5 19)  (-187.12 241.82  -3.92) 1.8)
 
  ((2 5 21) (2 5 20)  (-190.22 245.19  -3.92) 1.8)
 
  ((2 5 22) (2 5 21)  (-192.8 248.56  -3.92) 1.8)
 
  ((2 5 23) (2 5 22)  (-196.98 253.41  -3.92) 1.8)
 
  ((2 5 24) (2 5 23)  (-200.34 257.23  -3.92) 1.8)
 
  ((2 5 25) (2 5 24)  (-203.17 263.52  -3.92) 1.8)
 
  ((2 5 26) (2 5 25)  (-203.69 265.31  -5.45) 1.8)
;;; End of section ********* 
;;; End of neurite ********* 

))


(setq *process-ntscable-list* t)
(setq *nts-cell-type* "ntscable")
(setq *nts-cell-name* "ntscable")
(setq *nts-r-mem* 40000.0)
(setq *nts-soma-r-mem* 40000.0)
(setq *nts-r-a* 200.0)

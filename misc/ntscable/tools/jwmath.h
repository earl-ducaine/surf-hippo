	/*-------------------------------------------------------------
	|
	|	MODULE:	  %M%
	|
	|	MACHINE:  UNIX
	|
	|	STARTED:  10-OCT-90	   BY:	J.C. Wathey
	|
	|	REVISED:  %G%	   BY:	JCW
	|
	|	STATUS:	     incomplete or untested		
	|		     compiles; partly tested
	|		  -> runs; revisions in progress
	|		     runs; stable version
	|
	|	CONTAINS: handy math macros
	|
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/


#include <math.h>
#define ftrunc		aint
#define trunc		aint

#define D_RND_ERROR	2.0e-15	           /* double round-off error */

#define odd(x)		((x) % 2)
#define	frac(x)		((x)-trunc(x))
#define	ffrac(x)	((x)-ftrunc(x))
#define	irint(x)	((int) rint((double)(x)))
#define	exp10(x)	pow(10.0, x)
#define max(x,y)	(((x)>(y))?(x):(y))
#define min(x,y)	(((x)<(y))?(x):(y))
#define fequal(x,y)     (fabs((x)-(y)) < fabs((x)+(y))*D_RND_ERROR)

	/*-------------------------------------------------------------
	|							      |
	|	MODULE:   %M%                                    |
	|							      |
	|	MACHINE:  UNIX                                        |
	|							      |
	|	STARTED:  12-APR-88  BY:  J.C. Wathey                 |
	|							      |
	|	REVISED:  %G%  BY:  JCW                         |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: global definitions for getnum.c             |
	|                                                             |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/

#define	INCLUDE_MIN	1	            /* range test masks; see */
#define INCLUDE_MAX	2	            /* d_out_of_range, etc.  */
#define	INCLUDE_BOTH	3
#define	EXCLUDE_BOTH	0

#define PI		3.1415926535897932

float  get_float();
double get_double();
double get_double_in_range();

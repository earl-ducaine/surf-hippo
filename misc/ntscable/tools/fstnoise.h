	/*-------------------------------------------------------------
	|
	|	PROGRAM:
	|
	|	MODULE:   %W%
	|
	|	RELATED
	|	MODULES:  fstnoise.c
	|
	|	MACHINE:  Any unix machine with 32-bit word size
	|
	|	STARTED:  06-SEP-89        BY:  J.C. Wathey
	|
	|	REVISED:  %G%         BY:  JCW
	|
	|	STATUS:      incomplete or untested
	|                    compiles; partly tested
	|                    runs; revisions in progress
	|                 -> runs; stable version
	|
	|       CONTAINS: public #defines related to fstnoise.c,
	|                 and declarations of functions which do not
	|                 return int.
	|
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

						    /* integer sizes */
#define Int4		long
#define Int2		short
#define Int1		char

#define Int4u 		unsigned long
#define Int2u		unsigned short
#define Int1u		unsigned char


        /* The noise values will vary between +/- MAX_DA. */

#define MAX_DA 			2047

	/* MAX_WIDTH_IN_SIGMAS is the maximum width bell curve which
	can be represented in the Gaussian look-up table.  It depends
	upon MAX_DA and MAX_RAND in a complex way and, as far as I
	know, cannot be solved for analytically.  The value 4.1608
	was determined empirically (with a binary search) to be
	appropriate for MAX_DA = 2047 and MAX_RAND = 1771874. */

#define MAX_WIDTH_IN_SIGMAS	4.1608

void init_random_sequence();

Int4 max_seed();

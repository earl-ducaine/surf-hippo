	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:                                              |
	|							      |
	|	MODULE:   .h                                          |
	|							      |
	|	RELATED						      |
	|	MODULES:  .c                                          |
	|							      |
	|	MACHINE:  UNIX                                        |
	|							      |
	|	STARTED:  12-APR-88  BY:  J.C. Wathey                 |
	|							      |
	|	REVISED:  12-APR-88  BY:  JCW                         |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: file inclusions, global definitions,        |
	|                 global variable declarations                |
	|                                                             |
	-------------------------------------------------------------*/

/*---------------------------------- GLOBAL DEFINITIONS -------------*/

#define CBREAK_MASK	2		  /* sgttyb masks; see */
#define ECHO_MASK	8		  /* tty(4)	       */


	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  sets					      |
	|							      |
	|	MODULE:	  sets.h				      |
	|							      |
	|	RELATED						      |
	|	MODULES:  sets.c				      |
	|							      |
	|	MACHINE:  Any UNIX machine			      |
	|							      |
	|	STARTED:  11-MAR-89	   BY:	J.C. Wathey	      |
	|							      |
	|	REVISED:  11-MAR-89	   BY:	JCW		      |
	|							      |
	|	STATUS:	     incomplete or untested		      |
	|		     compiles; partly tested		      |
	|		     runs; revisions in progress	      |
	|		  -> runs; stable version		      |
	|							      |
	|	CONTAINS: global definitions and declarations for     |
	|		  sets routines				      |
	|							      |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/

#define SET_SIZE	16

typedef unsigned char	SET[ SET_SIZE ];


/*-------------------------------- GLOBAL DECLARATIONS --------------*/

    void clear_set();
    void set_assign();
    void set_plus();
    void set_minus();
    void set_times();
    void set_plus_char();
    void set_minus_char();
    int  in_set();
    SET *str_to_set();

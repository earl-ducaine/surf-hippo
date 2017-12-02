
	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:                                              |
	|							      |
	|	MODULE:   screen.c                                    |
	|							      |
	|	RELATED						      |
	|	MODULES:  screen.h                                    |
	|							      |
	|	MACHINE:  UNIX                                        |
	|							      |
	|	STARTED:  12-APR-88  BY:  J.C. Wathey                 |
	|							      |
	|	REVISED:  22-MAR-89  BY:  JCW                         |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: routines for screen handling                |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       cc -c screen.c                                        |
	|       cc pgmname.c screen.o jwtools.o -ltermcap             |
	|                                                             |
	-------------------------------------------------------------*/


#include <stdio.h>
#include "tf.h"

static char * cls_str = (char *) NULL;

/*-------------------------------------------------------------------*/
int init_screen()

	      /* Gets screen clearing info from termcap database. */

{
				       /*----- functions called -----*/
    char       * getenv();

				       /*----- extern variables -----*/
    extern char * cls_str;

				       /*----- local  variables -----*/
    int 	error, 
		status;

    char	termcap_buffer[BUFSIZ],
		cls_buffer[BUFSIZ],
	      * buf_ptr,
	      * terminal_name;


				       /*----- start function -------*/

    terminal_name = getenv("TERM");
    buf_ptr = cls_buffer;

    if (error = (terminal_name == (char *) NULL)) {
	fprintf(stderr,"init_screen: can't find TERM in environment\n");
    }
    else if ((status=tgetent(termcap_buffer, terminal_name)) == -1) {
	fprintf(stderr,"init_screen: can't open termcap file\n");
	error = TRUE;
    }
    else if (error = !status) {
	fprintf(stderr,"init_screen: no entry for %s in termcap file\n",
	terminal_name);
    }
    else if (error = !(tgetstr("cl", &buf_ptr))) {
	fprintf(stderr,
	"init_screen: can't get screen clearing string for %s\n",
	terminal_name);
    } 
    else {
	error = strsave(&cls_str, cls_buffer, "screen clearing string");
    }

    return(error);

}

/*-------------------------------------------------------------------*/
clear_screen()

	      /* Clears screen using info from termcap database */

{
    extern char * cls_str;

    if (cls_str) {
	printf(cls_str);
	fflush(stdout);
    }
    else
	system("clear");
}


	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:                                              |
	|							      |
	|	MODULE:   direct_io.c                                 |
	|							      |
	|	RELATED						      |
	|	MODULES:  direct_io.h                                 |
	|							      |
	|	MACHINE:  UNIX                                        |
	|							      |
	|	STARTED:  12-APR-88  BY:  J.C. Wathey                 |
	|							      |
	|	REVISED:  01-APR-89  BY:  JCW                         |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: routines                                    |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       cc direct_io.c -lm                                    |
	|                                                             |
	-------------------------------------------------------------*/

#include <stdio.h>
#include <sgtty.h>
#include <ctype.h>
#include "tf.h"
#include "directio.h"		       /* global defs & declarations */

/*---------------------------------- GLOBAL DECLARATIONS ------------*/

static struct sgttyb				/* for direct char I/O */
		tty_cooked_params,
		tty_cbreak_params;

static int	kbd_fdesc,
		scrn_fdesc;


/*-------------------------------------------------------------------*/
int return_pressed()

	/* Returns TRUE if user presses RETURN key; otherwise returns
	FALSE.  Must be in cbreak mode. */

{
				       /*----- functions called -----*/
    int		keypressed();
    char	getkey();
				       /*----- start function -------*/
    if (keypressed())
	return ( getkey() == '\n' );
    else
	return (FALSE);
}

/*-------------------------------------------------------------------*/
init_io_params()

	/* Gets default tty parameters and saves them in global struct
	variable tty_cooked_params; copies these to tty_cbreak_params
	and modifies the latter for c_break mode with no echo.  Also 
	initializes global file descriptors kbd_fdesc and scrn_fdesc.
	These globals are used in I/O routines io_cbreak(), 
	io_cooked(), keypressed(), getkey(), and echokey().  See 
	tty(4) for more info. */

{
				       /*----- extern variables -----*/
    extern struct sgttyb
		tty_cooked_params,
		tty_cbreak_params;

    extern int	kbd_fdesc,
		scrn_fdesc;
				       /*----- start function -------*/

    ioctl(0, TIOCGETP, &tty_cooked_params);	/* Get current tty   */
						/* parameters; 0 is  */
						/* standard input.   */

    tty_cbreak_params = tty_cooked_params;		     /* copy */

    tty_cbreak_params.sg_flags |= CBREAK_MASK; 	       /* set cbreak */

    tty_cbreak_params.sg_flags &= ~ECHO_MASK;	       /* clear echo */

    scrn_fdesc = 1;			   /* screen file descriptor */
    kbd_fdesc  = 0;			 /* keyboard file descriptor */

}

/*-------------------------------------------------------------------*/
wait_for_keypress()

	/* Prompts user to press a key, then waits until key is 
	pressed.  Uses cbreak mode while waiting, but then restores to 
	mode that was in effect on entry. */

{
				       /*----- functions called -----*/
    char 	getkey();
				       /*----- extern variables -----*/
    extern int	kbd_fdesc,
		scrn_fdesc;
				       /*----- local  variables -----*/

    struct sgttyb params_on_entry;
    char c;
				       /*----- start function -------*/

    ioctl(0, TIOCGETP, &params_on_entry);	/* Get current tty   */
						/* parameters; 0 is  */
						/* standard input.   */

    io_cbreak();				       /* set cbreak */

    printf("\n(press a key to continue) ");
    fflush(stdout);
    c = getkey();
    printf("\r                         \r");
    fflush(stdout);

    ioctl(kbd_fdesc, TIOCSETP, &params_on_entry);

}

/*-------------------------------------------------------------------*/
int user_presses(target_key)

	/* Prompts user to press a key, then waits until key is 
	pressed.  Uses cbreak mode while waiting, but then restores to 
	mode that was in effect on entry.  Returns TRUE if user presses 
	the key specified by target_key; FALSE otherwise. */

    char	target_key;

{
				       /*----- functions called -----*/
    char 	getkey();
				       /*----- extern variables -----*/
    extern int	kbd_fdesc,
		scrn_fdesc;
				       /*----- local  variables -----*/

    struct sgttyb params_on_entry;
    char c;
				       /*----- start function -------*/

    ioctl(0, TIOCGETP, &params_on_entry);	/* Get current tty   */
						/* parameters; 0 is  */
						/* standard input.   */

    io_cbreak();				       /* set cbreak */
    c = getkey();
    ioctl(kbd_fdesc, TIOCSETP, &params_on_entry);
    return(c==target_key);

}

/*-------------------------------------------------------------------*/
int user_quits_by_pressing(quit_key)

	/* Prompts user to press quit_key to quit, or any other key to
	continue. Waits until key is pressed, then returns TRUE if 
	quit_key or its uppercase equivalent was pressed; returns
	FALSE otherwise.  Uses cbreak mode while waiting, but then 
	restores to mode that was in effect on entry. */

    char	quit_key;
{
				       /*----- functions called -----*/
    char 	getkey();
				       /*----- extern variables -----*/
    extern int	kbd_fdesc,
		scrn_fdesc;
				       /*----- local  variables -----*/

    struct sgttyb params_on_entry;
    char c;
				       /*----- start function -------*/

    ioctl(0, TIOCGETP, &params_on_entry);	/* Get current tty   */
						/* parameters; 0 is  */
						/* standard input.   */

    io_cbreak();				       /* set cbreak */

    if (quit_key < ' ')
	quit_key = '\n';

    printf("\n(press ");
    if (quit_key=='\n')
	printf("RETURN");
    else
	printf("%c", quit_key);

    printf(" to quit, or any other key to continue) ");
    fflush(stdout);
    c = getkey();
    printf("\r                                                    \r");
    fflush(stdout);

    ioctl(kbd_fdesc, TIOCSETP, &params_on_entry);

    if (islower(quit_key))
	quit_key=toupper(quit_key);

    if (islower(c))
	c = toupper(c);

    return(c == quit_key);
}


/*-------------------------------------------------------------------*/
io_cbreak()

	/* Changes terminal I/O parameters so that chars can be read
	directly from keyboard without echo to screen.  See tty(4). */

{
				       /*----- extern variables -----*/
    extern struct sgttyb
		tty_cbreak_params;

    extern int	kbd_fdesc;
				       /*----- start function -------*/
    ioctl(kbd_fdesc, TIOCSETP, &tty_cbreak_params);
}

/*-------------------------------------------------------------------*/
io_cooked()

	/* Changes terminal I/O parameters so that chars are read
	only after normal buffered line editing.  See tty(4). */

{
				       /*----- extern variables -----*/
    extern struct sgttyb
		tty_cooked_params;

    extern int	kbd_fdesc;
				       /*----- start function -------*/
    ioctl(kbd_fdesc, TIOCSETP, &tty_cooked_params);
}

/*-------------------------------------------------------------------*/
int keypressed()

	/* Returns TRUE if there is keyboard input waiting to be read;
	otherwise returns FALSE.  Works both in cbreak and cooked
	modes. */

{
				       /*----- extern variables -----*/
    extern int	kbd_fdesc;
				       /*----- local  variables -----*/
    long	num_chars_ready;
				       /*----- start function -------*/

    ioctl(kbd_fdesc, FIONREAD, &num_chars_ready);
    return(num_chars_ready > 0);
}

/*-------------------------------------------------------------------*/
char getkey()

	/* Waits for, reads and returns char from keyboard.  Reads
	char without line-editing and without echo to screen if in
	cbreak mode. */

{
				       /*----- extern variables -----*/
    extern int	kbd_fdesc;

				       /*----- local  variables -----*/
    char	c;
				       /*----- start function -------*/
    read(kbd_fdesc, &c, 1);
    return(c);
}

/*-------------------------------------------------------------------*/
purge_keyboard()

	/* Reads and discards all characters waiting to be read from
	keyboard. */

{
				       /*----- functions called -----*/
    char 	getkey();
				       /*----- extern variables -----*/
    extern int	kbd_fdesc;
				       /*----- local  variables -----*/
    long	num_chars_ready;
				       /*----- start function -------*/

    ioctl(kbd_fdesc, FIONREAD, &num_chars_ready);
    while(num_chars_ready--)
	getkey();
}

/*-------------------------------------------------------------------*/
echokey(c)

	/* Writes char c to screen.  Works in both cooked and cbreak 
	modes. */

    char c;
{
				       /*----- extern variables -----*/
    extern int	scrn_fdesc;
				       /*----- start function -------*/
    write(scrn_fdesc, &c, 1);
}

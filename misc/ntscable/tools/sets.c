	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  sets					      |
	|							      |
	|	MODULE:	  sets.c				      |
	|							      |
	|	RELATED						      |
	|	MODULES:  sets.h				      |
	|							      |
	|	MACHINE:  Any UNIX machine			      |
	|							      |
	|	STARTED:  11-MAR-89	   BY:	J.C. Wathey	      |
	|							      |
	|	REVISED:  12-MAR-89	   BY:	JCW		      |
	|							      |
	|	STATUS:	     incomplete or untested		      |
	|		     compiles; partly tested		      |
	|		     runs; revisions in progress	      |
	|		  -> runs; stable version		      |
	|							      |
	|	CONTAINS: routines for manipulating sets	      |
	|							      |
	-------------------------------------------------------------*/


	/* This package supports a limited set type, similar to the 
	Pascal set.  Elements are chars in the range 0 to 0x7F; i.e.,
	'\0' to '\177'.  Each set is encoded as an array of 16 unsigned
	chars, the individual bits of which indicate membership of the
	corresponding 7-bit chars.  Individual chars may be added to
	or deleted from a set using set_plus_char or set_minus_char,
	respectively.  Other routines allow analogous operations on
	pairs of sets.  A set may also be specified by listing its
	members explicitly, or as ranges, in a char string, which can
	then be translated to a set by str_to_set().  The syntax for
	this string representation is as follows.   Chars to be
	included in the set can be listed in any order.  For example,
	the strings "abcdeABCDE", "aAbBcCdDeE", and "EDCBAedcba" all
	specify the same set.  A range is indicated by 2 consecutive
	dots, thus: "a..e"; this is equivalent to "abcde".  Any
	combination of individually specified chars and ranges of chars
	may be used, e.g.: "a..eA..E", "abcdeA..E", and "A..Eedcba" are
	all legal and specify the same set.  The ordering of the
	endpoints of the range IS CRITICAL.  The leftmost (start-of-range)
	endpoint should be numerically less than or equal to the 
	rightmost (end-of-range) endpoint; if it is not, the range will
	be ignored during translation.  For examle, "e..aA..E" yields
	upon translation the same set as does "ABCDE".  The set "a..a"
	is the same as "a".  If the two dots occur at the start of the
	string, then the first element of the range is assumed to be
	'\0' (ascii NULL).  Similarly, if the .. occurs at the end of
	the string, the last element of the range is assumed to be
	'\177' (ascii DEL).  Thus, "..A", "q..", and ".." are the same
	as "\000..A", "q..\177", and "\000..\177", respectively; in the
	last of these examples, all bits in the set are set to 1.  Note
	that it is impossible, in practice, to specify explicitly the NULL 
	as part of the set (as in "\000..A"), because strings are
	null-terminated.  Null can be included only by including it as
	the assumed start of a range (e.g., "..A"), or by adding it to
	the set via set_plus_char(set, '\0').  If the dot char is
	listed by itself, it will be include in the set just like any
	other char (e.g., "a.z" includes only 'a', '.' and 'z').  A dot
	may also be used as the endpoint of a range: "*..." is
	equivalent to "*+'-.".   A dot can never be used as the
	start of a range; e.g., "+...3" is equivalent to "+'-.3",
	NOT "+./0123". */


#include "sets.h"

/*-------------------------------------------------------------------*/
void clear_set( set )

    SET		set;

{
    memset((char *) set, '\0', SET_SIZE);
}


/*-------------------------------------------------------------------*/
void set_assign( dest_set, source_set )

	/* Conceptually equivalent to: dest_set = source_set */

    SET		dest_set,
		source_set;


{
    memcpy((char *) dest_set, (char *) source_set, SET_SIZE);
}


/*-------------------------------------------------------------------*/
void set_plus( dest_set, source_set )

	/* Conceptually equivalent to: dest_set |= source_set */

    SET		dest_set,
		source_set;

{
				       /*----- local  variables -----*/
    int i;
    unsigned long * dest_ptr,
		  * source_ptr;
				       /*----- start function -------*/
    dest_ptr = (unsigned long *) dest_set;
    source_ptr = (unsigned long *) source_set;

    for (i=0; i<4; i++) {
	*dest_ptr |=  *source_ptr;
	dest_ptr++;
	source_ptr++;
    }
}

/*-------------------------------------------------------------------*/
void set_minus( dest_set, source_set )

	/* Conceptually equivalent to: dest_set &= ~source_set */

    SET		dest_set,
		source_set;

{
				       /*----- local  variables -----*/
    int i;
    unsigned long * dest_ptr,
		  * source_ptr;
				       /*----- start function -------*/
    dest_ptr = (unsigned long *) dest_set;
    source_ptr = (unsigned long *) source_set;

    for (i=0; i<4; i++) {
	*dest_ptr &= ~(*source_ptr);
	dest_ptr++;
	source_ptr++;
    }
}

/*-------------------------------------------------------------------*/
void set_times( dest_set, source_set )

	/* Conceptually equivalent to: dest_set &= source_set */

    SET		dest_set,
		source_set;

{
				       /*----- local  variables -----*/
    int i;
    unsigned long * dest_ptr,
		  * source_ptr;
				       /*----- start function -------*/
    dest_ptr = (unsigned long *) dest_set;
    source_ptr = (unsigned long *) source_set;

    for (i=0; i<4; i++) {
	*dest_ptr &= *source_ptr;
	dest_ptr++;
	source_ptr++;
    }
}

/*-------------------------------------------------------------------*/
void set_plus_char( dest_set, element )

	/* Sets the single bit in dest_set corresponding to
	element. */

    SET			dest_set;
    unsigned char	element;


{
    dest_set[ (element & 0x7F)/8 ] |= (1 << (element % 8));
}


/*-------------------------------------------------------------------*/
void set_minus_char( dest_set, element )

	/* Clears the single bit in dest_set corresponding to
	element. */

    SET			dest_set;
    unsigned char	element;


{
    dest_set[ (element & 0x7F)/8 ] &= ~(1 << (element % 8));
}


/*-------------------------------------------------------------------*/
int in_set( dest_set, element )

	/* Returns a nonzero value if the single bit in dest_set
	corresponding to element is set; otherwise returns FALSE. */

    SET			dest_set;
    unsigned char	element;


{
    return(
    dest_set[ (element & 0x7F)/8 ] & (1 << (element % 8)) );
}


/*-------------------------------------------------------------------*/
SET * str_to_set( set_string )

	/* Translates set_string to a set and returns this set. */

    char      * set_string;

{
				       /*----- functions called -----*/
    void set_plus_char();
    void clear_set();
				       /*----- local  variables -----*/
    static SET	returned_set;

    char      * ptr,
		range_start,
		range_end,
		c;
				       /*----- start function -------*/
    
    clear_set(returned_set);

    for (ptr = set_string; *ptr; ptr++)

	if (*ptr != '.')

	    set_plus_char(returned_set, *ptr);

	else if (*(ptr+1) != '.')

	    set_plus_char(returned_set, *ptr);

	else {					      /* parse range */ 

	    if (ptr == set_string)
		range_start = '\0';
	    else
		range_start = *(ptr-1) + 1;

	    if (*(ptr+2))
		range_end = *(ptr+2);
	    else
		range_end = '\177';

	    for ( c  = range_start; c <= range_end; c++ )
	    	set_plus_char(returned_set, c);

	}  /* end:  parse range */

    return( (SET *) returned_set);

}

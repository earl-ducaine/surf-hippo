	/*-------------------------------------------------------------
	|
	|	PROGRAM:
	|
	|	MODULE:   %W%
	|
	|	RELATED
	|	MODULES:
	|
	|	MACHINE:  UNIX   
	|
	|	STARTED:  08-JAN-89  BY:  J.C. Wathey
	|
	|	REVISED:  %G%   BY:  JCW       
	|
	|	STATUS:      incomplete or untested
	|                    compiles; partly tested
	|                    runs; revisions in progress
	|                 -> runs; stable version
	|
	|       CONTAINS: routines             
	|
	|       COMPILE:                     
	|
	|       cc jwtools.c               
	|
	-------------------------------------------------------------*/


#include <stdio.h>
#include <ctype.h>
/* #include <malloc.h> This has been replaced by explicit declarations 
of malloc and realloc(); MIPS lacks malloc.h */
char * malloc(),
     * realloc();
#include <string.h>
#include "tf.h"

/*-------------------------------------------------------------------*/
int user_says_yes(default_answer)

	/* Prompts for a 'yes' or 'no' response from the user; returns
	TRUE if 'yes', FALSE if 'no'.  If user types something invalid,
	the routine continues to prompt until a valid response is 
	received, or until the user presses only RETURN.  In the latter
	case default_answer (which is displayed in the prompt) will be
	returned. */

    int	default_answer;

{
				       /*----- local  variables -----*/
    int   returned_value, userquits;
    char  string[80], *strptr;

				       /*----- start function -------*/
	returned_value = default_answer;

    do {
	printf(" (Y or N)? %c\b", default_answer ? 'Y' : 'N');
	userquits =  !*(strptr=gets(string));
	printf("\n");

	if (islower(*strptr))
	    *strptr = toupper(*strptr);

	if (! userquits) {
	    switch (*strptr) {
		case 'N':  return(FALSE);
		case 'Y':  return(TRUE);
		default:
		printf("\7Try again");      /* explain error here */
	    }    
	}
    } while ( ! userquits );

    return( returned_value );
}
	




/*-------------------------------------------------------------------*/
void free_memory( ptr )

	/* Does nothing if *ptr is NULL on entry; otherwise, frees
	memory pointed to by *ptr and sets *ptr to NULL. */

    char ** ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

				       /*----- start function -------*/
    if (*ptr != (char *) NULL) {
	free(*ptr);
	*ptr = (char *) NULL;
    }
}


/*-------------------------------------------------------------------*/
int allocate_array(ptr, num_elements, element_size, array_name)

	/* Essentially the same as calloc(), but issues error msg and
	returns TRUE if error occurs.  If element_size or num_elements 
	is 0 on entry, the routine does the same as free_memory(ptr). 
	NOTE THE DOUBLY INDIRECT POINTER! */

    char ** ptr,
	  * array_name;

    unsigned int num_elements,
		 element_size;

{
				       /*----- functions called -----*/
    char * calloc();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int error;

				       /*----- start function -------*/
    if (!element_size || !num_elements) {
	free_memory(ptr);
	return(FALSE);
    }

    if ( error = ( (*ptr = calloc(num_elements, element_size)) == (char *) NULL ) )
	{
	fprintf(stderr,"allocate_array: can't allocate memory");
	if (*array_name)
	    fprintf(stderr," for %s\n",array_name);
	else
	    fprintf(stderr, "\n");
	}

    return(error);

}

/*-------------------------------------------------------------------*/
int allocate_bytes(ptr, area_size, buffer_name)

	/* Essentially the same as malloc(), but issues error msg and
	returns TRUE if error occurs.  If area_size is 0 on entry, the 
	routine does the same as free_memory(ptr).  NOTE THE DOUBLY 
	INDIRECT POINTER! */

    char ** ptr,
	  * buffer_name;

    unsigned int area_size;

{
				       /*----- local  variables -----*/
    int error;
				       /*----- start function -------*/
    if (!area_size) {
	free_memory(ptr);
	return(FALSE);
    }

    if ( error = ( (*ptr = malloc(area_size)) == (char *) NULL ) )
	{
	fprintf(stderr,"allocate_bytes: can't allocate memory");
	if (*buffer_name)
	    fprintf(stderr," for %s\n",buffer_name);
	else
	    fprintf(stderr, "\n");
	}

    return(error);
}


/*-------------------------------------------------------------------*/
int reallocate(ptr, new_size, buffer_name)

	/* Essentially the same as realloc(), but issues error msg and
	returns TRUE if error occurs.  If *ptr is NULL on entry, the 
	routine does a malloc instead of a realloc.  If new_size is 
	0 on entry, the routine does the same as free_memory(ptr).
	NOTE THE DOUBLY INDIRECT POINTER! */

    char ** ptr,
	  * buffer_name;

    unsigned int new_size;

{
				       /*----- local  variables -----*/
    int error;
				       /*----- start function -------*/

    if (!new_size) {
	free_memory(ptr);
	return(FALSE);
    }

    if (!(*ptr)) {
	error = allocate_bytes(ptr, new_size, buffer_name);
    }
    else if ( error = !(*ptr = realloc(*ptr,new_size)) ) {
	fprintf(stderr,"reallocate: can't allocate memory");
	if (*buffer_name)
	    fprintf(stderr," for %s\n",buffer_name);
	else
	    fprintf(stderr, "\n");
    }

    return(error);
}


/*-------------------------------------------------------------------*/
int strsave(dest_str_ptr, source_str)

	/* Allocates enough space to hold source_str, assigns the 
	addr of this buffer to *dest_str_ptr, and then copies
	source_str to *dest_str_ptr.  Gives error msg and returns 
	TRUE if malloc error occurs. NOTE THE DOUBLY INDIRECT 
	POINTER! */

    char **dest_str_ptr, *source_str;

{

    int error;

    if ( !(error = allocate_bytes(dest_str_ptr, strlen(source_str) + 1,
		  "use by strsave()" ) ))
	strcpy(*dest_str_ptr, source_str);

    return(error);
}


/*-------------------------------------------------------------------*/
int stradd(dest_str_ptr, source_str)

	/* Reallocates space allotted to *dest_str_ptr so that it can
	hold its current contents plus the string in source_str. 
	The routine then appends source_str to *dest_str_ptr.  Gives 
	error msg and returns TRUE if error occurs. NOTE THE DOUBLY 
	INDIRECT POINTER! */

    char **dest_str_ptr, *source_str;

{

    int error;

    if ( !(error = reallocate(dest_str_ptr, strlen(*dest_str_ptr) +
					   strlen(source_str) + 1,
		  "use by stradd()" ) ))
	strcat(*dest_str_ptr, source_str);

    return(error);
}

/*-------------------------------------------------------------------*/
int strnsave(dest_str_ptr, source_str, length)

	/* Same as strsave, except that no more than 'length' bytes
	will be copied from source_str.  */

    char **dest_str_ptr, *source_str;
    int    length;

{

    int error, source_length;

    if ( (source_length=strlen(source_str)) > length )
	source_length = length;

    if ( !(error = allocate_bytes(dest_str_ptr, source_length + 1,
		  "use by strnsave()" ) )) {
	strncpy(*dest_str_ptr, source_str, source_length);
	*(*dest_str_ptr + source_length) = '\0';
    }

    return(error);
}


/*-------------------------------------------------------------------*/
int strnadd(dest_str_ptr, source_str, length)

	/* Same as strnadd, except that no more than 'length' bytes
	will be appended from source_str.  */

    char **dest_str_ptr, *source_str;
    int    length;

{

    int error, source_length;

    if ( (source_length=strlen(source_str)) > length )
	source_length = length;

    if ( !(error = reallocate(dest_str_ptr, strlen(*dest_str_ptr) +
					   source_length + 1,
		  "use by strnadd()" ) ))
	strncat(*dest_str_ptr, source_str, source_length);

    return(error);
}


/*-------------------------------------------------------------------*/
int d_get_string(string_ptr, prompt, max_length)

	/* Prompts user for a string of up to max_length chars and 
	puts it into dynamic memory at the address returned in
	*string_ptr.  max_length must be <= BUFSIZ.  The string will 
	always end in \0. If no string was entered the routine does 
	nothing.  An existing string is deleted from dynamic memory 
	and *string_ptr is zeroed if the user enters "delete".  
	Returns TRUE if error, else FALSE.  */

    int		max_length;
    char     ** string_ptr,
	      * prompt;

{
				       /*----- functions called -----*/

    char * fgets();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char text[BUFSIZ + 2];
    char * newline_ptr;
    int error = FALSE;
				       /*----- start function -------*/

    if (max_length > BUFSIZ)
        max_length = BUFSIZ;

    if (*string_ptr) {
	printf("\n\ncurrent %s:\n\n%s\n\n", 
	(prompt && *prompt)? prompt : "string",
	*string_ptr);
    }

    printf("\nEnter new %s of %d chars or less%s:\n\n",
    (prompt && *prompt)? prompt : "string",
    max_length, (*string_ptr)? 
    ",\nor type 'delete' to delete the current string" : "");
    fgets(text, max_length + 1, stdin);

		   /* skip over any excess garbage from the keyboard */
    error = fseek(stdin, 0L, 2);
    if (error) {
	perror("stdin");
	return (error);
    }

    purge_leading_space(text);

    if (newline_ptr = strchr(text, '\n'))
    	*newline_ptr = '\0';

    if (!strcmp(text, "delete")) {
	if (*string_ptr) {
	    free(*string_ptr);
	    *string_ptr = (char *) NULL;
	}
    }
    else if (text[0]) {		  /* copy string if present */
	if (*string_ptr)
	    free(*string_ptr);
        error = strsave(string_ptr, text);
    }

    return(error);
}


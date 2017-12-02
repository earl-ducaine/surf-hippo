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
	|       CONTAINS: routines for getting numbers from the       |
	|                 keyboard                                    |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       cc getnum.c -lm                                       |
	|                                                             |
	-------------------------------------------------------------*/


#include <stdio.h>
#include <math.h>
#include "tf.h"
#include "getnum.h"	       /* global defs & declarations */


/*-------------------------------------------------------------------*/
int d_out_of_range(value, min_endpoint, max_endpoint, endpoint_flags)

	/* Returns TRUE if value is not within the range specified by
	min_endpoint and max_endpoint.  The test is inclusive-left if
	bit 0 of endpoint_flags is set and inclusive_right if bit 1
	is set.  If value is out of range, routine writes error message
	showing valid range and prompting user for another entry.  This
	routine is intended for use with get_double(). */

    double	value,
		min_endpoint,
		max_endpoint;
    short	endpoint_flags;

{

				       /*----- local  variables -----*/
    int		out_of_range,
		min_out_of_range,
		max_out_of_range;
    char	*min_comparison,
    		*max_comparison;

				       /*----- start function -------*/

						 /* test lower limit */
    if (endpoint_flags & INCLUDE_MIN) {
	min_out_of_range = (value < min_endpoint);
	min_comparison	 = ">=";
    }
    else {
	min_out_of_range = (value <= min_endpoint);
	min_comparison	 = ">";
    }

						 /* test upper limit */
    if (endpoint_flags & INCLUDE_MAX) {
	max_out_of_range = (value > max_endpoint);
	max_comparison	 = "<=";
    }
    else {
	max_out_of_range = (value >= max_endpoint);
	max_comparison	 = "<";
    }

    out_of_range = min_out_of_range || max_out_of_range;

    if (out_of_range) {
	printf("\7Out of range:  must be %s %g and %s %g\n",
		min_comparison, min_endpoint,
		max_comparison, max_endpoint);
	printf("Try again");
    }

    return(out_of_range);

}

/*-------------------------------------------------------------------*/
int f_out_of_range(value, min_endpoint, max_endpoint, endpoint_flags)

	/* Returns TRUE if value is not within the range specified by
	min_endpoint and max_endpoint.  The test is inclusive-left if
	bit 0 of endpoint_flags is set and inclusive_right if bit 1
	is set.  If value is out of range, routine writes error message
	showing valid range and prompting user for another entry.  This
	routine is intended for use with get_float(). */

    float	value,
		min_endpoint,
		max_endpoint;
    short	endpoint_flags;

{

				       /*----- local  variables -----*/
    int		out_of_range,
		min_out_of_range,
		max_out_of_range;
    char	*min_comparison,
    		*max_comparison;

				       /*----- start function -------*/

						 /* test lower limit */
    if (endpoint_flags & INCLUDE_MIN) {
	min_out_of_range = (value < min_endpoint);
	min_comparison	 = ">=";
    }
    else {
	min_out_of_range = (value <= min_endpoint);
	min_comparison	 = ">";
    }

						 /* test upper limit */
    if (endpoint_flags & INCLUDE_MAX) {
	max_out_of_range = (value > max_endpoint);
	max_comparison	 = "<=";
    }
    else {
	max_out_of_range = (value >= max_endpoint);
	max_comparison	 = "<";
    }

    out_of_range = min_out_of_range || max_out_of_range;

    if (out_of_range) {
	printf("\7Out of range:  must be %s %g and %s %g\n",
		min_comparison, min_endpoint,
		max_comparison, max_endpoint);
	printf("Try again");
    }

    return(out_of_range);

}

/*-------------------------------------------------------------------*/
int i_out_of_range(value, min_endpoint, max_endpoint, endpoint_flags)

	/* Returns TRUE if value is not within the range specified by
	min_endpoint and max_endpoint.  The test is inclusive-left if
	bit 0 of endpoint_flags is set and inclusive_right if bit 1
	is set.  If value is out of range, routine writes error message
	showing valid range and prompting user for another entry.  This
	routine is intended for use with get_int(). */

    int		value,
		min_endpoint,
		max_endpoint;
    short	endpoint_flags;

{

				       /*----- local  variables -----*/
    int		out_of_range,
		min_out_of_range,
		max_out_of_range;
    char	*min_comparison,
    		*max_comparison;

				       /*----- start function -------*/

						 /* test lower limit */
    if (endpoint_flags & INCLUDE_MIN) {
	min_out_of_range = (value < min_endpoint);
	min_comparison	 = ">=";
    }
    else {
	min_out_of_range = (value <= min_endpoint);
	min_comparison	 = ">";
    }

						 /* test upper limit */
    if (endpoint_flags & INCLUDE_MAX) {
	max_out_of_range = (value > max_endpoint);
	max_comparison	 = "<=";
    }
    else {
	max_out_of_range = (value >= max_endpoint);
	max_comparison	 = "<";
    }

    out_of_range = min_out_of_range || max_out_of_range;

    if (out_of_range) {
	printf("\7Out of range:  must be %s %d and %s %d\n",
		min_comparison, min_endpoint,
		max_comparison, max_endpoint);
	printf("Try again");
    }

    return(out_of_range);

}

/*-------------------------------------------------------------------*/
int get_int(current_value)

	/* Prompts for, inputs and returns an integer.  If user types 
	something invalid, the routine continues to prompt until a 
	valid number is received, or until the user presses only 
	RETURN.  In the latter case current_value (which is displayed 
	in the prompt) will be returned. */

    int	current_value;

{
				       /*----- local  variables -----*/
    int   returned_value, new_value, userquits, valid_number;
    char  string[80], *strptr;

				       /*----- start function -------*/
    do {
	returned_value = current_value;

	printf(" (%d): ", current_value);
	userquits =  !*(strptr=gets(string));
	printf("\n");


	if (! userquits) {

	    if (valid_number = sscanf(strptr,"%d", &new_value))
		returned_value = new_value;
	    else
		printf("\7Try again");      /* explain error here */
		    
	}
    } while ( !valid_number && ! userquits );

    return( returned_value );
}
	
/*-------------------------------------------------------------------*/
int get_int_in_range(	current_value, 
			min_endpoint, 
			max_endpoint, 
			endpoint_flags)

	/* Prompts for, inputs and returns an integer within the range 
	specified by min_endpoint and max_endpoint.  The test is 
	inclusive-left if bit 0 of endpoint_flags is set and 
	inclusive_right if bit 1 is set.  If value is out of range, 
	routine writes error message showing valid range and prompting 
	user for another entry.  The routine continues to prompt in a 
	loop until a valid number is received, or until the user 
	presses only RETURN.  In the latter case current_value (which 
	is displayed in the prompt) will be returned. */


    int		current_value,
		min_endpoint,
		max_endpoint;
    short	endpoint_flags;

{
				       /*----- local  variables -----*/

    int temp_i;

				       /*----- start function -------*/


    do {
    	temp_i = get_int(current_value);
    } while ( i_out_of_range(temp_i, 
			     min_endpoint,
			     max_endpoint,
			     endpoint_flags) );
    return(temp_i);
}


/*-------------------------------------------------------------------*/
float get_float(current_value)

	/* Prompts for, inputs and returns a float.  If user types 
	something invalid, the routine continues to prompt until a 
	valid number is received, or until the user presses only 
	RETURN.  In the latter case current_value (which is displayed 
	in the prompt) will be returned. */

    float	current_value;

{
				       /*----- functions called -----*/
    double get_double();
				       /*----- start function -------*/

    return( (float) get_double( (double) current_value));
}

/*-------------------------------------------------------------------*/
double get_double(current_value)

	/* Prompts for, inputs and returns a double.  If user types 
	something invalid, the routine continues to prompt until a 
	valid number is received, or until the user presses only 
	RETURN.  In the latter case current_value (which is displayed 
	in the prompt) will be returned. */

    double	current_value;

{
				       /*----- local  variables -----*/
    double	returned_value, new_value;
    int		userquits, valid_number;
    char	string[80], *strptr;
    /* static char default_format[]= " (%1.4e): "; */
    static char default_format[]= " (%g): ";

				       /*----- start function -------*/
    do {
	returned_value = current_value;

	printf(default_format, current_value);
	userquits =  !*(strptr=gets(string));
	printf("\n");

	if (! userquits) {
	    if (valid_number = sscanf(strptr,"%F", &new_value))
		returned_value = new_value;
	    else
		printf("\7Try again");      /* explain error here */
		    
	}
    } while ( !valid_number && ! userquits );

    return( returned_value );
}

/*-------------------------------------------------------------------*/
double get_double_in_range( current_value, 
			    min_endpoint, 
			    max_endpoint, 
			    endpoint_flags )

	/* Prompts for, inputs and returns a double within the range 
	specified by min_endpoint and max_endpoint.  The test is 
	inclusive-left if bit 0 of endpoint_flags is set and 
	inclusive_right if bit 1 is set.  If value is out of range, 
	routine writes error message showing valid range and prompting 
	user for another entry.  The routine continues to prompt in a 
	loop until a valid number is received, or until the user 
	presses only RETURN.  In the latter case current_value (which 
	is displayed in the prompt) will be returned. */


    double	current_value,
		min_endpoint,
		max_endpoint;
    short	endpoint_flags;

{
				       /*----- local  variables -----*/

    double temp;

				       /*----- start function -------*/


    do {
    	temp = get_double(current_value);
    } while ( d_out_of_range(temp, 
			     min_endpoint,
			     max_endpoint,
			     endpoint_flags) );
    return(temp);
}



	/*-------------------------------------------------------------
	|
	|	MODULE:   %M%
	|
	|	MACHINE:  Sun 3/60
	|
	|	STARTED:  11-FEB-91        BY:  J.C. Wathey
	|
	|	REVISED:  %G%         BY:  JCW
	|
	|	STATUS:      incomplete or untested
	|                    compiles; partly tested
	|                 -> runs; revisions in progress
	|                    runs; stable version
	|
	|       CONTAINS: routines for doing simple statistics
	|
	|	COMPILE:  (use makefile)
	|
	-------------------------------------------------------------*/



#include <stdio.h>
#include "tf.h"			       /* defines TRUE and FALSE */
#include "jwtools.h"
#include "jwmath.h"
#include "jwstat.h"		       /* global defs & declarations */


static int	n = 0;

static double * variate_list = (double *) NULL;


/*-------------------------------------------------------------------*/
void clear_statistics()

	/* Clears static variables used for statistics accumulations.
	and frees dynamic memory allocated to variate_list.  */

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int	n;

    extern double * variate_list;

				       /*----- start function -------*/
    free_memory((char **) &variate_list);

    n = 0;
}

/*-------------------------------------------------------------------*/
void add_variate_value( value )

	/* Increases space allocated to variate_list and stores value
	there.  Increments count of variates.  */

    double	value;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int	n;

    extern double * variate_list;

				       /*----- local  variables -----*/

				       /*----- start function -------*/

    if (reallocate( (char **) &variate_list,
		    (n+1)*sizeof( double ),
		    "variate in add_variate_value" ))
	exit(TRUE);

    variate_list[n++] = value;
}

/*-------------------------------------------------------------------*/
int delete_variate_value( value )

	/* Finds and deletes from variate_list the most recently 
	entered instance of value.  Decreases the space allocated to
	variate_list approriately and decrements the count of variates.
	Returns FALSE if value was found and deleted.  Returns TRUE and
	does nothing to variate_list if value was not found.  */

    double	value;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int	n;

    extern double * variate_list;

				       /*----- local  variables -----*/
    int		i;

				       /*----- start function -------*/


    for (i=n-1; i+1 && (variate_list[i] != value); i--);

    if (i<0) {
	return(TRUE);
    }

    n--;

    while (i<n) {
	variate_list[i] = variate_list[i+1];
	i++;
    }

    if (reallocate( (char **) &variate_list,
		    n * sizeof( double ),
		    "variate in delete_variate_value" ))
	exit(TRUE);

    return(FALSE);
}

/*-------------------------------------------------------------------*/
int  get_statistics( n_ptr, mean_ptr, sdev_ptr, min_ptr, max_ptr )

	/* Calculates and returns the number, mean, standard 
	deviation, minimum and maximum values of the variates in 
	variates_list.  Returns TRUE if error; FALSE otherwise. */

    int	      * n_ptr;
    double    * mean_ptr,
	      * sdev_ptr,
	      * min_ptr,
	      * max_ptr;



{
				       /*----- functions called -----*/
    double sqrt();

				       /*----- extern variables -----*/
    extern int	    n;
    extern double * variate_list;
				       /*----- local  variables -----*/

				       /*----- start function -------*/

    return (get_statistics_from_list( variate_list, 
				      (*n_ptr=n), 
				      mean_ptr, 
				      sdev_ptr, 
				      min_ptr, 
				      max_ptr ));

}


/*-------------------------------------------------------------------*/
int  get_statistics_from_list(  list, 
				list_size, 
				mean_ptr, 
				sdev_ptr, 
				min_ptr, 
				max_ptr )

	/* Calculates and returns the mean, standard deviation, 
	minimum and maximum values of the list_size variates in 
	list.  Returns *sdev_ptr = 0.0 if list_size = 1 on entry.
	Returns TRUE if list is NULL or list_size is 0 on entry;
	returns FALSE otherwise. */

    int	        list_size;
    double    * list,
	      * mean_ptr,
	      * sdev_ptr,
	      * min_ptr,
	      * max_ptr;



{
				       /*----- functions called -----*/
    double sqrt();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

    int		i;
    double	sum,
		sum_of_squares,
		variate,
		least,
		greatest,
		sdev,
		mean;
				       /*------ start function ------*/


    if (!list || !list_size) {
	return(TRUE);
    }

    least = greatest = list[0];
    
    mean = sum = sum_of_squares = sdev = 0.0;


    for (i=0; i<list_size; i++) {

	sum += (variate = list[i]);
	sum_of_squares += variate * variate;

	least    = min(variate, least);
	greatest = max(variate, greatest);
    }

    mean = sum/list_size;

    if (list_size>1)
	sdev = sqrt((sum_of_squares-sum*sum/list_size)/(list_size-1));

    * mean_ptr = mean;
    * sdev_ptr = sdev;
    * min_ptr  = least;
    * max_ptr  = greatest;

    return(FALSE);
}



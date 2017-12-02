	/* %W%  %G% */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                      |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  20-APR-89        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  %G%         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: routine translate() and related             |
	|                 private routines                            |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include "jwtools.h"
#include "jwmath.h"
#include "ntscable.h"		       /* global defs & declarations */


/*-------------------------------------------------------------------*/
void translate( neuron_ptr, cable_ptr, neurite_list_ptr) 

	/* Translates the morphological information in *neuron_ptr into
	a format from which the appropriate 'hoc' code can be easily
	generated.  The translated info is returned in *cable_ptr,
	if CABLE syntax is to be used, or in the list at 
	*neurite_list_ptr if NEURON syntax is to be used.  Returns in
	neuron_ptr->num_segments_used the number of segments actually
	used for the simulation. */


    NEURON    * neuron_ptr; 
    CABLE     * cable_ptr; 
    NEURITE  ** neurite_list_ptr;

{
				       /*----- functions called -----*/
    double	tree_length(),
		tree_area();
    void	get_neuron_branch();
    void   	get_cable_geometry();
    void   	get_neuron_geometry();
    char      * ctime();
    long   	time();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    long	xlate_time;

    double	total_length,
		total_area,
		max_dx;
    int		num_primary_neurites = 0;

    TREE_POINT *tree_ptr;
				       /*----- start function -------*/

    total_length = total_area = 0.0;

    for( tree_ptr = neuron_ptr->neurites;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->branch ) {

	total_length += tree_length(tree_ptr->next);
	total_area   += tree_area(tree_ptr->next);

	num_primary_neurites++;
    }

    neuron_ptr->total_length_of_neurites = total_length;
    neuron_ptr->total_area_of_neurites = total_area;
    neuron_ptr->num_primary_neurites = num_primary_neurites;

    max_dx = total_length/(neuron_ptr->num_segments_requested);
    if (neuron_ptr->max_dx > 0.0)
        neuron_ptr->max_dx = min( max_dx, neuron_ptr->max_dx );
    else
        neuron_ptr->max_dx = max_dx;


    			     /* get the date and time of translation */
    time(&xlate_time);


    if (strsave( &(neuron_ptr->time_of_translation),
                 ctime(&xlate_time) ))
	exit(TRUE);


    switch (neuron_ptr->destination) {

	case O_CABLE:
	case O_MULTI:
    	    get_cable_geometry( neuron_ptr, cable_ptr );
	    break;

	default:
	case O_NEURON:
    	    get_neuron_geometry( neuron_ptr, neurite_list_ptr );
	    break;
    }
}

/*-------------------------------------------------------------------*/
static void get_cable_geometry( neuron_ptr, cable_ptr )


	/* Translates the morphological information in *neuron_ptr into
	a format from which the appropriate 'hoc' code can be easily
	generated.  The translated info is returned as lists of structs
	(within the structure pointed to by cable_ptr).  Returns in
	neuron_ptr->num_segments_used the number of segments actually
	used for the cable simulation. */


    NEURON    * neuron_ptr; 
    CABLE     * cable_ptr; 


{
				       /*----- functions called -----*/
    void	get_cable_branch();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    int		first_index = 1;

    TREE_POINT *tree_ptr;
				       /*----- start function -------*/


    for( tree_ptr = neuron_ptr->neurites;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->branch )

	get_cable_branch( neuron_ptr->accuracy_level,
			   neuron_ptr->max_dx, 
			    neuron_ptr->max_dx_over_mean_diam, 
			     &first_index, 
			      tree_ptr->next,
			       cable_ptr );

    neuron_ptr->num_segments_used = first_index;

}
/*-------------------------------------------------------------------*/
static void get_neuron_geometry( neuron_ptr, neurite_list_ptr )


	/* Translates the morphological information in *neuron_ptr into
	a format from which the appropriate 'hoc' code can be easily
	generated.  The translated info is returned as lists of structs
	at *neurite_list_ptr.  Returns in neuron_ptr->num_segments_used
	the number of segments actually used for the NEURON simulation. 
	*/


    NEURON    * neuron_ptr; 
    NEURITE  ** neurite_list_ptr;


{
				       /*----- functions called -----*/
    void	get_neuron_branch();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    char	neurite_number_string[BUFSIZ];
    int		num_segments_used = 1,
		i;

    TREE_POINT * tree_ptr;
    NEURITE    * neurite_ptr;
				       /*----- start function -------*/

    			   /* allocate space for entire neurite list */
    
    if ( reallocate( (char **) neurite_list_ptr,
			neuron_ptr->num_primary_neurites
			* sizeof(NEURITE),
			 "neurite list" ))
	exit(TRUE);

    bzero( (char *) *neurite_list_ptr, 
	     neuron_ptr->num_primary_neurites * sizeof(NEURITE));

    for (i=0; i< neuron_ptr->num_primary_neurites; i++) {

	sprintf(neurite_number_string, "%d", i + FIRST_SECTION_NUMBER);

	neurite_ptr = (*neurite_list_ptr) + i;

	if ( strsave( &(neurite_ptr->name), SECTION_NAME)
	     ||
	     stradd(  &(neurite_ptr->name), neurite_number_string))
	    exit(TRUE);
    }

    for( tree_ptr = neuron_ptr->neurites, i=0;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->branch, i++ )

	get_neuron_branch( neuron_ptr->max_dx, 
			     neuron_ptr->max_dx_over_mean_diam, 
			      &num_segments_used, 
			        tree_ptr->next,
				 SOMA_SECTION_INDEX,
				  (*neurite_list_ptr)+i );

    neuron_ptr->num_segments_used = num_segments_used;

}

/*-------------------------------------------------------------------*/
static double tree_length(root_ptr)

	/* Returns total length of tree rooted at root_ptr->previous.
	*/

    TREE_POINT * root_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	 length = 0.0;
    TREE_POINT * tree_ptr;
				       /*----- start function -------*/
    for ( tree_ptr = root_ptr;
	  tree_ptr;
	  tree_ptr = tree_ptr->next ) {
	length += tree_ptr->length;
	if (tree_ptr->branch)
	    length += tree_length(tree_ptr->branch);
    }
    return(length);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static double tree_area(root_ptr)

	/* Returns total area of tree rooted at root_ptr->previous. */

    TREE_POINT * root_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	 area = 0.0;
    TREE_POINT * tree_ptr;
				       /*----- start function -------*/
    for ( tree_ptr = root_ptr;
	  tree_ptr;
	  tree_ptr = tree_ptr->next ) {
	area += M_PI * tree_ptr->length 
		* ( tree_ptr->diam
		    +
		    tree_ptr->previous->diam ) / 2.0;
	if (tree_ptr->branch)
	    area += tree_area(tree_ptr->branch);
    }
    return(area);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static void get_cable_branch( 	accuracy_level,
				max_dx_arg, 
			        max_dx_over_mean_diam, 
			        first_index_ptr, 
			        root_ptr,
			        cable_ptr )

	/* Gets info necessary for fcbs calls and dx, diam 
	initialization for the branch rooted at root_ptr->previous.  
        Recursively does the same for all higher order branches 
        connected to this one.  The results are placed into the arrays 
        in *cable_ptr.  The space for these arrays is dynamically
	allocated by this routine (or routines called by this one).
	The counts of elements in these arrays are appropriately 
	incremented.  On entry, *first_index_ptr is the 'cable' 
	index number for the first segment of the current branch 
	(i.e., the first argument of the next fcbs call to be 
	generated).  On exit, *first_index_ptr is incremented to 
	1 + the last 'cable' index number used.  The argument
	accuracy_level specifies how much of each branch is to
	be divided into segments of equal length (see comments to
	translation_interval() for details). */

    double	     max_dx_arg, 
		     max_dx_over_mean_diam;
    int	             accuracy_level,
		   * first_index_ptr;
    TREE_POINT     * root_ptr;
    CABLE	   * cable_ptr;


{
				       /*----- functions called -----*/
    void determine_diam_intervals(),
	 determine_dx_interval(),
	 translation_interval();

    double mean_diameter();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		first_index;
    double	dx,
		max_dx,
		max_dx_from_diam;
    FCBS_ARGS  *fcbs_ptr;
    TREE_POINT *tree_ptr,
	       *first_ptr,
	       *last_ptr;

				       /*----- start function -------*/

    			/* Main loop: if accuracy_level is
			BETWEEN_POINTS or BETWEEN_BRANCHPOINTS,
			each iteration handles a length of 
			neurite between adjacent points or
			branchpoints, respectively.  If accuracy_level
			is WHOLE_BRANCH, the loop executes only once 
			(for each call of this routine) and processes 
			the entire branch in that one iteration. */

    for( first_index = *first_index_ptr,
	 first_ptr = last_ptr = root_ptr;

	 translation_interval( accuracy_level,
		      	       first_ptr, 
		      	       &last_ptr),
	 first_ptr;

	 first_ptr = last_ptr->next ) {


	max_dx_from_diam = max_dx_over_mean_diam * 
			   mean_diameter(first_ptr, last_ptr);

	max_dx = (max_dx_from_diam > 0.0) ?
	         min(max_dx_from_diam, max_dx_arg) : max_dx_arg;

	determine_dx_interval( max_dx, 
			       first_index, 
			       first_ptr, 
			       last_ptr,
			       &dx,
			       cable_ptr );

	determine_diam_intervals( &first_index, 
			          first_ptr,
			          last_ptr,
				  dx,
			          cable_ptr );
    }

    if ( reallocate( (char **) &(cable_ptr->fcbs_args_list),
			++(cable_ptr->num_fcbs_args)
			* sizeof(FCBS_ARGS),
			 "fcbs arguments list" ))
	exit( TRUE );

    fcbs_ptr =  cable_ptr->fcbs_args_list +
		cable_ptr->num_fcbs_args  - 1;

				  /* store fcbs args for this branch */
    fcbs_ptr->first_index = *first_index_ptr;
    fcbs_ptr->last_index = last_ptr->cable_index;
    fcbs_ptr->root_index = root_ptr->previous->cable_index;

    *first_index_ptr = last_ptr->cable_index + 1;

				       /* go to distal end of branch */
    for( tree_ptr = root_ptr;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->next );

			       /* do connecting branches recursively */
    for ( ;
	  (tree_ptr->next) != root_ptr
	  &&
	  (tree_ptr->branch) != root_ptr;
	  tree_ptr = tree_ptr->previous )

	if (tree_ptr->branch)
	    get_cable_branch(accuracy_level,
				 max_dx_arg, 
			          max_dx_over_mean_diam, 
			  	   first_index_ptr, 
			  	    tree_ptr->branch,
				     cable_ptr);
	    
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static void get_neuron_branch(  max_dx_arg, 
			        max_dx_over_mean_diam, 
			        num_segments_used_ptr, 
			        root_ptr,
				parent_index,
			        neurite_ptr )

	/* Gets info necessary for the creation and connection of
	segments and for the creation of the x,y,z,diam list for 
	the branch rooted at root_ptr->previous.  Recursively does 
	the same for all higher order branches connected to this 
	one.  The results are placed into the hierarchically
	organized arrays in *neurite_ptr.  The space for these 
	arrays is dynamically allocated by this routine (or routines 
	called by this one).  The counts of elements in these arrays 
	are appropriately incremented.  On entry, 
	*num_segments_used_ptr is the number of segments used thus
	far.  On exit, *num_segments_used_ptr is updated to include
	the additional segments required for the current branch. */

    double	     max_dx_arg, 
		     max_dx_over_mean_diam;
    int	           * num_segments_used_ptr,
		     parent_index;

    TREE_POINT     * root_ptr;
    NEURITE	   * neurite_ptr;


{
				       /*----- functions called -----*/
    void create_xyzd_list(),
	 translation_interval();

    double mean_diameter();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

    int		section_index,

/* lbg change 5/6/94 */
                section_segment_index;
/* lbg change 5/6/94 */


    double	max_dx,
		max_dx_from_diam,
		distance_from_proximal_end,
		branch_length;

    SECTION    *section_ptr;

    TREE_POINT *tree_ptr,
	       *last_ptr;

				       /*----- start function -------*/

    translation_interval( WHOLE_BRANCH, root_ptr, &last_ptr);


    max_dx_from_diam = max_dx_over_mean_diam * 
			   mean_diameter(root_ptr, last_ptr);

    max_dx = (max_dx_from_diam > 0.0) ?
	         min(max_dx_from_diam, max_dx_arg) : max_dx_arg;

    		   /* calculate length between first and last points */

    branch_length = 0.0;
    for( tree_ptr = root_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next )
	branch_length += tree_ptr->length;

		       /* If length is 0.0, we're dealing with one
		       or more tree points all with length = 0.0.
		       Set the section name and section x value of
		       these points to those of their parent node 
		       and return.  The result is that any branch with 
		       total length 0.0 will be omitted from the
		       translation. */

    if (branch_length == 0.0) {

        for( tree_ptr = root_ptr;
	     tree_ptr != last_ptr->next;
	     tree_ptr = tree_ptr->next ) {

	    tree_ptr->section_name = root_ptr->previous->section_name;
	    tree_ptr->section_x    = root_ptr->previous->section_x;
	}

	return;
    }
    					   /* allocate a new section */

    if ( reallocate( (char **) &(neurite_ptr->section_list),
			++(neurite_ptr->num_sections)
			* sizeof(SECTION),
			 "section list" ))
	exit( TRUE );

    section_index = neurite_ptr->num_sections-1;
    
    section_ptr = neurite_ptr->section_list
                + section_index;

    bzero( (char *) section_ptr, sizeof(SECTION));

					     /* fill the new section */

    section_ptr->num_segments = (int) ceil(branch_length/max_dx);
    section_ptr->parent_index = parent_index;

/* lbg change - 4/6/94 */
    section_ptr->origin_in_name = root_ptr->previous->section_name;
    section_ptr->origin_in_index = root_ptr->previous->section_segment_index;


    section_ptr->origin_in_parent = root_ptr->previous->section_x; 
    section_ptr->length = branch_length;

    create_xyzd_list( section_ptr, root_ptr, last_ptr);
    

    		      /* increment count of total number of sections */

    *num_segments_used_ptr += section_ptr->num_segments;

    			        /* label tree points with corresponding 
			        section name and x value */

    distance_from_proximal_end = 0.0;

/* lbg change 4/6/94 */
    section_segment_index = 0; 
/* lbg change 4/6/94 */

    for( tree_ptr = root_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next ) {

        distance_from_proximal_end += tree_ptr->length;
	tree_ptr->section_name = neurite_ptr->name;
	tree_ptr->section_x    = distance_from_proximal_end
				 /branch_length;
/* lbg change 4/6/94 */
	tree_ptr->section_segment_index = section_segment_index;
	section_segment_index = section_segment_index + 1;
/* lbg change 4/6/94 */

    }

				       /* go to distal end of branch */
    for( tree_ptr = root_ptr;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->next );

			       /* do connecting branches recursively */
    for ( ;
	  (tree_ptr->next) != root_ptr
	  &&
	  (tree_ptr->branch) != root_ptr;
	  tree_ptr = tree_ptr->previous )

	if (tree_ptr->branch)
	    get_neuron_branch( max_dx_arg, 
			          max_dx_over_mean_diam, 
			  	   num_segments_used_ptr, 
			  	    tree_ptr->branch,
				     section_index,
				      neurite_ptr );

}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
static void create_xyzd_list( section_ptr, root_ptr, last_ptr)

	/* Allocates space for and fills the list of x,y,z,diam points
	to be used for specifying the shape and diameter of the section
	specified by section_ptr in the NEURON simulation. */

    TREE_POINT * root_ptr,
	       * last_ptr;

    SECTION    * section_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    TREE_POINT * tree_ptr;
    XYZD       * xyzd_ptr;
				       /*----- start function -------*/

    if ( reallocate( (char **) &(section_ptr->xyzd_list),
			++(section_ptr->num_xyzd_points)
			* sizeof(XYZD),
			 "xyzd list" ))
	exit( TRUE );
    
    xyzd_ptr = section_ptr->xyzd_list
             + section_ptr->num_xyzd_points - 1;

    xyzd_ptr->x    = root_ptr->previous->x;
    xyzd_ptr->y    = root_ptr->previous->y;
    xyzd_ptr->z    = root_ptr->previous->z;
    xyzd_ptr->diam = root_ptr->previous->diam;

    for( tree_ptr = root_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next ) {

	if ( reallocate( (char **) &(section_ptr->xyzd_list),
			    ++(section_ptr->num_xyzd_points)
			    * sizeof(XYZD),
			     "xyzd list" ))
	    exit( TRUE );

	xyzd_ptr = section_ptr->xyzd_list
		 + section_ptr->num_xyzd_points - 1;

	xyzd_ptr->x    = tree_ptr->x;
	xyzd_ptr->y    = tree_ptr->y;
	xyzd_ptr->z    = tree_ptr->z;
	xyzd_ptr->diam = tree_ptr->diam;
    }
}

/*-------------------------------------------------------------------*/
static void translation_interval( accuracy_level,
		         	  first_ptr, 
		         	  last_2ptr)

	/* This routine selects a portion of the neurite to be 
	translated into segments of equal length; this portion is
	called the "translation interval".  Its proximal end is
	first_ptr->previous (first_ptr is set by the calling program).
	Its distal end is returned by this routine in *last_2ptr.
	The location of the distal end is determined in one of three
	different ways, according to the value of accuracy_level. 
	If accuracy_level ==  WHOLE_BRANCH, the distal end of the
	translation interval will be the distal end of the branch; 
	the interval will therefore be the entire branch.  In this 
	case each branchpoint will be moved to the distal end of 
	the cable segment containing it.  If accuracy_level == 
	BETWEEN_BRANCHPOINTS, the distal end of the interval will 
	be the nearest branchpoint at or distal to *first_ptr (or the 
	distal end of the branch if there is no distal branchpoint).
	In this case the original locations of the branchpoints are
	exactly preserved in the translation.  If accuracy_level 
	== BETWEEN_POINTS, the distal end of the interval will be 
	*first_ptr.  In this case the branchpoint locations are 
	preserved, and each interval between consecutive digitized 
	points will be translated exactly into an integral number 
	of segments.  The routine does nothing if first_ptr is NULL 
	on entry. */


    int		     accuracy_level;
    TREE_POINT     * first_ptr,
		  ** last_2ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    TREE_POINT     * ptr;

				       /*----- start function -------*/


    if (first_ptr) {

        for (ptr = first_ptr;

	     ptr->next 
	     &&
	     ( (accuracy_level==BETWEEN_BRANCHPOINTS && !(ptr->branch)) 
	       ||
	       (accuracy_level==WHOLE_BRANCH) );

	     ptr = ptr->next );

        *last_2ptr = ptr;
	  
    }
}



/*-------------------------------------------------------------------*/
static void determine_dx_interval( max_dx, 
			           first_index, 
			           first_ptr, 
			           last_ptr,
				   dx_ptr,
			           cable_ptr )

	/* Gets info necessary for dx initialization for the cable
	segments representing *first_ptr through *last_ptr, 
	inclusive.  The most proximal of these segments will have the
	cable index number first_index.  This portion of the neurite 
	will be divided into the smallest possible number of segments 
	of equal length <= max_dx.  The results are placed into the 
	array cable_ptr->dx_interval_list (which this routine enlarges
	via reallocate()), and the count of elements in this array is 
	appropriately incremented.  The routine also determines the 
	corresponding CABLE index numbers for *first_ptr through 
	*last_ptr.  If the neurite length between *first_ptr and 
	*last_ptr is 0.0 on entry, the routine just sets *dx_ptr to
	0.0 without doing anything else. */

    int	             first_index;
    double	     max_dx,
		   * dx_ptr;
    TREE_POINT     * first_ptr,
		   * last_ptr;
    CABLE	   * cable_ptr;


{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double		x_interval_length,
			dx,
			distance_from_proximal_end;

    int			last_index,
			num_segments;

    TREE_POINT        * tree_ptr;

    DX_INTERVAL	      * dx_interval_ptr;

				       /*----- start function -------*/

    		   /* calculate length between first and last points */

    x_interval_length = 0.0;
    for( tree_ptr = first_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next )
	x_interval_length += tree_ptr->length;

		       /* If length is 0.0, we're dealing with one
		       or more tree points all with length = 0.0.
		       Set dx to 0.0, copy the cable index of the 
		       previous tree point to these and return. */

    if (x_interval_length == 0.0) {

	*dx_ptr = 0.0;

        for( tree_ptr = first_ptr;
	     tree_ptr != last_ptr->next;
	     tree_ptr = tree_ptr->next )
	    tree_ptr->cable_index = first_ptr->previous->cable_index;

	return;
    }
    		   	/* calculate number of segments between 
		   	first and last points */

    num_segments= (int) ceil(x_interval_length/max_dx);

				            /* calculate the dx info */

    last_index = first_index + num_segments - 1;
    *dx_ptr = dx = x_interval_length /  num_segments;

				   /* allocate space for the dx info */
 
    if ( reallocate( (char **) &(cable_ptr->dx_interval_list),
			++(cable_ptr->num_dx_intervals)
			* sizeof(DX_INTERVAL),
			 "dx interval list" ))
	exit( TRUE );

    dx_interval_ptr = cable_ptr->dx_interval_list +
		      cable_ptr->num_dx_intervals - 1;

				                /* store the dx info */
    dx_interval_ptr->first_index = first_index;
    dx_interval_ptr->last_index = last_index;
    dx_interval_ptr->dx = dx;

    	 		/* assign 'cable' indices to each tree point 
	 		between first and last points */

    distance_from_proximal_end = 0.0;

    for( tree_ptr = first_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next ) {

	distance_from_proximal_end += tree_ptr->length;
	tree_ptr->cable_index = 
	first_index + (int) (distance_from_proximal_end*(1+1E-8)/dx);

	if( tree_ptr->cable_index > last_index )
	    tree_ptr->cable_index = last_index;
    }

}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
double mean_diameter(first_ptr, last_ptr)

	/* Returns the diameter of cylinder having the same length and
	area as the portion of neurite from first_ptr->previous to
	last_ptr. */

    TREE_POINT  * first_ptr,
		* last_ptr;

{
				       /*----- functions called -----*/
    double area_increment();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    TREE_POINT * tree_ptr;
    double	 area,
		 length;
				       /*----- start function -------*/

    area   = 0.0;
    length = 0.0;

    for( tree_ptr = first_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next ) {

	length += tree_ptr->length;
	area += area_increment(0.0, tree_ptr->length, tree_ptr);
    }

    return( (length > 0.0) ? area/(2*length) : 0.0 );

}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
static void determine_diam_intervals( first_index_ptr, 
			              first_ptr,
			              last_ptr,
				      dx,
			              cable_ptr )

	/* Gets info necessary for diam initialization for the cable 
	segments representing *first_ptr through *last_ptr, 
	inclusive.  The most proximal of these segments will have the
	cable index number *first_index_ptr.  The results are placed
	into the array cable_ptr->diam_interval_list (which this 
	routine enlarges via reallocate()), and the count of elements 
	in this array is appropriately incremented.  On exit, 
	*first_index_ptr is incremented to 1 + the highest cable index
	number that has so far been used in the lists in *cable_ptr.
	The routine does nothing if dx is zero on entry. */

    int	           * first_index_ptr;
    TREE_POINT     * first_ptr,
		   * last_ptr;
    double	     dx;
    CABLE	   * cable_ptr;


{
				       /*----- functions called -----*/
    void   append_diam_interval();
    double area_increment(),
	   fabs();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double		interval_first_diam,
			interval_last_diam,
			proximal_x,
			leftover_length = 0.0,
			leftover_area = 0.0;

    int			first_index,
			doing_last_tree_point,
			interval_first_index,
			interval_last_index;

    TREE_POINT        * tree_ptr;

				       /*----- start function -------*/


    if (dx == 0.0)
	return;

    first_index = *first_index_ptr;

    for( tree_ptr = first_ptr;
	 tree_ptr != last_ptr->next;
	 tree_ptr = tree_ptr->next ) {

    	proximal_x = 0.0;
    	doing_last_tree_point = (tree_ptr == last_ptr)? 1:0;

	if ( tree_ptr->cable_index + doing_last_tree_point
	     >
	     first_index ) {

	    if (leftover_length > 0.0) {

		interval_first_diam =
		( leftover_area +
		  area_increment( 0.0, dx-leftover_length, tree_ptr)
		) / (2 * dx);

		append_diam_interval( first_index, 
				      first_index,
				      interval_first_diam,
				      interval_first_diam,
				      cable_ptr );
		first_index++;
		proximal_x = dx - leftover_length;
		leftover_length = leftover_area = 0.0;
	    }

	    if ( tree_ptr->cable_index + doing_last_tree_point
	         >
	         first_index ) {
		
		interval_first_index =
		interval_last_index = first_index;

		interval_first_diam = 
		interval_last_diam =
		area_increment( proximal_x,
				proximal_x + dx,
				tree_ptr ) / (2 * dx);
		
		while( tree_ptr->cable_index + doing_last_tree_point
	               >
	               first_index + 1) {

		    proximal_x += dx;
		    first_index++;
		    interval_last_index++;
		}

		if (interval_last_index > interval_first_index) {
		    interval_last_diam = 
		    area_increment( proximal_x,
				    proximal_x + dx,
				    tree_ptr ) / (2 * dx);
		    if (fequal(interval_last_diam,interval_first_diam))
		        interval_last_diam = interval_first_diam; 
		}

		append_diam_interval( interval_first_index, 
				      interval_last_index,
				      interval_first_diam,
				      interval_last_diam,
				      cable_ptr );
		first_index++;
		proximal_x += dx;
		leftover_length = leftover_area = 0.0;
	    }
        }

	leftover_length += tree_ptr->length - proximal_x;
	leftover_area += area_increment( proximal_x, 
					 tree_ptr->length,
					 tree_ptr );
    }

    *first_index_ptr = first_index;
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static void append_diam_interval( first_index, 
			          last_index,
				  first_diam,
				  last_diam,
			          cable_ptr )

	/* If on entry first_diam, last_diam and the first_diam and
	last_diam fields of the most recently appended diam_interval
	are all equal, then this routine just changes the last_index
	field of the existing diam_interval to the value in the 
	last_index argument.  Otherwise the routine appends the 
	diameter interval requested in the first 4 arguments to the 
	array cable_ptr->diam_interval_list (which this routine 
	enlarges via reallocate()), and the count of elements in this 
	array is appropriately incremented. */

    int	             first_index,
		     last_index;
    double	     first_diam,
		     last_diam;
    CABLE	   * cable_ptr;


{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

    DIAM_INTERVAL     * diam_interval_ptr;

				       /*----- start function -------*/


    if ( cable_ptr->num_diam_intervals
	 &&
	 first_diam == last_diam
	 &&
         ((diam_interval_ptr = cable_ptr->diam_interval_list +
	      		     cable_ptr->num_diam_intervals-1),
	 fequal(first_diam, diam_interval_ptr->first_diam))
	 &&
	 fequal(first_diam, diam_interval_ptr->last_diam) ) {

	diam_interval_ptr->last_index = last_index;
	diam_interval_ptr->first_diam = first_diam;
	diam_interval_ptr->last_diam  = first_diam;
    }
    else {
				 /* allocate space for the diam info */
 
	if ( reallocate( (char **) &(cable_ptr->diam_interval_list),
			    ++(cable_ptr->num_diam_intervals)
			    * sizeof(DIAM_INTERVAL),
			     "diam interval list" ))
	    exit( TRUE );

        diam_interval_ptr = cable_ptr->diam_interval_list +
	      		    cable_ptr->num_diam_intervals - 1;

	diam_interval_ptr->first_index = first_index;
	diam_interval_ptr->last_index  = last_index;
	diam_interval_ptr->first_diam  = first_diam;
	diam_interval_ptr->last_diam   = last_diam;
    }

}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
static double area_increment( proximal_x, distal_x, tree_ptr)

	/* Returns 2/pi times the area of part of the membrane lying
	between tree_ptr and tree_ptr->previous.  The portion measured
	is that which lies between planes perpendicular to the neurite
	at distances proximal_x and distal_x from tree_ptr->previous.
	*/

    double	 proximal_x,
		 distal_x;
    TREE_POINT * tree_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	slope,
		intercept;
				       /*----- start function -------*/

    if (tree_ptr->length == 0.0)
	return(0.0);

    intercept = tree_ptr->previous->diam;
    slope = (tree_ptr->diam - intercept) / tree_ptr->length;

    return( (slope*(proximal_x + distal_x) + 2*intercept)
	     * (distal_x - proximal_x) );

}


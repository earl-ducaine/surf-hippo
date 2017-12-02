	/* %W%  %G% */


	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                  |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  20-APR-89        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  %G%         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                 -> runs; revisions in progress	      |
	|                    runs; stable version		      |
	|							      |
	|       CONTAINS: routines for creating schematic diagrams    |
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
#include <memory.h>
#include <errno.h>
#include "figtools.h"
#include "jwmath.h"
#include "ntscable.h"
#include "diagrams.h"

static char label_spaces[] = " ";








/*-------------------------------------------------------------------*/
int draw_diagram_list( diagram_list )

	/* Draws a schematic diagram for each diagram in diagram_list.
	Returns TRUE if error, FALSE otherwise. */

    DIAGRAM * diagram_list;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;
    DIAGRAM   * diagram_ptr;

				       /*----- start function -------*/

    error = FALSE;

    for (diagram_ptr = diagram_list;
	 diagram_ptr;
	 diagram_ptr = diagram_ptr->next) {

	if (error = draw_diagram(diagram_ptr) )
	    break;
    }

    return(error);

}

/*-------------------------------------------------------------------*/
int draw_diagram( diagram_ptr )

	/* Draws the schematic diagram specified by *diagram_ptr.
	Returns TRUE if error, FALSE otherwise. */

    DIAGRAM * diagram_ptr;
{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;

				       /*----- start function -------*/
    error = FALSE;

    if (diagram_ptr->destinations & FIG)
        error = error || draw_fig_diagram(diagram_ptr,0.5,7.0,0.0,9.0);

    if (diagram_ptr->destinations & GRAPH)
        error = error || draw_graph_diagram(diagram_ptr);

    return(error);

}



/*-------------------------------------------------------------------*/
int draw_fig_diagram( diagram_ptr,
		      view_x_min,
		      view_x_max,
		      view_y_min,
		      view_y_max)

	/* Produces an ascii file which, when read into fig or filtered
	through f2ps | lpr, produces the schematic diagram specified 
	by *diagram_ptr.  The file is given the name in diagram_ptr->
	filename if that string is nonempty on entry; otherwise it is
	given a name of the form "SN_dn.fig", where: S is the data 
	source, either "in" if the data are from the original input 
	file or "out" if from the translation; N is the segment 
	number of the most proximal segment in the diagram, padded 
	with leading zeroes; _n, _d or _dn are optional and indicate 
	that the diagram includes segment number labels, diameters 
	encoded as linewidths, or both, respectively.  If the diagram
	must be printed in multiple panels, a separate file is created
	for each panel.  These files are named as just described,
	except that the panel number is appended to the name.  Panels
	are numbered left-to-right, top-to-bottom, starting with 1 for
	the panel in the upper left corner.  The last 4 arguments are 
	the absolute coordinates, in inches, of the region on the 
	screen (or page) occupied by the diagram.  Returns TRUE if 
	error; FALSE otherwise. */

    DIAGRAM   * diagram_ptr;
    double	view_x_min,
		view_x_max,
		view_y_min,
		view_y_max;

{
				       /*----- functions called -----*/
    void draw_fig_panel();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char	filename[BUFSIZ],
		filename_format[BUFSIZ],
		panel_filename[BUFSIZ],
		last_panel_as_string[50],
		legend_string[BUFSIZ];
    int 	panel_number,
		panel_x_index,
		panel_y_index,
		error,
		magnification,
		show_labels,
		show_diameters,
		soma_included;
    double	diam_min,
		diam_max,
		y_space_between_branches,
		y_min,
		y_max,
		x_min,
		x_max,
		panel_y_min,
		panel_y_max,
		panel_x_min,
		panel_x_max,
		panel_height,
		panel_width,
		label_height,
		legend_height,
		top_margin,
		y_step,
		y_min_branches;

				       /*----- start function -------*/
    error = FALSE;
    show_labels    = diagram_ptr->show_labels;
    show_diameters = diagram_ptr->show_diameters;
    diam_min       = diagram_ptr->diam_min;
    soma_included  = !(diagram_ptr->segment_list->index);

    if (!(diagram_ptr->filename) || !(*(diagram_ptr->filename))) {

        sprintf(filename_format, "%%s%%0%dd%%s%%s%%s.%%s",
            diagram_ptr->num_segment_chars);

        sprintf(filename, filename_format,
            diagram_ptr->from_input ? "in" : "out",
            diagram_ptr->segment_list->index,
            (show_labels || show_diameters) ? "_":"",
            show_diameters ? "d" : "",
            show_labels ? "n" : "",
            "fig");

	if (strsave( &(diagram_ptr->filename), filename))
	    return(TRUE);
    }
    else {
        strcpy(filename, diagram_ptr->filename);
    }

    strcpy(panel_filename, filename);

    if (diagram_ptr->branch_separation <= 0.0)
        diagram_ptr->branch_separation = DEF_BRANCH_SEPARATION;

    if (diagram_ptr->label_height <= 0.0)
        diagram_ptr->label_height = DEF_LABEL_HEIGHT;

    if (diagram_ptr->description_height <= 0.0)
        diagram_ptr->description_height = DEF_DESCRIPTION_HEIGHT;

    if (diagram_ptr->scalebar_length <= 0.0)
        diagram_ptr->scalebar_length = DEF_SCALEBAR_LENGTH;

    if (diagram_ptr->connection_width <= 0.0)
        diagram_ptr->connection_width = DEF_CONNECTION_WIDTH;

    diam_max = show_diameters ? diagram_ptr->diam_max : 1.0;
    y_step    = show_diameters ? 
    diagram_ptr->branch_separation : Y_STEP;

    x_min          = diagram_ptr->x_min;
    x_max 	   = diagram_ptr->x_max;
    top_margin     = y_space_between_branches = y_step * diam_max;
    label_height   = y_space_between_branches 
			* diagram_ptr->label_height;
    legend_height  = y_space_between_branches * 3;
    y_min_branches = y_space_between_branches * diagram_ptr->y_min;
    y_max          = y_space_between_branches * diagram_ptr->y_max
			+ top_margin;
    y_min          = y_min_branches - legend_height;

    			     /* correct origin if soma is in diagram */
    if (soma_included) {
	y_min -= show_diameters ? 
	diagram_ptr->segment_list->diam : diam_max;
    }

    viewport( view_x_min, view_x_max, view_y_min, view_y_max );
    window( x_min, x_max, y_min, y_max );

    magnification = set_charsize_in_y( label_height );

    if (diagram_ptr->magnification)
    	magnification = diagram_ptr->magnification;

    if (magnification > 1) {
        sprintf(filename_format, "%%s%%0%dd",
        strlen(sprintf(last_panel_as_string,"%d",
			magnification * magnification)));
    }

    panel_number = 1;
    panel_width = (x_max-x_min)/magnification;
    panel_height = (y_max-y_min)/magnification;

    for (panel_y_index = 0;
         panel_y_index < magnification;
         panel_y_index++) {

        for (panel_x_index = 0;
             panel_x_index < magnification;
             panel_number++,
             panel_x_index++) {

	    if (magnification > 1)
		sprintf(panel_filename, filename_format,
			filename, panel_number);

	    panel_x_min = x_min + panel_x_index * panel_width;
	    panel_x_max = panel_x_min + panel_width;
	    panel_y_max = y_max - panel_y_index * panel_height;
	    panel_y_min = panel_y_max - panel_height;

    	    if (open_fig_file(panel_filename, diagram_ptr->description))
		return(TRUE);

    	    viewport( view_x_min, view_x_max, view_y_min, view_y_max );
	    window( panel_x_min, panel_x_max, panel_y_min, panel_y_max);

	    if (magnification > 1) {
		sprintf(legend_string, "panel %d of %d: %s",
			panel_number, magnification * magnification,
			diagram_ptr->description);
		legend(legend_string, -1, 0.02);
	    }

	    draw_fig_panel( diagram_ptr,
		    	    diam_min,
		    	    diam_max,
		    	    y_space_between_branches,
		    	    y_min,
		    	    y_max,
		    	    x_min,
		    	    x_max,
		    	    label_height,
		    	    legend_height);

    	    if (error = close_fig_file())
		return(TRUE);
	}
    }


    return(error);
}




/*-------------------------------------------------------------------*/
static void draw_fig_panel( diagram_ptr,
		    	    diam_min,
		    	    diam_max,
		    	    y_space_between_branches,
		    	    y_min,
		    	    y_max,
		    	    x_min,
		    	    x_max,
		    	    label_height,
		    	    legend_height)

	/* Executes figtools instructions to produce one panel of the
	schematic diagram specified by *diagram_ptr.  The other arguments 
	specify grapical details of the diagram and are calculated by the 
	calling program using information in *diagram_ptr.  The contents
	of the panel are determined by the viewport and window settings,
	which must be made by the calling program before this routine is
	called. */

    DIAGRAM   * diagram_ptr;
    double	diam_min,
		diam_max,
		y_space_between_branches,
		y_min,
		y_max,
		x_min,
		x_max,
		label_height,
		legend_height;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char	bar_label[100];
    int 	i,
		num_segments,
		num_branches,
		num_filled_tags,
		num_open_tags,
		show_labels,
		show_diameters,
		soma_included;
    double	x,
		y,
		dx,
		scalebar_length,
		connection_width,
		segment_separation_in_x,
		tag_x,
		tag_y,
		tag_size_in_x,
		tag_size_in_y,
		filled_tag_size_with_overlap,
		open_tag_size_with_overlap,
		tag_separation,
		bar_height;
    SEGMENT   * segment_list;
    BRANCH    * branch_list;

				       /*----- start function -------*/
    show_labels      = diagram_ptr->show_labels;
    show_diameters   = diagram_ptr->show_diameters;
    num_segments     = diagram_ptr->num_segments;
    num_branches     = diagram_ptr->num_branches;
    segment_list     = diagram_ptr->segment_list;
    branch_list      = diagram_ptr->branch_list;
    soma_included    = !(segment_list->index);
    scalebar_length  = diagram_ptr->scalebar_length;
    connection_width = diagram_ptr->connection_width;
    tag_size_in_y    = diagram_ptr->max_tag_size*y_space_between_branches;
    tag_size_in_x    = y_to_x(tag_size_in_y);
    tag_separation   = diagram_ptr->tag_separation;

    segment_separation_in_x = (double) diagram_ptr->segment_separation;

    if (segment_separation_in_x < 0.0 )
        segment_separation_in_x = (double) DEF_SEGMENT_SEPARATION;

    if (segment_separation_in_x > 0.0
	&&
        segment_separation_in_x < (double) MIN_SEGMENT_SEPARATION )
        segment_separation_in_x = (double) MIN_SEGMENT_SEPARATION;

    segment_separation_in_x *= laser_pixel_in_x();


    set_charsize_in_y( label_height );

    						  /* start scale bar */
    start_compound("scale bar");
    if (show_diameters) {
        bar_height = (int) y_space_between_branches;
        if (!bar_height)
	    bar_height++;
	set_linewidth_in_y( bar_height );
        sprintf( bar_label, "%g X %g microns", 
	    bar_height, scalebar_length );
    }
    else {
	set_linewidth_in_y( diam_max/2.0 );
        sprintf(bar_label, "%g microns", scalebar_length);
    }
    move( x_min, y_min + legend_height/2 );
    rdraw( scalebar_length, 0.0 );
    move( x_min, y_min + label_height * 0.05 );
    write_text( bar_label );
    end_compound();				    /* end scale bar */

    						/* write description */
    move( 1.2 * max( x_min + scalebar_length,
		char_width_in_x() * strlen(bar_label)), 
		y_min + label_height * 0.05 );
    set_charsize_in_yrange( diagram_ptr->description_height );
    write_text( diagram_ptr->description );
    set_charsize_in_y( label_height );

    if (soma_included) {
        start_compound("soma");			       /* start soma */
	x = x_min;
	y = y_min + legend_height + ( show_diameters ? 
			    segment_list->diam : diam_max )/2;
	dx = segment_list->dx;
	move(x, y);
	set_linewidth_in_y( show_diameters ? 
			    segment_list->diam : diam_max );
	rdraw(dx, 0.0);
	x += dx;

	filled_tag_size_with_overlap = 0.0;
	open_tag_size_with_overlap = 0.0;

	num_filled_tags = segment_list->num_filled_tags;
	num_open_tags = segment_list->num_open_tags;

	if ( num_filled_tags ) {
	    filled_tag_size_with_overlap =
	    tag_size_in_x * segment_list->filled_tag_size
	    * (1 + tag_separation*(num_filled_tags-1));

	    tag_x = x + filled_tag_size_with_overlap 
		    - segment_list->filled_tag_size*tag_size_in_x/2,
	    tag_y = y;
	    if (num_open_tags)
		tag_y -= segment_list->filled_tag_size * tag_size_in_y/2;
	    move(tag_x, tag_y);
	    mark_point_in_y(segment_list->filled_tag_size * tag_size_in_y,
			    segment_list->filled_tag_shape,
			    FILLED,
			    num_filled_tags,
			    tag_separation);
	}

	if ( num_open_tags ) {
	    open_tag_size_with_overlap =
	    tag_size_in_x * segment_list->open_tag_size
	    * (1 + tag_separation*(num_open_tags-1));

	    tag_x = x + open_tag_size_with_overlap 
		    - segment_list->open_tag_size*tag_size_in_x/2,
	    tag_y = y;
	    if (num_filled_tags)
		tag_y += segment_list->open_tag_size * tag_size_in_y/2;
	    move(tag_x, tag_y);
	    mark_point_in_y(segment_list->open_tag_size * tag_size_in_y,
			    segment_list->open_tag_shape,
			    OPEN,
			    num_open_tags,
			    tag_separation);
	}

	x += max( filled_tag_size_with_overlap,
		    open_tag_size_with_overlap );

	move( x, y-label_height*0.4 );
	write_text(" 0 (soma)");
        end_compound();					 /* end soma */
    }


    						   /* start segments */
    start_compound("segments");
    for (i = soma_included ? 1:0; i<num_segments; i++) {
	set_linewidth_in_y( show_diameters ? 
	      segment_list[i].diam : diam_max);
	x = segment_list[i].x_start;
	y = segment_list[i].y_start * y_space_between_branches;
	dx = segment_list[i].dx;

	filled_tag_size_with_overlap = 0.0;
	open_tag_size_with_overlap = 0.0;

	num_filled_tags = segment_list[i].num_filled_tags;
	num_open_tags = segment_list[i].num_open_tags;

	if ( num_filled_tags ) {
	    filled_tag_size_with_overlap =
	    tag_size_in_x * segment_list[i].filled_tag_size
	    * (1 + tag_separation*(num_filled_tags-1));
	}
	if ( num_open_tags ) {
	    open_tag_size_with_overlap =
	    tag_size_in_x * segment_list[i].open_tag_size
	    * (1 + tag_separation*(num_open_tags-1));
	}

	dx -= max( filled_tag_size_with_overlap,
		     open_tag_size_with_overlap );

	if ( num_filled_tags ) {

	    if (dx > 0.0)
		tag_x = x + segment_list[i].dx 
			- segment_list[i].filled_tag_size*tag_size_in_x/2
			- ((dx > 2*segment_separation_in_x) ?
			segment_separation_in_x : 0.0);
	    else 
		tag_x = x + segment_list[i].dx/2;

	    tag_y = y;
	    if (num_open_tags)
		tag_y -= segment_list[i].filled_tag_size * tag_size_in_y/2;
	    move(tag_x, tag_y);
	    mark_point_in_y(segment_list[i].filled_tag_size * tag_size_in_y,
			    segment_list[i].filled_tag_shape,
			    FILLED,
			    num_filled_tags,
			    tag_separation);
	}

	if ( num_open_tags ) {

	    if (dx > 0.0)
		tag_x = x + segment_list[i].dx 
			- segment_list[i].open_tag_size*tag_size_in_x/2
			- ((dx > 2*segment_separation_in_x) ?
			segment_separation_in_x : 0.0);
	    else 
		tag_x = x + segment_list[i].dx/2;

	    tag_y = y;
	    if (num_filled_tags)
		tag_y += segment_list[i].open_tag_size * tag_size_in_y/2;
	    move(tag_x, tag_y);
	    mark_point_in_y(segment_list[i].open_tag_size * tag_size_in_y,
			    segment_list[i].open_tag_shape,
			    OPEN,
			    num_open_tags,
			    tag_separation);
	}

	if (dx > 2*segment_separation_in_x)
	    dx -= segment_separation_in_x;
	
	if (dx > 0.0) {
	    move(x, y);
	    rdraw(dx, 0.0);
	}

    }
    end_compound();				     /* end segments */


    						/* start connections */
    start_compound("connections");
    set_linewidth_in_y( connection_width
			* (show_diameters ? diam_min : diam_max/3));
    set_dash_in_y( y_space_between_branches / 3 );
    set_line_style( DASHED_LINE );
    for (i=0; i<num_branches; i++) {
	move( branch_list[i].connection_x,
	      branch_list[i].y * y_space_between_branches );
	rdraw( 0.0, -branch_list[i].connection_length
		    * y_space_between_branches );
    }
    end_compound();				  /* end connections */


    						     /* start labels */
    if (show_labels) {
    	start_compound("labels");
    	for (i=0; i<num_branches; i++) {
	    move( branch_list[i].label_x,
	          branch_list[i].y * y_space_between_branches 
	          - LABEL_OFFSET * label_height );
	    write_text( branch_list[i].label );
	}
    	end_compound();				      /* end labels */
    }

}





/*-------------------------------------------------------------------*/
int draw_graph_diagram( diagram_ptr )

	/* Produces an ascii file which, when filtered through the 
	Unix "graph" command, produces a schematic diagram as specified
	in *diagram_ptr.  The file is given a name of the form 
	"SN_dn.gph", where: S is the data source, either "in" if the
	data are from the original input file or "out" if from the
	translation; N is the segment number of the most proximal
	segment in the diagram, padded with leading zeroes; _n, _d
	or _dn are optional and indicate that the diagram includes
	segment number labels, diameters encoded as linewidths, or
	both, respectively.  At present the graph command on our
	system does not support variable linewidths, so the _d
	options never occurs in .gph files.  Returns TRUE if error; 
	FALSE otherwise. */

    DIAGRAM * diagram_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    char	filename[50],
		filename_format[50];
    int 	error;

				       /*----- start function -------*/

    sprintf(filename_format, "%%s%%0%dd%%s%%s.%%s",
        diagram_ptr->num_segment_chars);

    sprintf(filename, filename_format,
        diagram_ptr->from_input ? "in" : "out",
        diagram_ptr->segment_list->index,
        diagram_ptr->show_labels ? "_" : "",
        diagram_ptr->show_labels ? "n" : "",
        "gph");

    /*
    printf("%s\n", filename);
    */

    error = FALSE;

    return(error);
}


/*-------------------------------------------------------------------*/
int start_input_diagram_list( neuron_ptr,
			      diagram_params_ptr,
			      diagram_2ptr )

	/* Allocates memory for and fills a DIAGRAM structure using the
	original digitized description of the neuron in *neuron_ptr.  
	The address of this structure is written into *diagram_2ptr,
	which must be NULL on entry.  The diagram will contain the 
	entire neuron, including the soma.  Information in *neuron_ptr 
	is used to fill the num_segments, num_branches and description 
	fields of DIAGRAM; other fields are filled with info from 
	*diagram_params_ptr.  Returns TRUE if error occurs, FALSE */

    NEURON    * neuron_ptr;
    DIAGRAM  **	diagram_2ptr,
	      * diagram_params_ptr;
    

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;

				       /*----- start function -------*/
    error = FALSE;
    return(error);

}

/*-------------------------------------------------------------------*/
int append_diagram( first_segment, 
		    description,
		    destinations,
		    show_labels,
		    show_diameters,
		    diagram_list )

	/* Allocates memory for and appends to diagram_list a modified
	version of the first DIAGRAM in diagram_list.  The modified 
	diagram comprises the branch containing first_segment and 
	all higher order branches connected to that branch.  If 
	first_segment is 0, the diagram will be of the entire neuron, 
	including the soma.  The arguments show_labels, show_diameters
	and description are used to fill the corresponding fields in 
	the appended DIAGRAM.   The string argument "description" is 
	appended to the default description which is automatically 
	generated.  The segment_list and branch_list fields of the new 
	DIAGRAM point to elements of the corresponding arrays in the 
	first DIAGRAM of the list; no additional memory is allocated 
	for lists pointed to by these fields.  Returns TRUE if error 
	occurs, FALSE otherwise. */

    char	      * description;
    int			first_segment,
			destinations,
			show_labels,
			show_diameters;
    DIAGRAM	      * diagram_list;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return(FALSE);
}

/*-------------------------------------------------------------------*/
int start_output_diagram_list( neuron_ptr,
			       cable_ptr,
			       neurite_list,
	                       diagram_params_ptr,
			       diagram_2ptr)

	/* Allocates memory for and fills a DIAGRAM structure using the
	translated description of the neuron in *cable_ptr.  The 
	address of this structure is written into *diagram_2ptr, which 
	must be NULL on entry.  The diagram will contain the entire 
	neuron, including the soma.  Information in *neuron_ptr is 
	used to fill the num_segments, num_branches and description 
	fields of DIAGRAM; other fields are filled with info from 
	*diagram_params_ptr.  Returns TRUE if error occurs, FALSE 
	otherwise. */

    NEURON	      * neuron_ptr;
    CABLE	      * cable_ptr;
    NEURITE	      * neurite_list;
    DIAGRAM	     ** diagram_2ptr,
	              * diagram_params_ptr;

{
				       /*----- functions called -----*/
    void connect_branch();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/

    char      scratch_string[ BUFSIZ ];
    int       i, 
	      first,
	      last,
	      root,
	      num_segments,
	      num_branches,
	      x_max_branch,
	      seg_at_end;
    double    x_at_end,
	      x_max,
	      diam_min,
	      diam_max,
	      first_diam,
	      last_diam,
	      slope,
	      x_start;
    int	      y_start,
	      y_end;
    SEGMENT * segment_list,
	    * segment_ptr;
    BRANCH  * branch_list,
	    * branch_ptr;
    DIAGRAM * diagram_ptr;

    FCBS_ARGS         * fcbs_args_ptr,
		      * fcbs_list_end;
    DX_INTERVAL       * dx_interval_ptr,
		      * dx_list_end;
    DIAM_INTERVAL     * diam_interval_ptr,
		      * diam_list_end;

				       /*----- start function -------*/

    			/* Construction of diagrams of translations
    			intended for the NEURON simulator is not yet
    			implemented. */

    if (neuron_ptr->destination == O_NEURON)
	return(FALSE);




    fcbs_list_end = cable_ptr->fcbs_args_list + 
		    cable_ptr->num_fcbs_args;

    dx_list_end   = cable_ptr->dx_interval_list + 
		    cable_ptr->num_dx_intervals;

    diam_list_end = cable_ptr->diam_interval_list + 
		    cable_ptr->num_diam_intervals;


    num_segments = neuron_ptr->num_segments_used;
    num_branches = neuron_ptr->num_branches;

    if( allocate_bytes( (char **) diagram_2ptr, 
			sizeof( DIAGRAM ),
			"first output diagram") )
	return(TRUE);

    if( allocate_array( (char **) &segment_list, 
			num_segments, 
			sizeof(SEGMENT), 
			"segment_list") )
	return(TRUE);

    if( allocate_bytes( (char **) &branch_list, 
			num_branches * sizeof(BRANCH), 
			"branch_list") )
	return(TRUE);

    diagram_ptr = *diagram_2ptr;

    memcpy( (char *) diagram_ptr, 
	    (char *) diagram_params_ptr,
	    sizeof(DIAGRAM) );

    bzero( (char *) segment_list, num_segments * sizeof(SEGMENT));
    bzero( (char *) branch_list,  num_branches * sizeof(BRANCH));

    sprintf( scratch_string,
    "translation of %s %s; %d segments; accuracy %d",
    neuron_ptr->input_filename,
    neuron_ptr->input_version,
    num_segments,
    neuron_ptr->accuracy_level );

    if ( strsave( &(diagram_ptr->description), scratch_string))
	return(TRUE);

    sprintf( scratch_string,"%d", num_segments );
    diagram_ptr->filename  	    = (char *) NULL;
    diagram_ptr->num_segment_chars  = strlen(scratch_string); 
    diagram_ptr->num_segments       = num_segments;
    diagram_ptr->num_branches       = num_branches;
    diagram_ptr->segment_list       = segment_list;
    diagram_ptr->branch_list        = branch_list;
    
    		       /* initialize segment index numbers and tags */

    for (i=0; i < num_segments; i++) {
    	segment_list[i].index            = i;
	segment_list[i].num_filled_tags  = DEF_NUM_TAGS;
	segment_list[i].num_open_tags    = DEF_NUM_TAGS;
	segment_list[i].filled_tag_size  = DEF_TAG_SIZE;
	segment_list[i].open_tag_size    = DEF_TAG_SIZE;
	segment_list[i].filled_tag_shape = DEF_TAG_SHAPE;
	segment_list[i].open_tag_shape   = DEF_TAG_SHAPE;
    }
						         /* copy dx */
    segment_list->dx = neuron_ptr->soma_length;

    for ( dx_interval_ptr = cable_ptr->dx_interval_list;
	  dx_interval_ptr < dx_list_end;
	  dx_interval_ptr++ ) {
	
	for ( i  = dx_interval_ptr->first_index;
	      i <= dx_interval_ptr->last_index;
	      i++ )
	    segment_list[i].dx = dx_interval_ptr->dx;

    }
				     /* copy or calculate diameters */

    segment_list->diam = neuron_ptr->soma_diam;

    for ( diam_interval_ptr = cable_ptr->diam_interval_list;
	  diam_interval_ptr < diam_list_end;
	  diam_interval_ptr++ ) {
	
	first = diam_interval_ptr->first_index;
	last  = diam_interval_ptr->last_index;

	first_diam = diam_interval_ptr->first_diam;
	last_diam  = diam_interval_ptr->last_diam;

	if ( first == last) {
	    segment_list[first].diam = (first_diam + last_diam)/2;
	}
	else if ( first + 1 == last  ) {
	    segment_list[first].diam = first_diam;
	    segment_list[last].diam  = last_diam;
	}
	else if (first_diam == last_diam) {
	    for ( i=first; i<=last; i++)
	        segment_list[i].diam = first_diam;
	}
	else {
	    slope = (last_diam - first_diam)/(last-first);
	    for ( i=first; i<=last; i++)
	        segment_list[i].diam = first_diam + slope*(i-first); 
	}
    }
    			   /* copy fcbs args and make branch labels */

    for ( fcbs_args_ptr = cable_ptr->fcbs_args_list, i = 0;
	  i < num_branches;
	  fcbs_args_ptr++, i++ ) {

	branch_list[i].first_index= first= fcbs_args_ptr->first_index;
	branch_list[i].last_index = last = fcbs_args_ptr->last_index;
	branch_list[i].root_index = root = fcbs_args_ptr->root_index;

	if (first == last)
	    sprintf(scratch_string,"%s%d",
	    label_spaces, first);
	else
	    sprintf(scratch_string,"%s%d-%d", 
	    label_spaces, first, last);

	if (strsave(&(branch_list[i].label), scratch_string))
	    return(TRUE);
    }


                    /* determine coordinates of segments and labels */

    x_start = X_ORG;
    y_start = Y_ORG;
    y_end   = Y_ORG;
    				     /* first take care of the soma */
    segment_list->x_start = x_start;
    segment_list->y_start = y_start;

    x_start += segment_list->dx;

    		   /* Now do each primary branch and, recursively, all 
    		   higher-order branches.  Each iteration of this loop
    		   handles one primary neurite and its branches. */

    for (branch_ptr = branch_list;
	 branch_ptr < branch_list + num_branches; 
	 branch_ptr++) {

	root  = branch_ptr->root_index;

			      /* make sure this is a primary neurite */
	if (root) {
	    fprintf(stderr,
	    "connectivity error at branch %s\n", 
	    branch_ptr->label);
	    return(TRUE);
	}

        y_end++;

	connect_branch( num_segments, 
			x_start, 
			y_start, 
			&y_end, 
			&branch_ptr, 
			segment_list,
			branch_list);

        y_end += EXTRA_SPACE_BETWEEN_PRIMARY_NEURITES;

    }

    if (num_branches)
        y_end -= EXTRA_SPACE_BETWEEN_PRIMARY_NEURITES;


    				 /* find required max and min values */
    x_max_branch = 0;
    diam_max = x_max = 0.0;
    diam_min = segment_list->diam; 

    for (i=1; i < num_segments; i++) {

	if (diam_min > segment_list[i].diam)
	    diam_min = segment_list[i].diam;

	if (diam_max < segment_list[i].diam)
	    diam_max = segment_list[i].diam;
    }

    for (i=0; i < num_branches; i++) {

	seg_at_end = branch_list[i].last_index;
	x_at_end   =   segment_list[seg_at_end].x_start 
		     + segment_list[seg_at_end].dx;

	if (x_max < x_at_end) {
	    x_max = x_at_end;
	    x_max_branch = i;
	}
    }

    diagram_ptr->diam_max     = diam_max;
    diagram_ptr->diam_min     = diam_min;
    diagram_ptr->x_max        = x_max;
    diagram_ptr->y_max        = y_end + 1;
    diagram_ptr->x_max_branch = x_max_branch;

    return( FALSE );
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
void init_diagram( diagram_ptr )

	/* Fills the DIAGRAM structure pointed to by diagram_ptr with
	defaults defined in diagrams.h.   Fields for which there are
	no such defaults are cleared to NULL. */

    DIAGRAM	      * diagram_ptr;

{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

				       /*----- start function -------*/

    bzero( (char *) diagram_ptr, sizeof(DIAGRAM));

    diagram_ptr->magnification      = DEF_MAGNIFICATION; 
    diagram_ptr->destinations       = DEF_DIAGRAM_DESTINATIONS; 
    diagram_ptr->show_labels        = DEF_SHOW_LABELS; 
    diagram_ptr->show_diameters     = DEF_SHOW_DIAMETERS; 
    diagram_ptr->segment_separation = DEF_SEGMENT_SEPARATION;
    diagram_ptr->branch_separation  = DEF_BRANCH_SEPARATION;
    diagram_ptr->scalebar_length    = DEF_SCALEBAR_LENGTH;
    diagram_ptr->label_height       = DEF_LABEL_HEIGHT;
    diagram_ptr->description_height = DEF_DESCRIPTION_HEIGHT;
    diagram_ptr->max_tag_size       = DEF_MAX_TAG_SIZE;
    diagram_ptr->tag_separation     = DEF_TAG_SEPARATION;
    diagram_ptr->connection_width   = DEF_CONNECTION_WIDTH;
    
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static void connect_branch( num_segments, 
			    x_start, 
			    y_start, 
			    y_end_ptr, 
			    branch_2ptr, 
			    segment_list,
			    branch_list)

	/* Determines the graphical coordinates for all segments in the
	branch specified by **branch_2ptr, and for the label associated
	with this branch.  Recursively does the same for all higher-
	order branches connected to this one. */

    int		num_segments,
		y_start,
	      * y_end_ptr;
    double	x_start;
    BRANCH   ** branch_2ptr,
              * branch_list;
    SEGMENT   * segment_list;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		i, 
		connection_y_start,
		first,
		last,
		next_root;
    BRANCH    * branch_ptr;
				       /*----- start function -------*/

    first = (*branch_2ptr)->first_index;
    last  = (*branch_2ptr)->last_index;

   			/* determine connection coordinates & length */

    connection_y_start = y_start;

    for ( branch_ptr = (*branch_2ptr)-1 ;
	  branch_ptr >= branch_list 
	  &&
	  branch_ptr->connection_x >= x_start;
	  branch_ptr-- ) {

	if (branch_ptr->connection_x == x_start) {
	    connection_y_start = branch_ptr->y;
	    break;
	}
    }

    (*branch_2ptr)->connection_length= *y_end_ptr - connection_y_start;

    y_start = *y_end_ptr;
    (*branch_2ptr)->connection_x = x_start;
    (*branch_2ptr)->y = y_start;

    				    /* determine segment coordinates */
    for (i=first; i<=last; i++) {
        segment_list[i].x_start = x_start;
        segment_list[i].y_start = y_start;
	x_start += segment_list[i].dx;
    }
    				      /* determine label coordinates */

    (*branch_2ptr)->label_x = x_start;

    while ( (*branch_2ptr)->last_index < num_segments-1
	    &&
	    (next_root = (*branch_2ptr + 1)->root_index) <= last
	    &&
	    next_root >= first ) {

	(*y_end_ptr)++;

	(*branch_2ptr)++;

	connect_branch(  num_segments,
			 segment_list[next_root].x_start
	               + segment_list[next_root].dx,
			 y_start, 
			 y_end_ptr, 
			 branch_2ptr,
			 segment_list,
			 branch_list);
    }

}






	/* The following routines are for generating .gph files. */

/*-------------------------------------------------------------------*/
int draw_nts_neuron( neuron_ptr ) 

	/* Produces ascii files which, when filtered through the 
	appropriate Unix graphics programs, produce schematic diagrams 
        of the neurites of * neuron_ptr.  The files are given names of 
        the form "inN.gph", where N is the neurite number.  Neurites 
        are numbered sequentially, starting with 1, in the order in 
        which they were read from the input file.  As used here, a 
        single "neurite" is a primary neurite, rooted at the soma, and 
        all higher-order branches thereof.  Returns TRUE if error; 
        FALSE otherwise. */


    NEURON	     * neuron_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		error,
    		neurite_number;
    double	x_start,
		y_start,
		y_end;

    TREE_POINT        * tree_ptr;
    char		filename[15];
    FILE	      * schem_fp;
				       /*----- start function -------*/

    for( tree_ptr = neuron_ptr->neurites,
	 neurite_number = 1;
	 tree_ptr && tree_ptr->next;
	 neurite_number++,
	 tree_ptr = tree_ptr->branch ) {

	sprintf(filename, "in%d.gph", neurite_number);
	if (error = (!(schem_fp=fopen(filename,"w")))) {
	    perror(filename);
	    return(error);
	}

	if (error = draw_scale_bar( schem_fp, 100.0, 0.0, 0.0 ))
            return(error);

	x_start = 0.0;
	y_start = y_end = 2 * Y_STEP;


	if (error = draw_nts_neurite( schem_fp,
					  x_start,
					    y_start,
					      &y_end,
						tree_ptr->next,
						  filename ))
	    return(error);

	if (error = fclose(schem_fp)) {
	    fprintf(stderr, "%s: error closing file\n", filename);
	    return(error);
	}

    }

    return(error);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static int draw_nts_neurite( fp,
		             x_start,
		             y_start,
		             y_end_ptr,
		             root_ptr,
		             filename )

	/* Draws schematic for the branch specified by *root_ptr
	by generating an ascii file suitable for standard Unix 
	graphics filters.  Recursively does the same for all higher 
	order branches connected to this one.  Returns TRUE if error;
	FALSE otherwise. */

    FILE	   * fp;
    double	     x_start,
		     y_start,
		   * y_end_ptr;
    TREE_POINT     * root_ptr;
    char	   * filename;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	x_end,
		distance_to_branch;
    int		error,
		last_point_number,
		first_point_number,
		root_point_number;
    TREE_POINT *tree_ptr;
				       /*----- start function -------*/

    first_point_number = root_ptr->point_number;
    root_point_number  = (root_ptr->previous)->point_number;

    if (error=draw_vertical(fp, x_start, y_start, *y_end_ptr))
	return(error);

    y_start = *y_end_ptr;

    x_end = x_start + root_ptr->length;

    for( tree_ptr = root_ptr;
	 tree_ptr && tree_ptr->next;
	 tree_ptr = tree_ptr->next )
	x_end += tree_ptr->next->length;

    last_point_number  = tree_ptr->point_number;

    if (error = draw_horizontal( fp,
		                   x_start,
			             x_end,
			               *y_end_ptr,
			                 first_point_number,
			                   last_point_number ) )
	return(error);

			       /* do connecting branches recursively */
    for ( distance_to_branch = x_end;

	  (tree_ptr->next) != root_ptr
	  &&
	  (tree_ptr->branch) != root_ptr;

	  distance_to_branch -= tree_ptr->length,
	  tree_ptr = tree_ptr->previous )

	if (tree_ptr->branch) {

	    *y_end_ptr += Y_STEP;

	    if (error = draw_nts_neurite( fp,
				          distance_to_branch,		
				          y_start,
				          y_end_ptr,
				          tree_ptr->branch,
				          filename ))
	        return(error);
    	}

    return(error);
}				       /*----- end function ---------*/





/*-------------------------------------------------------------------*/
int draw_cable_neuron( neuron_ptr, 
		       cable_ptr )

	/* Produces ascii files which, when filtered through the 
	appropriate Unix graphics programs, produce schematic diagrams 
        of the neurites described in * cable_ptr.  The files are given 
	names of the form "outN.gph", where N is the neurite number.  
	Neurites are numbered sequentially, starting with 1, in the 
	order in which they were read from the input file.  As used 
	here, a single "neurite" is a primary neurite, rooted at
	the soma, and all higher-order branches thereof.  Returns 
	TRUE if error; FALSE otherwise. */


    NEURON	     * neuron_ptr;
    CABLE	     * cable_ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		error,
    		neurite_number;
    double	x_start,
		y_start,
		y_end;

    FCBS_ARGS         * fcbs_args_ptr,
		      * fcbs_list_end;
    DX_INTERVAL       * dx_interval_ptr;
    char		filename[15];
    FILE	      * schem_fp;
				       /*----- start function -------*/

    for( fcbs_args_ptr = cable_ptr->fcbs_args_list,
	 fcbs_list_end = fcbs_args_ptr + cable_ptr->num_fcbs_args,
	 dx_interval_ptr = cable_ptr->dx_interval_list,
	 neurite_number = 1;

	 fcbs_args_ptr < fcbs_list_end;

	 neurite_number++,
	 fcbs_args_ptr++,
	 dx_interval_ptr++) {

	sprintf(filename, "out%d.gph", neurite_number);
	if (error = (!(schem_fp=fopen(filename,"w")))) {
	    perror(filename);
	    return(error);
	}

	if (error = draw_scale_bar( schem_fp, 100.0, 0.0, 0.0 ))
            return(error);

	x_start = 0.0;
	y_start = y_end = 2 * Y_STEP;

	if (error = draw_cable_neurite( schem_fp,
					  x_start,
					    y_start,
					      &y_end,
						&fcbs_args_ptr,
						  &dx_interval_ptr,
						    filename ))
	    return(error);

	if (error = fclose(schem_fp)) {
	    fprintf(stderr, "%s: error closing file\n", filename);
	    return(error);
	}

    }

    return(error);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
static int draw_cable_neurite( fp,
			       x_start,
			       y_start,
			       y_end_ptr,
			       fcbs_args_2ptr,
			       dx_interval_2ptr,
			       filename )

	/* Draws schematic for the branch specified by *fcbs_args_2ptr
	by generating an ascii file suitable for standard Unix 
	graphics filters.  Recursively does the same for all higher 
	order branches connected to this one.  The pointers 
        *fcbs_args_2ptr, *dx_interval_2ptr are appropriately 
	incremented.  Returns TRUE if error; FALSE otherwise. */

    FILE	   * fp;
    double	     x_start,
		     y_start,
		   * y_end_ptr;
    FCBS_ARGS     ** fcbs_args_2ptr;
    DX_INTERVAL   ** dx_interval_2ptr;
    char	   * filename;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	x_end,
		distance_to_branch,
		dx;
    int		error,
		last_index,
		first_index,
		root_index;
    FCBS_ARGS * next_fcbs;
				       /*----- start function -------*/

    first_index = (*dx_interval_2ptr)->first_index;
    last_index  = (*dx_interval_2ptr)->last_index;
    dx = (*dx_interval_2ptr)->dx;

    if (error=( first_index != (*fcbs_args_2ptr)->first_index
		||
		last_index != (*fcbs_args_2ptr)->last_index )) {
	fprintf(stderr, "%s: index mismatch error\n", filename);
	return(error);
    }

    if (error=draw_vertical(fp, x_start, y_start, *y_end_ptr))
	return(error);

    y_start = *y_end_ptr;
    x_end = x_start + (last_index-first_index + 1) * dx;

    if (error = draw_horizontal( fp,
		                   x_start,
			             x_end,
			               *y_end_ptr,
			                 first_index,
			                   last_index ) )
	return(error);


    while ( (next_fcbs= *fcbs_args_2ptr+1)->first_index
	    &&
	    (root_index = next_fcbs->root_index) <= last_index
	    &&
	    root_index >= first_index ) {

	distance_to_branch = x_start +
			     (root_index - first_index + 1) * dx;

	*y_end_ptr += Y_STEP;

	(*fcbs_args_2ptr)++;
	(*dx_interval_2ptr)++;

	if (error = draw_cable_neurite( fp,
					distance_to_branch,		
					y_start,
					y_end_ptr,
					fcbs_args_2ptr,
					dx_interval_2ptr,
					filename ))
	    return(error);
    }

    return(error);
}				       /*----- end function ---------*/



/*-------------------------------------------------------------------*/
static int draw_vertical( fp, x, y_start, y_end)

	/* Draws a vertical line segment from (x,y_start) to (x,y_end)
	by writing the appropriate ascii coordinates to *fp.  The 
	actual drawing is done by standard Unix graphics filters.  
	Returns TRUE if error, FALSE otherwise. */

    FILE      * fp;
    double	x,
		y_start,
		y_end;


{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;

				       /*----- start function -------*/

    if (y_start == y_end)
	return(FALSE);

    fprintf(fp,"%g %g\n%g %g \" \"\n", x, y_start, x, y_end);

    if (error = ferror(fp))
    	fprintf(stderr,"error writing to schematic file\n");

    return(error);

}

/*-------------------------------------------------------------------*/
static int draw_horizontal( fp,
		            x_start,
		            x_end,
		            y,
		            first_index,
		            last_index )

	/* Draws a horizontal line segment from (x_start,y) to 
	(x_end,y) by writing the appropriate ascii coordinates to 
	*fp.  The actual drawing is done by standard Unix graphics 
	filters.  Returns TRUE if error, FALSE otherwise. */



    FILE      * fp;
    double	x_start,
		x_end,
		y;
    int		first_index,
		last_index;



{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;

				       /*----- start function -------*/

    if (x_start == x_end)
	return(FALSE);

    fprintf(fp,"%g %g\n%g %g \" %d", x_start, y, x_end, y, first_index);
    if (first_index != last_index)
        fprintf(fp,"-%d", last_index);
    fprintf(fp,"\"\n");

    if (error = ferror(fp))
    	fprintf(stderr, "error writing to schematic file\n");

    return(error);

}

/*-------------------------------------------------------------------*/
static int draw_scale_bar( fp, length, x, y )

	/* Draws a horizontal scale bar from (x, y) to 
	(x + length, y) by writing the appropriate ascii coordinates to 
	*fp.  The length is assumed to be in microns and is so labeled.
	The actual drawing is done by standard Unix graphics 
	filters.  Returns TRUE if error, FALSE otherwise. */



    FILE      * fp;
    double	length,
		x,
		y;



{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error;
				       /*----- start function -------*/

    if (length <= 0.0)
	return(FALSE);


    fprintf(fp,"%g %g\n%g %g \" %g microns\"\n",
    x, y, x + length, y, length);

    if (error = ferror(fp))
    	fprintf(stderr, "error writing to schematic file\n");

    return(error);

}


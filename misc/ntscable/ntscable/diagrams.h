	/* %W%   %G% */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                         |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  10-JAN-91        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  %G%         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                 -> runs; revisions in progress	      |
	|                    runs; stable version		      |
	|							      |
	|       CONTAINS: constants and data structures related to    |
	|                 schematic diagrams                          |
	|                                                             |
	|                                                             |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/


				/* ----------- CONSTANTS ----------- */

			     /* origin used in constructing DIAGRAMs */
#define X_ORG	0.0
#define Y_ORG	0
			   /* fraction of real label_height subtracted 
			   from label y position */
#define LABEL_OFFSET	0.4


			   /* in terms of the vertical spacing between 
			   branches; must be integer >= 0 */
#define EXTRA_SPACE_BETWEEN_PRIMARY_NEURITES 2

				/* vertical spacing between branches 
				in schematic diagram is Y_STEP if
				diameters are not displayed). */
#define Y_STEP		4.0

				/* ----------- DEFAULTS  ----------- */

				/* vertical spacing between branches 
				in schematic diagram is
				branch_separation * diam_max if 
				diameters are displayed). */
#define DEF_BRANCH_SEPARATION	1.0

#define WITH_LABELS		TRUE
#define NO_LABELS		FALSE
#define DEF_SHOW_LABELS 	WITH_LABELS

#define WITH_DIAMETERS		TRUE
#define NO_DIAMETERS		FALSE
#define DEF_SHOW_DIAMETERS	WITH_DIAMETERS

		/* width of end-to-end separation between segments
		in schematic diagrams, in laser printer pixels */
#define DEF_SEGMENT_SEPARATION	2

		/* smallest allowable segment separation */
#define MIN_SEGMENT_SEPARATION	2

				/* magnification: 0 means magnification
				will be automatically set to the 
				smallest value that yields legible 
				branch labels */

#define DEF_MAGNIFICATION	0

				/* plotting destinations */
#define FIG		1	
#define GRAPH		2	
#define DEF_DIAGRAM_DESTINATIONS	FIG

					/* scale bar in X data units */
#define DEF_SCALEBAR_LENGTH	100.0

			  /* fraction of vertical extent of viewport */
#define DEF_DESCRIPTION_HEIGHT	0.02

			       /* fraction of space between branches */
#define DEF_LABEL_HEIGHT	1.2
#define DEF_MAX_TAG_SIZE	0.9

			       /* center-to-center separation between
			       overlapping tags, as a fraction of tag 
			       size */
#define DEF_TAG_SEPARATION	0.5

			       		 /* fraction of max_tag_size */
#define DEF_TAG_SIZE		1.0

#define DEF_TAG_SHAPE		CIRCLE
#define DEF_NUM_TAGS		0


		  /* fraction of smallest diameter, if diameters shown,
		  or of (fixed diameter/3) if diameters not shown */
#define DEF_CONNECTION_WIDTH	0.4


	/* Both the original morphological data (in the NEURON and 
	TREE_POINT structures) and the translated data (in the lists
	of FCBS_ARGS, DX_INTERVALs and DIAM_INTERVALs) are
	translated into dynamically allocated arrrays of SEGMENTs and
	BRANCHes, from which schematic diagrams can easily be
	generated.  All schematic diagrams to be plotted are stored
	as a list of DIAGRAMs. */

typedef struct segment {
    int			index;
    double		dx;
    double		diam;
    double		x_start;
    int			y_start;
    double		filled_tag_size;
    double		open_tag_size;
    unsigned		num_filled_tags:8;
    unsigned		num_open_tags:8;
    unsigned		filled_tag_shape:8;
    unsigned		open_tag_shape:8;
} SEGMENT;

typedef struct branch {
    int			first_index;
    int			last_index;
    int			root_index;
    int			connection_length;
    double		connection_x;
    int			y;
    double		label_x;
    char	      * label;
} BRANCH;

typedef struct diagram {
    char	      * description;
    char	      * filename;
    int			magnification;
    int			from_input;
    int			num_segment_chars;
    int			destinations;
    int			show_labels;
    int			show_diameters;
    int			segment_separation;
    double		branch_separation;
    int			x_max_branch;
    int			y_min;
    int			y_max;
    double		x_min;
    double		x_max;
    double		diam_min;
    double		diam_max;
    double		scalebar_length;
    double		label_height;
    double		description_height;
    double		max_tag_size;
    double		tag_separation;
    double		connection_width;
    int			num_segments;
    int			num_branches;
    SEGMENT	      * segment_list;
    BRANCH	      * branch_list;
    struct diagram    * next;
} DIAGRAM;



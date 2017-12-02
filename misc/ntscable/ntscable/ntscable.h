	/* %W%   %G% */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                         |
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
	|       CONTAINS: global definitions, global variable         |
	|                 declarations                                |
	|                                                             |
	|                                                             |
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/


			       	/* tokens for the source file type */
#define EUTECTIC	0
#define DOUGLAS_2D	1
#define DOUGLAS_3D	2
#define NEVIN		3

			       	/* tokens for the destination file type */
#define O_CABLE		0
#define	O_NEURON	1
#define	O_MULTI		2

			       	/* tokens for accuracy levels */
#define WHOLE_BRANCH		0
#define BETWEEN_BRANCHPOINTS	1
#define BETWEEN_POINTS		2

#define DEF_ACCURACY_LEVEL	BETWEEN_BRANCHPOINTS
#define	DEF_NUM_SEGMENTS	1

				/* MAX_NSEG should equal NSEG in the
				cable header file membrane.h */
#define MAX_NSEG	4000

				/* maximum number of lines per HOC
				procedure allowed by CABLE */
#define MAX_LINES_PER_PROC	1000

				/* default cytoplasmic resistivity 
				in ohm-cm2 */
#define DEFAULT_RA	75.0

				/* default base names for sections
				used in the translation to NEURON 
				syntax */
#define SECTION_NAME	"dendrite_"
#define SOMA_NAME	"soma"

				/* other constants used in the 
				translation to NEURON syntax */
#define SOMA_SECTION_INDEX	-1
#define FIRST_SECTION_NUMBER	 1


		/* For the CABLE syntax the translated morphological 
		data are stored in dynamically allocated arrays of 
		FCBS_ARGS, DX_INTERVALs and DIAM_INTERVALs.  These 
		arrays and the numbers of elements in them are fields 
		of the CABLE structure. */

typedef struct fcbs_args {
    int			first_index;
    int			last_index;
    int			root_index;
} FCBS_ARGS;

typedef struct dx_interval {
    int			first_index;
    int			last_index;
    double	 	dx;
} DX_INTERVAL;

typedef struct diam_interval {
    int			first_index;
    int			last_index;
    double	 	first_diam;
    double	 	last_diam;
} DIAM_INTERVAL;

typedef struct cable {
    FCBS_ARGS         * fcbs_args_list;
    DX_INTERVAL       * dx_interval_list;
    DIAM_INTERVAL     * diam_interval_list;
    unsigned long       num_fcbs_args;
    unsigned long       num_dx_intervals;
    unsigned long       num_diam_intervals;
} CABLE;

		/* For the NEURON syntax the translated morphological 
		data are stored in a dynamically allocated array of 
		NEURITEs, one for each primary neurite.  Each NEURITE
		contains a list of SECTIONs, one for the 1st order
		branch and one for each higher-order branch connected
		to it.  Each SECTION contains a list of  XYZDs, from
		which NEURON determines section length, diameters, 
		segment areas and internodal resistances.  */

typedef struct xyzd {
    double		x;
    double		y;
    double		z;
    double		diam;
} XYZD;

typedef struct section {
    int			num_segments;
    double		length;
/* lbg change - 4/6/94 */
    double		origin_in_parent; 
    int		origin_in_index; 
    char	      *	origin_in_name;
/* lbg change - 4/6/94 */

    int			parent_index;
    int			num_xyzd_points;
    XYZD	      * xyzd_list;
} SECTION;

typedef struct neurite {
    int			num_sections;
    SECTION           * section_list;
    char	      * name;
} NEURITE;

		/* The next three structures are used for intermediate
		storage of the digitized data prior to translation. */

typedef struct tree_point {
    double		x;
    double		y;
    double		z;
    double		diam;
    double		length;
    double		section_x;
    char	      * section_name;
    char	      * point_label;
    int			point_number;
    int			cable_index;
/* lbg change 4/6/94 */
    int			section_segment_index;
/* lbg change 4/6/94 */

    struct tree_point * branch;
    struct tree_point * next;
    struct tree_point * previous;
} TREE_POINT;

typedef struct soma_point {
    int			point_number;
    double		x;
    double		y;
    double		z;
    struct soma_point * next;
    struct soma_point * previous;
} SOMA_POINT;

typedef struct neuron {
    double		cytoplasmic_resistivity;
    double		soma_diam;
    double		soma_length;
    double		soma_area;
    double		soma_outline_diam;
    double		total_length_of_neurites;
    double		total_area_of_neurites;
    double		max_dx;
    double		max_dx_over_mean_diam;
    int			num_tree_points;
    int			num_segments_requested;
    int			num_segments_used;
    int			num_branches;
    int			num_primary_neurites;
    int			num_soma_3D_points;
    int			num_soma_outline_points;
    int			first_soma_point;
    int			last_soma_point;
    int			average_outline_z_coords;
    int			source;
    int			destination;
    int			accuracy_level;
    char	      *	time_of_translation;
    char	      * comment;
    char	      * input_version;
    char	      * input_filename;
    char	      * output_filename;
    SOMA_POINT	      * soma_outline;
    XYZD              * soma_3D_list;
    TREE_POINT	      * neurites;
} NEURON;


/*-------------------------------- GLOBAL DECLARATIONS --------------*/

static char	version[]  = "%I%";
static char	pgm_date[] = "%G%";
static char   *	source_syntax[4]  = {
		    "Eutectic",
		    "Douglas 2D",
		    "Douglas 3D",
		    "Nevin"
		};
static char   *	output_syntax[3]  = {
		    "CABLE",
		    "NEURON",
		    "MULTI"
		};

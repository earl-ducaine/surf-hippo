
	/*-------------------------------------------------------------
	|
	|	PROGRAM:
	|
	|	MODULE:   %W%
	|
	|	RELATED
	|	MODULES:  fstnoise.h
	|
	|	MACHINE:  Any unix machine with 32-bit long ints
	|
	|	STARTED:  24-AUG-89        BY:  J.C. Wathey
	|
	|	REVISED:  %G%         BY:  JCW
	|
	|	STATUS:      incomplete or untested
	|                    compiles; partly tested
	|                    runs; revisions in progress
	|                 -> runs; stable version
	|
	|       CONTAINS: routine fast_noise() for generating
	|                 Gaussian white noise with a binary search 
	|                 look-up table algorithm developed by JCW;
	|                 also includes 2 initialization routines. 
	|
	-------------------------------------------------------------*/


/*--------------------------------- GLOBAL DEFINITIONS --------------*/

#include <stdio.h>
#include <math.h>
#include "fstnoise.h"


#define UNIFORM_TABLE_SIZE	100
#define GAUSSIAN_TABLE_SIZE	(MAX_DA+2)

	/* The next four constants are appropriate for
	machines which support 32-bit long integers.  See Numerical
	Recipes in C, section on portable random number
	generators, for values appropriate to other word
	sizes. */

#define IA		2416
#define IC		374441
#define IM		1771875
#define MAX_RAND	(IM-1)

#define random() (uniform_deviate=(uniform_deviate*IA+IC)%IM)


/*-------------------------------- GLOBAL DECLARATIONS --------------*/

static Int4u	gaussian_table[ GAUSSIAN_TABLE_SIZE ],
	    	uniform_table[   UNIFORM_TABLE_SIZE ],
		uniform_deviate = 0;

static int	gaussian_table_initialized = FALSE,
		uniform_table_initialized  = FALSE;


/*-------------------------------------------------------------------*/
int fast_noise( noise_buffer, num_noise_pts, frame_size) 

        /*  On entry,  noise_buffer points to a buffer large enough  to
        contain  num_noise_pts  short integers.    This  routine  fills 
        that  buffer with Gaussian white noise,   varying  between  +/- 
        MAX_DA.    The argument frame_size is the number of shorts  per 
        frame  and must be > 0.   If frame_size = 1,  every element  of 
        noise_buffer  is  filled;  if frame_size  =  2,  only  elements 
        0,2,4...  are filled; if frame_size = 3, only elements 0,3,6... 
        are  filled,  etc.   For generating 2-channel interlaced noise, 
        call  this  routine with frame_size=2 and then copy  the  even-
        indexed values  to  the  odd-indexed locations,  shifting   and 
        interpolating  as  necessary to create the  desired  interaural 
        time difference.  The sequence of values is uniquely determined
        by  the  arguments to the routines  init_random_sequence()  and 
        init_gaussian_table(), both of which must be called before this 
        routine  can  be  used.   If  either  of  these  initialization 
        routines  has  not  been called,  this routine gives  an  error 
        message  and  returns  TRUE without changing  the  contents  of 
        noise_buffer.  Returns FALSE if no error occurs. */

    short        * noise_buffer;
    Int4u	   num_noise_pts;

    register unsigned int frame_size;
    

{
                                       /*----- functions called -----*/

                                       /*----- extern variables -----*/
    extern Int4u gaussian_table[];
    extern Int4u uniform_deviate;
                                       /*----- local  variables -----*/
    int		   error;
    register Int4  low, 
                   high, 
                   middle; 
    register Int4u target,
		   uniform_i;

    register short * noise_ptr,
		   * noise_end;

                                       /*----- start function -------*/

    if ( error = (frame_size<=0) ) {
	fprintf(stderr,
	"fast_noise: frame_size must be > 0\n");
	return(error);
    }

    if (error = !gaussian_table_initialized) {
	fprintf(stderr,
	"fast_noise: init_gaussian_table() must be called first\n");
	return(error);
    }

    if (error = !uniform_table_initialized) {
	fprintf(stderr,
	"fast_noise: init_random_sequence() must be called first\n");
	return(error);
    }

    for( noise_ptr =  noise_buffer,
         noise_end =  noise_buffer + num_noise_pts;
         noise_ptr <  noise_end;
         noise_ptr += frame_size ) {

        low    = 0;
        high   = GAUSSIAN_TABLE_SIZE;
        middle = GAUSSIAN_TABLE_SIZE >> 1;

	uniform_i = (UNIFORM_TABLE_SIZE-1) 
		    * uniform_deviate / MAX_RAND;

        target = uniform_table[ uniform_i ];
	uniform_table[ uniform_i ] = random();

        while (high-low > 1) {
            if (target > gaussian_table[middle])
                low = middle;
            else
                high = middle;
            middle = (high + low) >> 1;
        }

        *noise_ptr = (short) ((target & 1)? -low : low);
    }
    return(error);
}

/*-------------------------------------------------------------------*/
int float_noise( noise_buffer, num_noise_pts, frame_size) 

        /*  This routine is identical in every way to fast_noise(),
	except that noise_buffer is an array of floats and the noise
	values stored in it vary from -1.0 to +1.0.  Although converted
	to floating point, the noise values are still discrete.  The
	total number of unique values is 2*MAX_DA + 1. */

    float        * noise_buffer;
    Int4u	   num_noise_pts;

    register unsigned int frame_size;
    

{
                                       /*----- functions called -----*/

                                       /*----- extern variables -----*/
    extern Int4u gaussian_table[];
    extern Int4u uniform_deviate;
                                       /*----- local  variables -----*/
    int		   error;
    register Int4  low, 
                   high, 
                   middle; 
    register Int4u target,
		   uniform_i;

    register float * noise_ptr,
		   * noise_end;

                                       /*----- start function -------*/

    if ( error = (frame_size<=0) ) {
	fprintf(stderr,
	"float_noise: frame_size must be > 0\n");
	return(error);
    }

    if (error = !gaussian_table_initialized) {
	fprintf(stderr,
	"float_noise: init_gaussian_table() must be called first\n");
	return(error);
    }

    if (error = !uniform_table_initialized) {
	fprintf(stderr,
	"float_noise: init_random_sequence() must be called first\n");
	return(error);
    }

    for( noise_ptr =  noise_buffer,
         noise_end =  noise_buffer + num_noise_pts;
         noise_ptr <  noise_end;
         noise_ptr += frame_size ) {

        low    = 0;
        high   = GAUSSIAN_TABLE_SIZE;
        middle = GAUSSIAN_TABLE_SIZE >> 1;

	uniform_i = (UNIFORM_TABLE_SIZE-1) 
		    * uniform_deviate / MAX_RAND;

        target = uniform_table[ uniform_i ];
	uniform_table[ uniform_i ] = random();

        while (high-low > 1) {
            if (target > gaussian_table[middle])
                low = middle;
            else
                high = middle;
            middle = (high + low) >> 1;
        }

        *noise_ptr = ((float) ((target & 1)? -low : low)) / MAX_DA;
    }
    return(error);
}



/*-------------------------------------------------------------------*/
Int4 max_seed()

	/* Returns the largest valid seed argument for use with 
	init_random_sequence(). */

{
    return(MAX_RAND);
}


/*-------------------------------------------------------------------*/
void init_random_sequence( seed_ptr )

        /* Initializes uniform_table[] using *seed_ptr as a seed if it 
        is between 0 and and the value returned by max_seed(), 
	inclusive.  If *seed_ptr is out of this range, the routine 
	generates a seed using the time(2) system call and copies 
	that seed to *seed_ptr.  */

    Int4      * seed_ptr;

{
                                       /*----- functions called -----*/

                                       /*----- extern variables -----*/

    extern Int4u	uniform_table[],
			uniform_deviate;

    extern int		uniform_table_initialized;

                                       /*----- local  variables -----*/
    long  time_value;
    Int4u low_8,
          low_24;
    int   i;
                                       /*----- start function -------*/

    if (*seed_ptr < 0 || *seed_ptr > MAX_RAND) {
	time(&time_value);
	low_8  = time_value & 0xFF;
	low_24 = time_value & 0xFFFFFF;
	*seed_ptr = ((unsigned)(low_24 | (low_8<<24))) % IM;
    }

    uniform_deviate = *seed_ptr;

    for( i=0; i < UNIFORM_TABLE_SIZE; i++ )
	random();

    for( i=0; i < UNIFORM_TABLE_SIZE; i++ ) 
	uniform_table[i] = random();

    random();

    uniform_table_initialized = TRUE;
    
}
 
/*-------------------------------------------------------------------*/
int init_gaussian_table( width_in_sigmas )

        /*   Initializes   noise  table for  generation   of   Gaussian 
        noise.  Argument width_in_sigmas gives the half-width (from 
	mean to the extreme value of one tail) of the bell curve  used,
	expressed as the number of  standard  deviations.
        This  routine  need  only  be called once  by  the  application 
        program.   The  routine fast_noise() can then be used  as  many 
        times  as  desired,  without calling this routine,  as long  as 
        there  is  no  need  to change the width  of  the  bell  curve.
        Returns   TRUE,    without   initializing   the    table,    if 
        width_in_sigmas is not in the useable range;  otherwise returns 
        FALSE. */

    double width_in_sigmas;

{
                                       /*----- functions called -----*/
    double exp();
                                       /*----- extern variables -----*/
    extern Int4u gaussian_table[];
    extern int	 gaussian_table_initialized;

                                       /*----- local  variables -----*/
    int 	error,
		i;
    double	bell_curve_integral[ GAUSSIAN_TABLE_SIZE ],
		sum,
		x;
                                       /*----- start function -------*/

    gaussian_table_initialized = FALSE;

    if (error = (width_in_sigmas <= 0.0 
		 || 
		 width_in_sigmas > MAX_WIDTH_IN_SIGMAS )) {

	fprintf(stderr,
	"init_gaussian_table: width_in_sigmas must be >0 and <=%g\n",
	MAX_WIDTH_IN_SIGMAS);
	return(error);
    }

    bell_curve_integral[ 0 ] = 0.0;
    bell_curve_integral[ 1 ] = 0.5;
    for (i = 2; i < GAUSSIAN_TABLE_SIZE; i++) {
	x = (i-1) * width_in_sigmas / MAX_DA;
        bell_curve_integral[i] = exp(-x*x/2.0) + 
	bell_curve_integral[i-1];
    }
    sum = bell_curve_integral[ GAUSSIAN_TABLE_SIZE-1 ];

    for (i = 1; i< GAUSSIAN_TABLE_SIZE; i++)
        bell_curve_integral[i] /= sum;

    for (i = 0; i< GAUSSIAN_TABLE_SIZE; i++)
        gaussian_table[i] = 
	(Int4u) (MAX_RAND * bell_curve_integral[i] + 0.5);

    if (error = ( gaussian_table[GAUSSIAN_TABLE_SIZE-1]
        	  <=
        	  gaussian_table[GAUSSIAN_TABLE_SIZE-2])) {
	fprintf(stderr,
	"init_gaussian_table: use a smaller width_in_sigmas\n");
	return(error);
    }

    gaussian_table_initialized = TRUE;

    return(error);
}


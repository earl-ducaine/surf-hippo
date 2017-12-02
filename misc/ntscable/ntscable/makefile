#######################################
#    %W%
#
#    Makefile for ntscable program    
#                                    
#      revised:  %G%
#                                   
#######################################

# subdir containing "toolbox" source code and obj files
T	= ../tools


CC	= cc

#CFLAGS	= -g -I$T -L$T
CFLAGS	= -O2 -I$T -L$T
LIBS	= -ljw -lm


OBJECTS	=	ntscable.o cmdline.o diagrams.o verify.o\
		read.o read_nts.o read_rd2.o read_rd3.o read_rn.o\
		xlate.o\
		surf-hippo-write.o
#		write.o

H	=	ntscable.h diagrams.h

ntscable:	$(OBJECTS)
		$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o ntscable

ntscable.o:	$H $T/tf.h
		$(CC) -c $(CFLAGS) ntscable.c

cmdline.o:	$H $T/tf.h $T/scf.h $T/sccstools.h $T/jwmath.h
		$(CC) -c $(CFLAGS) cmdline.c

read.o:		ntscable.h $T/tf.h $T/sets.h
		$(CC) -c $(CFLAGS) read.c

read_nts.o:	ntscable.h $T/tf.h $T/sets.h
		$(CC) -c $(CFLAGS) read_nts.c

read_rd2.o:	ntscable.h $T/tf.h $T/sets.h
		$(CC) -c $(CFLAGS) read_rd2.c

read_rd3.o:	ntscable.h $T/tf.h $T/sets.h
		$(CC) -c $(CFLAGS) read_rd3.c

read_rn.o:	ntscable.h $T/tf.h $T/sets.h
		$(CC) -c $(CFLAGS) read_rn.c

xlate.o:	ntscable.h $T/tf.h
		$(CC) -c $(CFLAGS) xlate.c

surf-hippo-write.o:	ntscable.h $T/tf.h
		$(CC) -c $(CFLAGS) surf-hippo-write.c
#		$(CC) -c $(CFLAGS) write.c



diagrams.o:	$H $T/jwmath.h $T/tf.h $T/figtools.h
		$(CC) -c $(CFLAGS) diagrams.c

verify.o:	$H $T/tf.h
		$(CC) -c $(CFLAGS) verify.c

clean:
	rm -f *.o


# Sample code for cshrc initializations, to be added to your .cshrc
# file. This example assumes that Surf-Hippo is installed in the
# directory /usr/local/surf-hippo.

setenv SURFHOME /usr/local/surf-hippo
setenv SURFUSERHOME /usr/local/surf-hippo

# This if for the Lisp.

setenv CMUCL_EMPTYFILE /tmp/empty

# (From the Garnet instructions)
# Lisp requires very large address spaces.  We have found on many Unix
# systems, that you need to expand the area that it is willing to give
# to a process.  The following commands work in many systems.

unlimit datasize
unlimit stacksize



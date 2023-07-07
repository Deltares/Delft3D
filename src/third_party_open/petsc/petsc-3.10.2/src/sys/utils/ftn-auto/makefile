
#requiresdefine   'PETSC_HAVE_FORTRAN'
ALL: lib

CFLAGS   =
FFLAGS   =
SOURCEC  = sortif.c mpiuf.c psleepf.c mpitsf.c pbarrierf.c memcf.c psplitf.c sortdf.c sortipf.c
SOURCEF  =
SOURCEH  = 
DIRS     =
LIBBASE	  = libpetscsys
LOCDIR	  = src/sys/utils/ftn-auto/
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules
include ${PETSC_DIR}/lib/petsc/conf/test

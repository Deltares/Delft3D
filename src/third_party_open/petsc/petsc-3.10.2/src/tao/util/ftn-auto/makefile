
#requiresdefine   'PETSC_HAVE_FORTRAN'
ALL: lib
CPPFLAGS = -DTAO_LIB_DIR='"${TAO_LIB_DIR}"'
CFLAGS   =
FFLAGS   =
SOURCEC  = tao_utilf.c
SOURCEF  =
SOURCEH  = 
DIRS     =
LIBBASE  = libpetsctao
LOCDIR   = src/tao/util/ftn-auto/
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules
include ${PETSC_DIR}/lib/petsc/conf/test

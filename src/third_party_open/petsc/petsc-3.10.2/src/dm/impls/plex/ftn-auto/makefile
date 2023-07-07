
#requiresdefine   'PETSC_HAVE_FORTRAN'
ALL: lib
CPPFLAGS = ${NETCFD_INCLUDE} ${EXODUSII_INCLUDE}
CFLAGS   =
FFLAGS   =
SOURCEC  = plexdistributef.c plextreef.c plexreorderf.c plexcgnsf.c plexinterpolatef.c plexfvmf.c plexgeometryf.c plexf.c plexcreatef.c plexnaturalf.c plexgmshf.c plexfemf.c plexrefinef.c plexpointf.c plexexodusiif.c plexsubmeshf.c plexorientf.c plexindicesf.c plexpartitionf.c
SOURCEF  =
SOURCEH  = 
DIRS     =
LIBBASE  = libpetscdm
LOCDIR   = src/dm/impls/plex/ftn-auto/
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules
include ${PETSC_DIR}/lib/petsc/conf/test

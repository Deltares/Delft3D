#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* misk.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsenmisksetdistance_ MATCOARSENMISKSETDISTANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsenmisksetdistance_ matcoarsenmisksetdistance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcoarsenmiskgetdistance_ MATCOARSENMISKGETDISTANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcoarsenmiskgetdistance_ matcoarsenmiskgetdistance
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcoarsenmisksetdistance_(MatCoarsen crs,PetscInt *k, int *__ierr)
{
*__ierr = MatCoarsenMISKSetDistance(
	(MatCoarsen)PetscToPointer((crs) ),*k);
}
PETSC_EXTERN void  matcoarsenmiskgetdistance_(MatCoarsen crs,PetscInt *k, int *__ierr)
{
*__ierr = MatCoarsenMISKGetDistance(
	(MatCoarsen)PetscToPointer((crs) ),k);
}
#if defined(__cplusplus)
}
#endif

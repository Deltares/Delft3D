#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexproject.c */
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

#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetactivepoint_ DMPLEXGETACTIVEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetactivepoint_ dmplexgetactivepoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetactivepoint_ DMPLEXSETACTIVEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetactivepoint_ dmplexsetactivepoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexgetactivepoint_(DM dm,PetscInt *point, int *__ierr)
{
*__ierr = DMPlexGetActivePoint(
	(DM)PetscToPointer((dm) ),point);
}
PETSC_EXTERN void  dmplexsetactivepoint_(DM dm,PetscInt *point, int *__ierr)
{
*__ierr = DMPlexSetActivePoint(
	(DM)PetscToPointer((dm) ),*point);
}
#if defined(__cplusplus)
}
#endif

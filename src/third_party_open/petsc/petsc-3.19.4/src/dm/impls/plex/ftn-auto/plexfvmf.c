#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexfvm.c */
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
#define dmplexreconstructgradientsfvm_ DMPLEXRECONSTRUCTGRADIENTSFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreconstructgradientsfvm_ dmplexreconstructgradientsfvm
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexreconstructgradientsfvm_(DM dm,Vec locX,Vec grad, int *__ierr)
{
*__ierr = DMPlexReconstructGradientsFVM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((grad) ));
}
#if defined(__cplusplus)
}
#endif

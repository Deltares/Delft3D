#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexreorder.c */
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
#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetordering_ DMPLEXGETORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetordering_ dmplexgetordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpermute_ DMPLEXPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpermute_ dmplexpermute
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexgetordering_(DM dm,MatOrderingType *otype,DMLabel label,IS *perm, int *__ierr){
*__ierr = DMPlexGetOrdering(
	(DM)PetscToPointer((dm) ),*otype,
	(DMLabel)PetscToPointer((label) ),perm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpermute_(DM dm,IS perm,DM *pdm, int *__ierr){
*__ierr = DMPlexPermute(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((perm) ),pdm);
}
#if defined(__cplusplus)
}
#endif

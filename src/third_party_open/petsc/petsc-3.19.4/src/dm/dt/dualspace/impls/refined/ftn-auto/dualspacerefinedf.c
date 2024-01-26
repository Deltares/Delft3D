#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dualspacerefined.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacerefinedsetcellspaces_ PETSCDUALSPACEREFINEDSETCELLSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacerefinedsetcellspaces_ petscdualspacerefinedsetcellspaces
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdualspacerefinedsetcellspaces_(PetscDualSpace sp, PetscDualSpace cellSpaces[], int *__ierr)
{
*__ierr = PetscDualSpaceRefinedSetCellSpaces(
	(PetscDualSpace)PetscToPointer((sp) ),cellSpaces);
}
#if defined(__cplusplus)
}
#endif

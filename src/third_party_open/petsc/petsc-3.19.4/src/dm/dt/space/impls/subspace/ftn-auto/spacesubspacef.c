#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* spacesubspace.c */
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
#define petscspacecreatesubspace_ PETSCSPACECREATESUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacecreatesubspace_ petscspacecreatesubspace
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspacecreatesubspace_(PetscSpace origSpace,PetscDualSpace dualSubspace,PetscReal *x,PetscReal *Jx,PetscReal *u,PetscReal *Ju,PetscCopyMode *copymode,PetscSpace *subspace, int *__ierr)
{
*__ierr = PetscSpaceCreateSubspace(
	(PetscSpace)PetscToPointer((origSpace) ),
	(PetscDualSpace)PetscToPointer((dualSubspace) ),x,Jx,u,Ju,*copymode,subspace);
}
#if defined(__cplusplus)
}
#endif

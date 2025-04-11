#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dagtona.c */
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

#include "petscdmda.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdaglobaltonaturalallcreate_ DMDAGLOBALTONATURALALLCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdaglobaltonaturalallcreate_ dmdaglobaltonaturalallcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdanaturalalltoglobalcreate_ DMDANATURALALLTOGLOBALCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdanaturalalltoglobalcreate_ dmdanaturalalltoglobalcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdaglobaltonaturalallcreate_(DM da,VecScatter *scatter, int *__ierr)
{
*__ierr = DMDAGlobalToNaturalAllCreate(
	(DM)PetscToPointer((da) ),scatter);
}
PETSC_EXTERN void  dmdanaturalalltoglobalcreate_(DM da,VecScatter *scatter, int *__ierr)
{
*__ierr = DMDANaturalAllToGlobalCreate(
	(DM)PetscToPointer((da) ),scatter);
}
#if defined(__cplusplus)
}
#endif

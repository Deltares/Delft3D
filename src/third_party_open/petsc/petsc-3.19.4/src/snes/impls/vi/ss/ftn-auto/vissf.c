#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* viss.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesvicomputemeritfunction_ SNESVICOMPUTEMERITFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesvicomputemeritfunction_ snesvicomputemeritfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesvicomputefunction_ SNESVICOMPUTEFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesvicomputefunction_ snesvicomputefunction
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesvicomputemeritfunction_(Vec phi,PetscReal *merit,PetscReal *phinorm, int *__ierr)
{
*__ierr = SNESVIComputeMeritFunction(
	(Vec)PetscToPointer((phi) ),merit,phinorm);
}
PETSC_EXTERN void  snesvicomputefunction_(SNES snes,Vec X,Vec phi,void*functx, int *__ierr)
{
*__ierr = SNESVIComputeFunction(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((phi) ),functx);
}
#if defined(__cplusplus)
}
#endif

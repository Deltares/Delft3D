#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* cholesky.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcfactorsetreuseordering_ PCFACTORSETREUSEORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcfactorsetreuseordering_ pcfactorsetreuseordering
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcfactorsetreuseordering_(PC pc,PetscBool *flag, int *__ierr)
{
*__ierr = PCFactorSetReuseOrdering(
	(PC)PetscToPointer((pc) ),*flag);
}
#if defined(__cplusplus)
}
#endif

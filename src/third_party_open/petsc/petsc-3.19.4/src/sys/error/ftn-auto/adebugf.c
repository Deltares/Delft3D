#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* adebug.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscwaitonerror_ PETSCWAITONERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscwaitonerror_ petscwaitonerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscattachdebugger_ PETSCATTACHDEBUGGER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscattachdebugger_ petscattachdebugger
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscwaitonerror_(int *__ierr)
{
*__ierr = PetscWaitOnError();
}
PETSC_EXTERN void  petscattachdebugger_(int *__ierr)
{
*__ierr = PetscAttachDebugger();
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mem.c */
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
#define petscmemorygetcurrentusage_ PETSCMEMORYGETCURRENTUSAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmemorygetcurrentusage_ petscmemorygetcurrentusage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmemorygetmaximumusage_ PETSCMEMORYGETMAXIMUMUSAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmemorygetmaximumusage_ petscmemorygetmaximumusage
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmemorysetgetmaximumusage_ PETSCMEMORYSETGETMAXIMUMUSAGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmemorysetgetmaximumusage_ petscmemorysetgetmaximumusage
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscmemorygetcurrentusage_(PetscLogDouble *mem, int *__ierr)
{
*__ierr = PetscMemoryGetCurrentUsage(mem);
}
PETSC_EXTERN void  petscmemorygetmaximumusage_(PetscLogDouble *mem, int *__ierr)
{
*__ierr = PetscMemoryGetMaximumUsage(mem);
}
PETSC_EXTERN void  petscmemorysetgetmaximumusage_(int *__ierr)
{
*__ierr = PetscMemorySetGetMaximumUsage();
}
#if defined(__cplusplus)
}
#endif

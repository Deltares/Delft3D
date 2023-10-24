#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fp.c */
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
#define petscfptrappush_ PETSCFPTRAPPUSH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfptrappush_ petscfptrappush
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfptrappop_ PETSCFPTRAPPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfptrappop_ petscfptrappop
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsetfptrap_ PETSCSETFPTRAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsetfptrap_ petscsetfptrap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdetermineinitialfptrap_ PETSCDETERMINEINITIALFPTRAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdetermineinitialfptrap_ petscdetermineinitialfptrap
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscfptrappush_(PetscFPTrap *trap, int *__ierr)
{
*__ierr = PetscFPTrapPush(*trap);
}
PETSC_EXTERN void  petscfptrappop_(int *__ierr)
{
*__ierr = PetscFPTrapPop();
}
PETSC_EXTERN void  petscsetfptrap_(PetscFPTrap *flag, int *__ierr)
{
*__ierr = PetscSetFPTrap(*flag);
}
PETSC_EXTERN void  petscdetermineinitialfptrap_(int *__ierr)
{
*__ierr = PetscDetermineInitialFPTrap();
}
#if defined(__cplusplus)
}
#endif

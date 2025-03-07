#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* signal.c */
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
#define petscsignalhandlerdefault_ PETSCSIGNALHANDLERDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsignalhandlerdefault_ petscsignalhandlerdefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpopsignalhandler_ PETSCPOPSIGNALHANDLER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpopsignalhandler_ petscpopsignalhandler
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsignalhandlerdefault_(int *sig,void*ptr, int *__ierr)
{
*__ierr = PetscSignalHandlerDefault(*sig,ptr);
}
PETSC_EXTERN void  petscpopsignalhandler_(int *__ierr)
{
*__ierr = PetscPopSignalHandler();
}
#if defined(__cplusplus)
}
#endif

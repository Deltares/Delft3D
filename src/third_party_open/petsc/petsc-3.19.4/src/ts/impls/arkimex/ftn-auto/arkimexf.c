#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* arkimex.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsarkimexsetfullyimplicit_ TSARKIMEXSETFULLYIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsarkimexsetfullyimplicit_ tsarkimexsetfullyimplicit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsarkimexgetfullyimplicit_ TSARKIMEXGETFULLYIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsarkimexgetfullyimplicit_ tsarkimexgetfullyimplicit
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsarkimexsetfullyimplicit_(TS ts,PetscBool *flg, int *__ierr)
{
*__ierr = TSARKIMEXSetFullyImplicit(
	(TS)PetscToPointer((ts) ),*flg);
}
PETSC_EXTERN void  tsarkimexgetfullyimplicit_(TS ts,PetscBool *flg, int *__ierr)
{
*__ierr = TSARKIMEXGetFullyImplicit(
	(TS)PetscToPointer((ts) ),flg);
}
#if defined(__cplusplus)
}
#endif

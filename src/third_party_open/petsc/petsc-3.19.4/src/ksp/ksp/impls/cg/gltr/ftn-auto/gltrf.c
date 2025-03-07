#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gltr.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgltrgetmineig_ KSPGLTRGETMINEIG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgltrgetmineig_ kspgltrgetmineig
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgltrgetlambda_ KSPGLTRGETLAMBDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgltrgetlambda_ kspgltrgetlambda
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspgltrgetmineig_(KSP ksp,PetscReal *e_min, int *__ierr)
{
*__ierr = KSPGLTRGetMinEig(
	(KSP)PetscToPointer((ksp) ),e_min);
}
PETSC_EXTERN void  kspgltrgetlambda_(KSP ksp,PetscReal *lambda, int *__ierr)
{
*__ierr = KSPGLTRGetLambda(
	(KSP)PetscToPointer((ksp) ),lambda);
}
#if defined(__cplusplus)
}
#endif

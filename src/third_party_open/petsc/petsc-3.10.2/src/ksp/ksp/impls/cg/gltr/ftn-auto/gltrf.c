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
#define kspcggltrgetmineig_ KSPCGGLTRGETMINEIG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcggltrgetmineig_ kspcggltrgetmineig
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcggltrgetlambda_ KSPCGGLTRGETLAMBDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcggltrgetlambda_ kspcggltrgetlambda
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  kspcggltrgetmineig_(KSP ksp,PetscReal *e_min, int *__ierr){
*__ierr = KSPCGGLTRGetMinEig(
	(KSP)PetscToPointer((ksp) ),e_min);
}
PETSC_EXTERN void PETSC_STDCALL  kspcggltrgetlambda_(KSP ksp,PetscReal *lambda, int *__ierr){
*__ierr = KSPCGGLTRGetLambda(
	(KSP)PetscToPointer((ksp) ),lambda);
}
#if defined(__cplusplus)
}
#endif

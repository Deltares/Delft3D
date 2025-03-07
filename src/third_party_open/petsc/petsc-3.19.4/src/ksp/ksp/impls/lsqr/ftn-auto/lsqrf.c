#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* lsqr.c */
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
#define ksplsqrsetcomputestandarderrorvec_ KSPLSQRSETCOMPUTESTANDARDERRORVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksplsqrsetcomputestandarderrorvec_ ksplsqrsetcomputestandarderrorvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksplsqrsetexactmatnorm_ KSPLSQRSETEXACTMATNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksplsqrsetexactmatnorm_ ksplsqrsetexactmatnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksplsqrgetstandarderrorvec_ KSPLSQRGETSTANDARDERRORVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksplsqrgetstandarderrorvec_ ksplsqrgetstandarderrorvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksplsqrgetnorms_ KSPLSQRGETNORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksplsqrgetnorms_ ksplsqrgetnorms
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  ksplsqrsetcomputestandarderrorvec_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPLSQRSetComputeStandardErrorVec(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  ksplsqrsetexactmatnorm_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPLSQRSetExactMatNorm(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  ksplsqrgetstandarderrorvec_(KSP ksp,Vec *se, int *__ierr)
{
*__ierr = KSPLSQRGetStandardErrorVec(
	(KSP)PetscToPointer((ksp) ),se);
}
PETSC_EXTERN void  ksplsqrgetnorms_(KSP ksp,PetscReal *arnorm,PetscReal *anorm, int *__ierr)
{
*__ierr = KSPLSQRGetNorms(
	(KSP)PetscToPointer((ksp) ),arnorm,anorm);
}
#if defined(__cplusplus)
}
#endif

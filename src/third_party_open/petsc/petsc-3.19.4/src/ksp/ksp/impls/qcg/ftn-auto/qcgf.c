#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* qcg.c */
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
#define kspqcgsettrustregionradius_ KSPQCGSETTRUSTREGIONRADIUS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspqcgsettrustregionradius_ kspqcgsettrustregionradius
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspqcggettrialstepnorm_ KSPQCGGETTRIALSTEPNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspqcggettrialstepnorm_ kspqcggettrialstepnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspqcggetquadratic_ KSPQCGGETQUADRATIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspqcggetquadratic_ kspqcggetquadratic
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspqcgsettrustregionradius_(KSP ksp,PetscReal *delta, int *__ierr)
{
*__ierr = KSPQCGSetTrustRegionRadius(
	(KSP)PetscToPointer((ksp) ),*delta);
}
PETSC_EXTERN void  kspqcggettrialstepnorm_(KSP ksp,PetscReal *tsnorm, int *__ierr)
{
*__ierr = KSPQCGGetTrialStepNorm(
	(KSP)PetscToPointer((ksp) ),tsnorm);
}
PETSC_EXTERN void  kspqcggetquadratic_(KSP ksp,PetscReal *quadratic, int *__ierr)
{
*__ierr = KSPQCGGetQuadratic(
	(KSP)PetscToPointer((ksp) ),quadratic);
}
#if defined(__cplusplus)
}
#endif

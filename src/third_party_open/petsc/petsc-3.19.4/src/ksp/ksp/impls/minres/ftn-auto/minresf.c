#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* minres.c */
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
#define kspminressetuseqlp_ KSPMINRESSETUSEQLP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspminressetuseqlp_ kspminressetuseqlp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspminressetradius_ KSPMINRESSETRADIUS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspminressetradius_ kspminressetradius
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspminresgetuseqlp_ KSPMINRESGETUSEQLP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspminresgetuseqlp_ kspminresgetuseqlp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspminressetuseqlp_(KSP ksp,PetscBool *qlp, int *__ierr)
{
*__ierr = KSPMINRESSetUseQLP(
	(KSP)PetscToPointer((ksp) ),*qlp);
}
PETSC_EXTERN void  kspminressetradius_(KSP ksp,PetscReal *radius, int *__ierr)
{
*__ierr = KSPMINRESSetRadius(
	(KSP)PetscToPointer((ksp) ),*radius);
}
PETSC_EXTERN void  kspminresgetuseqlp_(KSP ksp,PetscBool *qlp, int *__ierr)
{
*__ierr = KSPMINRESGetUseQLP(
	(KSP)PetscToPointer((ksp) ),qlp);
}
#if defined(__cplusplus)
}
#endif

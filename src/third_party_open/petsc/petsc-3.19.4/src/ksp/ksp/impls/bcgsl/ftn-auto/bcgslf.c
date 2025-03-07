#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bcgsl.c */
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
#define kspbcgslsetxres_ KSPBCGSLSETXRES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspbcgslsetxres_ kspbcgslsetxres
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspbcgslsetusepseudoinverse_ KSPBCGSLSETUSEPSEUDOINVERSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspbcgslsetusepseudoinverse_ kspbcgslsetusepseudoinverse
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspbcgslsetpol_ KSPBCGSLSETPOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspbcgslsetpol_ kspbcgslsetpol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspbcgslsetell_ KSPBCGSLSETELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspbcgslsetell_ kspbcgslsetell
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspbcgslsetxres_(KSP ksp,PetscReal *delta, int *__ierr)
{
*__ierr = KSPBCGSLSetXRes(
	(KSP)PetscToPointer((ksp) ),*delta);
}
PETSC_EXTERN void  kspbcgslsetusepseudoinverse_(KSP ksp,PetscBool *use_pinv, int *__ierr)
{
*__ierr = KSPBCGSLSetUsePseudoinverse(
	(KSP)PetscToPointer((ksp) ),*use_pinv);
}
PETSC_EXTERN void  kspbcgslsetpol_(KSP ksp,PetscBool *uMROR, int *__ierr)
{
*__ierr = KSPBCGSLSetPol(
	(KSP)PetscToPointer((ksp) ),*uMROR);
}
PETSC_EXTERN void  kspbcgslsetell_(KSP ksp,PetscInt *ell, int *__ierr)
{
*__ierr = KSPBCGSLSetEll(
	(KSP)PetscToPointer((ksp) ),*ell);
}
#if defined(__cplusplus)
}
#endif

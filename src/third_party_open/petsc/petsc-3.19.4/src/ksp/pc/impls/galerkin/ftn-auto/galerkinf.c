#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* galerkin.c */
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
#define pcgalerkinsetrestriction_ PCGALERKINSETRESTRICTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgalerkinsetrestriction_ pcgalerkinsetrestriction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgalerkinsetinterpolation_ PCGALERKINSETINTERPOLATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgalerkinsetinterpolation_ pcgalerkinsetinterpolation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgalerkinsetcomputesubmatrix_ PCGALERKINSETCOMPUTESUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgalerkinsetcomputesubmatrix_ pcgalerkinsetcomputesubmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgalerkingetksp_ PCGALERKINGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgalerkingetksp_ pcgalerkingetksp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcgalerkinsetrestriction_(PC pc,Mat R, int *__ierr)
{
*__ierr = PCGalerkinSetRestriction(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((R) ));
}
PETSC_EXTERN void  pcgalerkinsetinterpolation_(PC pc,Mat P, int *__ierr)
{
*__ierr = PCGalerkinSetInterpolation(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((P) ));
}
PETSC_EXTERN void  pcgalerkinsetcomputesubmatrix_(PC pc,PetscErrorCode (*computeAsub)(PC, Mat, Mat, Mat *, void *),void*ctx, int *__ierr)
{
*__ierr = PCGalerkinSetComputeSubmatrix(
	(PC)PetscToPointer((pc) ),computeAsub,ctx);
}
PETSC_EXTERN void  pcgalerkingetksp_(PC pc,KSP *ksp, int *__ierr)
{
*__ierr = PCGalerkinGetKSP(
	(PC)PetscToPointer((pc) ),ksp);
}
#if defined(__cplusplus)
}
#endif

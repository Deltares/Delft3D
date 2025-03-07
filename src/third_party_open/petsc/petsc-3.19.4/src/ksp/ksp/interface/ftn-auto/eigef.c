#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* eige.c */
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
#define kspcomputeoperator_ KSPCOMPUTEOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcomputeoperator_ kspcomputeoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcomputeeigenvaluesexplicitly_ KSPCOMPUTEEIGENVALUESEXPLICITLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcomputeeigenvaluesexplicitly_ kspcomputeeigenvaluesexplicitly
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspcomputeoperator_(KSP ksp,MatType *mattype,Mat *mat, int *__ierr)
{
*__ierr = KSPComputeOperator(
	(KSP)PetscToPointer((ksp) ),*mattype,mat);
}
PETSC_EXTERN void  kspcomputeeigenvaluesexplicitly_(KSP ksp,PetscInt *nmax,PetscReal r[],PetscReal c[], int *__ierr)
{
*__ierr = KSPComputeEigenvaluesExplicitly(
	(KSP)PetscToPointer((ksp) ),*nmax,r,c);
}
#if defined(__cplusplus)
}
#endif

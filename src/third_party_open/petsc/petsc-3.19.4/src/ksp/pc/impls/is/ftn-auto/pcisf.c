#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pcis.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetusestiffnessscaling_ PCISSETUSESTIFFNESSSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetusestiffnessscaling_ pcissetusestiffnessscaling
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetsubdomaindiagonalscaling_ PCISSETSUBDOMAINDIAGONALSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetsubdomaindiagonalscaling_ pcissetsubdomaindiagonalscaling
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcissetsubdomainscalingfactor_ PCISSETSUBDOMAINSCALINGFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcissetsubdomainscalingfactor_ pcissetsubdomainscalingfactor
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcissetusestiffnessscaling_(PC pc,PetscBool *use, int *__ierr)
{
*__ierr = PCISSetUseStiffnessScaling(
	(PC)PetscToPointer((pc) ),*use);
}
PETSC_EXTERN void  pcissetsubdomaindiagonalscaling_(PC pc,Vec scaling_factors, int *__ierr)
{
*__ierr = PCISSetSubdomainDiagonalScaling(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((scaling_factors) ));
}
PETSC_EXTERN void  pcissetsubdomainscalingfactor_(PC pc,PetscScalar *scal, int *__ierr)
{
*__ierr = PCISSetSubdomainScalingFactor(
	(PC)PetscToPointer((pc) ),*scal);
}
#if defined(__cplusplus)
}
#endif

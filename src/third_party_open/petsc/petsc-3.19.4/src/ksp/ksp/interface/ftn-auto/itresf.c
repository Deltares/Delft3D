#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* itres.c */
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
#define kspinitialresidual_ KSPINITIALRESIDUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspinitialresidual_ kspinitialresidual
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspunwindpreconditioner_ KSPUNWINDPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspunwindpreconditioner_ kspunwindpreconditioner
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

PETSC_EXTERN void  kspinitialresidual_(KSP ksp,Vec vsoln,Vec vt1,Vec vt2,Vec vres,Vec vb, int *__ierr)
{
*__ierr = KSPInitialResidual(
	(KSP)PetscToPointer((ksp) ),
	(Vec)PetscToPointer((vsoln) ),
	(Vec)PetscToPointer((vt1) ),
	(Vec)PetscToPointer((vt2) ),
	(Vec)PetscToPointer((vres) ),
	(Vec)PetscToPointer((vb) ));
}
PETSC_EXTERN void  kspunwindpreconditioner_(KSP ksp,Vec vsoln,Vec vt1, int *__ierr)
{
*__ierr = KSPUnwindPreconditioner(
	(KSP)PetscToPointer((ksp) ),
	(Vec)PetscToPointer((vsoln) ),
	(Vec)PetscToPointer((vt1) ));
}
#if defined(__cplusplus)
}
#endif

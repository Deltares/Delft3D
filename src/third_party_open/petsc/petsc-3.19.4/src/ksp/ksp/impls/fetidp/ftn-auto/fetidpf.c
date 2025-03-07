#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fetidp.c */
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

#include <petscksp.h>
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfetidpsetpressureoperator_ KSPFETIDPSETPRESSUREOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfetidpsetpressureoperator_ kspfetidpsetpressureoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfetidpgetinnerksp_ KSPFETIDPGETINNERKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfetidpgetinnerksp_ kspfetidpgetinnerksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfetidpgetinnerbddc_ KSPFETIDPGETINNERBDDC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfetidpgetinnerbddc_ kspfetidpgetinnerbddc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfetidpsetinnerbddc_ KSPFETIDPSETINNERBDDC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfetidpsetinnerbddc_ kspfetidpsetinnerbddc
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspfetidpsetpressureoperator_(KSP ksp,Mat P, int *__ierr)
{
*__ierr = KSPFETIDPSetPressureOperator(
	(KSP)PetscToPointer((ksp) ),
	(Mat)PetscToPointer((P) ));
}
PETSC_EXTERN void  kspfetidpgetinnerksp_(KSP ksp,KSP *innerksp, int *__ierr)
{
*__ierr = KSPFETIDPGetInnerKSP(
	(KSP)PetscToPointer((ksp) ),innerksp);
}
PETSC_EXTERN void  kspfetidpgetinnerbddc_(KSP ksp,PC *pc, int *__ierr)
{
*__ierr = KSPFETIDPGetInnerBDDC(
	(KSP)PetscToPointer((ksp) ),pc);
}
PETSC_EXTERN void  kspfetidpsetinnerbddc_(KSP ksp,PC pc, int *__ierr)
{
*__ierr = KSPFETIDPSetInnerBDDC(
	(KSP)PetscToPointer((ksp) ),
	(PC)PetscToPointer((pc) ));
}
#if defined(__cplusplus)
}
#endif

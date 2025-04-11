#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pcksp.c */
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
#define pckspsetksp_ PCKSPSETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pckspsetksp_ pckspsetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pckspgetksp_ PCKSPGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pckspgetksp_ pckspgetksp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pckspsetksp_(PC pc,KSP ksp, int *__ierr)
{
*__ierr = PCKSPSetKSP(
	(PC)PetscToPointer((pc) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  pckspgetksp_(PC pc,KSP *ksp, int *__ierr)
{
*__ierr = PCKSPGetKSP(
	(PC)PetscToPointer((pc) ),ksp);
}
#if defined(__cplusplus)
}
#endif

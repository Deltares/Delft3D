#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gcr.c */
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
#define kspgcrsetrestart_ KSPGCRSETRESTART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgcrsetrestart_ kspgcrsetrestart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgcrgetrestart_ KSPGCRGETRESTART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgcrgetrestart_ kspgcrgetrestart
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspgcrsetrestart_(KSP ksp,PetscInt *restart, int *__ierr)
{
*__ierr = KSPGCRSetRestart(
	(KSP)PetscToPointer((ksp) ),*restart);
}
PETSC_EXTERN void  kspgcrgetrestart_(KSP ksp,PetscInt *restart, int *__ierr)
{
*__ierr = KSPGCRGetRestart(
	(KSP)PetscToPointer((ksp) ),restart);
}
#if defined(__cplusplus)
}
#endif

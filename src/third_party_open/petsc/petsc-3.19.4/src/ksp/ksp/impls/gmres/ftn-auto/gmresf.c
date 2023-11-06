#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gmres.c */
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
#define kspgmressetcgsrefinementtype_ KSPGMRESSETCGSREFINEMENTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmressetcgsrefinementtype_ kspgmressetcgsrefinementtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgmresgetcgsrefinementtype_ KSPGMRESGETCGSREFINEMENTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmresgetcgsrefinementtype_ kspgmresgetcgsrefinementtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgmressetrestart_ KSPGMRESSETRESTART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmressetrestart_ kspgmressetrestart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgmresgetrestart_ KSPGMRESGETRESTART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmresgetrestart_ kspgmresgetrestart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgmressethaptol_ KSPGMRESSETHAPTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmressethaptol_ kspgmressethaptol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgmressetbreakdowntolerance_ KSPGMRESSETBREAKDOWNTOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgmressetbreakdowntolerance_ kspgmressetbreakdowntolerance
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspgmressetcgsrefinementtype_(KSP ksp,KSPGMRESCGSRefinementType *type, int *__ierr)
{
*__ierr = KSPGMRESSetCGSRefinementType(
	(KSP)PetscToPointer((ksp) ),*type);
}
PETSC_EXTERN void  kspgmresgetcgsrefinementtype_(KSP ksp,KSPGMRESCGSRefinementType *type, int *__ierr)
{
*__ierr = KSPGMRESGetCGSRefinementType(
	(KSP)PetscToPointer((ksp) ),type);
}
PETSC_EXTERN void  kspgmressetrestart_(KSP ksp,PetscInt *restart, int *__ierr)
{
*__ierr = KSPGMRESSetRestart(
	(KSP)PetscToPointer((ksp) ),*restart);
}
PETSC_EXTERN void  kspgmresgetrestart_(KSP ksp,PetscInt *restart, int *__ierr)
{
*__ierr = KSPGMRESGetRestart(
	(KSP)PetscToPointer((ksp) ),restart);
}
PETSC_EXTERN void  kspgmressethaptol_(KSP ksp,PetscReal *tol, int *__ierr)
{
*__ierr = KSPGMRESSetHapTol(
	(KSP)PetscToPointer((ksp) ),*tol);
}
PETSC_EXTERN void  kspgmressetbreakdowntolerance_(KSP ksp,PetscReal *tol, int *__ierr)
{
*__ierr = KSPGMRESSetBreakdownTolerance(
	(KSP)PetscToPointer((ksp) ),*tol);
}
#if defined(__cplusplus)
}
#endif

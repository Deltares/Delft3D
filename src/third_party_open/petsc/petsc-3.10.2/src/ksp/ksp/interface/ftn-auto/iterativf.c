#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* iterativ.c */
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
#define kspgetresidualnorm_ KSPGETRESIDUALNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetresidualnorm_ kspgetresidualnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetiterationnumber_ KSPGETITERATIONNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetiterationnumber_ kspgetiterationnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgettotaliterations_ KSPGETTOTALITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgettotaliterations_ kspgettotaliterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspconvergeddefaultsetuirnorm_ KSPCONVERGEDDEFAULTSETUIRNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspconvergeddefaultsetuirnorm_ kspconvergeddefaultsetuirnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspconvergeddefaultsetumirnorm_ KSPCONVERGEDDEFAULTSETUMIRNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspconvergeddefaultsetumirnorm_ kspconvergeddefaultsetumirnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetconvergedreason_ KSPGETCONVERGEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetconvergedreason_ kspgetconvergedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetdm_ KSPSETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetdm_ kspsetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetdmactive_ KSPSETDMACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetdmactive_ kspsetdmactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetdm_ KSPGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetdm_ kspgetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetapplicationcontext_ KSPSETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetapplicationcontext_ kspsetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetapplicationcontext_ KSPGETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetapplicationcontext_ kspgetapplicationcontext
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  kspgetresidualnorm_(KSP ksp,PetscReal *rnorm, int *__ierr){
*__ierr = KSPGetResidualNorm(
	(KSP)PetscToPointer((ksp) ),rnorm);
}
PETSC_EXTERN void PETSC_STDCALL  kspgetiterationnumber_(KSP ksp,PetscInt *its, int *__ierr){
*__ierr = KSPGetIterationNumber(
	(KSP)PetscToPointer((ksp) ),its);
}
PETSC_EXTERN void PETSC_STDCALL  kspgettotaliterations_(KSP ksp,PetscInt *its, int *__ierr){
*__ierr = KSPGetTotalIterations(
	(KSP)PetscToPointer((ksp) ),its);
}
PETSC_EXTERN void PETSC_STDCALL  kspconvergeddefaultsetuirnorm_(KSP ksp, int *__ierr){
*__ierr = KSPConvergedDefaultSetUIRNorm(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspconvergeddefaultsetumirnorm_(KSP ksp, int *__ierr){
*__ierr = KSPConvergedDefaultSetUMIRNorm(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspgetconvergedreason_(KSP ksp,KSPConvergedReason *reason, int *__ierr){
*__ierr = KSPGetConvergedReason(
	(KSP)PetscToPointer((ksp) ),reason);
}
PETSC_EXTERN void PETSC_STDCALL  kspsetdm_(KSP ksp,DM dm, int *__ierr){
*__ierr = KSPSetDM(
	(KSP)PetscToPointer((ksp) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  kspsetdmactive_(KSP ksp,PetscBool *flg, int *__ierr){
*__ierr = KSPSetDMActive(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  kspgetdm_(KSP ksp,DM *dm, int *__ierr){
*__ierr = KSPGetDM(
	(KSP)PetscToPointer((ksp) ),dm);
}
PETSC_EXTERN void PETSC_STDCALL  kspsetapplicationcontext_(KSP ksp,void*usrP, int *__ierr){
*__ierr = KSPSetApplicationContext(
	(KSP)PetscToPointer((ksp) ),usrP);
}
PETSC_EXTERN void PETSC_STDCALL  kspgetapplicationcontext_(KSP ksp,void*usrP, int *__ierr){
*__ierr = KSPGetApplicationContext(
	(KSP)PetscToPointer((ksp) ),usrP);
}
#if defined(__cplusplus)
}
#endif

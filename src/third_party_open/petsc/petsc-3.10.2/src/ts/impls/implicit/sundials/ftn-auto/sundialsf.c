#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sundials.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssettype_ TSSUNDIALSSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssettype_ tssundialssettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssetmaxl_ TSSUNDIALSSETMAXL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssetmaxl_ tssundialssetmaxl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssetlineartolerance_ TSSUNDIALSSETLINEARTOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssetlineartolerance_ tssundialssetlineartolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssetgramschmidttype_ TSSUNDIALSSETGRAMSCHMIDTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssetgramschmidttype_ tssundialssetgramschmidttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssettolerance_ TSSUNDIALSSETTOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssettolerance_ tssundialssettolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialsgetpc_ TSSUNDIALSGETPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialsgetpc_ tssundialsgetpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssetmintimestep_ TSSUNDIALSSETMINTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssetmintimestep_ tssundialssetmintimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialssetmaxtimestep_ TSSUNDIALSSETMAXTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialssetmaxtimestep_ tssundialssetmaxtimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssundialsmonitorinternalsteps_ TSSUNDIALSMONITORINTERNALSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssundialsmonitorinternalsteps_ tssundialsmonitorinternalsteps
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  tssundialssettype_(TS ts,TSSundialsLmmType *type, int *__ierr){
*__ierr = TSSundialsSetType(
	(TS)PetscToPointer((ts) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssetmaxl_(TS ts,PetscInt *maxl, int *__ierr){
*__ierr = TSSundialsSetMaxl(
	(TS)PetscToPointer((ts) ),*maxl);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssetlineartolerance_(TS ts,double *tol, int *__ierr){
*__ierr = TSSundialsSetLinearTolerance(
	(TS)PetscToPointer((ts) ),*tol);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssetgramschmidttype_(TS ts,TSSundialsGramSchmidtType *type, int *__ierr){
*__ierr = TSSundialsSetGramSchmidtType(
	(TS)PetscToPointer((ts) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssettolerance_(TS ts,double *aabs,double *rel, int *__ierr){
*__ierr = TSSundialsSetTolerance(
	(TS)PetscToPointer((ts) ),*aabs,*rel);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialsgetpc_(TS ts,PC *pc, int *__ierr){
*__ierr = TSSundialsGetPC(
	(TS)PetscToPointer((ts) ),pc);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssetmintimestep_(TS ts,PetscReal *mindt, int *__ierr){
*__ierr = TSSundialsSetMinTimeStep(
	(TS)PetscToPointer((ts) ),*mindt);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialssetmaxtimestep_(TS ts,PetscReal *maxdt, int *__ierr){
*__ierr = TSSundialsSetMaxTimeStep(
	(TS)PetscToPointer((ts) ),*maxdt);
}
PETSC_EXTERN void PETSC_STDCALL  tssundialsmonitorinternalsteps_(TS ts,PetscBool *ft, int *__ierr){
*__ierr = TSSundialsMonitorInternalSteps(
	(TS)PetscToPointer((ts) ),*ft);
}
#if defined(__cplusplus)
}
#endif

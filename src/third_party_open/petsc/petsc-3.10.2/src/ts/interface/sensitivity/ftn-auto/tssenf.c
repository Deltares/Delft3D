#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tssen.c */
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
#define tsgetcostintegral_ TSGETCOSTINTEGRAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetcostintegral_ tsgetcostintegral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputecostintegrand_ TSCOMPUTECOSTINTEGRAND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputecostintegrand_ tscomputecostintegrand
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputedrdyfunction_ TSCOMPUTEDRDYFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputedrdyfunction_ tscomputedrdyfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputedrdpfunction_ TSCOMPUTEDRDPFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputedrdpfunction_ tscomputedrdpfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetcostgradients_ TSSETCOSTGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetcostgradients_ tssetcostgradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetcostgradients_ TSGETCOSTGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetcostgradients_ tsgetcostgradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointsetup_ TSADJOINTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointsetup_ tsadjointsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointsetsteps_ TSADJOINTSETSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointsetsteps_ tsadjointsetsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointcomputedrdyfunction_ TSADJOINTCOMPUTEDRDYFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointcomputedrdyfunction_ tsadjointcomputedrdyfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointcomputedrdpfunction_ TSADJOINTCOMPUTEDRDPFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointcomputedrdpfunction_ tsadjointcomputedrdpfunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointstep_ TSADJOINTSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointstep_ tsadjointstep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointsolve_ TSADJOINTSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointsolve_ tsadjointsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointcostintegral_ TSADJOINTCOSTINTEGRAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointcostintegral_ tsadjointcostintegral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardsetup_ TSFORWARDSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardsetup_ tsforwardsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardsetintegralgradients_ TSFORWARDSETINTEGRALGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardsetintegralgradients_ tsforwardsetintegralgradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardgetintegralgradients_ TSFORWARDGETINTEGRALGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardgetintegralgradients_ tsforwardgetintegralgradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardstep_ TSFORWARDSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardstep_ tsforwardstep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardsetsensitivities_ TSFORWARDSETSENSITIVITIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardsetsensitivities_ tsforwardsetsensitivities
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardgetsensitivities_ TSFORWARDGETSENSITIVITIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardgetsensitivities_ tsforwardgetsensitivities
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardcostintegral_ TSFORWARDCOSTINTEGRAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardcostintegral_ tsforwardcostintegral
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  tsgetcostintegral_(TS ts,Vec *v, int *__ierr){
*__ierr = TSGetCostIntegral(
	(TS)PetscToPointer((ts) ),v);
}
PETSC_EXTERN void PETSC_STDCALL  tscomputecostintegrand_(TS ts,PetscReal *t,Vec y,Vec q, int *__ierr){
*__ierr = TSComputeCostIntegrand(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((q) ));
}
PETSC_EXTERN void PETSC_STDCALL  tscomputedrdyfunction_(TS ts,PetscReal *t,Vec y,Vec *drdy, int *__ierr){
*__ierr = TSComputeDRDYFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((y) ),drdy);
}
PETSC_EXTERN void PETSC_STDCALL  tscomputedrdpfunction_(TS ts,PetscReal *t,Vec y,Vec *drdp, int *__ierr){
*__ierr = TSComputeDRDPFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((y) ),drdp);
}
PETSC_EXTERN void PETSC_STDCALL  tssetcostgradients_(TS ts,PetscInt *numcost,Vec *lambda,Vec *mu, int *__ierr){
*__ierr = TSSetCostGradients(
	(TS)PetscToPointer((ts) ),*numcost,lambda,mu);
}
PETSC_EXTERN void PETSC_STDCALL  tsgetcostgradients_(TS ts,PetscInt *numcost,Vec **lambda,Vec **mu, int *__ierr){
*__ierr = TSGetCostGradients(
	(TS)PetscToPointer((ts) ),numcost,lambda,mu);
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointsetup_(TS ts, int *__ierr){
*__ierr = TSAdjointSetUp(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointsetsteps_(TS ts,PetscInt *steps, int *__ierr){
*__ierr = TSAdjointSetSteps(
	(TS)PetscToPointer((ts) ),*steps);
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointcomputedrdyfunction_(TS ts,PetscReal *t,Vec y,Vec *drdy, int *__ierr){
*__ierr = TSAdjointComputeDRDYFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((y) ),drdy);
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointcomputedrdpfunction_(TS ts,PetscReal *t,Vec y,Vec *drdp, int *__ierr){
*__ierr = TSAdjointComputeDRDPFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((y) ),drdp);
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointstep_(TS ts, int *__ierr){
*__ierr = TSAdjointStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointsolve_(TS ts, int *__ierr){
*__ierr = TSAdjointSolve(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsadjointcostintegral_(TS ts, int *__ierr){
*__ierr = TSAdjointCostIntegral(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardsetup_(TS ts, int *__ierr){
*__ierr = TSForwardSetUp(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardsetintegralgradients_(TS ts,PetscInt *numfwdint,Vec *vp, int *__ierr){
*__ierr = TSForwardSetIntegralGradients(
	(TS)PetscToPointer((ts) ),*numfwdint,vp);
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardgetintegralgradients_(TS ts,PetscInt *numfwdint,Vec **vp, int *__ierr){
*__ierr = TSForwardGetIntegralGradients(
	(TS)PetscToPointer((ts) ),numfwdint,vp);
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardstep_(TS ts, int *__ierr){
*__ierr = TSForwardStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardsetsensitivities_(TS ts,PetscInt *nump,Mat Smat, int *__ierr){
*__ierr = TSForwardSetSensitivities(
	(TS)PetscToPointer((ts) ),*nump,
	(Mat)PetscToPointer((Smat) ));
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardgetsensitivities_(TS ts,PetscInt *nump,Mat *Smat, int *__ierr){
*__ierr = TSForwardGetSensitivities(
	(TS)PetscToPointer((ts) ),nump,Smat);
}
PETSC_EXTERN void PETSC_STDCALL  tsforwardcostintegral_(TS ts, int *__ierr){
*__ierr = TSForwardCostIntegral(
	(TS)PetscToPointer((ts) ));
}
#if defined(__cplusplus)
}
#endif

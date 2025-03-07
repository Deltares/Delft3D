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
#define tssetcosthessianproducts_ TSSETCOSTHESSIANPRODUCTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetcosthessianproducts_ tssetcosthessianproducts
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetcosthessianproducts_ TSGETCOSTHESSIANPRODUCTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetcosthessianproducts_ tsgetcosthessianproducts
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointsetforward_ TSADJOINTSETFORWARD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointsetforward_ tsadjointsetforward
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointresetforward_ TSADJOINTRESETFORWARD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointresetforward_ tsadjointresetforward
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointsetup_ TSADJOINTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointsetup_ tsadjointsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadjointreset_ TSADJOINTRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadjointreset_ tsadjointreset
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
#define tsforwardreset_ TSFORWARDRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardreset_ tsforwardreset
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardsetinitialsensitivities_ TSFORWARDSETINITIALSENSITIVITIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardsetinitialsensitivities_ tsforwardsetinitialsensitivities
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsforwardgetstages_ TSFORWARDGETSTAGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsforwardgetstages_ tsforwardgetstages
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscreatequadraturets_ TSCREATEQUADRATURETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscreatequadraturets_ tscreatequadraturets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetquadraturets_ TSGETQUADRATURETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetquadraturets_ tsgetquadraturets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tscomputesnesjacobian_ TSCOMPUTESNESJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tscomputesnesjacobian_ tscomputesnesjacobian
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tssetcostgradients_(TS ts,PetscInt *numcost,Vec *lambda,Vec *mu, int *__ierr)
{
*__ierr = TSSetCostGradients(
	(TS)PetscToPointer((ts) ),*numcost,lambda,mu);
}
PETSC_EXTERN void  tsgetcostgradients_(TS ts,PetscInt *numcost,Vec **lambda,Vec **mu, int *__ierr)
{
*__ierr = TSGetCostGradients(
	(TS)PetscToPointer((ts) ),numcost,lambda,mu);
}
PETSC_EXTERN void  tssetcosthessianproducts_(TS ts,PetscInt *numcost,Vec *lambda2,Vec *mu2,Vec dir, int *__ierr)
{
*__ierr = TSSetCostHessianProducts(
	(TS)PetscToPointer((ts) ),*numcost,lambda2,mu2,
	(Vec)PetscToPointer((dir) ));
}
PETSC_EXTERN void  tsgetcosthessianproducts_(TS ts,PetscInt *numcost,Vec **lambda2,Vec **mu2,Vec *dir, int *__ierr)
{
*__ierr = TSGetCostHessianProducts(
	(TS)PetscToPointer((ts) ),numcost,lambda2,mu2,dir);
}
PETSC_EXTERN void  tsadjointsetforward_(TS ts,Mat didp, int *__ierr)
{
*__ierr = TSAdjointSetForward(
	(TS)PetscToPointer((ts) ),
	(Mat)PetscToPointer((didp) ));
}
PETSC_EXTERN void  tsadjointresetforward_(TS ts, int *__ierr)
{
*__ierr = TSAdjointResetForward(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsadjointsetup_(TS ts, int *__ierr)
{
*__ierr = TSAdjointSetUp(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsadjointreset_(TS ts, int *__ierr)
{
*__ierr = TSAdjointReset(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsadjointsetsteps_(TS ts,PetscInt *steps, int *__ierr)
{
*__ierr = TSAdjointSetSteps(
	(TS)PetscToPointer((ts) ),*steps);
}
PETSC_EXTERN void  tsadjointcomputedrdyfunction_(TS ts,PetscReal *t,Vec U,Vec *DRDU, int *__ierr)
{
*__ierr = TSAdjointComputeDRDYFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),DRDU);
}
PETSC_EXTERN void  tsadjointcomputedrdpfunction_(TS ts,PetscReal *t,Vec U,Vec *DRDP, int *__ierr)
{
*__ierr = TSAdjointComputeDRDPFunction(
	(TS)PetscToPointer((ts) ),*t,
	(Vec)PetscToPointer((U) ),DRDP);
}
PETSC_EXTERN void  tsadjointstep_(TS ts, int *__ierr)
{
*__ierr = TSAdjointStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsadjointsolve_(TS ts, int *__ierr)
{
*__ierr = TSAdjointSolve(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsadjointcostintegral_(TS ts, int *__ierr)
{
*__ierr = TSAdjointCostIntegral(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsforwardsetup_(TS ts, int *__ierr)
{
*__ierr = TSForwardSetUp(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsforwardreset_(TS ts, int *__ierr)
{
*__ierr = TSForwardReset(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsforwardsetintegralgradients_(TS ts,PetscInt *numfwdint,Vec *vp, int *__ierr)
{
*__ierr = TSForwardSetIntegralGradients(
	(TS)PetscToPointer((ts) ),*numfwdint,vp);
}
PETSC_EXTERN void  tsforwardgetintegralgradients_(TS ts,PetscInt *numfwdint,Vec **vp, int *__ierr)
{
*__ierr = TSForwardGetIntegralGradients(
	(TS)PetscToPointer((ts) ),numfwdint,vp);
}
PETSC_EXTERN void  tsforwardstep_(TS ts, int *__ierr)
{
*__ierr = TSForwardStep(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsforwardsetsensitivities_(TS ts,PetscInt *nump,Mat Smat, int *__ierr)
{
*__ierr = TSForwardSetSensitivities(
	(TS)PetscToPointer((ts) ),*nump,
	(Mat)PetscToPointer((Smat) ));
}
PETSC_EXTERN void  tsforwardgetsensitivities_(TS ts,PetscInt *nump,Mat *Smat, int *__ierr)
{
*__ierr = TSForwardGetSensitivities(
	(TS)PetscToPointer((ts) ),nump,Smat);
}
PETSC_EXTERN void  tsforwardcostintegral_(TS ts, int *__ierr)
{
*__ierr = TSForwardCostIntegral(
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tsforwardsetinitialsensitivities_(TS ts,Mat didp, int *__ierr)
{
*__ierr = TSForwardSetInitialSensitivities(
	(TS)PetscToPointer((ts) ),
	(Mat)PetscToPointer((didp) ));
}
PETSC_EXTERN void  tsforwardgetstages_(TS ts,PetscInt *ns,Mat **S, int *__ierr)
{
*__ierr = TSForwardGetStages(
	(TS)PetscToPointer((ts) ),ns,S);
}
PETSC_EXTERN void  tscreatequadraturets_(TS ts,PetscBool *fwd,TS *quadts, int *__ierr)
{
*__ierr = TSCreateQuadratureTS(
	(TS)PetscToPointer((ts) ),*fwd,quadts);
}
PETSC_EXTERN void  tsgetquadraturets_(TS ts,PetscBool *fwd,TS *quadts, int *__ierr)
{
*__ierr = TSGetQuadratureTS(
	(TS)PetscToPointer((ts) ),fwd,quadts);
}
PETSC_EXTERN void  tscomputesnesjacobian_(TS ts,Vec x,Mat J,Mat Jpre, int *__ierr)
{
*__ierr = TSComputeSNESJacobian(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((x) ),
	(Mat)PetscToPointer((J) ),
	(Mat)PetscToPointer((Jpre) ));
}
#if defined(__cplusplus)
}
#endif

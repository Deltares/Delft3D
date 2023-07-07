#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmplexsnes.c */
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

#include "petscdmplex.h"
#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnesgetgeometryfvm_ DMPLEXSNESGETGEOMETRYFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnesgetgeometryfvm_ dmplexsnesgetgeometryfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnesgetgradientdm_ DMPLEXSNESGETGRADIENTDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnesgetgradientdm_ dmplexsnesgetgradientdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputeresidualfem_ DMPLEXSNESCOMPUTERESIDUALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputeresidualfem_ dmplexsnescomputeresidualfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputeboundaryfem_ DMPLEXSNESCOMPUTEBOUNDARYFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputeboundaryfem_ dmplexsnescomputeboundaryfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputejacobianaction_ DMPLEXCOMPUTEJACOBIANACTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputejacobianaction_ dmplexcomputejacobianaction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputejacobianfem_ DMPLEXSNESCOMPUTEJACOBIANFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputejacobianfem_ dmplexsnescomputejacobianfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetsneslocalfem_ DMPLEXSETSNESLOCALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetsneslocalfem_ dmplexsetsneslocalfem
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexsnesgetgeometryfvm_(DM dm,Vec *facegeom,Vec *cellgeom,PetscReal *minRadius, int *__ierr){
*__ierr = DMPlexSNESGetGeometryFVM(
	(DM)PetscToPointer((dm) ),facegeom,cellgeom,minRadius);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsnesgetgradientdm_(DM dm,PetscFV fv,DM *dmGrad, int *__ierr){
*__ierr = DMPlexSNESGetGradientDM(
	(DM)PetscToPointer((dm) ),
	(PetscFV)PetscToPointer((fv) ),dmGrad);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsnescomputeresidualfem_(DM dm,Vec X,Vec F,void*user, int *__ierr){
*__ierr = DMPlexSNESComputeResidualFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsnescomputeboundaryfem_(DM dm,Vec X,void*user, int *__ierr){
*__ierr = DMPlexSNESComputeBoundaryFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputejacobianaction_(DM dm,IS cellIS,PetscReal *t,PetscReal *X_tShift,Vec X,Vec X_t,Vec Y,Vec Z,void*user, int *__ierr){
*__ierr = DMPlexComputeJacobianAction(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((cellIS) ),*t,*X_tShift,
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((X_t) ),
	(Vec)PetscToPointer((Y) ),
	(Vec)PetscToPointer((Z) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsnescomputejacobianfem_(DM dm,Vec X,Mat Jac,Mat JacP,void*user, int *__ierr){
*__ierr = DMPlexSNESComputeJacobianFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Mat)PetscToPointer((Jac) ),
	(Mat)PetscToPointer((JacP) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetsneslocalfem_(DM dm,void*boundaryctx,void*residualctx,void*jacobianctx, int *__ierr){
*__ierr = DMPlexSetSNESLocalFEM(
	(DM)PetscToPointer((dm) ),boundaryctx,residualctx,jacobianctx);
}
#if defined(__cplusplus)
}
#endif

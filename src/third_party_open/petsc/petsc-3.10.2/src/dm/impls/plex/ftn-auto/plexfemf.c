#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexfem.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetscale_ DMPLEXGETSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetscale_ dmplexgetscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetscale_ DMPLEXSETSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetscale_ dmplexsetscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetmaxprojectionheight_ DMPLEXSETMAXPROJECTIONHEIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetmaxprojectionheight_ dmplexsetmaxprojectionheight
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetmaxprojectionheight_ DMPLEXGETMAXPROJECTIONHEIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetmaxprojectionheight_ dmplexgetmaxprojectionheight
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexinsertboundaryvalues_ DMPLEXINSERTBOUNDARYVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexinsertboundaryvalues_ dmplexinsertboundaryvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeintegralfem_ DMPLEXCOMPUTEINTEGRALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeintegralfem_ dmplexcomputeintegralfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputecellwiseintegralfem_ DMPLEXCOMPUTECELLWISEINTEGRALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputecellwiseintegralfem_ dmplexcomputecellwiseintegralfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputebdintegral_ DMPLEXCOMPUTEBDINTEGRAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputebdintegral_ dmplexcomputebdintegral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeinterpolatornested_ DMPLEXCOMPUTEINTERPOLATORNESTED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeinterpolatornested_ dmplexcomputeinterpolatornested
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeinterpolatorgeneral_ DMPLEXCOMPUTEINTERPOLATORGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeinterpolatorgeneral_ dmplexcomputeinterpolatorgeneral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputemassmatrixgeneral_ DMPLEXCOMPUTEMASSMATRIXGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputemassmatrixgeneral_ dmplexcomputemassmatrixgeneral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeinjectorfem_ DMPLEXCOMPUTEINJECTORFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeinjectorfem_ dmplexcomputeinjectorfem
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexgetscale_(DM dm,PetscUnit *unit,PetscReal *scale, int *__ierr){
*__ierr = DMPlexGetScale(
	(DM)PetscToPointer((dm) ),*unit,scale);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetscale_(DM dm,PetscUnit *unit,PetscReal *scale, int *__ierr){
*__ierr = DMPlexSetScale(
	(DM)PetscToPointer((dm) ),*unit,*scale);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetmaxprojectionheight_(DM dm,PetscInt *height, int *__ierr){
*__ierr = DMPlexSetMaxProjectionHeight(
	(DM)PetscToPointer((dm) ),*height);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetmaxprojectionheight_(DM dm,PetscInt *height, int *__ierr){
*__ierr = DMPlexGetMaxProjectionHeight(
	(DM)PetscToPointer((dm) ),height);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexinsertboundaryvalues_(DM dm,PetscBool *insertEssential,Vec locX,PetscReal *time,Vec faceGeomFVM,Vec cellGeomFVM,Vec gradFVM, int *__ierr){
*__ierr = DMPlexInsertBoundaryValues(
	(DM)PetscToPointer((dm) ),*insertEssential,
	(Vec)PetscToPointer((locX) ),*time,
	(Vec)PetscToPointer((faceGeomFVM) ),
	(Vec)PetscToPointer((cellGeomFVM) ),
	(Vec)PetscToPointer((gradFVM) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputeintegralfem_(DM dm,Vec X,PetscScalar *integral,void*user, int *__ierr){
*__ierr = DMPlexComputeIntegralFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),integral,user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputecellwiseintegralfem_(DM dm,Vec X,Vec F,void*user, int *__ierr){
*__ierr = DMPlexComputeCellwiseIntegralFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputebdintegral_(DM dm,Vec X,DMLabel label,PetscInt *numVals, PetscInt vals[],
                                       void(*func)(PetscInt, PetscInt, PetscInt,
                                                    const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                    const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                    PetscReal, const PetscReal[], const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]),
                                       PetscScalar *integral,void*user, int *__ierr){
*__ierr = DMPlexComputeBdIntegral(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(DMLabel)PetscToPointer((label) ),*numVals,vals,func,integral,user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputeinterpolatornested_(DM dmc,DM dmf,Mat In,void*user, int *__ierr){
*__ierr = DMPlexComputeInterpolatorNested(
	(DM)PetscToPointer((dmc) ),
	(DM)PetscToPointer((dmf) ),
	(Mat)PetscToPointer((In) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputeinterpolatorgeneral_(DM dmc,DM dmf,Mat In,void*user, int *__ierr){
*__ierr = DMPlexComputeInterpolatorGeneral(
	(DM)PetscToPointer((dmc) ),
	(DM)PetscToPointer((dmf) ),
	(Mat)PetscToPointer((In) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputemassmatrixgeneral_(DM dmc,DM dmf,Mat mass,void*user, int *__ierr){
*__ierr = DMPlexComputeMassMatrixGeneral(
	(DM)PetscToPointer((dmc) ),
	(DM)PetscToPointer((dmf) ),
	(Mat)PetscToPointer((mass) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputeinjectorfem_(DM dmc,DM dmf,VecScatter *sc,void*user, int *__ierr){
*__ierr = DMPlexComputeInjectorFEM(
	(DM)PetscToPointer((dmc) ),
	(DM)PetscToPointer((dmf) ),sc,user);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexgeometry.c */
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
#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeprojection3dto2d_ DMPLEXCOMPUTEPROJECTION3DTO2D
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeprojection3dto2d_ dmplexcomputeprojection3dto2d
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputegeometryfem_ DMPLEXCOMPUTEGEOMETRYFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputegeometryfem_ dmplexcomputegeometryfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputegeometryfvm_ DMPLEXCOMPUTEGEOMETRYFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputegeometryfvm_ dmplexcomputegeometryfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputegradientfvm_ DMPLEXCOMPUTEGRADIENTFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputegradientfvm_ dmplexcomputegradientfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetdatafvm_ DMPLEXGETDATAFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetdatafvm_ dmplexgetdatafvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcoordinatestoreference_ DMPLEXCOORDINATESTOREFERENCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcoordinatestoreference_ dmplexcoordinatestoreference
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexreferencetocoordinates_ DMPLEXREFERENCETOCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreferencetocoordinates_ dmplexreferencetocoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputeprojection3dto2d_(PetscInt *coordSize,PetscScalar coords[],PetscReal R[], int *__ierr){
*__ierr = DMPlexComputeProjection3Dto2D(*coordSize,coords,R);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputegeometryfem_(DM dm,Vec *cellgeom, int *__ierr){
*__ierr = DMPlexComputeGeometryFEM(
	(DM)PetscToPointer((dm) ),cellgeom);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputegeometryfvm_(DM dm,Vec *cellgeom,Vec *facegeom, int *__ierr){
*__ierr = DMPlexComputeGeometryFVM(
	(DM)PetscToPointer((dm) ),cellgeom,facegeom);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcomputegradientfvm_(DM dm,PetscFV fvm,Vec faceGeometry,Vec cellGeometry,DM *dmGrad, int *__ierr){
*__ierr = DMPlexComputeGradientFVM(
	(DM)PetscToPointer((dm) ),
	(PetscFV)PetscToPointer((fvm) ),
	(Vec)PetscToPointer((faceGeometry) ),
	(Vec)PetscToPointer((cellGeometry) ),dmGrad);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetdatafvm_(DM dm,PetscFV fv,Vec *cellgeom,Vec *facegeom,DM *gradDM, int *__ierr){
*__ierr = DMPlexGetDataFVM(
	(DM)PetscToPointer((dm) ),
	(PetscFV)PetscToPointer((fv) ),cellgeom,facegeom,gradDM);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcoordinatestoreference_(DM dm,PetscInt *cell,PetscInt *numPoints, PetscReal realCoords[],PetscReal refCoords[], int *__ierr){
*__ierr = DMPlexCoordinatesToReference(
	(DM)PetscToPointer((dm) ),*cell,*numPoints,realCoords,refCoords);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexreferencetocoordinates_(DM dm,PetscInt *cell,PetscInt *numPoints, PetscReal refCoords[],PetscReal realCoords[], int *__ierr){
*__ierr = DMPlexReferenceToCoordinates(
	(DM)PetscToPointer((dm) ),*cell,*numPoints,refCoords,realCoords);
}
#if defined(__cplusplus)
}
#endif

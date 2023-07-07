#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexcreate.c */
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
#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatedoublet_ DMPLEXCREATEDOUBLET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatedoublet_ dmplexcreatedoublet
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatesquareboundary_ DMPLEXCREATESQUAREBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatesquareboundary_ dmplexcreatesquareboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatecubeboundary_ DMPLEXCREATECUBEBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatecubeboundary_ dmplexcreatecubeboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatewedgeboxmesh_ DMPLEXCREATEWEDGEBOXMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatewedgeboxmesh_ dmplexcreatewedgeboxmesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexextrude_ DMPLEXEXTRUDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexextrude_ dmplexextrude
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatehexcylindermesh_ DMPLEXCREATEHEXCYLINDERMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatehexcylindermesh_ dmplexcreatehexcylindermesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatewedgecylindermesh_ DMPLEXCREATEWEDGECYLINDERMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatewedgecylindermesh_ dmplexcreatewedgecylindermesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatespheremesh_ DMPLEXCREATESPHEREMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatespheremesh_ dmplexcreatespheremesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreate_ DMPLEXCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreate_ dmplexcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatefromdag_ DMPLEXCREATEFROMDAG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatefromdag_ dmplexcreatefromdag
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatereferencecell_ DMPLEXCREATEREFERENCECELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatereferencecell_ dmplexcreatereferencecell
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatedoublet_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,PetscBool *interpolate,PetscBool *refinementUniform,PetscReal *refinementLimit,DM *newdm, int *__ierr){
*__ierr = DMPlexCreateDoublet(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,*interpolate,*refinementUniform,*refinementLimit,newdm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatesquareboundary_(DM dm, PetscReal lower[], PetscReal upper[], PetscInt edges[], int *__ierr){
*__ierr = DMPlexCreateSquareBoundary(
	(DM)PetscToPointer((dm) ),lower,upper,edges);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatecubeboundary_(DM dm, PetscReal lower[], PetscReal upper[], PetscInt faces[], int *__ierr){
*__ierr = DMPlexCreateCubeBoundary(
	(DM)PetscToPointer((dm) ),lower,upper,faces);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatewedgeboxmesh_(MPI_Fint * comm, PetscInt faces[], PetscReal lower[], PetscReal upper[], DMBoundaryType periodicity[],PetscBool *ordExt,PetscBool *interpolate,DM *dm, int *__ierr){
*__ierr = DMPlexCreateWedgeBoxMesh(
	MPI_Comm_f2c(*(comm)),faces,lower,upper,periodicity,*ordExt,*interpolate,dm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexextrude_(DM idm,PetscInt *layers,PetscReal *height,PetscBool *ordExt,PetscBool *interpolate,DM* dm, int *__ierr){
*__ierr = DMPlexExtrude(
	(DM)PetscToPointer((idm) ),*layers,*height,*ordExt,*interpolate,dm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatehexcylindermesh_(MPI_Fint * comm,PetscInt *numRefine,DMBoundaryType *periodicZ,DM *dm, int *__ierr){
*__ierr = DMPlexCreateHexCylinderMesh(
	MPI_Comm_f2c(*(comm)),*numRefine,*periodicZ,dm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatewedgecylindermesh_(MPI_Fint * comm,PetscInt *n,PetscBool *interpolate,DM *dm, int *__ierr){
*__ierr = DMPlexCreateWedgeCylinderMesh(
	MPI_Comm_f2c(*(comm)),*n,*interpolate,dm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatespheremesh_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,DM *dm, int *__ierr){
*__ierr = DMPlexCreateSphereMesh(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,dm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreate_(MPI_Fint * comm,DM *mesh, int *__ierr){
*__ierr = DMPlexCreate(
	MPI_Comm_f2c(*(comm)),mesh);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatefromdag_(DM dm,PetscInt *depth, PetscInt numPoints[], PetscInt coneSize[], PetscInt cones[], PetscInt coneOrientations[], PetscScalar vertexCoords[], int *__ierr){
*__ierr = DMPlexCreateFromDAG(
	(DM)PetscToPointer((dm) ),*depth,numPoints,coneSize,cones,coneOrientations,vertexCoords);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatereferencecell_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,DM *refdm, int *__ierr){
*__ierr = DMPlexCreateReferenceCell(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,refdm);
}
#if defined(__cplusplus)
}
#endif

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
#define dmplexcreatewedgeboxmesh_ DMPLEXCREATEWEDGEBOXMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatewedgeboxmesh_ dmplexcreatewedgeboxmesh
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
#define dmplexcreatetpsmesh_ DMPLEXCREATETPSMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatetpsmesh_ dmplexcreatetpsmesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatespheremesh_ DMPLEXCREATESPHEREMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatespheremesh_ dmplexcreatespheremesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreateballmesh_ DMPLEXCREATEBALLMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreateballmesh_ dmplexcreateballmesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatereferencecell_ DMPLEXCREATEREFERENCECELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatereferencecell_ dmplexcreatereferencecell
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreate_ DMPLEXCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreate_ dmplexcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatefromcelllistparallelpetsc_ DMPLEXCREATEFROMCELLLISTPARALLELPETSC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatefromcelllistparallelpetsc_ dmplexcreatefromcelllistparallelpetsc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatefromcelllistpetsc_ DMPLEXCREATEFROMCELLLISTPETSC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatefromcelllistpetsc_ dmplexcreatefromcelllistpetsc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatefromdag_ DMPLEXCREATEFROMDAG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatefromdag_ dmplexcreatefromdag
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexcreatedoublet_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,PetscBool *interpolate,PetscReal *refinementLimit,DM *newdm, int *__ierr)
{
*__ierr = DMPlexCreateDoublet(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,*interpolate,*refinementLimit,newdm);
}
PETSC_EXTERN void  dmplexcreatewedgeboxmesh_(MPI_Fint * comm, PetscInt faces[], PetscReal lower[], PetscReal upper[], DMBoundaryType periodicity[],PetscBool *orderHeight,PetscBool *interpolate,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateWedgeBoxMesh(
	MPI_Comm_f2c(*(comm)),faces,lower,upper,periodicity,*orderHeight,*interpolate,dm);
}
PETSC_EXTERN void  dmplexcreatehexcylindermesh_(MPI_Fint * comm,DMBoundaryType *periodicZ,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateHexCylinderMesh(
	MPI_Comm_f2c(*(comm)),*periodicZ,dm);
}
PETSC_EXTERN void  dmplexcreatewedgecylindermesh_(MPI_Fint * comm,PetscInt *n,PetscBool *interpolate,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateWedgeCylinderMesh(
	MPI_Comm_f2c(*(comm)),*n,*interpolate,dm);
}
PETSC_EXTERN void  dmplexcreatetpsmesh_(MPI_Fint * comm,DMPlexTPSType *tpstype, PetscInt extent[], DMBoundaryType periodic[],PetscBool *tps_distribute,PetscInt *refinements,PetscInt *layers,PetscReal *thickness,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateTPSMesh(
	MPI_Comm_f2c(*(comm)),*tpstype,extent,periodic,*tps_distribute,*refinements,*layers,*thickness,dm);
}
PETSC_EXTERN void  dmplexcreatespheremesh_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,PetscReal *R,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateSphereMesh(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,*R,dm);
}
PETSC_EXTERN void  dmplexcreateballmesh_(MPI_Fint * comm,PetscInt *dim,PetscReal *R,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateBallMesh(
	MPI_Comm_f2c(*(comm)),*dim,*R,dm);
}
PETSC_EXTERN void  dmplexcreatereferencecell_(MPI_Fint * comm,DMPolytopeType *ct,DM *refdm, int *__ierr)
{
*__ierr = DMPlexCreateReferenceCell(
	MPI_Comm_f2c(*(comm)),*ct,refdm);
}
PETSC_EXTERN void  dmplexcreate_(MPI_Fint * comm,DM *mesh, int *__ierr)
{
*__ierr = DMPlexCreate(
	MPI_Comm_f2c(*(comm)),mesh);
}
PETSC_EXTERN void  dmplexcreatefromcelllistparallelpetsc_(MPI_Fint * comm,PetscInt *dim,PetscInt *numCells,PetscInt *numVertices,PetscInt *NVertices,PetscInt *numCorners,PetscBool *interpolate, PetscInt cells[],PetscInt *spaceDim, PetscReal vertexCoords[],PetscSF *vertexSF,PetscInt **verticesAdj,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateFromCellListParallelPetsc(
	MPI_Comm_f2c(*(comm)),*dim,*numCells,*numVertices,*NVertices,*numCorners,*interpolate,cells,*spaceDim,vertexCoords,vertexSF,verticesAdj,dm);
}
PETSC_EXTERN void  dmplexcreatefromcelllistpetsc_(MPI_Fint * comm,PetscInt *dim,PetscInt *numCells,PetscInt *numVertices,PetscInt *numCorners,PetscBool *interpolate, PetscInt cells[],PetscInt *spaceDim, PetscReal vertexCoords[],DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateFromCellListPetsc(
	MPI_Comm_f2c(*(comm)),*dim,*numCells,*numVertices,*numCorners,*interpolate,cells,*spaceDim,vertexCoords,dm);
}
PETSC_EXTERN void  dmplexcreatefromdag_(DM dm,PetscInt *depth, PetscInt numPoints[], PetscInt coneSize[], PetscInt cones[], PetscInt coneOrientations[], PetscScalar vertexCoords[], int *__ierr)
{
*__ierr = DMPlexCreateFromDAG(
	(DM)PetscToPointer((dm) ),*depth,numPoints,coneSize,cones,coneOrientations,vertexCoords);
}
#if defined(__cplusplus)
}
#endif

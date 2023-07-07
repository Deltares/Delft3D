#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexsubmesh.c */
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
#include "petscdmlabel.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmarkboundaryfaces_ DMPLEXMARKBOUNDARYFACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmarkboundaryfaces_ dmplexmarkboundaryfaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabelcomplete_ DMPLEXLABELCOMPLETE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabelcomplete_ dmplexlabelcomplete
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabeladdcells_ DMPLEXLABELADDCELLS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabeladdcells_ dmplexlabeladdcells
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabelclearcells_ DMPLEXLABELCLEARCELLS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabelclearcells_ dmplexlabelclearcells
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabelcohesivecomplete_ DMPLEXLABELCOHESIVECOMPLETE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabelcohesivecomplete_ dmplexlabelcohesivecomplete
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatehybridmesh_ DMPLEXCREATEHYBRIDMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatehybridmesh_ dmplexcreatehybridmesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetorientedface_ DMPLEXGETORIENTEDFACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetorientedface_ dmplexgetorientedface
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatesubmesh_ DMPLEXCREATESUBMESH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatesubmesh_ dmplexcreatesubmesh
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexfilter_ DMPLEXFILTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexfilter_ dmplexfilter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetsubpointmap_ DMPLEXGETSUBPOINTMAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsubpointmap_ dmplexgetsubpointmap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetsubpointmap_ DMPLEXSETSUBPOINTMAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetsubpointmap_ dmplexsetsubpointmap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatesubpointis_ DMPLEXCREATESUBPOINTIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatesubpointis_ dmplexcreatesubpointis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetsubpoint_ DMPLEXGETSUBPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsubpoint_ dmplexgetsubpoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexmarkboundaryfaces_(DM dm,PetscInt *val,DMLabel label, int *__ierr){
*__ierr = DMPlexMarkBoundaryFaces(
	(DM)PetscToPointer((dm) ),*val,
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexlabelcomplete_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexLabelComplete(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexlabeladdcells_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexLabelAddCells(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexlabelclearcells_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexLabelClearCells(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexlabelcohesivecomplete_(DM dm,DMLabel label,DMLabel blabel,PetscBool *flip,DM subdm, int *__ierr){
*__ierr = DMPlexLabelCohesiveComplete(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),
	(DMLabel)PetscToPointer((blabel) ),*flip,
	(DM)PetscToPointer((subdm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatehybridmesh_(DM dm,DMLabel label,DMLabel bdlabel,DMLabel *hybridLabel,DMLabel *splitLabel,DM *dmInterface,DM *dmHybrid, int *__ierr){
*__ierr = DMPlexCreateHybridMesh(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),
	(DMLabel)PetscToPointer((bdlabel) ),hybridLabel,splitLabel,dmInterface,dmHybrid);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetorientedface_(DM dm,PetscInt *cell,PetscInt *faceSize, PetscInt face[],PetscInt *numCorners,PetscInt indices[],PetscInt origVertices[],PetscInt faceVertices[],PetscBool *posOriented, int *__ierr){
*__ierr = DMPlexGetOrientedFace(
	(DM)PetscToPointer((dm) ),*cell,*faceSize,face,*numCorners,indices,origVertices,faceVertices,posOriented);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatesubmesh_(DM dm,DMLabel vertexLabel,PetscInt *value,PetscBool *markedFaces,DM *subdm, int *__ierr){
*__ierr = DMPlexCreateSubmesh(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((vertexLabel) ),*value,*markedFaces,subdm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexfilter_(DM dm,DMLabel cellLabel,PetscInt *value,DM *subdm, int *__ierr){
*__ierr = DMPlexFilter(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((cellLabel) ),*value,subdm);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetsubpointmap_(DM dm,DMLabel *subpointMap, int *__ierr){
*__ierr = DMPlexGetSubpointMap(
	(DM)PetscToPointer((dm) ),subpointMap);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetsubpointmap_(DM dm,DMLabel subpointMap, int *__ierr){
*__ierr = DMPlexSetSubpointMap(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((subpointMap) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreatesubpointis_(DM dm,IS *subpointIS, int *__ierr){
*__ierr = DMPlexCreateSubpointIS(
	(DM)PetscToPointer((dm) ),subpointIS);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetsubpoint_(DM dm,PetscInt *p,PetscInt *subp, int *__ierr){
*__ierr = DMPlexGetSubpoint(
	(DM)PetscToPointer((dm) ),*p,subp);
}
#if defined(__cplusplus)
}
#endif

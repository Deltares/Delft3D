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
#define dmplexlabeladdfacecells_ DMPLEXLABELADDFACECELLS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabeladdfacecells_ dmplexlabeladdfacecells
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
#define dmplexreordercohesivesupports_ DMPLEXREORDERCOHESIVESUPPORTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreordercohesivesupports_ dmplexreordercohesivesupports
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
#define dmplexgetsubpointis_ DMPLEXGETSUBPOINTIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsubpointis_ dmplexgetsubpointis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetenclosurerelation_ DMGETENCLOSURERELATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetenclosurerelation_ dmgetenclosurerelation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetenclosurepoint_ DMGETENCLOSUREPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetenclosurepoint_ dmgetenclosurepoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexmarkboundaryfaces_(DM dm,PetscInt *val,DMLabel label, int *__ierr)
{
*__ierr = DMPlexMarkBoundaryFaces(
	(DM)PetscToPointer((dm) ),*val,
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexlabelcomplete_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexLabelComplete(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexlabeladdcells_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexLabelAddCells(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexlabeladdfacecells_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexLabelAddFaceCells(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexlabelclearcells_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexLabelClearCells(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexlabelcohesivecomplete_(DM dm,DMLabel label,DMLabel blabel,PetscInt *bvalue,PetscBool *flip,DM subdm, int *__ierr)
{
*__ierr = DMPlexLabelCohesiveComplete(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),
	(DMLabel)PetscToPointer((blabel) ),*bvalue,*flip,
	(DM)PetscToPointer((subdm) ));
}
PETSC_EXTERN void  dmplexcreatehybridmesh_(DM dm,DMLabel label,DMLabel bdlabel,PetscInt *bdvalue,DMLabel *hybridLabel,DMLabel *splitLabel,DM *dmInterface,DM *dmHybrid, int *__ierr)
{
*__ierr = DMPlexCreateHybridMesh(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),
	(DMLabel)PetscToPointer((bdlabel) ),*bdvalue,hybridLabel,splitLabel,dmInterface,dmHybrid);
}
PETSC_EXTERN void  dmplexgetorientedface_(DM dm,PetscInt *cell,PetscInt *faceSize, PetscInt face[],PetscInt *numCorners,PetscInt indices[],PetscInt origVertices[],PetscInt faceVertices[],PetscBool *posOriented, int *__ierr)
{
*__ierr = DMPlexGetOrientedFace(
	(DM)PetscToPointer((dm) ),*cell,*faceSize,face,*numCorners,indices,origVertices,faceVertices,posOriented);
}
PETSC_EXTERN void  dmplexcreatesubmesh_(DM dm,DMLabel vertexLabel,PetscInt *value,PetscBool *markedFaces,DM *subdm, int *__ierr)
{
*__ierr = DMPlexCreateSubmesh(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((vertexLabel) ),*value,*markedFaces,subdm);
}
PETSC_EXTERN void  dmplexreordercohesivesupports_(DM dm, int *__ierr)
{
*__ierr = DMPlexReorderCohesiveSupports(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexfilter_(DM dm,DMLabel cellLabel,PetscInt *value,DM *subdm, int *__ierr)
{
*__ierr = DMPlexFilter(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((cellLabel) ),*value,subdm);
}
PETSC_EXTERN void  dmplexgetsubpointmap_(DM dm,DMLabel *subpointMap, int *__ierr)
{
*__ierr = DMPlexGetSubpointMap(
	(DM)PetscToPointer((dm) ),subpointMap);
}
PETSC_EXTERN void  dmplexsetsubpointmap_(DM dm,DMLabel subpointMap, int *__ierr)
{
*__ierr = DMPlexSetSubpointMap(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((subpointMap) ));
}
PETSC_EXTERN void  dmplexgetsubpointis_(DM dm,IS *subpointIS, int *__ierr)
{
*__ierr = DMPlexGetSubpointIS(
	(DM)PetscToPointer((dm) ),subpointIS);
}
PETSC_EXTERN void  dmgetenclosurerelation_(DM dmA,DM dmB,DMEnclosureType *rel, int *__ierr)
{
*__ierr = DMGetEnclosureRelation(
	(DM)PetscToPointer((dmA) ),
	(DM)PetscToPointer((dmB) ),rel);
}
PETSC_EXTERN void  dmgetenclosurepoint_(DM dmA,DM dmB,DMEnclosureType *etype,PetscInt *pB,PetscInt *pA, int *__ierr)
{
*__ierr = DMGetEnclosurePoint(
	(DM)PetscToPointer((dmA) ),
	(DM)PetscToPointer((dmB) ),*etype,*pB,pA);
}
#if defined(__cplusplus)
}
#endif

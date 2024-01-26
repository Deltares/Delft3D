#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plex.c */
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
#define dmplexissimplex_ DMPLEXISSIMPLEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexissimplex_ dmplexissimplex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetsimplexorboxcells_ DMPLEXGETSIMPLEXORBOXCELLS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsimplexorboxcells_ dmplexgetsimplexorboxcells
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexvecview1d_ DMPLEXVECVIEW1D
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexvecview1d_ dmplexvecview1d
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextopologyview_ DMPLEXTOPOLOGYVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextopologyview_ dmplextopologyview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcoordinatesview_ DMPLEXCOORDINATESVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcoordinatesview_ dmplexcoordinatesview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabelsview_ DMPLEXLABELSVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabelsview_ dmplexlabelsview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsectionview_ DMPLEXSECTIONVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsectionview_ dmplexsectionview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexglobalvectorview_ DMPLEXGLOBALVECTORVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexglobalvectorview_ dmplexglobalvectorview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlocalvectorview_ DMPLEXLOCALVECTORVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlocalvectorview_ dmplexlocalvectorview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextopologyload_ DMPLEXTOPOLOGYLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextopologyload_ dmplextopologyload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcoordinatesload_ DMPLEXCOORDINATESLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcoordinatesload_ dmplexcoordinatesload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlabelsload_ DMPLEXLABELSLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlabelsload_ dmplexlabelsload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsectionload_ DMPLEXSECTIONLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsectionload_ dmplexsectionload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexglobalvectorload_ DMPLEXGLOBALVECTORLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexglobalvectorload_ dmplexglobalvectorload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlocalvectorload_ DMPLEXLOCALVECTORLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlocalvectorload_ dmplexlocalvectorload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetsubdomainsection_ DMPLEXGETSUBDOMAINSECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsubdomainsection_ dmplexgetsubdomainsection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetchart_ DMPLEXGETCHART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetchart_ dmplexgetchart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetchart_ DMPLEXSETCHART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetchart_ dmplexsetchart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetconesize_ DMPLEXGETCONESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetconesize_ dmplexgetconesize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetconesize_ DMPLEXSETCONESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetconesize_ dmplexsetconesize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetconerecursivevertices_ DMPLEXGETCONERECURSIVEVERTICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetconerecursivevertices_ dmplexgetconerecursivevertices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetconerecursive_ DMPLEXGETCONERECURSIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetconerecursive_ dmplexgetconerecursive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexrestoreconerecursive_ DMPLEXRESTORECONERECURSIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexrestoreconerecursive_ dmplexrestoreconerecursive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetcone_ DMPLEXSETCONE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetcone_ dmplexsetcone
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetconeorientation_ DMPLEXSETCONEORIENTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetconeorientation_ dmplexsetconeorientation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexinsertcone_ DMPLEXINSERTCONE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexinsertcone_ dmplexinsertcone
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexinsertconeorientation_ DMPLEXINSERTCONEORIENTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexinsertconeorientation_ dmplexinsertconeorientation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetsupportsize_ DMPLEXGETSUPPORTSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetsupportsize_ dmplexgetsupportsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetsupportsize_ DMPLEXSETSUPPORTSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetsupportsize_ dmplexsetsupportsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetsupport_ DMPLEXSETSUPPORT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetsupport_ dmplexsetsupport
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexinsertsupport_ DMPLEXINSERTSUPPORT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexinsertsupport_ dmplexinsertsupport
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetmaxsizes_ DMPLEXGETMAXSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetmaxsizes_ dmplexgetmaxsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsymmetrize_ DMPLEXSYMMETRIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsymmetrize_ dmplexsymmetrize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexstratify_ DMPLEXSTRATIFY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexstratify_ dmplexstratify
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputecelltypes_ DMPLEXCOMPUTECELLTYPES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputecelltypes_ dmplexcomputecelltypes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetdepthlabel_ DMPLEXGETDEPTHLABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetdepthlabel_ dmplexgetdepthlabel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetdepth_ DMPLEXGETDEPTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetdepth_ dmplexgetdepth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetdepthstratum_ DMPLEXGETDEPTHSTRATUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetdepthstratum_ dmplexgetdepthstratum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetheightstratum_ DMPLEXGETHEIGHTSTRATUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetheightstratum_ dmplexgetheightstratum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointdepth_ DMPLEXGETPOINTDEPTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointdepth_ dmplexgetpointdepth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpointheight_ DMPLEXGETPOINTHEIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpointheight_ dmplexgetpointheight
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetcelltypelabel_ DMPLEXGETCELLTYPELABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetcelltypelabel_ dmplexgetcelltypelabel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetcelltype_ DMPLEXGETCELLTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetcelltype_ dmplexgetcelltype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetcelltype_ DMPLEXSETCELLTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetcelltype_ dmplexsetcelltype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetclosurepermutationtensor_ DMPLEXSETCLOSUREPERMUTATIONTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetclosurepermutationtensor_ dmplexsetclosurepermutationtensor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetghostcellstratum_ DMPLEXGETGHOSTCELLSTRATUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetghostcellstratum_ dmplexgetghostcellstratum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetcellnumbering_ DMPLEXGETCELLNUMBERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetcellnumbering_ dmplexgetcellnumbering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetvertexnumbering_ DMPLEXGETVERTEXNUMBERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetvertexnumbering_ dmplexgetvertexnumbering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatepointnumbering_ DMPLEXCREATEPOINTNUMBERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatepointnumbering_ dmplexcreatepointnumbering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreaterankfield_ DMPLEXCREATERANKFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreaterankfield_ dmplexcreaterankfield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatelabelfield_ DMPLEXCREATELABELFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatelabelfield_ dmplexcreatelabelfield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexchecksymmetry_ DMPLEXCHECKSYMMETRY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexchecksymmetry_ dmplexchecksymmetry
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheckskeleton_ DMPLEXCHECKSKELETON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheckskeleton_ dmplexcheckskeleton
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheckfaces_ DMPLEXCHECKFACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheckfaces_ dmplexcheckfaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheckgeometry_ DMPLEXCHECKGEOMETRY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheckgeometry_ dmplexcheckgeometry
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheckpointsf_ DMPLEXCHECKPOINTSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheckpointsf_ dmplexcheckpointsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheck_ DMPLEXCHECK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheck_ dmplexcheck
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcheckcellshape_ DMPLEXCHECKCELLSHAPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcheckcellshape_ dmplexcheckcellshape
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcomputeorthogonalquality_ DMPLEXCOMPUTEORTHOGONALQUALITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcomputeorthogonalquality_ dmplexcomputeorthogonalquality
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetregularrefinement_ DMPLEXGETREGULARREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetregularrefinement_ dmplexgetregularrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetregularrefinement_ DMPLEXSETREGULARREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetregularrefinement_ dmplexsetregularrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetanchors_ DMPLEXGETANCHORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetanchors_ dmplexgetanchors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetanchors_ DMPLEXSETANCHORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetanchors_ dmplexsetanchors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexmonitorthroughput_ DMPLEXMONITORTHROUGHPUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexmonitorthroughput_ dmplexmonitorthroughput
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexissimplex_(DM dm,PetscBool *simplex, int *__ierr)
{
*__ierr = DMPlexIsSimplex(
	(DM)PetscToPointer((dm) ),simplex);
}
PETSC_EXTERN void  dmplexgetsimplexorboxcells_(DM dm,PetscInt *height,PetscInt *cStart,PetscInt *cEnd, int *__ierr)
{
*__ierr = DMPlexGetSimplexOrBoxCells(
	(DM)PetscToPointer((dm) ),*height,cStart,cEnd);
}
PETSC_EXTERN void  dmplexvecview1d_(DM dm,PetscInt *n,Vec u[],PetscViewer viewer, int *__ierr)
{
*__ierr = DMPlexVecView1D(
	(DM)PetscToPointer((dm) ),*n,u,
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  dmplextopologyview_(DM dm,PetscViewer viewer, int *__ierr)
{
*__ierr = DMPlexTopologyView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  dmplexcoordinatesview_(DM dm,PetscViewer viewer, int *__ierr)
{
*__ierr = DMPlexCoordinatesView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  dmplexlabelsview_(DM dm,PetscViewer viewer, int *__ierr)
{
*__ierr = DMPlexLabelsView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  dmplexsectionview_(DM dm,PetscViewer viewer,DM sectiondm, int *__ierr)
{
*__ierr = DMPlexSectionView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ));
}
PETSC_EXTERN void  dmplexglobalvectorview_(DM dm,PetscViewer viewer,DM sectiondm,Vec vec, int *__ierr)
{
*__ierr = DMPlexGlobalVectorView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ),
	(Vec)PetscToPointer((vec) ));
}
PETSC_EXTERN void  dmplexlocalvectorview_(DM dm,PetscViewer viewer,DM sectiondm,Vec vec, int *__ierr)
{
*__ierr = DMPlexLocalVectorView(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ),
	(Vec)PetscToPointer((vec) ));
}
PETSC_EXTERN void  dmplextopologyload_(DM dm,PetscViewer viewer,PetscSF *globalToLocalPointSF, int *__ierr)
{
*__ierr = DMPlexTopologyLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),globalToLocalPointSF);
}
PETSC_EXTERN void  dmplexcoordinatesload_(DM dm,PetscViewer viewer,PetscSF globalToLocalPointSF, int *__ierr)
{
*__ierr = DMPlexCoordinatesLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(PetscSF)PetscToPointer((globalToLocalPointSF) ));
}
PETSC_EXTERN void  dmplexlabelsload_(DM dm,PetscViewer viewer,PetscSF globalToLocalPointSF, int *__ierr)
{
*__ierr = DMPlexLabelsLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(PetscSF)PetscToPointer((globalToLocalPointSF) ));
}
PETSC_EXTERN void  dmplexsectionload_(DM dm,PetscViewer viewer,DM sectiondm,PetscSF globalToLocalPointSF,PetscSF *globalDofSF,PetscSF *localDofSF, int *__ierr)
{
*__ierr = DMPlexSectionLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ),
	(PetscSF)PetscToPointer((globalToLocalPointSF) ),globalDofSF,localDofSF);
}
PETSC_EXTERN void  dmplexglobalvectorload_(DM dm,PetscViewer viewer,DM sectiondm,PetscSF sf,Vec vec, int *__ierr)
{
*__ierr = DMPlexGlobalVectorLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ),
	(PetscSF)PetscToPointer((sf) ),
	(Vec)PetscToPointer((vec) ));
}
PETSC_EXTERN void  dmplexlocalvectorload_(DM dm,PetscViewer viewer,DM sectiondm,PetscSF sf,Vec vec, int *__ierr)
{
*__ierr = DMPlexLocalVectorLoad(
	(DM)PetscToPointer((dm) ),
	(PetscViewer)PetscToPointer((viewer) ),
	(DM)PetscToPointer((sectiondm) ),
	(PetscSF)PetscToPointer((sf) ),
	(Vec)PetscToPointer((vec) ));
}
PETSC_EXTERN void  dmplexgetsubdomainsection_(DM dm,PetscSection *subsection, int *__ierr)
{
*__ierr = DMPlexGetSubdomainSection(
	(DM)PetscToPointer((dm) ),subsection);
}
PETSC_EXTERN void  dmplexgetchart_(DM dm,PetscInt *pStart,PetscInt *pEnd, int *__ierr)
{
*__ierr = DMPlexGetChart(
	(DM)PetscToPointer((dm) ),pStart,pEnd);
}
PETSC_EXTERN void  dmplexsetchart_(DM dm,PetscInt *pStart,PetscInt *pEnd, int *__ierr)
{
*__ierr = DMPlexSetChart(
	(DM)PetscToPointer((dm) ),*pStart,*pEnd);
}
PETSC_EXTERN void  dmplexgetconesize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr)
{
*__ierr = DMPlexGetConeSize(
	(DM)PetscToPointer((dm) ),*p,size);
}
PETSC_EXTERN void  dmplexsetconesize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr)
{
*__ierr = DMPlexSetConeSize(
	(DM)PetscToPointer((dm) ),*p,*size);
}
PETSC_EXTERN void  dmplexgetconerecursivevertices_(DM dm,IS points,IS *expandedPoints, int *__ierr)
{
*__ierr = DMPlexGetConeRecursiveVertices(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((points) ),expandedPoints);
}
PETSC_EXTERN void  dmplexgetconerecursive_(DM dm,IS points,PetscInt *depth,IS *expandedPoints[],PetscSection *sections[], int *__ierr)
{
*__ierr = DMPlexGetConeRecursive(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((points) ),depth,expandedPoints,sections);
}
PETSC_EXTERN void  dmplexrestoreconerecursive_(DM dm,IS points,PetscInt *depth,IS *expandedPoints[],PetscSection *sections[], int *__ierr)
{
*__ierr = DMPlexRestoreConeRecursive(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((points) ),depth,expandedPoints,sections);
}
PETSC_EXTERN void  dmplexsetcone_(DM dm,PetscInt *p, PetscInt cone[], int *__ierr)
{
*__ierr = DMPlexSetCone(
	(DM)PetscToPointer((dm) ),*p,cone);
}
PETSC_EXTERN void  dmplexsetconeorientation_(DM dm,PetscInt *p, PetscInt coneOrientation[], int *__ierr)
{
*__ierr = DMPlexSetConeOrientation(
	(DM)PetscToPointer((dm) ),*p,coneOrientation);
}
PETSC_EXTERN void  dmplexinsertcone_(DM dm,PetscInt *p,PetscInt *conePos,PetscInt *conePoint, int *__ierr)
{
*__ierr = DMPlexInsertCone(
	(DM)PetscToPointer((dm) ),*p,*conePos,*conePoint);
}
PETSC_EXTERN void  dmplexinsertconeorientation_(DM dm,PetscInt *p,PetscInt *conePos,PetscInt *coneOrientation, int *__ierr)
{
*__ierr = DMPlexInsertConeOrientation(
	(DM)PetscToPointer((dm) ),*p,*conePos,*coneOrientation);
}
PETSC_EXTERN void  dmplexgetsupportsize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr)
{
*__ierr = DMPlexGetSupportSize(
	(DM)PetscToPointer((dm) ),*p,size);
}
PETSC_EXTERN void  dmplexsetsupportsize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr)
{
*__ierr = DMPlexSetSupportSize(
	(DM)PetscToPointer((dm) ),*p,*size);
}
PETSC_EXTERN void  dmplexsetsupport_(DM dm,PetscInt *p, PetscInt support[], int *__ierr)
{
*__ierr = DMPlexSetSupport(
	(DM)PetscToPointer((dm) ),*p,support);
}
PETSC_EXTERN void  dmplexinsertsupport_(DM dm,PetscInt *p,PetscInt *supportPos,PetscInt *supportPoint, int *__ierr)
{
*__ierr = DMPlexInsertSupport(
	(DM)PetscToPointer((dm) ),*p,*supportPos,*supportPoint);
}
PETSC_EXTERN void  dmplexgetmaxsizes_(DM dm,PetscInt *maxConeSize,PetscInt *maxSupportSize, int *__ierr)
{
*__ierr = DMPlexGetMaxSizes(
	(DM)PetscToPointer((dm) ),maxConeSize,maxSupportSize);
}
PETSC_EXTERN void  dmplexsymmetrize_(DM dm, int *__ierr)
{
*__ierr = DMPlexSymmetrize(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexstratify_(DM dm, int *__ierr)
{
*__ierr = DMPlexStratify(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexcomputecelltypes_(DM dm, int *__ierr)
{
*__ierr = DMPlexComputeCellTypes(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexgetdepthlabel_(DM dm,DMLabel *depthLabel, int *__ierr)
{
*__ierr = DMPlexGetDepthLabel(
	(DM)PetscToPointer((dm) ),depthLabel);
}
PETSC_EXTERN void  dmplexgetdepth_(DM dm,PetscInt *depth, int *__ierr)
{
*__ierr = DMPlexGetDepth(
	(DM)PetscToPointer((dm) ),depth);
}
PETSC_EXTERN void  dmplexgetdepthstratum_(DM dm,PetscInt *depth,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetDepthStratum(
	(DM)PetscToPointer((dm) ),*depth,start,end);
}
PETSC_EXTERN void  dmplexgetheightstratum_(DM dm,PetscInt *height,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = DMPlexGetHeightStratum(
	(DM)PetscToPointer((dm) ),*height,start,end);
}
PETSC_EXTERN void  dmplexgetpointdepth_(DM dm,PetscInt *point,PetscInt *depth, int *__ierr)
{
*__ierr = DMPlexGetPointDepth(
	(DM)PetscToPointer((dm) ),*point,depth);
}
PETSC_EXTERN void  dmplexgetpointheight_(DM dm,PetscInt *point,PetscInt *height, int *__ierr)
{
*__ierr = DMPlexGetPointHeight(
	(DM)PetscToPointer((dm) ),*point,height);
}
PETSC_EXTERN void  dmplexgetcelltypelabel_(DM dm,DMLabel *celltypeLabel, int *__ierr)
{
*__ierr = DMPlexGetCellTypeLabel(
	(DM)PetscToPointer((dm) ),celltypeLabel);
}
PETSC_EXTERN void  dmplexgetcelltype_(DM dm,PetscInt *cell,DMPolytopeType *celltype, int *__ierr)
{
*__ierr = DMPlexGetCellType(
	(DM)PetscToPointer((dm) ),*cell,celltype);
}
PETSC_EXTERN void  dmplexsetcelltype_(DM dm,PetscInt *cell,DMPolytopeType *celltype, int *__ierr)
{
*__ierr = DMPlexSetCellType(
	(DM)PetscToPointer((dm) ),*cell,*celltype);
}
PETSC_EXTERN void  dmplexsetclosurepermutationtensor_(DM dm,PetscInt *point,PetscSection section, int *__ierr)
{
*__ierr = DMPlexSetClosurePermutationTensor(
	(DM)PetscToPointer((dm) ),*point,
	(PetscSection)PetscToPointer((section) ));
}
PETSC_EXTERN void  dmplexgetghostcellstratum_(DM dm,PetscInt *gcStart,PetscInt *gcEnd, int *__ierr)
{
*__ierr = DMPlexGetGhostCellStratum(
	(DM)PetscToPointer((dm) ),gcStart,gcEnd);
}
PETSC_EXTERN void  dmplexgetcellnumbering_(DM dm,IS *globalCellNumbers, int *__ierr)
{
*__ierr = DMPlexGetCellNumbering(
	(DM)PetscToPointer((dm) ),globalCellNumbers);
}
PETSC_EXTERN void  dmplexgetvertexnumbering_(DM dm,IS *globalVertexNumbers, int *__ierr)
{
*__ierr = DMPlexGetVertexNumbering(
	(DM)PetscToPointer((dm) ),globalVertexNumbers);
}
PETSC_EXTERN void  dmplexcreatepointnumbering_(DM dm,IS *globalPointNumbers, int *__ierr)
{
*__ierr = DMPlexCreatePointNumbering(
	(DM)PetscToPointer((dm) ),globalPointNumbers);
}
PETSC_EXTERN void  dmplexcreaterankfield_(DM dm,Vec *ranks, int *__ierr)
{
*__ierr = DMPlexCreateRankField(
	(DM)PetscToPointer((dm) ),ranks);
}
PETSC_EXTERN void  dmplexcreatelabelfield_(DM dm,DMLabel label,Vec *val, int *__ierr)
{
*__ierr = DMPlexCreateLabelField(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),val);
}
PETSC_EXTERN void  dmplexchecksymmetry_(DM dm, int *__ierr)
{
*__ierr = DMPlexCheckSymmetry(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexcheckskeleton_(DM dm,PetscInt *cellHeight, int *__ierr)
{
*__ierr = DMPlexCheckSkeleton(
	(DM)PetscToPointer((dm) ),*cellHeight);
}
PETSC_EXTERN void  dmplexcheckfaces_(DM dm,PetscInt *cellHeight, int *__ierr)
{
*__ierr = DMPlexCheckFaces(
	(DM)PetscToPointer((dm) ),*cellHeight);
}
PETSC_EXTERN void  dmplexcheckgeometry_(DM dm, int *__ierr)
{
*__ierr = DMPlexCheckGeometry(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexcheckpointsf_(DM dm,PetscSF pointSF,PetscBool *allowExtraRoots, int *__ierr)
{
*__ierr = DMPlexCheckPointSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((pointSF) ),*allowExtraRoots);
}
PETSC_EXTERN void  dmplexcheck_(DM dm, int *__ierr)
{
*__ierr = DMPlexCheck(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmplexcheckcellshape_(DM dm,PetscBool *output,PetscReal *condLimit, int *__ierr)
{
*__ierr = DMPlexCheckCellShape(
	(DM)PetscToPointer((dm) ),*output,*condLimit);
}
PETSC_EXTERN void  dmplexcomputeorthogonalquality_(DM dm,PetscFV fv,PetscReal *atol,Vec *OrthQual,DMLabel *OrthQualLabel, int *__ierr)
{
*__ierr = DMPlexComputeOrthogonalQuality(
	(DM)PetscToPointer((dm) ),
	(PetscFV)PetscToPointer((fv) ),*atol,OrthQual,OrthQualLabel);
}
PETSC_EXTERN void  dmplexgetregularrefinement_(DM dm,PetscBool *regular, int *__ierr)
{
*__ierr = DMPlexGetRegularRefinement(
	(DM)PetscToPointer((dm) ),regular);
}
PETSC_EXTERN void  dmplexsetregularrefinement_(DM dm,PetscBool *regular, int *__ierr)
{
*__ierr = DMPlexSetRegularRefinement(
	(DM)PetscToPointer((dm) ),*regular);
}
PETSC_EXTERN void  dmplexgetanchors_(DM dm,PetscSection *anchorSection,IS *anchorIS, int *__ierr)
{
*__ierr = DMPlexGetAnchors(
	(DM)PetscToPointer((dm) ),anchorSection,anchorIS);
}
PETSC_EXTERN void  dmplexsetanchors_(DM dm,PetscSection anchorSection,IS anchorIS, int *__ierr)
{
*__ierr = DMPlexSetAnchors(
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((anchorSection) ),
	(IS)PetscToPointer((anchorIS) ));
}
PETSC_EXTERN void  dmplexmonitorthroughput_(DM dm,void*dummy, int *__ierr)
{
*__ierr = DMPlexMonitorThroughput(
	(DM)PetscToPointer((dm) ),dummy);
}
#if defined(__cplusplus)
}
#endif

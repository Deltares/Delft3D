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
#define dmplexrefinesimplextotensor_ DMPLEXREFINESIMPLEXTOTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexrefinesimplextotensor_ dmplexrefinesimplextotensor
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
#define dmplexaddconesize_ DMPLEXADDCONESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexaddconesize_ dmplexaddconesize
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
#define dmplexgethybridbounds_ DMPLEXGETHYBRIDBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgethybridbounds_ dmplexgethybridbounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsethybridbounds_ DMPLEXSETHYBRIDBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsethybridbounds_ dmplexsethybridbounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreaterankfield_ DMPLEXCREATERANKFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreaterankfield_ dmplexcreaterankfield
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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplexrefinesimplextotensor_(DM dm,DM *dmRefined, int *__ierr){
*__ierr = DMPlexRefineSimplexToTensor(
	(DM)PetscToPointer((dm) ),dmRefined);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetsubdomainsection_(DM dm,PetscSection *subsection, int *__ierr){
*__ierr = DMPlexGetSubdomainSection(
	(DM)PetscToPointer((dm) ),subsection);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetchart_(DM dm,PetscInt *pStart,PetscInt *pEnd, int *__ierr){
*__ierr = DMPlexGetChart(
	(DM)PetscToPointer((dm) ),pStart,pEnd);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetchart_(DM dm,PetscInt *pStart,PetscInt *pEnd, int *__ierr){
*__ierr = DMPlexSetChart(
	(DM)PetscToPointer((dm) ),*pStart,*pEnd);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetconesize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr){
*__ierr = DMPlexGetConeSize(
	(DM)PetscToPointer((dm) ),*p,size);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetconesize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr){
*__ierr = DMPlexSetConeSize(
	(DM)PetscToPointer((dm) ),*p,*size);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexaddconesize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr){
*__ierr = DMPlexAddConeSize(
	(DM)PetscToPointer((dm) ),*p,*size);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetcone_(DM dm,PetscInt *p, PetscInt cone[], int *__ierr){
*__ierr = DMPlexSetCone(
	(DM)PetscToPointer((dm) ),*p,cone);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetconeorientation_(DM dm,PetscInt *p, PetscInt coneOrientation[], int *__ierr){
*__ierr = DMPlexSetConeOrientation(
	(DM)PetscToPointer((dm) ),*p,coneOrientation);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexinsertcone_(DM dm,PetscInt *p,PetscInt *conePos,PetscInt *conePoint, int *__ierr){
*__ierr = DMPlexInsertCone(
	(DM)PetscToPointer((dm) ),*p,*conePos,*conePoint);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexinsertconeorientation_(DM dm,PetscInt *p,PetscInt *conePos,PetscInt *coneOrientation, int *__ierr){
*__ierr = DMPlexInsertConeOrientation(
	(DM)PetscToPointer((dm) ),*p,*conePos,*coneOrientation);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetsupportsize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr){
*__ierr = DMPlexGetSupportSize(
	(DM)PetscToPointer((dm) ),*p,size);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetsupportsize_(DM dm,PetscInt *p,PetscInt *size, int *__ierr){
*__ierr = DMPlexSetSupportSize(
	(DM)PetscToPointer((dm) ),*p,*size);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetsupport_(DM dm,PetscInt *p, PetscInt support[], int *__ierr){
*__ierr = DMPlexSetSupport(
	(DM)PetscToPointer((dm) ),*p,support);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexinsertsupport_(DM dm,PetscInt *p,PetscInt *supportPos,PetscInt *supportPoint, int *__ierr){
*__ierr = DMPlexInsertSupport(
	(DM)PetscToPointer((dm) ),*p,*supportPos,*supportPoint);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetmaxsizes_(DM dm,PetscInt *maxConeSize,PetscInt *maxSupportSize, int *__ierr){
*__ierr = DMPlexGetMaxSizes(
	(DM)PetscToPointer((dm) ),maxConeSize,maxSupportSize);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsymmetrize_(DM dm, int *__ierr){
*__ierr = DMPlexSymmetrize(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexstratify_(DM dm, int *__ierr){
*__ierr = DMPlexStratify(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetdepthlabel_(DM dm,DMLabel *depthLabel, int *__ierr){
*__ierr = DMPlexGetDepthLabel(
	(DM)PetscToPointer((dm) ),depthLabel);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetdepth_(DM dm,PetscInt *depth, int *__ierr){
*__ierr = DMPlexGetDepth(
	(DM)PetscToPointer((dm) ),depth);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetdepthstratum_(DM dm,PetscInt *stratumValue,PetscInt *start,PetscInt *end, int *__ierr){
*__ierr = DMPlexGetDepthStratum(
	(DM)PetscToPointer((dm) ),*stratumValue,start,end);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetheightstratum_(DM dm,PetscInt *stratumValue,PetscInt *start,PetscInt *end, int *__ierr){
*__ierr = DMPlexGetHeightStratum(
	(DM)PetscToPointer((dm) ),*stratumValue,start,end);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgethybridbounds_(DM dm,PetscInt *cMax,PetscInt *fMax,PetscInt *eMax,PetscInt *vMax, int *__ierr){
*__ierr = DMPlexGetHybridBounds(
	(DM)PetscToPointer((dm) ),cMax,fMax,eMax,vMax);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsethybridbounds_(DM dm,PetscInt *cMax,PetscInt *fMax,PetscInt *eMax,PetscInt *vMax, int *__ierr){
*__ierr = DMPlexSetHybridBounds(
	(DM)PetscToPointer((dm) ),*cMax,*fMax,*eMax,*vMax);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcreaterankfield_(DM dm,Vec *ranks, int *__ierr){
*__ierr = DMPlexCreateRankField(
	(DM)PetscToPointer((dm) ),ranks);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexchecksymmetry_(DM dm, int *__ierr){
*__ierr = DMPlexCheckSymmetry(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcheckskeleton_(DM dm,PetscBool *isSimplex,PetscInt *cellHeight, int *__ierr){
*__ierr = DMPlexCheckSkeleton(
	(DM)PetscToPointer((dm) ),*isSimplex,*cellHeight);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexcheckfaces_(DM dm,PetscBool *isSimplex,PetscInt *cellHeight, int *__ierr){
*__ierr = DMPlexCheckFaces(
	(DM)PetscToPointer((dm) ),*isSimplex,*cellHeight);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetregularrefinement_(DM dm,PetscBool *regular, int *__ierr){
*__ierr = DMPlexGetRegularRefinement(
	(DM)PetscToPointer((dm) ),regular);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetregularrefinement_(DM dm,PetscBool *regular, int *__ierr){
*__ierr = DMPlexSetRegularRefinement(
	(DM)PetscToPointer((dm) ),*regular);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetanchors_(DM dm,PetscSection *anchorSection,IS *anchorIS, int *__ierr){
*__ierr = DMPlexGetAnchors(
	(DM)PetscToPointer((dm) ),anchorSection,anchorIS);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetanchors_(DM dm,PetscSection anchorSection,IS anchorIS, int *__ierr){
*__ierr = DMPlexSetAnchors(
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((anchorSection) ),
	(IS)PetscToPointer((anchorIS) ));
}
#if defined(__cplusplus)
}
#endif

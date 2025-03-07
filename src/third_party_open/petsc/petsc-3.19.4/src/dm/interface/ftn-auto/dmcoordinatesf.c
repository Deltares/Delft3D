#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmcoordinates.c */
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

#include "petscdm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinatedm_ DMGETCOORDINATEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinatedm_ dmgetcoordinatedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcoordinatedm_ DMSETCOORDINATEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcoordinatedm_ dmsetcoordinatedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinatedm_ DMGETCELLCOORDINATEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinatedm_ dmgetcellcoordinatedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcellcoordinatedm_ DMSETCELLCOORDINATEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcellcoordinatedm_ dmsetcellcoordinatedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinatedim_ DMGETCOORDINATEDIM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinatedim_ dmgetcoordinatedim
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcoordinatedim_ DMSETCOORDINATEDIM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcoordinatedim_ dmsetcoordinatedim
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinatesection_ DMGETCOORDINATESECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinatesection_ dmgetcoordinatesection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcoordinatesection_ DMSETCOORDINATESECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcoordinatesection_ dmsetcoordinatesection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinatesection_ DMGETCELLCOORDINATESECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinatesection_ dmgetcellcoordinatesection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcellcoordinatesection_ DMSETCELLCOORDINATESECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcellcoordinatesection_ dmsetcellcoordinatesection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinates_ DMGETCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinates_ dmgetcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcoordinates_ DMSETCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcoordinates_ dmsetcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinates_ DMGETCELLCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinates_ dmgetcellcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcellcoordinates_ DMSETCELLCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcellcoordinates_ dmsetcellcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocalsetup_ DMGETCOORDINATESLOCALSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocalsetup_ dmgetcoordinateslocalsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocal_ DMGETCOORDINATESLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocal_ dmgetcoordinateslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocalnoncollective_ DMGETCOORDINATESLOCALNONCOLLECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocalnoncollective_ dmgetcoordinateslocalnoncollective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocaltuple_ DMGETCOORDINATESLOCALTUPLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocaltuple_ dmgetcoordinateslocaltuple
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcoordinateslocal_ DMSETCOORDINATESLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcoordinateslocal_ dmsetcoordinateslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinateslocalsetup_ DMGETCELLCOORDINATESLOCALSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinateslocalsetup_ dmgetcellcoordinateslocalsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinateslocal_ DMGETCELLCOORDINATESLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinateslocal_ dmgetcellcoordinateslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcellcoordinateslocalnoncollective_ DMGETCELLCOORDINATESLOCALNONCOLLECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcellcoordinateslocalnoncollective_ dmgetcellcoordinateslocalnoncollective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsetcellcoordinateslocal_ DMSETCELLCOORDINATESLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsetcellcoordinateslocal_ dmsetcellcoordinateslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetlocalboundingbox_ DMGETLOCALBOUNDINGBOX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetlocalboundingbox_ dmgetlocalboundingbox
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetboundingbox_ DMGETBOUNDINGBOX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetboundingbox_ dmgetboundingbox
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmprojectcoordinates_ DMPROJECTCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmprojectcoordinates_ dmprojectcoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocatepoints_ DMLOCATEPOINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocatepoints_ dmlocatepoints
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmgetcoordinatedm_(DM dm,DM *cdm, int *__ierr)
{
*__ierr = DMGetCoordinateDM(
	(DM)PetscToPointer((dm) ),cdm);
}
PETSC_EXTERN void  dmsetcoordinatedm_(DM dm,DM cdm, int *__ierr)
{
*__ierr = DMSetCoordinateDM(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((cdm) ));
}
PETSC_EXTERN void  dmgetcellcoordinatedm_(DM dm,DM *cdm, int *__ierr)
{
*__ierr = DMGetCellCoordinateDM(
	(DM)PetscToPointer((dm) ),cdm);
}
PETSC_EXTERN void  dmsetcellcoordinatedm_(DM dm,DM cdm, int *__ierr)
{
*__ierr = DMSetCellCoordinateDM(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((cdm) ));
}
PETSC_EXTERN void  dmgetcoordinatedim_(DM dm,PetscInt *dim, int *__ierr)
{
*__ierr = DMGetCoordinateDim(
	(DM)PetscToPointer((dm) ),dim);
}
PETSC_EXTERN void  dmsetcoordinatedim_(DM dm,PetscInt *dim, int *__ierr)
{
*__ierr = DMSetCoordinateDim(
	(DM)PetscToPointer((dm) ),*dim);
}
PETSC_EXTERN void  dmgetcoordinatesection_(DM dm,PetscSection *section, int *__ierr)
{
*__ierr = DMGetCoordinateSection(
	(DM)PetscToPointer((dm) ),section);
}
PETSC_EXTERN void  dmsetcoordinatesection_(DM dm,PetscInt *dim,PetscSection section, int *__ierr)
{
*__ierr = DMSetCoordinateSection(
	(DM)PetscToPointer((dm) ),*dim,
	(PetscSection)PetscToPointer((section) ));
}
PETSC_EXTERN void  dmgetcellcoordinatesection_(DM dm,PetscSection *section, int *__ierr)
{
*__ierr = DMGetCellCoordinateSection(
	(DM)PetscToPointer((dm) ),section);
}
PETSC_EXTERN void  dmsetcellcoordinatesection_(DM dm,PetscInt *dim,PetscSection section, int *__ierr)
{
*__ierr = DMSetCellCoordinateSection(
	(DM)PetscToPointer((dm) ),*dim,
	(PetscSection)PetscToPointer((section) ));
}
PETSC_EXTERN void  dmgetcoordinates_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCoordinates(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmsetcoordinates_(DM dm,Vec c, int *__ierr)
{
*__ierr = DMSetCoordinates(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  dmgetcellcoordinates_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCellCoordinates(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmsetcellcoordinates_(DM dm,Vec c, int *__ierr)
{
*__ierr = DMSetCellCoordinates(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  dmgetcoordinateslocalsetup_(DM dm, int *__ierr)
{
*__ierr = DMGetCoordinatesLocalSetUp(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmgetcoordinateslocal_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCoordinatesLocal(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmgetcoordinateslocalnoncollective_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCoordinatesLocalNoncollective(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmgetcoordinateslocaltuple_(DM dm,IS p,PetscSection *pCoordSection,Vec *pCoord, int *__ierr)
{
*__ierr = DMGetCoordinatesLocalTuple(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((p) ),pCoordSection,pCoord);
}
PETSC_EXTERN void  dmsetcoordinateslocal_(DM dm,Vec c, int *__ierr)
{
*__ierr = DMSetCoordinatesLocal(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  dmgetcellcoordinateslocalsetup_(DM dm, int *__ierr)
{
*__ierr = DMGetCellCoordinatesLocalSetUp(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmgetcellcoordinateslocal_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCellCoordinatesLocal(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmgetcellcoordinateslocalnoncollective_(DM dm,Vec *c, int *__ierr)
{
*__ierr = DMGetCellCoordinatesLocalNoncollective(
	(DM)PetscToPointer((dm) ),c);
}
PETSC_EXTERN void  dmsetcellcoordinateslocal_(DM dm,Vec c, int *__ierr)
{
*__ierr = DMSetCellCoordinatesLocal(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  dmgetlocalboundingbox_(DM dm,PetscReal lmin[],PetscReal lmax[], int *__ierr)
{
*__ierr = DMGetLocalBoundingBox(
	(DM)PetscToPointer((dm) ),lmin,lmax);
}
PETSC_EXTERN void  dmgetboundingbox_(DM dm,PetscReal gmin[],PetscReal gmax[], int *__ierr)
{
*__ierr = DMGetBoundingBox(
	(DM)PetscToPointer((dm) ),gmin,gmax);
}
PETSC_EXTERN void  dmprojectcoordinates_(DM dm,PetscFE disc, int *__ierr)
{
*__ierr = DMProjectCoordinates(
	(DM)PetscToPointer((dm) ),
	(PetscFE)PetscToPointer((disc) ));
}
PETSC_EXTERN void  dmlocatepoints_(DM dm,Vec v,DMPointLocationType *ltype,PetscSF *cellSF, int *__ierr)
{
*__ierr = DMLocatePoints(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((v) ),*ltype,cellSF);
}
#if defined(__cplusplus)
}
#endif

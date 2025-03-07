#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plextransform.c */
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

#include "petscdmplextransform.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformcreate_ DMPLEXTRANSFORMCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformcreate_ dmplextransformcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformsetfromoptions_ DMPLEXTRANSFORMSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformsetfromoptions_ dmplextransformsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformsetdimensions_ DMPLEXTRANSFORMSETDIMENSIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformsetdimensions_ dmplextransformsetdimensions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformgettargetpoint_ DMPLEXTRANSFORMGETTARGETPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformgettargetpoint_ dmplextransformgettargetpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformgetsourcepoint_ DMPLEXTRANSFORMGETSOURCEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformgetsourcepoint_ dmplextransformgetsourcepoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformcelltransform_ DMPLEXTRANSFORMCELLTRANSFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformcelltransform_ dmplextransformcelltransform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformgetsubcellorientation_ DMPLEXTRANSFORMGETSUBCELLORIENTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformgetsubcellorientation_ dmplextransformgetsubcellorientation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransformmapcoordinates_ DMPLEXTRANSFORMMAPCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransformmapcoordinates_ dmplextransformmapcoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplextransformcreate_(MPI_Fint * comm,DMPlexTransform *tr, int *__ierr)
{
*__ierr = DMPlexTransformCreate(
	MPI_Comm_f2c(*(comm)),tr);
}
PETSC_EXTERN void  dmplextransformsetfromoptions_(DMPlexTransform tr, int *__ierr)
{
*__ierr = DMPlexTransformSetFromOptions(
	(DMPlexTransform)PetscToPointer((tr) ));
}
PETSC_EXTERN void  dmplextransformsetdimensions_(DMPlexTransform tr,DM dm,DM tdm, int *__ierr)
{
*__ierr = DMPlexTransformSetDimensions(
	(DMPlexTransform)PetscToPointer((tr) ),
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((tdm) ));
}
PETSC_EXTERN void  dmplextransformgettargetpoint_(DMPlexTransform tr,DMPolytopeType *ct,DMPolytopeType *ctNew,PetscInt *p,PetscInt *r,PetscInt *pNew, int *__ierr)
{
*__ierr = DMPlexTransformGetTargetPoint(
	(DMPlexTransform)PetscToPointer((tr) ),*ct,*ctNew,*p,*r,pNew);
}
PETSC_EXTERN void  dmplextransformgetsourcepoint_(DMPlexTransform tr,PetscInt *pNew,DMPolytopeType *ct,DMPolytopeType *ctNew,PetscInt *p,PetscInt *r, int *__ierr)
{
*__ierr = DMPlexTransformGetSourcePoint(
	(DMPlexTransform)PetscToPointer((tr) ),*pNew,ct,ctNew,p,r);
}
PETSC_EXTERN void  dmplextransformcelltransform_(DMPlexTransform tr,DMPolytopeType *source,PetscInt *p,PetscInt *rt,PetscInt *Nt,DMPolytopeType *target[],PetscInt *size[],PetscInt *cone[],PetscInt *ornt[], int *__ierr)
{
*__ierr = DMPlexTransformCellTransform(
	(DMPlexTransform)PetscToPointer((tr) ),*source,*p,rt,Nt,target,size,cone,ornt);
}
PETSC_EXTERN void  dmplextransformgetsubcellorientation_(DMPlexTransform tr,DMPolytopeType *sct,PetscInt *sp,PetscInt *so,DMPolytopeType *tct,PetscInt *r,PetscInt *o,PetscInt *rnew,PetscInt *onew, int *__ierr)
{
*__ierr = DMPlexTransformGetSubcellOrientation(
	(DMPlexTransform)PetscToPointer((tr) ),*sct,*sp,*so,*tct,*r,*o,rnew,onew);
}
PETSC_EXTERN void  dmplextransformmapcoordinates_(DMPlexTransform tr,DMPolytopeType *pct,DMPolytopeType *ct,PetscInt *p,PetscInt *r,PetscInt *Nv,PetscInt *dE, PetscScalar in[],PetscScalar out[], int *__ierr)
{
*__ierr = DMPlexTransformMapCoordinates(
	(DMPlexTransform)PetscToPointer((tr) ),*pct,*ct,*p,*r,*Nv,*dE,in,out);
}
#if defined(__cplusplus)
}
#endif

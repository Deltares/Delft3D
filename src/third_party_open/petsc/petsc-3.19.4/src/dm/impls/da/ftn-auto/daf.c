#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* da.c */
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

#include "petscdmda.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetsizes_ DMDASETSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetsizes_ dmdasetsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetnumprocs_ DMDASETNUMPROCS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetnumprocs_ dmdasetnumprocs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetboundarytype_ DMDASETBOUNDARYTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetboundarytype_ dmdasetboundarytype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetdof_ DMDASETDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetdof_ dmdasetdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetdof_ DMDAGETDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetdof_ dmdagetdof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetoverlap_ DMDAGETOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetoverlap_ dmdagetoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetoverlap_ DMDASETOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetoverlap_ dmdasetoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetnumlocalsubdomains_ DMDAGETNUMLOCALSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetnumlocalsubdomains_ dmdagetnumlocalsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetnumlocalsubdomains_ DMDASETNUMLOCALSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetnumlocalsubdomains_ dmdasetnumlocalsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetoffset_ DMDASETOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetoffset_ dmdasetoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetoffset_ DMDAGETOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetoffset_ dmdagetoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetnonoverlappingregion_ DMDAGETNONOVERLAPPINGREGION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetnonoverlappingregion_ dmdagetnonoverlappingregion
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetnonoverlappingregion_ DMDASETNONOVERLAPPINGREGION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetnonoverlappingregion_ dmdasetnonoverlappingregion
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetstenciltype_ DMDASETSTENCILTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetstenciltype_ dmdasetstenciltype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetstenciltype_ DMDAGETSTENCILTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetstenciltype_ dmdagetstenciltype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetstencilwidth_ DMDASETSTENCILWIDTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetstencilwidth_ dmdasetstencilwidth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetstencilwidth_ DMDAGETSTENCILWIDTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetstencilwidth_ dmdagetstencilwidth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetownershipranges_ DMDASETOWNERSHIPRANGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetownershipranges_ dmdasetownershipranges
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetinterpolationtype_ DMDASETINTERPOLATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetinterpolationtype_ dmdasetinterpolationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetinterpolationtype_ DMDAGETINTERPOLATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetinterpolationtype_ dmdagetinterpolationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetrefinementfactor_ DMDASETREFINEMENTFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetrefinementfactor_ dmdasetrefinementfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdamapmatstenciltoglobal_ DMDAMAPMATSTENCILTOGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdamapmatstenciltoglobal_ dmdamapmatstenciltoglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetgllcoordinates_ DMDASETGLLCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetgllcoordinates_ dmdasetgllcoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdasetsizes_(DM da,PetscInt *M,PetscInt *N,PetscInt *P, int *__ierr)
{
*__ierr = DMDASetSizes(
	(DM)PetscToPointer((da) ),*M,*N,*P);
}
PETSC_EXTERN void  dmdasetnumprocs_(DM da,PetscInt *m,PetscInt *n,PetscInt *p, int *__ierr)
{
*__ierr = DMDASetNumProcs(
	(DM)PetscToPointer((da) ),*m,*n,*p);
}
PETSC_EXTERN void  dmdasetboundarytype_(DM da,DMBoundaryType *bx,DMBoundaryType *by,DMBoundaryType *bz, int *__ierr)
{
*__ierr = DMDASetBoundaryType(
	(DM)PetscToPointer((da) ),*bx,*by,*bz);
}
PETSC_EXTERN void  dmdasetdof_(DM da,PetscInt *dof, int *__ierr)
{
*__ierr = DMDASetDof(
	(DM)PetscToPointer((da) ),*dof);
}
PETSC_EXTERN void  dmdagetdof_(DM da,PetscInt *dof, int *__ierr)
{
*__ierr = DMDAGetDof(
	(DM)PetscToPointer((da) ),dof);
}
PETSC_EXTERN void  dmdagetoverlap_(DM da,PetscInt *x,PetscInt *y,PetscInt *z, int *__ierr)
{
*__ierr = DMDAGetOverlap(
	(DM)PetscToPointer((da) ),x,y,z);
}
PETSC_EXTERN void  dmdasetoverlap_(DM da,PetscInt *x,PetscInt *y,PetscInt *z, int *__ierr)
{
*__ierr = DMDASetOverlap(
	(DM)PetscToPointer((da) ),*x,*y,*z);
}
PETSC_EXTERN void  dmdagetnumlocalsubdomains_(DM da,PetscInt *Nsub, int *__ierr)
{
*__ierr = DMDAGetNumLocalSubDomains(
	(DM)PetscToPointer((da) ),Nsub);
}
PETSC_EXTERN void  dmdasetnumlocalsubdomains_(DM da,PetscInt *Nsub, int *__ierr)
{
*__ierr = DMDASetNumLocalSubDomains(
	(DM)PetscToPointer((da) ),*Nsub);
}
PETSC_EXTERN void  dmdasetoffset_(DM da,PetscInt *xo,PetscInt *yo,PetscInt *zo,PetscInt *Mo,PetscInt *No,PetscInt *Po, int *__ierr)
{
*__ierr = DMDASetOffset(
	(DM)PetscToPointer((da) ),*xo,*yo,*zo,*Mo,*No,*Po);
}
PETSC_EXTERN void  dmdagetoffset_(DM da,PetscInt *xo,PetscInt *yo,PetscInt *zo,PetscInt *Mo,PetscInt *No,PetscInt *Po, int *__ierr)
{
*__ierr = DMDAGetOffset(
	(DM)PetscToPointer((da) ),xo,yo,zo,Mo,No,Po);
}
PETSC_EXTERN void  dmdagetnonoverlappingregion_(DM da,PetscInt *xs,PetscInt *ys,PetscInt *zs,PetscInt *xm,PetscInt *ym,PetscInt *zm, int *__ierr)
{
*__ierr = DMDAGetNonOverlappingRegion(
	(DM)PetscToPointer((da) ),xs,ys,zs,xm,ym,zm);
}
PETSC_EXTERN void  dmdasetnonoverlappingregion_(DM da,PetscInt *xs,PetscInt *ys,PetscInt *zs,PetscInt *xm,PetscInt *ym,PetscInt *zm, int *__ierr)
{
*__ierr = DMDASetNonOverlappingRegion(
	(DM)PetscToPointer((da) ),*xs,*ys,*zs,*xm,*ym,*zm);
}
PETSC_EXTERN void  dmdasetstenciltype_(DM da,DMDAStencilType *stype, int *__ierr)
{
*__ierr = DMDASetStencilType(
	(DM)PetscToPointer((da) ),*stype);
}
PETSC_EXTERN void  dmdagetstenciltype_(DM da,DMDAStencilType *stype, int *__ierr)
{
*__ierr = DMDAGetStencilType(
	(DM)PetscToPointer((da) ),stype);
}
PETSC_EXTERN void  dmdasetstencilwidth_(DM da,PetscInt *width, int *__ierr)
{
*__ierr = DMDASetStencilWidth(
	(DM)PetscToPointer((da) ),*width);
}
PETSC_EXTERN void  dmdagetstencilwidth_(DM da,PetscInt *width, int *__ierr)
{
*__ierr = DMDAGetStencilWidth(
	(DM)PetscToPointer((da) ),width);
}
PETSC_EXTERN void  dmdasetownershipranges_(DM da, PetscInt lx[], PetscInt ly[], PetscInt lz[], int *__ierr)
{
*__ierr = DMDASetOwnershipRanges(
	(DM)PetscToPointer((da) ),lx,ly,lz);
}
PETSC_EXTERN void  dmdasetinterpolationtype_(DM da,DMDAInterpolationType *ctype, int *__ierr)
{
*__ierr = DMDASetInterpolationType(
	(DM)PetscToPointer((da) ),*ctype);
}
PETSC_EXTERN void  dmdagetinterpolationtype_(DM da,DMDAInterpolationType *ctype, int *__ierr)
{
*__ierr = DMDAGetInterpolationType(
	(DM)PetscToPointer((da) ),ctype);
}
PETSC_EXTERN void  dmdasetrefinementfactor_(DM da,PetscInt *refine_x,PetscInt *refine_y,PetscInt *refine_z, int *__ierr)
{
*__ierr = DMDASetRefinementFactor(
	(DM)PetscToPointer((da) ),*refine_x,*refine_y,*refine_z);
}
PETSC_EXTERN void  dmdamapmatstenciltoglobal_(DM da,PetscInt *m, MatStencil idxm[],PetscInt gidxm[], int *__ierr)
{
*__ierr = DMDAMapMatStencilToGlobal(
	(DM)PetscToPointer((da) ),*m,idxm,gidxm);
}
PETSC_EXTERN void  dmdasetgllcoordinates_(DM da,PetscInt *n,PetscReal *nodes, int *__ierr)
{
*__ierr = DMDASetGLLCoordinates(
	(DM)PetscToPointer((da) ),*n,nodes);
}
#if defined(__cplusplus)
}
#endif

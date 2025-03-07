#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dscatter.c */
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

#include "petscdraw.h"
#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsetdimension_ PETSCDRAWSPSETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsetdimension_ petscdrawspsetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspgetdimension_ PETSCDRAWSPGETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspgetdimension_ petscdrawspgetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspreset_ PETSCDRAWSPRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspreset_ petscdrawspreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspdestroy_ PETSCDRAWSPDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspdestroy_ petscdrawspdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspaddpoint_ PETSCDRAWSPADDPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspaddpoint_ petscdrawspaddpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspaddpointcolorized_ PETSCDRAWSPADDPOINTCOLORIZED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspaddpointcolorized_ petscdrawspaddpointcolorized
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspdraw_ PETSCDRAWSPDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspdraw_ petscdrawspdraw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsave_ PETSCDRAWSPSAVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsave_ petscdrawspsave
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspsetlimits_ PETSCDRAWSPSETLIMITS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspsetlimits_ petscdrawspsetlimits
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspgetaxis_ PETSCDRAWSPGETAXIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspgetaxis_ petscdrawspgetaxis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawspgetdraw_ PETSCDRAWSPGETDRAW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawspgetdraw_ petscdrawspgetdraw
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawspsetdimension_(PetscDrawSP sp,int *dim, int *__ierr)
{
*__ierr = PetscDrawSPSetDimension(
	(PetscDrawSP)PetscToPointer((sp) ),*dim);
}
PETSC_EXTERN void  petscdrawspgetdimension_(PetscDrawSP sp,int *dim, int *__ierr)
{
*__ierr = PetscDrawSPGetDimension(
	(PetscDrawSP)PetscToPointer((sp) ),dim);
}
PETSC_EXTERN void  petscdrawspreset_(PetscDrawSP sp, int *__ierr)
{
*__ierr = PetscDrawSPReset(
	(PetscDrawSP)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscdrawspdestroy_(PetscDrawSP *sp, int *__ierr)
{
*__ierr = PetscDrawSPDestroy(sp);
}
PETSC_EXTERN void  petscdrawspaddpoint_(PetscDrawSP sp,PetscReal *x,PetscReal *y, int *__ierr)
{
*__ierr = PetscDrawSPAddPoint(
	(PetscDrawSP)PetscToPointer((sp) ),x,y);
}
PETSC_EXTERN void  petscdrawspaddpointcolorized_(PetscDrawSP sp,PetscReal *x,PetscReal *y,PetscReal *z, int *__ierr)
{
*__ierr = PetscDrawSPAddPointColorized(
	(PetscDrawSP)PetscToPointer((sp) ),x,y,z);
}
PETSC_EXTERN void  petscdrawspdraw_(PetscDrawSP sp,PetscBool *clear, int *__ierr)
{
*__ierr = PetscDrawSPDraw(
	(PetscDrawSP)PetscToPointer((sp) ),*clear);
}
PETSC_EXTERN void  petscdrawspsave_(PetscDrawSP sp, int *__ierr)
{
*__ierr = PetscDrawSPSave(
	(PetscDrawSP)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscdrawspsetlimits_(PetscDrawSP sp,PetscReal *x_min,PetscReal *x_max,PetscReal *y_min,PetscReal *y_max, int *__ierr)
{
*__ierr = PetscDrawSPSetLimits(
	(PetscDrawSP)PetscToPointer((sp) ),*x_min,*x_max,*y_min,*y_max);
}
PETSC_EXTERN void  petscdrawspgetaxis_(PetscDrawSP sp,PetscDrawAxis *axis, int *__ierr)
{
*__ierr = PetscDrawSPGetAxis(
	(PetscDrawSP)PetscToPointer((sp) ),axis);
}
PETSC_EXTERN void  petscdrawspgetdraw_(PetscDrawSP sp,PetscDraw *draw, int *__ierr)
{
*__ierr = PetscDrawSPGetDraw(
	(PetscDrawSP)PetscToPointer((sp) ),draw);
}
#if defined(__cplusplus)
}
#endif

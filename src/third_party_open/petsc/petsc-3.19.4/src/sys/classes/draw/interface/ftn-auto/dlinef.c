#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dline.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetboundingbox_ PETSCDRAWGETBOUNDINGBOX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetboundingbox_ petscdrawgetboundingbox
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetcurrentpoint_ PETSCDRAWGETCURRENTPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetcurrentpoint_ petscdrawgetcurrentpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawsetcurrentpoint_ PETSCDRAWSETCURRENTPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawsetcurrentpoint_ petscdrawsetcurrentpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawpushcurrentpoint_ PETSCDRAWPUSHCURRENTPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpushcurrentpoint_ petscdrawpushcurrentpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawpopcurrentpoint_ PETSCDRAWPOPCURRENTPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpopcurrentpoint_ petscdrawpopcurrentpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawline_ PETSCDRAWLINE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawline_ petscdrawline
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawarrow_ PETSCDRAWARROW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawarrow_ petscdrawarrow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlinesetwidth_ PETSCDRAWLINESETWIDTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlinesetwidth_ petscdrawlinesetwidth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawlinegetwidth_ PETSCDRAWLINEGETWIDTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawlinegetwidth_ petscdrawlinegetwidth
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawgetboundingbox_(PetscDraw draw,PetscReal *xl,PetscReal *yl,PetscReal *xr,PetscReal *yr, int *__ierr)
{
*__ierr = PetscDrawGetBoundingBox(
	(PetscDraw)PetscToPointer((draw) ),xl,yl,xr,yr);
}
PETSC_EXTERN void  petscdrawgetcurrentpoint_(PetscDraw draw,PetscReal *x,PetscReal *y, int *__ierr)
{
*__ierr = PetscDrawGetCurrentPoint(
	(PetscDraw)PetscToPointer((draw) ),x,y);
}
PETSC_EXTERN void  petscdrawsetcurrentpoint_(PetscDraw draw,PetscReal *x,PetscReal *y, int *__ierr)
{
*__ierr = PetscDrawSetCurrentPoint(
	(PetscDraw)PetscToPointer((draw) ),*x,*y);
}
PETSC_EXTERN void  petscdrawpushcurrentpoint_(PetscDraw draw,PetscReal *x,PetscReal *y, int *__ierr)
{
*__ierr = PetscDrawPushCurrentPoint(
	(PetscDraw)PetscToPointer((draw) ),*x,*y);
}
PETSC_EXTERN void  petscdrawpopcurrentpoint_(PetscDraw draw, int *__ierr)
{
*__ierr = PetscDrawPopCurrentPoint(
	(PetscDraw)PetscToPointer((draw) ));
}
PETSC_EXTERN void  petscdrawline_(PetscDraw draw,PetscReal *xl,PetscReal *yl,PetscReal *xr,PetscReal *yr,int *cl, int *__ierr)
{
*__ierr = PetscDrawLine(
	(PetscDraw)PetscToPointer((draw) ),*xl,*yl,*xr,*yr,*cl);
}
PETSC_EXTERN void  petscdrawarrow_(PetscDraw draw,PetscReal *xl,PetscReal *yl,PetscReal *xr,PetscReal *yr,int *cl, int *__ierr)
{
*__ierr = PetscDrawArrow(
	(PetscDraw)PetscToPointer((draw) ),*xl,*yl,*xr,*yr,*cl);
}
PETSC_EXTERN void  petscdrawlinesetwidth_(PetscDraw draw,PetscReal *width, int *__ierr)
{
*__ierr = PetscDrawLineSetWidth(
	(PetscDraw)PetscToPointer((draw) ),*width);
}
PETSC_EXTERN void  petscdrawlinegetwidth_(PetscDraw draw,PetscReal *width, int *__ierr)
{
*__ierr = PetscDrawLineGetWidth(
	(PetscDraw)PetscToPointer((draw) ),width);
}
#if defined(__cplusplus)
}
#endif

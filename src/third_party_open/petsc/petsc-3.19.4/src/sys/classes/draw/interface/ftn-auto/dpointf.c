#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dpoint.c */
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
#define petscdrawpoint_ PETSCDRAWPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpoint_ petscdrawpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawpointpixel_ PETSCDRAWPOINTPIXEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpointpixel_ petscdrawpointpixel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawpointsetsize_ PETSCDRAWPOINTSETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpointsetsize_ petscdrawpointsetsize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawpoint_(PetscDraw draw,PetscReal *xl,PetscReal *yl,int *cl, int *__ierr)
{
*__ierr = PetscDrawPoint(
	(PetscDraw)PetscToPointer((draw) ),*xl,*yl,*cl);
}
PETSC_EXTERN void  petscdrawpointpixel_(PetscDraw draw,int *x,int *y,int *c, int *__ierr)
{
*__ierr = PetscDrawPointPixel(
	(PetscDraw)PetscToPointer((draw) ),*x,*y,*c);
}
PETSC_EXTERN void  petscdrawpointsetsize_(PetscDraw draw,PetscReal *width, int *__ierr)
{
*__ierr = PetscDrawPointSetSize(
	(PetscDraw)PetscToPointer((draw) ),*width);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dtri.c */
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
#define petscdrawtriangle_ PETSCDRAWTRIANGLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawtriangle_ petscdrawtriangle
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawscalepopup_ PETSCDRAWSCALEPOPUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawscalepopup_ petscdrawscalepopup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawtensorcontourpatch_ PETSCDRAWTENSORCONTOURPATCH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawtensorcontourpatch_ petscdrawtensorcontourpatch
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawtriangle_(PetscDraw draw,PetscReal *x1,PetscReal *y_1,PetscReal *x2,PetscReal *y2,PetscReal *x3,PetscReal *y3,int *c1,int *c2,int *c3, int *__ierr)
{
*__ierr = PetscDrawTriangle(
	(PetscDraw)PetscToPointer((draw) ),*x1,*y_1,*x2,*y2,*x3,*y3,*c1,*c2,*c3);
}
PETSC_EXTERN void  petscdrawscalepopup_(PetscDraw popup,PetscReal *min,PetscReal *max, int *__ierr)
{
*__ierr = PetscDrawScalePopup(
	(PetscDraw)PetscToPointer((popup) ),*min,*max);
}
PETSC_EXTERN void  petscdrawtensorcontourpatch_(PetscDraw draw,int *m,int *n,PetscReal *x,PetscReal *y,PetscReal *min,PetscReal *max,PetscReal *v, int *__ierr)
{
*__ierr = PetscDrawTensorContourPatch(
	(PetscDraw)PetscToPointer((draw) ),*m,*n,x,y,*min,*max,v);
}
#if defined(__cplusplus)
}
#endif

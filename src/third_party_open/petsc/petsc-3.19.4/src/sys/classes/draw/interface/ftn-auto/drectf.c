#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* drect.c */
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
#define petscdrawrectangle_ PETSCDRAWRECTANGLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawrectangle_ petscdrawrectangle
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawrectangle_(PetscDraw draw,PetscReal *xl,PetscReal *yl,PetscReal *xr,PetscReal *yr,int *c1,int *c2,int *c3,int *c4, int *__ierr)
{
*__ierr = PetscDrawRectangle(
	(PetscDraw)PetscToPointer((draw) ),*xl,*yl,*xr,*yr,*c1,*c2,*c3,*c4);
}
#if defined(__cplusplus)
}
#endif

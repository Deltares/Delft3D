#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dtext.c */
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
#define petscdrawstringsetsize_ PETSCDRAWSTRINGSETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawstringsetsize_ petscdrawstringsetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawstringgetsize_ PETSCDRAWSTRINGGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawstringgetsize_ petscdrawstringgetsize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawstringsetsize_(PetscDraw draw,PetscReal *width,PetscReal *height, int *__ierr)
{
*__ierr = PetscDrawStringSetSize(
	(PetscDraw)PetscToPointer((draw) ),*width,*height);
}
PETSC_EXTERN void  petscdrawstringgetsize_(PetscDraw draw,PetscReal *width,PetscReal *height, int *__ierr)
{
*__ierr = PetscDrawStringGetSize(
	(PetscDraw)PetscToPointer((draw) ),width,height);
}
#if defined(__cplusplus)
}
#endif

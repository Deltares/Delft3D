#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dpause.c */
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
#define petscdrawpause_ PETSCDRAWPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawpause_ petscdrawpause
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawsetpause_ PETSCDRAWSETPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawsetpause_ petscdrawsetpause
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetpause_ PETSCDRAWGETPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetpause_ petscdrawgetpause
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawpause_(PetscDraw draw, int *__ierr)
{
*__ierr = PetscDrawPause(
	(PetscDraw)PetscToPointer((draw) ));
}
PETSC_EXTERN void  petscdrawsetpause_(PetscDraw draw,PetscReal *lpause, int *__ierr)
{
*__ierr = PetscDrawSetPause(
	(PetscDraw)PetscToPointer((draw) ),*lpause);
}
PETSC_EXTERN void  petscdrawgetpause_(PetscDraw draw,PetscReal *lpause, int *__ierr)
{
*__ierr = PetscDrawGetPause(
	(PetscDraw)PetscToPointer((draw) ),lpause);
}
#if defined(__cplusplus)
}
#endif

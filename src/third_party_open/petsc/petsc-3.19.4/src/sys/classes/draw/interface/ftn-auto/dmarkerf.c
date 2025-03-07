#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmarker.c */
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
#define petscdrawmarker_ PETSCDRAWMARKER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawmarker_ petscdrawmarker
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawsetmarkertype_ PETSCDRAWSETMARKERTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawsetmarkertype_ petscdrawsetmarkertype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetmarkertype_ PETSCDRAWGETMARKERTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetmarkertype_ petscdrawgetmarkertype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawmarker_(PetscDraw draw,PetscReal *xl,PetscReal *yl,int *cl, int *__ierr)
{
*__ierr = PetscDrawMarker(
	(PetscDraw)PetscToPointer((draw) ),*xl,*yl,*cl);
}
PETSC_EXTERN void  petscdrawsetmarkertype_(PetscDraw draw,PetscDrawMarkerType *mtype, int *__ierr)
{
*__ierr = PetscDrawSetMarkerType(
	(PetscDraw)PetscToPointer((draw) ),*mtype);
}
PETSC_EXTERN void  petscdrawgetmarkertype_(PetscDraw draw,PetscDrawMarkerType *mtype, int *__ierr)
{
*__ierr = PetscDrawGetMarkerType(
	(PetscDraw)PetscToPointer((draw) ),mtype);
}
#if defined(__cplusplus)
}
#endif

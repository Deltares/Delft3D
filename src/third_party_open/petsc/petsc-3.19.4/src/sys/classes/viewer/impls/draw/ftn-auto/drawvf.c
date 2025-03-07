#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* drawv.c */
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
#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawclear_ PETSCVIEWERDRAWCLEAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawclear_ petscviewerdrawclear
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawgetpause_ PETSCVIEWERDRAWGETPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawgetpause_ petscviewerdrawgetpause
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawsetpause_ PETSCVIEWERDRAWSETPAUSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawsetpause_ petscviewerdrawsetpause
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawsethold_ PETSCVIEWERDRAWSETHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawsethold_ petscviewerdrawsethold
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawgethold_ PETSCVIEWERDRAWGETHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawgethold_ petscviewerdrawgethold
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdrawsetbounds_ PETSCVIEWERDRAWSETBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdrawsetbounds_ petscviewerdrawsetbounds
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerdrawclear_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerDrawClear(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscviewerdrawgetpause_(PetscViewer viewer,PetscReal *pause, int *__ierr)
{
*__ierr = PetscViewerDrawGetPause(
	(PetscViewer)PetscToPointer((viewer) ),pause);
}
PETSC_EXTERN void  petscviewerdrawsetpause_(PetscViewer viewer,PetscReal *pause, int *__ierr)
{
*__ierr = PetscViewerDrawSetPause(
	(PetscViewer)PetscToPointer((viewer) ),*pause);
}
PETSC_EXTERN void  petscviewerdrawsethold_(PetscViewer viewer,PetscBool *hold, int *__ierr)
{
*__ierr = PetscViewerDrawSetHold(
	(PetscViewer)PetscToPointer((viewer) ),*hold);
}
PETSC_EXTERN void  petscviewerdrawgethold_(PetscViewer viewer,PetscBool *hold, int *__ierr)
{
*__ierr = PetscViewerDrawGetHold(
	(PetscViewer)PetscToPointer((viewer) ),hold);
}
PETSC_EXTERN void  petscviewerdrawsetbounds_(PetscViewer viewer,PetscInt *nbounds, PetscReal *bounds, int *__ierr)
{
*__ierr = PetscViewerDrawSetBounds(
	(PetscViewer)PetscToPointer((viewer) ),*nbounds,bounds);
}
#if defined(__cplusplus)
}
#endif

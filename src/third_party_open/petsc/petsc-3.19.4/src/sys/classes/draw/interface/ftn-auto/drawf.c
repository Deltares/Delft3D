#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* draw.c */
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
#define petscdrawresizewindow_ PETSCDRAWRESIZEWINDOW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawresizewindow_ petscdrawresizewindow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetwindowsize_ PETSCDRAWGETWINDOWSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetwindowsize_ petscdrawgetwindowsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawcheckresizedwindow_ PETSCDRAWCHECKRESIZEDWINDOW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawcheckresizedwindow_ petscdrawcheckresizedwindow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawdestroy_ PETSCDRAWDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawdestroy_ petscdrawdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawgetpopup_ PETSCDRAWGETPOPUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawgetpopup_ petscdrawgetpopup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdrawsetdoublebuffer_ PETSCDRAWSETDOUBLEBUFFER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdrawsetdoublebuffer_ petscdrawsetdoublebuffer
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdrawresizewindow_(PetscDraw draw,int *w,int *h, int *__ierr)
{
*__ierr = PetscDrawResizeWindow(
	(PetscDraw)PetscToPointer((draw) ),*w,*h);
}
PETSC_EXTERN void  petscdrawgetwindowsize_(PetscDraw draw,int *w,int *h, int *__ierr)
{
*__ierr = PetscDrawGetWindowSize(
	(PetscDraw)PetscToPointer((draw) ),w,h);
}
PETSC_EXTERN void  petscdrawcheckresizedwindow_(PetscDraw draw, int *__ierr)
{
*__ierr = PetscDrawCheckResizedWindow(
	(PetscDraw)PetscToPointer((draw) ));
}
PETSC_EXTERN void  petscdrawdestroy_(PetscDraw *draw, int *__ierr)
{
*__ierr = PetscDrawDestroy(draw);
}
PETSC_EXTERN void  petscdrawgetpopup_(PetscDraw draw,PetscDraw *popup, int *__ierr)
{
*__ierr = PetscDrawGetPopup(
	(PetscDraw)PetscToPointer((draw) ),popup);
}
PETSC_EXTERN void  petscdrawsetdoublebuffer_(PetscDraw draw, int *__ierr)
{
*__ierr = PetscDrawSetDoubleBuffer(
	(PetscDraw)PetscToPointer((draw) ));
}
#if defined(__cplusplus)
}
#endif

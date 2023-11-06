#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tsevent.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssetposteventintervalstep_ TSSETPOSTEVENTINTERVALSTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssetposteventintervalstep_ tssetposteventintervalstep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsseteventtolerances_ TSSETEVENTTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsseteventtolerances_ tsseteventtolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsgetnumevents_ TSGETNUMEVENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsgetnumevents_ tsgetnumevents
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tssetposteventintervalstep_(TS ts,PetscReal *dt, int *__ierr)
{
*__ierr = TSSetPostEventIntervalStep(
	(TS)PetscToPointer((ts) ),*dt);
}
PETSC_EXTERN void  tsseteventtolerances_(TS ts,PetscReal *tol,PetscReal vtol[], int *__ierr)
{
*__ierr = TSSetEventTolerances(
	(TS)PetscToPointer((ts) ),*tol,vtol);
}
PETSC_EXTERN void  tsgetnumevents_(TS ts,PetscInt *nevents, int *__ierr)
{
*__ierr = TSGetNumEvents(
	(TS)PetscToPointer((ts) ),nevents);
}
#if defined(__cplusplus)
}
#endif

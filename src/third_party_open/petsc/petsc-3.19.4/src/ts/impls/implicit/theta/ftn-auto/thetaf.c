#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* theta.c */
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
#define tsthetagettheta_ TSTHETAGETTHETA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsthetagettheta_ tsthetagettheta
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsthetasettheta_ TSTHETASETTHETA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsthetasettheta_ tsthetasettheta
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsthetagetendpoint_ TSTHETAGETENDPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsthetagetendpoint_ tsthetagetendpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsthetasetendpoint_ TSTHETASETENDPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsthetasetendpoint_ tsthetasetendpoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsthetagettheta_(TS ts,PetscReal *theta, int *__ierr)
{
*__ierr = TSThetaGetTheta(
	(TS)PetscToPointer((ts) ),theta);
}
PETSC_EXTERN void  tsthetasettheta_(TS ts,PetscReal *theta, int *__ierr)
{
*__ierr = TSThetaSetTheta(
	(TS)PetscToPointer((ts) ),*theta);
}
PETSC_EXTERN void  tsthetagetendpoint_(TS ts,PetscBool *endpoint, int *__ierr)
{
*__ierr = TSThetaGetEndpoint(
	(TS)PetscToPointer((ts) ),endpoint);
}
PETSC_EXTERN void  tsthetasetendpoint_(TS ts,PetscBool *flg, int *__ierr)
{
*__ierr = TSThetaSetEndpoint(
	(TS)PetscToPointer((ts) ),*flg);
}
#if defined(__cplusplus)
}
#endif

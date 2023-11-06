#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snesgs.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesngssettolerances_ SNESNGSSETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesngssettolerances_ snesngssettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesngsgettolerances_ SNESNGSGETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesngsgettolerances_ snesngsgettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesngssetsweeps_ SNESNGSSETSWEEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesngssetsweeps_ snesngssetsweeps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesngsgetsweeps_ SNESNGSGETSWEEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesngsgetsweeps_ snesngsgetsweeps
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesngssettolerances_(SNES snes,PetscReal *abstol,PetscReal *rtol,PetscReal *stol,PetscInt *maxit, int *__ierr)
{
*__ierr = SNESNGSSetTolerances(
	(SNES)PetscToPointer((snes) ),*abstol,*rtol,*stol,*maxit);
}
PETSC_EXTERN void  snesngsgettolerances_(SNES snes,PetscReal *atol,PetscReal *rtol,PetscReal *stol,PetscInt *maxit, int *__ierr)
{
*__ierr = SNESNGSGetTolerances(
	(SNES)PetscToPointer((snes) ),atol,rtol,stol,maxit);
}

PETSC_EXTERN void  snesngssetsweeps_(SNES snes,PetscInt *sweeps, int *__ierr)
{
*__ierr = SNESNGSSetSweeps(
	(SNES)PetscToPointer((snes) ),*sweeps);
}
PETSC_EXTERN void  snesngsgetsweeps_(SNES snes,PetscInt *sweeps, int *__ierr)
{
*__ierr = SNESNGSGetSweeps(
	(SNES)PetscToPointer((snes) ),sweeps);
}
#if defined(__cplusplus)
}
#endif

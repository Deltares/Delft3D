#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* posindep.c */
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
#define tspseudoverifytimestep_ TSPSEUDOVERIFYTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspseudoverifytimestep_ tspseudoverifytimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspseudosettimestepincrement_ TSPSEUDOSETTIMESTEPINCREMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspseudosettimestepincrement_ tspseudosettimestepincrement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspseudosetmaxtimestep_ TSPSEUDOSETMAXTIMESTEP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspseudosetmaxtimestep_ tspseudosetmaxtimestep
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tspseudoincrementdtfrominitialdt_ TSPSEUDOINCREMENTDTFROMINITIALDT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tspseudoincrementdtfrominitialdt_ tspseudoincrementdtfrominitialdt
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tspseudoverifytimestep_(TS ts,Vec update,PetscReal *dt,PetscBool *flag, int *__ierr)
{
*__ierr = TSPseudoVerifyTimeStep(
	(TS)PetscToPointer((ts) ),
	(Vec)PetscToPointer((update) ),dt,flag);
}
PETSC_EXTERN void  tspseudosettimestepincrement_(TS ts,PetscReal *inc, int *__ierr)
{
*__ierr = TSPseudoSetTimeStepIncrement(
	(TS)PetscToPointer((ts) ),*inc);
}
PETSC_EXTERN void  tspseudosetmaxtimestep_(TS ts,PetscReal *maxdt, int *__ierr)
{
*__ierr = TSPseudoSetMaxTimeStep(
	(TS)PetscToPointer((ts) ),*maxdt);
}
PETSC_EXTERN void  tspseudoincrementdtfrominitialdt_(TS ts, int *__ierr)
{
*__ierr = TSPseudoIncrementDtFromInitialDt(
	(TS)PetscToPointer((ts) ));
}
#if defined(__cplusplus)
}
#endif

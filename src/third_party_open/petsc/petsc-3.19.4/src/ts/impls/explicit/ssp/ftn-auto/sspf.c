#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* ssp.c */
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
#define tssspsetnumstages_ TSSSPSETNUMSTAGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssspsetnumstages_ tssspsetnumstages
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tssspgetnumstages_ TSSSPGETNUMSTAGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tssspgetnumstages_ tssspgetnumstages
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tssspsetnumstages_(TS ts,PetscInt *nstages, int *__ierr)
{
*__ierr = TSSSPSetNumStages(
	(TS)PetscToPointer((ts) ),*nstages);
}
PETSC_EXTERN void  tssspgetnumstages_(TS ts,PetscInt *nstages, int *__ierr)
{
*__ierr = TSSSPGetNumStages(
	(TS)PetscToPointer((ts) ),nstages);
}
#if defined(__cplusplus)
}
#endif

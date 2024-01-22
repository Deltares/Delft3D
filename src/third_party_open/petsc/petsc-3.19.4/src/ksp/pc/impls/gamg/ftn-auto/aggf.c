#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* agg.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetnsmooths_ PCGAMGSETNSMOOTHS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetnsmooths_ pcgamgsetnsmooths
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetaggressivelevels_ PCGAMGSETAGGRESSIVELEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetaggressivelevels_ pcgamgsetaggressivelevels
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcgamgsetnsmooths_(PC pc,PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetNSmooths(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgsetaggressivelevels_(PC pc,PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetAggressiveLevels(
	(PC)PetscToPointer((pc) ),*n);
}
#if defined(__cplusplus)
}
#endif

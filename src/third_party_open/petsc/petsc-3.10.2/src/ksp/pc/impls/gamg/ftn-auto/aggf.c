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
#define pcgamgsetsymgraph_ PCGAMGSETSYMGRAPH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetsymgraph_ pcgamgsetsymgraph
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetsquaregraph_ PCGAMGSETSQUAREGRAPH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetsquaregraph_ pcgamgsetsquaregraph
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetnsmooths_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetNSmooths(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetsymgraph_(PC pc,PetscBool *n, int *__ierr){
*__ierr = PCGAMGSetSymGraph(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetsquaregraph_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetSquareGraph(
	(PC)PetscToPointer((pc) ),*n);
}
#if defined(__cplusplus)
}
#endif

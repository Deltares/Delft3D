#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* hmg.c */
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
#define pchmgsetreuseinterpolation_ PCHMGSETREUSEINTERPOLATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchmgsetreuseinterpolation_ pchmgsetreuseinterpolation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchmgsetusesubspacecoarsening_ PCHMGSETUSESUBSPACECOARSENING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchmgsetusesubspacecoarsening_ pchmgsetusesubspacecoarsening
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchmgsetcoarseningcomponent_ PCHMGSETCOARSENINGCOMPONENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchmgsetcoarseningcomponent_ pchmgsetcoarseningcomponent
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchmgusematmaij_ PCHMGUSEMATMAIJ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchmgusematmaij_ pchmgusematmaij
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pchmgsetreuseinterpolation_(PC pc,PetscBool *reuse, int *__ierr)
{
*__ierr = PCHMGSetReuseInterpolation(
	(PC)PetscToPointer((pc) ),*reuse);
}
PETSC_EXTERN void  pchmgsetusesubspacecoarsening_(PC pc,PetscBool *subspace, int *__ierr)
{
*__ierr = PCHMGSetUseSubspaceCoarsening(
	(PC)PetscToPointer((pc) ),*subspace);
}
PETSC_EXTERN void  pchmgsetcoarseningcomponent_(PC pc,PetscInt *component, int *__ierr)
{
*__ierr = PCHMGSetCoarseningComponent(
	(PC)PetscToPointer((pc) ),*component);
}
PETSC_EXTERN void  pchmgusematmaij_(PC pc,PetscBool *usematmaij, int *__ierr)
{
*__ierr = PCHMGUseMatMAIJ(
	(PC)PetscToPointer((pc) ),*usematmaij);
}
#if defined(__cplusplus)
}
#endif

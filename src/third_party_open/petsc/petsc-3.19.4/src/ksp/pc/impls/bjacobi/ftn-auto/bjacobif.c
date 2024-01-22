#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bjacobi.c */
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
#define pcbjacobisettotalblocks_ PCBJACOBISETTOTALBLOCKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcbjacobisettotalblocks_ pcbjacobisettotalblocks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcbjacobisetlocalblocks_ PCBJACOBISETLOCALBLOCKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcbjacobisetlocalblocks_ pcbjacobisetlocalblocks
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcbjacobisettotalblocks_(PC pc,PetscInt *blocks, PetscInt lens[], int *__ierr)
{
*__ierr = PCBJacobiSetTotalBlocks(
	(PC)PetscToPointer((pc) ),*blocks,lens);
}
PETSC_EXTERN void  pcbjacobisetlocalblocks_(PC pc,PetscInt *blocks, PetscInt lens[], int *__ierr)
{
*__ierr = PCBJacobiSetLocalBlocks(
	(PC)PetscToPointer((pc) ),*blocks,lens);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dalocal.c */
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

#include "petscdmda.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetnumcells_ DMDAGETNUMCELLS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetnumcells_ dmdagetnumcells
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetcellpoint_ DMDAGETCELLPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetcellpoint_ dmdagetcellpoint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdagetnumcells_(DM dm,PetscInt *numCellsX,PetscInt *numCellsY,PetscInt *numCellsZ,PetscInt *numCells, int *__ierr)
{
*__ierr = DMDAGetNumCells(
	(DM)PetscToPointer((dm) ),numCellsX,numCellsY,numCellsZ,numCells);
}
PETSC_EXTERN void  dmdagetcellpoint_(DM dm,PetscInt *i,PetscInt *j,PetscInt *k,PetscInt *point, int *__ierr)
{
*__ierr = DMDAGetCellPoint(
	(DM)PetscToPointer((dm) ),*i,*j,*k,point);
}
#if defined(__cplusplus)
}
#endif

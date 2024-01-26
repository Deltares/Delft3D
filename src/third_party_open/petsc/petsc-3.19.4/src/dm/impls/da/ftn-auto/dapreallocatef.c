#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dapreallocate.c */
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
#define dmdasetpreallocationcenterdimension_ DMDASETPREALLOCATIONCENTERDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetpreallocationcenterdimension_ dmdasetpreallocationcenterdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetpreallocationcenterdimension_ DMDAGETPREALLOCATIONCENTERDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetpreallocationcenterdimension_ dmdagetpreallocationcenterdimension
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdasetpreallocationcenterdimension_(DM dm,PetscInt *preallocCenterDim, int *__ierr)
{
*__ierr = DMDASetPreallocationCenterDimension(
	(DM)PetscToPointer((dm) ),*preallocCenterDim);
}
PETSC_EXTERN void  dmdagetpreallocationcenterdimension_(DM dm,PetscInt *preallocCenterDim, int *__ierr)
{
*__ierr = DMDAGetPreallocationCenterDimension(
	(DM)PetscToPointer((dm) ),preallocCenterDim);
}
#if defined(__cplusplus)
}
#endif

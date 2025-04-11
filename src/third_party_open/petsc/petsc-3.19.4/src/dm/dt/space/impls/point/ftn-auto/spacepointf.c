#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* spacepoint.c */
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

#include "petscfe.h"
#include "petscdt.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacepointsetpoints_ PETSCSPACEPOINTSETPOINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacepointsetpoints_ petscspacepointsetpoints
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacepointgetpoints_ PETSCSPACEPOINTGETPOINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacepointgetpoints_ petscspacepointgetpoints
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspacepointsetpoints_(PetscSpace sp,PetscQuadrature q, int *__ierr)
{
*__ierr = PetscSpacePointSetPoints(
	(PetscSpace)PetscToPointer((sp) ),
	(PetscQuadrature)PetscToPointer((q) ));
}
PETSC_EXTERN void  petscspacepointgetpoints_(PetscSpace sp,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscSpacePointGetPoints(
	(PetscSpace)PetscToPointer((sp) ),q);
}
#if defined(__cplusplus)
}
#endif

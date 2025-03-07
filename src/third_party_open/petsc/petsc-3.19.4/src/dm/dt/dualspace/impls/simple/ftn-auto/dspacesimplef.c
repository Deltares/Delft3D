#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dspacesimple.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesimplesetdimension_ PETSCDUALSPACESIMPLESETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesimplesetdimension_ petscdualspacesimplesetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacesimplesetfunctional_ PETSCDUALSPACESIMPLESETFUNCTIONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacesimplesetfunctional_ petscdualspacesimplesetfunctional
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdualspacesimplesetdimension_(PetscDualSpace sp,PetscInt *dim, int *__ierr)
{
*__ierr = PetscDualSpaceSimpleSetDimension(
	(PetscDualSpace)PetscToPointer((sp) ),*dim);
}
PETSC_EXTERN void  petscdualspacesimplesetfunctional_(PetscDualSpace sp,PetscInt *func,PetscQuadrature q, int *__ierr)
{
*__ierr = PetscDualSpaceSimpleSetFunctional(
	(PetscDualSpace)PetscToPointer((sp) ),*func,
	(PetscQuadrature)PetscToPointer((q) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sortip.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwithpermutation_ PETSCSORTINTWITHPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwithpermutation_ petscsortintwithpermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortrealwithpermutation_ PETSCSORTREALWITHPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortrealwithpermutation_ petscsortrealwithpermutation
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsortintwithpermutation_(PetscInt *n, PetscInt i[],PetscInt idx[], int *__ierr)
{
*__ierr = PetscSortIntWithPermutation(*n,i,idx);
}
PETSC_EXTERN void  petscsortrealwithpermutation_(PetscInt *n, PetscReal i[],PetscInt idx[], int *__ierr)
{
*__ierr = PetscSortRealWithPermutation(*n,i,idx);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sortd.c */
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
#define petscsortedreal_ PETSCSORTEDREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedreal_ petscsortedreal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortreal_ PETSCSORTREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortreal_ petscsortreal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortrealwitharrayint_ PETSCSORTREALWITHARRAYINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortrealwitharrayint_ petscsortrealwitharrayint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfindreal_ PETSCFINDREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfindreal_ petscfindreal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortremovedupsreal_ PETSCSORTREMOVEDUPSREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortremovedupsreal_ petscsortremovedupsreal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortsplit_ PETSCSORTSPLIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortsplit_ petscsortsplit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortsplitreal_ PETSCSORTSPLITREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortsplitreal_ petscsortsplitreal
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsortedreal_(PetscInt *n, PetscReal X[],PetscBool *sorted, int *__ierr)
{
*__ierr = PetscSortedReal(*n,X,sorted);
}
PETSC_EXTERN void  petscsortreal_(PetscInt *n,PetscReal v[], int *__ierr)
{
*__ierr = PetscSortReal(*n,v);
}
PETSC_EXTERN void  petscsortrealwitharrayint_(PetscInt *n,PetscReal r[],PetscInt Ii[], int *__ierr)
{
*__ierr = PetscSortRealWithArrayInt(*n,r,Ii);
}
PETSC_EXTERN void  petscfindreal_(PetscReal *key,PetscInt *n, PetscReal t[],PetscReal *eps,PetscInt *loc, int *__ierr)
{
*__ierr = PetscFindReal(*key,*n,t,*eps,loc);
}
PETSC_EXTERN void  petscsortremovedupsreal_(PetscInt *n,PetscReal v[], int *__ierr)
{
*__ierr = PetscSortRemoveDupsReal(n,v);
}
PETSC_EXTERN void  petscsortsplit_(PetscInt *ncut,PetscInt *n,PetscScalar a[],PetscInt idx[], int *__ierr)
{
*__ierr = PetscSortSplit(*ncut,*n,a,idx);
}
PETSC_EXTERN void  petscsortsplitreal_(PetscInt *ncut,PetscInt *n,PetscReal a[],PetscInt idx[], int *__ierr)
{
*__ierr = PetscSortSplitReal(*ncut,*n,a,idx);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sortso.c */
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
#define petscintsortsemiordered_ PETSCINTSORTSEMIORDERED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscintsortsemiordered_ petscintsortsemiordered
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscintsortsemiorderedwitharray_ PETSCINTSORTSEMIORDEREDWITHARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscintsortsemiorderedwitharray_ petscintsortsemiorderedwitharray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmpiintsortsemiordered_ PETSCMPIINTSORTSEMIORDERED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmpiintsortsemiordered_ petscmpiintsortsemiordered
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmpiintsortsemiorderedwitharray_ PETSCMPIINTSORTSEMIORDEREDWITHARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmpiintsortsemiorderedwitharray_ petscmpiintsortsemiorderedwitharray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrealsortsemiordered_ PETSCREALSORTSEMIORDERED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrealsortsemiordered_ petscrealsortsemiordered
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrealsortsemiorderedwitharrayint_ PETSCREALSORTSEMIORDEREDWITHARRAYINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrealsortsemiorderedwitharrayint_ petscrealsortsemiorderedwitharrayint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscintsortsemiordered_(PetscInt *n,PetscInt arr[], int *__ierr)
{
*__ierr = PetscIntSortSemiOrdered(*n,arr);
}
PETSC_EXTERN void  petscintsortsemiorderedwitharray_(PetscInt *n,PetscInt arr1[],PetscInt arr2[], int *__ierr)
{
*__ierr = PetscIntSortSemiOrderedWithArray(*n,arr1,arr2);
}
PETSC_EXTERN void  petscmpiintsortsemiordered_(PetscInt *n,PetscMPIInt arr[], int *__ierr)
{
*__ierr = PetscMPIIntSortSemiOrdered(*n,arr);
}
PETSC_EXTERN void  petscmpiintsortsemiorderedwitharray_(PetscInt *n,PetscMPIInt arr1[],PetscMPIInt arr2[], int *__ierr)
{
*__ierr = PetscMPIIntSortSemiOrderedWithArray(*n,arr1,arr2);
}
PETSC_EXTERN void  petscrealsortsemiordered_(PetscInt *n,PetscReal arr[], int *__ierr)
{
*__ierr = PetscRealSortSemiOrdered(*n,arr);
}
PETSC_EXTERN void  petscrealsortsemiorderedwitharrayint_(PetscInt *n,PetscReal arr1[],PetscInt arr2[], int *__ierr)
{
*__ierr = PetscRealSortSemiOrderedWithArrayInt(*n,arr1,arr2);
}
#if defined(__cplusplus)
}
#endif

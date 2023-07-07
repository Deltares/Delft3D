#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mpidense.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensegetlocalmatrix_ MATDENSEGETLOCALMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetlocalmatrix_ matdensegetlocalmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenseplacearray_ MATDENSEPLACEARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenseplacearray_ matdenseplacearray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenseresetarray_ MATDENSERESETARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenseresetarray_ matdenseresetarray
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matdensegetlocalmatrix_(Mat A,Mat *B, int *__ierr){
*__ierr = MatDenseGetLocalMatrix(
	(Mat)PetscToPointer((A) ),B);
}
PETSC_EXTERN void PETSC_STDCALL  matdenseplacearray_(Mat mat, PetscScalar array[], int *__ierr){
*__ierr = MatDensePlaceArray(
	(Mat)PetscToPointer((mat) ),array);
}
PETSC_EXTERN void PETSC_STDCALL  matdenseresetarray_(Mat mat, int *__ierr){
*__ierr = MatDenseResetArray(
	(Mat)PetscToPointer((mat) ));
}
#if defined(__cplusplus)
}
#endif

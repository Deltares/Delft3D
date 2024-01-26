#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* bqnk.c */
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

#include "petsctao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taogetlmvmmatrix_ TAOGETLMVMMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taogetlmvmmatrix_ taogetlmvmmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taosetlmvmmatrix_ TAOSETLMVMMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosetlmvmmatrix_ taosetlmvmmatrix
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taogetlmvmmatrix_(Tao tao,Mat *B, int *__ierr)
{
*__ierr = TaoGetLMVMMatrix(
	(Tao)PetscToPointer((tao) ),B);
}
PETSC_EXTERN void  taosetlmvmmatrix_(Tao tao,Mat B, int *__ierr)
{
*__ierr = TaoSetLMVMMatrix(
	(Tao)PetscToPointer((tao) ),
	(Mat)PetscToPointer((B) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* normmh.c */
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
#define matnormalhermitiangetmat_ MATNORMALHERMITIANGETMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnormalhermitiangetmat_ matnormalhermitiangetmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatenormalhermitian_ MATCREATENORMALHERMITIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatenormalhermitian_ matcreatenormalhermitian
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matnormalhermitiangetmat_(Mat A,Mat *M, int *__ierr)
{
*__ierr = MatNormalHermitianGetMat(
	(Mat)PetscToPointer((A) ),M);
}
PETSC_EXTERN void  matcreatenormalhermitian_(Mat A,Mat *N, int *__ierr)
{
*__ierr = MatCreateNormalHermitian(
	(Mat)PetscToPointer((A) ),N);
}
#if defined(__cplusplus)
}
#endif

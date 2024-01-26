#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* htransm.c */
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
#define mathermitiantransposegetmat_ MATHERMITIANTRANSPOSEGETMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mathermitiantransposegetmat_ mathermitiantransposegetmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatehermitiantranspose_ MATCREATEHERMITIANTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatehermitiantranspose_ matcreatehermitiantranspose
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  mathermitiantransposegetmat_(Mat A,Mat *M, int *__ierr)
{
*__ierr = MatHermitianTransposeGetMat(
	(Mat)PetscToPointer((A) ),M);
}
PETSC_EXTERN void  matcreatehermitiantranspose_(Mat A,Mat *N, int *__ierr)
{
*__ierr = MatCreateHermitianTranspose(
	(Mat)PetscToPointer((A) ),N);
}
#if defined(__cplusplus)
}
#endif

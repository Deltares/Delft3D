#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* normm.c */
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
#define matnormalgetmat_ MATNORMALGETMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnormalgetmat_ matnormalgetmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatenormal_ MATCREATENORMAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatenormal_ matcreatenormal
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matnormalgetmat_(Mat A,Mat *M, int *__ierr)
{
*__ierr = MatNormalGetMat(
	(Mat)PetscToPointer((A) ),M);
}
PETSC_EXTERN void  matcreatenormal_(Mat A,Mat *N, int *__ierr)
{
*__ierr = MatCreateNormal(
	(Mat)PetscToPointer((A) ),N);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* multequal.c */
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
#define matmultequal_ MATMULTEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmultequal_ matmultequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmultaddequal_ MATMULTADDEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmultaddequal_ matmultaddequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulttransposeequal_ MATMULTTRANSPOSEEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulttransposeequal_ matmulttransposeequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulttransposeaddequal_ MATMULTTRANSPOSEADDEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulttransposeaddequal_ matmulttransposeaddequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatmultequal_ MATMATMULTEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatmultequal_ matmatmultequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mattransposematmultequal_ MATTRANSPOSEMATMULTEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mattransposematmultequal_ mattransposematmultequal
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matmultequal_(Mat A,Mat B,PetscInt *n,PetscBool  *flg, int *__ierr){
*__ierr = MatMultEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*n,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matmultaddequal_(Mat A,Mat B,PetscInt *n,PetscBool  *flg, int *__ierr){
*__ierr = MatMultAddEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*n,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matmulttransposeequal_(Mat A,Mat B,PetscInt *n,PetscBool  *flg, int *__ierr){
*__ierr = MatMultTransposeEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*n,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matmulttransposeaddequal_(Mat A,Mat B,PetscInt *n,PetscBool  *flg, int *__ierr){
*__ierr = MatMultTransposeAddEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*n,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matmatmultequal_(Mat A,Mat B,Mat C,PetscInt *n,PetscBool *flg, int *__ierr){
*__ierr = MatMatMultEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ),*n,flg);
}
PETSC_EXTERN void PETSC_STDCALL  mattransposematmultequal_(Mat A,Mat B,Mat C,PetscInt *n,PetscBool *flg, int *__ierr){
*__ierr = MatTransposeMatMultEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ),*n,flg);
}
#if defined(__cplusplus)
}
#endif

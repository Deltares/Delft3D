#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* axpy.c */
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
#define mataxpy_ MATAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mataxpy_ mataxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshift_ MATSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshift_ matshift
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalset_ MATDIAGONALSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalset_ matdiagonalset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mataypx_ MATAYPX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mataypx_ mataypx
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcomputeexplicitoperator_ MATCOMPUTEEXPLICITOPERATOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcomputeexplicitoperator_ matcomputeexplicitoperator
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcomputeexplicitoperatortranspose_ MATCOMPUTEEXPLICITOPERATORTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcomputeexplicitoperatortranspose_ matcomputeexplicitoperatortranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matchop_ MATCHOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matchop_ matchop
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  mataxpy_(Mat Y,PetscScalar *a,Mat X,MatStructure *str, int *__ierr){
*__ierr = MatAXPY(
	(Mat)PetscToPointer((Y) ),*a,
	(Mat)PetscToPointer((X) ),*str);
}
PETSC_EXTERN void PETSC_STDCALL  matshift_(Mat Y,PetscScalar *a, int *__ierr){
*__ierr = MatShift(
	(Mat)PetscToPointer((Y) ),*a);
}
PETSC_EXTERN void PETSC_STDCALL  matdiagonalset_(Mat Y,Vec D,InsertMode *is, int *__ierr){
*__ierr = MatDiagonalSet(
	(Mat)PetscToPointer((Y) ),
	(Vec)PetscToPointer((D) ),*is);
}
PETSC_EXTERN void PETSC_STDCALL  mataypx_(Mat Y,PetscScalar *a,Mat X,MatStructure *str, int *__ierr){
*__ierr = MatAYPX(
	(Mat)PetscToPointer((Y) ),*a,
	(Mat)PetscToPointer((X) ),*str);
}
PETSC_EXTERN void PETSC_STDCALL  matcomputeexplicitoperator_(Mat inmat,Mat *mat, int *__ierr){
*__ierr = MatComputeExplicitOperator(
	(Mat)PetscToPointer((inmat) ),mat);
}
PETSC_EXTERN void PETSC_STDCALL  matcomputeexplicitoperatortranspose_(Mat inmat,Mat *mat, int *__ierr){
*__ierr = MatComputeExplicitOperatorTranspose(
	(Mat)PetscToPointer((inmat) ),mat);
}
PETSC_EXTERN void PETSC_STDCALL  matchop_(Mat A,PetscReal *tol, int *__ierr){
*__ierr = MatChop(
	(Mat)PetscToPointer((A) ),*tol);
}
#if defined(__cplusplus)
}
#endif

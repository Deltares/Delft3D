#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mpiaij.c */
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
#define matmpiaijsetusescalableincreaseoverlap_ MATMPIAIJSETUSESCALABLEINCREASEOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijsetusescalableincreaseoverlap_ matmpiaijsetusescalableincreaseoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmpiaijsetpreallocationcsr_ MATMPIAIJSETPREALLOCATIONCSR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijsetpreallocationcsr_ matmpiaijsetpreallocationcsr
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatempiaijwitharrays_ MATCREATEMPIAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatempiaijwitharrays_ matcreatempiaijwitharrays
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmpiaijgetlocalmat_ MATMPIAIJGETLOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijgetlocalmat_ matmpiaijgetlocalmat
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matmpiaijsetusescalableincreaseoverlap_(Mat A,PetscBool *sc, int *__ierr){
*__ierr = MatMPIAIJSetUseScalableIncreaseOverlap(
	(Mat)PetscToPointer((A) ),*sc);
}
PETSC_EXTERN void PETSC_STDCALL  matmpiaijsetpreallocationcsr_(Mat B, PetscInt i[], PetscInt j[], PetscScalar v[], int *__ierr){
*__ierr = MatMPIAIJSetPreallocationCSR(
	(Mat)PetscToPointer((B) ),i,j,v);
}
PETSC_EXTERN void PETSC_STDCALL  matcreatempiaijwitharrays_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N, PetscInt i[], PetscInt j[], PetscScalar a[],Mat *mat, int *__ierr){
*__ierr = MatCreateMPIAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*m,*n,*M,*N,i,j,a,mat);
}
PETSC_EXTERN void PETSC_STDCALL  matmpiaijgetlocalmat_(Mat A,MatReuse *scall,Mat *A_loc, int *__ierr){
*__ierr = MatMPIAIJGetLocalMat(
	(Mat)PetscToPointer((A) ),*scall,A_loc);
}
#if defined(__cplusplus)
}
#endif

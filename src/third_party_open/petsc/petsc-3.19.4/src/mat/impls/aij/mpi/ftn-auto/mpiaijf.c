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
#define matmpiaijgetnumbernonzeros_ MATMPIAIJGETNUMBERNONZEROS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijgetnumbernonzeros_ matmpiaijgetnumbernonzeros
#endif
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
#define matupdatempiaijwitharrays_ MATUPDATEMPIAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matupdatempiaijwitharrays_ matupdatempiaijwitharrays
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matupdatempiaijwitharray_ MATUPDATEMPIAIJWITHARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matupdatempiaijwitharray_ matupdatempiaijwitharray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mataijgetlocalmat_ MATAIJGETLOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mataijgetlocalmat_ mataijgetlocalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmpiaijgetlocalmat_ MATMPIAIJGETLOCALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijgetlocalmat_ matmpiaijgetlocalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmpiaijgetlocalmatmerge_ MATMPIAIJGETLOCALMATMERGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpiaijgetlocalmatmerge_ matmpiaijgetlocalmatmerge
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matmpiaijgetnumbernonzeros_(Mat A,PetscCount *nz, int *__ierr)
{
*__ierr = MatMPIAIJGetNumberNonzeros(
	(Mat)PetscToPointer((A) ),nz);
}
PETSC_EXTERN void  matmpiaijsetusescalableincreaseoverlap_(Mat A,PetscBool *sc, int *__ierr)
{
*__ierr = MatMPIAIJSetUseScalableIncreaseOverlap(
	(Mat)PetscToPointer((A) ),*sc);
}
PETSC_EXTERN void  matmpiaijsetpreallocationcsr_(Mat B, PetscInt i[], PetscInt j[], PetscScalar v[], int *__ierr)
{
*__ierr = MatMPIAIJSetPreallocationCSR(
	(Mat)PetscToPointer((B) ),i,j,v);
}
PETSC_EXTERN void  matcreatempiaijwitharrays_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N, PetscInt i[], PetscInt j[], PetscScalar a[],Mat *mat, int *__ierr)
{
*__ierr = MatCreateMPIAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*m,*n,*M,*N,i,j,a,mat);
}
PETSC_EXTERN void  matupdatempiaijwitharrays_(Mat mat,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N, PetscInt Ii[], PetscInt J[], PetscScalar v[], int *__ierr)
{
*__ierr = MatUpdateMPIAIJWithArrays(
	(Mat)PetscToPointer((mat) ),*m,*n,*M,*N,Ii,J,v);
}
PETSC_EXTERN void  matupdatempiaijwitharray_(Mat mat, PetscScalar v[], int *__ierr)
{
*__ierr = MatUpdateMPIAIJWithArray(
	(Mat)PetscToPointer((mat) ),v);
}
PETSC_EXTERN void  mataijgetlocalmat_(Mat A,Mat *A_loc, int *__ierr)
{
*__ierr = MatAIJGetLocalMat(
	(Mat)PetscToPointer((A) ),A_loc);
}
PETSC_EXTERN void  matmpiaijgetlocalmat_(Mat A,MatReuse *scall,Mat *A_loc, int *__ierr)
{
*__ierr = MatMPIAIJGetLocalMat(
	(Mat)PetscToPointer((A) ),*scall,A_loc);
}
PETSC_EXTERN void  matmpiaijgetlocalmatmerge_(Mat A,MatReuse *scall,IS *glob,Mat *A_loc, int *__ierr)
{
*__ierr = MatMPIAIJGetLocalMatMerge(
	(Mat)PetscToPointer((A) ),*scall,glob,A_loc);
}
#if defined(__cplusplus)
}
#endif

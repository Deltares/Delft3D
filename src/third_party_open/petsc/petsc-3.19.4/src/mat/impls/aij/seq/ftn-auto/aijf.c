#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* aij.c */
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
#define matseqaijsettotalpreallocation_ MATSEQAIJSETTOTALPREALLOCATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqaijsettotalpreallocation_ matseqaijsettotalpreallocation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matseqaijsetcolumnindices_ MATSEQAIJSETCOLUMNINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqaijsetcolumnindices_ matseqaijsetcolumnindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstorevalues_ MATSTOREVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstorevalues_ matstorevalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matretrievevalues_ MATRETRIEVEVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matretrievevalues_ matretrievevalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matseqaijsetpreallocationcsr_ MATSEQAIJSETPREALLOCATIONCSR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqaijsetpreallocationcsr_ matseqaijsetpreallocationcsr
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matseqaijkron_ MATSEQAIJKRON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqaijkron_ matseqaijkron
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateseqaijwitharrays_ MATCREATESEQAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqaijwitharrays_ matcreateseqaijwitharrays
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateseqaijfromtriple_ MATCREATESEQAIJFROMTRIPLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqaijfromtriple_ matcreateseqaijfromtriple
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

PETSC_EXTERN void  matseqaijsettotalpreallocation_(Mat A,PetscInt *nztotal, int *__ierr)
{
*__ierr = MatSeqAIJSetTotalPreallocation(
	(Mat)PetscToPointer((A) ),*nztotal);
}
PETSC_EXTERN void  matseqaijsetcolumnindices_(Mat mat,PetscInt *indices, int *__ierr)
{
*__ierr = MatSeqAIJSetColumnIndices(
	(Mat)PetscToPointer((mat) ),indices);
}
PETSC_EXTERN void  matstorevalues_(Mat mat, int *__ierr)
{
*__ierr = MatStoreValues(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matretrievevalues_(Mat mat, int *__ierr)
{
*__ierr = MatRetrieveValues(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  matseqaijsetpreallocationcsr_(Mat B, PetscInt i[], PetscInt j[], PetscScalar v[], int *__ierr)
{
*__ierr = MatSeqAIJSetPreallocationCSR(
	(Mat)PetscToPointer((B) ),i,j,v);
}
PETSC_EXTERN void  matseqaijkron_(Mat A,Mat B,MatReuse *reuse,Mat *C, int *__ierr)
{
*__ierr = MatSeqAIJKron(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*reuse,C);
}
PETSC_EXTERN void  matcreateseqaijwitharrays_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt i[],PetscInt j[],PetscScalar a[],Mat *mat, int *__ierr)
{
*__ierr = MatCreateSeqAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*m,*n,i,j,a,mat);
}
PETSC_EXTERN void  matcreateseqaijfromtriple_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt i[],PetscInt j[],PetscScalar a[],Mat *mat,PetscInt *nz,PetscBool *idx, int *__ierr)
{
*__ierr = MatCreateSeqAIJFromTriple(
	MPI_Comm_f2c(*(comm)),*m,*n,i,j,a,mat,*nz,*idx);
}
#if defined(__cplusplus)
}
#endif

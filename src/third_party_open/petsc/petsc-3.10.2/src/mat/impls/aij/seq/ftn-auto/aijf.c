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
#define matcreateseqaijwitharrays_ MATCREATESEQAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqaijwitharrays_ matcreateseqaijwitharrays
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matseqaijsetcolumnindices_(Mat mat,PetscInt *indices, int *__ierr){
*__ierr = MatSeqAIJSetColumnIndices(
	(Mat)PetscToPointer((mat) ),indices);
}
PETSC_EXTERN void PETSC_STDCALL  matstorevalues_(Mat mat, int *__ierr){
*__ierr = MatStoreValues(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matretrievevalues_(Mat mat, int *__ierr){
*__ierr = MatRetrieveValues(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matseqaijsetpreallocationcsr_(Mat B, PetscInt i[], PetscInt j[], PetscScalar v[], int *__ierr){
*__ierr = MatSeqAIJSetPreallocationCSR(
	(Mat)PetscToPointer((B) ),i,j,v);
}
PETSC_EXTERN void PETSC_STDCALL  matcreateseqaijwitharrays_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt i[],PetscInt j[],PetscScalar a[],Mat *mat, int *__ierr){
*__ierr = MatCreateSeqAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*m,*n,i,j,a,mat);
}
#if defined(__cplusplus)
}
#endif

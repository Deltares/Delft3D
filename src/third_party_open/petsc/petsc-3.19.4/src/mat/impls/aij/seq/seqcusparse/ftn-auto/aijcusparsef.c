#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* aijcusparse.cu */
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
#define matcusparsesetformat_ MATCUSPARSESETFORMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcusparsesetformat_ matcusparsesetformat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcusparsesetusecpusolve_ MATCUSPARSESETUSECPUSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcusparsesetusecpusolve_ matcusparsesetusecpusolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateseqaijcusparse_ MATCREATESEQAIJCUSPARSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqaijcusparse_ matcreateseqaijcusparse
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcusparsesetformat_(Mat A,MatCUSPARSEFormatOperation *op,MatCUSPARSEStorageFormat *format, int *__ierr)
{
*__ierr = MatCUSPARSESetFormat(
	(Mat)PetscToPointer((A) ),*op,*format);
}
PETSC_EXTERN void  matcusparsesetusecpusolve_(Mat A,PetscBool *use_cpu, int *__ierr)
{
*__ierr = MatCUSPARSESetUseCPUSolve(
	(Mat)PetscToPointer((A) ),*use_cpu);
}
PETSC_EXTERN void  matcreateseqaijcusparse_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt *nz, PetscInt nnz[],Mat *A, int *__ierr)
{
*__ierr = MatCreateSeqAIJCUSPARSE(
	MPI_Comm_f2c(*(comm)),*m,*n,*nz,nnz,A);
}
#if defined(__cplusplus)
}
#endif

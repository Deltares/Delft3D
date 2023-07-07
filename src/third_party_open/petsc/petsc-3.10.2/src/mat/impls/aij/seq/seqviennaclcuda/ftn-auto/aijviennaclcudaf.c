#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* aijviennaclcuda.cu */
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
#define matcreateseqaijviennacl_ MATCREATESEQAIJVIENNACL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqaijviennacl_ matcreateseqaijviennacl
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matcreateseqaijviennacl_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt *nz, PetscInt nnz[],Mat *A, int *__ierr){
*__ierr = MatCreateSeqAIJViennaCL(
	MPI_Comm_f2c(*(comm)),*m,*n,*nz,nnz,A);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mpibaij.c */
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
#define matmpibaijsethashtablefactor_ MATMPIBAIJSETHASHTABLEFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmpibaijsethashtablefactor_ matmpibaijsethashtablefactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatempibaijwitharrays_ MATCREATEMPIBAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatempibaijwitharrays_ matcreatempibaijwitharrays
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matmpibaijsethashtablefactor_(Mat mat,PetscReal *fact, int *__ierr)
{
*__ierr = MatMPIBAIJSetHashTableFactor(
	(Mat)PetscToPointer((mat) ),*fact);
}
PETSC_EXTERN void  matcreatempibaijwitharrays_(MPI_Fint * comm,PetscInt *bs,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N, PetscInt i[], PetscInt j[], PetscScalar a[],Mat *mat, int *__ierr)
{
*__ierr = MatCreateMPIBAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*bs,*m,*n,*M,*N,i,j,a,mat);
}
#if defined(__cplusplus)
}
#endif

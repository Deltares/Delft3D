#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sbaij.c */
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
#define matseqsbaijsetcolumnindices_ MATSEQSBAIJSETCOLUMNINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqsbaijsetcolumnindices_ matseqsbaijsetcolumnindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateseqsbaijwitharrays_ MATCREATESEQSBAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqsbaijwitharrays_ matcreateseqsbaijwitharrays
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matseqsbaijsetcolumnindices_(Mat mat,PetscInt *indices, int *__ierr)
{
*__ierr = MatSeqSBAIJSetColumnIndices(
	(Mat)PetscToPointer((mat) ),indices);
}
PETSC_EXTERN void  matcreateseqsbaijwitharrays_(MPI_Fint * comm,PetscInt *bs,PetscInt *m,PetscInt *n,PetscInt i[],PetscInt j[],PetscScalar a[],Mat *mat, int *__ierr)
{
*__ierr = MatCreateSeqSBAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*bs,*m,*n,i,j,a,mat);
}
#if defined(__cplusplus)
}
#endif

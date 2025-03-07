#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* baij.c */
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
#define matseqbaijsetcolumnindices_ MATSEQBAIJSETCOLUMNINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matseqbaijsetcolumnindices_ matseqbaijsetcolumnindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateseqbaijwitharrays_ MATCREATESEQBAIJWITHARRAYS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateseqbaijwitharrays_ matcreateseqbaijwitharrays
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matseqbaijsetcolumnindices_(Mat mat,PetscInt *indices, int *__ierr)
{
*__ierr = MatSeqBAIJSetColumnIndices(
	(Mat)PetscToPointer((mat) ),indices);
}
PETSC_EXTERN void  matcreateseqbaijwitharrays_(MPI_Fint * comm,PetscInt *bs,PetscInt *m,PetscInt *n,PetscInt i[],PetscInt j[],PetscScalar a[],Mat *mat, int *__ierr)
{
*__ierr = MatCreateSeqBAIJWithArrays(
	MPI_Comm_f2c(*(comm)),*bs,*m,*n,i,j,a,mat);
}
#if defined(__cplusplus)
}
#endif

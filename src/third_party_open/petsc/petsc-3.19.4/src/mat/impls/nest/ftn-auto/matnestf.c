#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matnest.c */
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
#define matnestgetsubmat_ MATNESTGETSUBMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnestgetsubmat_ matnestgetsubmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnestsetsubmat_ MATNESTSETSUBMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnestsetsubmat_ matnestsetsubmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnestgetsize_ MATNESTGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnestgetsize_ matnestgetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnestsetsubmats_ MATNESTSETSUBMATS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnestsetsubmats_ matnestsetsubmats
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matnestgetsubmat_(Mat A,PetscInt *idxm,PetscInt *jdxm,Mat *sub, int *__ierr)
{
*__ierr = MatNestGetSubMat(
	(Mat)PetscToPointer((A) ),*idxm,*jdxm,sub);
}
PETSC_EXTERN void  matnestsetsubmat_(Mat A,PetscInt *idxm,PetscInt *jdxm,Mat sub, int *__ierr)
{
*__ierr = MatNestSetSubMat(
	(Mat)PetscToPointer((A) ),*idxm,*jdxm,
	(Mat)PetscToPointer((sub) ));
}
PETSC_EXTERN void  matnestgetsize_(Mat A,PetscInt *M,PetscInt *N, int *__ierr)
{
*__ierr = MatNestGetSize(
	(Mat)PetscToPointer((A) ),M,N);
}
PETSC_EXTERN void  matnestsetsubmats_(Mat A,PetscInt *nr, IS is_row[],PetscInt *nc, IS is_col[], Mat a[], int *__ierr)
{
*__ierr = MatNestSetSubMats(
	(Mat)PetscToPointer((A) ),*nr,is_row,*nc,is_col,a);
}
#if defined(__cplusplus)
}
#endif

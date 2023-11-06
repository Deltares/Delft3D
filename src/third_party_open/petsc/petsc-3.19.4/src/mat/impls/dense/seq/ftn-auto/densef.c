#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dense.c */
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
#define matdensegetlda_ MATDENSEGETLDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetlda_ matdensegetlda
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensesetlda_ MATDENSESETLDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensesetlda_ matdensesetlda
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensegetcolumnvec_ MATDENSEGETCOLUMNVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetcolumnvec_ matdensegetcolumnvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenserestorecolumnvec_ MATDENSERESTORECOLUMNVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenserestorecolumnvec_ matdenserestorecolumnvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensegetcolumnvecread_ MATDENSEGETCOLUMNVECREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetcolumnvecread_ matdensegetcolumnvecread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenserestorecolumnvecread_ MATDENSERESTORECOLUMNVECREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenserestorecolumnvecread_ matdenserestorecolumnvecread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensegetcolumnvecwrite_ MATDENSEGETCOLUMNVECWRITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetcolumnvecwrite_ matdensegetcolumnvecwrite
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenserestorecolumnvecwrite_ MATDENSERESTORECOLUMNVECWRITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenserestorecolumnvecwrite_ matdenserestorecolumnvecwrite
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdensegetsubmatrix_ MATDENSEGETSUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdensegetsubmatrix_ matdensegetsubmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdenserestoresubmatrix_ MATDENSERESTORESUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdenserestoresubmatrix_ matdenserestoresubmatrix
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matdensegetlda_(Mat A,PetscInt *lda, int *__ierr)
{
*__ierr = MatDenseGetLDA(
	(Mat)PetscToPointer((A) ),lda);
}
PETSC_EXTERN void  matdensesetlda_(Mat A,PetscInt *lda, int *__ierr)
{
*__ierr = MatDenseSetLDA(
	(Mat)PetscToPointer((A) ),*lda);
}
PETSC_EXTERN void  matdensegetcolumnvec_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseGetColumnVec(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdenserestorecolumnvec_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseRestoreColumnVec(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdensegetcolumnvecread_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseGetColumnVecRead(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdenserestorecolumnvecread_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseRestoreColumnVecRead(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdensegetcolumnvecwrite_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseGetColumnVecWrite(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdenserestorecolumnvecwrite_(Mat A,PetscInt *col,Vec *v, int *__ierr)
{
*__ierr = MatDenseRestoreColumnVecWrite(
	(Mat)PetscToPointer((A) ),*col,v);
}
PETSC_EXTERN void  matdensegetsubmatrix_(Mat A,PetscInt *rbegin,PetscInt *rend,PetscInt *cbegin,PetscInt *cend,Mat *v, int *__ierr)
{
*__ierr = MatDenseGetSubMatrix(
	(Mat)PetscToPointer((A) ),*rbegin,*rend,*cbegin,*cend,v);
}
PETSC_EXTERN void  matdenserestoresubmatrix_(Mat A,Mat *v, int *__ierr)
{
*__ierr = MatDenseRestoreSubMatrix(
	(Mat)PetscToPointer((A) ),v);
}
#if defined(__cplusplus)
}
#endif

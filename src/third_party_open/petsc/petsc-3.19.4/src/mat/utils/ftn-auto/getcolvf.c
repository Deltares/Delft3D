#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* getcolv.c */
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
#define matgetcolumnvector_ MATGETCOLUMNVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnvector_ matgetcolumnvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnnorms_ MATGETCOLUMNNORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnnorms_ matgetcolumnnorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnsumsrealpart_ MATGETCOLUMNSUMSREALPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnsumsrealpart_ matgetcolumnsumsrealpart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnsumsimaginarypart_ MATGETCOLUMNSUMSIMAGINARYPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnsumsimaginarypart_ matgetcolumnsumsimaginarypart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnsums_ MATGETCOLUMNSUMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnsums_ matgetcolumnsums
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnmeansrealpart_ MATGETCOLUMNMEANSREALPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnmeansrealpart_ matgetcolumnmeansrealpart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnmeansimaginarypart_ MATGETCOLUMNMEANSIMAGINARYPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnmeansimaginarypart_ matgetcolumnmeansimaginarypart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnmeans_ MATGETCOLUMNMEANS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnmeans_ matgetcolumnmeans
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetcolumnreductions_ MATGETCOLUMNREDUCTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetcolumnreductions_ matgetcolumnreductions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matgetcolumnvector_(Mat A,Vec yy,PetscInt *col, int *__ierr)
{
*__ierr = MatGetColumnVector(
	(Mat)PetscToPointer((A) ),
	(Vec)PetscToPointer((yy) ),*col);
}
PETSC_EXTERN void  matgetcolumnnorms_(Mat A,NormType *type,PetscReal norms[], int *__ierr)
{
*__ierr = MatGetColumnNorms(
	(Mat)PetscToPointer((A) ),*type,norms);
}
PETSC_EXTERN void  matgetcolumnsumsrealpart_(Mat A,PetscReal sums[], int *__ierr)
{
*__ierr = MatGetColumnSumsRealPart(
	(Mat)PetscToPointer((A) ),sums);
}
PETSC_EXTERN void  matgetcolumnsumsimaginarypart_(Mat A,PetscReal sums[], int *__ierr)
{
*__ierr = MatGetColumnSumsImaginaryPart(
	(Mat)PetscToPointer((A) ),sums);
}
PETSC_EXTERN void  matgetcolumnsums_(Mat A,PetscScalar sums[], int *__ierr)
{
*__ierr = MatGetColumnSums(
	(Mat)PetscToPointer((A) ),sums);
}
PETSC_EXTERN void  matgetcolumnmeansrealpart_(Mat A,PetscReal means[], int *__ierr)
{
*__ierr = MatGetColumnMeansRealPart(
	(Mat)PetscToPointer((A) ),means);
}
PETSC_EXTERN void  matgetcolumnmeansimaginarypart_(Mat A,PetscReal means[], int *__ierr)
{
*__ierr = MatGetColumnMeansImaginaryPart(
	(Mat)PetscToPointer((A) ),means);
}
PETSC_EXTERN void  matgetcolumnmeans_(Mat A,PetscScalar means[], int *__ierr)
{
*__ierr = MatGetColumnMeans(
	(Mat)PetscToPointer((A) ),means);
}
PETSC_EXTERN void  matgetcolumnreductions_(Mat A,PetscInt *type,PetscReal reductions[], int *__ierr)
{
*__ierr = MatGetColumnReductions(
	(Mat)PetscToPointer((A) ),*type,reductions);
}
#if defined(__cplusplus)
}
#endif

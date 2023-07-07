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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matgetcolumnvector_(Mat A,Vec yy,PetscInt *col, int *__ierr){
*__ierr = MatGetColumnVector(
	(Mat)PetscToPointer((A) ),
	(Vec)PetscToPointer((yy) ),*col);
}
PETSC_EXTERN void PETSC_STDCALL  matgetcolumnnorms_(Mat A,NormType *type,PetscReal norms[], int *__ierr){
*__ierr = MatGetColumnNorms(
	(Mat)PetscToPointer((A) ),*type,norms);
}
#if defined(__cplusplus)
}
#endif

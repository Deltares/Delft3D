#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* maij.c */
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
#define matmaijgetaij_ MATMAIJGETAIJ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmaijgetaij_ matmaijgetaij
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmaijredimension_ MATMAIJREDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmaijredimension_ matmaijredimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatemaij_ MATCREATEMAIJ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatemaij_ matcreatemaij
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matmaijgetaij_(Mat A,Mat *B, int *__ierr)
{
*__ierr = MatMAIJGetAIJ(
	(Mat)PetscToPointer((A) ),B);
}
PETSC_EXTERN void  matmaijredimension_(Mat A,PetscInt *dof,Mat *B, int *__ierr)
{
*__ierr = MatMAIJRedimension(
	(Mat)PetscToPointer((A) ),*dof,B);
}
PETSC_EXTERN void  matcreatemaij_(Mat A,PetscInt *dof,Mat *maij, int *__ierr)
{
*__ierr = MatCreateMAIJ(
	(Mat)PetscToPointer((A) ),*dof,maij);
}
#if defined(__cplusplus)
}
#endif

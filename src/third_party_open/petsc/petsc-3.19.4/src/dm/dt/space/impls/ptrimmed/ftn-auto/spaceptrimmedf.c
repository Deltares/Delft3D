#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* spaceptrimmed.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspaceptrimmedsetformdegree_ PETSCSPACEPTRIMMEDSETFORMDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspaceptrimmedsetformdegree_ petscspaceptrimmedsetformdegree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspaceptrimmedgetformdegree_ PETSCSPACEPTRIMMEDGETFORMDEGREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspaceptrimmedgetformdegree_ petscspaceptrimmedgetformdegree
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspaceptrimmedsetformdegree_(PetscSpace sp,PetscInt *formDegree, int *__ierr)
{
*__ierr = PetscSpacePTrimmedSetFormDegree(
	(PetscSpace)PetscToPointer((sp) ),*formDegree);
}
PETSC_EXTERN void  petscspaceptrimmedgetformdegree_(PetscSpace sp,PetscInt *formDegree, int *__ierr)
{
*__ierr = PetscSpacePTrimmedGetFormDegree(
	(PetscSpace)PetscToPointer((sp) ),formDegree);
}
#if defined(__cplusplus)
}
#endif

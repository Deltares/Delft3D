#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* superlu.c */
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
#define matsuperlusetiludroptol_ MATSUPERLUSETILUDROPTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsuperlusetiludroptol_ matsuperlusetiludroptol
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matsuperlusetiludroptol_(Mat F,PetscReal *dtol, int *__ierr)
{
*__ierr = MatSuperluSetILUDropTol(
	(Mat)PetscToPointer((F) ),*dtol);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* kaij.c */
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
#define matkaijsetaij_ MATKAIJSETAIJ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matkaijsetaij_ matkaijsetaij
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matkaijsetaij_(Mat A,Mat B, int *__ierr)
{
*__ierr = MatKAIJSetAIJ(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ));
}
#if defined(__cplusplus)
}
#endif

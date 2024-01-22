#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* adaptdsp.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsadaptdspsetpid_ TSADAPTDSPSETPID
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsadaptdspsetpid_ tsadaptdspsetpid
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsadaptdspsetpid_(TSAdapt adapt,PetscReal *kkI,PetscReal *kkP,PetscReal *kkD, int *__ierr)
{
*__ierr = TSAdaptDSPSetPID(
	(TSAdapt)PetscToPointer((adapt) ),*kkI,*kkP,*kkD);
}
#if defined(__cplusplus)
}
#endif

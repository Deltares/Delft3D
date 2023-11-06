#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* partshell.c */
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

#include "petscpartitioner.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionershellsetrandom_ PETSCPARTITIONERSHELLSETRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionershellsetrandom_ petscpartitionershellsetrandom
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionershellgetrandom_ PETSCPARTITIONERSHELLGETRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionershellgetrandom_ petscpartitionershellgetrandom
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscpartitionershellsetrandom_(PetscPartitioner part,PetscBool *random, int *__ierr)
{
*__ierr = PetscPartitionerShellSetRandom(
	(PetscPartitioner)PetscToPointer((part) ),*random);
}
PETSC_EXTERN void  petscpartitionershellgetrandom_(PetscPartitioner part,PetscBool *random, int *__ierr)
{
*__ierr = PetscPartitionerShellGetRandom(
	(PetscPartitioner)PetscToPointer((part) ),random);
}
#if defined(__cplusplus)
}
#endif

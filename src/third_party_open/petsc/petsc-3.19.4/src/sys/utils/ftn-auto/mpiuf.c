#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mpiu.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsequentialphasebegin_ PETSCSEQUENTIALPHASEBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsequentialphasebegin_ petscsequentialphasebegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsequentialphaseend_ PETSCSEQUENTIALPHASEEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsequentialphaseend_ petscsequentialphaseend
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsequentialphasebegin_(MPI_Fint * comm,int *ng, int *__ierr)
{
*__ierr = PetscSequentialPhaseBegin(
	MPI_Comm_f2c(*(comm)),*ng);
}
PETSC_EXTERN void  petscsequentialphaseend_(MPI_Fint * comm,int *ng, int *__ierr)
{
*__ierr = PetscSequentialPhaseEnd(
	MPI_Comm_f2c(*(comm)),*ng);
}
#if defined(__cplusplus)
}
#endif

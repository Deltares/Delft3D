#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vecmpicupm.cu */
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

#include <petscvec.h>
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veccreatempicuda_ VECCREATEMPICUDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veccreatempicuda_ veccreatempicuda
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  veccreatempicuda_(MPI_Fint * comm,PetscInt *n,PetscInt *N,Vec *v, int *__ierr)
{
*__ierr = VecCreateMPICUDA(
	MPI_Comm_f2c(*(comm)),*n,*N,v);
}
#if defined(__cplusplus)
}
#endif

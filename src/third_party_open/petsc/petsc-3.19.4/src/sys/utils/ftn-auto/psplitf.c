#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* psplit.c */
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
#define petscsplitownershipblock_ PETSCSPLITOWNERSHIPBLOCK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsplitownershipblock_ petscsplitownershipblock
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsplitownership_ PETSCSPLITOWNERSHIP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsplitownership_ petscsplitownership
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsplitownershipequal_ PETSCSPLITOWNERSHIPEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsplitownershipequal_ petscsplitownershipequal
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsplitownershipblock_(MPI_Fint * comm,PetscInt *bs,PetscInt *n,PetscInt *N, int *__ierr)
{
*__ierr = PetscSplitOwnershipBlock(
	MPI_Comm_f2c(*(comm)),*bs,n,N);
}
PETSC_EXTERN void  petscsplitownership_(MPI_Fint * comm,PetscInt *n,PetscInt *N, int *__ierr)
{
*__ierr = PetscSplitOwnership(
	MPI_Comm_f2c(*(comm)),n,N);
}
PETSC_EXTERN void  petscsplitownershipequal_(MPI_Fint * comm,PetscInt *n,PetscInt *N, int *__ierr)
{
*__ierr = PetscSplitOwnershipEqual(
	MPI_Comm_f2c(*(comm)),n,N);
}
#if defined(__cplusplus)
}
#endif

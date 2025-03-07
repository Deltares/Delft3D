#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mpits.c */
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
#define petsccommbuildtwosidedsettype_ PETSCCOMMBUILDTWOSIDEDSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsccommbuildtwosidedsettype_ petsccommbuildtwosidedsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsccommbuildtwosidedgettype_ PETSCCOMMBUILDTWOSIDEDGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsccommbuildtwosidedgettype_ petsccommbuildtwosidedgettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petsccommbuildtwosidedsettype_(MPI_Fint * comm,PetscBuildTwoSidedType *twosided, int *__ierr)
{
*__ierr = PetscCommBuildTwoSidedSetType(
	MPI_Comm_f2c(*(comm)),*twosided);
}
PETSC_EXTERN void  petsccommbuildtwosidedgettype_(MPI_Fint * comm,PetscBuildTwoSidedType *twosided, int *__ierr)
{
*__ierr = PetscCommBuildTwoSidedGetType(
	MPI_Comm_f2c(*(comm)),twosided);
}
#if defined(__cplusplus)
}
#endif

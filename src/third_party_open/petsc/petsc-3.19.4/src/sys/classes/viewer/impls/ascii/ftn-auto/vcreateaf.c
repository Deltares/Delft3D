#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vcreatea.c */
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

#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerasciigetstdout_ PETSCVIEWERASCIIGETSTDOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerasciigetstdout_ petscviewerasciigetstdout
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerasciigetstderr_ PETSCVIEWERASCIIGETSTDERR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerasciigetstderr_ petscviewerasciigetstderr
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerasciigetstdout_(MPI_Fint * comm,PetscViewer *viewer, int *__ierr)
{
*__ierr = PetscViewerASCIIGetStdout(
	MPI_Comm_f2c(*(comm)),viewer);
}
PETSC_EXTERN void  petscviewerasciigetstderr_(MPI_Fint * comm,PetscViewer *viewer, int *__ierr)
{
*__ierr = PetscViewerASCIIGetStderr(
	MPI_Comm_f2c(*(comm)),viewer);
}
#if defined(__cplusplus)
}
#endif

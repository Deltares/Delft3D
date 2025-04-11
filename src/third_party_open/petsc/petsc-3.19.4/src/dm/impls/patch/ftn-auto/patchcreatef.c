#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* patchcreate.c */
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

#include "petscdmpatch.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmpatchcreate_ DMPATCHCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmpatchcreate_ dmpatchcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmpatchcreate_(MPI_Fint * comm,DM *mesh, int *__ierr)
{
*__ierr = DMPatchCreate(
	MPI_Comm_f2c(*(comm)),mesh);
}
#if defined(__cplusplus)
}
#endif

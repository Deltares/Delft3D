#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sfcoord.c */
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

#include "petscsf.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetgraphfromcoordinates_ PETSCSFSETGRAPHFROMCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetgraphfromcoordinates_ petscsfsetgraphfromcoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsfsetgraphfromcoordinates_(PetscSF sf,PetscInt *nroots,PetscInt *nleaves,PetscInt *dim,PetscReal *tol, PetscReal *rootcoords, PetscReal *leafcoords, int *__ierr)
{
*__ierr = PetscSFSetGraphFromCoordinates(
	(PetscSF)PetscToPointer((sf) ),*nroots,*nleaves,*dim,*tol,rootcoords,leafcoords);
}
#if defined(__cplusplus)
}
#endif

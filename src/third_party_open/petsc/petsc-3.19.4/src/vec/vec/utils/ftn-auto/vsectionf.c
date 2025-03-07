#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vsection.c */
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

#include "petscsection.h"
#include "petscvec.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsectionvecview_ PETSCSECTIONVECVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsectionvecview_ petscsectionvecview
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsectionvecview_(PetscSection s,Vec v,PetscViewer viewer, int *__ierr)
{
*__ierr = PetscSectionVecView(
	(PetscSection)PetscToPointer((s) ),
	(Vec)PetscToPointer((v) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
#if defined(__cplusplus)
}
#endif

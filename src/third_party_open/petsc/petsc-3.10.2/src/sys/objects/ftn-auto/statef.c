#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* state.c */
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
#define petscobjectgetid_ PETSCOBJECTGETID
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectgetid_ petscobjectgetid
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscobjectgetid_(PetscObject obj,PetscObjectId *id, int *__ierr){
*__ierr = PetscObjectGetId(
	(PetscObject)PetscToPointer((obj) ),
	(PetscObjectId* )PetscToPointer((id) ));
}
#if defined(__cplusplus)
}
#endif

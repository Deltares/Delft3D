#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tagger.c */
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

#include "petscvec.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vectaggerdestroy_ VECTAGGERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vectaggerdestroy_ vectaggerdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vectaggersetup_ VECTAGGERSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vectaggersetup_ vectaggersetup
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  vectaggerdestroy_(VecTagger *tagger, int *__ierr){
*__ierr = VecTaggerDestroy(
	(VecTagger* )PetscToPointer((tagger) ));
}
PETSC_EXTERN void PETSC_STDCALL  vectaggersetup_(VecTagger *tagger, int *__ierr){
*__ierr = VecTaggerSetUp(*tagger);
}
#if defined(__cplusplus)
}
#endif

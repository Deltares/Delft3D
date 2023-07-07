#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vscreate.c */
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
#define vecscattersetfromoptions_ VECSCATTERSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattersetfromoptions_ vecscattersetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattercreate_ VECSCATTERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattercreate_ vecscattercreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  vecscattersetfromoptions_(VecScatter vscat, int *__ierr){
*__ierr = VecScatterSetFromOptions(
	(VecScatter)PetscToPointer((vscat) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecscattercreate_(Vec xin,IS ix,Vec yin,IS iy,VecScatter *newctx, int *__ierr){
*__ierr = VecScatterCreate(
	(Vec)PetscToPointer((xin) ),
	(IS)PetscToPointer((ix) ),
	(Vec)PetscToPointer((yin) ),
	(IS)PetscToPointer((iy) ),newctx);
}
#if defined(__cplusplus)
}
#endif

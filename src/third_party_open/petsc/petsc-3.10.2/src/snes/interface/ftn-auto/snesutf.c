#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snesut.c */
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

#include "petsc/private/snesimpl.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspmonitorsneslgresidualnormdestroy_ KSPMONITORSNESLGRESIDUALNORMDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspmonitorsneslgresidualnormdestroy_ kspmonitorsneslgresidualnormdestroy
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  kspmonitorsneslgresidualnormdestroy_(PetscObject **objs, int *__ierr){
*__ierr = KSPMonitorSNESLGResidualNormDestroy(objs);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dspacelagrange.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegetcontinuity_ PETSCDUALSPACELAGRANGEGETCONTINUITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegetcontinuity_ petscdualspacelagrangegetcontinuity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesetcontinuity_ PETSCDUALSPACELAGRANGESETCONTINUITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesetcontinuity_ petscdualspacelagrangesetcontinuity
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscdualspacelagrangegetcontinuity_(PetscDualSpace sp,PetscBool *continuous, int *__ierr){
*__ierr = PetscDualSpaceLagrangeGetContinuity(
	(PetscDualSpace)PetscToPointer((sp) ),continuous);
}
PETSC_EXTERN void PETSC_STDCALL  petscdualspacelagrangesetcontinuity_(PetscDualSpace sp,PetscBool *continuous, int *__ierr){
*__ierr = PetscDualSpaceLagrangeSetContinuity(
	(PetscDualSpace)PetscToPointer((sp) ),*continuous);
}
#if defined(__cplusplus)
}
#endif

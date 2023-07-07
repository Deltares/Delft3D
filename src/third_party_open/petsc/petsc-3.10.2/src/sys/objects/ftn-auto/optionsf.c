#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* options.c */
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
#define petscoptionscreate_ PETSCOPTIONSCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionscreate_ petscoptionscreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionsdestroy_ PETSCOPTIONSDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionsdestroy_ petscoptionsdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionsallused_ PETSCOPTIONSALLUSED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionsallused_ petscoptionsallused
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionsleft_ PETSCOPTIONSLEFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionsleft_ petscoptionsleft
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionsmonitorcancel_ PETSCOPTIONSMONITORCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionsmonitorcancel_ petscoptionsmonitorcancel
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscoptionscreate_(PetscOptions *options, int *__ierr){
*__ierr = PetscOptionsCreate(options);
}
PETSC_EXTERN void PETSC_STDCALL  petscoptionsdestroy_(PetscOptions *options, int *__ierr){
*__ierr = PetscOptionsDestroy(options);
}
PETSC_EXTERN void PETSC_STDCALL  petscoptionsallused_(PetscOptions options,PetscInt *N, int *__ierr){
*__ierr = PetscOptionsAllUsed(
	(PetscOptions)PetscToPointer((options) ),N);
}
PETSC_EXTERN void PETSC_STDCALL  petscoptionsleft_(PetscOptions options, int *__ierr){
*__ierr = PetscOptionsLeft(
	(PetscOptions)PetscToPointer((options) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscoptionsmonitorcancel_(int *__ierr ){
*__ierr = PetscOptionsMonitorCancel();
}
#if defined(__cplusplus)
}
#endif

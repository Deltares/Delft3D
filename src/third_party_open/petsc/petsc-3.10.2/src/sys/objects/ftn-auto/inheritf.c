#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* inherit.c */
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
#define petscobjectsetprintedoptions_ PETSCOBJECTSETPRINTEDOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectsetprintedoptions_ petscobjectsetprintedoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscobjectinheritprintedoptions_ PETSCOBJECTINHERITPRINTEDOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectinheritprintedoptions_ petscobjectinheritprintedoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscobjectsetfromoptions_ PETSCOBJECTSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectsetfromoptions_ petscobjectsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscobjectsetup_ PETSCOBJECTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectsetup_ petscobjectsetup
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscobjectsetprintedoptions_(PetscObject obj, int *__ierr){
*__ierr = PetscObjectSetPrintedOptions(
	(PetscObject)PetscToPointer((obj) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscobjectinheritprintedoptions_(PetscObject pobj,PetscObject obj, int *__ierr){
*__ierr = PetscObjectInheritPrintedOptions(
	(PetscObject)PetscToPointer((pobj) ),
	(PetscObject)PetscToPointer((obj) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscobjectsetfromoptions_(PetscObject obj, int *__ierr){
*__ierr = PetscObjectSetFromOptions(
	(PetscObject)PetscToPointer((obj) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscobjectsetup_(PetscObject obj, int *__ierr){
*__ierr = PetscObjectSetUp(
	(PetscObject)PetscToPointer((obj) ));
}
#if defined(__cplusplus)
}
#endif

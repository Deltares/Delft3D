#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gcomm.c */
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
#define petscobjectgettablevel_ PETSCOBJECTGETTABLEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectgettablevel_ petscobjectgettablevel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscobjectsettablevel_ PETSCOBJECTSETTABLEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectsettablevel_ petscobjectsettablevel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscobjectincrementtablevel_ PETSCOBJECTINCREMENTTABLEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscobjectincrementtablevel_ petscobjectincrementtablevel
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscobjectgettablevel_(PetscObject *obj,PetscInt *tab, int *__ierr)
{
*__ierr = PetscObjectGetTabLevel(*obj,tab);
}
PETSC_EXTERN void  petscobjectsettablevel_(PetscObject *obj,PetscInt *tab, int *__ierr)
{
*__ierr = PetscObjectSetTabLevel(*obj,*tab);
}
PETSC_EXTERN void  petscobjectincrementtablevel_(PetscObject *obj,PetscObject *oldobj,PetscInt *tab, int *__ierr)
{
*__ierr = PetscObjectIncrementTabLevel(*obj,*oldobj,*tab);
}
#if defined(__cplusplus)
}
#endif

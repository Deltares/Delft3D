#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matlab.c */
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

#include "petscmatlab.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmatlabenginedestroy_ PETSCMATLABENGINEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmatlabenginedestroy_ petscmatlabenginedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmatlabengineput_ PETSCMATLABENGINEPUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmatlabengineput_ petscmatlabengineput
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmatlabengineget_ PETSCMATLABENGINEGET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmatlabengineget_ petscmatlabengineget
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscmatlabenginedestroy_(PetscMatlabEngine *v, int *__ierr)
{
*__ierr = PetscMatlabEngineDestroy(v);
}
PETSC_EXTERN void  petscmatlabengineput_(PetscMatlabEngine mengine,PetscObject *obj, int *__ierr)
{
*__ierr = PetscMatlabEnginePut(
	(PetscMatlabEngine)PetscToPointer((mengine) ),*obj);
}
PETSC_EXTERN void  petscmatlabengineget_(PetscMatlabEngine mengine,PetscObject *obj, int *__ierr)
{
*__ierr = PetscMatlabEngineGet(
	(PetscMatlabEngine)PetscToPointer((mengine) ),*obj);
}
#if defined(__cplusplus)
}
#endif

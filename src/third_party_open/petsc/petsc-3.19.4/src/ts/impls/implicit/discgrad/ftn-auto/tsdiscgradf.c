#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tsdiscgrad.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsdiscgradisgonzalez_ TSDISCGRADISGONZALEZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsdiscgradisgonzalez_ tsdiscgradisgonzalez
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsdiscgradusegonzalez_ TSDISCGRADUSEGONZALEZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsdiscgradusegonzalez_ tsdiscgradusegonzalez
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsdiscgradisgonzalez_(TS ts,PetscBool *gonzalez, int *__ierr)
{
*__ierr = TSDiscGradIsGonzalez(
	(TS)PetscToPointer((ts) ),gonzalez);
}
PETSC_EXTERN void  tsdiscgradusegonzalez_(TS ts,PetscBool *flg, int *__ierr)
{
*__ierr = TSDiscGradUseGonzalez(
	(TS)PetscToPointer((ts) ),*flg);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* qn.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesqnsetrestarttype_ SNESQNSETRESTARTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesqnsetrestarttype_ snesqnsetrestarttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesqnsetscaletype_ SNESQNSETSCALETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesqnsetscaletype_ snesqnsetscaletype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesqnsettype_ SNESQNSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesqnsettype_ snesqnsettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesqnsetrestarttype_(SNES snes,SNESQNRestartType *rtype, int *__ierr)
{
*__ierr = SNESQNSetRestartType(
	(SNES)PetscToPointer((snes) ),*rtype);
}

PETSC_EXTERN void  snesqnsetscaletype_(SNES snes,SNESQNScaleType *stype, int *__ierr)
{
*__ierr = SNESQNSetScaleType(
	(SNES)PetscToPointer((snes) ),*stype);
}

PETSC_EXTERN void  snesqnsettype_(SNES snes,SNESQNType *qtype, int *__ierr)
{
*__ierr = SNESQNSetType(
	(SNES)PetscToPointer((snes) ),*qtype);
}
#if defined(__cplusplus)
}
#endif

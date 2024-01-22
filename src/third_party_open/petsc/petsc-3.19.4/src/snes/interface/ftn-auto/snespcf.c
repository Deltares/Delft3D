#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snespc.c */
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
#define snesapplynpc_ SNESAPPLYNPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesapplynpc_ snesapplynpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnpcfunction_ SNESGETNPCFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnpcfunction_ snesgetnpcfunction
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesapplynpc_(SNES snes,Vec x,Vec f,Vec y, int *__ierr)
{
*__ierr = SNESApplyNPC(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((f) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  snesgetnpcfunction_(SNES snes,Vec F,PetscReal *fnorm, int *__ierr)
{
*__ierr = SNESGetNPCFunction(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((F) ),fnorm);
}
#if defined(__cplusplus)
}
#endif

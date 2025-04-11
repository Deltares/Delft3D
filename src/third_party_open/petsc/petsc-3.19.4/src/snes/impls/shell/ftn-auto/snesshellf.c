#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snesshell.c */
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
#define snesshellgetcontext_ SNESSHELLGETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesshellgetcontext_ snesshellgetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesshellsetcontext_ SNESSHELLSETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesshellsetcontext_ snesshellsetcontext
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesshellgetcontext_(SNES snes,void*ctx, int *__ierr)
{
*__ierr = SNESShellGetContext(
	(SNES)PetscToPointer((snes) ),ctx);
}
PETSC_EXTERN void  snesshellsetcontext_(SNES snes,void*ctx, int *__ierr)
{
*__ierr = SNESShellSetContext(
	(SNES)PetscToPointer((snes) ),ctx);
}
#if defined(__cplusplus)
}
#endif

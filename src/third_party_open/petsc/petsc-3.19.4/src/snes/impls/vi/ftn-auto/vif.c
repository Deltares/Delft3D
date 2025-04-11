#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vi.c */
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
#define snesvisetvariablebounds_ SNESVISETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesvisetvariablebounds_ snesvisetvariablebounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesvigetvariablebounds_ SNESVIGETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesvigetvariablebounds_ snesvigetvariablebounds
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesvisetvariablebounds_(SNES snes,Vec xl,Vec xu, int *__ierr)
{
*__ierr = SNESVISetVariableBounds(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((xl) ),
	(Vec)PetscToPointer((xu) ));
}
PETSC_EXTERN void  snesvigetvariablebounds_(SNES snes,Vec *xl,Vec *xu, int *__ierr)
{
*__ierr = SNESVIGetVariableBounds(
	(SNES)PetscToPointer((snes) ),xl,xu);
}
#if defined(__cplusplus)
}
#endif

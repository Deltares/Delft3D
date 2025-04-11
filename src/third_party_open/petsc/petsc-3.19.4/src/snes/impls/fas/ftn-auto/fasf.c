#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fas.c */
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
#define snesfascreatecoarsevec_ SNESFASCREATECOARSEVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesfascreatecoarsevec_ snesfascreatecoarsevec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesfasrestrict_ SNESFASRESTRICT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesfasrestrict_ snesfasrestrict
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesfascreatecoarsevec_(SNES snes,Vec *Xcoarse, int *__ierr)
{
*__ierr = SNESFASCreateCoarseVec(
	(SNES)PetscToPointer((snes) ),Xcoarse);
}
PETSC_EXTERN void  snesfasrestrict_(SNES fine,Vec Xfine,Vec Xcoarse, int *__ierr)
{
*__ierr = SNESFASRestrict(
	(SNES)PetscToPointer((fine) ),
	(Vec)PetscToPointer((Xfine) ),
	(Vec)PetscToPointer((Xcoarse) ));
}
#if defined(__cplusplus)
}
#endif

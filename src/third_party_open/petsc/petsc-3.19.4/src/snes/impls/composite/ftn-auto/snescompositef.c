#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snescomposite.c */
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
#define snescompositegetsnes_ SNESCOMPOSITEGETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescompositegetsnes_ snescompositegetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescompositegetnumber_ SNESCOMPOSITEGETNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescompositegetnumber_ snescompositegetnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescompositesetdamping_ SNESCOMPOSITESETDAMPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescompositesetdamping_ snescompositesetdamping
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snescompositegetsnes_(SNES snes,PetscInt *n,SNES *subsnes, int *__ierr)
{
*__ierr = SNESCompositeGetSNES(
	(SNES)PetscToPointer((snes) ),*n,subsnes);
}
PETSC_EXTERN void  snescompositegetnumber_(SNES snes,PetscInt *n, int *__ierr)
{
*__ierr = SNESCompositeGetNumber(
	(SNES)PetscToPointer((snes) ),n);
}
PETSC_EXTERN void  snescompositesetdamping_(SNES snes,PetscInt *n,PetscReal *dmp, int *__ierr)
{
*__ierr = SNESCompositeSetDamping(
	(SNES)PetscToPointer((snes) ),*n,*dmp);
}
#if defined(__cplusplus)
}
#endif

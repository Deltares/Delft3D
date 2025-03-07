#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* itcl.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetusefischerguess_ KSPSETUSEFISCHERGUESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetusefischerguess_ kspsetusefischerguess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetguess_ KSPSETGUESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetguess_ kspsetguess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetguess_ KSPGETGUESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetguess_ kspgetguess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetfromoptions_ KSPSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetfromoptions_ kspsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspresetfromoptions_ KSPRESETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspresetfromoptions_ kspresetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspsetusefischerguess_(KSP ksp,PetscInt *model,PetscInt *size, int *__ierr)
{
*__ierr = KSPSetUseFischerGuess(
	(KSP)PetscToPointer((ksp) ),*model,*size);
}
PETSC_EXTERN void  kspsetguess_(KSP ksp,KSPGuess *guess, int *__ierr)
{
*__ierr = KSPSetGuess(
	(KSP)PetscToPointer((ksp) ),*guess);
}
PETSC_EXTERN void  kspgetguess_(KSP ksp,KSPGuess *guess, int *__ierr)
{
*__ierr = KSPGetGuess(
	(KSP)PetscToPointer((ksp) ),guess);
}
PETSC_EXTERN void  kspsetfromoptions_(KSP ksp, int *__ierr)
{
*__ierr = KSPSetFromOptions(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspresetfromoptions_(KSP ksp, int *__ierr)
{
*__ierr = KSPResetFromOptions(
	(KSP)PetscToPointer((ksp) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* alpha2.c */
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
#define tsalpha2setradius_ TSALPHA2SETRADIUS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalpha2setradius_ tsalpha2setradius
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsalpha2setparams_ TSALPHA2SETPARAMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalpha2setparams_ tsalpha2setparams
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsalpha2getparams_ TSALPHA2GETPARAMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalpha2getparams_ tsalpha2getparams
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsalpha2setradius_(TS ts,PetscReal *radius, int *__ierr)
{
*__ierr = TSAlpha2SetRadius(
	(TS)PetscToPointer((ts) ),*radius);
}
PETSC_EXTERN void  tsalpha2setparams_(TS ts,PetscReal *alpha_m,PetscReal *alpha_f,PetscReal *gamma,PetscReal *beta, int *__ierr)
{
*__ierr = TSAlpha2SetParams(
	(TS)PetscToPointer((ts) ),*alpha_m,*alpha_f,*gamma,*beta);
}
PETSC_EXTERN void  tsalpha2getparams_(TS ts,PetscReal *alpha_m,PetscReal *alpha_f,PetscReal *gamma,PetscReal *beta, int *__ierr)
{
*__ierr = TSAlpha2GetParams(
	(TS)PetscToPointer((ts) ),alpha_m,alpha_f,gamma,beta);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* alpha1.c */
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
#define tsalphasetradius_ TSALPHASETRADIUS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalphasetradius_ tsalphasetradius
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsalphasetparams_ TSALPHASETPARAMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalphasetparams_ tsalphasetparams
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tsalphagetparams_ TSALPHAGETPARAMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tsalphagetparams_ tsalphagetparams
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tsalphasetradius_(TS ts,PetscReal *radius, int *__ierr)
{
*__ierr = TSAlphaSetRadius(
	(TS)PetscToPointer((ts) ),*radius);
}
PETSC_EXTERN void  tsalphasetparams_(TS ts,PetscReal *alpha_m,PetscReal *alpha_f,PetscReal *gamma, int *__ierr)
{
*__ierr = TSAlphaSetParams(
	(TS)PetscToPointer((ts) ),*alpha_m,*alpha_f,*gamma);
}
PETSC_EXTERN void  tsalphagetparams_(TS ts,PetscReal *alpha_m,PetscReal *alpha_f,PetscReal *gamma, int *__ierr)
{
*__ierr = TSAlphaGetParams(
	(TS)PetscToPointer((ts) ),alpha_m,alpha_f,gamma);
}
#if defined(__cplusplus)
}
#endif

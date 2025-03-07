#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* eisen.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pceisenstatsetomega_ PCEISENSTATSETOMEGA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pceisenstatsetomega_ pceisenstatsetomega
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pceisenstatsetnodiagonalscaling_ PCEISENSTATSETNODIAGONALSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pceisenstatsetnodiagonalscaling_ pceisenstatsetnodiagonalscaling
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pceisenstatgetomega_ PCEISENSTATGETOMEGA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pceisenstatgetomega_ pceisenstatgetomega
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pceisenstatgetnodiagonalscaling_ PCEISENSTATGETNODIAGONALSCALING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pceisenstatgetnodiagonalscaling_ pceisenstatgetnodiagonalscaling
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pceisenstatsetomega_(PC pc,PetscReal *omega, int *__ierr)
{
*__ierr = PCEisenstatSetOmega(
	(PC)PetscToPointer((pc) ),*omega);
}
PETSC_EXTERN void  pceisenstatsetnodiagonalscaling_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCEisenstatSetNoDiagonalScaling(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pceisenstatgetomega_(PC pc,PetscReal *omega, int *__ierr)
{
*__ierr = PCEisenstatGetOmega(
	(PC)PetscToPointer((pc) ),omega);
}
PETSC_EXTERN void  pceisenstatgetnodiagonalscaling_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCEisenstatGetNoDiagonalScaling(
	(PC)PetscToPointer((pc) ),flg);
}
#if defined(__cplusplus)
}
#endif

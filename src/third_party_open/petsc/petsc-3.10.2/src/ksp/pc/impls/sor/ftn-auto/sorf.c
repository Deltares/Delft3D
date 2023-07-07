#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sor.c */
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
#define pcsorgetsymmetric_ PCSORGETSYMMETRIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorgetsymmetric_ pcsorgetsymmetric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsorgetomega_ PCSORGETOMEGA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorgetomega_ pcsorgetomega
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsorgetiterations_ PCSORGETITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorgetiterations_ pcsorgetiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsorsetsymmetric_ PCSORSETSYMMETRIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorsetsymmetric_ pcsorsetsymmetric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsorsetomega_ PCSORSETOMEGA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorsetomega_ pcsorsetomega
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsorsetiterations_ PCSORSETITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsorsetiterations_ pcsorsetiterations
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcsorgetsymmetric_(PC pc,MatSORType *flag, int *__ierr){
*__ierr = PCSORGetSymmetric(
	(PC)PetscToPointer((pc) ),
	(MatSORType* )PetscToPointer((flag) ));
}
PETSC_EXTERN void PETSC_STDCALL  pcsorgetomega_(PC pc,PetscReal *omega, int *__ierr){
*__ierr = PCSORGetOmega(
	(PC)PetscToPointer((pc) ),omega);
}
PETSC_EXTERN void PETSC_STDCALL  pcsorgetiterations_(PC pc,PetscInt *its,PetscInt *lits, int *__ierr){
*__ierr = PCSORGetIterations(
	(PC)PetscToPointer((pc) ),its,lits);
}
PETSC_EXTERN void PETSC_STDCALL  pcsorsetsymmetric_(PC pc,MatSORType *flag, int *__ierr){
*__ierr = PCSORSetSymmetric(
	(PC)PetscToPointer((pc) ),*flag);
}
PETSC_EXTERN void PETSC_STDCALL  pcsorsetomega_(PC pc,PetscReal *omega, int *__ierr){
*__ierr = PCSORSetOmega(
	(PC)PetscToPointer((pc) ),*omega);
}
PETSC_EXTERN void PETSC_STDCALL  pcsorsetiterations_(PC pc,PetscInt *its,PetscInt *lits, int *__ierr){
*__ierr = PCSORSetIterations(
	(PC)PetscToPointer((pc) ),*its,*lits);
}
#if defined(__cplusplus)
}
#endif

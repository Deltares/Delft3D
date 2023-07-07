#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* hypre.c */
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
#define pchypresetdiscretegradient_ PCHYPRESETDISCRETEGRADIENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetdiscretegradient_ pchypresetdiscretegradient
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchypresetdiscretecurl_ PCHYPRESETDISCRETECURL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetdiscretecurl_ pchypresetdiscretecurl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchypresetinterpolations_ PCHYPRESETINTERPOLATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetinterpolations_ pchypresetinterpolations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchypresetalphapoissonmatrix_ PCHYPRESETALPHAPOISSONMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetalphapoissonmatrix_ pchypresetalphapoissonmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchypresetbetapoissonmatrix_ PCHYPRESETBETAPOISSONMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetbetapoissonmatrix_ pchypresetbetapoissonmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchypresetedgeconstantvectors_ PCHYPRESETEDGECONSTANTVECTORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchypresetedgeconstantvectors_ pchypresetedgeconstantvectors
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pchypresetdiscretegradient_(PC pc,Mat G, int *__ierr){
*__ierr = PCHYPRESetDiscreteGradient(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((G) ));
}
PETSC_EXTERN void PETSC_STDCALL  pchypresetdiscretecurl_(PC pc,Mat C, int *__ierr){
*__ierr = PCHYPRESetDiscreteCurl(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((C) ));
}
PETSC_EXTERN void PETSC_STDCALL  pchypresetinterpolations_(PC pc,PetscInt *dim,Mat RT_PiFull,Mat RT_Pi[],Mat ND_PiFull,Mat ND_Pi[], int *__ierr){
*__ierr = PCHYPRESetInterpolations(
	(PC)PetscToPointer((pc) ),*dim,
	(Mat)PetscToPointer((RT_PiFull) ),RT_Pi,
	(Mat)PetscToPointer((ND_PiFull) ),ND_Pi);
}
PETSC_EXTERN void PETSC_STDCALL  pchypresetalphapoissonmatrix_(PC pc,Mat A, int *__ierr){
*__ierr = PCHYPRESetAlphaPoissonMatrix(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((A) ));
}
PETSC_EXTERN void PETSC_STDCALL  pchypresetbetapoissonmatrix_(PC pc,Mat A, int *__ierr){
*__ierr = PCHYPRESetBetaPoissonMatrix(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((A) ));
}
PETSC_EXTERN void PETSC_STDCALL  pchypresetedgeconstantvectors_(PC pc,Vec ozz,Vec zoz,Vec zzo, int *__ierr){
*__ierr = PCHYPRESetEdgeConstantVectors(
	(PC)PetscToPointer((pc) ),
	(Vec)PetscToPointer((ozz) ),
	(Vec)PetscToPointer((zoz) ),
	(Vec)PetscToPointer((zzo) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* brgn.c */
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

#include "petsctao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobrgngetsubsolver_ TAOBRGNGETSUBSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobrgngetsubsolver_ taobrgngetsubsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobrgnsetregularizerweight_ TAOBRGNSETREGULARIZERWEIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobrgnsetregularizerweight_ taobrgnsetregularizerweight
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobrgnsetl1smoothepsilon_ TAOBRGNSETL1SMOOTHEPSILON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobrgnsetl1smoothepsilon_ taobrgnsetl1smoothepsilon
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taobrgnsetdictionarymatrix_ TAOBRGNSETDICTIONARYMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taobrgnsetdictionarymatrix_ taobrgnsetdictionarymatrix
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taobrgngetsubsolver_(Tao tao,Tao *subsolver, int *__ierr)
{
*__ierr = TaoBRGNGetSubsolver(
	(Tao)PetscToPointer((tao) ),subsolver);
}
PETSC_EXTERN void  taobrgnsetregularizerweight_(Tao tao,PetscReal *lambda, int *__ierr)
{
*__ierr = TaoBRGNSetRegularizerWeight(
	(Tao)PetscToPointer((tao) ),*lambda);
}
PETSC_EXTERN void  taobrgnsetl1smoothepsilon_(Tao tao,PetscReal *epsilon, int *__ierr)
{
*__ierr = TaoBRGNSetL1SmoothEpsilon(
	(Tao)PetscToPointer((tao) ),*epsilon);
}
PETSC_EXTERN void  taobrgnsetdictionarymatrix_(Tao tao,Mat dict, int *__ierr)
{
*__ierr = TaoBRGNSetDictionaryMatrix(
	(Tao)PetscToPointer((tao) ),
	(Mat)PetscToPointer((dict) ));
}
#if defined(__cplusplus)
}
#endif

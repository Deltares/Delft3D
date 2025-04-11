#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* hpddm.cxx */
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
#define ksphpddmsetdeflationmat_ KSPHPDDMSETDEFLATIONMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksphpddmsetdeflationmat_ ksphpddmsetdeflationmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksphpddmgetdeflationmat_ KSPHPDDMGETDEFLATIONMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksphpddmgetdeflationmat_ ksphpddmgetdeflationmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksphpddmsettype_ KSPHPDDMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksphpddmsettype_ ksphpddmsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksphpddmgettype_ KSPHPDDMGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksphpddmgettype_ ksphpddmgettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  ksphpddmsetdeflationmat_(KSP ksp,Mat U, int *__ierr)
{
*__ierr = KSPHPDDMSetDeflationMat(
	(KSP)PetscToPointer((ksp) ),
	(Mat)PetscToPointer((U) ));
}
PETSC_EXTERN void  ksphpddmgetdeflationmat_(KSP ksp,Mat *U, int *__ierr)
{
*__ierr = KSPHPDDMGetDeflationMat(
	(KSP)PetscToPointer((ksp) ),U);
}
PETSC_EXTERN void  ksphpddmsettype_(KSP ksp,KSPHPDDMType *type, int *__ierr)
{
*__ierr = KSPHPDDMSetType(
	(KSP)PetscToPointer((ksp) ),*type);
}
PETSC_EXTERN void  ksphpddmgettype_(KSP ksp,KSPHPDDMType *type, int *__ierr)
{
*__ierr = KSPHPDDMGetType(
	(KSP)PetscToPointer((ksp) ),type);
}
#if defined(__cplusplus)
}
#endif

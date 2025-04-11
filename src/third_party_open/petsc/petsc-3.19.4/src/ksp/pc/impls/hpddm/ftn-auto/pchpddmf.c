#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pchpddm.cxx */
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
#define pchpddmsetauxiliarymat_ PCHPDDMSETAUXILIARYMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmsetauxiliarymat_ pchpddmsetauxiliarymat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmhasneumannmat_ PCHPDDMHASNEUMANNMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmhasneumannmat_ pchpddmhasneumannmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmsetrhsmat_ PCHPDDMSETRHSMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmsetrhsmat_ pchpddmsetrhsmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmsetcoarsecorrectiontype_ PCHPDDMSETCOARSECORRECTIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmsetcoarsecorrectiontype_ pchpddmsetcoarsecorrectiontype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmgetcoarsecorrectiontype_ PCHPDDMGETCOARSECORRECTIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmgetcoarsecorrectiontype_ pchpddmgetcoarsecorrectiontype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmsetstsharesubksp_ PCHPDDMSETSTSHARESUBKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmsetstsharesubksp_ pchpddmsetstsharesubksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmgetstsharesubksp_ PCHPDDMGETSTSHARESUBKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmgetstsharesubksp_ pchpddmgetstsharesubksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pchpddmsetdeflationmat_ PCHPDDMSETDEFLATIONMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pchpddmsetdeflationmat_ pchpddmsetdeflationmat
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pchpddmsetauxiliarymat_(PC pc,IS is,Mat A,PetscErrorCode (*setup)(Mat, PetscReal, Vec, Vec, PetscReal, IS, void *),void*setup_ctx, int *__ierr)
{
*__ierr = PCHPDDMSetAuxiliaryMat(
	(PC)PetscToPointer((pc) ),
	(IS)PetscToPointer((is) ),
	(Mat)PetscToPointer((A) ),setup,setup_ctx);
}
PETSC_EXTERN void  pchpddmhasneumannmat_(PC pc,PetscBool *has, int *__ierr)
{
*__ierr = PCHPDDMHasNeumannMat(
	(PC)PetscToPointer((pc) ),*has);
}
PETSC_EXTERN void  pchpddmsetrhsmat_(PC pc,Mat B, int *__ierr)
{
*__ierr = PCHPDDMSetRHSMat(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void  pchpddmsetcoarsecorrectiontype_(PC pc,PCHPDDMCoarseCorrectionType *type, int *__ierr)
{
*__ierr = PCHPDDMSetCoarseCorrectionType(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pchpddmgetcoarsecorrectiontype_(PC pc,PCHPDDMCoarseCorrectionType *type, int *__ierr)
{
*__ierr = PCHPDDMGetCoarseCorrectionType(
	(PC)PetscToPointer((pc) ),type);
}
PETSC_EXTERN void  pchpddmsetstsharesubksp_(PC pc,PetscBool *share, int *__ierr)
{
*__ierr = PCHPDDMSetSTShareSubKSP(
	(PC)PetscToPointer((pc) ),*share);
}
PETSC_EXTERN void  pchpddmgetstsharesubksp_(PC pc,PetscBool *share, int *__ierr)
{
*__ierr = PCHPDDMGetSTShareSubKSP(
	(PC)PetscToPointer((pc) ),share);
}
PETSC_EXTERN void  pchpddmsetdeflationmat_(PC pc,IS is,Mat U, int *__ierr)
{
*__ierr = PCHPDDMSetDeflationMat(
	(PC)PetscToPointer((pc) ),
	(IS)PetscToPointer((is) ),
	(Mat)PetscToPointer((U) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* deflation.c */
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
#define pcdeflationsetinitonly_ PCDEFLATIONSETINITONLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetinitonly_ pcdeflationsetinitonly
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetlevels_ PCDEFLATIONSETLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetlevels_ pcdeflationsetlevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetreductionfactor_ PCDEFLATIONSETREDUCTIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetreductionfactor_ pcdeflationsetreductionfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetcorrectionfactor_ PCDEFLATIONSETCORRECTIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetcorrectionfactor_ pcdeflationsetcorrectionfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetspacetocompute_ PCDEFLATIONSETSPACETOCOMPUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetspacetocompute_ pcdeflationsetspacetocompute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetspace_ PCDEFLATIONSETSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetspace_ pcdeflationsetspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetprojectionnullspacemat_ PCDEFLATIONSETPROJECTIONNULLSPACEMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetprojectionnullspacemat_ pcdeflationsetprojectionnullspacemat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationsetcoarsemat_ PCDEFLATIONSETCOARSEMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationsetcoarsemat_ pcdeflationsetcoarsemat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationgetcoarseksp_ PCDEFLATIONGETCOARSEKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationgetcoarseksp_ pcdeflationgetcoarseksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcdeflationgetpc_ PCDEFLATIONGETPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcdeflationgetpc_ pcdeflationgetpc
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcdeflationsetinitonly_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCDeflationSetInitOnly(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcdeflationsetlevels_(PC pc,PetscInt *max, int *__ierr)
{
*__ierr = PCDeflationSetLevels(
	(PC)PetscToPointer((pc) ),*max);
}
PETSC_EXTERN void  pcdeflationsetreductionfactor_(PC pc,PetscInt *red, int *__ierr)
{
*__ierr = PCDeflationSetReductionFactor(
	(PC)PetscToPointer((pc) ),*red);
}
PETSC_EXTERN void  pcdeflationsetcorrectionfactor_(PC pc,PetscScalar *fact, int *__ierr)
{
*__ierr = PCDeflationSetCorrectionFactor(
	(PC)PetscToPointer((pc) ),*fact);
}
PETSC_EXTERN void  pcdeflationsetspacetocompute_(PC pc,PCDeflationSpaceType *type,PetscInt *size, int *__ierr)
{
*__ierr = PCDeflationSetSpaceToCompute(
	(PC)PetscToPointer((pc) ),*type,*size);
}
PETSC_EXTERN void  pcdeflationsetspace_(PC pc,Mat W,PetscBool *transpose, int *__ierr)
{
*__ierr = PCDeflationSetSpace(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((W) ),*transpose);
}
PETSC_EXTERN void  pcdeflationsetprojectionnullspacemat_(PC pc,Mat mat, int *__ierr)
{
*__ierr = PCDeflationSetProjectionNullSpaceMat(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  pcdeflationsetcoarsemat_(PC pc,Mat mat, int *__ierr)
{
*__ierr = PCDeflationSetCoarseMat(
	(PC)PetscToPointer((pc) ),
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  pcdeflationgetcoarseksp_(PC pc,KSP *ksp, int *__ierr)
{
*__ierr = PCDeflationGetCoarseKSP(
	(PC)PetscToPointer((pc) ),ksp);
}
PETSC_EXTERN void  pcdeflationgetpc_(PC pc,PC *apc, int *__ierr)
{
*__ierr = PCDeflationGetPC(
	(PC)PetscToPointer((pc) ),apc);
}
#if defined(__cplusplus)
}
#endif

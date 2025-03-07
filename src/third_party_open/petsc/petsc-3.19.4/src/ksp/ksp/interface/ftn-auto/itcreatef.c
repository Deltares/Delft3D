#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* itcreate.c */
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
#define kspsetnormtype_ KSPSETNORMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetnormtype_ kspsetnormtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetchecknormiteration_ KSPSETCHECKNORMITERATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetchecknormiteration_ kspsetchecknormiteration
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetlagnorm_ KSPSETLAGNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetlagnorm_ kspsetlagnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetsupportednorm_ KSPSETSUPPORTEDNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetsupportednorm_ kspsetsupportednorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetnormtype_ KSPGETNORMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetnormtype_ kspgetnormtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetoperators_ KSPSETOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetoperators_ kspsetoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetoperators_ KSPGETOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetoperators_ kspgetoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcreate_ KSPCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcreate_ kspcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspsetnormtype_(KSP ksp,KSPNormType *normtype, int *__ierr)
{
*__ierr = KSPSetNormType(
	(KSP)PetscToPointer((ksp) ),*normtype);
}
PETSC_EXTERN void  kspsetchecknormiteration_(KSP ksp,PetscInt *it, int *__ierr)
{
*__ierr = KSPSetCheckNormIteration(
	(KSP)PetscToPointer((ksp) ),*it);
}
PETSC_EXTERN void  kspsetlagnorm_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetLagNorm(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspsetsupportednorm_(KSP ksp,KSPNormType *normtype,PCSide *pcside,PetscInt *priority, int *__ierr)
{
*__ierr = KSPSetSupportedNorm(
	(KSP)PetscToPointer((ksp) ),*normtype,*pcside,*priority);
}
PETSC_EXTERN void  kspgetnormtype_(KSP ksp,KSPNormType *normtype, int *__ierr)
{
*__ierr = KSPGetNormType(
	(KSP)PetscToPointer((ksp) ),normtype);
}
PETSC_EXTERN void  kspsetoperators_(KSP ksp,Mat Amat,Mat Pmat, int *__ierr)
{
*__ierr = KSPSetOperators(
	(KSP)PetscToPointer((ksp) ),
	(Mat)PetscToPointer((Amat) ),
	(Mat)PetscToPointer((Pmat) ));
}
PETSC_EXTERN void  kspgetoperators_(KSP ksp,Mat *Amat,Mat *Pmat, int *__ierr)
{
*__ierr = KSPGetOperators(
	(KSP)PetscToPointer((ksp) ),Amat,Pmat);
}
PETSC_EXTERN void  kspcreate_(MPI_Fint * comm,KSP *inksp, int *__ierr)
{
*__ierr = KSPCreate(
	MPI_Comm_f2c(*(comm)),inksp);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fcg.c */
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
#define kspfcgsetmmax_ KSPFCGSETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcgsetmmax_ kspfcgsetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfcggetmmax_ KSPFCGGETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcggetmmax_ kspfcggetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfcgsetnprealloc_ KSPFCGSETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcgsetnprealloc_ kspfcgsetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfcggetnprealloc_ KSPFCGGETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcggetnprealloc_ kspfcggetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfcgsettruncationtype_ KSPFCGSETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcgsettruncationtype_ kspfcgsettruncationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspfcggettruncationtype_ KSPFCGGETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspfcggettruncationtype_ kspfcggettruncationtype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspfcgsetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPFCGSetMmax(
	(KSP)PetscToPointer((ksp) ),*mmax);
}

PETSC_EXTERN void  kspfcggetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPFCGGetMmax(
	(KSP)PetscToPointer((ksp) ),mmax);
}
PETSC_EXTERN void  kspfcgsetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPFCGSetNprealloc(
	(KSP)PetscToPointer((ksp) ),*nprealloc);
}
PETSC_EXTERN void  kspfcggetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPFCGGetNprealloc(
	(KSP)PetscToPointer((ksp) ),nprealloc);
}
PETSC_EXTERN void  kspfcgsettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPFCGSetTruncationType(
	(KSP)PetscToPointer((ksp) ),*truncstrat);
}
PETSC_EXTERN void  kspfcggettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPFCGGetTruncationType(
	(KSP)PetscToPointer((ksp) ),truncstrat);
}
#if defined(__cplusplus)
}
#endif

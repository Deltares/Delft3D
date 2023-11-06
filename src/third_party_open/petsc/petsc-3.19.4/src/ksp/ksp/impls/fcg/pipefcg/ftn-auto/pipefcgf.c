#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pipefcg.c */
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
#define ksppipefcgsetmmax_ KSPPIPEFCGSETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcgsetmmax_ ksppipefcgsetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefcggetmmax_ KSPPIPEFCGGETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcggetmmax_ ksppipefcggetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefcgsetnprealloc_ KSPPIPEFCGSETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcgsetnprealloc_ ksppipefcgsetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefcggetnprealloc_ KSPPIPEFCGGETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcggetnprealloc_ ksppipefcggetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefcgsettruncationtype_ KSPPIPEFCGSETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcgsettruncationtype_ ksppipefcgsettruncationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefcggettruncationtype_ KSPPIPEFCGGETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefcggettruncationtype_ ksppipefcggettruncationtype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  ksppipefcgsetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPPIPEFCGSetMmax(
	(KSP)PetscToPointer((ksp) ),*mmax);
}
PETSC_EXTERN void  ksppipefcggetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPPIPEFCGGetMmax(
	(KSP)PetscToPointer((ksp) ),mmax);
}
PETSC_EXTERN void  ksppipefcgsetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPPIPEFCGSetNprealloc(
	(KSP)PetscToPointer((ksp) ),*nprealloc);
}
PETSC_EXTERN void  ksppipefcggetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPPIPEFCGGetNprealloc(
	(KSP)PetscToPointer((ksp) ),nprealloc);
}
PETSC_EXTERN void  ksppipefcgsettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPPIPEFCGSetTruncationType(
	(KSP)PetscToPointer((ksp) ),*truncstrat);
}
PETSC_EXTERN void  ksppipefcggettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPPIPEFCGGetTruncationType(
	(KSP)PetscToPointer((ksp) ),truncstrat);
}
#if defined(__cplusplus)
}
#endif

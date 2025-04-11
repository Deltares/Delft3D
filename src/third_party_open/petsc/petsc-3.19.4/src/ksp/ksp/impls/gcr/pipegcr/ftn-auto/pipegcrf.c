#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pipegcr.c */
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
#define ksppipegcrsetunrollw_ KSPPIPEGCRSETUNROLLW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrsetunrollw_ ksppipegcrsetunrollw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrgetunrollw_ KSPPIPEGCRGETUNROLLW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrgetunrollw_ ksppipegcrgetunrollw
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrsetmmax_ KSPPIPEGCRSETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrsetmmax_ ksppipegcrsetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrgetmmax_ KSPPIPEGCRGETMMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrgetmmax_ ksppipegcrgetmmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrsetnprealloc_ KSPPIPEGCRSETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrsetnprealloc_ ksppipegcrsetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrgetnprealloc_ KSPPIPEGCRGETNPREALLOC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrgetnprealloc_ ksppipegcrgetnprealloc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrsettruncationtype_ KSPPIPEGCRSETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrsettruncationtype_ ksppipegcrsettruncationtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipegcrgettruncationtype_ KSPPIPEGCRGETTRUNCATIONTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipegcrgettruncationtype_ ksppipegcrgettruncationtype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  ksppipegcrsetunrollw_(KSP ksp,PetscBool *unroll_w, int *__ierr)
{
*__ierr = KSPPIPEGCRSetUnrollW(
	(KSP)PetscToPointer((ksp) ),*unroll_w);
}
PETSC_EXTERN void  ksppipegcrgetunrollw_(KSP ksp,PetscBool *unroll_w, int *__ierr)
{
*__ierr = KSPPIPEGCRGetUnrollW(
	(KSP)PetscToPointer((ksp) ),unroll_w);
}
PETSC_EXTERN void  ksppipegcrsetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPPIPEGCRSetMmax(
	(KSP)PetscToPointer((ksp) ),*mmax);
}

PETSC_EXTERN void  ksppipegcrgetmmax_(KSP ksp,PetscInt *mmax, int *__ierr)
{
*__ierr = KSPPIPEGCRGetMmax(
	(KSP)PetscToPointer((ksp) ),mmax);
}
PETSC_EXTERN void  ksppipegcrsetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPPIPEGCRSetNprealloc(
	(KSP)PetscToPointer((ksp) ),*nprealloc);
}
PETSC_EXTERN void  ksppipegcrgetnprealloc_(KSP ksp,PetscInt *nprealloc, int *__ierr)
{
*__ierr = KSPPIPEGCRGetNprealloc(
	(KSP)PetscToPointer((ksp) ),nprealloc);
}
PETSC_EXTERN void  ksppipegcrsettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPPIPEGCRSetTruncationType(
	(KSP)PetscToPointer((ksp) ),*truncstrat);
}
PETSC_EXTERN void  ksppipegcrgettruncationtype_(KSP ksp,KSPFCDTruncationType *truncstrat, int *__ierr)
{
*__ierr = KSPPIPEGCRGetTruncationType(
	(KSP)PetscToPointer((ksp) ),truncstrat);
}
#if defined(__cplusplus)
}
#endif

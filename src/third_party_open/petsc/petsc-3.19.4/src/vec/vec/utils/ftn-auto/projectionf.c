#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* projection.c */
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

#include "petscvec.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichequal_ VECWHICHEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichequal_ vecwhichequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichlessthan_ VECWHICHLESSTHAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichlessthan_ vecwhichlessthan
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichgreaterthan_ VECWHICHGREATERTHAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichgreaterthan_ vecwhichgreaterthan
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichbetween_ VECWHICHBETWEEN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichbetween_ vecwhichbetween
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichbetweenorequal_ VECWHICHBETWEENOREQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichbetweenorequal_ vecwhichbetweenorequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwhichinactive_ VECWHICHINACTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwhichinactive_ vecwhichinactive
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecisaxpy_ VECISAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecisaxpy_ vecisaxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veciscopy_ VECISCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veciscopy_ veciscopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscomplementvec_ ISCOMPLEMENTVEC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscomplementvec_ iscomplementvec
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecisset_ VECISSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecisset_ vecisset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstepmaxbounded_ VECSTEPMAXBOUNDED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstepmaxbounded_ vecstepmaxbounded
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstepboundinfo_ VECSTEPBOUNDINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstepboundinfo_ vecstepboundinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstepmax_ VECSTEPMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstepmax_ vecstepmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecpow_ VECPOW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecpow_ vecpow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmedian_ VECMEDIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmedian_ vecmedian
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  vecwhichequal_(Vec Vec1,Vec Vec2,IS *S, int *__ierr)
{
*__ierr = VecWhichEqual(
	(Vec)PetscToPointer((Vec1) ),
	(Vec)PetscToPointer((Vec2) ),S);
}
PETSC_EXTERN void  vecwhichlessthan_(Vec Vec1,Vec Vec2,IS *S, int *__ierr)
{
*__ierr = VecWhichLessThan(
	(Vec)PetscToPointer((Vec1) ),
	(Vec)PetscToPointer((Vec2) ),S);
}
PETSC_EXTERN void  vecwhichgreaterthan_(Vec Vec1,Vec Vec2,IS *S, int *__ierr)
{
*__ierr = VecWhichGreaterThan(
	(Vec)PetscToPointer((Vec1) ),
	(Vec)PetscToPointer((Vec2) ),S);
}
PETSC_EXTERN void  vecwhichbetween_(Vec VecLow,Vec V,Vec VecHigh,IS *S, int *__ierr)
{
*__ierr = VecWhichBetween(
	(Vec)PetscToPointer((VecLow) ),
	(Vec)PetscToPointer((V) ),
	(Vec)PetscToPointer((VecHigh) ),S);
}

PETSC_EXTERN void  vecwhichbetweenorequal_(Vec VecLow,Vec V,Vec VecHigh,IS *S, int *__ierr)
{
*__ierr = VecWhichBetweenOrEqual(
	(Vec)PetscToPointer((VecLow) ),
	(Vec)PetscToPointer((V) ),
	(Vec)PetscToPointer((VecHigh) ),S);
}

PETSC_EXTERN void  vecwhichinactive_(Vec VecLow,Vec V,Vec D,Vec VecHigh,PetscBool *Strong,IS *S, int *__ierr)
{
*__ierr = VecWhichInactive(
	(Vec)PetscToPointer((VecLow) ),
	(Vec)PetscToPointer((V) ),
	(Vec)PetscToPointer((D) ),
	(Vec)PetscToPointer((VecHigh) ),*Strong,S);
}
PETSC_EXTERN void  vecisaxpy_(Vec vfull,IS is,PetscScalar *alpha,Vec vreduced, int *__ierr)
{
*__ierr = VecISAXPY(
	(Vec)PetscToPointer((vfull) ),
	(IS)PetscToPointer((is) ),*alpha,
	(Vec)PetscToPointer((vreduced) ));
}
PETSC_EXTERN void  veciscopy_(Vec vfull,IS is,ScatterMode *mode,Vec vreduced, int *__ierr)
{
*__ierr = VecISCopy(
	(Vec)PetscToPointer((vfull) ),
	(IS)PetscToPointer((is) ),*mode,
	(Vec)PetscToPointer((vreduced) ));
}
PETSC_EXTERN void  iscomplementvec_(IS S,Vec V,IS *T, int *__ierr)
{
*__ierr = ISComplementVec(
	(IS)PetscToPointer((S) ),
	(Vec)PetscToPointer((V) ),T);
}
PETSC_EXTERN void  vecisset_(Vec V,IS S,PetscScalar *c, int *__ierr)
{
*__ierr = VecISSet(
	(Vec)PetscToPointer((V) ),
	(IS)PetscToPointer((S) ),*c);
}
PETSC_EXTERN void  vecstepmaxbounded_(Vec X,Vec DX,Vec XL,Vec XU,PetscReal *stepmax, int *__ierr)
{
*__ierr = VecStepMaxBounded(
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((DX) ),
	(Vec)PetscToPointer((XL) ),
	(Vec)PetscToPointer((XU) ),stepmax);
}
PETSC_EXTERN void  vecstepboundinfo_(Vec X,Vec DX,Vec XL,Vec XU,PetscReal *boundmin,PetscReal *wolfemin,PetscReal *boundmax, int *__ierr)
{
*__ierr = VecStepBoundInfo(
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((DX) ),
	(Vec)PetscToPointer((XL) ),
	(Vec)PetscToPointer((XU) ),boundmin,wolfemin,boundmax);
}
PETSC_EXTERN void  vecstepmax_(Vec X,Vec DX,PetscReal *step, int *__ierr)
{
*__ierr = VecStepMax(
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((DX) ),step);
}
PETSC_EXTERN void  vecpow_(Vec v,PetscScalar *p, int *__ierr)
{
*__ierr = VecPow(
	(Vec)PetscToPointer((v) ),*p);
}
PETSC_EXTERN void  vecmedian_(Vec Vec1,Vec Vec2,Vec Vec3,Vec VMedian, int *__ierr)
{
*__ierr = VecMedian(
	(Vec)PetscToPointer((Vec1) ),
	(Vec)PetscToPointer((Vec2) ),
	(Vec)PetscToPointer((Vec3) ),
	(Vec)PetscToPointer((VMedian) ));
}
#if defined(__cplusplus)
}
#endif

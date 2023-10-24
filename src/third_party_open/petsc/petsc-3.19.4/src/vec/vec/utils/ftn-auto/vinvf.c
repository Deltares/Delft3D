#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vinv.c */
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
#define vecstrideset_ VECSTRIDESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstrideset_ vecstrideset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridescale_ VECSTRIDESCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridescale_ vecstridescale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridenorm_ VECSTRIDENORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridenorm_ vecstridenorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridemax_ VECSTRIDEMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridemax_ vecstridemax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridemin_ VECSTRIDEMIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridemin_ vecstridemin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridesum_ VECSTRIDESUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridesum_ vecstridesum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridescaleall_ VECSTRIDESCALEALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridescaleall_ vecstridescaleall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridenormall_ VECSTRIDENORMALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridenormall_ vecstridenormall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridemaxall_ VECSTRIDEMAXALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridemaxall_ vecstridemaxall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstrideminall_ VECSTRIDEMINALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstrideminall_ vecstrideminall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridesumall_ VECSTRIDESUMALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridesumall_ vecstridesumall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridegatherall_ VECSTRIDEGATHERALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridegatherall_ vecstridegatherall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridescatterall_ VECSTRIDESCATTERALL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridescatterall_ vecstridescatterall
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridegather_ VECSTRIDEGATHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridegather_ vecstridegather
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridescatter_ VECSTRIDESCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridescatter_ vecstridescatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridesubsetgather_ VECSTRIDESUBSETGATHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridesubsetgather_ vecstridesubsetgather
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecstridesubsetscatter_ VECSTRIDESUBSETSCATTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecstridesubsetscatter_ vecstridesubsetscatter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecexp_ VECEXP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecexp_ vecexp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veclog_ VECLOG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veclog_ veclog
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecabs_ VECABS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecabs_ vecabs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecsqrtabs_ VECSQRTABS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecsqrtabs_ vecsqrtabs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecimaginarypart_ VECIMAGINARYPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecimaginarypart_ vecimaginarypart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecrealpart_ VECREALPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecrealpart_ vecrealpart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecdotnorm2_ VECDOTNORM2
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecdotnorm2_ vecdotnorm2
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecsum_ VECSUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecsum_ vecsum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmean_ VECMEAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmean_ vecmean
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecshift_ VECSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecshift_ vecshift
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecpermute_ VECPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecpermute_ vecpermute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecequal_ VECEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecequal_ vecequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecuniqueentries_ VECUNIQUEENTRIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecuniqueentries_ vecuniqueentries
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  vecstrideset_(Vec v,PetscInt *start,PetscScalar *s, int *__ierr)
{
*__ierr = VecStrideSet(
	(Vec)PetscToPointer((v) ),*start,*s);
}
PETSC_EXTERN void  vecstridescale_(Vec v,PetscInt *start,PetscScalar *scale, int *__ierr)
{
*__ierr = VecStrideScale(
	(Vec)PetscToPointer((v) ),*start,*scale);
}
PETSC_EXTERN void  vecstridenorm_(Vec v,PetscInt *start,NormType *ntype,PetscReal *nrm, int *__ierr)
{
*__ierr = VecStrideNorm(
	(Vec)PetscToPointer((v) ),*start,*ntype,nrm);
}
PETSC_EXTERN void  vecstridemax_(Vec v,PetscInt *start,PetscInt *idex,PetscReal *nrm, int *__ierr)
{
*__ierr = VecStrideMax(
	(Vec)PetscToPointer((v) ),*start,idex,nrm);
}
PETSC_EXTERN void  vecstridemin_(Vec v,PetscInt *start,PetscInt *idex,PetscReal *nrm, int *__ierr)
{
*__ierr = VecStrideMin(
	(Vec)PetscToPointer((v) ),*start,idex,nrm);
}
PETSC_EXTERN void  vecstridesum_(Vec v,PetscInt *start,PetscScalar *sum, int *__ierr)
{
*__ierr = VecStrideSum(
	(Vec)PetscToPointer((v) ),*start,sum);
}
PETSC_EXTERN void  vecstridescaleall_(Vec v, PetscScalar *scales, int *__ierr)
{
*__ierr = VecStrideScaleAll(
	(Vec)PetscToPointer((v) ),scales);
}
PETSC_EXTERN void  vecstridenormall_(Vec v,NormType *ntype,PetscReal nrm[], int *__ierr)
{
*__ierr = VecStrideNormAll(
	(Vec)PetscToPointer((v) ),*ntype,nrm);
}
PETSC_EXTERN void  vecstridemaxall_(Vec v,PetscInt idex[],PetscReal nrm[], int *__ierr)
{
*__ierr = VecStrideMaxAll(
	(Vec)PetscToPointer((v) ),idex,nrm);
}
PETSC_EXTERN void  vecstrideminall_(Vec v,PetscInt idex[],PetscReal nrm[], int *__ierr)
{
*__ierr = VecStrideMinAll(
	(Vec)PetscToPointer((v) ),idex,nrm);
}
PETSC_EXTERN void  vecstridesumall_(Vec v,PetscScalar sums[], int *__ierr)
{
*__ierr = VecStrideSumAll(
	(Vec)PetscToPointer((v) ),sums);
}
PETSC_EXTERN void  vecstridegatherall_(Vec v,Vec s[],InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideGatherAll(
	(Vec)PetscToPointer((v) ),s,*addv);
}
PETSC_EXTERN void  vecstridescatterall_(Vec s[],Vec v,InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideScatterAll(s,
	(Vec)PetscToPointer((v) ),*addv);
}
PETSC_EXTERN void  vecstridegather_(Vec v,PetscInt *start,Vec s,InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideGather(
	(Vec)PetscToPointer((v) ),*start,
	(Vec)PetscToPointer((s) ),*addv);
}
PETSC_EXTERN void  vecstridescatter_(Vec s,PetscInt *start,Vec v,InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideScatter(
	(Vec)PetscToPointer((s) ),*start,
	(Vec)PetscToPointer((v) ),*addv);
}
PETSC_EXTERN void  vecstridesubsetgather_(Vec v,PetscInt *nidx, PetscInt idxv[], PetscInt idxs[],Vec s,InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideSubSetGather(
	(Vec)PetscToPointer((v) ),*nidx,idxv,idxs,
	(Vec)PetscToPointer((s) ),*addv);
}
PETSC_EXTERN void  vecstridesubsetscatter_(Vec s,PetscInt *nidx, PetscInt idxs[], PetscInt idxv[],Vec v,InsertMode *addv, int *__ierr)
{
*__ierr = VecStrideSubSetScatter(
	(Vec)PetscToPointer((s) ),*nidx,idxs,idxv,
	(Vec)PetscToPointer((v) ),*addv);
}
PETSC_EXTERN void  vecexp_(Vec v, int *__ierr)
{
*__ierr = VecExp(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  veclog_(Vec v, int *__ierr)
{
*__ierr = VecLog(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  vecabs_(Vec v, int *__ierr)
{
*__ierr = VecAbs(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  vecsqrtabs_(Vec v, int *__ierr)
{
*__ierr = VecSqrtAbs(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  vecimaginarypart_(Vec v, int *__ierr)
{
*__ierr = VecImaginaryPart(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  vecrealpart_(Vec v, int *__ierr)
{
*__ierr = VecRealPart(
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  vecdotnorm2_(Vec s,Vec t,PetscScalar *dp,PetscReal *nm, int *__ierr)
{
*__ierr = VecDotNorm2(
	(Vec)PetscToPointer((s) ),
	(Vec)PetscToPointer((t) ),dp,nm);
}
PETSC_EXTERN void  vecsum_(Vec v,PetscScalar *sum, int *__ierr)
{
*__ierr = VecSum(
	(Vec)PetscToPointer((v) ),sum);
}
PETSC_EXTERN void  vecmean_(Vec v,PetscScalar *mean, int *__ierr)
{
*__ierr = VecMean(
	(Vec)PetscToPointer((v) ),mean);
}
PETSC_EXTERN void  vecshift_(Vec v,PetscScalar *shift, int *__ierr)
{
*__ierr = VecShift(
	(Vec)PetscToPointer((v) ),*shift);
}
PETSC_EXTERN void  vecpermute_(Vec x,IS row,PetscBool *inv, int *__ierr)
{
*__ierr = VecPermute(
	(Vec)PetscToPointer((x) ),
	(IS)PetscToPointer((row) ),*inv);
}
PETSC_EXTERN void  vecequal_(Vec vec1,Vec vec2,PetscBool *flg, int *__ierr)
{
*__ierr = VecEqual(
	(Vec)PetscToPointer((vec1) ),
	(Vec)PetscToPointer((vec2) ),flg);
}
PETSC_EXTERN void  vecuniqueentries_(Vec vec,PetscInt *n,PetscScalar **e, int *__ierr)
{
*__ierr = VecUniqueEntries(
	(Vec)PetscToPointer((vec) ),n,e);
}
#if defined(__cplusplus)
}
#endif

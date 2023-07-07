#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* rvector.c */
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
#define vecmaxpointwisedivide_ VECMAXPOINTWISEDIVIDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmaxpointwisedivide_ vecmaxpointwisedivide
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecdot_ VECDOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecdot_ vecdot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecdotrealpart_ VECDOTREALPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecdotrealpart_ vecdotrealpart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecnorm_ VECNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecnorm_ vecnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecnormavailable_ VECNORMAVAILABLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecnormavailable_ vecnormavailable
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecnormalize_ VECNORMALIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecnormalize_ vecnormalize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vectdot_ VECTDOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vectdot_ vectdot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscale_ VECSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscale_ vecscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecset_ VECSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecset_ vecset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecaxpy_ VECAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecaxpy_ vecaxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecaxpby_ VECAXPBY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecaxpby_ vecaxpby
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecaxpbypcz_ VECAXPBYPCZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecaxpbypcz_ vecaxpbypcz
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecaypx_ VECAYPX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecaypx_ vecaypx
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecwaxpy_ VECWAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecwaxpy_ vecwaxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecgetvalues_ VECGETVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecgetvalues_ vecgetvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecsetvaluesblockedlocal_ VECSETVALUESBLOCKEDLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecsetvaluesblockedlocal_ vecsetvaluesblockedlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmtdot_ VECMTDOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmtdot_ vecmtdot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmdot_ VECMDOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmdot_ vecmdot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmaxpy_ VECMAXPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmaxpy_ vecmaxpy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecgetsubvector_ VECGETSUBVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecgetsubvector_ vecgetsubvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecrestoresubvector_ VECRESTORESUBVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecrestoresubvector_ vecrestoresubvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecgetlocalvectorread_ VECGETLOCALVECTORREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecgetlocalvectorread_ vecgetlocalvectorread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecrestorelocalvectorread_ VECRESTORELOCALVECTORREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecrestorelocalvectorread_ vecrestorelocalvectorread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecgetlocalvector_ VECGETLOCALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecgetlocalvector_ vecgetlocalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecrestorelocalvector_ VECRESTORELOCALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecrestorelocalvector_ vecrestorelocalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecplacearray_ VECPLACEARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecplacearray_ vecplacearray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veclockget_ VECLOCKGET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veclockget_ veclockget
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veclockpush_ VECLOCKPUSH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veclockpush_ veclockpush
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define veclockpop_ VECLOCKPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define veclockpop_ veclockpop
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  vecmaxpointwisedivide_(Vec x,Vec y,PetscReal *max, int *__ierr){
*__ierr = VecMaxPointwiseDivide(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),max);
}
PETSC_EXTERN void PETSC_STDCALL  vecdot_(Vec x,Vec y,PetscScalar *val, int *__ierr){
*__ierr = VecDot(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  vecdotrealpart_(Vec x,Vec y,PetscReal *val, int *__ierr){
*__ierr = VecDotRealPart(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  vecnorm_(Vec x,NormType *type,PetscReal *val, int *__ierr){
*__ierr = VecNorm(
	(Vec)PetscToPointer((x) ),*type,val);
}
PETSC_EXTERN void PETSC_STDCALL  vecnormavailable_(Vec x,NormType *type,PetscBool  *available,PetscReal *val, int *__ierr){
*__ierr = VecNormAvailable(
	(Vec)PetscToPointer((x) ),*type,available,val);
}
PETSC_EXTERN void PETSC_STDCALL  vecnormalize_(Vec x,PetscReal *val, int *__ierr){
*__ierr = VecNormalize(
	(Vec)PetscToPointer((x) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  vectdot_(Vec x,Vec y,PetscScalar *val, int *__ierr){
*__ierr = VecTDot(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  vecscale_(Vec x,PetscScalar *alpha, int *__ierr){
*__ierr = VecScale(
	(Vec)PetscToPointer((x) ),*alpha);
}
PETSC_EXTERN void PETSC_STDCALL  vecset_(Vec x,PetscScalar *alpha, int *__ierr){
*__ierr = VecSet(
	(Vec)PetscToPointer((x) ),*alpha);
}
PETSC_EXTERN void PETSC_STDCALL  vecaxpy_(Vec y,PetscScalar *alpha,Vec x, int *__ierr){
*__ierr = VecAXPY(
	(Vec)PetscToPointer((y) ),*alpha,
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecaxpby_(Vec y,PetscScalar *alpha,PetscScalar *beta,Vec x, int *__ierr){
*__ierr = VecAXPBY(
	(Vec)PetscToPointer((y) ),*alpha,*beta,
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecaxpbypcz_(Vec z,PetscScalar *alpha,PetscScalar *beta,PetscScalar *gamma,Vec x,Vec y, int *__ierr){
*__ierr = VecAXPBYPCZ(
	(Vec)PetscToPointer((z) ),*alpha,*beta,*gamma,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecaypx_(Vec y,PetscScalar *alpha,Vec x, int *__ierr){
*__ierr = VecAYPX(
	(Vec)PetscToPointer((y) ),*alpha,
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecwaxpy_(Vec w,PetscScalar *alpha,Vec x,Vec y, int *__ierr){
*__ierr = VecWAXPY(
	(Vec)PetscToPointer((w) ),*alpha,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecgetvalues_(Vec x,PetscInt *ni, PetscInt ix[],PetscScalar y[], int *__ierr){
*__ierr = VecGetValues(
	(Vec)PetscToPointer((x) ),*ni,ix,y);
}
PETSC_EXTERN void PETSC_STDCALL  vecsetvaluesblockedlocal_(Vec x,PetscInt *ni, PetscInt ix[], PetscScalar y[],InsertMode *iora, int *__ierr){
*__ierr = VecSetValuesBlockedLocal(
	(Vec)PetscToPointer((x) ),*ni,ix,y,*iora);
}
PETSC_EXTERN void PETSC_STDCALL  vecmtdot_(Vec x,PetscInt *nv, Vec y[],PetscScalar val[], int *__ierr){
*__ierr = VecMTDot(
	(Vec)PetscToPointer((x) ),*nv,y,val);
}
PETSC_EXTERN void PETSC_STDCALL  vecmdot_(Vec x,PetscInt *nv, Vec y[],PetscScalar val[], int *__ierr){
*__ierr = VecMDot(
	(Vec)PetscToPointer((x) ),*nv,y,val);
}
PETSC_EXTERN void PETSC_STDCALL  vecmaxpy_(Vec y,PetscInt *nv, PetscScalar alpha[],Vec x[], int *__ierr){
*__ierr = VecMAXPY(
	(Vec)PetscToPointer((y) ),*nv,alpha,x);
}
PETSC_EXTERN void PETSC_STDCALL  vecgetsubvector_(Vec X,IS is,Vec *Y, int *__ierr){
*__ierr = VecGetSubVector(
	(Vec)PetscToPointer((X) ),
	(IS)PetscToPointer((is) ),Y);
}
PETSC_EXTERN void PETSC_STDCALL  vecrestoresubvector_(Vec X,IS is,Vec *Y, int *__ierr){
*__ierr = VecRestoreSubVector(
	(Vec)PetscToPointer((X) ),
	(IS)PetscToPointer((is) ),Y);
}
PETSC_EXTERN void PETSC_STDCALL  vecgetlocalvectorread_(Vec v,Vec w, int *__ierr){
*__ierr = VecGetLocalVectorRead(
	(Vec)PetscToPointer((v) ),
	(Vec)PetscToPointer((w) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecrestorelocalvectorread_(Vec v,Vec w, int *__ierr){
*__ierr = VecRestoreLocalVectorRead(
	(Vec)PetscToPointer((v) ),
	(Vec)PetscToPointer((w) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecgetlocalvector_(Vec v,Vec w, int *__ierr){
*__ierr = VecGetLocalVector(
	(Vec)PetscToPointer((v) ),
	(Vec)PetscToPointer((w) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecrestorelocalvector_(Vec v,Vec w, int *__ierr){
*__ierr = VecRestoreLocalVector(
	(Vec)PetscToPointer((v) ),
	(Vec)PetscToPointer((w) ));
}
PETSC_EXTERN void PETSC_STDCALL  vecplacearray_(Vec vec, PetscScalar array[], int *__ierr){
*__ierr = VecPlaceArray(
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void PETSC_STDCALL  veclockget_(Vec x,PetscInt *state, int *__ierr){
*__ierr = VecLockGet(
	(Vec)PetscToPointer((x) ),state);
}
PETSC_EXTERN void PETSC_STDCALL  veclockpush_(Vec x, int *__ierr){
*__ierr = VecLockPush(
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  veclockpop_(Vec x, int *__ierr){
*__ierr = VecLockPop(
	(Vec)PetscToPointer((x) ));
}
#if defined(__cplusplus)
}
#endif

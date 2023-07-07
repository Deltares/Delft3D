#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vscatfce.c */
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
#define vecscattergetmerged_ VECSCATTERGETMERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattergetmerged_ vecscattergetmerged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscatterbegin_ VECSCATTERBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscatterbegin_ vecscatterbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscatterend_ VECSCATTEREND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscatterend_ vecscatterend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscatterdestroy_ VECSCATTERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscatterdestroy_ vecscatterdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattercopy_ VECSCATTERCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattercopy_ vecscattercopy
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  vecscattergetmerged_(VecScatter ctx,PetscBool  *flg, int *__ierr){
*__ierr = VecScatterGetMerged(
	(VecScatter)PetscToPointer((ctx) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  vecscatterbegin_(VecScatter ctx,Vec x,Vec y,InsertMode *addv,ScatterMode *mode, int *__ierr){
*__ierr = VecScatterBegin(
	(VecScatter)PetscToPointer((ctx) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),*addv,*mode);
}
PETSC_EXTERN void PETSC_STDCALL  vecscatterend_(VecScatter ctx,Vec x,Vec y,InsertMode *addv,ScatterMode *mode, int *__ierr){
*__ierr = VecScatterEnd(
	(VecScatter)PetscToPointer((ctx) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),*addv,*mode);
}
PETSC_EXTERN void PETSC_STDCALL  vecscatterdestroy_(VecScatter *ctx, int *__ierr){
*__ierr = VecScatterDestroy(ctx);
}
PETSC_EXTERN void PETSC_STDCALL  vecscattercopy_(VecScatter sctx,VecScatter *ctx, int *__ierr){
*__ierr = VecScatterCopy(
	(VecScatter)PetscToPointer((sctx) ),ctx);
}
#if defined(__cplusplus)
}
#endif

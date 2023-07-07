#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dt.c */
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

#include "petscdt.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturecreate_ PETSCQUADRATURECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturecreate_ petscquadraturecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadratureduplicate_ PETSCQUADRATUREDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadratureduplicate_ petscquadratureduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturedestroy_ PETSCQUADRATUREDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturedestroy_ petscquadraturedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturegetorder_ PETSCQUADRATUREGETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturegetorder_ petscquadraturegetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturesetorder_ PETSCQUADRATURESETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturesetorder_ petscquadraturesetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturegetnumcomponents_ PETSCQUADRATUREGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturegetnumcomponents_ petscquadraturegetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturesetnumcomponents_ PETSCQUADRATURESETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturesetnumcomponents_ petscquadraturesetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtlegendreeval_ PETSCDTLEGENDREEVAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtlegendreeval_ petscdtlegendreeval
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgaussquadrature_ PETSCDTGAUSSQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgaussquadrature_ petscdtgaussquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgausstensorquadrature_ PETSCDTGAUSSTENSORQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgausstensorquadrature_ petscdtgausstensorquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgaussjacobiquadrature_ PETSCDTGAUSSJACOBIQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgaussjacobiquadrature_ petscdtgaussjacobiquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdttanhsinhtensorquadrature_ PETSCDTTANHSINHTENSORQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdttanhsinhtensorquadrature_ petscdttanhsinhtensorquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtreconstructpoly_ PETSCDTRECONSTRUCTPOLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtreconstructpoly_ petscdtreconstructpoly
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscquadraturecreate_(MPI_Fint * comm,PetscQuadrature *q, int *__ierr){
*__ierr = PetscQuadratureCreate(
	MPI_Comm_f2c(*(comm)),q);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadratureduplicate_(PetscQuadrature q,PetscQuadrature *r, int *__ierr){
*__ierr = PetscQuadratureDuplicate(
	(PetscQuadrature)PetscToPointer((q) ),r);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadraturedestroy_(PetscQuadrature *q, int *__ierr){
*__ierr = PetscQuadratureDestroy(q);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadraturegetorder_(PetscQuadrature q,PetscInt *order, int *__ierr){
*__ierr = PetscQuadratureGetOrder(
	(PetscQuadrature)PetscToPointer((q) ),order);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadraturesetorder_(PetscQuadrature q,PetscInt *order, int *__ierr){
*__ierr = PetscQuadratureSetOrder(
	(PetscQuadrature)PetscToPointer((q) ),*order);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadraturegetnumcomponents_(PetscQuadrature q,PetscInt *Nc, int *__ierr){
*__ierr = PetscQuadratureGetNumComponents(
	(PetscQuadrature)PetscToPointer((q) ),Nc);
}
PETSC_EXTERN void PETSC_STDCALL  petscquadraturesetnumcomponents_(PetscQuadrature q,PetscInt *Nc, int *__ierr){
*__ierr = PetscQuadratureSetNumComponents(
	(PetscQuadrature)PetscToPointer((q) ),*Nc);
}
PETSC_EXTERN void PETSC_STDCALL  petscdtlegendreeval_(PetscInt *npoints, PetscReal *points,PetscInt *ndegree, PetscInt *degrees,PetscReal *B,PetscReal *D,PetscReal *D2, int *__ierr){
*__ierr = PetscDTLegendreEval(*npoints,points,*ndegree,degrees,B,D,D2);
}
PETSC_EXTERN void PETSC_STDCALL  petscdtgaussquadrature_(PetscInt *npoints,PetscReal *a,PetscReal *b,PetscReal *x,PetscReal *w, int *__ierr){
*__ierr = PetscDTGaussQuadrature(*npoints,*a,*b,x,w);
}
PETSC_EXTERN void PETSC_STDCALL  petscdtgausstensorquadrature_(PetscInt *dim,PetscInt *Nc,PetscInt *npoints,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr){
*__ierr = PetscDTGaussTensorQuadrature(*dim,*Nc,*npoints,*a,*b,q);
}
PETSC_EXTERN void PETSC_STDCALL  petscdtgaussjacobiquadrature_(PetscInt *dim,PetscInt *Nc,PetscInt *npoints,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr){
*__ierr = PetscDTGaussJacobiQuadrature(*dim,*Nc,*npoints,*a,*b,q);
}
PETSC_EXTERN void PETSC_STDCALL  petscdttanhsinhtensorquadrature_(PetscInt *dim,PetscInt *level,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr){
*__ierr = PetscDTTanhSinhTensorQuadrature(*dim,*level,*a,*b,q);
}
PETSC_EXTERN void PETSC_STDCALL  petscdtreconstructpoly_(PetscInt *degree,PetscInt *nsource, PetscReal *sourcex,PetscInt *ntarget, PetscReal *targetx,PetscReal *R, int *__ierr){
*__ierr = PetscDTReconstructPoly(*degree,*nsource,sourcex,*ntarget,targetx,R);
}
#if defined(__cplusplus)
}
#endif

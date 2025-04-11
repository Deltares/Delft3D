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
#define petscquadraturegetcelltype_ PETSCQUADRATUREGETCELLTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturegetcelltype_ petscquadraturegetcelltype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturesetcelltype_ PETSCQUADRATURESETCELLTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturesetcelltype_ petscquadraturesetcelltype
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
#define petscquadratureequal_ PETSCQUADRATUREEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadratureequal_ petscquadratureequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturepushforward_ PETSCQUADRATUREPUSHFORWARD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturepushforward_ petscquadraturepushforward
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtjacobinorm_ PETSCDTJACOBINORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtjacobinorm_ petscdtjacobinorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtjacobievaljet_ PETSCDTJACOBIEVALJET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtjacobievaljet_ petscdtjacobievaljet
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtjacobieval_ PETSCDTJACOBIEVAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtjacobieval_ petscdtjacobieval
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtlegendreeval_ PETSCDTLEGENDREEVAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtlegendreeval_ petscdtlegendreeval
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtindextogradedorder_ PETSCDTINDEXTOGRADEDORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtindextogradedorder_ petscdtindextogradedorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgradedordertoindex_ PETSCDTGRADEDORDERTOINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgradedordertoindex_ petscdtgradedordertoindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtpkdevaljet_ PETSCDTPKDEVALJET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtpkdevaljet_ petscdtpkdevaljet
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtptrimmedsize_ PETSCDTPTRIMMEDSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtptrimmedsize_ petscdtptrimmedsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtptrimmedevaljet_ PETSCDTPTRIMMEDEVALJET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtptrimmedevaljet_ petscdtptrimmedevaljet
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgaussjacobiquadrature_ PETSCDTGAUSSJACOBIQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgaussjacobiquadrature_ petscdtgaussjacobiquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtgausslobattojacobiquadrature_ PETSCDTGAUSSLOBATTOJACOBIQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtgausslobattojacobiquadrature_ petscdtgausslobattojacobiquadrature
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
#define petscdtstroudconicalquadrature_ PETSCDTSTROUDCONICALQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtstroudconicalquadrature_ petscdtstroudconicalquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtsimplexquadrature_ PETSCDTSIMPLEXQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtsimplexquadrature_ petscdtsimplexquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdttanhsinhtensorquadrature_ PETSCDTTANHSINHTENSORQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdttanhsinhtensorquadrature_ petscdttanhsinhtensorquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdttensorquadraturecreate_ PETSCDTTENSORQUADRATURECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdttensorquadraturecreate_ petscdttensorquadraturecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtreconstructpoly_ PETSCDTRECONSTRUCTPOLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtreconstructpoly_ petscdtreconstructpoly
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtindextobary_ PETSCDTINDEXTOBARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtindextobary_ petscdtindextobary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtbarytoindex_ PETSCDTBARYTOINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtbarytoindex_ petscdtbarytoindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscquadraturecomputepermutations_ PETSCQUADRATURECOMPUTEPERMUTATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscquadraturecomputepermutations_ petscquadraturecomputepermutations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtcreatedefaultquadrature_ PETSCDTCREATEDEFAULTQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtcreatedefaultquadrature_ petscdtcreatedefaultquadrature
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscquadraturecreate_(MPI_Fint * comm,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscQuadratureCreate(
	MPI_Comm_f2c(*(comm)),q);
}
PETSC_EXTERN void  petscquadratureduplicate_(PetscQuadrature q,PetscQuadrature *r, int *__ierr)
{
*__ierr = PetscQuadratureDuplicate(
	(PetscQuadrature)PetscToPointer((q) ),r);
}
PETSC_EXTERN void  petscquadraturedestroy_(PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscQuadratureDestroy(q);
}
PETSC_EXTERN void  petscquadraturegetcelltype_(PetscQuadrature q,DMPolytopeType *ct, int *__ierr)
{
*__ierr = PetscQuadratureGetCellType(
	(PetscQuadrature)PetscToPointer((q) ),ct);
}
PETSC_EXTERN void  petscquadraturesetcelltype_(PetscQuadrature q,DMPolytopeType *ct, int *__ierr)
{
*__ierr = PetscQuadratureSetCellType(
	(PetscQuadrature)PetscToPointer((q) ),*ct);
}
PETSC_EXTERN void  petscquadraturegetorder_(PetscQuadrature q,PetscInt *order, int *__ierr)
{
*__ierr = PetscQuadratureGetOrder(
	(PetscQuadrature)PetscToPointer((q) ),order);
}
PETSC_EXTERN void  petscquadraturesetorder_(PetscQuadrature q,PetscInt *order, int *__ierr)
{
*__ierr = PetscQuadratureSetOrder(
	(PetscQuadrature)PetscToPointer((q) ),*order);
}
PETSC_EXTERN void  petscquadraturegetnumcomponents_(PetscQuadrature q,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscQuadratureGetNumComponents(
	(PetscQuadrature)PetscToPointer((q) ),Nc);
}
PETSC_EXTERN void  petscquadraturesetnumcomponents_(PetscQuadrature q,PetscInt *Nc, int *__ierr)
{
*__ierr = PetscQuadratureSetNumComponents(
	(PetscQuadrature)PetscToPointer((q) ),*Nc);
}
PETSC_EXTERN void  petscquadratureequal_(PetscQuadrature A,PetscQuadrature B,PetscBool *equal, int *__ierr)
{
*__ierr = PetscQuadratureEqual(
	(PetscQuadrature)PetscToPointer((A) ),
	(PetscQuadrature)PetscToPointer((B) ),equal);
}
PETSC_EXTERN void  petscquadraturepushforward_(PetscQuadrature q,PetscInt *imageDim, PetscReal origin[], PetscReal originImage[], PetscReal J[],PetscInt *formDegree,PetscQuadrature *Jinvstarq, int *__ierr)
{
*__ierr = PetscQuadraturePushForward(
	(PetscQuadrature)PetscToPointer((q) ),*imageDim,origin,originImage,J,*formDegree,Jinvstarq);
}
PETSC_EXTERN void  petscdtjacobinorm_(PetscReal *alpha,PetscReal *beta,PetscInt *n,PetscReal *norm, int *__ierr)
{
*__ierr = PetscDTJacobiNorm(*alpha,*beta,*n,norm);
}
PETSC_EXTERN void  petscdtjacobievaljet_(PetscReal *alpha,PetscReal *beta,PetscInt *npoints, PetscReal points[],PetscInt *degree,PetscInt *k,PetscReal p[], int *__ierr)
{
*__ierr = PetscDTJacobiEvalJet(*alpha,*beta,*npoints,points,*degree,*k,p);
}
PETSC_EXTERN void  petscdtjacobieval_(PetscInt *npoints,PetscReal *alpha,PetscReal *beta, PetscReal *points,PetscInt *ndegree, PetscInt *degrees,PetscReal *B,PetscReal *D,PetscReal *D2, int *__ierr)
{
*__ierr = PetscDTJacobiEval(*npoints,*alpha,*beta,points,*ndegree,degrees,B,D,D2);
}
PETSC_EXTERN void  petscdtlegendreeval_(PetscInt *npoints, PetscReal *points,PetscInt *ndegree, PetscInt *degrees,PetscReal *B,PetscReal *D,PetscReal *D2, int *__ierr)
{
*__ierr = PetscDTLegendreEval(*npoints,points,*ndegree,degrees,B,D,D2);
}
PETSC_EXTERN void  petscdtindextogradedorder_(PetscInt *len,PetscInt *index,PetscInt degtup[], int *__ierr)
{
*__ierr = PetscDTIndexToGradedOrder(*len,*index,degtup);
}
PETSC_EXTERN void  petscdtgradedordertoindex_(PetscInt *len, PetscInt degtup[],PetscInt *index, int *__ierr)
{
*__ierr = PetscDTGradedOrderToIndex(*len,degtup,index);
}
PETSC_EXTERN void  petscdtpkdevaljet_(PetscInt *dim,PetscInt *npoints, PetscReal points[],PetscInt *degree,PetscInt *k,PetscReal p[], int *__ierr)
{
*__ierr = PetscDTPKDEvalJet(*dim,*npoints,points,*degree,*k,p);
}
PETSC_EXTERN void  petscdtptrimmedsize_(PetscInt *dim,PetscInt *degree,PetscInt *formDegree,PetscInt *size, int *__ierr)
{
*__ierr = PetscDTPTrimmedSize(*dim,*degree,*formDegree,size);
}
PETSC_EXTERN void  petscdtptrimmedevaljet_(PetscInt *dim,PetscInt *npoints, PetscReal points[],PetscInt *degree,PetscInt *formDegree,PetscInt *jetDegree,PetscReal p[], int *__ierr)
{
*__ierr = PetscDTPTrimmedEvalJet(*dim,*npoints,points,*degree,*formDegree,*jetDegree,p);
}
PETSC_EXTERN void  petscdtgaussjacobiquadrature_(PetscInt *npoints,PetscReal *a,PetscReal *b,PetscReal *alpha,PetscReal *beta,PetscReal x[],PetscReal w[], int *__ierr)
{
*__ierr = PetscDTGaussJacobiQuadrature(*npoints,*a,*b,*alpha,*beta,x,w);
}
PETSC_EXTERN void  petscdtgausslobattojacobiquadrature_(PetscInt *npoints,PetscReal *a,PetscReal *b,PetscReal *alpha,PetscReal *beta,PetscReal x[],PetscReal w[], int *__ierr)
{
*__ierr = PetscDTGaussLobattoJacobiQuadrature(*npoints,*a,*b,*alpha,*beta,x,w);
}
PETSC_EXTERN void  petscdtgaussquadrature_(PetscInt *npoints,PetscReal *a,PetscReal *b,PetscReal *x,PetscReal *w, int *__ierr)
{
*__ierr = PetscDTGaussQuadrature(*npoints,*a,*b,x,w);
}
PETSC_EXTERN void  petscdtgausstensorquadrature_(PetscInt *dim,PetscInt *Nc,PetscInt *npoints,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscDTGaussTensorQuadrature(*dim,*Nc,*npoints,*a,*b,q);
}
PETSC_EXTERN void  petscdtstroudconicalquadrature_(PetscInt *dim,PetscInt *Nc,PetscInt *npoints,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscDTStroudConicalQuadrature(*dim,*Nc,*npoints,*a,*b,q);
}
PETSC_EXTERN void  petscdtsimplexquadrature_(PetscInt *dim,PetscInt *degree,PetscDTSimplexQuadratureType *type,PetscQuadrature *quad, int *__ierr)
{
*__ierr = PetscDTSimplexQuadrature(*dim,*degree,*type,quad);
}
PETSC_EXTERN void  petscdttanhsinhtensorquadrature_(PetscInt *dim,PetscInt *level,PetscReal *a,PetscReal *b,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscDTTanhSinhTensorQuadrature(*dim,*level,*a,*b,q);
}
PETSC_EXTERN void  petscdttensorquadraturecreate_(PetscQuadrature q1,PetscQuadrature q2,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscDTTensorQuadratureCreate(
	(PetscQuadrature)PetscToPointer((q1) ),
	(PetscQuadrature)PetscToPointer((q2) ),q);
}
PETSC_EXTERN void  petscdtreconstructpoly_(PetscInt *degree,PetscInt *nsource, PetscReal *sourcex,PetscInt *ntarget, PetscReal *targetx,PetscReal *R, int *__ierr)
{
*__ierr = PetscDTReconstructPoly(*degree,*nsource,sourcex,*ntarget,targetx,R);
}
PETSC_EXTERN void  petscdtindextobary_(PetscInt *len,PetscInt *sum,PetscInt *index,PetscInt coord[], int *__ierr)
{
*__ierr = PetscDTIndexToBary(*len,*sum,*index,coord);
}
PETSC_EXTERN void  petscdtbarytoindex_(PetscInt *len,PetscInt *sum, PetscInt coord[],PetscInt *index, int *__ierr)
{
*__ierr = PetscDTBaryToIndex(*len,*sum,coord,index);
}
PETSC_EXTERN void  petscquadraturecomputepermutations_(PetscQuadrature quad,PetscInt *Np,IS *perm[], int *__ierr)
{
*__ierr = PetscQuadratureComputePermutations(
	(PetscQuadrature)PetscToPointer((quad) ),Np,perm);
}
PETSC_EXTERN void  petscdtcreatedefaultquadrature_(DMPolytopeType *ct,PetscInt *qorder,PetscQuadrature *q,PetscQuadrature *fq, int *__ierr)
{
*__ierr = PetscDTCreateDefaultQuadrature(*ct,*qorder,q,fq);
}
#if defined(__cplusplus)
}
#endif

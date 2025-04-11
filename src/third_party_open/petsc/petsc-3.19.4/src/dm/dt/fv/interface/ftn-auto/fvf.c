#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fv.c */
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

#include "petscfv.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclimitersetfromoptions_ PETSCLIMITERSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclimitersetfromoptions_ petsclimitersetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclimiterdestroy_ PETSCLIMITERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclimiterdestroy_ petsclimiterdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclimitercreate_ PETSCLIMITERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclimitercreate_ petsclimitercreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclimiterlimit_ PETSCLIMITERLIMIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclimiterlimit_ petsclimiterlimit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetfromoptions_ PETSCFVSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetfromoptions_ petscfvsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetup_ PETSCFVSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetup_ petscfvsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvdestroy_ PETSCFVDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvdestroy_ petscfvdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvcreate_ PETSCFVCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvcreate_ petscfvcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetlimiter_ PETSCFVSETLIMITER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetlimiter_ petscfvsetlimiter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetlimiter_ PETSCFVGETLIMITER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetlimiter_ petscfvgetlimiter
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetnumcomponents_ PETSCFVSETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetnumcomponents_ petscfvsetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetnumcomponents_ PETSCFVGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetnumcomponents_ petscfvgetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetspatialdimension_ PETSCFVSETSPATIALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetspatialdimension_ petscfvsetspatialdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetspatialdimension_ PETSCFVGETSPATIALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetspatialdimension_ petscfvgetspatialdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetcomputegradients_ PETSCFVSETCOMPUTEGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetcomputegradients_ petscfvsetcomputegradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetcomputegradients_ PETSCFVGETCOMPUTEGRADIENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetcomputegradients_ petscfvgetcomputegradients
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetquadrature_ PETSCFVSETQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetquadrature_ petscfvsetquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetquadrature_ PETSCFVGETQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetquadrature_ petscfvgetquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvgetdualspace_ PETSCFVGETDUALSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvgetdualspace_ petscfvgetdualspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvsetdualspace_ PETSCFVSETDUALSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvsetdualspace_ petscfvsetdualspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvrefine_ PETSCFVREFINE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvrefine_ petscfvrefine
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfvleastsquaressetmaxfaces_ PETSCFVLEASTSQUARESSETMAXFACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfvleastsquaressetmaxfaces_ petscfvleastsquaressetmaxfaces
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petsclimitersetfromoptions_(PetscLimiter lim, int *__ierr)
{
*__ierr = PetscLimiterSetFromOptions(
	(PetscLimiter)PetscToPointer((lim) ));
}
PETSC_EXTERN void  petsclimiterdestroy_(PetscLimiter *lim, int *__ierr)
{
*__ierr = PetscLimiterDestroy(lim);
}
PETSC_EXTERN void  petsclimitercreate_(MPI_Fint * comm,PetscLimiter *lim, int *__ierr)
{
*__ierr = PetscLimiterCreate(
	MPI_Comm_f2c(*(comm)),lim);
}
PETSC_EXTERN void  petsclimiterlimit_(PetscLimiter lim,PetscReal *flim,PetscReal *phi, int *__ierr)
{
*__ierr = PetscLimiterLimit(
	(PetscLimiter)PetscToPointer((lim) ),*flim,phi);
}
PETSC_EXTERN void  petscfvsetfromoptions_(PetscFV fvm, int *__ierr)
{
*__ierr = PetscFVSetFromOptions(
	(PetscFV)PetscToPointer((fvm) ));
}
PETSC_EXTERN void  petscfvsetup_(PetscFV fvm, int *__ierr)
{
*__ierr = PetscFVSetUp(
	(PetscFV)PetscToPointer((fvm) ));
}
PETSC_EXTERN void  petscfvdestroy_(PetscFV *fvm, int *__ierr)
{
*__ierr = PetscFVDestroy(fvm);
}
PETSC_EXTERN void  petscfvcreate_(MPI_Fint * comm,PetscFV *fvm, int *__ierr)
{
*__ierr = PetscFVCreate(
	MPI_Comm_f2c(*(comm)),fvm);
}
PETSC_EXTERN void  petscfvsetlimiter_(PetscFV fvm,PetscLimiter lim, int *__ierr)
{
*__ierr = PetscFVSetLimiter(
	(PetscFV)PetscToPointer((fvm) ),
	(PetscLimiter)PetscToPointer((lim) ));
}
PETSC_EXTERN void  petscfvgetlimiter_(PetscFV fvm,PetscLimiter *lim, int *__ierr)
{
*__ierr = PetscFVGetLimiter(
	(PetscFV)PetscToPointer((fvm) ),lim);
}
PETSC_EXTERN void  petscfvsetnumcomponents_(PetscFV fvm,PetscInt *comp, int *__ierr)
{
*__ierr = PetscFVSetNumComponents(
	(PetscFV)PetscToPointer((fvm) ),*comp);
}
PETSC_EXTERN void  petscfvgetnumcomponents_(PetscFV fvm,PetscInt *comp, int *__ierr)
{
*__ierr = PetscFVGetNumComponents(
	(PetscFV)PetscToPointer((fvm) ),comp);
}
PETSC_EXTERN void  petscfvsetspatialdimension_(PetscFV fvm,PetscInt *dim, int *__ierr)
{
*__ierr = PetscFVSetSpatialDimension(
	(PetscFV)PetscToPointer((fvm) ),*dim);
}
PETSC_EXTERN void  petscfvgetspatialdimension_(PetscFV fvm,PetscInt *dim, int *__ierr)
{
*__ierr = PetscFVGetSpatialDimension(
	(PetscFV)PetscToPointer((fvm) ),dim);
}
PETSC_EXTERN void  petscfvsetcomputegradients_(PetscFV fvm,PetscBool *computeGradients, int *__ierr)
{
*__ierr = PetscFVSetComputeGradients(
	(PetscFV)PetscToPointer((fvm) ),*computeGradients);
}
PETSC_EXTERN void  petscfvgetcomputegradients_(PetscFV fvm,PetscBool *computeGradients, int *__ierr)
{
*__ierr = PetscFVGetComputeGradients(
	(PetscFV)PetscToPointer((fvm) ),computeGradients);
}
PETSC_EXTERN void  petscfvsetquadrature_(PetscFV fvm,PetscQuadrature q, int *__ierr)
{
*__ierr = PetscFVSetQuadrature(
	(PetscFV)PetscToPointer((fvm) ),
	(PetscQuadrature)PetscToPointer((q) ));
}
PETSC_EXTERN void  petscfvgetquadrature_(PetscFV fvm,PetscQuadrature *q, int *__ierr)
{
*__ierr = PetscFVGetQuadrature(
	(PetscFV)PetscToPointer((fvm) ),q);
}
PETSC_EXTERN void  petscfvgetdualspace_(PetscFV fvm,PetscDualSpace *sp, int *__ierr)
{
*__ierr = PetscFVGetDualSpace(
	(PetscFV)PetscToPointer((fvm) ),sp);
}
PETSC_EXTERN void  petscfvsetdualspace_(PetscFV fvm,PetscDualSpace sp, int *__ierr)
{
*__ierr = PetscFVSetDualSpace(
	(PetscFV)PetscToPointer((fvm) ),
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void  petscfvrefine_(PetscFV fv,PetscFV *fvRef, int *__ierr)
{
*__ierr = PetscFVRefine(
	(PetscFV)PetscToPointer((fv) ),fvRef);
}
PETSC_EXTERN void  petscfvleastsquaressetmaxfaces_(PetscFV fvm,PetscInt *maxFaces, int *__ierr)
{
*__ierr = PetscFVLeastSquaresSetMaxFaces(
	(PetscFV)PetscToPointer((fvm) ),*maxFaces);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* comb.c */
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
#define petsccommsplitreductionbegin_ PETSCCOMMSPLITREDUCTIONBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsccommsplitreductionbegin_ petsccommsplitreductionbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecdotbegin_ VECDOTBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecdotbegin_ vecdotbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecdotend_ VECDOTEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecdotend_ vecdotend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vectdotbegin_ VECTDOTBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vectdotbegin_ vectdotbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vectdotend_ VECTDOTEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vectdotend_ vectdotend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecnormbegin_ VECNORMBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecnormbegin_ vecnormbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecnormend_ VECNORMEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecnormend_ vecnormend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmdotbegin_ VECMDOTBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmdotbegin_ vecmdotbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmdotend_ VECMDOTEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmdotend_ vecmdotend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmtdotbegin_ VECMTDOTBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmtdotbegin_ vecmtdotbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecmtdotend_ VECMTDOTEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecmtdotend_ vecmtdotend
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petsccommsplitreductionbegin_(MPI_Fint * comm, int *__ierr)
{
*__ierr = PetscCommSplitReductionBegin(
	MPI_Comm_f2c(*(comm)));
}
PETSC_EXTERN void  vecdotbegin_(Vec x,Vec y,PetscScalar *result, int *__ierr)
{
*__ierr = VecDotBegin(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),result);
}
PETSC_EXTERN void  vecdotend_(Vec x,Vec y,PetscScalar *result, int *__ierr)
{
*__ierr = VecDotEnd(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),result);
}
PETSC_EXTERN void  vectdotbegin_(Vec x,Vec y,PetscScalar *result, int *__ierr)
{
*__ierr = VecTDotBegin(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),result);
}
PETSC_EXTERN void  vectdotend_(Vec x,Vec y,PetscScalar *result, int *__ierr)
{
*__ierr = VecTDotEnd(
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),result);
}
PETSC_EXTERN void  vecnormbegin_(Vec x,NormType *ntype,PetscReal *result, int *__ierr)
{
*__ierr = VecNormBegin(
	(Vec)PetscToPointer((x) ),*ntype,result);
}
PETSC_EXTERN void  vecnormend_(Vec x,NormType *ntype,PetscReal *result, int *__ierr)
{
*__ierr = VecNormEnd(
	(Vec)PetscToPointer((x) ),*ntype,result);
}
PETSC_EXTERN void  vecmdotbegin_(Vec x,PetscInt *nv, Vec y[],PetscScalar result[], int *__ierr)
{
*__ierr = VecMDotBegin(
	(Vec)PetscToPointer((x) ),*nv,y,result);
}
PETSC_EXTERN void  vecmdotend_(Vec x,PetscInt *nv, Vec y[],PetscScalar result[], int *__ierr)
{
*__ierr = VecMDotEnd(
	(Vec)PetscToPointer((x) ),*nv,y,result);
}
PETSC_EXTERN void  vecmtdotbegin_(Vec x,PetscInt *nv, Vec y[],PetscScalar result[], int *__ierr)
{
*__ierr = VecMTDotBegin(
	(Vec)PetscToPointer((x) ),*nv,y,result);
}
PETSC_EXTERN void  vecmtdotend_(Vec x,PetscInt *nv, Vec y[],PetscScalar result[], int *__ierr)
{
*__ierr = VecMTDotEnd(
	(Vec)PetscToPointer((x) ),*nv,y,result);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dtaltv.c */
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
#define petscdtaltvapply_ PETSCDTALTVAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvapply_ petscdtaltvapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvwedge_ PETSCDTALTVWEDGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvwedge_ petscdtaltvwedge
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvwedgematrix_ PETSCDTALTVWEDGEMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvwedgematrix_ petscdtaltvwedgematrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvpullback_ PETSCDTALTVPULLBACK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvpullback_ petscdtaltvpullback
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvpullbackmatrix_ PETSCDTALTVPULLBACKMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvpullbackmatrix_ petscdtaltvpullbackmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvinterior_ PETSCDTALTVINTERIOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvinterior_ petscdtaltvinterior
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvinteriormatrix_ PETSCDTALTVINTERIORMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvinteriormatrix_ petscdtaltvinteriormatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvinteriorpattern_ PETSCDTALTVINTERIORPATTERN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvinteriorpattern_ petscdtaltvinteriorpattern
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdtaltvstar_ PETSCDTALTVSTAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdtaltvstar_ petscdtaltvstar
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdtaltvapply_(PetscInt *N,PetscInt *k, PetscReal *w, PetscReal *v,PetscReal *wv, int *__ierr)
{
*__ierr = PetscDTAltVApply(*N,*k,w,v,wv);
}
PETSC_EXTERN void  petscdtaltvwedge_(PetscInt *N,PetscInt *j,PetscInt *k, PetscReal *a, PetscReal *b,PetscReal *awedgeb, int *__ierr)
{
*__ierr = PetscDTAltVWedge(*N,*j,*k,a,b,awedgeb);
}
PETSC_EXTERN void  petscdtaltvwedgematrix_(PetscInt *N,PetscInt *j,PetscInt *k, PetscReal *a,PetscReal *awedgeMat, int *__ierr)
{
*__ierr = PetscDTAltVWedgeMatrix(*N,*j,*k,a,awedgeMat);
}
PETSC_EXTERN void  petscdtaltvpullback_(PetscInt *N,PetscInt *M, PetscReal *L,PetscInt *k, PetscReal *w,PetscReal *Lstarw, int *__ierr)
{
*__ierr = PetscDTAltVPullback(*N,*M,L,*k,w,Lstarw);
}
PETSC_EXTERN void  petscdtaltvpullbackmatrix_(PetscInt *N,PetscInt *M, PetscReal *L,PetscInt *k,PetscReal *Lstar, int *__ierr)
{
*__ierr = PetscDTAltVPullbackMatrix(*N,*M,L,*k,Lstar);
}
PETSC_EXTERN void  petscdtaltvinterior_(PetscInt *N,PetscInt *k, PetscReal *w, PetscReal *v,PetscReal *wIntv, int *__ierr)
{
*__ierr = PetscDTAltVInterior(*N,*k,w,v,wIntv);
}
PETSC_EXTERN void  petscdtaltvinteriormatrix_(PetscInt *N,PetscInt *k, PetscReal *v,PetscReal *intvMat, int *__ierr)
{
*__ierr = PetscDTAltVInteriorMatrix(*N,*k,v,intvMat);
}
PETSC_EXTERN void  petscdtaltvinteriorpattern_(PetscInt *N,PetscInt *k,PetscInt (*indices)[3], int *__ierr)
{
*__ierr = PetscDTAltVInteriorPattern(*N,*k,indices);
}
PETSC_EXTERN void  petscdtaltvstar_(PetscInt *N,PetscInt *k,PetscInt *pow, PetscReal *w,PetscReal *starw, int *__ierr)
{
*__ierr = PetscDTAltVStar(*N,*k,*pow,w,starw);
}
#if defined(__cplusplus)
}
#endif

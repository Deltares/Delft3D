#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* vscat.c */
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

#include "petscsf.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattersetup_ VECSCATTERSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattersetup_ vecscattersetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattergetmerged_ VECSCATTERGETMERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattergetmerged_ vecscattergetmerged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattercopy_ VECSCATTERCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattercopy_ vecscattercopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattersetfromoptions_ VECSCATTERSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattersetfromoptions_ vecscattersetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecscattercreate_ VECSCATTERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecscattercreate_ vecscattercreate
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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  vecscattersetup_(VecScatter sf, int *__ierr)
{
*__ierr = VecScatterSetUp(
	(VecScatter)PetscToPointer((sf) ));
}
PETSC_EXTERN void  vecscattergetmerged_(VecScatter sf,PetscBool *flg, int *__ierr)
{
*__ierr = VecScatterGetMerged(
	(VecScatter)PetscToPointer((sf) ),flg);
}
PETSC_EXTERN void  vecscattercopy_(VecScatter sf,VecScatter *newsf, int *__ierr)
{
*__ierr = VecScatterCopy(
	(VecScatter)PetscToPointer((sf) ),newsf);
}
PETSC_EXTERN void  vecscattersetfromoptions_(VecScatter sf, int *__ierr)
{
*__ierr = VecScatterSetFromOptions(
	(VecScatter)PetscToPointer((sf) ));
}
PETSC_EXTERN void  vecscattercreate_(Vec x,IS ix,Vec y,IS iy,VecScatter *newsf, int *__ierr)
{
*__ierr = VecScatterCreate(
	(Vec)PetscToPointer((x) ),
	(IS)PetscToPointer((ix) ),
	(Vec)PetscToPointer((y) ),
	(IS)PetscToPointer((iy) ),newsf);
}
PETSC_EXTERN void  vecscatterbegin_(VecScatter sf,Vec x,Vec y,InsertMode *addv,ScatterMode *mode, int *__ierr)
{
*__ierr = VecScatterBegin(
	(VecScatter)PetscToPointer((sf) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),*addv,*mode);
}
PETSC_EXTERN void  vecscatterend_(VecScatter sf,Vec x,Vec y,InsertMode *addv,ScatterMode *mode, int *__ierr)
{
*__ierr = VecScatterEnd(
	(VecScatter)PetscToPointer((sf) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),*addv,*mode);
}
#if defined(__cplusplus)
}
#endif

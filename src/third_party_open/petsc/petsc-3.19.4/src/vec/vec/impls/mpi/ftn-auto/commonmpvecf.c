#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* commonmpvec.c */
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
#define vecghostgetlocalform_ VECGHOSTGETLOCALFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecghostgetlocalform_ vecghostgetlocalform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecghostislocalform_ VECGHOSTISLOCALFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecghostislocalform_ vecghostislocalform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecghostrestorelocalform_ VECGHOSTRESTORELOCALFORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecghostrestorelocalform_ vecghostrestorelocalform
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecghostupdatebegin_ VECGHOSTUPDATEBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecghostupdatebegin_ vecghostupdatebegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecghostupdateend_ VECGHOSTUPDATEEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecghostupdateend_ vecghostupdateend
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  vecghostgetlocalform_(Vec g,Vec *l, int *__ierr)
{
*__ierr = VecGhostGetLocalForm(
	(Vec)PetscToPointer((g) ),l);
}
PETSC_EXTERN void  vecghostislocalform_(Vec g,Vec l,PetscBool *flg, int *__ierr)
{
*__ierr = VecGhostIsLocalForm(
	(Vec)PetscToPointer((g) ),
	(Vec)PetscToPointer((l) ),flg);
}
PETSC_EXTERN void  vecghostrestorelocalform_(Vec g,Vec *l, int *__ierr)
{
*__ierr = VecGhostRestoreLocalForm(
	(Vec)PetscToPointer((g) ),l);
}
PETSC_EXTERN void  vecghostupdatebegin_(Vec g,InsertMode *insertmode,ScatterMode *scattermode, int *__ierr)
{
*__ierr = VecGhostUpdateBegin(
	(Vec)PetscToPointer((g) ),*insertmode,*scattermode);
}
PETSC_EXTERN void  vecghostupdateend_(Vec g,InsertMode *insertmode,ScatterMode *scattermode, int *__ierr)
{
*__ierr = VecGhostUpdateEnd(
	(Vec)PetscToPointer((g) ),*insertmode,*scattermode);
}
#if defined(__cplusplus)
}
#endif

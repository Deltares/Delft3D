#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexreorder.c */
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

#include "petscdmplex.h"
#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetordering_ DMPLEXGETORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetordering_ dmplexgetordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetordering1d_ DMPLEXGETORDERING1D
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetordering1d_ dmplexgetordering1d
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpermute_ DMPLEXPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpermute_ dmplexpermute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexreordersetdefault_ DMPLEXREORDERSETDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreordersetdefault_ dmplexreordersetdefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexreordergetdefault_ DMPLEXREORDERGETDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreordergetdefault_ dmplexreordergetdefault
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexgetordering_(DM dm,MatOrderingType *otype,DMLabel label,IS *perm, int *__ierr)
{
*__ierr = DMPlexGetOrdering(
	(DM)PetscToPointer((dm) ),*otype,
	(DMLabel)PetscToPointer((label) ),perm);
}
PETSC_EXTERN void  dmplexgetordering1d_(DM dm,IS *perm, int *__ierr)
{
*__ierr = DMPlexGetOrdering1D(
	(DM)PetscToPointer((dm) ),perm);
}
PETSC_EXTERN void  dmplexpermute_(DM dm,IS perm,DM *pdm, int *__ierr)
{
*__ierr = DMPlexPermute(
	(DM)PetscToPointer((dm) ),
	(IS)PetscToPointer((perm) ),pdm);
}
PETSC_EXTERN void  dmplexreordersetdefault_(DM dm,DMPlexReorderDefaultFlag *reorder, int *__ierr)
{
*__ierr = DMPlexReorderSetDefault(
	(DM)PetscToPointer((dm) ),*reorder);
}
PETSC_EXTERN void  dmplexreordergetdefault_(DM dm,DMPlexReorderDefaultFlag *reorder, int *__ierr)
{
*__ierr = DMPlexReorderGetDefault(
	(DM)PetscToPointer((dm) ),reorder);
}
#if defined(__cplusplus)
}
#endif

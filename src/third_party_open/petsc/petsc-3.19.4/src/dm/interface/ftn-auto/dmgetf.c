#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmget.c */
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

#include "petscdm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetlocalvector_ DMGETLOCALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetlocalvector_ dmgetlocalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmrestorelocalvector_ DMRESTORELOCALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmrestorelocalvector_ dmrestorelocalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetglobalvector_ DMGETGLOBALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetglobalvector_ dmgetglobalvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmclearglobalvectors_ DMCLEARGLOBALVECTORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmclearglobalvectors_ dmclearglobalvectors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmclearlocalvectors_ DMCLEARLOCALVECTORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmclearlocalvectors_ dmclearlocalvectors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmrestoreglobalvector_ DMRESTOREGLOBALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmrestoreglobalvector_ dmrestoreglobalvector
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmgetlocalvector_(DM dm,Vec *g, int *__ierr)
{
*__ierr = DMGetLocalVector(
	(DM)PetscToPointer((dm) ),g);
}
PETSC_EXTERN void  dmrestorelocalvector_(DM dm,Vec *g, int *__ierr)
{
*__ierr = DMRestoreLocalVector(
	(DM)PetscToPointer((dm) ),g);
}
PETSC_EXTERN void  dmgetglobalvector_(DM dm,Vec *g, int *__ierr)
{
*__ierr = DMGetGlobalVector(
	(DM)PetscToPointer((dm) ),g);
}
PETSC_EXTERN void  dmclearglobalvectors_(DM dm, int *__ierr)
{
*__ierr = DMClearGlobalVectors(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmclearlocalvectors_(DM dm, int *__ierr)
{
*__ierr = DMClearLocalVectors(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmrestoreglobalvector_(DM dm,Vec *g, int *__ierr)
{
*__ierr = DMRestoreGlobalVector(
	(DM)PetscToPointer((dm) ),g);
}
#if defined(__cplusplus)
}
#endif

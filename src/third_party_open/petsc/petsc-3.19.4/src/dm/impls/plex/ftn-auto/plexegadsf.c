#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexegads.c */
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnaptogeommodel_ DMPLEXSNAPTOGEOMMODEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnaptogeommodel_ dmplexsnaptogeommodel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexinflatetogeommodel_ DMPLEXINFLATETOGEOMMODEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexinflatetogeommodel_ dmplexinflatetogeommodel
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsnaptogeommodel_(DM dm,PetscInt *p,PetscInt *dE, PetscScalar mcoords[],PetscScalar gcoords[], int *__ierr)
{
*__ierr = DMPlexSnapToGeomModel(
	(DM)PetscToPointer((dm) ),*p,*dE,mcoords,gcoords);
}
PETSC_EXTERN void  dmplexinflatetogeommodel_(DM dm, int *__ierr)
{
*__ierr = DMPlexInflateToGeomModel(
	(DM)PetscToPointer((dm) ));
}
#if defined(__cplusplus)
}
#endif

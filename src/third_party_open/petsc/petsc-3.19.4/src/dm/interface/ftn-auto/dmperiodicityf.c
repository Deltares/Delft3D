#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmperiodicity.c */
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
#define dmlocalizecoordinate_ DMLOCALIZECOORDINATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocalizecoordinate_ dmlocalizecoordinate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocalizedlocal_ DMGETCOORDINATESLOCALIZEDLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocalizedlocal_ dmgetcoordinateslocalizedlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmgetcoordinateslocalized_ DMGETCOORDINATESLOCALIZED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmgetcoordinateslocalized_ dmgetcoordinateslocalized
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlocalizecoordinates_ DMLOCALIZECOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlocalizecoordinates_ dmlocalizecoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmlocalizecoordinate_(DM dm, PetscScalar in[],PetscBool *endpoint,PetscScalar out[], int *__ierr)
{
*__ierr = DMLocalizeCoordinate(
	(DM)PetscToPointer((dm) ),in,*endpoint,out);
}
PETSC_EXTERN void  dmgetcoordinateslocalizedlocal_(DM dm,PetscBool *areLocalized, int *__ierr)
{
*__ierr = DMGetCoordinatesLocalizedLocal(
	(DM)PetscToPointer((dm) ),areLocalized);
}
PETSC_EXTERN void  dmgetcoordinateslocalized_(DM dm,PetscBool *areLocalized, int *__ierr)
{
*__ierr = DMGetCoordinatesLocalized(
	(DM)PetscToPointer((dm) ),areLocalized);
}
PETSC_EXTERN void  dmlocalizecoordinates_(DM dm, int *__ierr)
{
*__ierr = DMLocalizeCoordinates(
	(DM)PetscToPointer((dm) ));
}
#if defined(__cplusplus)
}
#endif

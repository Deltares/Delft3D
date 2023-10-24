#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexsfc.c */
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
#define dmplexsetisoperiodicfacesf_ DMPLEXSETISOPERIODICFACESF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetisoperiodicfacesf_ dmplexsetisoperiodicfacesf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetisoperiodicfacesf_ DMPLEXGETISOPERIODICFACESF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetisoperiodicfacesf_ dmplexgetisoperiodicfacesf
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsetisoperiodicfacesf_(DM dm,PetscSF face_sf, int *__ierr)
{
*__ierr = DMPlexSetIsoperiodicFaceSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((face_sf) ));
}
PETSC_EXTERN void  dmplexgetisoperiodicfacesf_(DM dm,PetscSF *face_sf, int *__ierr)
{
*__ierr = DMPlexGetIsoperiodicFaceSF(
	(DM)PetscToPointer((dm) ),face_sf);
}
#if defined(__cplusplus)
}
#endif

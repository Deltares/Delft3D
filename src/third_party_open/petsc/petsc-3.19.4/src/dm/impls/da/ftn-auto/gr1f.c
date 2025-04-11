#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gr1.c */
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

#include "petscdmda.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetuniformcoordinates_ DMDASETUNIFORMCOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetuniformcoordinates_ dmdasetuniformcoordinates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdasetuniformcoordinates_(DM da,PetscReal *xmin,PetscReal *xmax,PetscReal *ymin,PetscReal *ymax,PetscReal *zmin,PetscReal *zmax, int *__ierr)
{
*__ierr = DMDASetUniformCoordinates(
	(DM)PetscToPointer((da) ),*xmin,*xmax,*ymin,*ymax,*zmin,*zmax);
}
#if defined(__cplusplus)
}
#endif

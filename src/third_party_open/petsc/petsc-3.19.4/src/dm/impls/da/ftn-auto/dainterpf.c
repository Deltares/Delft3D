#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dainterp.c */
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
#define dmcreateaggregates_ DMCREATEAGGREGATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcreateaggregates_ dmcreateaggregates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdacreateaggregates_ DMDACREATEAGGREGATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdacreateaggregates_ dmdacreateaggregates
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmcreateaggregates_(DM dac,DM daf,Mat *mat, int *__ierr)
{
*__ierr = DMCreateAggregates(
	(DM)PetscToPointer((dac) ),
	(DM)PetscToPointer((daf) ),mat);
}
PETSC_EXTERN void  dmdacreateaggregates_(DM dac,DM daf,Mat *rest, int *__ierr)
{
*__ierr = DMDACreateAggregates(
	(DM)PetscToPointer((dac) ),
	(DM)PetscToPointer((daf) ),rest);
}
#if defined(__cplusplus)
}
#endif

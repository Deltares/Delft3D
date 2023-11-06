#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmproject.c */
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
#include "petscdmplex.h"
#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmglobaltolocalsolve_ DMGLOBALTOLOCALSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmglobaltolocalsolve_ dmglobaltolocalsolve
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmglobaltolocalsolve_(DM dm,Vec x,Vec y, int *__ierr)
{
*__ierr = DMGlobalToLocalSolve(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
#if defined(__cplusplus)
}
#endif

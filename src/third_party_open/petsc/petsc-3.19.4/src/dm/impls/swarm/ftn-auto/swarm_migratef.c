#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* swarm_migrate.c */
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

#include "petscdmswarm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmgetmigratetype_ DMSWARMGETMIGRATETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmgetmigratetype_ dmswarmgetmigratetype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsetmigratetype_ DMSWARMSETMIGRATETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsetmigratetype_ dmswarmsetmigratetype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmswarmgetmigratetype_(DM dm,DMSwarmMigrateType *mtype, int *__ierr)
{
*__ierr = DMSwarmGetMigrateType(
	(DM)PetscToPointer((dm) ),mtype);
}
PETSC_EXTERN void  dmswarmsetmigratetype_(DM dm,DMSwarmMigrateType *mtype, int *__ierr)
{
*__ierr = DMSwarmSetMigrateType(
	(DM)PetscToPointer((dm) ),*mtype);
}
#if defined(__cplusplus)
}
#endif

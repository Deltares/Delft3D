#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* swarmpic.c */
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
#define dmswarmgetnumspecies_ DMSWARMGETNUMSPECIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmgetnumspecies_ dmswarmgetnumspecies
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsetnumspecies_ DMSWARMSETNUMSPECIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsetnumspecies_ dmswarmsetnumspecies
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmcomputelocalsizefromoptions_ DMSWARMCOMPUTELOCALSIZEFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmcomputelocalsizefromoptions_ dmswarmcomputelocalsizefromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarminitializecoordinates_ DMSWARMINITIALIZECOORDINATES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarminitializecoordinates_ dmswarminitializecoordinates
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarminitializevelocitiesfromoptions_ DMSWARMINITIALIZEVELOCITIESFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarminitializevelocitiesfromoptions_ dmswarminitializevelocitiesfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmswarmgetnumspecies_(DM sw,PetscInt *Ns, int *__ierr)
{
*__ierr = DMSwarmGetNumSpecies(
	(DM)PetscToPointer((sw) ),Ns);
}
PETSC_EXTERN void  dmswarmsetnumspecies_(DM sw,PetscInt *Ns, int *__ierr)
{
*__ierr = DMSwarmSetNumSpecies(
	(DM)PetscToPointer((sw) ),*Ns);
}
PETSC_EXTERN void  dmswarmcomputelocalsizefromoptions_(DM sw, int *__ierr)
{
*__ierr = DMSwarmComputeLocalSizeFromOptions(
	(DM)PetscToPointer((sw) ));
}
PETSC_EXTERN void  dmswarminitializecoordinates_(DM sw, int *__ierr)
{
*__ierr = DMSwarmInitializeCoordinates(
	(DM)PetscToPointer((sw) ));
}
PETSC_EXTERN void  dmswarminitializevelocitiesfromoptions_(DM sw, PetscReal v0[], int *__ierr)
{
*__ierr = DMSwarmInitializeVelocitiesFromOptions(
	(DM)PetscToPointer((sw) ),v0);
}
#if defined(__cplusplus)
}
#endif

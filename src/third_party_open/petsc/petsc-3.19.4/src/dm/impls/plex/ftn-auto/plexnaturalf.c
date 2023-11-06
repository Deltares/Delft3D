#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexnatural.c */
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
#define dmplexsetmigrationsf_ DMPLEXSETMIGRATIONSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetmigrationsf_ dmplexsetmigrationsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetmigrationsf_ DMPLEXGETMIGRATIONSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetmigrationsf_ dmplexgetmigrationsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetglobaltonaturalsf_ DMPLEXSETGLOBALTONATURALSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetglobaltonaturalsf_ dmplexsetglobaltonaturalsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetglobaltonaturalsf_ DMPLEXGETGLOBALTONATURALSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetglobaltonaturalsf_ dmplexgetglobaltonaturalsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreateglobaltonaturalsf_ DMPLEXCREATEGLOBALTONATURALSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreateglobaltonaturalsf_ dmplexcreateglobaltonaturalsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexglobaltonaturalbegin_ DMPLEXGLOBALTONATURALBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexglobaltonaturalbegin_ dmplexglobaltonaturalbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexglobaltonaturalend_ DMPLEXGLOBALTONATURALEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexglobaltonaturalend_ dmplexglobaltonaturalend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexnaturaltoglobalbegin_ DMPLEXNATURALTOGLOBALBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexnaturaltoglobalbegin_ dmplexnaturaltoglobalbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexnaturaltoglobalend_ DMPLEXNATURALTOGLOBALEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexnaturaltoglobalend_ dmplexnaturaltoglobalend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatenaturalvector_ DMPLEXCREATENATURALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatenaturalvector_ dmplexcreatenaturalvector
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsetmigrationsf_(DM dm,PetscSF migrationSF, int *__ierr)
{
*__ierr = DMPlexSetMigrationSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((migrationSF) ));
}
PETSC_EXTERN void  dmplexgetmigrationsf_(DM dm,PetscSF *migrationSF, int *__ierr)
{
*__ierr = DMPlexGetMigrationSF(
	(DM)PetscToPointer((dm) ),migrationSF);
}
PETSC_EXTERN void  dmplexsetglobaltonaturalsf_(DM dm,PetscSF naturalSF, int *__ierr)
{
*__ierr = DMPlexSetGlobalToNaturalSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((naturalSF) ));
}
PETSC_EXTERN void  dmplexgetglobaltonaturalsf_(DM dm,PetscSF *naturalSF, int *__ierr)
{
*__ierr = DMPlexGetGlobalToNaturalSF(
	(DM)PetscToPointer((dm) ),naturalSF);
}
PETSC_EXTERN void  dmplexcreateglobaltonaturalsf_(DM dm,PetscSection section,PetscSF sfMigration,PetscSF *sfNatural, int *__ierr)
{
*__ierr = DMPlexCreateGlobalToNaturalSF(
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((section) ),
	(PetscSF)PetscToPointer((sfMigration) ),sfNatural);
}
PETSC_EXTERN void  dmplexglobaltonaturalbegin_(DM dm,Vec gv,Vec nv, int *__ierr)
{
*__ierr = DMPlexGlobalToNaturalBegin(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((gv) ),
	(Vec)PetscToPointer((nv) ));
}
PETSC_EXTERN void  dmplexglobaltonaturalend_(DM dm,Vec gv,Vec nv, int *__ierr)
{
*__ierr = DMPlexGlobalToNaturalEnd(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((gv) ),
	(Vec)PetscToPointer((nv) ));
}
PETSC_EXTERN void  dmplexnaturaltoglobalbegin_(DM dm,Vec nv,Vec gv, int *__ierr)
{
*__ierr = DMPlexNaturalToGlobalBegin(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((nv) ),
	(Vec)PetscToPointer((gv) ));
}
PETSC_EXTERN void  dmplexnaturaltoglobalend_(DM dm,Vec nv,Vec gv, int *__ierr)
{
*__ierr = DMPlexNaturalToGlobalEnd(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((nv) ),
	(Vec)PetscToPointer((gv) ));
}
PETSC_EXTERN void  dmplexcreatenaturalvector_(DM dm,Vec *nv, int *__ierr)
{
*__ierr = DMPlexCreateNaturalVector(
	(DM)PetscToPointer((dm) ),nv);
}
#if defined(__cplusplus)
}
#endif

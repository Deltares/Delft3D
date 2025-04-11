#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dagtol.c */
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
#define dmdaglobaltonaturalbegin_ DMDAGLOBALTONATURALBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdaglobaltonaturalbegin_ dmdaglobaltonaturalbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdaglobaltonaturalend_ DMDAGLOBALTONATURALEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdaglobaltonaturalend_ dmdaglobaltonaturalend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdanaturaltoglobalbegin_ DMDANATURALTOGLOBALBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdanaturaltoglobalbegin_ dmdanaturaltoglobalbegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdanaturaltoglobalend_ DMDANATURALTOGLOBALEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdanaturaltoglobalend_ dmdanaturaltoglobalend
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdaglobaltonaturalbegin_(DM da,Vec g,InsertMode *mode,Vec n, int *__ierr)
{
*__ierr = DMDAGlobalToNaturalBegin(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((n) ));
}
PETSC_EXTERN void  dmdaglobaltonaturalend_(DM da,Vec g,InsertMode *mode,Vec n, int *__ierr)
{
*__ierr = DMDAGlobalToNaturalEnd(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((g) ),*mode,
	(Vec)PetscToPointer((n) ));
}
PETSC_EXTERN void  dmdanaturaltoglobalbegin_(DM da,Vec n,InsertMode *mode,Vec g, int *__ierr)
{
*__ierr = DMDANaturalToGlobalBegin(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((n) ),*mode,
	(Vec)PetscToPointer((g) ));
}
PETSC_EXTERN void  dmdanaturaltoglobalend_(DM da,Vec n,InsertMode *mode,Vec g, int *__ierr)
{
*__ierr = DMDANaturalToGlobalEnd(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((n) ),*mode,
	(Vec)PetscToPointer((g) ));
}
#if defined(__cplusplus)
}
#endif

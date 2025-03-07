#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dagetarray.c */
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
#define dmdavecrestorearray_ DMDAVECRESTOREARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearray_ dmdavecrestorearray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdavecrestorearraywrite_ DMDAVECRESTOREARRAYWRITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearraywrite_ dmdavecrestorearraywrite
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdavecrestorearraydof_ DMDAVECRESTOREARRAYDOF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearraydof_ dmdavecrestorearraydof
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdavecrestorearrayread_ DMDAVECRESTOREARRAYREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearrayread_ dmdavecrestorearrayread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdavecrestorearraydofread_ DMDAVECRESTOREARRAYDOFREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearraydofread_ dmdavecrestorearraydofread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdavecrestorearraydofwrite_ DMDAVECRESTOREARRAYDOFWRITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdavecrestorearraydofwrite_ dmdavecrestorearraydofwrite
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdavecrestorearray_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArray(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void  dmdavecrestorearraywrite_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArrayWrite(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void  dmdavecrestorearraydof_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArrayDOF(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void  dmdavecrestorearrayread_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArrayRead(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void  dmdavecrestorearraydofread_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArrayDOFRead(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
PETSC_EXTERN void  dmdavecrestorearraydofwrite_(DM da,Vec vec,void*array, int *__ierr)
{
*__ierr = DMDAVecRestoreArrayDOFWrite(
	(DM)PetscToPointer((da) ),
	(Vec)PetscToPointer((vec) ),array);
}
#if defined(__cplusplus)
}
#endif

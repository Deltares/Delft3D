#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dagetelem.c */
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
#define dmdagetelementscorners_ DMDAGETELEMENTSCORNERS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetelementscorners_ dmdagetelementscorners
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetelementssizes_ DMDAGETELEMENTSSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetelementssizes_ dmdagetelementssizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdasetelementtype_ DMDASETELEMENTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdasetelementtype_ dmdasetelementtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetelementtype_ DMDAGETELEMENTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetelementtype_ dmdagetelementtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetsubdomaincornersis_ DMDAGETSUBDOMAINCORNERSIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetsubdomaincornersis_ dmdagetsubdomaincornersis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdarestoresubdomaincornersis_ DMDARESTORESUBDOMAINCORNERSIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdarestoresubdomaincornersis_ dmdarestoresubdomaincornersis
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdagetelementscorners_(DM da,PetscInt *gx,PetscInt *gy,PetscInt *gz, int *__ierr)
{
*__ierr = DMDAGetElementsCorners(
	(DM)PetscToPointer((da) ),gx,gy,gz);
}
PETSC_EXTERN void  dmdagetelementssizes_(DM da,PetscInt *mx,PetscInt *my,PetscInt *mz, int *__ierr)
{
*__ierr = DMDAGetElementsSizes(
	(DM)PetscToPointer((da) ),mx,my,mz);
}
PETSC_EXTERN void  dmdasetelementtype_(DM da,DMDAElementType *etype, int *__ierr)
{
*__ierr = DMDASetElementType(
	(DM)PetscToPointer((da) ),*etype);
}
PETSC_EXTERN void  dmdagetelementtype_(DM da,DMDAElementType *etype, int *__ierr)
{
*__ierr = DMDAGetElementType(
	(DM)PetscToPointer((da) ),etype);
}
PETSC_EXTERN void  dmdagetsubdomaincornersis_(DM dm,IS *is, int *__ierr)
{
*__ierr = DMDAGetSubdomainCornersIS(
	(DM)PetscToPointer((dm) ),is);
}
PETSC_EXTERN void  dmdarestoresubdomaincornersis_(DM dm,IS *is, int *__ierr)
{
*__ierr = DMDARestoreSubdomainCornersIS(
	(DM)PetscToPointer((dm) ),is);
}
#if defined(__cplusplus)
}
#endif

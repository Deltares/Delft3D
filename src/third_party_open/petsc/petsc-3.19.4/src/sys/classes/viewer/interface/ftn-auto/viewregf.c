#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* viewreg.c */
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

#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionspushgetvieweroff_ PETSCOPTIONSPUSHGETVIEWEROFF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionspushgetvieweroff_ petscoptionspushgetvieweroff
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionspopgetvieweroff_ PETSCOPTIONSPOPGETVIEWEROFF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionspopgetvieweroff_ petscoptionspopgetvieweroff
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscoptionsgetvieweroff_ PETSCOPTIONSGETVIEWEROFF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscoptionsgetvieweroff_ petscoptionsgetvieweroff
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewercreate_ PETSCVIEWERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewercreate_ petscviewercreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscoptionspushgetvieweroff_(PetscBool *flg, int *__ierr)
{
*__ierr = PetscOptionsPushGetViewerOff(*flg);
}
PETSC_EXTERN void  petscoptionspopgetvieweroff_(int *__ierr)
{
*__ierr = PetscOptionsPopGetViewerOff();
}
PETSC_EXTERN void  petscoptionsgetvieweroff_(PetscBool *flg, int *__ierr)
{
*__ierr = PetscOptionsGetViewerOff(flg);
}
PETSC_EXTERN void  petscviewercreate_(MPI_Fint * comm,PetscViewer *inviewer, int *__ierr)
{
*__ierr = PetscViewerCreate(
	MPI_Comm_f2c(*(comm)),inviewer);
}
#if defined(__cplusplus)
}
#endif

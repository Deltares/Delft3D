#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* spacesum.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumgetnumsubspaces_ PETSCSPACESUMGETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumgetnumsubspaces_ petscspacesumgetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumsetnumsubspaces_ PETSCSPACESUMSETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumsetnumsubspaces_ petscspacesumsetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumgetconcatenate_ PETSCSPACESUMGETCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumgetconcatenate_ petscspacesumgetconcatenate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumsetconcatenate_ PETSCSPACESUMSETCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumsetconcatenate_ petscspacesumsetconcatenate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumgetsubspace_ PETSCSPACESUMGETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumgetsubspace_ petscspacesumgetsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacesumsetsubspace_ PETSCSPACESUMSETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacesumsetsubspace_ petscspacesumsetsubspace
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspacesumgetnumsubspaces_(PetscSpace sp,PetscInt *numSumSpaces, int *__ierr)
{
*__ierr = PetscSpaceSumGetNumSubspaces(
	(PetscSpace)PetscToPointer((sp) ),numSumSpaces);
}
PETSC_EXTERN void  petscspacesumsetnumsubspaces_(PetscSpace sp,PetscInt *numSumSpaces, int *__ierr)
{
*__ierr = PetscSpaceSumSetNumSubspaces(
	(PetscSpace)PetscToPointer((sp) ),*numSumSpaces);
}
PETSC_EXTERN void  petscspacesumgetconcatenate_(PetscSpace sp,PetscBool *concatenate, int *__ierr)
{
*__ierr = PetscSpaceSumGetConcatenate(
	(PetscSpace)PetscToPointer((sp) ),concatenate);
}
PETSC_EXTERN void  petscspacesumsetconcatenate_(PetscSpace sp,PetscBool *concatenate, int *__ierr)
{
*__ierr = PetscSpaceSumSetConcatenate(
	(PetscSpace)PetscToPointer((sp) ),*concatenate);
}
PETSC_EXTERN void  petscspacesumgetsubspace_(PetscSpace sp,PetscInt *s,PetscSpace *subsp, int *__ierr)
{
*__ierr = PetscSpaceSumGetSubspace(
	(PetscSpace)PetscToPointer((sp) ),*s,subsp);
}
PETSC_EXTERN void  petscspacesumsetsubspace_(PetscSpace sp,PetscInt *s,PetscSpace subsp, int *__ierr)
{
*__ierr = PetscSpaceSumSetSubspace(
	(PetscSpace)PetscToPointer((sp) ),*s,
	(PetscSpace)PetscToPointer((subsp) ));
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* spacetensor.c */
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
#define petscspacetensorsetnumsubspaces_ PETSCSPACETENSORSETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacetensorsetnumsubspaces_ petscspacetensorsetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacetensorgetnumsubspaces_ PETSCSPACETENSORGETNUMSUBSPACES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacetensorgetnumsubspaces_ petscspacetensorgetnumsubspaces
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacetensorsetsubspace_ PETSCSPACETENSORSETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacetensorsetsubspace_ petscspacetensorsetsubspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscspacetensorgetsubspace_ PETSCSPACETENSORGETSUBSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscspacetensorgetsubspace_ petscspacetensorgetsubspace
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscspacetensorsetnumsubspaces_(PetscSpace sp,PetscInt *numTensSpaces, int *__ierr)
{
*__ierr = PetscSpaceTensorSetNumSubspaces(
	(PetscSpace)PetscToPointer((sp) ),*numTensSpaces);
}
PETSC_EXTERN void  petscspacetensorgetnumsubspaces_(PetscSpace sp,PetscInt *numTensSpaces, int *__ierr)
{
*__ierr = PetscSpaceTensorGetNumSubspaces(
	(PetscSpace)PetscToPointer((sp) ),numTensSpaces);
}
PETSC_EXTERN void  petscspacetensorsetsubspace_(PetscSpace sp,PetscInt *s,PetscSpace subsp, int *__ierr)
{
*__ierr = PetscSpaceTensorSetSubspace(
	(PetscSpace)PetscToPointer((sp) ),*s,
	(PetscSpace)PetscToPointer((subsp) ));
}
PETSC_EXTERN void  petscspacetensorgetsubspace_(PetscSpace sp,PetscInt *s,PetscSpace *subsp, int *__ierr)
{
*__ierr = PetscSpaceTensorGetSubspace(
	(PetscSpace)PetscToPointer((sp) ),*s,subsp);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* multiblock.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmultiblocksetfields_ SNESMULTIBLOCKSETFIELDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmultiblocksetfields_ snesmultiblocksetfields
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmultiblocksetis_ SNESMULTIBLOCKSETIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmultiblocksetis_ snesmultiblocksetis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmultiblocksettype_ SNESMULTIBLOCKSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmultiblocksettype_ snesmultiblocksettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmultiblocksetblocksize_ SNESMULTIBLOCKSETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmultiblocksetblocksize_ snesmultiblocksetblocksize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesmultiblocksetfields_(SNES snes, char name[],PetscInt *n, PetscInt *fields, int *__ierr, int cl0)
{
*__ierr = SNESMultiblockSetFields(
	(SNES)PetscToPointer((snes) ),name,*n,fields);
}
PETSC_EXTERN void  snesmultiblocksetis_(SNES snes, char name[],IS is, int *__ierr, int cl0)
{
*__ierr = SNESMultiblockSetIS(
	(SNES)PetscToPointer((snes) ),name,
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  snesmultiblocksettype_(SNES snes,PCCompositeType *type, int *__ierr)
{
*__ierr = SNESMultiblockSetType(
	(SNES)PetscToPointer((snes) ),*type);
}
PETSC_EXTERN void  snesmultiblocksetblocksize_(SNES snes,PetscInt *bs, int *__ierr)
{
*__ierr = SNESMultiblockSetBlockSize(
	(SNES)PetscToPointer((snes) ),*bs);
}
#if defined(__cplusplus)
}
#endif

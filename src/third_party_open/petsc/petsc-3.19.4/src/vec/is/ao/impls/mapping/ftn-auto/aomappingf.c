#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* aomapping.c */
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

#include "petscao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aomappinghaspetscindex_ AOMAPPINGHASPETSCINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aomappinghaspetscindex_ aomappinghaspetscindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define aocreatemappingis_ AOCREATEMAPPINGIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define aocreatemappingis_ aocreatemappingis
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  aomappinghaspetscindex_(AO ao,PetscInt *idex,PetscBool *hasIndex, int *__ierr)
{
*__ierr = AOMappingHasPetscIndex(
	(AO)PetscToPointer((ao) ),*idex,hasIndex);
}
PETSC_EXTERN void  aocreatemappingis_(IS isapp,IS ispetsc,AO *aoout, int *__ierr)
{
*__ierr = AOCreateMappingIS(
	(IS)PetscToPointer((isapp) ),
	(IS)PetscToPointer((ispetsc) ),aoout);
}
#if defined(__cplusplus)
}
#endif

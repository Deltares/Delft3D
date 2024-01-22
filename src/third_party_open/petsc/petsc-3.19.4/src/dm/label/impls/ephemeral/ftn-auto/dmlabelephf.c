#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmlabeleph.c */
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

#include "petscdmlabelephemeral.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelephemeralgetlabel_ DMLABELEPHEMERALGETLABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelephemeralgetlabel_ dmlabelephemeralgetlabel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmlabelephemeralsetlabel_ DMLABELEPHEMERALSETLABEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmlabelephemeralsetlabel_ dmlabelephemeralsetlabel
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmlabelephemeralgetlabel_(DMLabel label,DMLabel *olabel, int *__ierr)
{
*__ierr = DMLabelEphemeralGetLabel(
	(DMLabel)PetscToPointer((label) ),olabel);
}
PETSC_EXTERN void  dmlabelephemeralsetlabel_(DMLabel label,DMLabel olabel, int *__ierr)
{
*__ierr = DMLabelEphemeralSetLabel(
	(DMLabel)PetscToPointer((label) ),
	(DMLabel)PetscToPointer((olabel) ));
}
#if defined(__cplusplus)
}
#endif

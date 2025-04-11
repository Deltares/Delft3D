#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dacorn.c */
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
#define dmdagetreduceddmda_ DMDAGETREDUCEDDMDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetreduceddmda_ dmdagetreduceddmda
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdacreatecompatibledmda_ DMDACREATECOMPATIBLEDMDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdacreatecompatibledmda_ dmdacreatecompatibledmda
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdagetreduceddmda_(DM da,PetscInt *nfields,DM *nda, int *__ierr)
{
*__ierr = DMDAGetReducedDMDA(
	(DM)PetscToPointer((da) ),*nfields,nda);
}
PETSC_EXTERN void  dmdacreatecompatibledmda_(DM da,PetscInt *nfields,DM *nda, int *__ierr)
{
*__ierr = DMDACreateCompatibleDMDA(
	(DM)PetscToPointer((da) ),*nfields,nda);
}
#if defined(__cplusplus)
}
#endif

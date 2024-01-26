#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* feopencl.c */
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
#define petscfeopenclsetrealtype_ PETSCFEOPENCLSETREALTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfeopenclsetrealtype_ petscfeopenclsetrealtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfeopenclgetrealtype_ PETSCFEOPENCLGETREALTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfeopenclgetrealtype_ petscfeopenclgetrealtype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscfeopenclsetrealtype_(PetscFE fem,PetscDataType *realType, int *__ierr)
{
*__ierr = PetscFEOpenCLSetRealType(
	(PetscFE)PetscToPointer((fem) ),*realType);
}
PETSC_EXTERN void  petscfeopenclgetrealtype_(PetscFE fem,PetscDataType *realType, int *__ierr)
{
*__ierr = PetscFEOpenCLGetRealType(
	(PetscFE)PetscToPointer((fem) ),realType);
}
#if defined(__cplusplus)
}
#endif

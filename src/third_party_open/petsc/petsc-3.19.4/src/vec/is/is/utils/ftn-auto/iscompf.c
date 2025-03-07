#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* iscomp.c */
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

#include "petscis.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isequal_ ISEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isequal_ isequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isequalunsorted_ ISEQUALUNSORTED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isequalunsorted_ isequalunsorted
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  isequal_(IS is1,IS is2,PetscBool *flg, int *__ierr)
{
*__ierr = ISEqual(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),flg);
}
PETSC_EXTERN void  isequalunsorted_(IS is1,IS is2,PetscBool *flg, int *__ierr)
{
*__ierr = ISEqualUnsorted(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),flg);
}
#if defined(__cplusplus)
}
#endif

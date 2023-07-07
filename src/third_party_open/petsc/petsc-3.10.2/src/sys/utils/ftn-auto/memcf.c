#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* memc.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmemcmp_ PETSCMEMCMP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmemcmp_ petscmemcmp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmemmove_ PETSCMEMMOVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmemmove_ petscmemmove
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscmemcmp_( void*str1, void*str2,size_t *len,PetscBool  *e, int *__ierr){
*__ierr = PetscMemcmp(str1,str2,*len,e);
}
PETSC_EXTERN void PETSC_STDCALL  petscmemmove_(void*a,void*b,size_t *n, int *__ierr){
*__ierr = PetscMemmove(a,b,*n);
}
#if defined(__cplusplus)
}
#endif

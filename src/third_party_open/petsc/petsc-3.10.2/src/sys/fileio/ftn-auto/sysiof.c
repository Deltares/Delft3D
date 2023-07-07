#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sysio.c */
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

#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbinaryread_ PETSCBINARYREAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbinaryread_ petscbinaryread
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbinarywrite_ PETSCBINARYWRITE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbinarywrite_ petscbinarywrite
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscbinaryclose_ PETSCBINARYCLOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscbinaryclose_ petscbinaryclose
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscbinaryread_(int *fd,void*p,PetscInt *n,PetscDataType *type, int *__ierr){
*__ierr = PetscBinaryRead(*fd,p,*n,*type);
}
PETSC_EXTERN void PETSC_STDCALL  petscbinarywrite_(int *fd,void*p,PetscInt *n,PetscDataType *type,PetscBool  *istemp, int *__ierr){
*__ierr = PetscBinaryWrite(*fd,p,*n,*type,*istemp);
}
PETSC_EXTERN void PETSC_STDCALL  petscbinaryclose_(int *fd, int *__ierr){
*__ierr = PetscBinaryClose(*fd);
}
#if defined(__cplusplus)
}
#endif

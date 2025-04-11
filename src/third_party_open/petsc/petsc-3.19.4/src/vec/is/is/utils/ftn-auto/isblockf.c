#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* isblock.c */
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
#define iscompressindicesgeneral_ ISCOMPRESSINDICESGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscompressindicesgeneral_ iscompressindicesgeneral
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  iscompressindicesgeneral_(PetscInt *n,PetscInt *nkeys,PetscInt *bs,PetscInt *imax, IS is_in[],IS is_out[], int *__ierr)
{
*__ierr = ISCompressIndicesGeneral(*n,*nkeys,*bs,*imax,is_in,is_out);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* block.c */
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
#define isblocksetindices_ ISBLOCKSETINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isblocksetindices_ isblocksetindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscreateblock_ ISCREATEBLOCK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscreateblock_ iscreateblock
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isblockgetlocalsize_ ISBLOCKGETLOCALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isblockgetlocalsize_ isblockgetlocalsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isblockgetsize_ ISBLOCKGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isblockgetsize_ isblockgetsize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  isblocksetindices_(IS is,PetscInt *bs,PetscInt *n, PetscInt idx[],PetscCopyMode *mode, int *__ierr)
{
*__ierr = ISBlockSetIndices(
	(IS)PetscToPointer((is) ),*bs,*n,idx,*mode);
}
PETSC_EXTERN void  iscreateblock_(MPI_Fint * comm,PetscInt *bs,PetscInt *n, PetscInt idx[],PetscCopyMode *mode,IS *is, int *__ierr)
{
*__ierr = ISCreateBlock(
	MPI_Comm_f2c(*(comm)),*bs,*n,idx,*mode,is);
}
PETSC_EXTERN void  isblockgetlocalsize_(IS is,PetscInt *size, int *__ierr)
{
*__ierr = ISBlockGetLocalSize(
	(IS)PetscToPointer((is) ),size);
}
PETSC_EXTERN void  isblockgetsize_(IS is,PetscInt *size, int *__ierr)
{
*__ierr = ISBlockGetSize(
	(IS)PetscToPointer((is) ),size);
}
#if defined(__cplusplus)
}
#endif

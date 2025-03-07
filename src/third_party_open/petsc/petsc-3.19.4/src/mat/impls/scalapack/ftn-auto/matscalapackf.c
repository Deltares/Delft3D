#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matscalapack.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matscalapacksetblocksizes_ MATSCALAPACKSETBLOCKSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matscalapacksetblocksizes_ matscalapacksetblocksizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matscalapackgetblocksizes_ MATSCALAPACKGETBLOCKSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matscalapackgetblocksizes_ matscalapackgetblocksizes
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matscalapacksetblocksizes_(Mat A,PetscInt *mb,PetscInt *nb, int *__ierr)
{
*__ierr = MatScaLAPACKSetBlockSizes(
	(Mat)PetscToPointer((A) ),*mb,*nb);
}
PETSC_EXTERN void  matscalapackgetblocksizes_(Mat A,PetscInt *mb,PetscInt *nb, int *__ierr)
{
*__ierr = MatScaLAPACKGetBlockSizes(
	(Mat)PetscToPointer((A) ),mb,nb);
}
#if defined(__cplusplus)
}
#endif

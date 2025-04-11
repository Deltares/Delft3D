#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mkl_cpardiso.c */
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
#define matmkl_cpardisosetcntl_ MATMKL_CPARDISOSETCNTL
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define matmkl_cpardisosetcntl_ matmkl_cpardisosetcntl__
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE)
#define matmkl_cpardisosetcntl_ matmkl_cpardisosetcntl
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matmkl_cpardisosetcntl_(Mat F,PetscInt *icntl,PetscInt *ival, int *__ierr)
{
*__ierr = MatMkl_CPardisoSetCntl(
	(Mat)PetscToPointer((F) ),*icntl,*ival);
}
#if defined(__cplusplus)
}
#endif

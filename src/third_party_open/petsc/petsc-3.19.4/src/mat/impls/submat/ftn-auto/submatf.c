#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* submat.c */
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
#define matcreatesubmatrixvirtual_ MATCREATESUBMATRIXVIRTUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatesubmatrixvirtual_ matcreatesubmatrixvirtual
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsubmatrixvirtualupdate_ MATSUBMATRIXVIRTUALUPDATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsubmatrixvirtualupdate_ matsubmatrixvirtualupdate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreatesubmatrixvirtual_(Mat A,IS isrow,IS iscol,Mat *newmat, int *__ierr)
{
*__ierr = MatCreateSubMatrixVirtual(
	(Mat)PetscToPointer((A) ),
	(IS)PetscToPointer((isrow) ),
	(IS)PetscToPointer((iscol) ),newmat);
}
PETSC_EXTERN void  matsubmatrixvirtualupdate_(Mat N,Mat A,IS isrow,IS iscol, int *__ierr)
{
*__ierr = MatSubMatrixVirtualUpdate(
	(Mat)PetscToPointer((N) ),
	(Mat)PetscToPointer((A) ),
	(IS)PetscToPointer((isrow) ),
	(IS)PetscToPointer((iscol) ));
}
#if defined(__cplusplus)
}
#endif

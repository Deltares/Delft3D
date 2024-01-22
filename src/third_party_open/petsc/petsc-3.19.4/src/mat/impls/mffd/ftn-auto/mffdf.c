#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mffd.c */
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
#define matcreatemffd_ MATCREATEMFFD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatemffd_ matcreatemffd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdgeth_ MATMFFDGETH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdgeth_ matmffdgeth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdsetperiod_ MATMFFDSETPERIOD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdsetperiod_ matmffdsetperiod
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdsetfunctionerror_ MATMFFDSETFUNCTIONERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdsetfunctionerror_ matmffdsetfunctionerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdsethhistory_ MATMFFDSETHHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdsethhistory_ matmffdsethhistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdresethhistory_ MATMFFDRESETHHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdresethhistory_ matmffdresethhistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdsetbase_ MATMFFDSETBASE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdsetbase_ matmffdsetbase
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmffdcheckpositivity_ MATMFFDCHECKPOSITIVITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmffdcheckpositivity_ matmffdcheckpositivity
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matcreatemffd_(MPI_Fint * comm,PetscInt *m,PetscInt *n,PetscInt *M,PetscInt *N,Mat *J, int *__ierr)
{
*__ierr = MatCreateMFFD(
	MPI_Comm_f2c(*(comm)),*m,*n,*M,*N,J);
}
PETSC_EXTERN void  matmffdgeth_(Mat mat,PetscScalar *h, int *__ierr)
{
*__ierr = MatMFFDGetH(
	(Mat)PetscToPointer((mat) ),h);
}
PETSC_EXTERN void  matmffdsetperiod_(Mat mat,PetscInt *period, int *__ierr)
{
*__ierr = MatMFFDSetPeriod(
	(Mat)PetscToPointer((mat) ),*period);
}
PETSC_EXTERN void  matmffdsetfunctionerror_(Mat mat,PetscReal *error, int *__ierr)
{
*__ierr = MatMFFDSetFunctionError(
	(Mat)PetscToPointer((mat) ),*error);
}
PETSC_EXTERN void  matmffdsethhistory_(Mat J,PetscScalar history[],PetscInt *nhistory, int *__ierr)
{
*__ierr = MatMFFDSetHHistory(
	(Mat)PetscToPointer((J) ),history,*nhistory);
}
PETSC_EXTERN void  matmffdresethhistory_(Mat J, int *__ierr)
{
*__ierr = MatMFFDResetHHistory(
	(Mat)PetscToPointer((J) ));
}
PETSC_EXTERN void  matmffdsetbase_(Mat J,Vec U,Vec F, int *__ierr)
{
*__ierr = MatMFFDSetBase(
	(Mat)PetscToPointer((J) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((F) ));
}
PETSC_EXTERN void  matmffdcheckpositivity_(void*dummy,Vec U,Vec a,PetscScalar *h, int *__ierr)
{
*__ierr = MatMFFDCheckPositivity(dummy,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((a) ),h);
}
#if defined(__cplusplus)
}
#endif

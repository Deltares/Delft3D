#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mumps.c */
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
#define matmumpsseticntl_ MATMUMPSSETICNTL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsseticntl_ matmumpsseticntl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgeticntl_ MATMUMPSGETICNTL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgeticntl_ matmumpsgeticntl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpssetcntl_ MATMUMPSSETCNTL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpssetcntl_ matmumpssetcntl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetcntl_ MATMUMPSGETCNTL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetcntl_ matmumpsgetcntl
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetinverse_ MATMUMPSGETINVERSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetinverse_ matmumpsgetinverse
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetinversetranspose_ MATMUMPSGETINVERSETRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetinversetranspose_ matmumpsgetinversetranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetinfo_ MATMUMPSGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetinfo_ matmumpsgetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetinfog_ MATMUMPSGETINFOG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetinfog_ matmumpsgetinfog
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetrinfo_ MATMUMPSGETRINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetrinfo_ matmumpsgetrinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetrinfog_ MATMUMPSGETRINFOG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetrinfog_ matmumpsgetrinfog
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmumpsgetnullpivots_ MATMUMPSGETNULLPIVOTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmumpsgetnullpivots_ matmumpsgetnullpivots
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matmumpsseticntl_(Mat F,PetscInt *icntl,PetscInt *ival, int *__ierr)
{
*__ierr = MatMumpsSetIcntl(
	(Mat)PetscToPointer((F) ),*icntl,*ival);
}
PETSC_EXTERN void  matmumpsgeticntl_(Mat F,PetscInt *icntl,PetscInt *ival, int *__ierr)
{
*__ierr = MatMumpsGetIcntl(
	(Mat)PetscToPointer((F) ),*icntl,ival);
}
PETSC_EXTERN void  matmumpssetcntl_(Mat F,PetscInt *icntl,PetscReal *val, int *__ierr)
{
*__ierr = MatMumpsSetCntl(
	(Mat)PetscToPointer((F) ),*icntl,*val);
}
PETSC_EXTERN void  matmumpsgetcntl_(Mat F,PetscInt *icntl,PetscReal *val, int *__ierr)
{
*__ierr = MatMumpsGetCntl(
	(Mat)PetscToPointer((F) ),*icntl,val);
}
PETSC_EXTERN void  matmumpsgetinverse_(Mat F,Mat spRHS, int *__ierr)
{
*__ierr = MatMumpsGetInverse(
	(Mat)PetscToPointer((F) ),
	(Mat)PetscToPointer((spRHS) ));
}
PETSC_EXTERN void  matmumpsgetinversetranspose_(Mat F,Mat spRHST, int *__ierr)
{
*__ierr = MatMumpsGetInverseTranspose(
	(Mat)PetscToPointer((F) ),
	(Mat)PetscToPointer((spRHST) ));
}
PETSC_EXTERN void  matmumpsgetinfo_(Mat F,PetscInt *icntl,PetscInt *ival, int *__ierr)
{
*__ierr = MatMumpsGetInfo(
	(Mat)PetscToPointer((F) ),*icntl,ival);
}
PETSC_EXTERN void  matmumpsgetinfog_(Mat F,PetscInt *icntl,PetscInt *ival, int *__ierr)
{
*__ierr = MatMumpsGetInfog(
	(Mat)PetscToPointer((F) ),*icntl,ival);
}
PETSC_EXTERN void  matmumpsgetrinfo_(Mat F,PetscInt *icntl,PetscReal *val, int *__ierr)
{
*__ierr = MatMumpsGetRinfo(
	(Mat)PetscToPointer((F) ),*icntl,val);
}
PETSC_EXTERN void  matmumpsgetrinfog_(Mat F,PetscInt *icntl,PetscReal *val, int *__ierr)
{
*__ierr = MatMumpsGetRinfog(
	(Mat)PetscToPointer((F) ),*icntl,val);
}
PETSC_EXTERN void  matmumpsgetnullpivots_(Mat F,PetscInt *size,PetscInt **array, int *__ierr)
{
*__ierr = MatMumpsGetNullPivots(
	(Mat)PetscToPointer((F) ),size,array);
}
#if defined(__cplusplus)
}
#endif

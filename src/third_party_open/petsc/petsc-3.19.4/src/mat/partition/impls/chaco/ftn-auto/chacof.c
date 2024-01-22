#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* chaco.c */
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
#define matpartitioningchacosetglobal_ MATPARTITIONINGCHACOSETGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacosetglobal_ matpartitioningchacosetglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacogetglobal_ MATPARTITIONINGCHACOGETGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacogetglobal_ matpartitioningchacogetglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacosetlocal_ MATPARTITIONINGCHACOSETLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacosetlocal_ matpartitioningchacosetlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacogetlocal_ MATPARTITIONINGCHACOGETLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacogetlocal_ matpartitioningchacogetlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacosetcoarselevel_ MATPARTITIONINGCHACOSETCOARSELEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacosetcoarselevel_ matpartitioningchacosetcoarselevel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacoseteigensolver_ MATPARTITIONINGCHACOSETEIGENSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacoseteigensolver_ matpartitioningchacoseteigensolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacogeteigensolver_ MATPARTITIONINGCHACOGETEIGENSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacogeteigensolver_ matpartitioningchacogeteigensolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacoseteigentol_ MATPARTITIONINGCHACOSETEIGENTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacoseteigentol_ matpartitioningchacoseteigentol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacogeteigentol_ MATPARTITIONINGCHACOGETEIGENTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacogeteigentol_ matpartitioningchacogeteigentol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacoseteigennumber_ MATPARTITIONINGCHACOSETEIGENNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacoseteigennumber_ matpartitioningchacoseteigennumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningchacogeteigennumber_ MATPARTITIONINGCHACOGETEIGENNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningchacogeteigennumber_ matpartitioningchacogeteigennumber
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matpartitioningchacosetglobal_(MatPartitioning part,MPChacoGlobalType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoSetGlobal(
	(MatPartitioning)PetscToPointer((part) ),*method);
}
PETSC_EXTERN void  matpartitioningchacogetglobal_(MatPartitioning part,MPChacoGlobalType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoGetGlobal(
	(MatPartitioning)PetscToPointer((part) ),method);
}
PETSC_EXTERN void  matpartitioningchacosetlocal_(MatPartitioning part,MPChacoLocalType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoSetLocal(
	(MatPartitioning)PetscToPointer((part) ),*method);
}
PETSC_EXTERN void  matpartitioningchacogetlocal_(MatPartitioning part,MPChacoLocalType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoGetLocal(
	(MatPartitioning)PetscToPointer((part) ),method);
}
PETSC_EXTERN void  matpartitioningchacosetcoarselevel_(MatPartitioning part,PetscReal *level, int *__ierr)
{
*__ierr = MatPartitioningChacoSetCoarseLevel(
	(MatPartitioning)PetscToPointer((part) ),*level);
}
PETSC_EXTERN void  matpartitioningchacoseteigensolver_(MatPartitioning part,MPChacoEigenType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoSetEigenSolver(
	(MatPartitioning)PetscToPointer((part) ),*method);
}
PETSC_EXTERN void  matpartitioningchacogeteigensolver_(MatPartitioning part,MPChacoEigenType *method, int *__ierr)
{
*__ierr = MatPartitioningChacoGetEigenSolver(
	(MatPartitioning)PetscToPointer((part) ),method);
}
PETSC_EXTERN void  matpartitioningchacoseteigentol_(MatPartitioning part,PetscReal *tol, int *__ierr)
{
*__ierr = MatPartitioningChacoSetEigenTol(
	(MatPartitioning)PetscToPointer((part) ),*tol);
}
PETSC_EXTERN void  matpartitioningchacogeteigentol_(MatPartitioning part,PetscReal *tol, int *__ierr)
{
*__ierr = MatPartitioningChacoGetEigenTol(
	(MatPartitioning)PetscToPointer((part) ),tol);
}
PETSC_EXTERN void  matpartitioningchacoseteigennumber_(MatPartitioning part,PetscInt *num, int *__ierr)
{
*__ierr = MatPartitioningChacoSetEigenNumber(
	(MatPartitioning)PetscToPointer((part) ),*num);
}
PETSC_EXTERN void  matpartitioningchacogeteigennumber_(MatPartitioning part,PetscInt *num, int *__ierr)
{
*__ierr = MatPartitioningChacoGetEigenNumber(
	(MatPartitioning)PetscToPointer((part) ),num);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* strumpack.c */
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
#define matstrumpacksetreordering_ MATSTRUMPACKSETREORDERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetreordering_ matstrumpacksetreordering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksetcolperm_ MATSTRUMPACKSETCOLPERM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksetcolperm_ matstrumpacksetcolperm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksethssreltol_ MATSTRUMPACKSETHSSRELTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksethssreltol_ matstrumpacksethssreltol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksethssabstol_ MATSTRUMPACKSETHSSABSTOL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksethssabstol_ matstrumpacksethssabstol
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksethssmaxrank_ MATSTRUMPACKSETHSSMAXRANK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksethssmaxrank_ matstrumpacksethssmaxrank
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksethssleafsize_ MATSTRUMPACKSETHSSLEAFSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksethssleafsize_ matstrumpacksethssleafsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstrumpacksethssminsepsize_ MATSTRUMPACKSETHSSMINSEPSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstrumpacksethssminsepsize_ matstrumpacksethssminsepsize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matstrumpacksetreordering_(Mat F,MatSTRUMPACKReordering *reordering, int *__ierr)
{
*__ierr = MatSTRUMPACKSetReordering(
	(Mat)PetscToPointer((F) ),*reordering);
}
PETSC_EXTERN void  matstrumpacksetcolperm_(Mat F,PetscBool *cperm, int *__ierr)
{
*__ierr = MatSTRUMPACKSetColPerm(
	(Mat)PetscToPointer((F) ),*cperm);
}
PETSC_EXTERN void  matstrumpacksethssreltol_(Mat F,PetscReal *rtol, int *__ierr)
{
*__ierr = MatSTRUMPACKSetHSSRelTol(
	(Mat)PetscToPointer((F) ),*rtol);
}
PETSC_EXTERN void  matstrumpacksethssabstol_(Mat F,PetscReal *atol, int *__ierr)
{
*__ierr = MatSTRUMPACKSetHSSAbsTol(
	(Mat)PetscToPointer((F) ),*atol);
}
PETSC_EXTERN void  matstrumpacksethssmaxrank_(Mat F,PetscInt *hssmaxrank, int *__ierr)
{
*__ierr = MatSTRUMPACKSetHSSMaxRank(
	(Mat)PetscToPointer((F) ),*hssmaxrank);
}
PETSC_EXTERN void  matstrumpacksethssleafsize_(Mat F,PetscInt *leaf_size, int *__ierr)
{
*__ierr = MatSTRUMPACKSetHSSLeafSize(
	(Mat)PetscToPointer((F) ),*leaf_size);
}
PETSC_EXTERN void  matstrumpacksethssminsepsize_(Mat F,PetscInt *hssminsize, int *__ierr)
{
*__ierr = MatSTRUMPACKSetHSSMinSepSize(
	(Mat)PetscToPointer((F) ),*hssminsize);
}
#if defined(__cplusplus)
}
#endif

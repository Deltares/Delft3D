#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* general.c */
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
#define iscreategeneral_ ISCREATEGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscreategeneral_ iscreategeneral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgeneralsetindices_ ISGENERALSETINDICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgeneralsetindices_ isgeneralsetindices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgeneralsetindicesfrommask_ ISGENERALSETINDICESFROMMASK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgeneralsetindicesfrommask_ isgeneralsetindicesfrommask
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgeneralfilter_ ISGENERALFILTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgeneralfilter_ isgeneralfilter
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  iscreategeneral_(MPI_Fint * comm,PetscInt *n, PetscInt idx[],PetscCopyMode *mode,IS *is, int *__ierr)
{
*__ierr = ISCreateGeneral(
	MPI_Comm_f2c(*(comm)),*n,idx,*mode,is);
}
PETSC_EXTERN void  isgeneralsetindices_(IS is,PetscInt *n, PetscInt idx[],PetscCopyMode *mode, int *__ierr)
{
*__ierr = ISGeneralSetIndices(
	(IS)PetscToPointer((is) ),*n,idx,*mode);
}
PETSC_EXTERN void  isgeneralsetindicesfrommask_(IS is,PetscInt *rstart,PetscInt *rend, PetscBool mask[], int *__ierr)
{
*__ierr = ISGeneralSetIndicesFromMask(
	(IS)PetscToPointer((is) ),*rstart,*rend,mask);
}
PETSC_EXTERN void  isgeneralfilter_(IS is,PetscInt *start,PetscInt *end, int *__ierr)
{
*__ierr = ISGeneralFilter(
	(IS)PetscToPointer((is) ),*start,*end);
}
#if defined(__cplusplus)
}
#endif

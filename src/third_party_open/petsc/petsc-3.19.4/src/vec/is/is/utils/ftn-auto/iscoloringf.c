#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* iscoloring.c */
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
#define iscoloringdestroy_ ISCOLORINGDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscoloringdestroy_ iscoloringdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscoloringcreate_ ISCOLORINGCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscoloringcreate_ iscoloringcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isbuildtwosided_ ISBUILDTWOSIDED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isbuildtwosided_ isbuildtwosided
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ispartitioningtonumbering_ ISPARTITIONINGTONUMBERING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ispartitioningtonumbering_ ispartitioningtonumbering
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ispartitioningcount_ ISPARTITIONINGCOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ispartitioningcount_ ispartitioningcount
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isallgather_ ISALLGATHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isallgather_ isallgather
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscomplement_ ISCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscomplement_ iscomplement
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  iscoloringdestroy_(ISColoring *iscoloring, int *__ierr)
{
*__ierr = ISColoringDestroy(iscoloring);
}
PETSC_EXTERN void  iscoloringcreate_(MPI_Fint * comm,PetscInt *ncolors,PetscInt *n, ISColoringValue colors[],PetscCopyMode *mode,ISColoring *iscoloring, int *__ierr)
{
*__ierr = ISColoringCreate(
	MPI_Comm_f2c(*(comm)),*ncolors,*n,
	(ISColoringValue* )PetscToPointer((colors) ),*mode,iscoloring);
}
PETSC_EXTERN void  isbuildtwosided_(IS ito,IS toindx,IS *rows, int *__ierr)
{
*__ierr = ISBuildTwoSided(
	(IS)PetscToPointer((ito) ),
	(IS)PetscToPointer((toindx) ),rows);
}
PETSC_EXTERN void  ispartitioningtonumbering_(IS part,IS *is, int *__ierr)
{
*__ierr = ISPartitioningToNumbering(
	(IS)PetscToPointer((part) ),is);
}
PETSC_EXTERN void  ispartitioningcount_(IS part,PetscInt *len,PetscInt count[], int *__ierr)
{
*__ierr = ISPartitioningCount(
	(IS)PetscToPointer((part) ),*len,count);
}
PETSC_EXTERN void  isallgather_(IS is,IS *isout, int *__ierr)
{
*__ierr = ISAllGather(
	(IS)PetscToPointer((is) ),isout);
}
PETSC_EXTERN void  iscomplement_(IS is,PetscInt *nmin,PetscInt *nmax,IS *isout, int *__ierr)
{
*__ierr = ISComplement(
	(IS)PetscToPointer((is) ),*nmin,*nmax,isout);
}
#if defined(__cplusplus)
}
#endif

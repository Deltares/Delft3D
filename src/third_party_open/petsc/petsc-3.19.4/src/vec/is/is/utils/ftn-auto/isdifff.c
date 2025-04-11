#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* isdiff.c */
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
#define isdifference_ ISDIFFERENCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isdifference_ isdifference
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issum_ ISSUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issum_ issum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isexpand_ ISEXPAND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isexpand_ isexpand
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isintersect_ ISINTERSECT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isintersect_ isintersect
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isconcatenate_ ISCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isconcatenate_ isconcatenate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define islisttopair_ ISLISTTOPAIR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define islisttopair_ islisttopair
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ispairtolist_ ISPAIRTOLIST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ispairtolist_ ispairtolist
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isembed_ ISEMBED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isembed_ isembed
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issortpermutation_ ISSORTPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issortpermutation_ issortpermutation
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  isdifference_(IS is1,IS is2,IS *isout, int *__ierr)
{
*__ierr = ISDifference(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),isout);
}
PETSC_EXTERN void  issum_(IS is1,IS is2,IS *is3, int *__ierr)
{
*__ierr = ISSum(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),is3);
}
PETSC_EXTERN void  isexpand_(IS is1,IS is2,IS *isout, int *__ierr)
{
*__ierr = ISExpand(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),isout);
}
PETSC_EXTERN void  isintersect_(IS is1,IS is2,IS *isout, int *__ierr)
{
*__ierr = ISIntersect(
	(IS)PetscToPointer((is1) ),
	(IS)PetscToPointer((is2) ),isout);
}
PETSC_EXTERN void  isconcatenate_(MPI_Fint * comm,PetscInt *len, IS islist[],IS *isout, int *__ierr)
{
*__ierr = ISConcatenate(
	MPI_Comm_f2c(*(comm)),*len,islist,isout);
}
PETSC_EXTERN void  islisttopair_(MPI_Fint * comm,PetscInt *listlen,IS islist[],IS *xis,IS *yis, int *__ierr)
{
*__ierr = ISListToPair(
	MPI_Comm_f2c(*(comm)),*listlen,islist,xis,yis);
}
PETSC_EXTERN void  ispairtolist_(IS xis,IS yis,PetscInt *listlen,IS **islist, int *__ierr)
{
*__ierr = ISPairToList(
	(IS)PetscToPointer((xis) ),
	(IS)PetscToPointer((yis) ),listlen,islist);
}
PETSC_EXTERN void  isembed_(IS a,IS b,PetscBool *drop,IS *c, int *__ierr)
{
*__ierr = ISEmbed(
	(IS)PetscToPointer((a) ),
	(IS)PetscToPointer((b) ),*drop,c);
}
PETSC_EXTERN void  issortpermutation_(IS f,PetscBool *always,IS *h, int *__ierr)
{
*__ierr = ISSortPermutation(
	(IS)PetscToPointer((f) ),*always,h);
}
#if defined(__cplusplus)
}
#endif

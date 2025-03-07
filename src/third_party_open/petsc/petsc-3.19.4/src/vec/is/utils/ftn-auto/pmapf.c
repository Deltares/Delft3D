#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pmap.c */
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
#define petsclayoutcreate_ PETSCLAYOUTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutcreate_ petsclayoutcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutcreatefromsizes_ PETSCLAYOUTCREATEFROMSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutcreatefromsizes_ petsclayoutcreatefromsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutdestroy_ PETSCLAYOUTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutdestroy_ petsclayoutdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutcreatefromranges_ PETSCLAYOUTCREATEFROMRANGES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutcreatefromranges_ petsclayoutcreatefromranges
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutsetup_ PETSCLAYOUTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutsetup_ petsclayoutsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutduplicate_ PETSCLAYOUTDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutduplicate_ petsclayoutduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutreference_ PETSCLAYOUTREFERENCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutreference_ petsclayoutreference
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutsetislocaltoglobalmapping_ PETSCLAYOUTSETISLOCALTOGLOBALMAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutsetislocaltoglobalmapping_ petsclayoutsetislocaltoglobalmapping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutsetlocalsize_ PETSCLAYOUTSETLOCALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutsetlocalsize_ petsclayoutsetlocalsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutgetlocalsize_ PETSCLAYOUTGETLOCALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutgetlocalsize_ petsclayoutgetlocalsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutsetsize_ PETSCLAYOUTSETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutsetsize_ petsclayoutsetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutgetsize_ PETSCLAYOUTGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutgetsize_ petsclayoutgetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutsetblocksize_ PETSCLAYOUTSETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutsetblocksize_ petsclayoutsetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutgetblocksize_ PETSCLAYOUTGETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutgetblocksize_ petsclayoutgetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutgetrange_ PETSCLAYOUTGETRANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutgetrange_ petsclayoutgetrange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclayoutcompare_ PETSCLAYOUTCOMPARE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclayoutcompare_ petsclayoutcompare
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petsclayoutcreate_(MPI_Fint * comm,PetscLayout *map, int *__ierr)
{
*__ierr = PetscLayoutCreate(
	MPI_Comm_f2c(*(comm)),map);
}
PETSC_EXTERN void  petsclayoutcreatefromsizes_(MPI_Fint * comm,PetscInt *n,PetscInt *N,PetscInt *bs,PetscLayout *map, int *__ierr)
{
*__ierr = PetscLayoutCreateFromSizes(
	MPI_Comm_f2c(*(comm)),*n,*N,*bs,map);
}
PETSC_EXTERN void  petsclayoutdestroy_(PetscLayout *map, int *__ierr)
{
*__ierr = PetscLayoutDestroy(map);
}
PETSC_EXTERN void  petsclayoutcreatefromranges_(MPI_Fint * comm, PetscInt range[],PetscCopyMode *mode,PetscInt *bs,PetscLayout *newmap, int *__ierr)
{
*__ierr = PetscLayoutCreateFromRanges(
	MPI_Comm_f2c(*(comm)),range,*mode,*bs,newmap);
}
PETSC_EXTERN void  petsclayoutsetup_(PetscLayout map, int *__ierr)
{
*__ierr = PetscLayoutSetUp(
	(PetscLayout)PetscToPointer((map) ));
}
PETSC_EXTERN void  petsclayoutduplicate_(PetscLayout in,PetscLayout *out, int *__ierr)
{
*__ierr = PetscLayoutDuplicate(
	(PetscLayout)PetscToPointer((in) ),out);
}
PETSC_EXTERN void  petsclayoutreference_(PetscLayout in,PetscLayout *out, int *__ierr)
{
*__ierr = PetscLayoutReference(
	(PetscLayout)PetscToPointer((in) ),out);
}
PETSC_EXTERN void  petsclayoutsetislocaltoglobalmapping_(PetscLayout in,ISLocalToGlobalMapping ltog, int *__ierr)
{
*__ierr = PetscLayoutSetISLocalToGlobalMapping(
	(PetscLayout)PetscToPointer((in) ),
	(ISLocalToGlobalMapping)PetscToPointer((ltog) ));
}
PETSC_EXTERN void  petsclayoutsetlocalsize_(PetscLayout map,PetscInt *n, int *__ierr)
{
*__ierr = PetscLayoutSetLocalSize(
	(PetscLayout)PetscToPointer((map) ),*n);
}
PETSC_EXTERN void  petsclayoutgetlocalsize_(PetscLayout map,PetscInt *n, int *__ierr)
{
*__ierr = PetscLayoutGetLocalSize(
	(PetscLayout)PetscToPointer((map) ),n);
}
PETSC_EXTERN void  petsclayoutsetsize_(PetscLayout map,PetscInt *n, int *__ierr)
{
*__ierr = PetscLayoutSetSize(
	(PetscLayout)PetscToPointer((map) ),*n);
}
PETSC_EXTERN void  petsclayoutgetsize_(PetscLayout map,PetscInt *n, int *__ierr)
{
*__ierr = PetscLayoutGetSize(
	(PetscLayout)PetscToPointer((map) ),n);
}
PETSC_EXTERN void  petsclayoutsetblocksize_(PetscLayout map,PetscInt *bs, int *__ierr)
{
*__ierr = PetscLayoutSetBlockSize(
	(PetscLayout)PetscToPointer((map) ),*bs);
}
PETSC_EXTERN void  petsclayoutgetblocksize_(PetscLayout map,PetscInt *bs, int *__ierr)
{
*__ierr = PetscLayoutGetBlockSize(
	(PetscLayout)PetscToPointer((map) ),bs);
}
PETSC_EXTERN void  petsclayoutgetrange_(PetscLayout map,PetscInt *rstart,PetscInt *rend, int *__ierr)
{
*__ierr = PetscLayoutGetRange(
	(PetscLayout)PetscToPointer((map) ),rstart,rend);
}
PETSC_EXTERN void  petsclayoutcompare_(PetscLayout mapa,PetscLayout mapb,PetscBool *congruent, int *__ierr)
{
*__ierr = PetscLayoutCompare(
	(PetscLayout)PetscToPointer((mapa) ),
	(PetscLayout)PetscToPointer((mapb) ),congruent);
}
#if defined(__cplusplus)
}
#endif

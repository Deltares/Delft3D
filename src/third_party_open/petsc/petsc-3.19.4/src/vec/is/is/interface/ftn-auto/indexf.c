#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* index.c */
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
#define isrenumber_ ISRENUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isrenumber_ isrenumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscreatesubis_ ISCREATESUBIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscreatesubis_ iscreatesubis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isclearinfocache_ ISCLEARINFOCACHE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isclearinfocache_ isclearinfocache
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issetinfo_ ISSETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issetinfo_ issetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetinfo_ ISGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetinfo_ isgetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isidentity_ ISIDENTITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isidentity_ isidentity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issetidentity_ ISSETIDENTITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issetidentity_ issetidentity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscontiguouslocal_ ISCONTIGUOUSLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscontiguouslocal_ iscontiguouslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ispermutation_ ISPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ispermutation_ ispermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issetpermutation_ ISSETPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issetpermutation_ issetpermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isinvertpermutation_ ISINVERTPERMUTATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isinvertpermutation_ isinvertpermutation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetsize_ ISGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetsize_ isgetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetlocalsize_ ISGETLOCALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetlocalsize_ isgetlocalsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetlayout_ ISGETLAYOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetlayout_ isgetlayout
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issetlayout_ ISSETLAYOUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issetlayout_ issetlayout
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define islocate_ ISLOCATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define islocate_ islocate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetnonlocalis_ ISGETNONLOCALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetnonlocalis_ isgetnonlocalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isrestorenonlocalis_ ISRESTORENONLOCALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isrestorenonlocalis_ isrestorenonlocalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isload_ ISLOAD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isload_ isload
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issort_ ISSORT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issort_ issort
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issortremovedups_ ISSORTREMOVEDUPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issortremovedups_ issortremovedups
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define istogeneral_ ISTOGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define istogeneral_ istogeneral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issorted_ ISSORTED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issorted_ issorted
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isduplicate_ ISDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isduplicate_ isduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define iscopy_ ISCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define iscopy_ iscopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isshift_ ISSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isshift_ isshift
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isoncomm_ ISONCOMM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isoncomm_ isoncomm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define issetblocksize_ ISSETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define issetblocksize_ issetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define isgetblocksize_ ISGETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define isgetblocksize_ isgetblocksize
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  isrenumber_(IS subset,IS subset_mult,PetscInt *N,IS *subset_n, int *__ierr)
{
*__ierr = ISRenumber(
	(IS)PetscToPointer((subset) ),
	(IS)PetscToPointer((subset_mult) ),N,subset_n);
}
PETSC_EXTERN void  iscreatesubis_(IS is,IS comps,IS *subis, int *__ierr)
{
*__ierr = ISCreateSubIS(
	(IS)PetscToPointer((is) ),
	(IS)PetscToPointer((comps) ),subis);
}
PETSC_EXTERN void  isclearinfocache_(IS is,PetscBool *clear_permanent_local, int *__ierr)
{
*__ierr = ISClearInfoCache(
	(IS)PetscToPointer((is) ),*clear_permanent_local);
}
PETSC_EXTERN void  issetinfo_(IS is,ISInfo *info,ISInfoType *type,PetscBool *permanent,PetscBool *flg, int *__ierr)
{
*__ierr = ISSetInfo(
	(IS)PetscToPointer((is) ),*info,*type,*permanent,*flg);
}
PETSC_EXTERN void  isgetinfo_(IS is,ISInfo *info,ISInfoType *type,PetscBool *compute,PetscBool *flg, int *__ierr)
{
*__ierr = ISGetInfo(
	(IS)PetscToPointer((is) ),*info,*type,*compute,flg);
}
PETSC_EXTERN void  isidentity_(IS is,PetscBool *ident, int *__ierr)
{
*__ierr = ISIdentity(
	(IS)PetscToPointer((is) ),ident);
}
PETSC_EXTERN void  issetidentity_(IS is, int *__ierr)
{
*__ierr = ISSetIdentity(
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  iscontiguouslocal_(IS is,PetscInt *gstart,PetscInt *gend,PetscInt *start,PetscBool *contig, int *__ierr)
{
*__ierr = ISContiguousLocal(
	(IS)PetscToPointer((is) ),*gstart,*gend,start,contig);
}
PETSC_EXTERN void  ispermutation_(IS is,PetscBool *perm, int *__ierr)
{
*__ierr = ISPermutation(
	(IS)PetscToPointer((is) ),perm);
}
PETSC_EXTERN void  issetpermutation_(IS is, int *__ierr)
{
*__ierr = ISSetPermutation(
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  isinvertpermutation_(IS is,PetscInt *nlocal,IS *isout, int *__ierr)
{
*__ierr = ISInvertPermutation(
	(IS)PetscToPointer((is) ),*nlocal,isout);
}
PETSC_EXTERN void  isgetsize_(IS is,PetscInt *size, int *__ierr)
{
*__ierr = ISGetSize(
	(IS)PetscToPointer((is) ),size);
}
PETSC_EXTERN void  isgetlocalsize_(IS is,PetscInt *size, int *__ierr)
{
*__ierr = ISGetLocalSize(
	(IS)PetscToPointer((is) ),size);
}
PETSC_EXTERN void  isgetlayout_(IS is,PetscLayout *map, int *__ierr)
{
*__ierr = ISGetLayout(
	(IS)PetscToPointer((is) ),map);
}
PETSC_EXTERN void  issetlayout_(IS is,PetscLayout map, int *__ierr)
{
*__ierr = ISSetLayout(
	(IS)PetscToPointer((is) ),
	(PetscLayout)PetscToPointer((map) ));
}
PETSC_EXTERN void  islocate_(IS is,PetscInt *key,PetscInt *location, int *__ierr)
{
*__ierr = ISLocate(
	(IS)PetscToPointer((is) ),*key,location);
}
PETSC_EXTERN void  isgetnonlocalis_(IS is,IS *complement, int *__ierr)
{
*__ierr = ISGetNonlocalIS(
	(IS)PetscToPointer((is) ),complement);
}
PETSC_EXTERN void  isrestorenonlocalis_(IS is,IS *complement, int *__ierr)
{
*__ierr = ISRestoreNonlocalIS(
	(IS)PetscToPointer((is) ),complement);
}
PETSC_EXTERN void  isload_(IS is,PetscViewer viewer, int *__ierr)
{
*__ierr = ISLoad(
	(IS)PetscToPointer((is) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  issort_(IS is, int *__ierr)
{
*__ierr = ISSort(
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  issortremovedups_(IS is, int *__ierr)
{
*__ierr = ISSortRemoveDups(
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  istogeneral_(IS is, int *__ierr)
{
*__ierr = ISToGeneral(
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void  issorted_(IS is,PetscBool *flg, int *__ierr)
{
*__ierr = ISSorted(
	(IS)PetscToPointer((is) ),flg);
}
PETSC_EXTERN void  isduplicate_(IS is,IS *newIS, int *__ierr)
{
*__ierr = ISDuplicate(
	(IS)PetscToPointer((is) ),newIS);
}
PETSC_EXTERN void  iscopy_(IS is,IS isy, int *__ierr)
{
*__ierr = ISCopy(
	(IS)PetscToPointer((is) ),
	(IS)PetscToPointer((isy) ));
}
PETSC_EXTERN void  isshift_(IS is,PetscInt *offset,IS isy, int *__ierr)
{
*__ierr = ISShift(
	(IS)PetscToPointer((is) ),*offset,
	(IS)PetscToPointer((isy) ));
}
PETSC_EXTERN void  isoncomm_(IS is,MPI_Fint * comm,PetscCopyMode *mode,IS *newis, int *__ierr)
{
*__ierr = ISOnComm(
	(IS)PetscToPointer((is) ),
	MPI_Comm_f2c(*(comm)),*mode,newis);
}
PETSC_EXTERN void  issetblocksize_(IS is,PetscInt *bs, int *__ierr)
{
*__ierr = ISSetBlockSize(
	(IS)PetscToPointer((is) ),*bs);
}
PETSC_EXTERN void  isgetblocksize_(IS is,PetscInt *size, int *__ierr)
{
*__ierr = ISGetBlockSize(
	(IS)PetscToPointer((is) ),size);
}
#if defined(__cplusplus)
}
#endif

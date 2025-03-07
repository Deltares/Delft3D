#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sorti.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortedint_ PETSCSORTEDINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedint_ petscsortedint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortedint64_ PETSCSORTEDINT64
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedint64_ petscsortedint64
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortint_ PETSCSORTINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortint_ petscsortint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortint64_ PETSCSORTINT64
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortint64_ petscsortint64
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortcount_ PETSCSORTCOUNT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortcount_ petscsortcount
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortreverseint_ PETSCSORTREVERSEINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortreverseint_ petscsortreverseint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortedremovedupsint_ PETSCSORTEDREMOVEDUPSINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedremovedupsint_ petscsortedremovedupsint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortedcheckdupsint_ PETSCSORTEDCHECKDUPSINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedcheckdupsint_ petscsortedcheckdupsint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortremovedupsint_ PETSCSORTREMOVEDUPSINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortremovedupsint_ petscsortremovedupsint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfindint_ PETSCFINDINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfindint_ petscfindint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsccheckdupsint_ PETSCCHECKDUPSINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsccheckdupsint_ petsccheckdupsint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfindmpiint_ PETSCFINDMPIINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfindmpiint_ petscfindmpiint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwitharray_ PETSCSORTINTWITHARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwitharray_ petscsortintwitharray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwitharraypair_ PETSCSORTINTWITHARRAYPAIR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwitharraypair_ petscsortintwitharraypair
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwithcountarray_ PETSCSORTINTWITHCOUNTARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwithcountarray_ petscsortintwithcountarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwithintcountarraypair_ PETSCSORTINTWITHINTCOUNTARRAYPAIR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwithintcountarraypair_ petscsortintwithintcountarraypair
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortedmpiint_ PETSCSORTEDMPIINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortedmpiint_ petscsortedmpiint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortmpiint_ PETSCSORTMPIINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortmpiint_ petscsortmpiint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortremovedupsmpiint_ PETSCSORTREMOVEDUPSMPIINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortremovedupsmpiint_ petscsortremovedupsmpiint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortmpiintwitharray_ PETSCSORTMPIINTWITHARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortmpiintwitharray_ petscsortmpiintwitharray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortmpiintwithintarray_ PETSCSORTMPIINTWITHINTARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortmpiintwithintarray_ petscsortmpiintwithintarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsortintwithscalararray_ PETSCSORTINTWITHSCALARARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsortintwithscalararray_ petscsortintwithscalararray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmergeintarray_ PETSCMERGEINTARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmergeintarray_ petscmergeintarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmergeintarraypair_ PETSCMERGEINTARRAYPAIR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmergeintarraypair_ petscmergeintarraypair
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscmergempiintarray_ PETSCMERGEMPIINTARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscmergempiintarray_ petscmergempiintarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscparallelsortedint_ PETSCPARALLELSORTEDINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscparallelsortedint_ petscparallelsortedint
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsortedint_(PetscInt *n, PetscInt X[],PetscBool *sorted, int *__ierr)
{
*__ierr = PetscSortedInt(*n,X,sorted);
}
PETSC_EXTERN void  petscsortedint64_(PetscInt *n, PetscInt64 X[],PetscBool *sorted, int *__ierr)
{
*__ierr = PetscSortedInt64(*n,X,sorted);
}
PETSC_EXTERN void  petscsortint_(PetscInt *n,PetscInt X[], int *__ierr)
{
*__ierr = PetscSortInt(*n,X);
}
PETSC_EXTERN void  petscsortint64_(PetscInt *n,PetscInt64 X[], int *__ierr)
{
*__ierr = PetscSortInt64(*n,X);
}
PETSC_EXTERN void  petscsortcount_(PetscInt *n,PetscCount X[], int *__ierr)
{
*__ierr = PetscSortCount(*n,X);
}
PETSC_EXTERN void  petscsortreverseint_(PetscInt *n,PetscInt X[], int *__ierr)
{
*__ierr = PetscSortReverseInt(*n,X);
}
PETSC_EXTERN void  petscsortedremovedupsint_(PetscInt *n,PetscInt X[], int *__ierr)
{
*__ierr = PetscSortedRemoveDupsInt(n,X);
}
PETSC_EXTERN void  petscsortedcheckdupsint_(PetscInt *n, PetscInt X[],PetscBool *flg, int *__ierr)
{
*__ierr = PetscSortedCheckDupsInt(*n,X,flg);
}
PETSC_EXTERN void  petscsortremovedupsint_(PetscInt *n,PetscInt X[], int *__ierr)
{
*__ierr = PetscSortRemoveDupsInt(n,X);
}
PETSC_EXTERN void  petscfindint_(PetscInt *key,PetscInt *n, PetscInt X[],PetscInt *loc, int *__ierr)
{
*__ierr = PetscFindInt(*key,*n,X,loc);
}
PETSC_EXTERN void  petsccheckdupsint_(PetscInt *n, PetscInt X[],PetscBool *dups, int *__ierr)
{
*__ierr = PetscCheckDupsInt(*n,X,dups);
}
PETSC_EXTERN void  petscfindmpiint_(PetscMPIInt *key,PetscInt *n, PetscMPIInt X[],PetscInt *loc, int *__ierr)
{
*__ierr = PetscFindMPIInt(*key,*n,X,loc);
}
PETSC_EXTERN void  petscsortintwitharray_(PetscInt *n,PetscInt X[],PetscInt Y[], int *__ierr)
{
*__ierr = PetscSortIntWithArray(*n,X,Y);
}
PETSC_EXTERN void  petscsortintwitharraypair_(PetscInt *n,PetscInt X[],PetscInt Y[],PetscInt Z[], int *__ierr)
{
*__ierr = PetscSortIntWithArrayPair(*n,X,Y,Z);
}
PETSC_EXTERN void  petscsortintwithcountarray_(PetscCount *n,PetscInt X[],PetscCount Y[], int *__ierr)
{
*__ierr = PetscSortIntWithCountArray(*n,X,Y);
}
PETSC_EXTERN void  petscsortintwithintcountarraypair_(PetscCount *n,PetscInt X[],PetscInt Y[],PetscCount Z[], int *__ierr)
{
*__ierr = PetscSortIntWithIntCountArrayPair(*n,X,Y,Z);
}
PETSC_EXTERN void  petscsortedmpiint_(PetscInt *n, PetscMPIInt X[],PetscBool *sorted, int *__ierr)
{
*__ierr = PetscSortedMPIInt(*n,X,sorted);
}
PETSC_EXTERN void  petscsortmpiint_(PetscInt *n,PetscMPIInt X[], int *__ierr)
{
*__ierr = PetscSortMPIInt(*n,X);
}
PETSC_EXTERN void  petscsortremovedupsmpiint_(PetscInt *n,PetscMPIInt X[], int *__ierr)
{
*__ierr = PetscSortRemoveDupsMPIInt(n,X);
}
PETSC_EXTERN void  petscsortmpiintwitharray_(PetscMPIInt *n,PetscMPIInt X[],PetscMPIInt Y[], int *__ierr)
{
*__ierr = PetscSortMPIIntWithArray(*n,X,Y);
}
PETSC_EXTERN void  petscsortmpiintwithintarray_(PetscMPIInt *n,PetscMPIInt X[],PetscInt Y[], int *__ierr)
{
*__ierr = PetscSortMPIIntWithIntArray(*n,X,Y);
}
PETSC_EXTERN void  petscsortintwithscalararray_(PetscInt *n,PetscInt X[],PetscScalar Y[], int *__ierr)
{
*__ierr = PetscSortIntWithScalarArray(*n,X,Y);
}
PETSC_EXTERN void  petscmergeintarray_(PetscInt *an, PetscInt aI[],PetscInt *bn, PetscInt bI[],PetscInt *n,PetscInt **L, int *__ierr)
{
*__ierr = PetscMergeIntArray(*an,aI,*bn,bI,n,L);
}
PETSC_EXTERN void  petscmergeintarraypair_(PetscInt *an, PetscInt aI[], PetscInt aJ[],PetscInt *bn, PetscInt bI[], PetscInt bJ[],PetscInt *n,PetscInt **L,PetscInt **J, int *__ierr)
{
*__ierr = PetscMergeIntArrayPair(*an,aI,aJ,*bn,bI,bJ,n,L,J);
}
PETSC_EXTERN void  petscmergempiintarray_(PetscInt *an, PetscMPIInt aI[],PetscInt *bn, PetscMPIInt bI[],PetscInt *n,PetscMPIInt **L, int *__ierr)
{
*__ierr = PetscMergeMPIIntArray(*an,aI,*bn,bI,n,L);
}
PETSC_EXTERN void  petscparallelsortedint_(MPI_Fint * comm,PetscInt *n, PetscInt keys[],PetscBool *is_sorted, int *__ierr)
{
*__ierr = PetscParallelSortedInt(
	MPI_Comm_f2c(*(comm)),*n,keys,is_sorted);
}
#if defined(__cplusplus)
}
#endif

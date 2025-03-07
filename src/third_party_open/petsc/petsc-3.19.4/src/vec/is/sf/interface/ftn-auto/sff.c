#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* sf.c */
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

#include "petscsf.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfcreate_ PETSCSFCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfcreate_ petscsfcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfreset_ PETSCSFRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfreset_ petscsfreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetup_ PETSCSFSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetup_ petscsfsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetfromoptions_ PETSCSFSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetfromoptions_ petscsfsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetrankorder_ PETSCSFSETRANKORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetrankorder_ petscsfsetrankorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfsetgraphwithpattern_ PETSCSFSETGRAPHWITHPATTERN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetgraphwithpattern_ petscsfsetgraphwithpattern
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfcreateinversesf_ PETSCSFCREATEINVERSESF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfcreateinversesf_ petscsfcreateinversesf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfduplicate_ PETSCSFDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfduplicate_ petscsfduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfgetleafrange_ PETSCSFGETLEAFRANGE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfgetleafrange_ petscsfgetleafrange
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfgetmultisf_ PETSCSFGETMULTISF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfgetmultisf_ petscsfgetmultisf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfcompose_ PETSCSFCOMPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfcompose_ petscsfcompose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfcomposeinverse_ PETSCSFCOMPOSEINVERSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfcomposeinverse_ petscsfcomposeinverse
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsfconcatenate_ PETSCSFCONCATENATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfconcatenate_ petscsfconcatenate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsfcreate_(MPI_Fint * comm,PetscSF *sf, int *__ierr)
{
*__ierr = PetscSFCreate(
	MPI_Comm_f2c(*(comm)),sf);
}
PETSC_EXTERN void  petscsfreset_(PetscSF sf, int *__ierr)
{
*__ierr = PetscSFReset(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void  petscsfsetup_(PetscSF sf, int *__ierr)
{
*__ierr = PetscSFSetUp(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void  petscsfsetfromoptions_(PetscSF sf, int *__ierr)
{
*__ierr = PetscSFSetFromOptions(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void  petscsfsetrankorder_(PetscSF sf,PetscBool *flg, int *__ierr)
{
*__ierr = PetscSFSetRankOrder(
	(PetscSF)PetscToPointer((sf) ),*flg);
}
PETSC_EXTERN void  petscsfsetgraphwithpattern_(PetscSF sf,PetscLayout map,PetscSFPattern *pattern, int *__ierr)
{
*__ierr = PetscSFSetGraphWithPattern(
	(PetscSF)PetscToPointer((sf) ),
	(PetscLayout)PetscToPointer((map) ),*pattern);
}
PETSC_EXTERN void  petscsfcreateinversesf_(PetscSF sf,PetscSF *isf, int *__ierr)
{
*__ierr = PetscSFCreateInverseSF(
	(PetscSF)PetscToPointer((sf) ),isf);
}
PETSC_EXTERN void  petscsfduplicate_(PetscSF sf,PetscSFDuplicateOption *opt,PetscSF *newsf, int *__ierr)
{
*__ierr = PetscSFDuplicate(
	(PetscSF)PetscToPointer((sf) ),*opt,newsf);
}
PETSC_EXTERN void  petscsfgetleafrange_(PetscSF sf,PetscInt *minleaf,PetscInt *maxleaf, int *__ierr)
{
*__ierr = PetscSFGetLeafRange(
	(PetscSF)PetscToPointer((sf) ),minleaf,maxleaf);
}
PETSC_EXTERN void  petscsfgetmultisf_(PetscSF sf,PetscSF *multi, int *__ierr)
{
*__ierr = PetscSFGetMultiSF(
	(PetscSF)PetscToPointer((sf) ),multi);
}
PETSC_EXTERN void  petscsfcompose_(PetscSF sfA,PetscSF sfB,PetscSF *sfBA, int *__ierr)
{
*__ierr = PetscSFCompose(
	(PetscSF)PetscToPointer((sfA) ),
	(PetscSF)PetscToPointer((sfB) ),sfBA);
}
PETSC_EXTERN void  petscsfcomposeinverse_(PetscSF sfA,PetscSF sfB,PetscSF *sfBA, int *__ierr)
{
*__ierr = PetscSFComposeInverse(
	(PetscSF)PetscToPointer((sfA) ),
	(PetscSF)PetscToPointer((sfB) ),sfBA);
}
PETSC_EXTERN void  petscsfconcatenate_(MPI_Fint * comm,PetscInt *nsfs,PetscSF sfs[],PetscSFConcatenateRootMode *rootMode,PetscInt leafOffsets[],PetscSF *newsf, int *__ierr)
{
*__ierr = PetscSFConcatenate(
	MPI_Comm_f2c(*(comm)),*nsfs,sfs,*rootMode,leafOffsets,newsf);
}
#if defined(__cplusplus)
}
#endif

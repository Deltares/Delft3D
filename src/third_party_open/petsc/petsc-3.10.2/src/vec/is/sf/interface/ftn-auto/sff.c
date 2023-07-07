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
#define petscsfdestroy_ PETSCSFDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfdestroy_ petscsfdestroy
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
#define petscsfsetgraph_ PETSCSFSETGRAPH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsfsetgraph_ petscsfsetgraph
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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscsfcreate_(MPI_Fint * comm,PetscSF *sf, int *__ierr){
*__ierr = PetscSFCreate(
	MPI_Comm_f2c(*(comm)),sf);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfreset_(PetscSF sf, int *__ierr){
*__ierr = PetscSFReset(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsfdestroy_(PetscSF *sf, int *__ierr){
*__ierr = PetscSFDestroy(sf);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfsetup_(PetscSF sf, int *__ierr){
*__ierr = PetscSFSetUp(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsfsetfromoptions_(PetscSF sf, int *__ierr){
*__ierr = PetscSFSetFromOptions(
	(PetscSF)PetscToPointer((sf) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscsfsetrankorder_(PetscSF sf,PetscBool *flg, int *__ierr){
*__ierr = PetscSFSetRankOrder(
	(PetscSF)PetscToPointer((sf) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfsetgraph_(PetscSF sf,PetscInt *nroots,PetscInt *nleaves, PetscInt *ilocal,PetscCopyMode *localmode, PetscSFNode *iremote,PetscCopyMode *remotemode, int *__ierr){
*__ierr = PetscSFSetGraph(
	(PetscSF)PetscToPointer((sf) ),*nroots,*nleaves,ilocal,*localmode,iremote,*remotemode);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfcreateinversesf_(PetscSF sf,PetscSF *isf, int *__ierr){
*__ierr = PetscSFCreateInverseSF(
	(PetscSF)PetscToPointer((sf) ),isf);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfduplicate_(PetscSF sf,PetscSFDuplicateOption *opt,PetscSF *newsf, int *__ierr){
*__ierr = PetscSFDuplicate(
	(PetscSF)PetscToPointer((sf) ),*opt,newsf);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfgetleafrange_(PetscSF sf,PetscInt *minleaf,PetscInt *maxleaf, int *__ierr){
*__ierr = PetscSFGetLeafRange(
	(PetscSF)PetscToPointer((sf) ),minleaf,maxleaf);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfgetmultisf_(PetscSF sf,PetscSF *multi, int *__ierr){
*__ierr = PetscSFGetMultiSF(
	(PetscSF)PetscToPointer((sf) ),multi);
}
PETSC_EXTERN void PETSC_STDCALL  petscsfcompose_(PetscSF sfA,PetscSF sfB,PetscSF *sfBA, int *__ierr){
*__ierr = PetscSFCompose(
	(PetscSF)PetscToPointer((sfA) ),
	(PetscSF)PetscToPointer((sfB) ),sfBA);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dtds.c */
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

#include "petscds.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetfromoptions_ PETSCDSSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetfromoptions_ petscdssetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsdestroy_ PETSCDSDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsdestroy_ petscdsdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscreate_ PETSCDSCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscreate_ petscdscreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetnumfields_ PETSCDSGETNUMFIELDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetnumfields_ petscdsgetnumfields
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetspatialdimension_ PETSCDSGETSPATIALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetspatialdimension_ petscdsgetspatialdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcoordinatedimension_ PETSCDSGETCOORDINATEDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcoordinatedimension_ petscdsgetcoordinatedimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetcoordinatedimension_ PETSCDSSETCOORDINATEDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetcoordinatedimension_ petscdssetcoordinatedimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgettotaldimension_ PETSCDSGETTOTALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgettotaldimension_ petscdsgettotaldimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgettotalcomponents_ PETSCDSGETTOTALCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgettotalcomponents_ petscdsgettotalcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetdiscretization_ PETSCDSGETDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetdiscretization_ petscdsgetdiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetdiscretization_ PETSCDSSETDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetdiscretization_ petscdssetdiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsadddiscretization_ PETSCDSADDDISCRETIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsadddiscretization_ petscdsadddiscretization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetimplicit_ PETSCDSGETIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetimplicit_ petscdsgetimplicit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetimplicit_ PETSCDSSETIMPLICIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetimplicit_ petscdssetimplicit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetadjacency_ PETSCDSGETADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetadjacency_ petscdsgetadjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdssetadjacency_ PETSCDSSETADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdssetadjacency_ petscdssetadjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldindex_ PETSCDSGETFIELDINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldindex_ petscdsgetfieldindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldsize_ PETSCDSGETFIELDSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldsize_ petscdsgetfieldsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetfieldoffset_ PETSCDSGETFIELDOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetfieldoffset_ petscdsgetfieldoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetdimensions_ PETSCDSGETDIMENSIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetdimensions_ petscdsgetdimensions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponents_ PETSCDSGETCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponents_ petscdsgetcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentoffset_ PETSCDSGETCOMPONENTOFFSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentoffset_ petscdsgetcomponentoffset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentoffsets_ PETSCDSGETCOMPONENTOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentoffsets_ petscdsgetcomponentoffsets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetcomponentderivativeoffsets_ PETSCDSGETCOMPONENTDERIVATIVEOFFSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetcomponentderivativeoffsets_ petscdsgetcomponentderivativeoffsets
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdsgetnumboundary_ PETSCDSGETNUMBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdsgetnumboundary_ petscdsgetnumboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyboundary_ PETSCDSCOPYBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyboundary_ petscdscopyboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyequations_ PETSCDSCOPYEQUATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyequations_ petscdscopyequations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdscopyconstants_ PETSCDSCOPYCONSTANTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdscopyconstants_ petscdscopyconstants
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscdssetfromoptions_(PetscDS prob, int *__ierr){
*__ierr = PetscDSSetFromOptions(
	(PetscDS)PetscToPointer((prob) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdsdestroy_(PetscDS *prob, int *__ierr){
*__ierr = PetscDSDestroy(prob);
}
PETSC_EXTERN void PETSC_STDCALL  petscdscreate_(MPI_Fint * comm,PetscDS *prob, int *__ierr){
*__ierr = PetscDSCreate(
	MPI_Comm_f2c(*(comm)),prob);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetnumfields_(PetscDS prob,PetscInt *Nf, int *__ierr){
*__ierr = PetscDSGetNumFields(
	(PetscDS)PetscToPointer((prob) ),Nf);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetspatialdimension_(PetscDS prob,PetscInt *dim, int *__ierr){
*__ierr = PetscDSGetSpatialDimension(
	(PetscDS)PetscToPointer((prob) ),dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetcoordinatedimension_(PetscDS prob,PetscInt *dimEmbed, int *__ierr){
*__ierr = PetscDSGetCoordinateDimension(
	(PetscDS)PetscToPointer((prob) ),dimEmbed);
}
PETSC_EXTERN void PETSC_STDCALL  petscdssetcoordinatedimension_(PetscDS prob,PetscInt *dimEmbed, int *__ierr){
*__ierr = PetscDSSetCoordinateDimension(
	(PetscDS)PetscToPointer((prob) ),*dimEmbed);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgettotaldimension_(PetscDS prob,PetscInt *dim, int *__ierr){
*__ierr = PetscDSGetTotalDimension(
	(PetscDS)PetscToPointer((prob) ),dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgettotalcomponents_(PetscDS prob,PetscInt *Nc, int *__ierr){
*__ierr = PetscDSGetTotalComponents(
	(PetscDS)PetscToPointer((prob) ),Nc);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetdiscretization_(PetscDS prob,PetscInt *f,PetscObject *disc, int *__ierr){
*__ierr = PetscDSGetDiscretization(
	(PetscDS)PetscToPointer((prob) ),*f,disc);
}
PETSC_EXTERN void PETSC_STDCALL  petscdssetdiscretization_(PetscDS prob,PetscInt *f,PetscObject disc, int *__ierr){
*__ierr = PetscDSSetDiscretization(
	(PetscDS)PetscToPointer((prob) ),*f,
	(PetscObject)PetscToPointer((disc) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdsadddiscretization_(PetscDS prob,PetscObject disc, int *__ierr){
*__ierr = PetscDSAddDiscretization(
	(PetscDS)PetscToPointer((prob) ),
	(PetscObject)PetscToPointer((disc) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetimplicit_(PetscDS prob,PetscInt *f,PetscBool *implicit, int *__ierr){
*__ierr = PetscDSGetImplicit(
	(PetscDS)PetscToPointer((prob) ),*f,implicit);
}
PETSC_EXTERN void PETSC_STDCALL  petscdssetimplicit_(PetscDS prob,PetscInt *f,PetscBool *implicit, int *__ierr){
*__ierr = PetscDSSetImplicit(
	(PetscDS)PetscToPointer((prob) ),*f,*implicit);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetadjacency_(PetscDS prob,PetscInt *f,PetscBool *useCone,PetscBool *useClosure, int *__ierr){
*__ierr = PetscDSGetAdjacency(
	(PetscDS)PetscToPointer((prob) ),*f,useCone,useClosure);
}
PETSC_EXTERN void PETSC_STDCALL  petscdssetadjacency_(PetscDS prob,PetscInt *f,PetscBool *useCone,PetscBool *useClosure, int *__ierr){
*__ierr = PetscDSSetAdjacency(
	(PetscDS)PetscToPointer((prob) ),*f,*useCone,*useClosure);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetfieldindex_(PetscDS prob,PetscObject disc,PetscInt *f, int *__ierr){
*__ierr = PetscDSGetFieldIndex(
	(PetscDS)PetscToPointer((prob) ),
	(PetscObject)PetscToPointer((disc) ),f);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetfieldsize_(PetscDS prob,PetscInt *f,PetscInt *size, int *__ierr){
*__ierr = PetscDSGetFieldSize(
	(PetscDS)PetscToPointer((prob) ),*f,size);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetfieldoffset_(PetscDS prob,PetscInt *f,PetscInt *off, int *__ierr){
*__ierr = PetscDSGetFieldOffset(
	(PetscDS)PetscToPointer((prob) ),*f,off);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetdimensions_(PetscDS prob,PetscInt *dimensions[], int *__ierr){
*__ierr = PetscDSGetDimensions(
	(PetscDS)PetscToPointer((prob) ),dimensions);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetcomponents_(PetscDS prob,PetscInt *components[], int *__ierr){
*__ierr = PetscDSGetComponents(
	(PetscDS)PetscToPointer((prob) ),components);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetcomponentoffset_(PetscDS prob,PetscInt *f,PetscInt *off, int *__ierr){
*__ierr = PetscDSGetComponentOffset(
	(PetscDS)PetscToPointer((prob) ),*f,off);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetcomponentoffsets_(PetscDS prob,PetscInt *offsets[], int *__ierr){
*__ierr = PetscDSGetComponentOffsets(
	(PetscDS)PetscToPointer((prob) ),offsets);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetcomponentderivativeoffsets_(PetscDS prob,PetscInt *offsets[], int *__ierr){
*__ierr = PetscDSGetComponentDerivativeOffsets(
	(PetscDS)PetscToPointer((prob) ),offsets);
}
PETSC_EXTERN void PETSC_STDCALL  petscdsgetnumboundary_(PetscDS ds,PetscInt *numBd, int *__ierr){
*__ierr = PetscDSGetNumBoundary(
	(PetscDS)PetscToPointer((ds) ),numBd);
}
PETSC_EXTERN void PETSC_STDCALL  petscdscopyboundary_(PetscDS probA,PetscDS probB, int *__ierr){
*__ierr = PetscDSCopyBoundary(
	(PetscDS)PetscToPointer((probA) ),
	(PetscDS)PetscToPointer((probB) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdscopyequations_(PetscDS prob,PetscDS newprob, int *__ierr){
*__ierr = PetscDSCopyEquations(
	(PetscDS)PetscToPointer((prob) ),
	(PetscDS)PetscToPointer((newprob) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscdscopyconstants_(PetscDS prob,PetscDS newprob, int *__ierr){
*__ierr = PetscDSCopyConstants(
	(PetscDS)PetscToPointer((prob) ),
	(PetscDS)PetscToPointer((newprob) ));
}
#if defined(__cplusplus)
}
#endif

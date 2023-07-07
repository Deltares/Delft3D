#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmadapt.c */
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

#include "petscdmadaptor.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorcreate_ DMADAPTORCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorcreate_ dmadaptorcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptordestroy_ DMADAPTORDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptordestroy_ dmadaptordestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorsetfromoptions_ DMADAPTORSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorsetfromoptions_ dmadaptorsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorview_ DMADAPTORVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorview_ dmadaptorview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorgetsolver_ DMADAPTORGETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorgetsolver_ dmadaptorgetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorsetsolver_ DMADAPTORSETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorsetsolver_ dmadaptorsetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorgetsequencelength_ DMADAPTORGETSEQUENCELENGTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorgetsequencelength_ dmadaptorgetsequencelength
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorsetsequencelength_ DMADAPTORSETSEQUENCELENGTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorsetsequencelength_ dmadaptorsetsequencelength
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptorsetup_ DMADAPTORSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptorsetup_ dmadaptorsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmadaptoradapt_ DMADAPTORADAPT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmadaptoradapt_ dmadaptoradapt
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmadaptorcreate_(MPI_Fint * comm,DMAdaptor *adaptor, int *__ierr){
*__ierr = DMAdaptorCreate(
	MPI_Comm_f2c(*(comm)),
	(DMAdaptor* )PetscToPointer((adaptor) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptordestroy_(DMAdaptor *adaptor, int *__ierr){
*__ierr = DMAdaptorDestroy(
	(DMAdaptor* )PetscToPointer((adaptor) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorsetfromoptions_(DMAdaptor *adaptor, int *__ierr){
*__ierr = DMAdaptorSetFromOptions(*adaptor);
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorview_(DMAdaptor *adaptor,PetscViewer viewer, int *__ierr){
*__ierr = DMAdaptorView(*adaptor,
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorgetsolver_(DMAdaptor *adaptor,SNES *snes, int *__ierr){
*__ierr = DMAdaptorGetSolver(*adaptor,snes);
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorsetsolver_(DMAdaptor *adaptor,SNES snes, int *__ierr){
*__ierr = DMAdaptorSetSolver(*adaptor,
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorgetsequencelength_(DMAdaptor *adaptor,PetscInt *num, int *__ierr){
*__ierr = DMAdaptorGetSequenceLength(*adaptor,num);
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorsetsequencelength_(DMAdaptor *adaptor,PetscInt *num, int *__ierr){
*__ierr = DMAdaptorSetSequenceLength(*adaptor,*num);
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptorsetup_(DMAdaptor *adaptor, int *__ierr){
*__ierr = DMAdaptorSetUp(*adaptor);
}
PETSC_EXTERN void PETSC_STDCALL  dmadaptoradapt_(DMAdaptor *adaptor,Vec x,DMAdaptationStrategy *strategy,DM *adm,Vec *ax, int *__ierr){
*__ierr = DMAdaptorAdapt(*adaptor,
	(Vec)PetscToPointer((x) ),*strategy,adm,ax);
}
#if defined(__cplusplus)
}
#endif

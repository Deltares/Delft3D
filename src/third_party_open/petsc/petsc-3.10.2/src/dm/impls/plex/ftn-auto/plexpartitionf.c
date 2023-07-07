#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexpartition.c */
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

#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionersetfromoptions_ PETSCPARTITIONERSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionersetfromoptions_ petscpartitionersetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerdestroy_ PETSCPARTITIONERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerdestroy_ petscpartitionerdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionercreate_ PETSCPARTITIONERCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionercreate_ petscpartitionercreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionerpartition_ PETSCPARTITIONERPARTITION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerpartition_ petscpartitionerpartition
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionershellsetrandom_ PETSCPARTITIONERSHELLSETRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionershellsetrandom_ petscpartitionershellsetrandom
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscpartitionershellgetrandom_ PETSCPARTITIONERSHELLGETRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionershellgetrandom_ petscpartitionershellgetrandom
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpartitioner_ DMPLEXGETPARTITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpartitioner_ dmplexgetpartitioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetpartitioner_ DMPLEXSETPARTITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetpartitioner_ dmplexsetpartitioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpartitionlabelclosure_ DMPLEXPARTITIONLABELCLOSURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpartitionlabelclosure_ dmplexpartitionlabelclosure
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpartitionlabeladjacency_ DMPLEXPARTITIONLABELADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpartitionlabeladjacency_ dmplexpartitionlabeladjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpartitionlabelpropagate_ DMPLEXPARTITIONLABELPROPAGATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpartitionlabelpropagate_ dmplexpartitionlabelpropagate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpartitionlabelinvert_ DMPLEXPARTITIONLABELINVERT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpartitionlabelinvert_ dmplexpartitionlabelinvert
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexpartitionlabelcreatesf_ DMPLEXPARTITIONLABELCREATESF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexpartitionlabelcreatesf_ dmplexpartitionlabelcreatesf
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscpartitionersetfromoptions_(PetscPartitioner *part, int *__ierr){
*__ierr = PetscPartitionerSetFromOptions(*part);
}
PETSC_EXTERN void PETSC_STDCALL  petscpartitionerdestroy_(PetscPartitioner *part, int *__ierr){
*__ierr = PetscPartitionerDestroy(
	(PetscPartitioner* )PetscToPointer((part) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscpartitionercreate_(MPI_Fint * comm,PetscPartitioner *part, int *__ierr){
*__ierr = PetscPartitionerCreate(
	MPI_Comm_f2c(*(comm)),
	(PetscPartitioner* )PetscToPointer((part) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscpartitionerpartition_(PetscPartitioner *part,DM dm,PetscSection partSection,IS *partition, int *__ierr){
*__ierr = PetscPartitionerPartition(*part,
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((partSection) ),partition);
}
PETSC_EXTERN void PETSC_STDCALL  petscpartitionershellsetrandom_(PetscPartitioner *part,PetscBool *random, int *__ierr){
*__ierr = PetscPartitionerShellSetRandom(*part,*random);
}
PETSC_EXTERN void PETSC_STDCALL  petscpartitionershellgetrandom_(PetscPartitioner *part,PetscBool *random, int *__ierr){
*__ierr = PetscPartitionerShellGetRandom(*part,random);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexgetpartitioner_(DM dm,PetscPartitioner *part, int *__ierr){
*__ierr = DMPlexGetPartitioner(
	(DM)PetscToPointer((dm) ),
	(PetscPartitioner* )PetscToPointer((part) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexsetpartitioner_(DM dm,PetscPartitioner *part, int *__ierr){
*__ierr = DMPlexSetPartitioner(
	(DM)PetscToPointer((dm) ),*part);
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpartitionlabelclosure_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexPartitionLabelClosure(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpartitionlabeladjacency_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexPartitionLabelAdjacency(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpartitionlabelpropagate_(DM dm,DMLabel label, int *__ierr){
*__ierr = DMPlexPartitionLabelPropagate(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpartitionlabelinvert_(DM dm,DMLabel rootLabel,PetscSF processSF,DMLabel leafLabel, int *__ierr){
*__ierr = DMPlexPartitionLabelInvert(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((rootLabel) ),
	(PetscSF)PetscToPointer((processSF) ),
	(DMLabel)PetscToPointer((leafLabel) ));
}
PETSC_EXTERN void PETSC_STDCALL  dmplexpartitionlabelcreatesf_(DM dm,DMLabel label,PetscSF *sf, int *__ierr){
*__ierr = DMPlexPartitionLabelCreateSF(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),sf);
}
#if defined(__cplusplus)
}
#endif

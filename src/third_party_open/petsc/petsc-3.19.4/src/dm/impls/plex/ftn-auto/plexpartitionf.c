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
#define petscpartitionerdmplexpartition_ PETSCPARTITIONERDMPLEXPARTITION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscpartitionerdmplexpartition_ petscpartitionerdmplexpartition
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
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexrebalancesharedpoints_ DMPLEXREBALANCESHAREDPOINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexrebalancesharedpoints_ dmplexrebalancesharedpoints
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscpartitionerdmplexpartition_(PetscPartitioner part,DM dm,PetscSection targetSection,PetscSection partSection,IS *partition, int *__ierr)
{
*__ierr = PetscPartitionerDMPlexPartition(
	(PetscPartitioner)PetscToPointer((part) ),
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((targetSection) ),
	(PetscSection)PetscToPointer((partSection) ),partition);
}
PETSC_EXTERN void  dmplexgetpartitioner_(DM dm,PetscPartitioner *part, int *__ierr)
{
*__ierr = DMPlexGetPartitioner(
	(DM)PetscToPointer((dm) ),part);
}
PETSC_EXTERN void  dmplexsetpartitioner_(DM dm,PetscPartitioner part, int *__ierr)
{
*__ierr = DMPlexSetPartitioner(
	(DM)PetscToPointer((dm) ),
	(PetscPartitioner)PetscToPointer((part) ));
}
PETSC_EXTERN void  dmplexpartitionlabelclosure_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexPartitionLabelClosure(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexpartitionlabeladjacency_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexPartitionLabelAdjacency(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexpartitionlabelpropagate_(DM dm,DMLabel label, int *__ierr)
{
*__ierr = DMPlexPartitionLabelPropagate(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ));
}
PETSC_EXTERN void  dmplexpartitionlabelinvert_(DM dm,DMLabel rootLabel,PetscSF processSF,DMLabel leafLabel, int *__ierr)
{
*__ierr = DMPlexPartitionLabelInvert(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((rootLabel) ),
	(PetscSF)PetscToPointer((processSF) ),
	(DMLabel)PetscToPointer((leafLabel) ));
}
PETSC_EXTERN void  dmplexpartitionlabelcreatesf_(DM dm,DMLabel label,PetscSF *sf, int *__ierr)
{
*__ierr = DMPlexPartitionLabelCreateSF(
	(DM)PetscToPointer((dm) ),
	(DMLabel)PetscToPointer((label) ),sf);
}
PETSC_EXTERN void  dmplexrebalancesharedpoints_(DM dm,PetscInt *entityDepth,PetscBool *useInitialGuess,PetscBool *parallel,PetscBool *success, int *__ierr)
{
*__ierr = DMPlexRebalanceSharedPoints(
	(DM)PetscToPointer((dm) ),*entityDepth,*useInitialGuess,*parallel,success);
}
#if defined(__cplusplus)
}
#endif

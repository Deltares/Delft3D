#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexdistribute.c */
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
#include "petscdmlabel.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetadjacencyuseanchors_ DMPLEXSETADJACENCYUSEANCHORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetadjacencyuseanchors_ dmplexsetadjacencyuseanchors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetadjacencyuseanchors_ DMPLEXGETADJACENCYUSEANCHORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetadjacencyuseanchors_ dmplexgetadjacencyuseanchors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetadjacency_ DMPLEXGETADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetadjacency_ dmplexgetadjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatetwosidedprocesssf_ DMPLEXCREATETWOSIDEDPROCESSSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatetwosidedprocesssf_ dmplexcreatetwosidedprocesssf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributeownership_ DMPLEXDISTRIBUTEOWNERSHIP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributeownership_ dmplexdistributeownership
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexstratifymigrationsf_ DMPLEXSTRATIFYMIGRATIONSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexstratifymigrationsf_ dmplexstratifymigrationsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributefield_ DMPLEXDISTRIBUTEFIELD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributefield_ dmplexdistributefield
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributefieldis_ DMPLEXDISTRIBUTEFIELDIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributefieldis_ dmplexdistributefieldis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributedata_ DMPLEXDISTRIBUTEDATA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributedata_ dmplexdistributedata
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetpartitionbalance_ DMPLEXSETPARTITIONBALANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetpartitionbalance_ dmplexsetpartitionbalance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetpartitionbalance_ DMPLEXGETPARTITIONBALANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetpartitionbalance_ dmplexgetpartitionbalance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetoverlap_ DMPLEXGETOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetoverlap_ dmplexgetoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetoverlap_ DMPLEXSETOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetoverlap_ dmplexsetoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributesetdefault_ DMPLEXDISTRIBUTESETDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributesetdefault_ dmplexdistributesetdefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexdistributegetdefault_ DMPLEXDISTRIBUTEGETDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexdistributegetdefault_ dmplexdistributegetdefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexisdistributed_ DMPLEXISDISTRIBUTED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexisdistributed_ dmplexisdistributed
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsetadjacencyuseanchors_(DM dm,PetscBool *useAnchors, int *__ierr)
{
*__ierr = DMPlexSetAdjacencyUseAnchors(
	(DM)PetscToPointer((dm) ),*useAnchors);
}
PETSC_EXTERN void  dmplexgetadjacencyuseanchors_(DM dm,PetscBool *useAnchors, int *__ierr)
{
*__ierr = DMPlexGetAdjacencyUseAnchors(
	(DM)PetscToPointer((dm) ),useAnchors);
}
PETSC_EXTERN void  dmplexgetadjacency_(DM dm,PetscInt *p,PetscInt *adjSize,PetscInt *adj[], int *__ierr)
{
*__ierr = DMPlexGetAdjacency(
	(DM)PetscToPointer((dm) ),*p,adjSize,adj);
}
PETSC_EXTERN void  dmplexcreatetwosidedprocesssf_(DM dm,PetscSF sfPoint,PetscSection rootRankSection,IS rootRanks,PetscSection leafRankSection,IS leafRanks,IS *processRanks,PetscSF *sfProcess, int *__ierr)
{
*__ierr = DMPlexCreateTwoSidedProcessSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((sfPoint) ),
	(PetscSection)PetscToPointer((rootRankSection) ),
	(IS)PetscToPointer((rootRanks) ),
	(PetscSection)PetscToPointer((leafRankSection) ),
	(IS)PetscToPointer((leafRanks) ),processRanks,sfProcess);
}
PETSC_EXTERN void  dmplexdistributeownership_(DM dm,PetscSection rootSection,IS *rootrank,PetscSection leafSection,IS *leafrank, int *__ierr)
{
*__ierr = DMPlexDistributeOwnership(
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((rootSection) ),rootrank,
	(PetscSection)PetscToPointer((leafSection) ),leafrank);
}
PETSC_EXTERN void  dmplexstratifymigrationsf_(DM dm,PetscSF sf,PetscSF *migrationSF, int *__ierr)
{
*__ierr = DMPlexStratifyMigrationSF(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((sf) ),migrationSF);
}
PETSC_EXTERN void  dmplexdistributefield_(DM dm,PetscSF pointSF,PetscSection originalSection,Vec originalVec,PetscSection newSection,Vec newVec, int *__ierr)
{
*__ierr = DMPlexDistributeField(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((pointSF) ),
	(PetscSection)PetscToPointer((originalSection) ),
	(Vec)PetscToPointer((originalVec) ),
	(PetscSection)PetscToPointer((newSection) ),
	(Vec)PetscToPointer((newVec) ));
}
PETSC_EXTERN void  dmplexdistributefieldis_(DM dm,PetscSF pointSF,PetscSection originalSection,IS originalIS,PetscSection newSection,IS *newIS, int *__ierr)
{
*__ierr = DMPlexDistributeFieldIS(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((pointSF) ),
	(PetscSection)PetscToPointer((originalSection) ),
	(IS)PetscToPointer((originalIS) ),
	(PetscSection)PetscToPointer((newSection) ),newIS);
}
PETSC_EXTERN void  dmplexdistributedata_(DM dm,PetscSF pointSF,PetscSection originalSection,MPI_Fint * datatype,void*originalData,PetscSection newSection,void**newData, int *__ierr)
{
*__ierr = DMPlexDistributeData(
	(DM)PetscToPointer((dm) ),
	(PetscSF)PetscToPointer((pointSF) ),
	(PetscSection)PetscToPointer((originalSection) ),
	MPI_Type_f2c(*(datatype)),originalData,
	(PetscSection)PetscToPointer((newSection) ),newData);
}
PETSC_EXTERN void  dmplexsetpartitionbalance_(DM dm,PetscBool *flg, int *__ierr)
{
*__ierr = DMPlexSetPartitionBalance(
	(DM)PetscToPointer((dm) ),*flg);
}
PETSC_EXTERN void  dmplexgetpartitionbalance_(DM dm,PetscBool *flg, int *__ierr)
{
*__ierr = DMPlexGetPartitionBalance(
	(DM)PetscToPointer((dm) ),flg);
}
PETSC_EXTERN void  dmplexgetoverlap_(DM dm,PetscInt *overlap, int *__ierr)
{
*__ierr = DMPlexGetOverlap(
	(DM)PetscToPointer((dm) ),overlap);
}
PETSC_EXTERN void  dmplexsetoverlap_(DM dm,DM dmSrc,PetscInt *overlap, int *__ierr)
{
*__ierr = DMPlexSetOverlap(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((dmSrc) ),*overlap);
}
PETSC_EXTERN void  dmplexdistributesetdefault_(DM dm,PetscBool *dist, int *__ierr)
{
*__ierr = DMPlexDistributeSetDefault(
	(DM)PetscToPointer((dm) ),*dist);
}
PETSC_EXTERN void  dmplexdistributegetdefault_(DM dm,PetscBool *dist, int *__ierr)
{
*__ierr = DMPlexDistributeGetDefault(
	(DM)PetscToPointer((dm) ),dist);
}
PETSC_EXTERN void  dmplexisdistributed_(DM dm,PetscBool *distributed, int *__ierr)
{
*__ierr = DMPlexIsDistributed(
	(DM)PetscToPointer((dm) ),distributed);
}
#if defined(__cplusplus)
}
#endif

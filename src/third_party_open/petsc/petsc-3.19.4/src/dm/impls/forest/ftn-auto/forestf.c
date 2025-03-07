#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* forest.c */
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

#include "petscdmforest.h"
#include "petscdm.h"
#include "petscdmlabel.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmisforest_ DMISFOREST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmisforest_ dmisforest
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforesttemplate_ DMFORESTTEMPLATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforesttemplate_ dmforesttemplate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetbasedm_ DMFORESTSETBASEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetbasedm_ dmforestsetbasedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetbasedm_ DMFORESTGETBASEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetbasedm_ dmforestgetbasedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetadaptivityforest_ DMFORESTSETADAPTIVITYFOREST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetadaptivityforest_ dmforestsetadaptivityforest
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadaptivityforest_ DMFORESTGETADAPTIVITYFOREST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadaptivityforest_ dmforestgetadaptivityforest
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetadaptivitypurpose_ DMFORESTSETADAPTIVITYPURPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetadaptivitypurpose_ dmforestsetadaptivitypurpose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadaptivitypurpose_ DMFORESTGETADAPTIVITYPURPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadaptivitypurpose_ dmforestgetadaptivitypurpose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetadjacencydimension_ DMFORESTSETADJACENCYDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetadjacencydimension_ dmforestsetadjacencydimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetadjacencycodimension_ DMFORESTSETADJACENCYCODIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetadjacencycodimension_ dmforestsetadjacencycodimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadjacencydimension_ DMFORESTGETADJACENCYDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadjacencydimension_ dmforestgetadjacencydimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadjacencycodimension_ DMFORESTGETADJACENCYCODIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadjacencycodimension_ dmforestgetadjacencycodimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetpartitionoverlap_ DMFORESTSETPARTITIONOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetpartitionoverlap_ dmforestsetpartitionoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetpartitionoverlap_ DMFORESTGETPARTITIONOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetpartitionoverlap_ dmforestgetpartitionoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetminimumrefinement_ DMFORESTSETMINIMUMREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetminimumrefinement_ dmforestsetminimumrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetminimumrefinement_ DMFORESTGETMINIMUMREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetminimumrefinement_ dmforestgetminimumrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetinitialrefinement_ DMFORESTSETINITIALREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetinitialrefinement_ dmforestsetinitialrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetinitialrefinement_ DMFORESTGETINITIALREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetinitialrefinement_ dmforestgetinitialrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetmaximumrefinement_ DMFORESTSETMAXIMUMREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetmaximumrefinement_ dmforestsetmaximumrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetmaximumrefinement_ DMFORESTGETMAXIMUMREFINEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetmaximumrefinement_ dmforestgetmaximumrefinement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadaptivitysuccess_ DMFORESTGETADAPTIVITYSUCCESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadaptivitysuccess_ dmforestgetadaptivitysuccess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetcomputeadaptivitysf_ DMFORESTSETCOMPUTEADAPTIVITYSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetcomputeadaptivitysf_ dmforestsetcomputeadaptivitysf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetcomputeadaptivitysf_ DMFORESTGETCOMPUTEADAPTIVITYSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetcomputeadaptivitysf_ dmforestgetcomputeadaptivitysf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetadaptivitysf_ DMFORESTGETADAPTIVITYSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetadaptivitysf_ dmforestgetadaptivitysf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetgradefactor_ DMFORESTSETGRADEFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetgradefactor_ dmforestsetgradefactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetgradefactor_ DMFORESTGETGRADEFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetgradefactor_ dmforestgetgradefactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetcellweightfactor_ DMFORESTSETCELLWEIGHTFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetcellweightfactor_ dmforestsetcellweightfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetcellweightfactor_ DMFORESTGETCELLWEIGHTFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetcellweightfactor_ dmforestgetcellweightfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetcellchart_ DMFORESTGETCELLCHART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetcellchart_ dmforestgetcellchart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetcellsf_ DMFORESTGETCELLSF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetcellsf_ dmforestgetcellsf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetcellweights_ DMFORESTSETCELLWEIGHTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetcellweights_ dmforestsetcellweights
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetcellweights_ DMFORESTGETCELLWEIGHTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetcellweights_ dmforestgetcellweights
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestsetweightcapacity_ DMFORESTSETWEIGHTCAPACITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestsetweightcapacity_ dmforestsetweightcapacity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmforestgetweightcapacity_ DMFORESTGETWEIGHTCAPACITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmforestgetweightcapacity_ dmforestgetweightcapacity
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmisforest_(DM dm,PetscBool *isForest, int *__ierr)
{
*__ierr = DMIsForest(
	(DM)PetscToPointer((dm) ),isForest);
}
PETSC_EXTERN void  dmforesttemplate_(DM dm,MPI_Fint * comm,DM *tdm, int *__ierr)
{
*__ierr = DMForestTemplate(
	(DM)PetscToPointer((dm) ),
	MPI_Comm_f2c(*(comm)),tdm);
}
PETSC_EXTERN void  dmforestsetbasedm_(DM dm,DM base, int *__ierr)
{
*__ierr = DMForestSetBaseDM(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((base) ));
}
PETSC_EXTERN void  dmforestgetbasedm_(DM dm,DM *base, int *__ierr)
{
*__ierr = DMForestGetBaseDM(
	(DM)PetscToPointer((dm) ),base);
}
PETSC_EXTERN void  dmforestsetadaptivityforest_(DM dm,DM adapt, int *__ierr)
{
*__ierr = DMForestSetAdaptivityForest(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((adapt) ));
}
PETSC_EXTERN void  dmforestgetadaptivityforest_(DM dm,DM *adapt, int *__ierr)
{
*__ierr = DMForestGetAdaptivityForest(
	(DM)PetscToPointer((dm) ),adapt);
}
PETSC_EXTERN void  dmforestsetadaptivitypurpose_(DM dm,DMAdaptFlag *purpose, int *__ierr)
{
*__ierr = DMForestSetAdaptivityPurpose(
	(DM)PetscToPointer((dm) ),*purpose);
}
PETSC_EXTERN void  dmforestgetadaptivitypurpose_(DM dm,DMAdaptFlag *purpose, int *__ierr)
{
*__ierr = DMForestGetAdaptivityPurpose(
	(DM)PetscToPointer((dm) ),purpose);
}
PETSC_EXTERN void  dmforestsetadjacencydimension_(DM dm,PetscInt *adjDim, int *__ierr)
{
*__ierr = DMForestSetAdjacencyDimension(
	(DM)PetscToPointer((dm) ),*adjDim);
}
PETSC_EXTERN void  dmforestsetadjacencycodimension_(DM dm,PetscInt *adjCodim, int *__ierr)
{
*__ierr = DMForestSetAdjacencyCodimension(
	(DM)PetscToPointer((dm) ),*adjCodim);
}
PETSC_EXTERN void  dmforestgetadjacencydimension_(DM dm,PetscInt *adjDim, int *__ierr)
{
*__ierr = DMForestGetAdjacencyDimension(
	(DM)PetscToPointer((dm) ),adjDim);
}
PETSC_EXTERN void  dmforestgetadjacencycodimension_(DM dm,PetscInt *adjCodim, int *__ierr)
{
*__ierr = DMForestGetAdjacencyCodimension(
	(DM)PetscToPointer((dm) ),adjCodim);
}
PETSC_EXTERN void  dmforestsetpartitionoverlap_(DM dm,PetscInt *overlap, int *__ierr)
{
*__ierr = DMForestSetPartitionOverlap(
	(DM)PetscToPointer((dm) ),*overlap);
}
PETSC_EXTERN void  dmforestgetpartitionoverlap_(DM dm,PetscInt *overlap, int *__ierr)
{
*__ierr = DMForestGetPartitionOverlap(
	(DM)PetscToPointer((dm) ),overlap);
}
PETSC_EXTERN void  dmforestsetminimumrefinement_(DM dm,PetscInt *minRefinement, int *__ierr)
{
*__ierr = DMForestSetMinimumRefinement(
	(DM)PetscToPointer((dm) ),*minRefinement);
}
PETSC_EXTERN void  dmforestgetminimumrefinement_(DM dm,PetscInt *minRefinement, int *__ierr)
{
*__ierr = DMForestGetMinimumRefinement(
	(DM)PetscToPointer((dm) ),minRefinement);
}
PETSC_EXTERN void  dmforestsetinitialrefinement_(DM dm,PetscInt *initRefinement, int *__ierr)
{
*__ierr = DMForestSetInitialRefinement(
	(DM)PetscToPointer((dm) ),*initRefinement);
}
PETSC_EXTERN void  dmforestgetinitialrefinement_(DM dm,PetscInt *initRefinement, int *__ierr)
{
*__ierr = DMForestGetInitialRefinement(
	(DM)PetscToPointer((dm) ),initRefinement);
}
PETSC_EXTERN void  dmforestsetmaximumrefinement_(DM dm,PetscInt *maxRefinement, int *__ierr)
{
*__ierr = DMForestSetMaximumRefinement(
	(DM)PetscToPointer((dm) ),*maxRefinement);
}
PETSC_EXTERN void  dmforestgetmaximumrefinement_(DM dm,PetscInt *maxRefinement, int *__ierr)
{
*__ierr = DMForestGetMaximumRefinement(
	(DM)PetscToPointer((dm) ),maxRefinement);
}
PETSC_EXTERN void  dmforestgetadaptivitysuccess_(DM dm,PetscBool *success, int *__ierr)
{
*__ierr = DMForestGetAdaptivitySuccess(
	(DM)PetscToPointer((dm) ),success);
}
PETSC_EXTERN void  dmforestsetcomputeadaptivitysf_(DM dm,PetscBool *computeSF, int *__ierr)
{
*__ierr = DMForestSetComputeAdaptivitySF(
	(DM)PetscToPointer((dm) ),*computeSF);
}
PETSC_EXTERN void  dmforestgetcomputeadaptivitysf_(DM dm,PetscBool *computeSF, int *__ierr)
{
*__ierr = DMForestGetComputeAdaptivitySF(
	(DM)PetscToPointer((dm) ),computeSF);
}
PETSC_EXTERN void  dmforestgetadaptivitysf_(DM dm,PetscSF *preCoarseToFine,PetscSF *coarseToPreFine, int *__ierr)
{
*__ierr = DMForestGetAdaptivitySF(
	(DM)PetscToPointer((dm) ),preCoarseToFine,coarseToPreFine);
}
PETSC_EXTERN void  dmforestsetgradefactor_(DM dm,PetscInt *grade, int *__ierr)
{
*__ierr = DMForestSetGradeFactor(
	(DM)PetscToPointer((dm) ),*grade);
}
PETSC_EXTERN void  dmforestgetgradefactor_(DM dm,PetscInt *grade, int *__ierr)
{
*__ierr = DMForestGetGradeFactor(
	(DM)PetscToPointer((dm) ),grade);
}
PETSC_EXTERN void  dmforestsetcellweightfactor_(DM dm,PetscReal *weightsFactor, int *__ierr)
{
*__ierr = DMForestSetCellWeightFactor(
	(DM)PetscToPointer((dm) ),*weightsFactor);
}
PETSC_EXTERN void  dmforestgetcellweightfactor_(DM dm,PetscReal *weightsFactor, int *__ierr)
{
*__ierr = DMForestGetCellWeightFactor(
	(DM)PetscToPointer((dm) ),weightsFactor);
}
PETSC_EXTERN void  dmforestgetcellchart_(DM dm,PetscInt *cStart,PetscInt *cEnd, int *__ierr)
{
*__ierr = DMForestGetCellChart(
	(DM)PetscToPointer((dm) ),cStart,cEnd);
}
PETSC_EXTERN void  dmforestgetcellsf_(DM dm,PetscSF *cellSF, int *__ierr)
{
*__ierr = DMForestGetCellSF(
	(DM)PetscToPointer((dm) ),cellSF);
}
PETSC_EXTERN void  dmforestsetcellweights_(DM dm,PetscReal weights[],PetscCopyMode *copyMode, int *__ierr)
{
*__ierr = DMForestSetCellWeights(
	(DM)PetscToPointer((dm) ),weights,*copyMode);
}
PETSC_EXTERN void  dmforestgetcellweights_(DM dm,PetscReal **weights, int *__ierr)
{
*__ierr = DMForestGetCellWeights(
	(DM)PetscToPointer((dm) ),weights);
}
PETSC_EXTERN void  dmforestsetweightcapacity_(DM dm,PetscReal *capacity, int *__ierr)
{
*__ierr = DMForestSetWeightCapacity(
	(DM)PetscToPointer((dm) ),*capacity);
}
PETSC_EXTERN void  dmforestgetweightcapacity_(DM dm,PetscReal *capacity, int *__ierr)
{
*__ierr = DMForestGetWeightCapacity(
	(DM)PetscToPointer((dm) ),capacity);
}
#if defined(__cplusplus)
}
#endif

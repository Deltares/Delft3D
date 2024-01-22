#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plextree.c */
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
#define dmplexsetreferencetree_ DMPLEXSETREFERENCETREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetreferencetree_ dmplexsetreferencetree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgetreferencetree_ DMPLEXGETREFERENCETREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgetreferencetree_ dmplexgetreferencetree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexreferencetreegetchildsymmetry_ DMPLEXREFERENCETREEGETCHILDSYMMETRY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexreferencetreegetchildsymmetry_ dmplexreferencetreegetchildsymmetry
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreatedefaultreferencetree_ DMPLEXCREATEDEFAULTREFERENCETREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreatedefaultreferencetree_ dmplexcreatedefaultreferencetree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsettree_ DMPLEXSETTREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsettree_ dmplexsettree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgettree_ DMPLEXGETTREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgettree_ dmplexgettree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexgettreeparent_ DMPLEXGETTREEPARENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexgettreeparent_ dmplexgettreeparent
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextransfervectree_ DMPLEXTRANSFERVECTREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextransfervectree_ dmplextransfervectree
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsetreferencetree_(DM dm,DM ref, int *__ierr)
{
*__ierr = DMPlexSetReferenceTree(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((ref) ));
}
PETSC_EXTERN void  dmplexgetreferencetree_(DM dm,DM *ref, int *__ierr)
{
*__ierr = DMPlexGetReferenceTree(
	(DM)PetscToPointer((dm) ),ref);
}
PETSC_EXTERN void  dmplexreferencetreegetchildsymmetry_(DM dm,PetscInt *parent,PetscInt *parentOrientA,PetscInt *childOrientA,PetscInt *childA,PetscInt *parentOrientB,PetscInt *childOrientB,PetscInt *childB, int *__ierr)
{
*__ierr = DMPlexReferenceTreeGetChildSymmetry(
	(DM)PetscToPointer((dm) ),*parent,*parentOrientA,*childOrientA,*childA,*parentOrientB,childOrientB,childB);
}
PETSC_EXTERN void  dmplexcreatedefaultreferencetree_(MPI_Fint * comm,PetscInt *dim,PetscBool *simplex,DM *ref, int *__ierr)
{
*__ierr = DMPlexCreateDefaultReferenceTree(
	MPI_Comm_f2c(*(comm)),*dim,*simplex,ref);
}
PETSC_EXTERN void  dmplexsettree_(DM dm,PetscSection parentSection,PetscInt parents[],PetscInt childIDs[], int *__ierr)
{
*__ierr = DMPlexSetTree(
	(DM)PetscToPointer((dm) ),
	(PetscSection)PetscToPointer((parentSection) ),parents,childIDs);
}
PETSC_EXTERN void  dmplexgettree_(DM dm,PetscSection *parentSection,PetscInt *parents[],PetscInt *childIDs[],PetscSection *childSection,PetscInt *children[], int *__ierr)
{
*__ierr = DMPlexGetTree(
	(DM)PetscToPointer((dm) ),parentSection,parents,childIDs,childSection,children);
}
PETSC_EXTERN void  dmplexgettreeparent_(DM dm,PetscInt *point,PetscInt *parent,PetscInt *childID, int *__ierr)
{
*__ierr = DMPlexGetTreeParent(
	(DM)PetscToPointer((dm) ),*point,parent,childID);
}
PETSC_EXTERN void  dmplextransfervectree_(DM dmIn,Vec vecIn,DM dmOut,Vec vecOut,PetscSF sfRefine,PetscSF sfCoarsen,PetscInt *cidsRefine,PetscInt *cidsCoarsen,PetscBool *useBCs,PetscReal *time, int *__ierr)
{
*__ierr = DMPlexTransferVecTree(
	(DM)PetscToPointer((dmIn) ),
	(Vec)PetscToPointer((vecIn) ),
	(DM)PetscToPointer((dmOut) ),
	(Vec)PetscToPointer((vecOut) ),
	(PetscSF)PetscToPointer((sfRefine) ),
	(PetscSF)PetscToPointer((sfCoarsen) ),cidsRefine,cidsCoarsen,*useBCs,*time);
}
#if defined(__cplusplus)
}
#endif

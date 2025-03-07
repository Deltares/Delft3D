#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pmetis.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningparmetissetcoarsesequential_ MATPARTITIONINGPARMETISSETCOARSESEQUENTIAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningparmetissetcoarsesequential_ matpartitioningparmetissetcoarsesequential
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningparmetissetrepartition_ MATPARTITIONINGPARMETISSETREPARTITION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningparmetissetrepartition_ matpartitioningparmetissetrepartition
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningparmetisgetedgecut_ MATPARTITIONINGPARMETISGETEDGECUT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningparmetisgetedgecut_ matpartitioningparmetisgetedgecut
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmeshtocellgraph_ MATMESHTOCELLGRAPH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmeshtocellgraph_ matmeshtocellgraph
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matpartitioningparmetissetcoarsesequential_(MatPartitioning part, int *__ierr)
{
*__ierr = MatPartitioningParmetisSetCoarseSequential(
	(MatPartitioning)PetscToPointer((part) ));
}
PETSC_EXTERN void  matpartitioningparmetissetrepartition_(MatPartitioning part, int *__ierr)
{
*__ierr = MatPartitioningParmetisSetRepartition(
	(MatPartitioning)PetscToPointer((part) ));
}
PETSC_EXTERN void  matpartitioningparmetisgetedgecut_(MatPartitioning part,PetscInt *cut, int *__ierr)
{
*__ierr = MatPartitioningParmetisGetEdgeCut(
	(MatPartitioning)PetscToPointer((part) ),cut);
}
PETSC_EXTERN void  matmeshtocellgraph_(Mat mesh,PetscInt *ncommonnodes,Mat *dual, int *__ierr)
{
*__ierr = MatMeshToCellGraph(
	(Mat)PetscToPointer((mesh) ),*ncommonnodes,dual);
}
#if defined(__cplusplus)
}
#endif

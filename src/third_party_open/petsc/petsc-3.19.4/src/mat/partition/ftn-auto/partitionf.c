#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* partition.c */
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
#define matpartitioningapplynd_ MATPARTITIONINGAPPLYND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningapplynd_ matpartitioningapplynd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningapply_ MATPARTITIONINGAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningapply_ matpartitioningapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningimprove_ MATPARTITIONINGIMPROVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningimprove_ matpartitioningimprove
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningviewimbalance_ MATPARTITIONINGVIEWIMBALANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningviewimbalance_ matpartitioningviewimbalance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningsetadjacency_ MATPARTITIONINGSETADJACENCY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningsetadjacency_ matpartitioningsetadjacency
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningdestroy_ MATPARTITIONINGDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningdestroy_ matpartitioningdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningsetuseedgeweights_ MATPARTITIONINGSETUSEEDGEWEIGHTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningsetuseedgeweights_ matpartitioningsetuseedgeweights
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioninggetuseedgeweights_ MATPARTITIONINGGETUSEEDGEWEIGHTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioninggetuseedgeweights_ matpartitioninggetuseedgeweights
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningcreate_ MATPARTITIONINGCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningcreate_ matpartitioningcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningsetfromoptions_ MATPARTITIONINGSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningsetfromoptions_ matpartitioningsetfromoptions
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matpartitioningapplynd_(MatPartitioning matp,IS *partitioning, int *__ierr)
{
*__ierr = MatPartitioningApplyND(
	(MatPartitioning)PetscToPointer((matp) ),partitioning);
}
PETSC_EXTERN void  matpartitioningapply_(MatPartitioning matp,IS *partitioning, int *__ierr)
{
*__ierr = MatPartitioningApply(
	(MatPartitioning)PetscToPointer((matp) ),partitioning);
}
PETSC_EXTERN void  matpartitioningimprove_(MatPartitioning matp,IS *partitioning, int *__ierr)
{
*__ierr = MatPartitioningImprove(
	(MatPartitioning)PetscToPointer((matp) ),partitioning);
}
PETSC_EXTERN void  matpartitioningviewimbalance_(MatPartitioning matp,IS partitioning, int *__ierr)
{
*__ierr = MatPartitioningViewImbalance(
	(MatPartitioning)PetscToPointer((matp) ),
	(IS)PetscToPointer((partitioning) ));
}
PETSC_EXTERN void  matpartitioningsetadjacency_(MatPartitioning part,Mat adj, int *__ierr)
{
*__ierr = MatPartitioningSetAdjacency(
	(MatPartitioning)PetscToPointer((part) ),
	(Mat)PetscToPointer((adj) ));
}
PETSC_EXTERN void  matpartitioningdestroy_(MatPartitioning *part, int *__ierr)
{
*__ierr = MatPartitioningDestroy(part);
}
PETSC_EXTERN void  matpartitioningsetuseedgeweights_(MatPartitioning part,PetscBool *use_edge_weights, int *__ierr)
{
*__ierr = MatPartitioningSetUseEdgeWeights(
	(MatPartitioning)PetscToPointer((part) ),*use_edge_weights);
}
PETSC_EXTERN void  matpartitioninggetuseedgeweights_(MatPartitioning part,PetscBool *use_edge_weights, int *__ierr)
{
*__ierr = MatPartitioningGetUseEdgeWeights(
	(MatPartitioning)PetscToPointer((part) ),use_edge_weights);
}
PETSC_EXTERN void  matpartitioningcreate_(MPI_Fint * comm,MatPartitioning *newp, int *__ierr)
{
*__ierr = MatPartitioningCreate(
	MPI_Comm_f2c(*(comm)),newp);
}
PETSC_EXTERN void  matpartitioningsetfromoptions_(MatPartitioning part, int *__ierr)
{
*__ierr = MatPartitioningSetFromOptions(
	(MatPartitioning)PetscToPointer((part) ));
}
#if defined(__cplusplus)
}
#endif

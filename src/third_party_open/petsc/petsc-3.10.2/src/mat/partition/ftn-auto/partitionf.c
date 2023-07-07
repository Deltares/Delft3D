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
PETSC_EXTERN void PETSC_STDCALL  matpartitioningapplynd_(MatPartitioning matp,IS *partitioning, int *__ierr){
*__ierr = MatPartitioningApplyND(
	(MatPartitioning)PetscToPointer((matp) ),partitioning);
}
PETSC_EXTERN void PETSC_STDCALL  matpartitioningapply_(MatPartitioning matp,IS *partitioning, int *__ierr){
*__ierr = MatPartitioningApply(
	(MatPartitioning)PetscToPointer((matp) ),partitioning);
}
PETSC_EXTERN void PETSC_STDCALL  matpartitioningsetadjacency_(MatPartitioning part,Mat adj, int *__ierr){
*__ierr = MatPartitioningSetAdjacency(
	(MatPartitioning)PetscToPointer((part) ),
	(Mat)PetscToPointer((adj) ));
}
PETSC_EXTERN void PETSC_STDCALL  matpartitioningdestroy_(MatPartitioning *part, int *__ierr){
*__ierr = MatPartitioningDestroy(part);
}
PETSC_EXTERN void PETSC_STDCALL  matpartitioningcreate_(MPI_Fint * comm,MatPartitioning *newp, int *__ierr){
*__ierr = MatPartitioningCreate(
	MPI_Comm_f2c(*(comm)),newp);
}
PETSC_EXTERN void PETSC_STDCALL  matpartitioningsetfromoptions_(MatPartitioning part, int *__ierr){
*__ierr = MatPartitioningSetFromOptions(
	(MatPartitioning)PetscToPointer((part) ));
}
#if defined(__cplusplus)
}
#endif

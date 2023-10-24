#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* party.c */
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
#define matpartitioningpartysetcoarselevel_ MATPARTITIONINGPARTYSETCOARSELEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningpartysetcoarselevel_ matpartitioningpartysetcoarselevel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningpartysetmatchoptimization_ MATPARTITIONINGPARTYSETMATCHOPTIMIZATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningpartysetmatchoptimization_ matpartitioningpartysetmatchoptimization
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpartitioningpartysetbipart_ MATPARTITIONINGPARTYSETBIPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpartitioningpartysetbipart_ matpartitioningpartysetbipart
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matpartitioningpartysetcoarselevel_(MatPartitioning part,PetscReal *level, int *__ierr)
{
*__ierr = MatPartitioningPartySetCoarseLevel(
	(MatPartitioning)PetscToPointer((part) ),*level);
}
PETSC_EXTERN void  matpartitioningpartysetmatchoptimization_(MatPartitioning part,PetscBool *opt, int *__ierr)
{
*__ierr = MatPartitioningPartySetMatchOptimization(
	(MatPartitioning)PetscToPointer((part) ),*opt);
}
PETSC_EXTERN void  matpartitioningpartysetbipart_(MatPartitioning part,PetscBool *bp, int *__ierr)
{
*__ierr = MatPartitioningPartySetBipart(
	(MatPartitioning)PetscToPointer((part) ),*bp);
}
#if defined(__cplusplus)
}
#endif

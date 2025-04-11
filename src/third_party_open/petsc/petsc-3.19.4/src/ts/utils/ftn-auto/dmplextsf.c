#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmplexts.c */
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
#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputerhsfunctionfvm_ DMPLEXTSCOMPUTERHSFUNCTIONFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputerhsfunctionfvm_ dmplextscomputerhsfunctionfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeboundary_ DMPLEXTSCOMPUTEBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeboundary_ dmplextscomputeboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeifunctionfem_ DMPLEXTSCOMPUTEIFUNCTIONFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeifunctionfem_ dmplextscomputeifunctionfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeijacobianfem_ DMPLEXTSCOMPUTEIJACOBIANFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeijacobianfem_ dmplextscomputeijacobianfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputerhsfunctionfem_ DMPLEXTSCOMPUTERHSFUNCTIONFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputerhsfunctionfem_ dmplextscomputerhsfunctionfem
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplextscomputerhsfunctionfvm_(DM dm,PetscReal *time,Vec locX,Vec F,void*user, int *__ierr)
{
*__ierr = DMPlexTSComputeRHSFunctionFVM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void  dmplextscomputeboundary_(DM dm,PetscReal *time,Vec locX,Vec locX_t,void*user, int *__ierr)
{
*__ierr = DMPlexTSComputeBoundary(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),user);
}
PETSC_EXTERN void  dmplextscomputeifunctionfem_(DM dm,PetscReal *time,Vec locX,Vec locX_t,Vec locF,void*user, int *__ierr)
{
*__ierr = DMPlexTSComputeIFunctionFEM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),
	(Vec)PetscToPointer((locF) ),user);
}
PETSC_EXTERN void  dmplextscomputeijacobianfem_(DM dm,PetscReal *time,Vec locX,Vec locX_t,PetscReal *X_tShift,Mat Jac,Mat JacP,void*user, int *__ierr)
{
*__ierr = DMPlexTSComputeIJacobianFEM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),*X_tShift,
	(Mat)PetscToPointer((Jac) ),
	(Mat)PetscToPointer((JacP) ),user);
}
PETSC_EXTERN void  dmplextscomputerhsfunctionfem_(DM dm,PetscReal *time,Vec locX,Vec locG,void*user, int *__ierr)
{
*__ierr = DMPlexTSComputeRHSFunctionFEM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locG) ),user);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmplexsnes.c */
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
#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputeresidualfem_ DMPLEXSNESCOMPUTERESIDUALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputeresidualfem_ dmplexsnescomputeresidualfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputeboundaryfem_ DMPLEXSNESCOMPUTEBOUNDARYFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputeboundaryfem_ dmplexsnescomputeboundaryfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsnescomputejacobianaction_ DMSNESCOMPUTEJACOBIANACTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsnescomputejacobianaction_ dmsnescomputejacobianaction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsnescomputejacobianfem_ DMPLEXSNESCOMPUTEJACOBIANFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsnescomputejacobianfem_ dmplexsnescomputejacobianfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmsnescreatejacobianmf_ DMSNESCREATEJACOBIANMF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmsnescreatejacobianmf_ dmsnescreatejacobianmf
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexsetsneslocalfem_ DMPLEXSETSNESLOCALFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexsetsneslocalfem_ dmplexsetsneslocalfem
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexsnescomputeresidualfem_(DM dm,Vec X,Vec F,void*user, int *__ierr)
{
*__ierr = DMPlexSNESComputeResidualFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void  dmplexsnescomputeboundaryfem_(DM dm,Vec X,void*user, int *__ierr)
{
*__ierr = DMPlexSNESComputeBoundaryFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),user);
}
PETSC_EXTERN void  dmsnescomputejacobianaction_(DM dm,Vec X,Vec Y,Vec F,void*user, int *__ierr)
{
*__ierr = DMSNESComputeJacobianAction(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Y) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void  dmplexsnescomputejacobianfem_(DM dm,Vec X,Mat Jac,Mat JacP,void*user, int *__ierr)
{
*__ierr = DMPlexSNESComputeJacobianFEM(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),
	(Mat)PetscToPointer((Jac) ),
	(Mat)PetscToPointer((JacP) ),user);
}
PETSC_EXTERN void  dmsnescreatejacobianmf_(DM dm,Vec X,void*user,Mat *J, int *__ierr)
{
*__ierr = DMSNESCreateJacobianMF(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),user,J);
}
PETSC_EXTERN void  dmplexsetsneslocalfem_(DM dm,void*boundaryctx,void*residualctx,void*jacobianctx, int *__ierr)
{
*__ierr = DMPlexSetSNESLocalFEM(
	(DM)PetscToPointer((dm) ),boundaryctx,residualctx,jacobianctx);
}
#if defined(__cplusplus)
}
#endif

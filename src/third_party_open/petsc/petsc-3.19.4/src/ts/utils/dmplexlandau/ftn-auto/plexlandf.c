#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexland.c */
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
#include "petsclandau.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandauaddmaxwellians_ DMPLEXLANDAUADDMAXWELLIANS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandauaddmaxwellians_ dmplexlandauaddmaxwellians
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandauaccess_ DMPLEXLANDAUACCESS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandauaccess_ dmplexlandauaccess
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandaudestroyvelocityspace_ DMPLEXLANDAUDESTROYVELOCITYSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandaudestroyvelocityspace_ dmplexlandaudestroyvelocityspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandauprintnorms_ DMPLEXLANDAUPRINTNORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandauprintnorms_ dmplexlandauprintnorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandaucreatemassmatrix_ DMPLEXLANDAUCREATEMASSMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandaucreatemassmatrix_ dmplexlandaucreatemassmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandauifunction_ DMPLEXLANDAUIFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandauifunction_ dmplexlandauifunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexlandauijacobian_ DMPLEXLANDAUIJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexlandauijacobian_ dmplexlandauijacobian
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmplexlandauaddmaxwellians_(DM dm,Vec X,PetscReal *time,PetscReal temps[],PetscReal ns[],PetscInt *grid,PetscInt *b_id,PetscInt *n_batch,void*actx, int *__ierr)
{
*__ierr = DMPlexLandauAddMaxwellians(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((X) ),*time,temps,ns,*grid,*b_id,*n_batch,actx);
}
PETSC_EXTERN void  dmplexlandauaccess_(DM pack,Vec X,PetscErrorCode (*func)(DM, Vec, PetscInt, PetscInt, PetscInt, void *),void*user_ctx, int *__ierr)
{
*__ierr = DMPlexLandauAccess(
	(DM)PetscToPointer((pack) ),
	(Vec)PetscToPointer((X) ),func,user_ctx);
}
PETSC_EXTERN void  dmplexlandaudestroyvelocityspace_(DM *dm, int *__ierr)
{
*__ierr = DMPlexLandauDestroyVelocitySpace(dm);
}
PETSC_EXTERN void  dmplexlandauprintnorms_(Vec X,PetscInt *stepi, int *__ierr)
{
*__ierr = DMPlexLandauPrintNorms(
	(Vec)PetscToPointer((X) ),*stepi);
}
PETSC_EXTERN void  dmplexlandaucreatemassmatrix_(DM pack,Mat *Amat, int *__ierr)
{
*__ierr = DMPlexLandauCreateMassMatrix(
	(DM)PetscToPointer((pack) ),Amat);
}
PETSC_EXTERN void  dmplexlandauifunction_(TS ts,PetscReal *time_dummy,Vec X,Vec X_t,Vec F,void*actx, int *__ierr)
{
*__ierr = DMPlexLandauIFunction(
	(TS)PetscToPointer((ts) ),*time_dummy,
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((X_t) ),
	(Vec)PetscToPointer((F) ),actx);
}
PETSC_EXTERN void  dmplexlandauijacobian_(TS ts,PetscReal *time_dummy,Vec X,Vec U_tdummy,PetscReal *shift,Mat Amat,Mat Pmat,void*actx, int *__ierr)
{
*__ierr = DMPlexLandauIJacobian(
	(TS)PetscToPointer((ts) ),*time_dummy,
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((U_tdummy) ),*shift,
	(Mat)PetscToPointer((Amat) ),
	(Mat)PetscToPointer((Pmat) ),actx);
}
#if defined(__cplusplus)
}
#endif

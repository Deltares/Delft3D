#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mgfunc.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetcoarsesolve_ PCMGGETCOARSESOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetcoarsesolve_ pcmggetcoarsesolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetinterpolation_ PCMGSETINTERPOLATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetinterpolation_ pcmgsetinterpolation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetoperators_ PCMGSETOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetoperators_ pcmgsetoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetinterpolation_ PCMGGETINTERPOLATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetinterpolation_ pcmggetinterpolation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetrestriction_ PCMGSETRESTRICTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetrestriction_ pcmgsetrestriction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetrestriction_ PCMGGETRESTRICTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetrestriction_ pcmggetrestriction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetrscale_ PCMGSETRSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetrscale_ pcmgsetrscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetrscale_ PCMGGETRSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetrscale_ pcmggetrscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetinjection_ PCMGSETINJECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetinjection_ pcmgsetinjection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetinjection_ PCMGGETINJECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetinjection_ pcmggetinjection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetsmoother_ PCMGGETSMOOTHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetsmoother_ pcmggetsmoother
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetsmootherup_ PCMGGETSMOOTHERUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetsmootherup_ pcmggetsmootherup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetsmootherdown_ PCMGGETSMOOTHERDOWN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetsmootherdown_ pcmggetsmootherdown
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetcycletypeonlevel_ PCMGSETCYCLETYPEONLEVEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetcycletypeonlevel_ pcmgsetcycletypeonlevel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetrhs_ PCMGSETRHS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetrhs_ pcmgsetrhs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetx_ PCMGSETX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetx_ pcmgsetx
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetr_ PCMGSETR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetr_ pcmgsetr
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcmggetcoarsesolve_(PC pc,KSP *ksp, int *__ierr)
{
*__ierr = PCMGGetCoarseSolve(
	(PC)PetscToPointer((pc) ),ksp);
}
PETSC_EXTERN void  pcmgsetinterpolation_(PC pc,PetscInt *l,Mat mat, int *__ierr)
{
*__ierr = PCMGSetInterpolation(
	(PC)PetscToPointer((pc) ),*l,
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  pcmgsetoperators_(PC pc,PetscInt *l,Mat Amat,Mat Pmat, int *__ierr)
{
*__ierr = PCMGSetOperators(
	(PC)PetscToPointer((pc) ),*l,
	(Mat)PetscToPointer((Amat) ),
	(Mat)PetscToPointer((Pmat) ));
}
PETSC_EXTERN void  pcmggetinterpolation_(PC pc,PetscInt *l,Mat *mat, int *__ierr)
{
*__ierr = PCMGGetInterpolation(
	(PC)PetscToPointer((pc) ),*l,mat);
}
PETSC_EXTERN void  pcmgsetrestriction_(PC pc,PetscInt *l,Mat mat, int *__ierr)
{
*__ierr = PCMGSetRestriction(
	(PC)PetscToPointer((pc) ),*l,
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  pcmggetrestriction_(PC pc,PetscInt *l,Mat *mat, int *__ierr)
{
*__ierr = PCMGGetRestriction(
	(PC)PetscToPointer((pc) ),*l,mat);
}
PETSC_EXTERN void  pcmgsetrscale_(PC pc,PetscInt *l,Vec rscale, int *__ierr)
{
*__ierr = PCMGSetRScale(
	(PC)PetscToPointer((pc) ),*l,
	(Vec)PetscToPointer((rscale) ));
}
PETSC_EXTERN void  pcmggetrscale_(PC pc,PetscInt *l,Vec *rscale, int *__ierr)
{
*__ierr = PCMGGetRScale(
	(PC)PetscToPointer((pc) ),*l,rscale);
}
PETSC_EXTERN void  pcmgsetinjection_(PC pc,PetscInt *l,Mat mat, int *__ierr)
{
*__ierr = PCMGSetInjection(
	(PC)PetscToPointer((pc) ),*l,
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void  pcmggetinjection_(PC pc,PetscInt *l,Mat *mat, int *__ierr)
{
*__ierr = PCMGGetInjection(
	(PC)PetscToPointer((pc) ),*l,mat);
}
PETSC_EXTERN void  pcmggetsmoother_(PC pc,PetscInt *l,KSP *ksp, int *__ierr)
{
*__ierr = PCMGGetSmoother(
	(PC)PetscToPointer((pc) ),*l,ksp);
}
PETSC_EXTERN void  pcmggetsmootherup_(PC pc,PetscInt *l,KSP *ksp, int *__ierr)
{
*__ierr = PCMGGetSmootherUp(
	(PC)PetscToPointer((pc) ),*l,ksp);
}
PETSC_EXTERN void  pcmggetsmootherdown_(PC pc,PetscInt *l,KSP *ksp, int *__ierr)
{
*__ierr = PCMGGetSmootherDown(
	(PC)PetscToPointer((pc) ),*l,ksp);
}
PETSC_EXTERN void  pcmgsetcycletypeonlevel_(PC pc,PetscInt *l,PCMGCycleType *c, int *__ierr)
{
*__ierr = PCMGSetCycleTypeOnLevel(
	(PC)PetscToPointer((pc) ),*l,*c);
}
PETSC_EXTERN void  pcmgsetrhs_(PC pc,PetscInt *l,Vec c, int *__ierr)
{
*__ierr = PCMGSetRhs(
	(PC)PetscToPointer((pc) ),*l,
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  pcmgsetx_(PC pc,PetscInt *l,Vec c, int *__ierr)
{
*__ierr = PCMGSetX(
	(PC)PetscToPointer((pc) ),*l,
	(Vec)PetscToPointer((c) ));
}
PETSC_EXTERN void  pcmgsetr_(PC pc,PetscInt *l,Vec c, int *__ierr)
{
*__ierr = PCMGSetR(
	(PC)PetscToPointer((pc) ),*l,
	(Vec)PetscToPointer((c) ));
}
#if defined(__cplusplus)
}
#endif

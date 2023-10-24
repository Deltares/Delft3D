#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* itfunc.c */
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
#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcomputeextremesingularvalues_ KSPCOMPUTEEXTREMESINGULARVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcomputeextremesingularvalues_ kspcomputeextremesingularvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcomputeeigenvalues_ KSPCOMPUTEEIGENVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcomputeeigenvalues_ kspcomputeeigenvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspcomputeritz_ KSPCOMPUTERITZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspcomputeritz_ kspcomputeritz
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetuponblocks_ KSPSETUPONBLOCKS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetuponblocks_ kspsetuponblocks
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetreusepreconditioner_ KSPSETREUSEPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetreusepreconditioner_ kspsetreusepreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetreusepreconditioner_ KSPGETREUSEPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetreusepreconditioner_ kspgetreusepreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetskippcsetfromoptions_ KSPSETSKIPPCSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetskippcsetfromoptions_ kspsetskippcsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetup_ KSPSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetup_ kspsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspconvergedreasonviewcancel_ KSPCONVERGEDREASONVIEWCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspconvergedreasonviewcancel_ kspconvergedreasonviewcancel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspconvergedreasonviewfromoptions_ KSPCONVERGEDREASONVIEWFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspconvergedreasonviewfromoptions_ kspconvergedreasonviewfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsolve_ KSPSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsolve_ kspsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsolvetranspose_ KSPSOLVETRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsolvetranspose_ kspsolvetranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspmatsolve_ KSPMATSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspmatsolve_ kspmatsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspmatsolvetranspose_ KSPMATSOLVETRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspmatsolvetranspose_ kspmatsolvetranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetmatsolvebatchsize_ KSPSETMATSOLVEBATCHSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetmatsolvebatchsize_ kspsetmatsolvebatchsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetmatsolvebatchsize_ KSPGETMATSOLVEBATCHSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetmatsolvebatchsize_ kspgetmatsolvebatchsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspresetviewers_ KSPRESETVIEWERS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspresetviewers_ kspresetviewers
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspreset_ KSPRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspreset_ kspreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetpcside_ KSPSETPCSIDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetpcside_ kspsetpcside
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetpcside_ KSPGETPCSIDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetpcside_ kspgetpcside
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgettolerances_ KSPGETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgettolerances_ kspgettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsettolerances_ KSPSETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsettolerances_ kspsettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetinitialguessnonzero_ KSPSETINITIALGUESSNONZERO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetinitialguessnonzero_ kspsetinitialguessnonzero
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetinitialguessnonzero_ KSPGETINITIALGUESSNONZERO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetinitialguessnonzero_ kspgetinitialguessnonzero
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspseterrorifnotconverged_ KSPSETERRORIFNOTCONVERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspseterrorifnotconverged_ kspseterrorifnotconverged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgeterrorifnotconverged_ KSPGETERRORIFNOTCONVERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgeterrorifnotconverged_ kspgeterrorifnotconverged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetinitialguessknoll_ KSPSETINITIALGUESSKNOLL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetinitialguessknoll_ kspsetinitialguessknoll
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetinitialguessknoll_ KSPGETINITIALGUESSKNOLL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetinitialguessknoll_ kspgetinitialguessknoll
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetcomputesingularvalues_ KSPGETCOMPUTESINGULARVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetcomputesingularvalues_ kspgetcomputesingularvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetcomputesingularvalues_ KSPSETCOMPUTESINGULARVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetcomputesingularvalues_ kspsetcomputesingularvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetcomputeeigenvalues_ KSPGETCOMPUTEEIGENVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetcomputeeigenvalues_ kspgetcomputeeigenvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetcomputeeigenvalues_ KSPSETCOMPUTEEIGENVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetcomputeeigenvalues_ kspsetcomputeeigenvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetcomputeritz_ KSPSETCOMPUTERITZ
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetcomputeritz_ kspsetcomputeritz
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetrhs_ KSPGETRHS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetrhs_ kspgetrhs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetsolution_ KSPGETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetsolution_ kspgetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetpc_ KSPSETPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetpc_ kspsetpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetpc_ KSPGETPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetpc_ kspgetpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspmonitor_ KSPMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspmonitor_ kspmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspmonitorcancel_ KSPMONITORCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspmonitorcancel_ kspmonitorcancel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetresidualhistory_ KSPSETRESIDUALHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetresidualhistory_ kspsetresidualhistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspseterrorhistory_ KSPSETERRORHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspseterrorhistory_ kspseterrorhistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetdiagonalscale_ KSPSETDIAGONALSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetdiagonalscale_ kspsetdiagonalscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetdiagonalscale_ KSPGETDIAGONALSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetdiagonalscale_ kspgetdiagonalscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetdiagonalscalefix_ KSPSETDIAGONALSCALEFIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetdiagonalscalefix_ kspsetdiagonalscalefix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspgetdiagonalscalefix_ KSPGETDIAGONALSCALEFIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspgetdiagonalscalefix_ kspgetdiagonalscalefix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspsetuseexplicittranspose_ KSPSETUSEEXPLICITTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspsetuseexplicittranspose_ kspsetuseexplicittranspose
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  kspcomputeextremesingularvalues_(KSP ksp,PetscReal *emax,PetscReal *emin, int *__ierr)
{
*__ierr = KSPComputeExtremeSingularValues(
	(KSP)PetscToPointer((ksp) ),emax,emin);
}
PETSC_EXTERN void  kspcomputeeigenvalues_(KSP ksp,PetscInt *n,PetscReal r[],PetscReal c[],PetscInt *neig, int *__ierr)
{
*__ierr = KSPComputeEigenvalues(
	(KSP)PetscToPointer((ksp) ),*n,r,c,neig);
}
PETSC_EXTERN void  kspcomputeritz_(KSP ksp,PetscBool *ritz,PetscBool *small,PetscInt *nrit,Vec S[],PetscReal tetar[],PetscReal tetai[], int *__ierr)
{
*__ierr = KSPComputeRitz(
	(KSP)PetscToPointer((ksp) ),*ritz,*small,nrit,S,tetar,tetai);
}
PETSC_EXTERN void  kspsetuponblocks_(KSP ksp, int *__ierr)
{
*__ierr = KSPSetUpOnBlocks(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspsetreusepreconditioner_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPSetReusePreconditioner(
	(KSP)PetscToPointer((ksp) ),*flag);
}
PETSC_EXTERN void  kspgetreusepreconditioner_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPGetReusePreconditioner(
	(KSP)PetscToPointer((ksp) ),flag);
}
PETSC_EXTERN void  kspsetskippcsetfromoptions_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPSetSkipPCSetFromOptions(
	(KSP)PetscToPointer((ksp) ),*flag);
}
PETSC_EXTERN void  kspsetup_(KSP ksp, int *__ierr)
{
*__ierr = KSPSetUp(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspconvergedreasonviewcancel_(KSP ksp, int *__ierr)
{
*__ierr = KSPConvergedReasonViewCancel(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspconvergedreasonviewfromoptions_(KSP ksp, int *__ierr)
{
*__ierr = KSPConvergedReasonViewFromOptions(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspsolve_(KSP ksp,Vec b,Vec x, int *__ierr)
{
*__ierr = KSPSolve(
	(KSP)PetscToPointer((ksp) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void  kspsolvetranspose_(KSP ksp,Vec b,Vec x, int *__ierr)
{
*__ierr = KSPSolveTranspose(
	(KSP)PetscToPointer((ksp) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void  kspmatsolve_(KSP ksp,Mat B,Mat X, int *__ierr)
{
*__ierr = KSPMatSolve(
	(KSP)PetscToPointer((ksp) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((X) ));
}
PETSC_EXTERN void  kspmatsolvetranspose_(KSP ksp,Mat B,Mat X, int *__ierr)
{
*__ierr = KSPMatSolveTranspose(
	(KSP)PetscToPointer((ksp) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((X) ));
}
PETSC_EXTERN void  kspsetmatsolvebatchsize_(KSP ksp,PetscInt *bs, int *__ierr)
{
*__ierr = KSPSetMatSolveBatchSize(
	(KSP)PetscToPointer((ksp) ),*bs);
}
PETSC_EXTERN void  kspgetmatsolvebatchsize_(KSP ksp,PetscInt *bs, int *__ierr)
{
*__ierr = KSPGetMatSolveBatchSize(
	(KSP)PetscToPointer((ksp) ),bs);
}
PETSC_EXTERN void  kspresetviewers_(KSP ksp, int *__ierr)
{
*__ierr = KSPResetViewers(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspreset_(KSP ksp, int *__ierr)
{
*__ierr = KSPReset(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspsetpcside_(KSP ksp,PCSide *side, int *__ierr)
{
*__ierr = KSPSetPCSide(
	(KSP)PetscToPointer((ksp) ),*side);
}
PETSC_EXTERN void  kspgetpcside_(KSP ksp,PCSide *side, int *__ierr)
{
*__ierr = KSPGetPCSide(
	(KSP)PetscToPointer((ksp) ),side);
}
PETSC_EXTERN void  kspgettolerances_(KSP ksp,PetscReal *rtol,PetscReal *abstol,PetscReal *dtol,PetscInt *maxits, int *__ierr)
{
*__ierr = KSPGetTolerances(
	(KSP)PetscToPointer((ksp) ),rtol,abstol,dtol,maxits);
}
PETSC_EXTERN void  kspsettolerances_(KSP ksp,PetscReal *rtol,PetscReal *abstol,PetscReal *dtol,PetscInt *maxits, int *__ierr)
{
*__ierr = KSPSetTolerances(
	(KSP)PetscToPointer((ksp) ),*rtol,*abstol,*dtol,*maxits);
}
PETSC_EXTERN void  kspsetinitialguessnonzero_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetInitialGuessNonzero(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspgetinitialguessnonzero_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPGetInitialGuessNonzero(
	(KSP)PetscToPointer((ksp) ),flag);
}
PETSC_EXTERN void  kspseterrorifnotconverged_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetErrorIfNotConverged(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspgeterrorifnotconverged_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPGetErrorIfNotConverged(
	(KSP)PetscToPointer((ksp) ),flag);
}
PETSC_EXTERN void  kspsetinitialguessknoll_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetInitialGuessKnoll(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspgetinitialguessknoll_(KSP ksp,PetscBool *flag, int *__ierr)
{
*__ierr = KSPGetInitialGuessKnoll(
	(KSP)PetscToPointer((ksp) ),flag);
}
PETSC_EXTERN void  kspgetcomputesingularvalues_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPGetComputeSingularValues(
	(KSP)PetscToPointer((ksp) ),flg);
}
PETSC_EXTERN void  kspsetcomputesingularvalues_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetComputeSingularValues(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspgetcomputeeigenvalues_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPGetComputeEigenvalues(
	(KSP)PetscToPointer((ksp) ),flg);
}
PETSC_EXTERN void  kspsetcomputeeigenvalues_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetComputeEigenvalues(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspsetcomputeritz_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetComputeRitz(
	(KSP)PetscToPointer((ksp) ),*flg);
}
PETSC_EXTERN void  kspgetrhs_(KSP ksp,Vec *r, int *__ierr)
{
*__ierr = KSPGetRhs(
	(KSP)PetscToPointer((ksp) ),r);
}
PETSC_EXTERN void  kspgetsolution_(KSP ksp,Vec *v, int *__ierr)
{
*__ierr = KSPGetSolution(
	(KSP)PetscToPointer((ksp) ),v);
}
PETSC_EXTERN void  kspsetpc_(KSP ksp,PC pc, int *__ierr)
{
*__ierr = KSPSetPC(
	(KSP)PetscToPointer((ksp) ),
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  kspgetpc_(KSP ksp,PC *pc, int *__ierr)
{
*__ierr = KSPGetPC(
	(KSP)PetscToPointer((ksp) ),pc);
}
PETSC_EXTERN void  kspmonitor_(KSP ksp,PetscInt *it,PetscReal *rnorm, int *__ierr)
{
*__ierr = KSPMonitor(
	(KSP)PetscToPointer((ksp) ),*it,*rnorm);
}
PETSC_EXTERN void  kspmonitorcancel_(KSP ksp, int *__ierr)
{
*__ierr = KSPMonitorCancel(
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  kspsetresidualhistory_(KSP ksp,PetscReal a[],PetscInt *na,PetscBool *reset, int *__ierr)
{
*__ierr = KSPSetResidualHistory(
	(KSP)PetscToPointer((ksp) ),a,*na,*reset);
}
PETSC_EXTERN void  kspseterrorhistory_(KSP ksp,PetscReal a[],PetscInt *na,PetscBool *reset, int *__ierr)
{
*__ierr = KSPSetErrorHistory(
	(KSP)PetscToPointer((ksp) ),a,*na,*reset);
}
PETSC_EXTERN void  kspsetdiagonalscale_(KSP ksp,PetscBool *scale, int *__ierr)
{
*__ierr = KSPSetDiagonalScale(
	(KSP)PetscToPointer((ksp) ),*scale);
}
PETSC_EXTERN void  kspgetdiagonalscale_(KSP ksp,PetscBool *scale, int *__ierr)
{
*__ierr = KSPGetDiagonalScale(
	(KSP)PetscToPointer((ksp) ),scale);
}
PETSC_EXTERN void  kspsetdiagonalscalefix_(KSP ksp,PetscBool *fix, int *__ierr)
{
*__ierr = KSPSetDiagonalScaleFix(
	(KSP)PetscToPointer((ksp) ),*fix);
}
PETSC_EXTERN void  kspgetdiagonalscalefix_(KSP ksp,PetscBool *fix, int *__ierr)
{
*__ierr = KSPGetDiagonalScaleFix(
	(KSP)PetscToPointer((ksp) ),fix);
}
PETSC_EXTERN void  kspsetuseexplicittranspose_(KSP ksp,PetscBool *flg, int *__ierr)
{
*__ierr = KSPSetUseExplicitTranspose(
	(KSP)PetscToPointer((ksp) ),*flg);
}
#if defined(__cplusplus)
}
#endif

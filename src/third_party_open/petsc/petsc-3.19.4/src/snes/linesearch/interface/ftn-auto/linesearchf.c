#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* linesearch.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchmonitorcancel_ SNESLINESEARCHMONITORCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchmonitorcancel_ sneslinesearchmonitorcancel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchmonitor_ SNESLINESEARCHMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchmonitor_ sneslinesearchmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchcreate_ SNESLINESEARCHCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchcreate_ sneslinesearchcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetup_ SNESLINESEARCHSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetup_ sneslinesearchsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchreset_ SNESLINESEARCHRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchreset_ sneslinesearchreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchprecheck_ SNESLINESEARCHPRECHECK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchprecheck_ sneslinesearchprecheck
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchpostcheck_ SNESLINESEARCHPOSTCHECK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchpostcheck_ sneslinesearchpostcheck
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchapply_ SNESLINESEARCHAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchapply_ sneslinesearchapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchdestroy_ SNESLINESEARCHDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchdestroy_ sneslinesearchdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetdefaultmonitor_ SNESLINESEARCHSETDEFAULTMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetdefaultmonitor_ sneslinesearchsetdefaultmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetdefaultmonitor_ SNESLINESEARCHGETDEFAULTMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetdefaultmonitor_ sneslinesearchgetdefaultmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetfromoptions_ SNESLINESEARCHSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetfromoptions_ sneslinesearchsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchview_ SNESLINESEARCHVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchview_ sneslinesearchview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetsnes_ SNESLINESEARCHSETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetsnes_ sneslinesearchsetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetsnes_ SNESLINESEARCHGETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetsnes_ sneslinesearchgetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetlambda_ SNESLINESEARCHGETLAMBDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetlambda_ sneslinesearchgetlambda
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetlambda_ SNESLINESEARCHSETLAMBDA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetlambda_ sneslinesearchsetlambda
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgettolerances_ SNESLINESEARCHGETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgettolerances_ sneslinesearchgettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsettolerances_ SNESLINESEARCHSETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsettolerances_ sneslinesearchsettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetdamping_ SNESLINESEARCHGETDAMPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetdamping_ sneslinesearchgetdamping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetdamping_ SNESLINESEARCHSETDAMPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetdamping_ sneslinesearchsetdamping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetorder_ SNESLINESEARCHGETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetorder_ sneslinesearchgetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetorder_ SNESLINESEARCHSETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetorder_ sneslinesearchsetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetnorms_ SNESLINESEARCHGETNORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetnorms_ sneslinesearchgetnorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetnorms_ SNESLINESEARCHSETNORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetnorms_ sneslinesearchsetnorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchcomputenorms_ SNESLINESEARCHCOMPUTENORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchcomputenorms_ sneslinesearchcomputenorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetcomputenorms_ SNESLINESEARCHSETCOMPUTENORMS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetcomputenorms_ sneslinesearchsetcomputenorms
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetvecs_ SNESLINESEARCHGETVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetvecs_ sneslinesearchgetvecs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetvecs_ SNESLINESEARCHSETVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetvecs_ sneslinesearchsetvecs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchgetreason_ SNESLINESEARCHGETREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchgetreason_ sneslinesearchgetreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneslinesearchsetreason_ SNESLINESEARCHSETREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneslinesearchsetreason_ sneslinesearchsetreason
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  sneslinesearchmonitorcancel_(SNESLineSearch ls, int *__ierr)
{
*__ierr = SNESLineSearchMonitorCancel(
	(SNESLineSearch)PetscToPointer((ls) ));
}
PETSC_EXTERN void  sneslinesearchmonitor_(SNESLineSearch ls, int *__ierr)
{
*__ierr = SNESLineSearchMonitor(
	(SNESLineSearch)PetscToPointer((ls) ));
}
PETSC_EXTERN void  sneslinesearchcreate_(MPI_Fint * comm,SNESLineSearch *outlinesearch, int *__ierr)
{
*__ierr = SNESLineSearchCreate(
	MPI_Comm_f2c(*(comm)),outlinesearch);
}

PETSC_EXTERN void  sneslinesearchsetup_(SNESLineSearch linesearch, int *__ierr)
{
*__ierr = SNESLineSearchSetUp(
	(SNESLineSearch)PetscToPointer((linesearch) ));
}

PETSC_EXTERN void  sneslinesearchreset_(SNESLineSearch linesearch, int *__ierr)
{
*__ierr = SNESLineSearchReset(
	(SNESLineSearch)PetscToPointer((linesearch) ));
}
PETSC_EXTERN void  sneslinesearchprecheck_(SNESLineSearch linesearch,Vec X,Vec Y,PetscBool *changed, int *__ierr)
{
*__ierr = SNESLineSearchPreCheck(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Y) ),changed);
}
PETSC_EXTERN void  sneslinesearchpostcheck_(SNESLineSearch linesearch,Vec X,Vec Y,Vec W,PetscBool *changed_Y,PetscBool *changed_W, int *__ierr)
{
*__ierr = SNESLineSearchPostCheck(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Y) ),
	(Vec)PetscToPointer((W) ),changed_Y,changed_W);
}
PETSC_EXTERN void  sneslinesearchapply_(SNESLineSearch linesearch,Vec X,Vec F,PetscReal *fnorm,Vec Y, int *__ierr)
{
*__ierr = SNESLineSearchApply(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),fnorm,
	(Vec)PetscToPointer((Y) ));
}
PETSC_EXTERN void  sneslinesearchdestroy_(SNESLineSearch *linesearch, int *__ierr)
{
*__ierr = SNESLineSearchDestroy(linesearch);
}
PETSC_EXTERN void  sneslinesearchsetdefaultmonitor_(SNESLineSearch linesearch,PetscViewer viewer, int *__ierr)
{
*__ierr = SNESLineSearchSetDefaultMonitor(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  sneslinesearchgetdefaultmonitor_(SNESLineSearch linesearch,PetscViewer *monitor, int *__ierr)
{
*__ierr = SNESLineSearchGetDefaultMonitor(
	(SNESLineSearch)PetscToPointer((linesearch) ),monitor);
}
PETSC_EXTERN void  sneslinesearchsetfromoptions_(SNESLineSearch linesearch, int *__ierr)
{
*__ierr = SNESLineSearchSetFromOptions(
	(SNESLineSearch)PetscToPointer((linesearch) ));
}
PETSC_EXTERN void  sneslinesearchview_(SNESLineSearch linesearch,PetscViewer viewer, int *__ierr)
{
*__ierr = SNESLineSearchView(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  sneslinesearchsetsnes_(SNESLineSearch linesearch,SNES snes, int *__ierr)
{
*__ierr = SNESLineSearchSetSNES(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  sneslinesearchgetsnes_(SNESLineSearch linesearch,SNES *snes, int *__ierr)
{
*__ierr = SNESLineSearchGetSNES(
	(SNESLineSearch)PetscToPointer((linesearch) ),snes);
}
PETSC_EXTERN void  sneslinesearchgetlambda_(SNESLineSearch linesearch,PetscReal *lambda, int *__ierr)
{
*__ierr = SNESLineSearchGetLambda(
	(SNESLineSearch)PetscToPointer((linesearch) ),lambda);
}
PETSC_EXTERN void  sneslinesearchsetlambda_(SNESLineSearch linesearch,PetscReal *lambda, int *__ierr)
{
*__ierr = SNESLineSearchSetLambda(
	(SNESLineSearch)PetscToPointer((linesearch) ),*lambda);
}
PETSC_EXTERN void  sneslinesearchgettolerances_(SNESLineSearch linesearch,PetscReal *steptol,PetscReal *maxstep,PetscReal *rtol,PetscReal *atol,PetscReal *ltol,PetscInt *max_its, int *__ierr)
{
*__ierr = SNESLineSearchGetTolerances(
	(SNESLineSearch)PetscToPointer((linesearch) ),steptol,maxstep,rtol,atol,ltol,max_its);
}
PETSC_EXTERN void  sneslinesearchsettolerances_(SNESLineSearch linesearch,PetscReal *steptol,PetscReal *maxstep,PetscReal *rtol,PetscReal *atol,PetscReal *ltol,PetscInt *max_its, int *__ierr)
{
*__ierr = SNESLineSearchSetTolerances(
	(SNESLineSearch)PetscToPointer((linesearch) ),*steptol,*maxstep,*rtol,*atol,*ltol,*max_its);
}

PETSC_EXTERN void  sneslinesearchgetdamping_(SNESLineSearch linesearch,PetscReal *damping, int *__ierr)
{
*__ierr = SNESLineSearchGetDamping(
	(SNESLineSearch)PetscToPointer((linesearch) ),damping);
}
PETSC_EXTERN void  sneslinesearchsetdamping_(SNESLineSearch linesearch,PetscReal *damping, int *__ierr)
{
*__ierr = SNESLineSearchSetDamping(
	(SNESLineSearch)PetscToPointer((linesearch) ),*damping);
}

PETSC_EXTERN void  sneslinesearchgetorder_(SNESLineSearch linesearch,PetscInt *order, int *__ierr)
{
*__ierr = SNESLineSearchGetOrder(
	(SNESLineSearch)PetscToPointer((linesearch) ),order);
}
PETSC_EXTERN void  sneslinesearchsetorder_(SNESLineSearch linesearch,PetscInt *order, int *__ierr)
{
*__ierr = SNESLineSearchSetOrder(
	(SNESLineSearch)PetscToPointer((linesearch) ),*order);
}
PETSC_EXTERN void  sneslinesearchgetnorms_(SNESLineSearch linesearch,PetscReal *xnorm,PetscReal *fnorm,PetscReal *ynorm, int *__ierr)
{
*__ierr = SNESLineSearchGetNorms(
	(SNESLineSearch)PetscToPointer((linesearch) ),xnorm,fnorm,ynorm);
}
PETSC_EXTERN void  sneslinesearchsetnorms_(SNESLineSearch linesearch,PetscReal *xnorm,PetscReal *fnorm,PetscReal *ynorm, int *__ierr)
{
*__ierr = SNESLineSearchSetNorms(
	(SNESLineSearch)PetscToPointer((linesearch) ),*xnorm,*fnorm,*ynorm);
}
PETSC_EXTERN void  sneslinesearchcomputenorms_(SNESLineSearch linesearch, int *__ierr)
{
*__ierr = SNESLineSearchComputeNorms(
	(SNESLineSearch)PetscToPointer((linesearch) ));
}
PETSC_EXTERN void  sneslinesearchsetcomputenorms_(SNESLineSearch linesearch,PetscBool *flg, int *__ierr)
{
*__ierr = SNESLineSearchSetComputeNorms(
	(SNESLineSearch)PetscToPointer((linesearch) ),*flg);
}
PETSC_EXTERN void  sneslinesearchgetvecs_(SNESLineSearch linesearch,Vec *X,Vec *F,Vec *Y,Vec *W,Vec *G, int *__ierr)
{
*__ierr = SNESLineSearchGetVecs(
	(SNESLineSearch)PetscToPointer((linesearch) ),X,F,Y,W,G);
}
PETSC_EXTERN void  sneslinesearchsetvecs_(SNESLineSearch linesearch,Vec X,Vec F,Vec Y,Vec W,Vec G, int *__ierr)
{
*__ierr = SNESLineSearchSetVecs(
	(SNESLineSearch)PetscToPointer((linesearch) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),
	(Vec)PetscToPointer((Y) ),
	(Vec)PetscToPointer((W) ),
	(Vec)PetscToPointer((G) ));
}
PETSC_EXTERN void  sneslinesearchgetreason_(SNESLineSearch linesearch,SNESLineSearchReason *result, int *__ierr)
{
*__ierr = SNESLineSearchGetReason(
	(SNESLineSearch)PetscToPointer((linesearch) ),result);
}
PETSC_EXTERN void  sneslinesearchsetreason_(SNESLineSearch linesearch,SNESLineSearchReason *result, int *__ierr)
{
*__ierr = SNESLineSearchSetReason(
	(SNESLineSearch)PetscToPointer((linesearch) ),*result);
}
#if defined(__cplusplus)
}
#endif

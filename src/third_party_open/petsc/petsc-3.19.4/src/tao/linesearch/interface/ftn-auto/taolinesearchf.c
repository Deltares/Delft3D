#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* taolinesearch.c */
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

#include "petsctaolinesearch.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchsetup_ TAOLINESEARCHSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchsetup_ taolinesearchsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchreset_ TAOLINESEARCHRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchreset_ taolinesearchreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchdestroy_ TAOLINESEARCHDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchdestroy_ taolinesearchdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchapply_ TAOLINESEARCHAPPLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchapply_ taolinesearchapply
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchsetfromoptions_ TAOLINESEARCHSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchsetfromoptions_ taolinesearchsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetnumberfunctionevaluations_ TAOLINESEARCHGETNUMBERFUNCTIONEVALUATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetnumberfunctionevaluations_ taolinesearchgetnumberfunctionevaluations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchisusingtaoroutines_ TAOLINESEARCHISUSINGTAOROUTINES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchisusingtaoroutines_ taolinesearchisusingtaoroutines
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchcomputeobjective_ TAOLINESEARCHCOMPUTEOBJECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchcomputeobjective_ taolinesearchcomputeobjective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchcomputeobjectiveandgradient_ TAOLINESEARCHCOMPUTEOBJECTIVEANDGRADIENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchcomputeobjectiveandgradient_ taolinesearchcomputeobjectiveandgradient
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchcomputegradient_ TAOLINESEARCHCOMPUTEGRADIENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchcomputegradient_ taolinesearchcomputegradient
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchcomputeobjectiveandgts_ TAOLINESEARCHCOMPUTEOBJECTIVEANDGTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchcomputeobjectiveandgts_ taolinesearchcomputeobjectiveandgts
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetsolution_ TAOLINESEARCHGETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetsolution_ taolinesearchgetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetstartingvector_ TAOLINESEARCHGETSTARTINGVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetstartingvector_ taolinesearchgetstartingvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetstepdirection_ TAOLINESEARCHGETSTEPDIRECTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetstepdirection_ taolinesearchgetstepdirection
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetfullstepobjective_ TAOLINESEARCHGETFULLSTEPOBJECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetfullstepobjective_ taolinesearchgetfullstepobjective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchsetvariablebounds_ TAOLINESEARCHSETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchsetvariablebounds_ taolinesearchsetvariablebounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchsetinitialsteplength_ TAOLINESEARCHSETINITIALSTEPLENGTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchsetinitialsteplength_ taolinesearchsetinitialsteplength
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolinesearchgetsteplength_ TAOLINESEARCHGETSTEPLENGTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolinesearchgetsteplength_ taolinesearchgetsteplength
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taolinesearchsetup_(TaoLineSearch ls, int *__ierr)
{
*__ierr = TaoLineSearchSetUp(
	(TaoLineSearch)PetscToPointer((ls) ));
}
PETSC_EXTERN void  taolinesearchreset_(TaoLineSearch ls, int *__ierr)
{
*__ierr = TaoLineSearchReset(
	(TaoLineSearch)PetscToPointer((ls) ));
}
PETSC_EXTERN void  taolinesearchdestroy_(TaoLineSearch *ls, int *__ierr)
{
*__ierr = TaoLineSearchDestroy(ls);
}
PETSC_EXTERN void  taolinesearchapply_(TaoLineSearch ls,Vec x,PetscReal *f,Vec g,Vec s,PetscReal *steplength,TaoLineSearchConvergedReason *reason, int *__ierr)
{
*__ierr = TaoLineSearchApply(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),f,
	(Vec)PetscToPointer((g) ),
	(Vec)PetscToPointer((s) ),steplength,
	((reason) ));
}
PETSC_EXTERN void  taolinesearchsetfromoptions_(TaoLineSearch ls, int *__ierr)
{
*__ierr = TaoLineSearchSetFromOptions(
	(TaoLineSearch)PetscToPointer((ls) ));
}
PETSC_EXTERN void  taolinesearchgetnumberfunctionevaluations_(TaoLineSearch ls,PetscInt *nfeval,PetscInt *ngeval,PetscInt *nfgeval, int *__ierr)
{
*__ierr = TaoLineSearchGetNumberFunctionEvaluations(
	(TaoLineSearch)PetscToPointer((ls) ),nfeval,ngeval,nfgeval);
}
PETSC_EXTERN void  taolinesearchisusingtaoroutines_(TaoLineSearch ls,PetscBool *flg, int *__ierr)
{
*__ierr = TaoLineSearchIsUsingTaoRoutines(
	(TaoLineSearch)PetscToPointer((ls) ),flg);
}
PETSC_EXTERN void  taolinesearchcomputeobjective_(TaoLineSearch ls,Vec x,PetscReal *f, int *__ierr)
{
*__ierr = TaoLineSearchComputeObjective(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),f);
}
PETSC_EXTERN void  taolinesearchcomputeobjectiveandgradient_(TaoLineSearch ls,Vec x,PetscReal *f,Vec g, int *__ierr)
{
*__ierr = TaoLineSearchComputeObjectiveAndGradient(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),f,
	(Vec)PetscToPointer((g) ));
}
PETSC_EXTERN void  taolinesearchcomputegradient_(TaoLineSearch ls,Vec x,Vec g, int *__ierr)
{
*__ierr = TaoLineSearchComputeGradient(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((g) ));
}
PETSC_EXTERN void  taolinesearchcomputeobjectiveandgts_(TaoLineSearch ls,Vec x,PetscReal *f,PetscReal *gts, int *__ierr)
{
*__ierr = TaoLineSearchComputeObjectiveAndGTS(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),f,gts);
}
PETSC_EXTERN void  taolinesearchgetsolution_(TaoLineSearch ls,Vec x,PetscReal *f,Vec g,PetscReal *steplength,TaoLineSearchConvergedReason *reason, int *__ierr)
{
*__ierr = TaoLineSearchGetSolution(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((x) ),f,
	(Vec)PetscToPointer((g) ),steplength,
	((reason) ));
}
PETSC_EXTERN void  taolinesearchgetstartingvector_(TaoLineSearch ls,Vec *x, int *__ierr)
{
*__ierr = TaoLineSearchGetStartingVector(
	(TaoLineSearch)PetscToPointer((ls) ),x);
}
PETSC_EXTERN void  taolinesearchgetstepdirection_(TaoLineSearch ls,Vec *s, int *__ierr)
{
*__ierr = TaoLineSearchGetStepDirection(
	(TaoLineSearch)PetscToPointer((ls) ),s);
}

PETSC_EXTERN void  taolinesearchgetfullstepobjective_(TaoLineSearch ls,PetscReal *f_fullstep, int *__ierr)
{
*__ierr = TaoLineSearchGetFullStepObjective(
	(TaoLineSearch)PetscToPointer((ls) ),f_fullstep);
}
PETSC_EXTERN void  taolinesearchsetvariablebounds_(TaoLineSearch ls,Vec xl,Vec xu, int *__ierr)
{
*__ierr = TaoLineSearchSetVariableBounds(
	(TaoLineSearch)PetscToPointer((ls) ),
	(Vec)PetscToPointer((xl) ),
	(Vec)PetscToPointer((xu) ));
}
PETSC_EXTERN void  taolinesearchsetinitialsteplength_(TaoLineSearch ls,PetscReal *s, int *__ierr)
{
*__ierr = TaoLineSearchSetInitialStepLength(
	(TaoLineSearch)PetscToPointer((ls) ),*s);
}
PETSC_EXTERN void  taolinesearchgetsteplength_(TaoLineSearch ls,PetscReal *s, int *__ierr)
{
*__ierr = TaoLineSearchGetStepLength(
	(TaoLineSearch)PetscToPointer((ls) ),s);
}
#if defined(__cplusplus)
}
#endif

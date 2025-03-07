#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* traj.c */
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

#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectoryset_ TSTRAJECTORYSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectoryset_ tstrajectoryset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorygetnumsteps_ TSTRAJECTORYGETNUMSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorygetnumsteps_ tstrajectorygetnumsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectoryget_ TSTRAJECTORYGET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectoryget_ tstrajectoryget
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorygetvecs_ TSTRAJECTORYGETVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorygetvecs_ tstrajectorygetvecs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorycreate_ TSTRAJECTORYCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorycreate_ tstrajectorycreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectoryreset_ TSTRAJECTORYRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectoryreset_ tstrajectoryreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorydestroy_ TSTRAJECTORYDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorydestroy_ tstrajectorydestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetusehistory_ TSTRAJECTORYSETUSEHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetusehistory_ tstrajectorysetusehistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetmonitor_ TSTRAJECTORYSETMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetmonitor_ tstrajectorysetmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetkeepfiles_ TSTRAJECTORYSETKEEPFILES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetkeepfiles_ tstrajectorysetkeepfiles
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetfromoptions_ TSTRAJECTORYSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetfromoptions_ tstrajectorysetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetup_ TSTRAJECTORYSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetup_ tstrajectorysetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorysetsolutiononly_ TSTRAJECTORYSETSOLUTIONONLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorysetsolutiononly_ tstrajectorysetsolutiononly
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorygetsolutiononly_ TSTRAJECTORYGETSOLUTIONONLY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorygetsolutiononly_ tstrajectorygetsolutiononly
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectorygetupdatedhistoryvecs_ TSTRAJECTORYGETUPDATEDHISTORYVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectorygetupdatedhistoryvecs_ tstrajectorygetupdatedhistoryvecs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define tstrajectoryrestoreupdatedhistoryvecs_ TSTRAJECTORYRESTOREUPDATEDHISTORYVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define tstrajectoryrestoreupdatedhistoryvecs_ tstrajectoryrestoreupdatedhistoryvecs
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  tstrajectoryset_(TSTrajectory tj,TS ts,PetscInt *stepnum,PetscReal *time,Vec X, int *__ierr)
{
*__ierr = TSTrajectorySet(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ),*stepnum,*time,
	(Vec)PetscToPointer((X) ));
}
PETSC_EXTERN void  tstrajectorygetnumsteps_(TSTrajectory tj,PetscInt *steps, int *__ierr)
{
*__ierr = TSTrajectoryGetNumSteps(
	(TSTrajectory)PetscToPointer((tj) ),steps);
}
PETSC_EXTERN void  tstrajectoryget_(TSTrajectory tj,TS ts,PetscInt *stepnum,PetscReal *time, int *__ierr)
{
*__ierr = TSTrajectoryGet(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ),*stepnum,time);
}
PETSC_EXTERN void  tstrajectorygetvecs_(TSTrajectory tj,TS ts,PetscInt *stepnum,PetscReal *time,Vec U,Vec Udot, int *__ierr)
{
*__ierr = TSTrajectoryGetVecs(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ),*stepnum,time,
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((Udot) ));
}
PETSC_EXTERN void  tstrajectorycreate_(MPI_Fint * comm,TSTrajectory *tj, int *__ierr)
{
*__ierr = TSTrajectoryCreate(
	MPI_Comm_f2c(*(comm)),tj);
}
PETSC_EXTERN void  tstrajectoryreset_(TSTrajectory tj, int *__ierr)
{
*__ierr = TSTrajectoryReset(
	(TSTrajectory)PetscToPointer((tj) ));
}
PETSC_EXTERN void  tstrajectorydestroy_(TSTrajectory *tj, int *__ierr)
{
*__ierr = TSTrajectoryDestroy(tj);
}
PETSC_EXTERN void  tstrajectorysetusehistory_(TSTrajectory tj,PetscBool *flg, int *__ierr)
{
*__ierr = TSTrajectorySetUseHistory(
	(TSTrajectory)PetscToPointer((tj) ),*flg);
}
PETSC_EXTERN void  tstrajectorysetmonitor_(TSTrajectory tj,PetscBool *flg, int *__ierr)
{
*__ierr = TSTrajectorySetMonitor(
	(TSTrajectory)PetscToPointer((tj) ),*flg);
}
PETSC_EXTERN void  tstrajectorysetkeepfiles_(TSTrajectory tj,PetscBool *flg, int *__ierr)
{
*__ierr = TSTrajectorySetKeepFiles(
	(TSTrajectory)PetscToPointer((tj) ),*flg);
}
PETSC_EXTERN void  tstrajectorysetfromoptions_(TSTrajectory tj,TS ts, int *__ierr)
{
*__ierr = TSTrajectorySetFromOptions(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tstrajectorysetup_(TSTrajectory tj,TS ts, int *__ierr)
{
*__ierr = TSTrajectorySetUp(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void  tstrajectorysetsolutiononly_(TSTrajectory tj,PetscBool *solution_only, int *__ierr)
{
*__ierr = TSTrajectorySetSolutionOnly(
	(TSTrajectory)PetscToPointer((tj) ),*solution_only);
}
PETSC_EXTERN void  tstrajectorygetsolutiononly_(TSTrajectory tj,PetscBool *solution_only, int *__ierr)
{
*__ierr = TSTrajectoryGetSolutionOnly(
	(TSTrajectory)PetscToPointer((tj) ),solution_only);
}
PETSC_EXTERN void  tstrajectorygetupdatedhistoryvecs_(TSTrajectory tj,TS ts,PetscReal *time,Vec *U,Vec *Udot, int *__ierr)
{
*__ierr = TSTrajectoryGetUpdatedHistoryVecs(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ),*time,U,Udot);
}
PETSC_EXTERN void  tstrajectoryrestoreupdatedhistoryvecs_(TSTrajectory tj,Vec *U,Vec *Udot, int *__ierr)
{
*__ierr = TSTrajectoryRestoreUpdatedHistoryVecs(
	(TSTrajectory)PetscToPointer((tj) ),U,Udot);
}
#if defined(__cplusplus)
}
#endif

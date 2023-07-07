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


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  tstrajectorycreate_(MPI_Fint * comm,TSTrajectory *tj, int *__ierr){
*__ierr = TSTrajectoryCreate(
	MPI_Comm_f2c(*(comm)),tj);
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectoryreset_(TSTrajectory tj, int *__ierr){
*__ierr = TSTrajectoryReset(
	(TSTrajectory)PetscToPointer((tj) ));
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectorydestroy_(TSTrajectory *tj, int *__ierr){
*__ierr = TSTrajectoryDestroy(tj);
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectorysetmonitor_(TSTrajectory tj,PetscBool *flg, int *__ierr){
*__ierr = TSTrajectorySetMonitor(
	(TSTrajectory)PetscToPointer((tj) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectorysetkeepfiles_(TSTrajectory tj,PetscBool *flg, int *__ierr){
*__ierr = TSTrajectorySetKeepFiles(
	(TSTrajectory)PetscToPointer((tj) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectorysetfromoptions_(TSTrajectory tj,TS ts, int *__ierr){
*__ierr = TSTrajectorySetFromOptions(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ));
}
PETSC_EXTERN void PETSC_STDCALL  tstrajectorysetup_(TSTrajectory tj,TS ts, int *__ierr){
*__ierr = TSTrajectorySetUp(
	(TSTrajectory)PetscToPointer((tj) ),
	(TS)PetscToPointer((ts) ));
}
#if defined(__cplusplus)
}
#endif

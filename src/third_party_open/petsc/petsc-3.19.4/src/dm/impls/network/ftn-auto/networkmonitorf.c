#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* networkmonitor.c */
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

#include "petscdmnetwork.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkmonitorcreate_ DMNETWORKMONITORCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkmonitorcreate_ dmnetworkmonitorcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkmonitordestroy_ DMNETWORKMONITORDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkmonitordestroy_ dmnetworkmonitordestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkmonitorpop_ DMNETWORKMONITORPOP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkmonitorpop_ dmnetworkmonitorpop
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmnetworkmonitorview_ DMNETWORKMONITORVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmnetworkmonitorview_ dmnetworkmonitorview
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmnetworkmonitorcreate_(DM network,DMNetworkMonitor *monitorptr, int *__ierr)
{
*__ierr = DMNetworkMonitorCreate(
	(DM)PetscToPointer((network) ),monitorptr);
}
PETSC_EXTERN void  dmnetworkmonitordestroy_(DMNetworkMonitor *monitor, int *__ierr)
{
*__ierr = DMNetworkMonitorDestroy(monitor);
}
PETSC_EXTERN void  dmnetworkmonitorpop_(DMNetworkMonitor monitor, int *__ierr)
{
*__ierr = DMNetworkMonitorPop(
	(DMNetworkMonitor)PetscToPointer((monitor) ));
}

PETSC_EXTERN void  dmnetworkmonitorview_(DMNetworkMonitor monitor,Vec x, int *__ierr)
{
*__ierr = DMNetworkMonitorView(
	(DMNetworkMonitor)PetscToPointer((monitor) ),
	(Vec)PetscToPointer((x) ));
}
#if defined(__cplusplus)
}
#endif

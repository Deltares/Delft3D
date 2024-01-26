#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* swarm.c */
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

#include "petscdmswarm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmfinalizefieldregister_ DMSWARMFINALIZEFIELDREGISTER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmfinalizefieldregister_ dmswarmfinalizefieldregister
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsetlocalsizes_ DMSWARMSETLOCALSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsetlocalsizes_ dmswarmsetlocalsizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsetcelldm_ DMSWARMSETCELLDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsetcelldm_ dmswarmsetcelldm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmgetcelldm_ DMSWARMGETCELLDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmgetcelldm_ dmswarmgetcelldm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmgetlocalsize_ DMSWARMGETLOCALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmgetlocalsize_ dmswarmgetlocalsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmgetsize_ DMSWARMGETSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmgetsize_ dmswarmgetsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmaddpoint_ DMSWARMADDPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmaddpoint_ dmswarmaddpoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmaddnpoints_ DMSWARMADDNPOINTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmaddnpoints_ dmswarmaddnpoints
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmremovepoint_ DMSWARMREMOVEPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmremovepoint_ dmswarmremovepoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmremovepointatindex_ DMSWARMREMOVEPOINTATINDEX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmremovepointatindex_ dmswarmremovepointatindex
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmcopypoint_ DMSWARMCOPYPOINT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmcopypoint_ dmswarmcopypoint
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmmigrate_ DMSWARMMIGRATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmmigrate_ dmswarmmigrate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmcollectviewcreate_ DMSWARMCOLLECTVIEWCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmcollectviewcreate_ dmswarmcollectviewcreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmcollectviewdestroy_ DMSWARMCOLLECTVIEWDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmcollectviewdestroy_ dmswarmcollectviewdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsetpointcoordinatesrandom_ DMSWARMSETPOINTCOORDINATESRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsetpointcoordinatesrandom_ dmswarmsetpointcoordinatesrandom
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmswarmsettype_ DMSWARMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmswarmsettype_ dmswarmsettype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmswarmfinalizefieldregister_(DM dm, int *__ierr)
{
*__ierr = DMSwarmFinalizeFieldRegister(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmswarmsetlocalsizes_(DM dm,PetscInt *nlocal,PetscInt *buffer, int *__ierr)
{
*__ierr = DMSwarmSetLocalSizes(
	(DM)PetscToPointer((dm) ),*nlocal,*buffer);
}
PETSC_EXTERN void  dmswarmsetcelldm_(DM dm,DM dmcell, int *__ierr)
{
*__ierr = DMSwarmSetCellDM(
	(DM)PetscToPointer((dm) ),
	(DM)PetscToPointer((dmcell) ));
}
PETSC_EXTERN void  dmswarmgetcelldm_(DM dm,DM *dmcell, int *__ierr)
{
*__ierr = DMSwarmGetCellDM(
	(DM)PetscToPointer((dm) ),dmcell);
}
PETSC_EXTERN void  dmswarmgetlocalsize_(DM dm,PetscInt *nlocal, int *__ierr)
{
*__ierr = DMSwarmGetLocalSize(
	(DM)PetscToPointer((dm) ),nlocal);
}
PETSC_EXTERN void  dmswarmgetsize_(DM dm,PetscInt *n, int *__ierr)
{
*__ierr = DMSwarmGetSize(
	(DM)PetscToPointer((dm) ),n);
}
PETSC_EXTERN void  dmswarmaddpoint_(DM dm, int *__ierr)
{
*__ierr = DMSwarmAddPoint(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmswarmaddnpoints_(DM dm,PetscInt *npoints, int *__ierr)
{
*__ierr = DMSwarmAddNPoints(
	(DM)PetscToPointer((dm) ),*npoints);
}
PETSC_EXTERN void  dmswarmremovepoint_(DM dm, int *__ierr)
{
*__ierr = DMSwarmRemovePoint(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmswarmremovepointatindex_(DM dm,PetscInt *idx, int *__ierr)
{
*__ierr = DMSwarmRemovePointAtIndex(
	(DM)PetscToPointer((dm) ),*idx);
}
PETSC_EXTERN void  dmswarmcopypoint_(DM dm,PetscInt *pi,PetscInt *pj, int *__ierr)
{
*__ierr = DMSwarmCopyPoint(
	(DM)PetscToPointer((dm) ),*pi,*pj);
}
PETSC_EXTERN void  dmswarmmigrate_(DM dm,PetscBool *remove_sent_points, int *__ierr)
{
*__ierr = DMSwarmMigrate(
	(DM)PetscToPointer((dm) ),*remove_sent_points);
}
PETSC_EXTERN void  dmswarmcollectviewcreate_(DM dm, int *__ierr)
{
*__ierr = DMSwarmCollectViewCreate(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmswarmcollectviewdestroy_(DM dm, int *__ierr)
{
*__ierr = DMSwarmCollectViewDestroy(
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmswarmsetpointcoordinatesrandom_(DM dm,PetscInt *Npc, int *__ierr)
{
*__ierr = DMSwarmSetPointCoordinatesRandom(
	(DM)PetscToPointer((dm) ),*Npc);
}
PETSC_EXTERN void  dmswarmsettype_(DM dm,DMSwarmType *stype, int *__ierr)
{
*__ierr = DMSwarmSetType(
	(DM)PetscToPointer((dm) ),*stype);
}
#if defined(__cplusplus)
}
#endif

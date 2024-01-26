#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* gamg.c */
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

#include "petscpc.h"
#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetproceqlim_ PCGAMGSETPROCEQLIM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetproceqlim_ pcgamgsetproceqlim
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetcoarseeqlim_ PCGAMGSETCOARSEEQLIM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetcoarseeqlim_ pcgamgsetcoarseeqlim
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetrepartition_ PCGAMGSETREPARTITION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetrepartition_ pcgamgsetrepartition
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetusesaesteig_ PCGAMGSETUSESAESTEIG
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetusesaesteig_ pcgamgsetusesaesteig
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgseteigenvalues_ PCGAMGSETEIGENVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgseteigenvalues_ pcgamgseteigenvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetreuseinterpolation_ PCGAMGSETREUSEINTERPOLATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetreuseinterpolation_ pcgamgsetreuseinterpolation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgasmsetuseaggs_ PCGAMGASMSETUSEAGGS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgasmsetuseaggs_ pcgamgasmsetuseaggs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetuseparallelcoarsegridsolve_ PCGAMGSETUSEPARALLELCOARSEGRIDSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetuseparallelcoarsegridsolve_ pcgamgsetuseparallelcoarsegridsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetcpupincoarsegrids_ PCGAMGSETCPUPINCOARSEGRIDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetcpupincoarsegrids_ pcgamgsetcpupincoarsegrids
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetcoarsegridlayouttype_ PCGAMGSETCOARSEGRIDLAYOUTTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetcoarsegridlayouttype_ pcgamgsetcoarsegridlayouttype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetnlevels_ PCGAMGSETNLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetnlevels_ pcgamgsetnlevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetthreshold_ PCGAMGSETTHRESHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetthreshold_ pcgamgsetthreshold
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetrankreductionfactors_ PCGAMGSETRANKREDUCTIONFACTORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetrankreductionfactors_ pcgamgsetrankreductionfactors
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgamgsetthresholdscale_ PCGAMGSETTHRESHOLDSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetthresholdscale_ pcgamgsetthresholdscale
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcgamgsetproceqlim_(PC pc,PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetProcEqLim(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgsetcoarseeqlim_(PC pc,PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetCoarseEqLim(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgsetrepartition_(PC pc,PetscBool *n, int *__ierr)
{
*__ierr = PCGAMGSetRepartition(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgsetusesaesteig_(PC pc,PetscBool *n, int *__ierr)
{
*__ierr = PCGAMGSetUseSAEstEig(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgseteigenvalues_(PC pc,PetscReal *emax,PetscReal *emin, int *__ierr)
{
*__ierr = PCGAMGSetEigenvalues(
	(PC)PetscToPointer((pc) ),*emax,*emin);
}
PETSC_EXTERN void  pcgamgsetreuseinterpolation_(PC pc,PetscBool *n, int *__ierr)
{
*__ierr = PCGAMGSetReuseInterpolation(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgasmsetuseaggs_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGAMGASMSetUseAggs(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgamgsetuseparallelcoarsegridsolve_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGAMGSetUseParallelCoarseGridSolve(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgamgsetcpupincoarsegrids_(PC pc,PetscBool *flg, int *__ierr)
{
*__ierr = PCGAMGSetCpuPinCoarseGrids(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgamgsetcoarsegridlayouttype_(PC pc,PCGAMGLayoutType *flg, int *__ierr)
{
*__ierr = PCGAMGSetCoarseGridLayoutType(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void  pcgamgsetnlevels_(PC pc,PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetNlevels(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void  pcgamgsetthreshold_(PC pc,PetscReal v[],PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetThreshold(
	(PC)PetscToPointer((pc) ),v,*n);
}
PETSC_EXTERN void  pcgamgsetrankreductionfactors_(PC pc,PetscInt v[],PetscInt *n, int *__ierr)
{
*__ierr = PCGAMGSetRankReductionFactors(
	(PC)PetscToPointer((pc) ),v,*n);
}
PETSC_EXTERN void  pcgamgsetthresholdscale_(PC pc,PetscReal *v, int *__ierr)
{
*__ierr = PCGAMGSetThresholdScale(
	(PC)PetscToPointer((pc) ),*v);
}
#if defined(__cplusplus)
}
#endif

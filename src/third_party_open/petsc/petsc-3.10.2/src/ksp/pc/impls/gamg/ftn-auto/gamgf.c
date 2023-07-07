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
#define pcgamgsetthresholdscale_ PCGAMGSETTHRESHOLDSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgamgsetthresholdscale_ pcgamgsetthresholdscale
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetproceqlim_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetProcEqLim(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetcoarseeqlim_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetCoarseEqLim(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetrepartition_(PC pc,PetscBool *n, int *__ierr){
*__ierr = PCGAMGSetRepartition(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetreuseinterpolation_(PC pc,PetscBool *n, int *__ierr){
*__ierr = PCGAMGSetReuseInterpolation(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgasmsetuseaggs_(PC pc,PetscBool *flg, int *__ierr){
*__ierr = PCGAMGASMSetUseAggs(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetuseparallelcoarsegridsolve_(PC pc,PetscBool *flg, int *__ierr){
*__ierr = PCGAMGSetUseParallelCoarseGridSolve(
	(PC)PetscToPointer((pc) ),*flg);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetnlevels_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetNlevels(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetthreshold_(PC pc,PetscReal v[],PetscInt *n, int *__ierr){
*__ierr = PCGAMGSetThreshold(
	(PC)PetscToPointer((pc) ),v,*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcgamgsetthresholdscale_(PC pc,PetscReal *v, int *__ierr){
*__ierr = PCGAMGSetThresholdScale(
	(PC)PetscToPointer((pc) ),*v);
}
#if defined(__cplusplus)
}
#endif

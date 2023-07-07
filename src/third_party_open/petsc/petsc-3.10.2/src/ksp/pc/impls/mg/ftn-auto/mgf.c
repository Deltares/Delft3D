#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mg.c */
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
#define pcmggetlevels_ PCMGGETLEVELS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetlevels_ pcmggetlevels
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsettype_ PCMGSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsettype_ pcmgsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggettype_ PCMGGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggettype_ pcmggettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetcycletype_ PCMGSETCYCLETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetcycletype_ pcmgsetcycletype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgmultiplicativesetcycles_ PCMGMULTIPLICATIVESETCYCLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgmultiplicativesetcycles_ pcmgmultiplicativesetcycles
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetgalerkin_ PCMGSETGALERKIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetgalerkin_ pcmgsetgalerkin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmggetgalerkin_ PCMGGETGALERKIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmggetgalerkin_ pcmggetgalerkin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetnumbersmooth_ PCMGSETNUMBERSMOOTH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetnumbersmooth_ pcmgsetnumbersmooth
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcmgsetdistinctsmoothup_ PCMGSETDISTINCTSMOOTHUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcmgsetdistinctsmoothup_ pcmgsetdistinctsmoothup
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcmggetlevels_(PC pc,PetscInt *levels, int *__ierr){
*__ierr = PCMGGetLevels(
	(PC)PetscToPointer((pc) ),levels);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgsettype_(PC pc,PCMGType *form, int *__ierr){
*__ierr = PCMGSetType(
	(PC)PetscToPointer((pc) ),*form);
}
PETSC_EXTERN void PETSC_STDCALL  pcmggettype_(PC pc,PCMGType *type, int *__ierr){
*__ierr = PCMGGetType(
	(PC)PetscToPointer((pc) ),type);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgsetcycletype_(PC pc,PCMGCycleType *n, int *__ierr){
*__ierr = PCMGSetCycleType(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgmultiplicativesetcycles_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCMGMultiplicativeSetCycles(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgsetgalerkin_(PC pc,PCMGGalerkinType *use, int *__ierr){
*__ierr = PCMGSetGalerkin(
	(PC)PetscToPointer((pc) ),*use);
}
PETSC_EXTERN void PETSC_STDCALL  pcmggetgalerkin_(PC pc,PCMGGalerkinType  *galerkin, int *__ierr){
*__ierr = PCMGGetGalerkin(
	(PC)PetscToPointer((pc) ),galerkin);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgsetnumbersmooth_(PC pc,PetscInt *n, int *__ierr){
*__ierr = PCMGSetNumberSmooth(
	(PC)PetscToPointer((pc) ),*n);
}
PETSC_EXTERN void PETSC_STDCALL  pcmgsetdistinctsmoothup_(PC pc, int *__ierr){
*__ierr = PCMGSetDistinctSmoothUp(
	(PC)PetscToPointer((pc) ));
}
#if defined(__cplusplus)
}
#endif

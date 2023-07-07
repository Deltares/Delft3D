#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* shell.c */
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

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshellgetcontext_ MATSHELLGETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshellgetcontext_ matshellgetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshellsetcontext_ MATSHELLSETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshellsetcontext_ matshellsetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshellsetmanagescalingshifts_ MATSHELLSETMANAGESCALINGSHIFTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshellsetmanagescalingshifts_ matshellsetmanagescalingshifts
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matshellgetcontext_(Mat mat,void*ctx, int *__ierr){
*__ierr = MatShellGetContext(
	(Mat)PetscToPointer((mat) ),ctx);
}
PETSC_EXTERN void PETSC_STDCALL  matshellsetcontext_(Mat mat,void*ctx, int *__ierr){
*__ierr = MatShellSetContext(
	(Mat)PetscToPointer((mat) ),ctx);
}
PETSC_EXTERN void PETSC_STDCALL  matshellsetmanagescalingshifts_(Mat A, int *__ierr){
*__ierr = MatShellSetManageScalingShifts(
	(Mat)PetscToPointer((A) ));
}
#if defined(__cplusplus)
}
#endif

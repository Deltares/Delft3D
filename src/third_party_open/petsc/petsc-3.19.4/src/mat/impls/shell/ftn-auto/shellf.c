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
#define matshellsetcontextdestroy_ MATSHELLSETCONTEXTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshellsetcontextdestroy_ matshellsetcontextdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matshellsetmanagescalingshifts_ MATSHELLSETMANAGESCALINGSHIFTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matshellsetmanagescalingshifts_ matshellsetmanagescalingshifts
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisshell_ MATISSHELL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisshell_ matisshell
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matshellgetcontext_(Mat mat,void*ctx, int *__ierr)
{
*__ierr = MatShellGetContext(
	(Mat)PetscToPointer((mat) ),ctx);
}
PETSC_EXTERN void  matshellsetcontext_(Mat mat,void*ctx, int *__ierr)
{
*__ierr = MatShellSetContext(
	(Mat)PetscToPointer((mat) ),ctx);
}
PETSC_EXTERN void  matshellsetcontextdestroy_(Mat mat,PetscErrorCode (*f)(void *), int *__ierr)
{
*__ierr = MatShellSetContextDestroy(
	(Mat)PetscToPointer((mat) ),f);
}
PETSC_EXTERN void  matshellsetmanagescalingshifts_(Mat A, int *__ierr)
{
*__ierr = MatShellSetManageScalingShifts(
	(Mat)PetscToPointer((A) ));
}
PETSC_EXTERN void  matisshell_(Mat mat,PetscBool *flg, int *__ierr)
{
*__ierr = MatIsShell(
	(Mat)PetscToPointer((mat) ),flg);
}
#if defined(__cplusplus)
}
#endif

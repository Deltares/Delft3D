#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* taoshell.c */
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

#include "petsctao.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoshellgetcontext_ TAOSHELLGETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoshellgetcontext_ taoshellgetcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoshellsetcontext_ TAOSHELLSETCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoshellsetcontext_ taoshellsetcontext
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taoshellgetcontext_(Tao tao,void*ctx, int *__ierr)
{
*__ierr = TaoShellGetContext(
	(Tao)PetscToPointer((tao) ),ctx);
}
PETSC_EXTERN void  taoshellsetcontext_(Tao tao,void*ctx, int *__ierr)
{
*__ierr = TaoShellSetContext(
	(Tao)PetscToPointer((tao) ),ctx);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* blmvm.c */
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

#include "petsctaolinesearch.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolmvmrecycle_ TAOLMVMRECYCLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolmvmrecycle_ taolmvmrecycle
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolmvmseth0_ TAOLMVMSETH0
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolmvmseth0_ taolmvmseth0
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolmvmgeth0_ TAOLMVMGETH0
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolmvmgeth0_ taolmvmgeth0
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taolmvmgeth0ksp_ TAOLMVMGETH0KSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taolmvmgeth0ksp_ taolmvmgeth0ksp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taolmvmrecycle_(Tao tao,PetscBool *flg, int *__ierr)
{
*__ierr = TaoLMVMRecycle(
	(Tao)PetscToPointer((tao) ),*flg);
}
PETSC_EXTERN void  taolmvmseth0_(Tao tao,Mat H0, int *__ierr)
{
*__ierr = TaoLMVMSetH0(
	(Tao)PetscToPointer((tao) ),
	(Mat)PetscToPointer((H0) ));
}
PETSC_EXTERN void  taolmvmgeth0_(Tao tao,Mat *H0, int *__ierr)
{
*__ierr = TaoLMVMGetH0(
	(Tao)PetscToPointer((tao) ),H0);
}
PETSC_EXTERN void  taolmvmgeth0ksp_(Tao tao,KSP *ksp, int *__ierr)
{
*__ierr = TaoLMVMGetH0KSP(
	(Tao)PetscToPointer((tao) ),ksp);
}
#if defined(__cplusplus)
}
#endif

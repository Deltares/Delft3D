#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matnull.c */
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
#define matnullspacecreaterigidbody_ MATNULLSPACECREATERIGIDBODY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnullspacecreaterigidbody_ matnullspacecreaterigidbody
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnullspacedestroy_ MATNULLSPACEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnullspacedestroy_ matnullspacedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnullspacetest_ MATNULLSPACETEST
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnullspacetest_ matnullspacetest
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  matnullspacecreaterigidbody_(Vec coords,MatNullSpace *sp, int *__ierr)
{
*__ierr = MatNullSpaceCreateRigidBody(
	(Vec)PetscToPointer((coords) ),sp);
}
PETSC_EXTERN void  matnullspacedestroy_(MatNullSpace *sp, int *__ierr)
{
*__ierr = MatNullSpaceDestroy(sp);
}
PETSC_EXTERN void  matnullspacetest_(MatNullSpace sp,Mat mat,PetscBool *isNull, int *__ierr)
{
*__ierr = MatNullSpaceTest(
	(MatNullSpace)PetscToPointer((sp) ),
	(Mat)PetscToPointer((mat) ),isNull);
}
#if defined(__cplusplus)
}
#endif

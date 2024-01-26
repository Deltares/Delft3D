#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dasub.c */
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

#include "petscdmda.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetlogicalcoordinate_ DMDAGETLOGICALCOORDINATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetlogicalcoordinate_ dmdagetlogicalcoordinate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmdagetray_ DMDAGETRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmdagetray_ dmdagetray
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmdagetlogicalcoordinate_(DM da,PetscScalar *x,PetscScalar *y,PetscScalar *z,PetscInt *II,PetscInt *JJ,PetscInt *KK,PetscScalar *X,PetscScalar *Y,PetscScalar *Z, int *__ierr)
{
*__ierr = DMDAGetLogicalCoordinate(
	(DM)PetscToPointer((da) ),*x,*y,*z,II,JJ,KK,X,Y,Z);
}
PETSC_EXTERN void  dmdagetray_(DM da,DMDirection *dir,PetscInt *gp,Vec *newvec,VecScatter *scatter, int *__ierr)
{
*__ierr = DMDAGetRay(
	(DM)PetscToPointer((da) ),*dir,*gp,newvec,scatter);
}
#if defined(__cplusplus)
}
#endif

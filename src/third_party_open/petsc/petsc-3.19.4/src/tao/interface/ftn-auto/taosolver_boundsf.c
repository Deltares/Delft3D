#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* taosolver_bounds.c */
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
#define taosetvariablebounds_ TAOSETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosetvariablebounds_ taosetvariablebounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taogetvariablebounds_ TAOGETVARIABLEBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taogetvariablebounds_ taogetvariablebounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taosetinequalitybounds_ TAOSETINEQUALITYBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosetinequalitybounds_ taosetinequalitybounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taogetinequalitybounds_ TAOGETINEQUALITYBOUNDS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taogetinequalitybounds_ taogetinequalitybounds
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taocomputedualvariables_ TAOCOMPUTEDUALVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taocomputedualvariables_ taocomputedualvariables
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taogetdualvariables_ TAOGETDUALVARIABLES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taogetdualvariables_ taogetdualvariables
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taosetvariablebounds_(Tao tao,Vec XL,Vec XU, int *__ierr)
{
*__ierr = TaoSetVariableBounds(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((XL) ),
	(Vec)PetscToPointer((XU) ));
}
PETSC_EXTERN void  taogetvariablebounds_(Tao tao,Vec *XL,Vec *XU, int *__ierr)
{
*__ierr = TaoGetVariableBounds(
	(Tao)PetscToPointer((tao) ),XL,XU);
}
PETSC_EXTERN void  taosetinequalitybounds_(Tao tao,Vec IL,Vec IU, int *__ierr)
{
*__ierr = TaoSetInequalityBounds(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((IL) ),
	(Vec)PetscToPointer((IU) ));
}
PETSC_EXTERN void  taogetinequalitybounds_(Tao tao,Vec *IL,Vec *IU, int *__ierr)
{
*__ierr = TaoGetInequalityBounds(
	(Tao)PetscToPointer((tao) ),IL,IU);
}
PETSC_EXTERN void  taocomputedualvariables_(Tao tao,Vec DL,Vec DU, int *__ierr)
{
*__ierr = TaoComputeDualVariables(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((DL) ),
	(Vec)PetscToPointer((DU) ));
}
PETSC_EXTERN void  taogetdualvariables_(Tao tao,Vec *DE,Vec *DI, int *__ierr)
{
*__ierr = TaoGetDualVariables(
	(Tao)PetscToPointer((tao) ),DE,DI);
}
#if defined(__cplusplus)
}
#endif

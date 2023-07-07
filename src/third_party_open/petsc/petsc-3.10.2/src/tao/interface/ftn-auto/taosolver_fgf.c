#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* taosolver_fg.c */
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
#define taosetinitialvector_ TAOSETINITIALVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosetinitialvector_ taosetinitialvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taocomputegradient_ TAOCOMPUTEGRADIENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taocomputegradient_ taocomputegradient
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taocomputeobjective_ TAOCOMPUTEOBJECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taocomputeobjective_ taocomputeobjective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taocomputeobjectiveandgradient_ TAOCOMPUTEOBJECTIVEANDGRADIENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taocomputeobjectiveandgradient_ taocomputeobjectiveandgradient
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taosetseparableobjectiveweights_ TAOSETSEPARABLEOBJECTIVEWEIGHTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosetseparableobjectiveweights_ taosetseparableobjectiveweights
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taocomputeseparableobjective_ TAOCOMPUTESEPARABLEOBJECTIVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taocomputeseparableobjective_ taocomputeseparableobjective
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoisobjectivedefined_ TAOISOBJECTIVEDEFINED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoisobjectivedefined_ taoisobjectivedefined
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoisgradientdefined_ TAOISGRADIENTDEFINED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoisgradientdefined_ taoisgradientdefined
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoisobjectiveandgradientdefined_ TAOISOBJECTIVEANDGRADIENTDEFINED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoisobjectiveandgradientdefined_ taoisobjectiveandgradientdefined
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

PETSC_EXTERN void PETSC_STDCALL  taosetinitialvector_(Tao tao,Vec x0, int *__ierr){
*__ierr = TaoSetInitialVector(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((x0) ));
}
PETSC_EXTERN void PETSC_STDCALL  taocomputegradient_(Tao tao,Vec X,Vec G, int *__ierr){
*__ierr = TaoComputeGradient(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((G) ));
}
PETSC_EXTERN void PETSC_STDCALL  taocomputeobjective_(Tao tao,Vec X,PetscReal *f, int *__ierr){
*__ierr = TaoComputeObjective(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((X) ),f);
}
PETSC_EXTERN void PETSC_STDCALL  taocomputeobjectiveandgradient_(Tao tao,Vec X,PetscReal *f,Vec G, int *__ierr){
*__ierr = TaoComputeObjectiveAndGradient(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((X) ),f,
	(Vec)PetscToPointer((G) ));
}
PETSC_EXTERN void PETSC_STDCALL  taosetseparableobjectiveweights_(Tao tao,Vec sigma_v,PetscInt *n,PetscInt *rows,PetscInt *cols,PetscReal *vals, int *__ierr){
*__ierr = TaoSetSeparableObjectiveWeights(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((sigma_v) ),*n,rows,cols,vals);
}
PETSC_EXTERN void PETSC_STDCALL  taocomputeseparableobjective_(Tao tao,Vec X,Vec F, int *__ierr){
*__ierr = TaoComputeSeparableObjective(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ));
}
PETSC_EXTERN void PETSC_STDCALL  taoisobjectivedefined_(Tao tao,PetscBool *flg, int *__ierr){
*__ierr = TaoIsObjectiveDefined(
	(Tao)PetscToPointer((tao) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  taoisgradientdefined_(Tao tao,PetscBool *flg, int *__ierr){
*__ierr = TaoIsGradientDefined(
	(Tao)PetscToPointer((tao) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  taoisobjectiveandgradientdefined_(Tao tao,PetscBool *flg, int *__ierr){
*__ierr = TaoIsObjectiveAndGradientDefined(
	(Tao)PetscToPointer((tao) ),flg);
}
#if defined(__cplusplus)
}
#endif

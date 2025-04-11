#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* tao_util.c */
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
#define vecfischer_ VECFISCHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecfischer_ vecfischer
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define vecsfischer_ VECSFISCHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define vecsfischer_ vecsfischer
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdfischer_ MATDFISCHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdfischer_ matdfischer
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdsfischer_ MATDSFISCHER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdsfischer_ matdsfischer
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taosoftthreshold_ TAOSOFTTHRESHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taosoftthreshold_ taosoftthreshold
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  vecfischer_(Vec X,Vec F,Vec L,Vec U,Vec FB, int *__ierr)
{
*__ierr = VecFischer(
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),
	(Vec)PetscToPointer((L) ),
	(Vec)PetscToPointer((U) ),
	(Vec)PetscToPointer((FB) ));
}
PETSC_EXTERN void  vecsfischer_(Vec X,Vec F,Vec L,Vec U,PetscReal *mu,Vec FB, int *__ierr)
{
*__ierr = VecSFischer(
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((F) ),
	(Vec)PetscToPointer((L) ),
	(Vec)PetscToPointer((U) ),*mu,
	(Vec)PetscToPointer((FB) ));
}
PETSC_EXTERN void  matdfischer_(Mat jac,Vec X,Vec Con,Vec XL,Vec XU,Vec T1,Vec T2,Vec Da,Vec Db, int *__ierr)
{
*__ierr = MatDFischer(
	(Mat)PetscToPointer((jac) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Con) ),
	(Vec)PetscToPointer((XL) ),
	(Vec)PetscToPointer((XU) ),
	(Vec)PetscToPointer((T1) ),
	(Vec)PetscToPointer((T2) ),
	(Vec)PetscToPointer((Da) ),
	(Vec)PetscToPointer((Db) ));
}
PETSC_EXTERN void  matdsfischer_(Mat jac,Vec X,Vec Con,Vec XL,Vec XU,PetscReal *mu,Vec T1,Vec T2,Vec Da,Vec Db,Vec Dm, int *__ierr)
{
*__ierr = MatDSFischer(
	(Mat)PetscToPointer((jac) ),
	(Vec)PetscToPointer((X) ),
	(Vec)PetscToPointer((Con) ),
	(Vec)PetscToPointer((XL) ),
	(Vec)PetscToPointer((XU) ),*mu,
	(Vec)PetscToPointer((T1) ),
	(Vec)PetscToPointer((T2) ),
	(Vec)PetscToPointer((Da) ),
	(Vec)PetscToPointer((Db) ),
	(Vec)PetscToPointer((Dm) ));
}
PETSC_EXTERN void  taosoftthreshold_(Vec in,PetscReal *lb,PetscReal *ub,Vec out, int *__ierr)
{
*__ierr = TaoSoftThreshold(
	(Vec)PetscToPointer((in) ),*lb,*ub,
	(Vec)PetscToPointer((out) ));
}
#if defined(__cplusplus)
}
#endif

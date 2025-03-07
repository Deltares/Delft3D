#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* almmutils.c */
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
#define taoalmmgettype_ TAOALMMGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmgettype_ taoalmmgettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmsettype_ TAOALMMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmsettype_ taoalmmsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmgetsubsolver_ TAOALMMGETSUBSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmgetsubsolver_ taoalmmgetsubsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmsetsubsolver_ TAOALMMSETSUBSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmsetsubsolver_ taoalmmsetsubsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmgetmultipliers_ TAOALMMGETMULTIPLIERS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmgetmultipliers_ taoalmmgetmultipliers
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmsetmultipliers_ TAOALMMSETMULTIPLIERS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmsetmultipliers_ taoalmmsetmultipliers
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmgetprimalis_ TAOALMMGETPRIMALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmgetprimalis_ taoalmmgetprimalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define taoalmmgetdualis_ TAOALMMGETDUALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define taoalmmgetdualis_ taoalmmgetdualis
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  taoalmmgettype_(Tao tao,TaoALMMType *type, int *__ierr)
{
*__ierr = TaoALMMGetType(
	(Tao)PetscToPointer((tao) ),type);
}
PETSC_EXTERN void  taoalmmsettype_(Tao tao,TaoALMMType *type, int *__ierr)
{
*__ierr = TaoALMMSetType(
	(Tao)PetscToPointer((tao) ),*type);
}
PETSC_EXTERN void  taoalmmgetsubsolver_(Tao tao,Tao *subsolver, int *__ierr)
{
*__ierr = TaoALMMGetSubsolver(
	(Tao)PetscToPointer((tao) ),subsolver);
}
PETSC_EXTERN void  taoalmmsetsubsolver_(Tao tao,Tao subsolver, int *__ierr)
{
*__ierr = TaoALMMSetSubsolver(
	(Tao)PetscToPointer((tao) ),
	(Tao)PetscToPointer((subsolver) ));
}
PETSC_EXTERN void  taoalmmgetmultipliers_(Tao tao,Vec *Y, int *__ierr)
{
*__ierr = TaoALMMGetMultipliers(
	(Tao)PetscToPointer((tao) ),Y);
}
PETSC_EXTERN void  taoalmmsetmultipliers_(Tao tao,Vec Y, int *__ierr)
{
*__ierr = TaoALMMSetMultipliers(
	(Tao)PetscToPointer((tao) ),
	(Vec)PetscToPointer((Y) ));
}
PETSC_EXTERN void  taoalmmgetprimalis_(Tao tao,IS *opt_is,IS *slack_is, int *__ierr)
{
*__ierr = TaoALMMGetPrimalIS(
	(Tao)PetscToPointer((tao) ),opt_is,slack_is);
}
PETSC_EXTERN void  taoalmmgetdualis_(Tao tao,IS *eq_is,IS *ineq_is, int *__ierr)
{
*__ierr = TaoALMMGetDualIS(
	(Tao)PetscToPointer((tao) ),eq_is,ineq_is);
}
#if defined(__cplusplus)
}
#endif

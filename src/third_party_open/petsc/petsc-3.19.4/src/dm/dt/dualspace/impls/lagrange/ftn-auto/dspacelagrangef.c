#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dspacelagrange.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegetcontinuity_ PETSCDUALSPACELAGRANGEGETCONTINUITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegetcontinuity_ petscdualspacelagrangegetcontinuity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesetcontinuity_ PETSCDUALSPACELAGRANGESETCONTINUITY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesetcontinuity_ petscdualspacelagrangesetcontinuity
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegettensor_ PETSCDUALSPACELAGRANGEGETTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegettensor_ petscdualspacelagrangegettensor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesettensor_ PETSCDUALSPACELAGRANGESETTENSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesettensor_ petscdualspacelagrangesettensor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegettrimmed_ PETSCDUALSPACELAGRANGEGETTRIMMED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegettrimmed_ petscdualspacelagrangegettrimmed
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesettrimmed_ PETSCDUALSPACELAGRANGESETTRIMMED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesettrimmed_ petscdualspacelagrangesettrimmed
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegetnodetype_ PETSCDUALSPACELAGRANGEGETNODETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegetnodetype_ petscdualspacelagrangegetnodetype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesetnodetype_ PETSCDUALSPACELAGRANGESETNODETYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesetnodetype_ petscdualspacelagrangesetnodetype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegetusemoments_ PETSCDUALSPACELAGRANGEGETUSEMOMENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegetusemoments_ petscdualspacelagrangegetusemoments
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesetusemoments_ PETSCDUALSPACELAGRANGESETUSEMOMENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesetusemoments_ petscdualspacelagrangesetusemoments
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangegetmomentorder_ PETSCDUALSPACELAGRANGEGETMOMENTORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangegetmomentorder_ petscdualspacelagrangegetmomentorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscdualspacelagrangesetmomentorder_ PETSCDUALSPACELAGRANGESETMOMENTORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscdualspacelagrangesetmomentorder_ petscdualspacelagrangesetmomentorder
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscdualspacelagrangegetcontinuity_(PetscDualSpace sp,PetscBool *continuous, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetContinuity(
	(PetscDualSpace)PetscToPointer((sp) ),continuous);
}
PETSC_EXTERN void  petscdualspacelagrangesetcontinuity_(PetscDualSpace sp,PetscBool *continuous, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetContinuity(
	(PetscDualSpace)PetscToPointer((sp) ),*continuous);
}
PETSC_EXTERN void  petscdualspacelagrangegettensor_(PetscDualSpace sp,PetscBool *tensor, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetTensor(
	(PetscDualSpace)PetscToPointer((sp) ),tensor);
}
PETSC_EXTERN void  petscdualspacelagrangesettensor_(PetscDualSpace sp,PetscBool *tensor, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetTensor(
	(PetscDualSpace)PetscToPointer((sp) ),*tensor);
}
PETSC_EXTERN void  petscdualspacelagrangegettrimmed_(PetscDualSpace sp,PetscBool *trimmed, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetTrimmed(
	(PetscDualSpace)PetscToPointer((sp) ),trimmed);
}
PETSC_EXTERN void  petscdualspacelagrangesettrimmed_(PetscDualSpace sp,PetscBool *trimmed, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetTrimmed(
	(PetscDualSpace)PetscToPointer((sp) ),*trimmed);
}
PETSC_EXTERN void  petscdualspacelagrangegetnodetype_(PetscDualSpace sp,PetscDTNodeType *nodeType,PetscBool *boundary,PetscReal *exponent, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetNodeType(
	(PetscDualSpace)PetscToPointer((sp) ),
	(PetscDTNodeType* )PetscToPointer((nodeType) ),boundary,exponent);
}
PETSC_EXTERN void  petscdualspacelagrangesetnodetype_(PetscDualSpace sp,PetscDTNodeType *nodeType,PetscBool *boundary,PetscReal *exponent, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetNodeType(
	(PetscDualSpace)PetscToPointer((sp) ),*nodeType,*boundary,*exponent);
}
PETSC_EXTERN void  petscdualspacelagrangegetusemoments_(PetscDualSpace sp,PetscBool *useMoments, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetUseMoments(
	(PetscDualSpace)PetscToPointer((sp) ),useMoments);
}
PETSC_EXTERN void  petscdualspacelagrangesetusemoments_(PetscDualSpace sp,PetscBool *useMoments, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetUseMoments(
	(PetscDualSpace)PetscToPointer((sp) ),*useMoments);
}
PETSC_EXTERN void  petscdualspacelagrangegetmomentorder_(PetscDualSpace sp,PetscInt *order, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeGetMomentOrder(
	(PetscDualSpace)PetscToPointer((sp) ),order);
}
PETSC_EXTERN void  petscdualspacelagrangesetmomentorder_(PetscDualSpace sp,PetscInt *order, int *__ierr)
{
*__ierr = PetscDualSpaceLagrangeSetMomentOrder(
	(PetscDualSpace)PetscToPointer((sp) ),*order);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* verboseinfo.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfoenabled_ PETSCINFOENABLED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfoenabled_ petscinfoenabled
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfoallow_ PETSCINFOALLOW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfoallow_ petscinfoallow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfogetinfo_ PETSCINFOGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfogetinfo_ petscinfogetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfosetfiltercommself_ PETSCINFOSETFILTERCOMMSELF
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfosetfiltercommself_ petscinfosetfiltercommself
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfosetfromoptions_ PETSCINFOSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfosetfromoptions_ petscinfosetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfodestroy_ PETSCINFODESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfodestroy_ petscinfodestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfodeactivateclass_ PETSCINFODEACTIVATECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfodeactivateclass_ petscinfodeactivateclass
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscinfoactivateclass_ PETSCINFOACTIVATECLASS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscinfoactivateclass_ petscinfoactivateclass
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscinfoenabled_(PetscClassId *classid,PetscBool *enabled, int *__ierr)
{
*__ierr = PetscInfoEnabled(*classid,enabled);
}
PETSC_EXTERN void  petscinfoallow_(PetscBool *flag, int *__ierr)
{
*__ierr = PetscInfoAllow(*flag);
}
PETSC_EXTERN void  petscinfogetinfo_(PetscBool *infoEnabled,PetscBool *classesSet,PetscBool *exclude,PetscBool *locked,PetscInfoCommFlag *commSelfFlag, int *__ierr)
{
*__ierr = PetscInfoGetInfo(infoEnabled,classesSet,exclude,locked,commSelfFlag);
}
PETSC_EXTERN void  petscinfosetfiltercommself_(PetscInfoCommFlag *commSelfFlag, int *__ierr)
{
*__ierr = PetscInfoSetFilterCommSelf(*commSelfFlag);
}
PETSC_EXTERN void  petscinfosetfromoptions_(PetscOptions options, int *__ierr)
{
*__ierr = PetscInfoSetFromOptions(
	(PetscOptions)PetscToPointer((options) ));
}
PETSC_EXTERN void  petscinfodestroy_(int *__ierr)
{
*__ierr = PetscInfoDestroy();
}
PETSC_EXTERN void  petscinfodeactivateclass_(PetscClassId *classid, int *__ierr)
{
*__ierr = PetscInfoDeactivateClass(*classid);
}
PETSC_EXTERN void  petscinfoactivateclass_(PetscClassId *classid, int *__ierr)
{
*__ierr = PetscInfoActivateClass(*classid);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* telescope.c */
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

#include "petscksp.h"
#include "petscdm.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetksp_ PCTELESCOPEGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetksp_ pctelescopegetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetreductionfactor_ PCTELESCOPEGETREDUCTIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetreductionfactor_ pctelescopegetreductionfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopesetreductionfactor_ PCTELESCOPESETREDUCTIONFACTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopesetreductionfactor_ pctelescopesetreductionfactor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetignoredm_ PCTELESCOPEGETIGNOREDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetignoredm_ pctelescopegetignoredm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopesetignoredm_ PCTELESCOPESETIGNOREDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopesetignoredm_ pctelescopesetignoredm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetusecoarsedm_ PCTELESCOPEGETUSECOARSEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetusecoarsedm_ pctelescopegetusecoarsedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopesetusecoarsedm_ PCTELESCOPESETUSECOARSEDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopesetusecoarsedm_ pctelescopesetusecoarsedm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetignorekspcomputeoperators_ PCTELESCOPEGETIGNOREKSPCOMPUTEOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetignorekspcomputeoperators_ pctelescopegetignorekspcomputeoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopesetignorekspcomputeoperators_ PCTELESCOPESETIGNOREKSPCOMPUTEOPERATORS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopesetignorekspcomputeoperators_ pctelescopesetignorekspcomputeoperators
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetdm_ PCTELESCOPEGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetdm_ pctelescopegetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopesetsubcommtype_ PCTELESCOPESETSUBCOMMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopesetsubcommtype_ pctelescopesetsubcommtype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pctelescopegetsubcommtype_ PCTELESCOPEGETSUBCOMMTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pctelescopegetsubcommtype_ pctelescopegetsubcommtype
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pctelescopegetksp_(PC pc,KSP *subksp, int *__ierr)
{
*__ierr = PCTelescopeGetKSP(
	(PC)PetscToPointer((pc) ),subksp);
}
PETSC_EXTERN void  pctelescopegetreductionfactor_(PC pc,PetscInt *fact, int *__ierr)
{
*__ierr = PCTelescopeGetReductionFactor(
	(PC)PetscToPointer((pc) ),fact);
}
PETSC_EXTERN void  pctelescopesetreductionfactor_(PC pc,PetscInt *fact, int *__ierr)
{
*__ierr = PCTelescopeSetReductionFactor(
	(PC)PetscToPointer((pc) ),*fact);
}
PETSC_EXTERN void  pctelescopegetignoredm_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeGetIgnoreDM(
	(PC)PetscToPointer((pc) ),v);
}
PETSC_EXTERN void  pctelescopesetignoredm_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeSetIgnoreDM(
	(PC)PetscToPointer((pc) ),*v);
}
PETSC_EXTERN void  pctelescopegetusecoarsedm_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeGetUseCoarseDM(
	(PC)PetscToPointer((pc) ),v);
}
PETSC_EXTERN void  pctelescopesetusecoarsedm_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeSetUseCoarseDM(
	(PC)PetscToPointer((pc) ),*v);
}
PETSC_EXTERN void  pctelescopegetignorekspcomputeoperators_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeGetIgnoreKSPComputeOperators(
	(PC)PetscToPointer((pc) ),v);
}
PETSC_EXTERN void  pctelescopesetignorekspcomputeoperators_(PC pc,PetscBool *v, int *__ierr)
{
*__ierr = PCTelescopeSetIgnoreKSPComputeOperators(
	(PC)PetscToPointer((pc) ),*v);
}
PETSC_EXTERN void  pctelescopegetdm_(PC pc,DM *subdm, int *__ierr)
{
*__ierr = PCTelescopeGetDM(
	(PC)PetscToPointer((pc) ),subdm);
}
PETSC_EXTERN void  pctelescopesetsubcommtype_(PC pc,PetscSubcommType *subcommtype, int *__ierr)
{
*__ierr = PCTelescopeSetSubcommType(
	(PC)PetscToPointer((pc) ),*subcommtype);
}
PETSC_EXTERN void  pctelescopegetsubcommtype_(PC pc,PetscSubcommType *subcommtype, int *__ierr)
{
*__ierr = PCTelescopeGetSubcommType(
	(PC)PetscToPointer((pc) ),subcommtype);
}
#if defined(__cplusplus)
}
#endif

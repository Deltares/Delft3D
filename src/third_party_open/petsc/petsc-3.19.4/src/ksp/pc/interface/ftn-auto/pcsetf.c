#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pcset.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetfromoptions_ PCSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetfromoptions_ pcsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetdm_ PCSETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetdm_ pcsetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetdm_ PCGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetdm_ pcgetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcsetapplicationcontext_ PCSETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcsetapplicationcontext_ pcsetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcgetapplicationcontext_ PCGETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcgetapplicationcontext_ pcgetapplicationcontext
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcsetfromoptions_(PC pc, int *__ierr)
{
*__ierr = PCSetFromOptions(
	(PC)PetscToPointer((pc) ));
}
PETSC_EXTERN void  pcsetdm_(PC pc,DM dm, int *__ierr)
{
*__ierr = PCSetDM(
	(PC)PetscToPointer((pc) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  pcgetdm_(PC pc,DM *dm, int *__ierr)
{
*__ierr = PCGetDM(
	(PC)PetscToPointer((pc) ),dm);
}
PETSC_EXTERN void  pcsetapplicationcontext_(PC pc,void*usrP, int *__ierr)
{
*__ierr = PCSetApplicationContext(
	(PC)PetscToPointer((pc) ),usrP);
}
PETSC_EXTERN void  pcgetapplicationcontext_(PC pc,void*usrP, int *__ierr)
{
*__ierr = PCGetApplicationContext(
	(PC)PetscToPointer((pc) ),usrP);
}
#if defined(__cplusplus)
}
#endif

#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* snes.c */
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

#include "petscsnes.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesseterrorifnotconverged_ SNESSETERRORIFNOTCONVERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesseterrorifnotconverged_ snesseterrorifnotconverged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgeterrorifnotconverged_ SNESGETERRORIFNOTCONVERGED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgeterrorifnotconverged_ snesgeterrorifnotconverged
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetalwayscomputesfinalresidual_ SNESSETALWAYSCOMPUTESFINALRESIDUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetalwayscomputesfinalresidual_ snessetalwayscomputesfinalresidual
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetalwayscomputesfinalresidual_ SNESGETALWAYSCOMPUTESFINALRESIDUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetalwayscomputesfinalresidual_ snesgetalwayscomputesfinalresidual
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetfunctiondomainerror_ SNESSETFUNCTIONDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetfunctiondomainerror_ snessetfunctiondomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetjacobiandomainerror_ SNESSETJACOBIANDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetjacobiandomainerror_ snessetjacobiandomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetcheckjacobiandomainerror_ SNESSETCHECKJACOBIANDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetcheckjacobiandomainerror_ snessetcheckjacobiandomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetcheckjacobiandomainerror_ SNESGETCHECKJACOBIANDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetcheckjacobiandomainerror_ snesgetcheckjacobiandomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetfunctiondomainerror_ SNESGETFUNCTIONDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetfunctiondomainerror_ snesgetfunctiondomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetjacobiandomainerror_ SNESGETJACOBIANDOMAINERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetjacobiandomainerror_ snesgetjacobiandomainerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetupmatrices_ SNESSETUPMATRICES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetupmatrices_ snessetupmatrices
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetfromoptions_ SNESSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetfromoptions_ snessetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesresetfromoptions_ SNESRESETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesresetfromoptions_ snesresetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetapplicationcontext_ SNESSETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetapplicationcontext_ snessetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetapplicationcontext_ SNESGETAPPLICATIONCONTEXT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetapplicationcontext_ snesgetapplicationcontext
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetusematrixfree_ SNESSETUSEMATRIXFREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetusematrixfree_ snessetusematrixfree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetusematrixfree_ SNESGETUSEMATRIXFREE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetusematrixfree_ snesgetusematrixfree
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetiterationnumber_ SNESGETITERATIONNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetiterationnumber_ snesgetiterationnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetiterationnumber_ SNESSETITERATIONNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetiterationnumber_ snessetiterationnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnonlinearstepfailures_ SNESGETNONLINEARSTEPFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnonlinearstepfailures_ snesgetnonlinearstepfailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetmaxnonlinearstepfailures_ SNESSETMAXNONLINEARSTEPFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetmaxnonlinearstepfailures_ snessetmaxnonlinearstepfailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetmaxnonlinearstepfailures_ SNESGETMAXNONLINEARSTEPFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetmaxnonlinearstepfailures_ snesgetmaxnonlinearstepfailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnumberfunctionevals_ SNESGETNUMBERFUNCTIONEVALS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnumberfunctionevals_ snesgetnumberfunctionevals
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetlinearsolvefailures_ SNESGETLINEARSOLVEFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetlinearsolvefailures_ snesgetlinearsolvefailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetmaxlinearsolvefailures_ SNESSETMAXLINEARSOLVEFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetmaxlinearsolvefailures_ snessetmaxlinearsolvefailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetmaxlinearsolvefailures_ SNESGETMAXLINEARSOLVEFAILURES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetmaxlinearsolvefailures_ snesgetmaxlinearsolvefailures
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetlinearsolveiterations_ SNESGETLINEARSOLVEITERATIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetlinearsolveiterations_ snesgetlinearsolveiterations
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetcountersreset_ SNESSETCOUNTERSRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetcountersreset_ snessetcountersreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetksp_ SNESSETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetksp_ snessetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescreate_ SNESCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescreate_ snescreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetnormschedule_ SNESSETNORMSCHEDULE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetnormschedule_ snessetnormschedule
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnormschedule_ SNESGETNORMSCHEDULE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnormschedule_ snesgetnormschedule
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetfunctionnorm_ SNESSETFUNCTIONNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetfunctionnorm_ snessetfunctionnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetfunctionnorm_ SNESGETFUNCTIONNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetfunctionnorm_ snesgetfunctionnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetupdatenorm_ SNESGETUPDATENORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetupdatenorm_ snesgetupdatenorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetsolutionnorm_ SNESGETSOLUTIONNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetsolutionnorm_ snesgetsolutionnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescomputefunction_ SNESCOMPUTEFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescomputefunction_ snescomputefunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescomputemffunction_ SNESCOMPUTEMFFUNCTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescomputemffunction_ snescomputemffunction
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescomputengs_ SNESCOMPUTENGS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescomputengs_ snescomputengs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snescomputejacobian_ SNESCOMPUTEJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snescomputejacobian_ snescomputejacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetup_ SNESSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetup_ snessetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesreset_ SNESRESET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesreset_ snesreset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesconvergedreasonviewcancel_ SNESCONVERGEDREASONVIEWCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesconvergedreasonviewcancel_ snesconvergedreasonviewcancel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetlagpreconditioner_ SNESSETLAGPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetlagpreconditioner_ snessetlagpreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetgridsequence_ SNESSETGRIDSEQUENCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetgridsequence_ snessetgridsequence
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetgridsequence_ SNESGETGRIDSEQUENCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetgridsequence_ snesgetgridsequence
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetlagpreconditioner_ SNESGETLAGPRECONDITIONER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetlagpreconditioner_ snesgetlagpreconditioner
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetlagjacobian_ SNESSETLAGJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetlagjacobian_ snessetlagjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetlagjacobian_ SNESGETLAGJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetlagjacobian_ snesgetlagjacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetlagjacobianpersists_ SNESSETLAGJACOBIANPERSISTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetlagjacobianpersists_ snessetlagjacobianpersists
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetlagpreconditionerpersists_ SNESSETLAGPRECONDITIONERPERSISTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetlagpreconditionerpersists_ snessetlagpreconditionerpersists
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetforceiteration_ SNESSETFORCEITERATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetforceiteration_ snessetforceiteration
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetforceiteration_ SNESGETFORCEITERATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetforceiteration_ snesgetforceiteration
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessettolerances_ SNESSETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessettolerances_ snessettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetdivergencetolerance_ SNESSETDIVERGENCETOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetdivergencetolerance_ snessetdivergencetolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgettolerances_ SNESGETTOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgettolerances_ snesgettolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetdivergencetolerance_ SNESGETDIVERGENCETOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetdivergencetolerance_ snesgetdivergencetolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessettrustregiontolerance_ SNESSETTRUSTREGIONTOLERANCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessettrustregiontolerance_ snessettrustregiontolerance
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmonitor_ SNESMONITOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmonitor_ snesmonitor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesmonitorcancel_ SNESMONITORCANCEL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesmonitorcancel_ snesmonitorcancel
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetconvergedreason_ SNESGETCONVERGEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetconvergedreason_ snesgetconvergedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetconvergedreason_ SNESSETCONVERGEDREASON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetconvergedreason_ snessetconvergedreason
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetconvergencehistory_ SNESSETCONVERGENCEHISTORY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetconvergencehistory_ snessetconvergencehistory
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesconvergedreasonviewfromoptions_ SNESCONVERGEDREASONVIEWFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesconvergedreasonviewfromoptions_ snesconvergedreasonviewfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessolve_ SNESSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessolve_ snessolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetsolution_ SNESSETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetsolution_ snessetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetsolution_ SNESGETSOLUTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetsolution_ snesgetsolution
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetsolutionupdate_ SNESGETSOLUTIONUPDATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetsolutionupdate_ snesgetsolutionupdate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneskspsetuseew_ SNESKSPSETUSEEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneskspsetuseew_ sneskspsetuseew
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneskspgetuseew_ SNESKSPGETUSEEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneskspgetuseew_ sneskspgetuseew
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneskspsetparametersew_ SNESKSPSETPARAMETERSEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneskspsetparametersew_ sneskspsetparametersew
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneskspgetparametersew_ SNESKSPGETPARAMETERSEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneskspgetparametersew_ sneskspgetparametersew
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetksp_ SNESGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetksp_ snesgetksp
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetdm_ SNESSETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetdm_ snessetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetdm_ SNESGETDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetdm_ snesgetdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetnpc_ SNESSETNPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetnpc_ snessetnpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnpc_ SNESGETNPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnpc_ snesgetnpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define sneshasnpc_ SNESHASNPC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define sneshasnpc_ sneshasnpc
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetnpcside_ SNESSETNPCSIDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetnpcside_ snessetnpcside
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetnpcside_ SNESGETNPCSIDE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetnpcside_ snesgetnpcside
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snessetlinesearch_ SNESSETLINESEARCH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snessetlinesearch_ snessetlinesearch
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesgetlinesearch_ SNESGETLINESEARCH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesgetlinesearch_ snesgetlinesearch
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesseterrorifnotconverged_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESSetErrorIfNotConverged(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snesgeterrorifnotconverged_(SNES snes,PetscBool *flag, int *__ierr)
{
*__ierr = SNESGetErrorIfNotConverged(
	(SNES)PetscToPointer((snes) ),flag);
}
PETSC_EXTERN void  snessetalwayscomputesfinalresidual_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESSetAlwaysComputesFinalResidual(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snesgetalwayscomputesfinalresidual_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESGetAlwaysComputesFinalResidual(
	(SNES)PetscToPointer((snes) ),flg);
}
PETSC_EXTERN void  snessetfunctiondomainerror_(SNES snes, int *__ierr)
{
*__ierr = SNESSetFunctionDomainError(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessetjacobiandomainerror_(SNES snes, int *__ierr)
{
*__ierr = SNESSetJacobianDomainError(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessetcheckjacobiandomainerror_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESSetCheckJacobianDomainError(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snesgetcheckjacobiandomainerror_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESGetCheckJacobianDomainError(
	(SNES)PetscToPointer((snes) ),flg);
}
PETSC_EXTERN void  snesgetfunctiondomainerror_(SNES snes,PetscBool *domainerror, int *__ierr)
{
*__ierr = SNESGetFunctionDomainError(
	(SNES)PetscToPointer((snes) ),domainerror);
}
PETSC_EXTERN void  snesgetjacobiandomainerror_(SNES snes,PetscBool *domainerror, int *__ierr)
{
*__ierr = SNESGetJacobianDomainError(
	(SNES)PetscToPointer((snes) ),domainerror);
}
PETSC_EXTERN void  snessetupmatrices_(SNES snes, int *__ierr)
{
*__ierr = SNESSetUpMatrices(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessetfromoptions_(SNES snes, int *__ierr)
{
*__ierr = SNESSetFromOptions(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snesresetfromoptions_(SNES snes, int *__ierr)
{
*__ierr = SNESResetFromOptions(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessetapplicationcontext_(SNES snes,void*usrP, int *__ierr)
{
*__ierr = SNESSetApplicationContext(
	(SNES)PetscToPointer((snes) ),usrP);
}
PETSC_EXTERN void  snesgetapplicationcontext_(SNES snes,void*usrP, int *__ierr)
{
*__ierr = SNESGetApplicationContext(
	(SNES)PetscToPointer((snes) ),usrP);
}
PETSC_EXTERN void  snessetusematrixfree_(SNES snes,PetscBool *mf_operator,PetscBool *mf, int *__ierr)
{
*__ierr = SNESSetUseMatrixFree(
	(SNES)PetscToPointer((snes) ),*mf_operator,*mf);
}
PETSC_EXTERN void  snesgetusematrixfree_(SNES snes,PetscBool *mf_operator,PetscBool *mf, int *__ierr)
{
*__ierr = SNESGetUseMatrixFree(
	(SNES)PetscToPointer((snes) ),mf_operator,mf);
}
PETSC_EXTERN void  snesgetiterationnumber_(SNES snes,PetscInt *iter, int *__ierr)
{
*__ierr = SNESGetIterationNumber(
	(SNES)PetscToPointer((snes) ),iter);
}
PETSC_EXTERN void  snessetiterationnumber_(SNES snes,PetscInt *iter, int *__ierr)
{
*__ierr = SNESSetIterationNumber(
	(SNES)PetscToPointer((snes) ),*iter);
}
PETSC_EXTERN void  snesgetnonlinearstepfailures_(SNES snes,PetscInt *nfails, int *__ierr)
{
*__ierr = SNESGetNonlinearStepFailures(
	(SNES)PetscToPointer((snes) ),nfails);
}
PETSC_EXTERN void  snessetmaxnonlinearstepfailures_(SNES snes,PetscInt *maxFails, int *__ierr)
{
*__ierr = SNESSetMaxNonlinearStepFailures(
	(SNES)PetscToPointer((snes) ),*maxFails);
}
PETSC_EXTERN void  snesgetmaxnonlinearstepfailures_(SNES snes,PetscInt *maxFails, int *__ierr)
{
*__ierr = SNESGetMaxNonlinearStepFailures(
	(SNES)PetscToPointer((snes) ),maxFails);
}
PETSC_EXTERN void  snesgetnumberfunctionevals_(SNES snes,PetscInt *nfuncs, int *__ierr)
{
*__ierr = SNESGetNumberFunctionEvals(
	(SNES)PetscToPointer((snes) ),nfuncs);
}
PETSC_EXTERN void  snesgetlinearsolvefailures_(SNES snes,PetscInt *nfails, int *__ierr)
{
*__ierr = SNESGetLinearSolveFailures(
	(SNES)PetscToPointer((snes) ),nfails);
}
PETSC_EXTERN void  snessetmaxlinearsolvefailures_(SNES snes,PetscInt *maxFails, int *__ierr)
{
*__ierr = SNESSetMaxLinearSolveFailures(
	(SNES)PetscToPointer((snes) ),*maxFails);
}
PETSC_EXTERN void  snesgetmaxlinearsolvefailures_(SNES snes,PetscInt *maxFails, int *__ierr)
{
*__ierr = SNESGetMaxLinearSolveFailures(
	(SNES)PetscToPointer((snes) ),maxFails);
}
PETSC_EXTERN void  snesgetlinearsolveiterations_(SNES snes,PetscInt *lits, int *__ierr)
{
*__ierr = SNESGetLinearSolveIterations(
	(SNES)PetscToPointer((snes) ),lits);
}
PETSC_EXTERN void  snessetcountersreset_(SNES snes,PetscBool *reset, int *__ierr)
{
*__ierr = SNESSetCountersReset(
	(SNES)PetscToPointer((snes) ),*reset);
}
PETSC_EXTERN void  snessetksp_(SNES snes,KSP ksp, int *__ierr)
{
*__ierr = SNESSetKSP(
	(SNES)PetscToPointer((snes) ),
	(KSP)PetscToPointer((ksp) ));
}
PETSC_EXTERN void  snescreate_(MPI_Fint * comm,SNES *outsnes, int *__ierr)
{
*__ierr = SNESCreate(
	MPI_Comm_f2c(*(comm)),outsnes);
}
PETSC_EXTERN void  snessetnormschedule_(SNES snes,SNESNormSchedule *normschedule, int *__ierr)
{
*__ierr = SNESSetNormSchedule(
	(SNES)PetscToPointer((snes) ),*normschedule);
}
PETSC_EXTERN void  snesgetnormschedule_(SNES snes,SNESNormSchedule *normschedule, int *__ierr)
{
*__ierr = SNESGetNormSchedule(
	(SNES)PetscToPointer((snes) ),normschedule);
}
PETSC_EXTERN void  snessetfunctionnorm_(SNES snes,PetscReal *norm, int *__ierr)
{
*__ierr = SNESSetFunctionNorm(
	(SNES)PetscToPointer((snes) ),*norm);
}
PETSC_EXTERN void  snesgetfunctionnorm_(SNES snes,PetscReal *norm, int *__ierr)
{
*__ierr = SNESGetFunctionNorm(
	(SNES)PetscToPointer((snes) ),norm);
}
PETSC_EXTERN void  snesgetupdatenorm_(SNES snes,PetscReal *ynorm, int *__ierr)
{
*__ierr = SNESGetUpdateNorm(
	(SNES)PetscToPointer((snes) ),ynorm);
}
PETSC_EXTERN void  snesgetsolutionnorm_(SNES snes,PetscReal *xnorm, int *__ierr)
{
*__ierr = SNESGetSolutionNorm(
	(SNES)PetscToPointer((snes) ),xnorm);
}
PETSC_EXTERN void  snescomputefunction_(SNES snes,Vec x,Vec y, int *__ierr)
{
*__ierr = SNESComputeFunction(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  snescomputemffunction_(SNES snes,Vec x,Vec y, int *__ierr)
{
*__ierr = SNESComputeMFFunction(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void  snescomputengs_(SNES snes,Vec b,Vec x, int *__ierr)
{
*__ierr = SNESComputeNGS(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void  snescomputejacobian_(SNES snes,Vec X,Mat A,Mat B, int *__ierr)
{
*__ierr = SNESComputeJacobian(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((X) ),
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ));
}
PETSC_EXTERN void  snessetup_(SNES snes, int *__ierr)
{
*__ierr = SNESSetUp(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snesreset_(SNES snes, int *__ierr)
{
*__ierr = SNESReset(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snesconvergedreasonviewcancel_(SNES snes, int *__ierr)
{
*__ierr = SNESConvergedReasonViewCancel(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessetlagpreconditioner_(SNES snes,PetscInt *lag, int *__ierr)
{
*__ierr = SNESSetLagPreconditioner(
	(SNES)PetscToPointer((snes) ),*lag);
}
PETSC_EXTERN void  snessetgridsequence_(SNES snes,PetscInt *steps, int *__ierr)
{
*__ierr = SNESSetGridSequence(
	(SNES)PetscToPointer((snes) ),*steps);
}
PETSC_EXTERN void  snesgetgridsequence_(SNES snes,PetscInt *steps, int *__ierr)
{
*__ierr = SNESGetGridSequence(
	(SNES)PetscToPointer((snes) ),steps);
}
PETSC_EXTERN void  snesgetlagpreconditioner_(SNES snes,PetscInt *lag, int *__ierr)
{
*__ierr = SNESGetLagPreconditioner(
	(SNES)PetscToPointer((snes) ),lag);
}
PETSC_EXTERN void  snessetlagjacobian_(SNES snes,PetscInt *lag, int *__ierr)
{
*__ierr = SNESSetLagJacobian(
	(SNES)PetscToPointer((snes) ),*lag);
}
PETSC_EXTERN void  snesgetlagjacobian_(SNES snes,PetscInt *lag, int *__ierr)
{
*__ierr = SNESGetLagJacobian(
	(SNES)PetscToPointer((snes) ),lag);
}
PETSC_EXTERN void  snessetlagjacobianpersists_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESSetLagJacobianPersists(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snessetlagpreconditionerpersists_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESSetLagPreconditionerPersists(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snessetforceiteration_(SNES snes,PetscBool *force, int *__ierr)
{
*__ierr = SNESSetForceIteration(
	(SNES)PetscToPointer((snes) ),*force);
}
PETSC_EXTERN void  snesgetforceiteration_(SNES snes,PetscBool *force, int *__ierr)
{
*__ierr = SNESGetForceIteration(
	(SNES)PetscToPointer((snes) ),force);
}
PETSC_EXTERN void  snessettolerances_(SNES snes,PetscReal *abstol,PetscReal *rtol,PetscReal *stol,PetscInt *maxit,PetscInt *maxf, int *__ierr)
{
*__ierr = SNESSetTolerances(
	(SNES)PetscToPointer((snes) ),*abstol,*rtol,*stol,*maxit,*maxf);
}
PETSC_EXTERN void  snessetdivergencetolerance_(SNES snes,PetscReal *divtol, int *__ierr)
{
*__ierr = SNESSetDivergenceTolerance(
	(SNES)PetscToPointer((snes) ),*divtol);
}
PETSC_EXTERN void  snesgettolerances_(SNES snes,PetscReal *atol,PetscReal *rtol,PetscReal *stol,PetscInt *maxit,PetscInt *maxf, int *__ierr)
{
*__ierr = SNESGetTolerances(
	(SNES)PetscToPointer((snes) ),atol,rtol,stol,maxit,maxf);
}
PETSC_EXTERN void  snesgetdivergencetolerance_(SNES snes,PetscReal *divtol, int *__ierr)
{
*__ierr = SNESGetDivergenceTolerance(
	(SNES)PetscToPointer((snes) ),divtol);
}
PETSC_EXTERN void  snessettrustregiontolerance_(SNES snes,PetscReal *tol, int *__ierr)
{
*__ierr = SNESSetTrustRegionTolerance(
	(SNES)PetscToPointer((snes) ),*tol);
}
PETSC_EXTERN void  snesmonitor_(SNES snes,PetscInt *iter,PetscReal *rnorm, int *__ierr)
{
*__ierr = SNESMonitor(
	(SNES)PetscToPointer((snes) ),*iter,*rnorm);
}
PETSC_EXTERN void  snesmonitorcancel_(SNES snes, int *__ierr)
{
*__ierr = SNESMonitorCancel(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snesgetconvergedreason_(SNES snes,SNESConvergedReason *reason, int *__ierr)
{
*__ierr = SNESGetConvergedReason(
	(SNES)PetscToPointer((snes) ),reason);
}
PETSC_EXTERN void  snessetconvergedreason_(SNES snes,SNESConvergedReason *reason, int *__ierr)
{
*__ierr = SNESSetConvergedReason(
	(SNES)PetscToPointer((snes) ),*reason);
}
PETSC_EXTERN void  snessetconvergencehistory_(SNES snes,PetscReal a[],PetscInt its[],PetscInt *na,PetscBool *reset, int *__ierr)
{
*__ierr = SNESSetConvergenceHistory(
	(SNES)PetscToPointer((snes) ),a,its,*na,*reset);
}
PETSC_EXTERN void  snesconvergedreasonviewfromoptions_(SNES snes, int *__ierr)
{
*__ierr = SNESConvergedReasonViewFromOptions(
	(SNES)PetscToPointer((snes) ));
}
PETSC_EXTERN void  snessolve_(SNES snes,Vec b,Vec x, int *__ierr)
{
*__ierr = SNESSolve(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void  snessetsolution_(SNES snes,Vec u, int *__ierr)
{
*__ierr = SNESSetSolution(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((u) ));
}
PETSC_EXTERN void  snesgetsolution_(SNES snes,Vec *x, int *__ierr)
{
*__ierr = SNESGetSolution(
	(SNES)PetscToPointer((snes) ),x);
}
PETSC_EXTERN void  snesgetsolutionupdate_(SNES snes,Vec *x, int *__ierr)
{
*__ierr = SNESGetSolutionUpdate(
	(SNES)PetscToPointer((snes) ),x);
}
PETSC_EXTERN void  sneskspsetuseew_(SNES snes,PetscBool *flag, int *__ierr)
{
*__ierr = SNESKSPSetUseEW(
	(SNES)PetscToPointer((snes) ),*flag);
}
PETSC_EXTERN void  sneskspgetuseew_(SNES snes,PetscBool *flag, int *__ierr)
{
*__ierr = SNESKSPGetUseEW(
	(SNES)PetscToPointer((snes) ),flag);
}
PETSC_EXTERN void  sneskspsetparametersew_(SNES snes,PetscInt *version,PetscReal *rtol_0,PetscReal *rtol_max,PetscReal *gamma,PetscReal *alpha,PetscReal *alpha2,PetscReal *threshold, int *__ierr)
{
*__ierr = SNESKSPSetParametersEW(
	(SNES)PetscToPointer((snes) ),*version,*rtol_0,*rtol_max,*gamma,*alpha,*alpha2,*threshold);
}
PETSC_EXTERN void  sneskspgetparametersew_(SNES snes,PetscInt *version,PetscReal *rtol_0,PetscReal *rtol_max,PetscReal *gamma,PetscReal *alpha,PetscReal *alpha2,PetscReal *threshold, int *__ierr)
{
*__ierr = SNESKSPGetParametersEW(
	(SNES)PetscToPointer((snes) ),version,rtol_0,rtol_max,gamma,alpha,alpha2,threshold);
}
PETSC_EXTERN void  snesgetksp_(SNES snes,KSP *ksp, int *__ierr)
{
*__ierr = SNESGetKSP(
	(SNES)PetscToPointer((snes) ),ksp);
}
PETSC_EXTERN void  snessetdm_(SNES snes,DM dm, int *__ierr)
{
*__ierr = SNESSetDM(
	(SNES)PetscToPointer((snes) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  snesgetdm_(SNES snes,DM *dm, int *__ierr)
{
*__ierr = SNESGetDM(
	(SNES)PetscToPointer((snes) ),dm);
}
PETSC_EXTERN void  snessetnpc_(SNES snes,SNES npc, int *__ierr)
{
*__ierr = SNESSetNPC(
	(SNES)PetscToPointer((snes) ),
	(SNES)PetscToPointer((npc) ));
}
PETSC_EXTERN void  snesgetnpc_(SNES snes,SNES *pc, int *__ierr)
{
*__ierr = SNESGetNPC(
	(SNES)PetscToPointer((snes) ),pc);
}
PETSC_EXTERN void  sneshasnpc_(SNES snes,PetscBool *has_npc, int *__ierr)
{
*__ierr = SNESHasNPC(
	(SNES)PetscToPointer((snes) ),has_npc);
}
PETSC_EXTERN void  snessetnpcside_(SNES snes,PCSide *side, int *__ierr)
{
*__ierr = SNESSetNPCSide(
	(SNES)PetscToPointer((snes) ),*side);
}
PETSC_EXTERN void  snesgetnpcside_(SNES snes,PCSide *side, int *__ierr)
{
*__ierr = SNESGetNPCSide(
	(SNES)PetscToPointer((snes) ),side);
}
PETSC_EXTERN void  snessetlinesearch_(SNES snes,SNESLineSearch linesearch, int *__ierr)
{
*__ierr = SNESSetLineSearch(
	(SNES)PetscToPointer((snes) ),
	(SNESLineSearch)PetscToPointer((linesearch) ));
}
PETSC_EXTERN void  snesgetlinesearch_(SNES snes,SNESLineSearch *linesearch, int *__ierr)
{
*__ierr = SNESGetLineSearch(
	(SNES)PetscToPointer((snes) ),linesearch);
}
#if defined(__cplusplus)
}
#endif

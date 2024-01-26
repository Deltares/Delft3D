#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* subcomm.c */
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
#define petscsubcommsetfromoptions_ PETSCSUBCOMMSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommsetfromoptions_ petscsubcommsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsubcommsetnumber_ PETSCSUBCOMMSETNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommsetnumber_ petscsubcommsetnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsubcommsettype_ PETSCSUBCOMMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommsettype_ petscsubcommsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsubcommsettypegeneral_ PETSCSUBCOMMSETTYPEGENERAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommsettypegeneral_ petscsubcommsettypegeneral
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsubcommdestroy_ PETSCSUBCOMMDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommdestroy_ petscsubcommdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscsubcommcreate_ PETSCSUBCOMMCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscsubcommcreate_ petscsubcommcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscsubcommsetfromoptions_(PetscSubcomm psubcomm, int *__ierr)
{
*__ierr = PetscSubcommSetFromOptions(
	(PetscSubcomm)PetscToPointer((psubcomm) ));
}
PETSC_EXTERN void  petscsubcommsetnumber_(PetscSubcomm psubcomm,PetscInt *nsubcomm, int *__ierr)
{
*__ierr = PetscSubcommSetNumber(
	(PetscSubcomm)PetscToPointer((psubcomm) ),*nsubcomm);
}
PETSC_EXTERN void  petscsubcommsettype_(PetscSubcomm psubcomm,PetscSubcommType *subcommtype, int *__ierr)
{
*__ierr = PetscSubcommSetType(
	(PetscSubcomm)PetscToPointer((psubcomm) ),*subcommtype);
}
PETSC_EXTERN void  petscsubcommsettypegeneral_(PetscSubcomm psubcomm,PetscMPIInt *color,PetscMPIInt *subrank, int *__ierr)
{
*__ierr = PetscSubcommSetTypeGeneral(
	(PetscSubcomm)PetscToPointer((psubcomm) ),*color,*subrank);
}
PETSC_EXTERN void  petscsubcommdestroy_(PetscSubcomm *psubcomm, int *__ierr)
{
*__ierr = PetscSubcommDestroy(psubcomm);
}
PETSC_EXTERN void  petscsubcommcreate_(MPI_Fint * comm,PetscSubcomm *psubcomm, int *__ierr)
{
*__ierr = PetscSubcommCreate(
	MPI_Comm_f2c(*(comm)),psubcomm);
}
#if defined(__cplusplus)
}
#endif

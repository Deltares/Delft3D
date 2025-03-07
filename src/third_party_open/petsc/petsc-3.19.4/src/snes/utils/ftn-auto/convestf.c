#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* convest.c */
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

#include "petscconvest.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestdestroy_ PETSCCONVESTDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestdestroy_ petscconvestdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetfromoptions_ PETSCCONVESTSETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetfromoptions_ petscconvestsetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestview_ PETSCCONVESTVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestview_ petscconvestview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestgetsolver_ PETSCCONVESTGETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestgetsolver_ petscconvestgetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetsolver_ PETSCCONVESTSETSOLVER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetsolver_ petscconvestsetsolver
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestsetup_ PETSCCONVESTSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestsetup_ petscconvestsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestmonitordefault_ PETSCCONVESTMONITORDEFAULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestmonitordefault_ petscconvestmonitordefault
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestgetconvrate_ PETSCCONVESTGETCONVRATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestgetconvrate_ petscconvestgetconvrate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestrateview_ PETSCCONVESTRATEVIEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestrateview_ petscconvestrateview
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscconvestcreate_ PETSCCONVESTCREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscconvestcreate_ petscconvestcreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscconvestdestroy_(PetscConvEst *ce, int *__ierr)
{
*__ierr = PetscConvEstDestroy(ce);
}
PETSC_EXTERN void  petscconvestsetfromoptions_(PetscConvEst ce, int *__ierr)
{
*__ierr = PetscConvEstSetFromOptions(
	(PetscConvEst)PetscToPointer((ce) ));
}
PETSC_EXTERN void  petscconvestview_(PetscConvEst ce,PetscViewer viewer, int *__ierr)
{
*__ierr = PetscConvEstView(
	(PetscConvEst)PetscToPointer((ce) ),
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscconvestgetsolver_(PetscConvEst ce,PetscObject *solver, int *__ierr)
{
*__ierr = PetscConvEstGetSolver(
	(PetscConvEst)PetscToPointer((ce) ),solver);
}
PETSC_EXTERN void  petscconvestsetsolver_(PetscConvEst ce,PetscObject *solver, int *__ierr)
{
*__ierr = PetscConvEstSetSolver(
	(PetscConvEst)PetscToPointer((ce) ),*solver);
}
PETSC_EXTERN void  petscconvestsetup_(PetscConvEst ce, int *__ierr)
{
*__ierr = PetscConvEstSetUp(
	(PetscConvEst)PetscToPointer((ce) ));
}
PETSC_EXTERN void  petscconvestmonitordefault_(PetscConvEst ce,PetscInt *r, int *__ierr)
{
*__ierr = PetscConvEstMonitorDefault(
	(PetscConvEst)PetscToPointer((ce) ),*r);
}
PETSC_EXTERN void  petscconvestgetconvrate_(PetscConvEst ce,PetscReal alpha[], int *__ierr)
{
*__ierr = PetscConvEstGetConvRate(
	(PetscConvEst)PetscToPointer((ce) ),alpha);
}
PETSC_EXTERN void  petscconvestrateview_(PetscConvEst ce, PetscReal alpha[],PetscViewer viewer, int *__ierr)
{
*__ierr = PetscConvEstRateView(
	(PetscConvEst)PetscToPointer((ce) ),alpha,
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscconvestcreate_(MPI_Fint * comm,PetscConvEst *ce, int *__ierr)
{
*__ierr = PetscConvEstCreate(
	MPI_Comm_f2c(*(comm)),ce);
}
#if defined(__cplusplus)
}
#endif

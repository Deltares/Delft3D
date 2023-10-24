#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* parms.c */
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
#define pcparmssetglobal_ PCPARMSSETGLOBAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetglobal_ pcparmssetglobal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcparmssetlocal_ PCPARMSSETLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetlocal_ pcparmssetlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcparmssetsolvetolerances_ PCPARMSSETSOLVETOLERANCES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetsolvetolerances_ pcparmssetsolvetolerances
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcparmssetsolverestart_ PCPARMSSETSOLVERESTART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetsolverestart_ pcparmssetsolverestart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcparmssetnonsymperm_ PCPARMSSETNONSYMPERM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetnonsymperm_ pcparmssetnonsymperm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcparmssetfill_ PCPARMSSETFILL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcparmssetfill_ pcparmssetfill
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  pcparmssetglobal_(PC pc,PCPARMSGlobalType *type, int *__ierr)
{
*__ierr = PCPARMSSetGlobal(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pcparmssetlocal_(PC pc,PCPARMSLocalType *type, int *__ierr)
{
*__ierr = PCPARMSSetLocal(
	(PC)PetscToPointer((pc) ),*type);
}
PETSC_EXTERN void  pcparmssetsolvetolerances_(PC pc,PetscReal *tol,PetscInt *maxits, int *__ierr)
{
*__ierr = PCPARMSSetSolveTolerances(
	(PC)PetscToPointer((pc) ),*tol,*maxits);
}
PETSC_EXTERN void  pcparmssetsolverestart_(PC pc,PetscInt *restart, int *__ierr)
{
*__ierr = PCPARMSSetSolveRestart(
	(PC)PetscToPointer((pc) ),*restart);
}
PETSC_EXTERN void  pcparmssetnonsymperm_(PC pc,PetscBool *nonsym, int *__ierr)
{
*__ierr = PCPARMSSetNonsymPerm(
	(PC)PetscToPointer((pc) ),*nonsym);
}
PETSC_EXTERN void  pcparmssetfill_(PC pc,PetscInt *lfil0,PetscInt *lfil1,PetscInt *lfil2, int *__ierr)
{
*__ierr = PCPARMSSetFill(
	(PC)PetscToPointer((pc) ),*lfil0,*lfil1,*lfil2);
}
#if defined(__cplusplus)
}
#endif

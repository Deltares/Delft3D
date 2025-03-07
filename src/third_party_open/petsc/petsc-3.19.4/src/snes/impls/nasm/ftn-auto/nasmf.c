#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* nasm.c */
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
#define snesnasmsettype_ SNESNASMSETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmsettype_ snesnasmsettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgettype_ SNESNASMGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgettype_ snesnasmgettype
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmsetsubdomains_ SNESNASMSETSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmsetsubdomains_ snesnasmsetsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgetsubdomains_ SNESNASMGETSUBDOMAINS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgetsubdomains_ snesnasmgetsubdomains
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgetsubdomainvecs_ SNESNASMGETSUBDOMAINVECS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgetsubdomainvecs_ snesnasmgetsubdomainvecs
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmsetcomputefinaljacobian_ SNESNASMSETCOMPUTEFINALJACOBIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmsetcomputefinaljacobian_ snesnasmsetcomputefinaljacobian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmsetdamping_ SNESNASMSETDAMPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmsetdamping_ snesnasmsetdamping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgetdamping_ SNESNASMGETDAMPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgetdamping_ snesnasmgetdamping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgetsnes_ SNESNASMGETSNES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgetsnes_ snesnasmgetsnes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmgetnumber_ SNESNASMGETNUMBER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmgetnumber_ snesnasmgetnumber
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define snesnasmsetweight_ SNESNASMSETWEIGHT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define snesnasmsetweight_ snesnasmsetweight
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  snesnasmsettype_(SNES snes,PCASMType *type, int *__ierr)
{
*__ierr = SNESNASMSetType(
	(SNES)PetscToPointer((snes) ),*type);
}
PETSC_EXTERN void  snesnasmgettype_(SNES snes,PCASMType *type, int *__ierr)
{
*__ierr = SNESNASMGetType(
	(SNES)PetscToPointer((snes) ),type);
}
PETSC_EXTERN void  snesnasmsetsubdomains_(SNES snes,PetscInt *n,SNES subsnes[],VecScatter iscatter[],VecScatter oscatter[],VecScatter gscatter[], int *__ierr)
{
*__ierr = SNESNASMSetSubdomains(
	(SNES)PetscToPointer((snes) ),*n,subsnes,iscatter,oscatter,gscatter);
}
PETSC_EXTERN void  snesnasmgetsubdomains_(SNES snes,PetscInt *n,SNES *subsnes[],VecScatter *iscatter[],VecScatter *oscatter[],VecScatter *gscatter[], int *__ierr)
{
*__ierr = SNESNASMGetSubdomains(
	(SNES)PetscToPointer((snes) ),n,subsnes,iscatter,oscatter,gscatter);
}
PETSC_EXTERN void  snesnasmgetsubdomainvecs_(SNES snes,PetscInt *n,Vec **x,Vec **y,Vec **b,Vec **xl, int *__ierr)
{
*__ierr = SNESNASMGetSubdomainVecs(
	(SNES)PetscToPointer((snes) ),n,x,y,b,xl);
}
PETSC_EXTERN void  snesnasmsetcomputefinaljacobian_(SNES snes,PetscBool *flg, int *__ierr)
{
*__ierr = SNESNASMSetComputeFinalJacobian(
	(SNES)PetscToPointer((snes) ),*flg);
}
PETSC_EXTERN void  snesnasmsetdamping_(SNES snes,PetscReal *dmp, int *__ierr)
{
*__ierr = SNESNASMSetDamping(
	(SNES)PetscToPointer((snes) ),*dmp);
}
PETSC_EXTERN void  snesnasmgetdamping_(SNES snes,PetscReal *dmp, int *__ierr)
{
*__ierr = SNESNASMGetDamping(
	(SNES)PetscToPointer((snes) ),dmp);
}
PETSC_EXTERN void  snesnasmgetsnes_(SNES snes,PetscInt *i,SNES *subsnes, int *__ierr)
{
*__ierr = SNESNASMGetSNES(
	(SNES)PetscToPointer((snes) ),*i,subsnes);
}
PETSC_EXTERN void  snesnasmgetnumber_(SNES snes,PetscInt *n, int *__ierr)
{
*__ierr = SNESNASMGetNumber(
	(SNES)PetscToPointer((snes) ),n);
}
PETSC_EXTERN void  snesnasmsetweight_(SNES snes,Vec weight, int *__ierr)
{
*__ierr = SNESNASMSetWeight(
	(SNES)PetscToPointer((snes) ),
	(Vec)PetscToPointer((weight) ));
}
#if defined(__cplusplus)
}
#endif

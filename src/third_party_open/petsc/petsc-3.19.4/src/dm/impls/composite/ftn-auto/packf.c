#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pack.c */
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

#include "petscdmcomposite.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmcompositegetnumberdm_ DMCOMPOSITEGETNUMBERDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcompositegetnumberdm_ dmcompositegetnumberdm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmcompositescatterarray_ DMCOMPOSITESCATTERARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcompositescatterarray_ dmcompositescatterarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmcompositegatherarray_ DMCOMPOSITEGATHERARRAY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcompositegatherarray_ dmcompositegatherarray
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmcompositeadddm_ DMCOMPOSITEADDDM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcompositeadddm_ dmcompositeadddm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmcompositecreate_ DMCOMPOSITECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmcompositecreate_ dmcompositecreate
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  dmcompositegetnumberdm_(DM dm,PetscInt *nDM, int *__ierr)
{
*__ierr = DMCompositeGetNumberDM(
	(DM)PetscToPointer((dm) ),nDM);
}
PETSC_EXTERN void  dmcompositescatterarray_(DM dm,Vec gvec,Vec *lvecs, int *__ierr)
{
*__ierr = DMCompositeScatterArray(
	(DM)PetscToPointer((dm) ),
	(Vec)PetscToPointer((gvec) ),lvecs);
}
PETSC_EXTERN void  dmcompositegatherarray_(DM dm,InsertMode *imode,Vec gvec,Vec *lvecs, int *__ierr)
{
*__ierr = DMCompositeGatherArray(
	(DM)PetscToPointer((dm) ),*imode,
	(Vec)PetscToPointer((gvec) ),lvecs);
}
PETSC_EXTERN void  dmcompositeadddm_(DM dmc,DM dm, int *__ierr)
{
*__ierr = DMCompositeAddDM(
	(DM)PetscToPointer((dmc) ),
	(DM)PetscToPointer((dm) ));
}
PETSC_EXTERN void  dmcompositecreate_(MPI_Fint * comm,DM *packer, int *__ierr)
{
*__ierr = DMCompositeCreate(
	MPI_Comm_f2c(*(comm)),packer);
}
#if defined(__cplusplus)
}
#endif

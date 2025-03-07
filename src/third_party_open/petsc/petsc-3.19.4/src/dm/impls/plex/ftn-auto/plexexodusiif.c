#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* plexexodusii.c */
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

#include "petscdmplex.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerexodusiigetid_ PETSCVIEWEREXODUSIIGETID
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerexodusiigetid_ petscviewerexodusiigetid
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerexodusiisetorder_ PETSCVIEWEREXODUSIISETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerexodusiisetorder_ petscviewerexodusiisetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerexodusiigetorder_ PETSCVIEWEREXODUSIIGETORDER
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerexodusiigetorder_ petscviewerexodusiigetorder
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplexcreateexodus_ DMPLEXCREATEEXODUS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplexcreateexodus_ dmplexcreateexodus
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerexodusiigetid_(PetscViewer viewer,int *exoid, int *__ierr)
{
*__ierr = PetscViewerExodusIIGetId(
	(PetscViewer)PetscToPointer((viewer) ),exoid);
}
PETSC_EXTERN void  petscviewerexodusiisetorder_(PetscViewer viewer,PetscInt *order, int *__ierr)
{
*__ierr = PetscViewerExodusIISetOrder(
	(PetscViewer)PetscToPointer((viewer) ),*order);
}
PETSC_EXTERN void  petscviewerexodusiigetorder_(PetscViewer viewer,PetscInt *order, int *__ierr)
{
*__ierr = PetscViewerExodusIIGetOrder(
	(PetscViewer)PetscToPointer((viewer) ),order);
}
PETSC_EXTERN void  dmplexcreateexodus_(MPI_Fint * comm,PetscInt *exoid,PetscBool *interpolate,DM *dm, int *__ierr)
{
*__ierr = DMPlexCreateExodus(
	MPI_Comm_f2c(*(comm)),*exoid,*interpolate,dm);
}
#if defined(__cplusplus)
}
#endif

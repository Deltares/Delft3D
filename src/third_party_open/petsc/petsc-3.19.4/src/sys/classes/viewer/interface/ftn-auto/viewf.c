#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* view.c */
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

#include "petscviewer.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerdestroy_ PETSCVIEWERDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerdestroy_ petscviewerdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewersetup_ PETSCVIEWERSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewersetup_ petscviewersetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerreadable_ PETSCVIEWERREADABLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerreadable_ petscviewerreadable
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerwritable_ PETSCVIEWERWRITABLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerwritable_ petscviewerwritable
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewercheckreadable_ PETSCVIEWERCHECKREADABLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewercheckreadable_ petscviewercheckreadable
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewercheckwritable_ PETSCVIEWERCHECKWRITABLE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewercheckwritable_ petscviewercheckwritable
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerdestroy_(PetscViewer *viewer, int *__ierr)
{
*__ierr = PetscViewerDestroy(viewer);
}
PETSC_EXTERN void  petscviewersetup_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerSetUp(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscviewerreadable_(PetscViewer viewer,PetscBool *flg, int *__ierr)
{
*__ierr = PetscViewerReadable(
	(PetscViewer)PetscToPointer((viewer) ),flg);
}
PETSC_EXTERN void  petscviewerwritable_(PetscViewer viewer,PetscBool *flg, int *__ierr)
{
*__ierr = PetscViewerWritable(
	(PetscViewer)PetscToPointer((viewer) ),flg);
}
PETSC_EXTERN void  petscviewercheckreadable_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerCheckReadable(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscviewercheckwritable_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerCheckWritable(
	(PetscViewer)PetscToPointer((viewer) ));
}
#if defined(__cplusplus)
}
#endif

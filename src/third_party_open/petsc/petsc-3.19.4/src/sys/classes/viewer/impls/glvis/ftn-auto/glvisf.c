#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* glvis.c */
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
#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerglvissetprecision_ PETSCVIEWERGLVISSETPRECISION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerglvissetprecision_ petscviewerglvissetprecision
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewerglvissetsnapid_ PETSCVIEWERGLVISSETSNAPID
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewerglvissetsnapid_ petscviewerglvissetsnapid
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewerglvissetprecision_(PetscViewer viewer,PetscInt *prec, int *__ierr)
{
*__ierr = PetscViewerGLVisSetPrecision(
	(PetscViewer)PetscToPointer((viewer) ),*prec);
}
PETSC_EXTERN void  petscviewerglvissetsnapid_(PetscViewer viewer,PetscInt *id, int *__ierr)
{
*__ierr = PetscViewerGLVisSetSnapId(
	(PetscViewer)PetscToPointer((viewer) ),*id);
}
#if defined(__cplusplus)
}
#endif

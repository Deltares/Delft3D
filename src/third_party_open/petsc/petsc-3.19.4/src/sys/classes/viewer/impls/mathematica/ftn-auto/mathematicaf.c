#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* mathematica.c */
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

#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewermathematicaclearname_ PETSCVIEWERMATHEMATICACLEARNAME
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewermathematicaclearname_ petscviewermathematicaclearname
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewermathematicagetvector_ PETSCVIEWERMATHEMATICAGETVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewermathematicagetvector_ petscviewermathematicagetvector
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscviewermathematicaputvector_ PETSCVIEWERMATHEMATICAPUTVECTOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscviewermathematicaputvector_ petscviewermathematicaputvector
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void  petscviewermathematicaclearname_(PetscViewer viewer, int *__ierr)
{
*__ierr = PetscViewerMathematicaClearName(
	(PetscViewer)PetscToPointer((viewer) ));
}
PETSC_EXTERN void  petscviewermathematicagetvector_(PetscViewer viewer,Vec v, int *__ierr)
{
*__ierr = PetscViewerMathematicaGetVector(
	(PetscViewer)PetscToPointer((viewer) ),
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void  petscviewermathematicaputvector_(PetscViewer viewer,Vec v, int *__ierr)
{
*__ierr = PetscViewerMathematicaPutVector(
	(PetscViewer)PetscToPointer((viewer) ),
	(Vec)PetscToPointer((v) ));
}
#if defined(__cplusplus)
}
#endif
